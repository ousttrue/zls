const std = @import("std");
const lsp = @import("lsp");
const ClientCapabilities = @import("./ClientCapabilities.zig");
const Document = @import("./Document.zig");
const Workspace = @import("./Workspace.zig");
const DocumentPosition = @import("./DocumentPosition.zig");
const position_context = @import("./position_context.zig");
const offsets = @import("./offsets.zig");
const analysis = @import("./analysis.zig");
const ast = @import("./ast.zig");
const builtin_completions = @import("./builtin_completions.zig");
const Ast = std.zig.Ast;
const logger = std.log.scoped(.hover);

fn hoverDefinitionBuiltin(arena: *std.heap.ArenaAllocator, id: i64, pos_index: usize, handle: *Document) !lsp.Response {
    const name = try offsets.identifierFromPosition(pos_index, handle.document.text);
    for (builtin_completions.data()) |builtin| {
        if (std.mem.eql(u8, builtin.name[1..], name)) {
            return lsp.Response{
                .id = id,
                .result = .{
                    .Hover = .{
                        .contents = .{
                            .value = try std.fmt.allocPrint(
                                arena.allocator(),
                                "```zig\n{s}\n```\n{s}",
                                .{ builtin.signature, builtin.documentation },
                            ),
                        },
                    },
                },
            };
        }
    }

    unreachable;
}

fn hoverSymbol(arena: *std.heap.ArenaAllocator, workspace: *Workspace, id: i64, decl_handle: analysis.DeclWithHandle, client_capabilities: *ClientCapabilities,) (std.os.WriteError || error{OutOfMemory})!lsp.Response {
    const handle = decl_handle.handle;
    const tree = handle.tree;

    const hover_kind: lsp.MarkupContent.Kind = if (client_capabilities.hover_supports_md) .Markdown else .PlainText;
    var doc_str: ?[]const u8 = null;

    const def_str = switch (decl_handle.decl.*) {
        .ast_node => |node| def: {
            if (try analysis.resolveVarDeclAlias(arena, workspace, .{ .node = node, .handle = handle })) |result| {
                return try hoverSymbol(arena, workspace, id, result, client_capabilities);
            }
            doc_str = try analysis.getDocComments(arena.allocator(), tree, node, hover_kind);

            var buf: [1]Ast.Node.Index = undefined;

            if (ast.varDecl(tree, node)) |var_decl| {
                break :def analysis.getVariableSignature(tree, var_decl);
            } else if (ast.fnProto(tree, node, &buf)) |fn_proto| {
                break :def analysis.getFunctionSignature(tree, fn_proto);
            } else if (ast.containerField(tree, node)) |field| {
                break :def analysis.getContainerFieldSignature(tree, field);
            } else {
                break :def analysis.nodeToString(tree, node) orelse
                    return lsp.Response.createNull(id);
            }
        },
        .param_decl => |param| def: {
            if (param.first_doc_comment) |doc_comments| {
                doc_str = try analysis.collectDocComments(arena.allocator(), handle.tree, doc_comments, hover_kind, false);
            }

            const first_token = param.first_doc_comment orelse
                param.comptime_noalias orelse
                param.name_token orelse
                tree.firstToken(param.type_expr); // extern fn
            const last_token = param.anytype_ellipsis3 orelse tree.lastToken(param.type_expr);

            const start = offsets.tokenLocation(tree, first_token).start;
            const end = offsets.tokenLocation(tree, last_token).end;
            break :def tree.source[start..end];
        },
        .pointer_payload => |payload| tree.tokenSlice(payload.name),
        .array_payload => |payload| handle.tree.tokenSlice(payload.identifier),
        .array_index => |payload| handle.tree.tokenSlice(payload),
        .switch_payload => |payload| tree.tokenSlice(payload.node),
        .label_decl => |label_decl| tree.tokenSlice(label_decl),
    };

    var hover_text: []const u8 = undefined;
    if (hover_kind == .Markdown) {
        hover_text =
            if (doc_str) |doc|
            try std.fmt.allocPrint(arena.allocator(), "```zig\n{s}\n```\n{s}", .{ def_str, doc })
        else
            try std.fmt.allocPrint(arena.allocator(), "```zig\n{s}\n```", .{def_str});
    } else {
        hover_text =
            if (doc_str) |doc|
            try std.fmt.allocPrint(arena.allocator(), "{s}\n{s}", .{ def_str, doc })
        else
            def_str;
    }

    return lsp.Response{
        .id = id,
        .result = .{
            .Hover = .{
                .contents = .{ .value = hover_text },
            },
        },
    };
}

pub fn process(arena: *std.heap.ArenaAllocator, workspace: *Workspace, id: i64, handle: *Document, doc_position: DocumentPosition, client_capabilities: *ClientCapabilities,) !lsp.Response
{
    const pos_context = position_context.documentPositionContext(arena, doc_position);
    switch (pos_context) {
        .builtin => {
            logger.debug("[hover][builtin]", .{});
            return try hoverDefinitionBuiltin(arena, id, doc_position.absolute_index, handle);
        },
        .var_access => {
            logger.debug("[hover][var_access]", .{});
            const decl = try offsets.getSymbolGlobal(arena, workspace, doc_position.absolute_index, handle);
            return try hoverSymbol(arena, workspace, id, decl, client_capabilities);
        },
        .field_access => |range| {
            logger.debug("[hover][field_access]", .{});
            const decl = try offsets.getSymbolFieldAccess(arena, workspace, handle, doc_position, range);
            return try hoverSymbol(arena, workspace, id, decl, client_capabilities);
        },
        .label => {
            logger.debug("[hover][label_access]", .{});
            const decl = (try offsets.getLabelGlobal(doc_position.absolute_index, handle)) orelse return lsp.Response.createNull(id);
            return try hoverSymbol(arena, workspace, id, decl, client_capabilities);
        },
        else => {
            logger.debug("[hover][{s}]", .{@tagName(pos_context)});
            return lsp.Response.createNull(id);
        },
    }
}
