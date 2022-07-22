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
const AstGetChildren = @import("./AstGetChildren.zig");
const logger = std.log.scoped(.hover);

fn hoverSymbol(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    id: i64,
    decl_handle: analysis.DeclWithHandle,
    client_capabilities: *ClientCapabilities,
) (std.os.WriteError || error{OutOfMemory})!?[]const u8 {
    const handle = decl_handle.handle;
    const tree = handle.tree;

    const hover_kind: lsp.MarkupContent.Kind = if (client_capabilities.hover_supports_md) .Markdown else .PlainText;
    var doc_str: ?[]const u8 = null;

    const def_str = switch (decl_handle.decl.*) {
        .ast_node => |node| def: {
            if (try analysis.resolveVarDeclAlias(arena, workspace, handle, node)) |result| {
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
                if (analysis.nodeToString(tree, node)) |text| {
                    break :def text;
                }
                return null;
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
        hover_text = if (doc_str) |doc|
            try std.fmt.allocPrint(arena.allocator(), "```zig\n{s}\n```\n{s}", .{ def_str, doc })
        else
            try std.fmt.allocPrint(arena.allocator(), "```zig\n{s}\n```", .{def_str});
    } else {
        hover_text = if (doc_str) |doc|
            try std.fmt.allocPrint(arena.allocator(), "{s}\n{s}", .{ def_str, doc })
        else
            def_str;
    }
    return hover_text;
}

pub fn process(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    id: i64,
    doc: *Document,
    doc_position: DocumentPosition,
    client_capabilities: *ClientCapabilities,
) !?[]const u8 {
    if (doc.ast_context.tokenFromBytePos(doc_position.absolute_index)) |token_with_index| {
        const name = doc.ast_context.getTokenText(token_with_index.token);
        switch (token_with_index.token.tag) {
            .builtin => {
                logger.debug("(hover)[builtin]: {s}", .{name});
                if (builtin_completions.find(name)) |builtin| {
                    return try std.fmt.allocPrint(
                        arena.allocator(),
                        "```zig\n{s}\n```\n{s}",
                        .{ builtin.signature, builtin.documentation },
                    );
                } else {
                    logger.debug("builtin {s} not found", .{name});
                    return error.HoverError;
                }
            },
            else => {
                const tag = doc.tree.nodes.items(.tag);
                const idx = doc.ast_context.tokens_node[token_with_index.index];
                const node_tag = tag[idx];
                switch (node_tag) {
                    .simple_var_decl => {
                        logger.debug("(hover)[var_access]", .{});
                        if (try workspace.getSymbolGlobal(arena, doc, doc_position.absolute_index)) |decl| {
                            return try hoverSymbol(arena, workspace, id, decl, client_capabilities);
                        } else {
                            return error.HoverError;
                        }
                    },
                    .fn_proto_multi => {
                        return analysis.getFunctionSignature(doc.tree, doc.tree.fnProtoMulti(idx));
                    },
                    .field_access => {
                        const lhs = AstGetChildren.getChild(arena.allocator(), &doc.tree, idx);
                        const lhs_tag = tag[lhs];

                        // const decl = try getSymbolFieldAccess(arena, workspace, doc, doc_position, token_with_index.token.loc. name);
                        // return try hoverSymbol(arena, workspace, id, decl, client_capabilities);
                        var buffer = std.ArrayList(u8).init(arena.allocator());
                        const w = buffer.writer();
                        try w.print("[field_access] ({}).{s}: {} =>\n* [0]{}\n", .{
                            lhs_tag,
                            name,
                            token_with_index.token.tag,
                            node_tag,
                        });
                        var current = doc.ast_context.nodes_parent[idx];
                        var i: u32 = 1;
                        while (current != 0) : ({
                            current = doc.ast_context.nodes_parent[current];
                            i += 1;
                        }) {
                            const current_tag = tag[current];
                            try w.print("* [{}]{}\n", .{ i, current_tag });
                        }
                        return buffer.items;
                    },
                    else => {
                        var buffer = std.ArrayList(u8).init(arena.allocator());
                        const w = buffer.writer();
                        try w.print("{s}: {} =>\n* [0]{}\n", .{
                            name,
                            token_with_index.token.tag,
                            node_tag,
                        });
                        var current = doc.ast_context.nodes_parent[idx];
                        var i: u32 = 1;
                        while (current != 0) : ({
                            current = doc.ast_context.nodes_parent[current];
                            i += 1;
                        }) {
                            const current_tag = tag[current];
                            try w.print("* [{}]{}\n", .{ i, current_tag });
                        }
                        return buffer.items;
                    },
                }
            },
        }
    }

    // // const pos_context = position_context.documentPositionContext(arena, doc_position);
    // const pos_context = doc.getPositionContext(doc_position.absolute_index);
    // switch (pos_context) {
    //     .var_access => {
    //         logger.debug("[hover][var_access]", .{});
    //         const decl = try offsets.getSymbolGlobal(arena, workspace, doc_position.absolute_index, doc);
    //         return try hoverSymbol(arena, workspace, id, decl, client_capabilities);
    //     },
    //     .field_access => |range| {
    //         logger.debug("[hover][field_access]", .{});
    //         const decl = try offsets.getSymbolFieldAccess(arena, workspace, doc, doc_position, range);
    //         return try hoverSymbol(arena, workspace, id, decl, client_capabilities);
    //     },
    //     .label => {
    //         logger.debug("[hover][label_access]", .{});
    //         if (try offsets.getLabelGlobal(doc_position.absolute_index, doc)) |decl| {
    //             return try hoverSymbol(arena, workspace, id, decl, client_capabilities);
    //         }
    //     },
    //     else => {
    //         logger.debug("[hover][{s}]", .{@tagName(pos_context)});
    //     },
    // }

    // return try std.fmt.allocPrint(arena.allocator(), "{}", .{pos_context});
    return null;
}
