const std = @import("std");
const ClientCapabilities = @import("./ClientCapabilities.zig");
const Document = @import("./Document.zig");
const Workspace = @import("./Workspace.zig");
const DeclWithHandle = @import("./DeclWithHandle.zig");
const ast = @import("./ast.zig");
const builtin_completions = @import("./builtin_completions.zig");
const Ast = std.zig.Ast;
const AstGetChildren = @import("./AstGetChildren.zig");
const logger = std.log.scoped(.hover);

fn hoverSymbol(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    decl_handle: DeclWithHandle,
    client_capabilities: *ClientCapabilities,
) (std.os.WriteError || error{OutOfMemory})!?[]const u8 {
    const handle = decl_handle.handle;
    const tree = handle.tree;

    const hover_kind: ast.MarkupFormat = if (client_capabilities.hover_supports_md) .Markdown else .PlainText;
    var doc_str: ?[]const u8 = null;

    const def_str = switch (decl_handle.decl.*) {
        .ast_node => |node| def: {
            if (try DeclWithHandle.resolveVarDeclAlias(arena, workspace, handle, node)) |result| {
                return try hoverSymbol(arena, workspace, result, client_capabilities);
            }
            doc_str = try ast.getDocComments(arena.allocator(), tree, node, hover_kind);

            var buf: [1]Ast.Node.Index = undefined;

            if (ast.varDecl(tree, node)) |var_decl| {
                break :def ast.getVariableSignature(tree, var_decl);
            } else if (ast.fnProto(tree, node, &buf)) |fn_proto| {
                break :def ast.getFunctionSignature(tree, fn_proto);
            } else if (ast.containerField(tree, node)) |field| {
                break :def ast.getContainerFieldSignature(tree, field);
            } else {
                if (ast.nodeToString(tree, node)) |text| {
                    break :def text;
                }
                return null;
            }
        },
        .param_decl => |param| def: {
            if (param.first_doc_comment) |doc_comments| {
                doc_str = try ast.collectDocComments(arena.allocator(), handle.tree, doc_comments, hover_kind, false);
            }

            const first_token = param.first_doc_comment orelse
                param.comptime_noalias orelse
                param.name_token orelse
                tree.firstToken(param.type_expr); // extern fn
            const last_token = param.anytype_ellipsis3 orelse tree.lastToken(param.type_expr);

            const start = ast.tokenLocation(tree, first_token).start;
            const end = ast.tokenLocation(tree, last_token).end;
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
    doc: *Document,
    byte_position: u32,
    client_capabilities: *ClientCapabilities,
) !?[]const u8 {
    _ = workspace;
    _ = client_capabilities;
    if (doc.ast_context.tokenFromBytePos(byte_position)) |token_with_index| {
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
                    // .simple_var_decl => {
                    //     logger.debug("(hover)[var_access]", .{});
                    //     if (try workspace.getSymbolGlobal(arena, doc, byte_position)) |decl| {
                    //         return try hoverSymbol(arena, workspace, decl, client_capabilities);
                    //     } else {
                    //         return error.HoverError;
                    //     }
                    // },
                    // .fn_proto_multi => {
                    //     return ast.getFunctionSignature(doc.tree, doc.tree.fnProtoMulti(idx));
                    // },
                    // .field_access => {
                    //     const lhs = AstGetChildren.getChild(arena.allocator(), &doc.tree, idx);
                    //     const lhs_tag = tag[lhs];

                    //     // const decl = try getSymbolFieldAccess(arena, workspace, doc, doc_position, token_with_index.token.loc. name);
                    //     // return try hoverSymbol(arena, workspace, id, decl, client_capabilities);
                    //     var buffer = std.ArrayList(u8).init(arena.allocator());
                    //     const w = buffer.writer();
                    //     try w.print("[field_access] ({}).{s}: {} =>\n* [0]{}\n", .{
                    //         lhs_tag,
                    //         name,
                    //         token_with_index.token.tag,
                    //         node_tag,
                    //     });
                    //     var current = doc.ast_context.nodes_parent[idx];
                    //     var i: u32 = 1;
                    //     while (current != 0) : ({
                    //         current = doc.ast_context.nodes_parent[current];
                    //         i += 1;
                    //     }) {
                    //         const current_tag = tag[current];
                    //         try w.print("* [{}]{}\n", .{ i, current_tag });
                    //     }
                    //     return buffer.items;
                    // },
                    //     .label => {
                    //         logger.debug("[hover][label_access]", .{});
                    //         if (try offsets.getLabelGlobal(doc_position.absolute_index, doc)) |decl| {
                    //             return try hoverSymbol(arena, workspace, id, decl, client_capabilities);
                    //         }
                    //     },
                    else => {
                        return try doc.ast_context.getTokenIndexContext(arena.allocator(), token_with_index.index);
                    },
                }
            },
        }
    }
    else{
        return null;
    }
}
