const std = @import("std");
const Ast = std.zig.Ast;
const logger = std.log.scoped(.hover);
const ws = @import("workspace");
const ClientCapabilities = ws.ClientCapabilities;
const Workspace = ws.Workspace;
const Document = ws.Document;
const DeclWithHandle = ws.DeclWithHandle;
const ast = ws.ast;
const builtin_completions = ws.builtin_completions;
const AstGetChildren = ws.AstGetChildren;

pub fn process(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    doc: *Document,
    byte_position: u32,
    client_capabilities: *ClientCapabilities,
) !?[]const u8 {
    const token_with_index = doc.ast_context.tokenFromBytePos(byte_position) orelse {
        // token not found. return no hover.
        return null;
    };

    var context_info = try doc.ast_context.getTokenIndexContext(arena.allocator(), token_with_index.index);

    _ = workspace;
    _ = client_capabilities;

    const name = doc.ast_context.getTokenText(token_with_index.token);
    const allocator = arena.allocator();
    switch (token_with_index.token.tag) {
        .builtin => {
            if (builtin_completions.find(name)) |builtin| {
                return try std.fmt.allocPrint(
                    allocator,
                    "# builtin: {s}\n\n```zig\n{s}\n```\n\n`{s}`\n\n{s}",
                    .{ name, builtin.signature, context_info, builtin.documentation },
                );
            } else {
                return try std.fmt.allocPrint(
                    allocator,
                    "{s}\n* builtin {s} not found",
                    .{ context_info, name },
                );
            }
        },
        .identifier => {
            const tag = doc.ast_context.tree.nodes.items(.tag);
            const idx = doc.ast_context.tokens_node[token_with_index.index];
            const node_tag = tag[idx];
            switch (node_tag) {
                .identifier => {
                    if (try DeclWithHandle.getSymbolGlobal(arena, workspace, doc, byte_position)) |decl| {
                        const hover = try decl.hoverSymbol(arena, workspace, if (client_capabilities.hover_supports_md) .Markdown else .PlainText);
                        return try std.fmt.allocPrint(
                            allocator,
                            "# {s}\n\n{s}\n\n{s}",
                            .{ name, context_info, hover },
                        );
                    } else {
                        return try std.fmt.allocPrint(
                            allocator,
                            "{s}\n* decl {s} not found",
                            .{ context_info, name },
                        );
                    }
                },
                // .fn_proto_multi => {
                //     const signature = ast.getFunctionSignature(doc.ast_context.tree, doc.ast_context.tree.fnProtoMulti(idx));
                //     return try std.fmt.allocPrint(
                //         allocator,
                //         "# function: {s}\n\n```zig\n{s}\n```",
                //         .{ name, signature },
                //     );
                // },
                .field_access => {
                    const decl = try DeclWithHandle.getSymbolFieldAccess(arena, workspace, doc, byte_position);
                    const hover = try decl.hoverSymbol(arena, workspace, if (client_capabilities.hover_supports_md) .Markdown else .PlainText);
                    return try std.fmt.allocPrint(
                        allocator,
                        "# {s}\n\n{s}\n\n{s}",
                        .{ name, context_info, hover },
                    );
                    // var buffer = std.ArrayList(u8).init(arena.allocator());
                    // const w = buffer.writer();
                    // try w.print("# field_access: {s}\n\n{s}\n\n", .{name, context_info});
                    // var current = doc.ast_context.nodes_parent[idx];
                    // var i: u32 = 1;
                    // while (current != 0) : ({
                    //     current = doc.ast_context.nodes_parent[current];
                    //     i += 1;
                    // }) {
                    //     const current_tag = tag[current];
                    //     try w.print("* [{}]{}\n", .{ i, current_tag });
                    // }
                    // return buffer.items;
                },
                //     .label => {
                //         logger.debug("[hover][label_access]", .{});
                //         if (try offsets.getLabelGlobal(doc_position.absolute_index, doc)) |decl| {
                //             return try hoverSymbol(arena, workspace, id, decl, client_capabilities);
                //         }
                //     },
                else => {
                    return context_info;
                },
            }
        },
        else => {
            return null;
        },
    }
}
