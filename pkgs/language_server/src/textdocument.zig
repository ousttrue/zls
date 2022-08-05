const std = @import("std");
const Ast = std.zig.Ast;
const lsp = @import("lsp");
const ws = @import("workspace");
const Config = ws.Config;
const Document = ws.Document;
const Line = ws.Line;
const TypeWithHandle = ws.TypeWithHandle;
const SymbolTree = ws.SymbolTree;
const ast = ws.ast;

// TODO: Is this correct or can we get a better end?
fn astLocationToRange(loc: Ast.Location) lsp.Range {
    return .{
        .start = .{
            .line = @intCast(i64, loc.line),
            .character = @intCast(i64, loc.column),
        },
        .end = .{
            .line = @intCast(i64, loc.line),
            .character = @intCast(i64, loc.column),
        },
    };
}

pub fn createNotifyDiagnostics(arena: *std.heap.ArenaAllocator, doc: *const Document, config: *Config) !lsp.Notification {
    const tree = doc.tree;

    var diagnostics = std.ArrayList(lsp.Diagnostic).init(arena.allocator());

    for (tree.errors) |err| {
        const loc = tree.tokenLocation(0, err.token);

        var mem_buffer: [256]u8 = undefined;
        var fbs = std.io.fixedBufferStream(&mem_buffer);
        try tree.renderError(err, fbs.writer());

        try diagnostics.append(.{
            .range = astLocationToRange(loc),
            .severity = .Error,
            .code = @tagName(err.tag),
            .source = "zls",
            .message = try arena.allocator().dupe(u8, fbs.getWritten()),
            // .relatedInformation = undefined
        });
    }

    // TODO: style warnings for types, values and declarations below root scope
    if (tree.errors.len == 0) {
        for (tree.rootDecls()) |decl_idx| {
            const decl = tree.nodes.items(.tag)[decl_idx];
            switch (decl) {
                .fn_proto,
                .fn_proto_multi,
                .fn_proto_one,
                .fn_proto_simple,
                .fn_decl,
                => blk: {
                    var buf: [1]Ast.Node.Index = undefined;
                    const func = ast.fnProto(tree, decl_idx, &buf).?;
                    if (func.extern_export_inline_token != null) break :blk;

                    if (config.warn_style) {
                        if (func.name_token) |name_token| {
                            const loc = tree.tokenLocation(0, name_token);

                            const is_type_function = TypeWithHandle.isTypeFunction(tree, func);

                            const func_name = tree.tokenSlice(name_token);
                            if (!is_type_function and !ast.isCamelCase(func_name)) {
                                try diagnostics.append(.{
                                    .range = astLocationToRange(loc),
                                    .severity = .Information,
                                    .code = "BadStyle",
                                    .source = "zls",
                                    .message = "Functions should be camelCase",
                                });
                            } else if (is_type_function and !ast.isPascalCase(func_name)) {
                                try diagnostics.append(.{
                                    .range = astLocationToRange(loc),
                                    .severity = .Information,
                                    .code = "BadStyle",
                                    .source = "zls",
                                    .message = "Type functions should be PascalCase",
                                });
                            }
                        }
                    }
                },
                else => {},
            }
        }
    }

    // logger.debug("[Diagnostics] {s}: {}", .{ doc.utf8_buffer.uri, diagnostics.items.len });
    return lsp.Notification{
        .method = "textDocument/publishDiagnostics",
        .params = .{
            .PublishDiagnostics = .{
                .uri = doc.uri,
                .diagnostics = diagnostics.items,
            },
        },
    };
}

fn node_tag_to_lsp_symbol_kind(node_tag: std.zig.Ast.Node.Tag) lsp.DocumentSymbol.Kind {
    return switch (node_tag) {
        .fn_proto,
        .fn_proto_simple,
        .fn_proto_multi,
        .fn_proto_one,
        .fn_decl,
        => .Function,
        .local_var_decl,
        .global_var_decl,
        .aligned_var_decl,
        .simple_var_decl,
        => .Variable,
        .container_field,
        .container_field_align,
        .container_field_init,
        .tagged_union_enum_tag,
        .tagged_union_enum_tag_trailing,
        .tagged_union,
        .tagged_union_trailing,
        .tagged_union_two,
        .tagged_union_two_trailing,
        => .Field,
        else => .Variable,
    };
}

pub fn to_symbols(allocator: std.mem.Allocator, doc: *Document, encoding: Line.Encoding, src: []const SymbolTree.Symbol, parent: u32) anyerror![]lsp.DocumentSymbol {
    var dst = std.ArrayList(lsp.DocumentSymbol).init(allocator);
    const tree = doc.ast_context.tree;
    const ast_context = doc.ast_context;
    const tags = tree.nodes.items(.tag);
    for (src) |symbol| {
        if (symbol.parent == parent) {
            const first = ast_context.tokens[tree.firstToken(symbol.node)];
            var start_loc = try doc.utf8_buffer.getPositionFromBytePosition(first.loc.start, encoding);
            const last = ast_context.tokens[tree.lastToken(symbol.node)];
            var end_loc = try doc.utf8_buffer.getPositionFromBytePosition(last.loc.end, encoding);

            var range = lsp.Range{
                .start = .{
                    .line = @intCast(i64, start_loc.line),
                    .character = @intCast(i64, start_loc.x),
                },
                .end = .{
                    .line = @intCast(i64, end_loc.line),
                    .character = @intCast(i64, end_loc.x),
                },
            };

            try dst.append(lsp.DocumentSymbol{
                .name = symbol.name,
                .kind = node_tag_to_lsp_symbol_kind(tags[symbol.node]),
                .range = range,
                .selectionRange = range,
                .detail = "",
                .children = try to_symbols(allocator, doc, encoding, src, symbol.node),
            });
        }
    }
    return dst.items;
}
