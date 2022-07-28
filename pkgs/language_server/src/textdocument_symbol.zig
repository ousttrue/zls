const std = @import("std");
const lsp = @import("lsp");
const ws = @import("workspace");
const Document = ws.Document;
const LinePosition = ws.LinePosition;
const Line = ws.Line;
const SymbolTree = ws.SymbolTree;

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
            const first = ast_context.tokens.items[tree.firstToken(symbol.node)];
            var start_loc = try doc.utf8_buffer.getPositionFromBytePosition(first.loc.start, encoding);
            const last = ast_context.tokens.items[tree.lastToken(symbol.node)];
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
