const std = @import("std");
const lsp = @import("lsp");
const offsets = @import("./offsets.zig");
const ast = @import("./ast.zig");
const analysis = @import("./analysis.zig");
const TypeWithHandle = @import("./TypeWithHandle.zig");
const Ast = std.zig.Ast;

const GetDocumentSymbolsContext = struct {
    prev_loc: offsets.TokenLocation = .{
        .line = 0,
        .column = 0,
        .offset = 0,
    },
    symbols: *std.ArrayList(lsp.DocumentSymbol),
    encoding: offsets.Encoding,
};

fn addOutlineNodes(allocator: std.mem.Allocator, tree: Ast, child: Ast.Node.Index, context: *GetDocumentSymbolsContext) anyerror!void {
    switch (tree.nodes.items(.tag)[child]) {
        .string_literal,
        .integer_literal,
        .builtin_call,
        .builtin_call_comma,
        .builtin_call_two,
        .builtin_call_two_comma,
        .call,
        .call_comma,
        .call_one,
        .call_one_comma,
        .async_call,
        .async_call_comma,
        .async_call_one,
        .async_call_one_comma,
        .identifier,
        .add,
        .add_wrap,
        .array_cat,
        .array_mult,
        .assign,
        .assign_bit_and,
        .assign_bit_or,
        .assign_shl,
        .assign_shr,
        .assign_bit_xor,
        .assign_div,
        .assign_sub,
        .assign_sub_wrap,
        .assign_mod,
        .assign_add,
        .assign_add_wrap,
        .assign_mul,
        .assign_mul_wrap,
        .bang_equal,
        .bit_and,
        .bit_or,
        .shl,
        .shr,
        .bit_xor,
        .bool_and,
        .bool_or,
        .div,
        .equal_equal,
        .error_union,
        .greater_or_equal,
        .greater_than,
        .less_or_equal,
        .less_than,
        .merge_error_sets,
        .mod,
        .mul,
        .mul_wrap,
        .field_access,
        .switch_range,
        .sub,
        .sub_wrap,
        .@"orelse",
        .address_of,
        .@"await",
        .bit_not,
        .bool_not,
        .optional_type,
        .negation,
        .negation_wrap,
        .@"resume",
        .@"try",
        .array_type,
        .array_type_sentinel,
        .ptr_type,
        .ptr_type_aligned,
        .ptr_type_bit_range,
        .ptr_type_sentinel,
        .slice_open,
        .slice_sentinel,
        .deref,
        .unwrap_optional,
        .array_access,
        .@"return",
        .@"break",
        .@"continue",
        .array_init,
        .array_init_comma,
        .array_init_dot,
        .array_init_dot_comma,
        .array_init_dot_two,
        .array_init_dot_two_comma,
        .array_init_one,
        .array_init_one_comma,
        .@"switch",
        .switch_comma,
        .switch_case,
        .switch_case_one,
        .@"for",
        .for_simple,
        .enum_literal,
        .struct_init,
        .struct_init_comma,
        .struct_init_dot,
        .struct_init_dot_comma,
        .struct_init_dot_two,
        .struct_init_dot_two_comma,
        .struct_init_one,
        .struct_init_one_comma,
        .@"while",
        .while_simple,
        .while_cont,
        .@"defer",
        .@"if",
        .if_simple,
        .multiline_string_literal,
        .block,
        .block_semicolon,
        .block_two,
        .block_two_semicolon,
        .error_set_decl,
        => return,
        .container_decl,
        .container_decl_arg,
        .container_decl_arg_trailing,
        .container_decl_two,
        .container_decl_two_trailing,
        .tagged_union,
        .tagged_union_trailing,
        .tagged_union_enum_tag,
        .tagged_union_enum_tag_trailing,
        .tagged_union_two,
        .tagged_union_two_trailing,
        => {
            var buf: [2]Ast.Node.Index = undefined;
            for (ast.declMembers(tree, child, &buf)) |member|
                try addOutlineNodes(allocator, tree, member, context);
            return;
        },
        else => {},
    }
    try getDocumentSymbolsInternal(allocator, tree, child, context);
}

fn getDocumentSymbolsInternal(allocator: std.mem.Allocator, tree: Ast, node: Ast.Node.Index, context: *GetDocumentSymbolsContext) anyerror!void {
    const name = TypeWithHandle.getDeclName(tree, node) orelse return;
    if (name.len == 0)
        return;

    const starts = tree.tokens.items(.start);
    const start_loc = context.prev_loc.add(try offsets.tokenRelativeLocation(
        tree,
        context.prev_loc.offset,
        starts[tree.firstToken(node)],
        context.encoding,
    ));
    const end_loc = start_loc.add(try offsets.tokenRelativeLocation(
        tree,
        start_loc.offset,
        starts[ast.lastToken(tree, node)],
        context.encoding,
    ));
    context.prev_loc = end_loc;
    const range = lsp.Range{
        .start = .{
            .line = @intCast(i64, start_loc.line),
            .character = @intCast(i64, start_loc.column),
        },
        .end = .{
            .line = @intCast(i64, end_loc.line),
            .character = @intCast(i64, end_loc.column),
        },
    };

    const tags = tree.nodes.items(.tag);
    (try context.symbols.addOne()).* = .{
        .name = name,
        .kind = switch (tags[node]) {
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
        },
        .range = range,
        .selectionRange = range,
        .detail = "",
        .children = ch: {
            var children = std.ArrayList(lsp.DocumentSymbol).init(allocator);

            var child_context = GetDocumentSymbolsContext{
                .prev_loc = start_loc,
                .symbols = &children,
                .encoding = context.encoding,
            };

            if (ast.isContainer(tree, node)) {
                var buf: [2]Ast.Node.Index = undefined;
                for (ast.declMembers(tree, node, &buf)) |child|
                    try addOutlineNodes(allocator, tree, child, &child_context);
            }

            if (ast.varDecl(tree, node)) |var_decl| {
                if (var_decl.ast.init_node != 0)
                    try addOutlineNodes(allocator, tree, var_decl.ast.init_node, &child_context);
            }
            if (tags[node] == .fn_decl) fn_ch: {
                const fn_decl = tree.nodes.items(.data)[node];
                var params: [1]Ast.Node.Index = undefined;
                const fn_proto = ast.fnProto(tree, fn_decl.lhs, &params) orelse break :fn_ch;
                if (!TypeWithHandle.isTypeFunction(tree, fn_proto)) break :fn_ch;
                const ret_stmt = TypeWithHandle.findReturnStatement(tree, fn_proto, fn_decl.rhs) orelse break :fn_ch;
                const type_decl = tree.nodes.items(.data)[ret_stmt].lhs;
                if (type_decl != 0)
                    try addOutlineNodes(allocator, tree, type_decl, &child_context);
            }
            break :ch children.items;
        },
    };
}

pub fn getDocumentSymbols(allocator: std.mem.Allocator, tree: Ast, encoding: offsets.Encoding) ![]lsp.DocumentSymbol {
    var symbols = try std.ArrayList(lsp.DocumentSymbol).initCapacity(allocator, tree.rootDecls().len);

    var context = GetDocumentSymbolsContext{
        .symbols = &symbols,
        .encoding = encoding,
    };

    for (tree.rootDecls()) |idx| {
        try getDocumentSymbolsInternal(allocator, tree, idx, &context);
    }

    return symbols.items;
}
