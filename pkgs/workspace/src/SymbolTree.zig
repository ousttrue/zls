const std = @import("std");
const ast = @import("./ast.zig");
const TypeWithHandle = @import("./TypeWithHandle.zig");
const Ast = std.zig.Ast;
const AstGetChildren = @import("./AstGetChildren.zig");
const logger = std.log.scoped(.SymbolTree);

pub const Symbol = struct {
    name: []const u8,
    parent: u32,
    node: u32,
};

const Self = @This();
allocator: std.mem.Allocator,
symbols: std.ArrayList(Symbol),
tree: std.zig.Ast,

pub fn init(allocator: std.mem.Allocator, tree: std.zig.Ast) Self {
    return .{
        .allocator = allocator,
        .symbols = std.ArrayList(Symbol).init(allocator),
        .tree = tree,
    };
}

pub fn deinit(self: Self) void {
    self.symbols.deinit();
}

pub fn traverse(self: *Self, node: Ast.Node.Index) anyerror!void {
    var children = AstGetChildren.init(self.allocator);
    defer children.deinit();
    for (children.getChildren(&self.tree, node)) |child| {
        if (TypeWithHandle.getDeclName(self.tree, child)) |name| {
            if (name.len > 0) {
                // logger.debug("symbol: {s}", .{name});
                try self.symbols.append(.{
                    .name = name,
                    .parent = node,
                    .node = child,
                });
                try self.traverse(child);
            }
        } 
    }
}

// fn addOutlineNodes(allocator: std.mem.Allocator, tree: Ast, child: Ast.Node.Index, context: *GetDocumentSymbolsContext) anyerror!void {
//     switch (tree.nodes.items(.tag)[child]) {
//         .string_literal,
//         .integer_literal,
//         .builtin_call,
//         .builtin_call_comma,
//         .builtin_call_two,
//         .builtin_call_two_comma,
//         .call,
//         .call_comma,
//         .call_one,
//         .call_one_comma,
//         .async_call,
//         .async_call_comma,
//         .async_call_one,
//         .async_call_one_comma,
//         .identifier,
//         .add,
//         .add_wrap,
//         .array_cat,
//         .array_mult,
//         .assign,
//         .assign_bit_and,
//         .assign_bit_or,
//         .assign_shl,
//         .assign_shr,
//         .assign_bit_xor,
//         .assign_div,
//         .assign_sub,
//         .assign_sub_wrap,
//         .assign_mod,
//         .assign_add,
//         .assign_add_wrap,
//         .assign_mul,
//         .assign_mul_wrap,
//         .bang_equal,
//         .bit_and,
//         .bit_or,
//         .shl,
//         .shr,
//         .bit_xor,
//         .bool_and,
//         .bool_or,
//         .div,
//         .equal_equal,
//         .error_union,
//         .greater_or_equal,
//         .greater_than,
//         .less_or_equal,
//         .less_than,
//         .merge_error_sets,
//         .mod,
//         .mul,
//         .mul_wrap,
//         .field_access,
//         .switch_range,
//         .sub,
//         .sub_wrap,
//         .@"orelse",
//         .address_of,
//         .@"await",
//         .bit_not,
//         .bool_not,
//         .optional_type,
//         .negation,
//         .negation_wrap,
//         .@"resume",
//         .@"try",
//         .array_type,
//         .array_type_sentinel,
//         .ptr_type,
//         .ptr_type_aligned,
//         .ptr_type_bit_range,
//         .ptr_type_sentinel,
//         .slice_open,
//         .slice_sentinel,
//         .deref,
//         .unwrap_optional,
//         .array_access,
//         .@"return",
//         .@"break",
//         .@"continue",
//         .array_init,
//         .array_init_comma,
//         .array_init_dot,
//         .array_init_dot_comma,
//         .array_init_dot_two,
//         .array_init_dot_two_comma,
//         .array_init_one,
//         .array_init_one_comma,
//         .@"switch",
//         .switch_comma,
//         .switch_case,
//         .switch_case_one,
//         .@"for",
//         .for_simple,
//         .enum_literal,
//         .struct_init,
//         .struct_init_comma,
//         .struct_init_dot,
//         .struct_init_dot_comma,
//         .struct_init_dot_two,
//         .struct_init_dot_two_comma,
//         .struct_init_one,
//         .struct_init_one_comma,
//         .@"while",
//         .while_simple,
//         .while_cont,
//         .@"defer",
//         .@"if",
//         .if_simple,
//         .multiline_string_literal,
//         .block,
//         .block_semicolon,
//         .block_two,
//         .block_two_semicolon,
//         .error_set_decl,
//         => return,
//         .container_decl,
//         .container_decl_arg,
//         .container_decl_arg_trailing,
//         .container_decl_two,
//         .container_decl_two_trailing,
//         .tagged_union,
//         .tagged_union_trailing,
//         .tagged_union_enum_tag,
//         .tagged_union_enum_tag_trailing,
//         .tagged_union_two,
//         .tagged_union_two_trailing,
//         => {
//             var buf: [2]Ast.Node.Index = undefined;
//             for (ast.declMembers(tree, child, &buf)) |member|
//                 try addOutlineNodes(allocator, tree, member, context);
//             return;
//         },
//         else => {},
//     }
//     try getDocumentSymbolsInternal(allocator, tree, child, context);
// }

// var children = std.ArrayList(lsp.DocumentSymbol).init(allocator);

// var child_context = GetDocumentSymbolsContext{
//     .prev_loc = start_loc,
//     .symbols = &children,
// };

// if (ast.isContainer(tree, node)) {
//     var buf: [2]Ast.Node.Index = undefined;
//     for (ast.declMembers(tree, node, &buf)) |child|
//         try addOutlineNodes(allocator, tree, child, &child_context);
// }

// if (ast.varDecl(tree, node)) |var_decl| {
//     if (var_decl.ast.init_node != 0)
//         try addOutlineNodes(allocator, tree, var_decl.ast.init_node, &child_context);
// }

// if (tags[node] == .fn_decl) fn_ch: {
//     const fn_decl = tree.nodes.items(.data)[node];
//     var params: [1]Ast.Node.Index = undefined;
//     const fn_proto = ast.fnProto(tree, fn_decl.lhs, &params) orelse break :fn_ch;
//     if (!TypeWithHandle.isTypeFunction(tree, fn_proto)) break :fn_ch;
//     const ret_stmt = TypeWithHandle.findReturnStatement(tree, fn_proto, fn_decl.rhs) orelse break :fn_ch;
//     const type_decl = tree.nodes.items(.data)[ret_stmt].lhs;
//     if (type_decl != 0)
//         try addOutlineNodes(allocator, tree, type_decl, &child_context);
// }
