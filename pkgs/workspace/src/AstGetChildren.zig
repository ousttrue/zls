const std = @import("std");
const Self = @This();

children: std.ArrayList(u32),

pub fn init(allocator: std.mem.Allocator) Self {
    return .{
        .children = std.ArrayList(u32).init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    self.children.deinit();
}

pub fn append(self: *Self, node: u32) void {
    if (node != 0) {
        self.children.append(node) catch unreachable;
    }
}

pub fn nodeData(self: *Self, data: std.zig.Ast.Node.Data) void {
    if (data.lhs != 0) {
        self.children.append(data.lhs) catch unreachable;
    }
    if (data.rhs != 0) {
        self.children.append(data.rhs) catch unreachable;
    }
}

pub fn items(self: Self) []const u32 {
    return self.children.items;
}

pub fn getChildren(self: *Self, tree: *const std.zig.Ast, idx: u32) []const u32 {
    const tag = tree.nodes.items(.tag);
    const node_tag = tag[idx];
    const data = tree.nodes.items(.data);
    const node_data = data[idx];

    switch (node_tag) {
        .root => {
            for (tree.rootDecls()) |child| {
                self.append(child);
            }
        },
        // .test_decl,
        .global_var_decl, .local_var_decl, .simple_var_decl, .aligned_var_decl => {
            self.nodeData(node_data);
        },
        .@"errdefer" => {
            self.nodeData(node_data);
        },
        .@"defer" => {
            self.append(node_data.rhs);
        },
        .@"catch" => {
            self.nodeData(node_data);
        },
        .field_access, .unwrap_optional => {
            self.append(node_data.lhs);
        },
        // == != < > <= >=
        .equal_equal, .bang_equal, .less_than, .greater_than, .less_or_equal, .greater_or_equal => {
            self.nodeData(node_data);
        },
        // *= /= %= += -= <<= <<|= >>= &= ^= |= *%= +%= -%= *|= +|= -|= =
        .assign_mul, .assign_div, .assign_mod, .assign_add, .assign_sub, .assign_shl, .assign_shl_sat, .assign_shr, .assign_bit_and, .assign_bit_xor, .assign_bit_or, .assign_mul_wrap, .assign_add_wrap, .assign_sub_wrap, .assign_mul_sat, .assign_add_sat, .assign_sub_sat, .assign => {
            self.nodeData(node_data);
        },
        // || * / % *% *| + - ++
        .merge_error_sets, .mul, .div, .mod, .array_mult, .mul_wrap, .mul_sat, .add, .sub, .array_cat => {
            self.nodeData(node_data);
        },
        // +% -% +| -| << <<| >>
        .add_wrap, .sub_wrap, .add_sat, .sub_sat, .shl, .shl_sat, .shr => {
            self.nodeData(node_data);
        },
        // & ^ | orelse and or
        .bit_and, .bit_xor, .bit_or, .@"orelse", .bool_and, .bool_or => {
            self.nodeData(node_data);
        },
        // !x -x
        .bool_not, .negation, .bit_not, .negation_wrap, .address_of, .@"try", .@"await", .optional_type => {
            self.append(node_data.lhs);
        },
        .array_type, .array_type_sentinel, .ptr_type_aligned, .ptr_type_sentinel, .ptr_type, .ptr_type_bit_range => {
            self.nodeData(node_data);
        },
        .slice => {
            const s = tree.slice(idx);
            self.append(s.ast.sliced);
            self.append(s.ast.start);
            self.append(s.ast.end);
            self.append(s.ast.sentinel);
        },
        .array_access, .array_init_one, .array_init_one_comma, .array_init_dot_two, .array_init_dot_two_comma => {
            self.nodeData(node_data);
        },
        .struct_init_one, .struct_init_one_comma, .struct_init_dot_two, .struct_init_dot_two_comma => {
            self.nodeData(node_data);
        },
        .call_one, .call_one_comma => {
            self.nodeData(node_data);
        },
        .fn_decl,
        .builtin_call_two,
        .block_two,
        .block_two_semicolon,
        .if_simple,
        .while_simple,
        .for_simple,
        .@"for",
        .container_field_init,
        .container_decl_two,
        .container_decl_two_trailing,
        => {
            self.nodeData(node_data);
        },
        .string_literal => {
            // leaf. no children
        },
        .deref, .@"return" => {
            self.append(node_data.lhs);
        },
        .call, .call_comma => {
            self.append(node_data.lhs);
            const call_full = tree.callFull(idx);
            for (call_full.ast.params) |child| {
                self.append(child);
            }
        },
        .block, .block_semicolon => {
            for (tree.extra_data[node_data.lhs..node_data.rhs]) |child| {
                self.append(child);
            }
        },
        .@"if" => {
            // const extra = tree.extraData(node_data.rhs, Node.If);
            const if_full = tree.ifFull(idx);
            self.append(if_full.ast.cond_expr);
            self.append(if_full.ast.then_expr);
            self.append(if_full.ast.else_expr);
        },
        .@"switch",
        .switch_comma,
        => {
            self.append(node_data.lhs);
            const extra = tree.extraData(node_data.rhs, std.zig.Ast.Node.SubRange);
            for (tree.extra_data[extra.start..extra.end]) |child| {
                self.append(child);
            }
        },
        .switch_case => {
            const switch_case = tree.switchCase(idx);
            for (switch_case.ast.values) |child| {
                self.append(child);
            }
            self.append(switch_case.ast.target_expr);
        },
        .switch_case_one => {
            const switch_case = tree.switchCaseOne(idx);
            for (switch_case.ast.values) |child| {
                self.append(child);
            }
            self.append(switch_case.ast.target_expr);
        },
        .while_cont => {
            self.append(node_data.lhs);
            const extra = tree.extraData(node_data.rhs, std.zig.Ast.Node.WhileCont);
            self.append(extra.cont_expr);
            self.append(extra.then_expr);
        },
        .struct_init_dot_comma => {
            const struct_init = tree.structInitDot(idx);
            self.append(struct_init.ast.type_expr);
            for (struct_init.ast.fields) |child| {
                self.append(child);
            }
        },
        .struct_init, .struct_init_comma => {
            const struct_init = tree.structInit(idx);
            self.append(struct_init.ast.type_expr);
            for (struct_init.ast.fields) |child| {
                self.append(child);
            }
        },
        .array_init, .array_init_comma => {
            const array_init = tree.arrayInit(idx);
            self.append(array_init.ast.type_expr);
            for (array_init.ast.elements) |child| {
                self.append(child);
            }
        },
        .array_init_dot, .array_init_dot_comma => {
            const array_init = tree.arrayInitDot(idx);
            self.append(array_init.ast.type_expr);
            for (array_init.ast.elements) |child| {
                self.append(child);
            }
        },
        .fn_proto_multi => {
            const fn_proto = tree.fnProtoMulti(idx);
            for (fn_proto.ast.params) |child| {
                self.append(child);
            }
            self.append(fn_proto.ast.return_type);
        },
        .fn_proto_simple => {
            var buf: [1]std.zig.Ast.Node.Index = undefined;
            const fn_proto = tree.fnProtoSimple(&buf, idx);
            for (fn_proto.ast.params) |child| {
                self.append(child);
            }
            self.append(fn_proto.ast.return_type);
        },
        .container_decl, .container_decl_trailing => {
            const decl = tree.containerDecl(idx);
            for (decl.ast.members) |child| {
                self.append(child);
            }
        },
        .container_decl_arg, .container_decl_arg_trailing => {
            const decl = tree.containerDeclArg(idx);
            for (decl.ast.members) |child| {
                self.append(child);
            }
        },
        .tagged_union, .tagged_union_trailing => {
            const decl = tree.taggedUnion(idx);
            for (decl.ast.members) |child| {
                self.append(child);
            }
        },
        .grouped_expression => {
            self.append(node_data.lhs);
        },
        .error_union => {
            self.nodeData(node_data);
        },
        else => {
            // std.debug.print("unknown node: {s}\n", .{@tagName(node_tag)});
        },
    }

    return self.children.items;
}

pub fn getChild(allocator: std.mem.Allocator, tree: *const std.zig.Ast, idx: u32) u32 {
    var children = Self.init(allocator);
    defer children.deinit();
    for (children.getChildren(tree, idx)) |child| {
        return child;
    }
    unreachable;
}
