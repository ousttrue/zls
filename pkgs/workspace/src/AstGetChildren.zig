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
        .string_literal => {
            // leaf. no children
        },
        .simple_var_decl,
        .fn_decl,
        .builtin_call_two,
        .block_two,
        .block_two_semicolon,
        .@"catch",
        .if_simple,
        .while_simple,
        .for_simple,
        .call_one,
        .call_one_comma,
        => {
            self.nodeData(node_data);
        },
        .@"try",
        .@"return",
        .field_access,
        => {
            self.append(node_data.lhs);
        },
        .call => {
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
        .struct_init_dot_comma => {
            const struct_init = tree.structInitDot(idx);
            for (struct_init.ast.fields) |child| {
                self.append(child);
            }
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
