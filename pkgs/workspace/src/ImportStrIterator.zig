const std = @import("std");
const Ast = std.zig.Ast;
const ast = @import("./ast.zig");
const Self = @This();

const Payload = union(enum) {
    decl: Ast.Node.Index,
    decls: []const Ast.Node.Index,
};

const Item = struct {
    payload: Payload,
    pos: usize = 0,

    fn node(self: Item) Ast.Node.Index {
        return switch (self.payload) {
            .decls => |decls| return decls[self.pos],
            .decl => |decl| return decl,
        };
    }

    fn next(self: Item) ?Item {
        switch (self.payload) {
            .decls => |decls| {
                if (self.pos + 1 < decls.len) {
                    return Item{ .payload = .{ .decls = decls }, .pos = self.pos + 1 };
                }
                return null;
            },
            .decl => return null,
        }
    }
};

tree: Ast,
stack: [256]Item = undefined,
depth: usize,

pub fn init(tree: Ast) @This() {
    var self = @This(){
        .tree = tree,
        .depth = 0,
    };
    self.push(.{ .payload = .{ .decl = 0 } });
    return self;
}

fn push(self: *@This(), item: Item) void {
    self.stack[self.depth] = item;
    self.depth += 1;
}

fn pop(self: *@This()) Item {
    const node = self.stack[self.depth - 1];
    self.depth -= 1;
    return node;
}

pub fn next(self: *@This()) ?u32 {
    const node_tags = self.tree.nodes.items(.tag);
    var buf: [2]Ast.Node.Index = undefined;
    while (self.depth > 0) {
        const item = self.pop();
        if (item.next()) |next_item| {
            self.push(next_item);
        }

        const node = item.node();

        if (ast.isContainer(self.tree, node)) {
            const decls = ast.declMembers(self.tree, node, &buf);
            if (decls.len > 0) {
                self.push(.{ .payload = .{ .decls = decls } });
            }
        } else if (ast.varDecl(self.tree, node)) |var_decl| {
            self.push(.{ .payload = .{ .decl = var_decl.ast.init_node } });
        } else if (node_tags[node] == .@"usingnamespace") {
            self.push(.{ .payload = .{ .decl = self.tree.nodes.items(.data)[node].lhs } });
        }

        if (ast.isBuiltinCall(self.tree, node)) {
            const builtin_token = self.tree.nodes.items(.main_token)[node];
            const call_name = self.tree.tokenSlice(builtin_token);
            if (std.mem.eql(u8, call_name, "@import")) {
                return node;
            }
        }
    }

    return null;
}
