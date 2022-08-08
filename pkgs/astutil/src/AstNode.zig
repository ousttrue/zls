const std = @import("std");
const Ast = std.zig.Ast;
const AstContext = @import("./AstContext.zig");
const AstToken = @import("./AstToken.zig");
const AstNodeIterator = @import("./AstNodeIterator.zig");
const logger = std.log.scoped(.AstNode);
const Self = @This();

context: *const AstContext,
index: u32,

pub fn init(context: *const AstContext, index: u32) Self {
    return Self{
        .context = context,
        .index = index,
    };
}

pub fn fromTokenIndex(context: *const AstContext, token_idx: u32) Self {
    const idx = context.tokens_node[token_idx];
    return init(context, idx);
}

fn printRec(self: Self, w: anytype) std.mem.Allocator.Error!void {
    var buffer: [2]u32 = undefined;
    const children = self.getChildren(&buffer);
    switch (children) {
        .container_decl, .block => {},
        else => {
            if (self.getParent()) |parent| {
                try parent.printRec(w);
                try w.print("/", .{});
            }
        },
    }
    switch (children) {
        .container_decl => try w.print("<{s}>", .{@tagName(self.getTag())}),
        .block => try w.print("[{s}]", .{@tagName(self.getTag())}),
        else => try w.print("{s}", .{@tagName(self.getTag())}),
    }
}

pub fn allocPrint(self: Self, allocator: std.mem.Allocator) ![]const u8 {
    var buffer = std.ArrayList(u8).init(allocator);
    const w = buffer.writer();
    try self.printRec(w);
    return buffer.items;
}

pub fn getTag(self: Self) Ast.Node.Tag {
    const tag = self.context.tree.nodes.items(.tag);
    return tag[self.index];
}

pub fn getData(self: Self) Ast.Node.Data {
    const data = self.context.tree.nodes.items(.data);
    return data[self.index];
}

pub fn getMainToken(self: Self) AstToken {
    const main_token = self.context.tree.nodes.items(.main_token);
    return AstToken.init(&self.context.tree, main_token[self.index]);
}

pub fn getChildren(self: Self, buffer: []u32) AstNodeIterator.NodeChildren {
    return AstNodeIterator.NodeChildren.init(self.context.tree, self.index, buffer);
}

pub fn isChildrenType(self: Self, childrenType: AstNodeIterator.NodeChildren) bool {
    var buffer: [2]u32 = undefined;
    const children = self.getChildren(&buffer);
    return children == childrenType;
}

pub fn getFnProto(self: Self, buffer: []u32) ?Ast.full.FnProto {
    const children = self.getChildren(buffer);
    return switch (children) {
        .fn_proto => |fn_proto| fn_proto,
        else => null,
    };
}

pub const Member = union(enum) {
    field: Ast.full.ContainerField,
    fn_decl: Self,
    var_decl: Ast.full.VarDecl,
};

pub fn getMember(self: Self, name: []const u8, buffer: []u32) ?Member {
    var buf: [2]u32 = undefined;
    switch (self.getChildren(&buf)) {
        .container_decl => |container_decl| {
            for (container_decl.ast.members) |member| {
                const child_node = init(self.context, member);
                switch (child_node.getChildren(buffer)) {
                    .container_field => |container_field| {
                        const token = AstToken.init(&self.context.tree, container_field.ast.name_token);
                        logger.debug("{s} <=> {s}", .{ token.getText(), name });
                        if (std.mem.eql(u8, token.getText(), name)) {
                            return Member{ .field = container_field };
                        }
                    },
                    .var_decl => |var_decl| {
                        return Member{ .var_decl = var_decl };
                    },
                    else => {
                        if (child_node.getTag() == .fn_decl) {
                            return Member{ .fn_decl = child_node };
                        } else {
                            logger.err("not field and fn: {s}", .{@tagName(child_node.getTag())});
                        }
                    },
                }
            }
        },
        else => {
            logger.err("not container", .{});
        },
    }
    return null;
}

pub fn getContainerDecl(self: Self, buffer: []u32) ?Ast.full.ContainerDecl {
    const children = self.getChildren(buffer);
    return switch (children) {
        .container_decl => |container_decl| container_decl,
        else => null,
    };
}

pub fn getParent(self: Self) ?Self {
    if (self.index == 0) {
        return null;
    }
    const index = self.context.nodes_parent[self.index];
    return init(self.context, index);
}

pub const Iterator = struct {
    current: ?Self,

    pub fn next(self: *@This()) void {
        if (self.current) |current| {
            self.current = current.getParent();
        }
    }
};

pub fn parentIterator(self: Self) Iterator {
    return Iterator{ .current = self };
}

test {
    const source =
        \\pub fn main() !void {
        \\    
        \\}
    ;
    const allocator = std.testing.allocator;
    const text: [:0]const u8 = try allocator.dupeZ(u8, source);
    defer allocator.free(text);

    const context = try AstContext.new(allocator, text);
    defer context.delete();

    const node = fromTokenIndex(context, 0);
    try std.testing.expectEqual(node.getTag(), .fn_proto_simple);

    const parent_node = node.parent().?;
    try std.testing.expectEqual(parent_node.getTag(), .fn_decl);

    const root_node = parent_node.parent().?;
    try std.testing.expectEqual(root_node.getTag(), .root);
}
