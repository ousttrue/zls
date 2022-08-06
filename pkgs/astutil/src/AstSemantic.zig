const std = @import("std");
const AstContext = @import("./AstContext.zig");
const AstNode = @import("./AstNode.zig");
const AstToken = @import("./AstToken.zig");
const LocalVar = @import("./LocalVar.zig");
const Declaration = @import("./Declaration.zig");

var debug_buffer: [1024]u8 = undefined;

pub const Semantics = union(enum) {
    local: LocalVar,
    decl: Declaration,
    field_access,
    unknown,

    fn fromIdentifierNode(node: AstNode) @This() {
        if (LocalVar.find(node)) |local| {
            return Semantics{ .local = local };
        } else if (Declaration.find(node)) |decl| {
            return Semantics{ .decl = decl };
        } else {
            return .unknown;
        }
    }
};

const Self = @This();

token: AstToken,
node: AstNode,
reference: Semantics,

pub fn init(context: *const AstContext, token: AstToken) Self {
    const node_idx = context.tokens_node[token.index];
    const node = AstNode.init(context, node_idx);
    return Self{
        .token = token,
        .node = node,
        .reference = switch (node.getTag()) {
            .identifier => Semantics.fromIdentifierNode(node),
            .field_access => .field_access,
            // .enum_literal => {},
            else => .unknown,
        },
    };
}

pub fn allocPrint(self: Self, allocator: std.mem.Allocator) ![]const u8 {
    var buffer = std.ArrayList(u8).init(allocator);
    const w = buffer.writer();

    const token = try self.token.allocPrint(allocator);
    defer allocator.free(token);
    const node = try self.node.allocPrint(allocator);
    defer allocator.free(node);
    try w.print("`{s} => {s}`\n", .{ node, token });

    switch (self.reference) {
        .local => |local| {
            try w.print("## identifier\n\n", .{});
            const info = try local.allocPrint(allocator);
            defer allocator.free(info);
            try w.print("{s}", .{info});
        },
        .decl => |decl| {
            try w.print("## identifier\n\n", .{});
            const info = try decl.allocPrint(allocator);
            defer allocator.free(info);
            try w.print("{s}", .{info});
        },
        else => {
            try w.print("not found", .{});
        },
    }

    // identifier => getDecl
    // field_access => getLhs identifier / field_access

    // switch (self) {
    //     .identifier => |value| {
    //         return std.fmt.allocPrint(
    //             allocator,
    //             "identifier: {s}",
    //             .{value.getMainToken().getText()},
    //         ) catch unreachable;
    //     },
    //     .field_access => |value| {
    //         const data = value.getData();
    //         const rhs = AstToken.init(&value.context.tree, data.rhs);
    //         return std.fmt.allocPrint(
    //             allocator,
    //             "field_access.{s}",
    //             .{rhs.getText()},
    //         ) catch unreachable;
    //     },
    //     .unknown => |value| {
    //         return std.fmt.allocPrint(
    //             allocator,
    //             "{s}",
    //             .{@tagName(value.getTag())},
    //         ) catch unreachable;
    //     },
    // }

    return buffer.items;
}
