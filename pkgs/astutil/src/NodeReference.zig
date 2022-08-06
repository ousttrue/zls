///
/// 変数参照。コンテナ変数、関数引数、ローカル変数以外の変数。参照先を示す。
///
const std = @import("std");
const AstContext = @import("./AstContext.zig");
const AstNode = @import("./AstNode.zig");
const AstToken = @import("./AstToken.zig");
const LocalVar = @import("./LocalVar.zig");
const ContainerVar = @import("./ContainerVar.zig");

var debug_buffer: [1024]u8 = undefined;

pub const Reference = union(enum) {
    local: LocalVar,
    decl: ContainerVar,

    pub fn fromIdentifierNode(node: AstNode) ?@This() {
        return if (LocalVar.find(node)) |local|
            @This(){ .local = local }
        else if (ContainerVar.find(node)) |decl|
            @This(){ .decl = decl }
        else
            null;
    }
};

const Self = @This();

token: AstToken,
node: AstNode,
target: Reference,

pub fn fromToken(context: *const AstContext, token: AstToken) ?Self {
    const node_idx = context.tokens_node[token.index];
    const node = AstNode.init(context, node_idx);
    return if (Reference.fromIdentifierNode(node)) |reference|
        Self{
            .token = token,
            .node = node,
            .target = reference,
        }
    else
        null;
}

pub fn allocPrint(self: Self, allocator: std.mem.Allocator) ![]const u8 {
    var buffer = std.ArrayList(u8).init(allocator);
    const w = buffer.writer();

    const token = try self.token.allocPrint(allocator);
    defer allocator.free(token);
    const node = try self.node.allocPrint(allocator);
    defer allocator.free(node);
    try w.print("`{s} => {s}`\n", .{ node, token });

    switch (self.target) {
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
