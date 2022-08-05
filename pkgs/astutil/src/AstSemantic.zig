const std = @import("std");
const AstContext = @import("./AstContext.zig");
const AstNode = @import("./AstNode.zig");
const AstToken = @import("./AstToken.zig");

var debug_buffer: [1024]u8 = undefined;

pub const Semantics = union(enum) {
    identifier,
    field_access,
    unknown,
};

const Self = @This();

token: AstToken,
node: AstNode,
semantics: Semantics,

pub fn init(context: *const AstContext, token: AstToken) Self {
    const node_idx = context.tokens_node[token.index];
    const node = AstNode.init(context, node_idx);
    return Self{
        .token = token,
        .node = node,
        .semantics = switch (node.getTag()) {
            // ref: 他の宣言を参照している。scope search
            // param: 関数引数
            // decl: 変数宣言
            .identifier => .identifier,
            // lhs
            // rhs(identifier)
            .field_access => .field_access,
            else => .unknown,
            // .enum_literal => {},
        },
    };
}

pub fn allocPrint(self: Self, allocator: std.mem.Allocator) ![]const u8 {
    // token / node
    const token = try self.token.allocPrint(allocator);
    defer allocator.free(token);
    const node = try self.node.allocPrint(allocator);
    defer allocator.free(node);
    return std.fmt.allocPrint(allocator, "`{s} => {s}`\n", .{ token, node });

    // const children = AstNodeIterator.NodeChildren.init(self.tree, node_idx, &u32_2);

    // var fixed_buffer = std.heap.FixedBufferAllocator.init(&debug_buffer);
    // const allocator = fixed_buffer.allocator();
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
}
