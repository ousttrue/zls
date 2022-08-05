const std = @import("std");
const AstContext = @import("./AstContext.zig");
const AstNode = @import("./AstNode.zig");
const AstToken = @import("./AstToken.zig");

pub const AstSemantic = union(enum) {
    const Self = @This();

    identifier: AstNode,
    field_access: AstNode,
    unknown: AstNode,

    var debug_buffer: [1024]u8 = undefined;
    pub fn debugInfo(self: Self) []const u8 {
        var fixed_buffer = std.heap.FixedBufferAllocator.init(&debug_buffer);
        const allocator = fixed_buffer.allocator();
        switch (self) {
            .identifier => |value| {
                return std.fmt.allocPrint(
                    allocator,
                    "identifier: {s}",
                    .{value.getMainToken().getText()},
                ) catch unreachable;
            },
            .field_access => |value| {
                const data = value.getData();
                const rhs = AstToken.init(&value.context.tree, data.rhs);
                return std.fmt.allocPrint(
                    allocator,
                    "field_access.{s}",
                    .{rhs.getText()},
                ) catch unreachable;
            },
            .unknown => |value| {
                return std.fmt.allocPrint(
                    allocator,
                    "{s}",
                    .{@tagName(value.getTag())},
                ) catch unreachable;
            },
        }
    }
};

pub fn inspect(context: *const AstContext, token: AstToken) AstSemantic {
    const node_idx = context.tokens_node[token.index];
    const node = AstNode.init(context, node_idx);

    return switch (node.getTag()) {
        .identifier => AstSemantic{
            // ref: 他の宣言を参照している。scope search
            // param: 関数引数
            // decl: 変数宣言

            .identifier = node,
        },
        .field_access => AstSemantic{
            // lhs
            // rhs(identifier)
            .field_access = node,
        },
        else => AstSemantic{
            .unknown = node,
        },
    };
}
