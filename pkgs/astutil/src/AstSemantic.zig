const std = @import("std");
const AstContext = @import("./AstContext.zig");
const AstNode = @import("./AstNode.zig");
const AstToken = @import("./AstToken.zig");

pub const AstSemantic = union(enum)
{
    identifier: AstNode,
    field_access: AstNode,
};

pub fn inspect(context: *const AstContext, token: AstToken) ?AstSemantic
{
    const node_idx = context.tokens_node[token.index];
    const node = AstNode.init(context, node_idx);

    return switch(node.getTag())
    {
        .identifier => AstSemantic{
            .identifier = node,
        },
        .field_access => AstSemantic{
            .field_access = node,
        },
        else => null,
    };
}
