const std = @import("std");
const Ast = std.zig.Ast;
const AstContext = @import("./AstContext.zig");
const AstNode = @import("./AstNode.zig");
const AstToken = @import("./AstToken.zig");
const AstNodeReference = @import("./AstNodeReference.zig");

pub const AstIdentifier = union(enum) {
    const Self = @This();

    reference: AstNodeReference,
    local,
    container,

    pub fn init(context: *const AstContext, token: AstToken) ?Self {
        std.debug.assert(token.getTag() == .identifier);

        const node = AstNode.init(context, context.tokens_node[token.index]);
        var buf: [2]u32 = undefined;
        switch (node.getChildren(&buf)) {
            else => {
                switch (node.getTag()) {
                    .identifier => {
                        if (AstNodeReference.init(context, token)) |reference| {
                            return Self{
                                .reference = reference,
                            };
                        }
                    },
                    else => {},
                }
            },
        }

        return null;
    }
};
