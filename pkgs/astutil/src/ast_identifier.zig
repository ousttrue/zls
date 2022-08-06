const std = @import("std");
const Ast = std.zig.Ast;
const AstContext = @import("./AstContext.zig");
const AstNode = @import("./AstNode.zig");
const AstToken = @import("./AstToken.zig");
const NodeReference = @import("./NodeReference.zig");
const Declaration = @import("./Declaration.zig");

pub const AstIdentifier = union(enum) {
    const Self = @This();

    reference: NodeReference,
    decl: Declaration,

    pub fn init(context: *const AstContext, token: AstToken) ?Self {
        std.debug.assert(token.getTag() == .identifier);
        return if (NodeReference.fromToken(context, token)) |reference|
            Self{ .reference = reference }
        else if (Declaration.fromToken(context, token)) |declaration|
            Self{ .decl = declaration }
        else
            null;
    }
};
