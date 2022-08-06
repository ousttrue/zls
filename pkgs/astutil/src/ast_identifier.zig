const std = @import("std");
const Ast = std.zig.Ast;
const AstContext = @import("./AstContext.zig");
const AstNode = @import("./AstNode.zig");
const AstToken = @import("./AstToken.zig");
const NodeReference = @import("./NodeReference.zig");
const ContainerVar = @import("./ContainerVar.zig");
const LocalVar = @import("./LocalVar.zig");

pub const AstIdentifier = union(enum) {
    const Self = @This();

    reference: NodeReference,
    container: ContainerVar,
    local: LocalVar,

    pub fn init(context: *const AstContext, token: AstToken) ?Self {
        std.debug.assert(token.getTag() == .identifier);
        return if (NodeReference.fromToken(context, token)) |reference|
            Self{ .reference = reference }
        else if (ContainerVar.fromToken(context, token)) |container_var|
            Self{ .container = container_var }
        else if (LocalVar.fromToken(context, token)) |local_var|
            Self{ .local = local_var }
        else
            null;
    }
};
