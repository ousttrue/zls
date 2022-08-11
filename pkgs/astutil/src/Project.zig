const std = @import("std");
const ImportSolver = @import("./ImportSolver.zig");
const DocumentStore = @import("./DocumentStore.zig");
const Document = @import("./Document.zig");
const AstToken = @import("./AstToken.zig");
const AstNode = @import("./AstNode.zig");
const Declaration = @import("./declaration.zig").Declaration;
const logger = std.log.scoped(.Project);
const Self = @This();

import_solver: ImportSolver,
store: *DocumentStore,

pub fn init(import_solver: ImportSolver, store: *DocumentStore) Self {
    return Self{
        .import_solver = import_solver,
        .store = store,
    };
}

pub fn resolveFieldAccess(self: Self, node: AstNode) anyerror!AstNode {
    std.debug.assert(node.getTag() == .field_access);
    const data = node.getData();
    const lhs = AstNode.init(node.context, data.lhs);
    const rhs = AstToken.init(&node.context.tree, data.rhs);
    switch (lhs.getTag()) {
        .field_access => {
            // recursive
            return try self.resolveFieldAccess(lhs);
        },
        .identifier => {
            // get container,
            if (Declaration.find(lhs)) |decl| {
                switch (decl) {
                    .local => |local| {
                        if (self.resolveType(try local.getTypeNode())) |type_node| {
                            if (type_node.getMember(rhs.getText())) |field| {
                                return field;
                            } else {
                                return error.FieldNotFound;
                            }
                        } else {
                            return error.NoType;
                        }
                    },
                    .container => |container| {
                        if (self.resolveType(try container.getTypeNode())) |type_node| {
                            if (type_node.getMember(rhs.getText())) |field| {
                                return field;
                            } else {
                                return error.FieldNotFound;
                            }
                        } else {
                            return error.NoType;
                        }
                    },
                    .primitive => {
                        return error.PrimitiveFieldAccess;
                    },
                }
            } else {
                return error.NoDecl;
            }
        },
        else => {
            logger.err("unknown lhs: {}", .{lhs.getTag()});
            return error.UnknownLhs;
        },
    }
}

pub fn resolveType(self: Self, node: AstNode) ?AstNode {
    _ = self;
    _ = node;
    return null;
}
