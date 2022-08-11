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

pub fn resolveImport(self: Self, node: AstNode) !?AstNode {
    var buf: [2]u32 = undefined;
    switch (node.getChildren(&buf)) {
        .builtin_call => |full| {
            if (std.mem.eql(u8, node.getMainToken().getText(), "@import")) {
                if (full.ast.params.len == 1) {
                    const text = AstNode.init(node.context, full.ast.params[0]).getMainToken().getText();
                    if (self.import_solver.solve(node.context.path, text)) |path| {
                        if (try self.store.getOrLoad(path)) |doc| {
                            // root node
                            return AstNode.init(doc.ast_context, 0);
                        } else {
                            return error.DocumentNotFound;
                        }
                    } else {
                        return error.FailPath;
                    }
                } else {
                    return error.InalidParams;
                }
            } else {
                return error.NotImport;
            }
        },
        else => {
            return error.NotBuiltinCall;
        },
    }
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
            const type_node = try self.resolveType(lhs);
            if (type_node.getMember(rhs.getText())) |field| {
                return field;
            } else {
                return error.FieldNotFound;
            }
        },
        else => {
            logger.err("unknown lhs: {}", .{lhs.getTag()});
            return error.UnknownLhs;
        },
    }
}

pub fn resolveType(self: Self, node: AstNode) anyerror!AstNode {
    _ = self;
    var buf: [2]u32 = undefined;
    switch (node.getChildren(&buf)) {
        .container_decl => {
            return node;
        },
        .builtin_call => {
            const builtin_name = node.getMainToken().getText();
            if (std.mem.eql(u8, builtin_name, "@import")) {
                if (try self.resolveImport(node)) |imported| {
                    return imported;
                } else {
                    return error.FailImport;
                }
            } else if (std.mem.eql(u8, builtin_name, "@This")) {
                //
                if (node.getContainerNodeForThis()) |container| {
                    return container;
                } else {
                    return error.NoConainerDecl;
                }
            } else {
                logger.err("{s}", .{builtin_name});
                return error.UnknownBuiltin;
            }
        },
        else => {
            switch (node.getTag()) {
                .identifier => {
                    if (Declaration.find(node)) |decl| {
                        switch (decl) {
                            .local => |local| {
                                return self.resolveType(try local.getTypeNode());
                            },
                            .container => |container| {
                                return self.resolveType(try container.getTypeNode());
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
                    node.debugPrint();
                    return error.ResovleType;
                },
            }
        },
    }
}
