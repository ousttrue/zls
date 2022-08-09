const std = @import("std");
const Ast = std.zig.Ast;
const AstToken = @import("./AstToken.zig");
const AstNode = @import("./AstNode.zig");
const AstContext = @import("./AstContext.zig");
const Declaration = @import("./Declaration.zig");
const Primitive = @import("./Primitive.zig");
const ImportSolver = @import("./ImportSolver.zig");
const DocumentStore = @import("./DocumentStore.zig");
const logger = std.log.scoped(.VarType);

const Self = @This();

node: AstNode,
kind: union(enum) {
    import: ImportSolver.ImportType,
    this,
    builtin,
    call,
    array,
    ptr,
    optional,
    error_union,
    enum_literal,
    container,
    enum_type,
    primitive: Primitive,
    unknown,
} = .unknown,

pub fn init(import_solver: ImportSolver, store: *DocumentStore, node: AstNode) anyerror!Self {
    var buf: [2]u32 = undefined;
    const children = node.getChildren(&buf);
    switch (children) {
        .builtin_call => |builtin_call| {
            const builtin = node.getMainToken().getText();
            if (std.mem.eql(u8, builtin, "@import")) {
                const param = AstNode.init(node.context, builtin_call.ast.params[0]);
                const text = param.getMainToken().getText();
                if (std.mem.endsWith(u8, text, ".zig\"")) {
                    return Self{
                        .node = node,
                        .kind = .{
                            .import = .{
                                .file = text,
                            },
                        },
                    };
                } else {
                    return Self{
                        .node = node,
                        .kind = .{
                            .import = .{
                                .pkg = text,
                            },
                        },
                    };
                }
            } else if (std.mem.eql(u8, builtin, "@This")) {
                // TODO: eval
                return Self{
                    .node = node,
                    .kind = .this,
                };
            } else {
                return Self{
                    .node = node,
                    .kind = .builtin,
                };
            }
        },
        .call => {
            // TODO: eval
            return Self{
                .node = node,
                .kind = .call,
            };
        },
        .array_type => {
            return Self{
                .node = node,
                .kind = .array,
            };
        },
        .ptr_type => {
            // TODO: deref
            return Self{
                .node = node,
                .kind = .ptr,
            };
        },
        .var_decl => |var_decl| {
            return fromVarDecl(import_solver, store, node.context, var_decl);
        },
        .container_decl => |container_decl| {
            const token = AstToken.init(&node.context.tree, container_decl.ast.main_token);
            if (std.mem.eql(u8, token.getText(), "enum")) {
                return Self{
                    .node = node,
                    .kind = .enum_type,
                };
            } else {
                return Self{
                    .node = node,
                    .kind = .container,
                };
            }
        },
        else => {
            switch (node.getTag()) {
                .identifier => {
                    const token = node.getMainToken();
                    if (Primitive.fromName(token.getText())) |primitive| {
                        return Self{
                            .node = node,
                            .kind = .{ .primitive = primitive },
                        };
                    } else if (Declaration.findFromContainer(node)) |decl| {
                        // deref
                        // return Self{
                        //     .node = node,
                        //     .kind = .{ .ref = decl },
                        // };
                        return init(import_solver, store, AstNode.fromTokenIndex(node.context, decl.token.index));
                    } else {
                        // try w.print("no ref: {s}", .{node.getMainToken().getText()});
                    }
                },
                .optional_type => {
                    return Self{
                        .node = node,
                        .kind = .optional,
                    };
                },
                .field_access => {
                    // resolve field_access
                    var data = node.getData();
                    var lhs = AstNode.init(node.context, data.lhs);
                    var var_type = try init(import_solver, store, lhs);
                    var rhs = AstToken.init(&node.context.tree, data.rhs);
                    if (try var_type.getMember(import_solver, store, rhs.getText())) |member| {
                        return init(import_solver, store, member.getNode());
                    } else {
                        logger.err("fail to getMember", .{});
                    }
                },
                .error_union => {
                    return Self{
                        .node = node,
                        .kind = .error_union,
                    };
                },
                else => {
                    logger.err("unknown node tag: {s}", .{@tagName(node.getTag())});
                },
            }
        },
    }

    return Self{
        .node = node,
    };
}

pub fn fromVarDecl(
    import_solver: ImportSolver,
    store: *DocumentStore,
    context: *const AstContext,
    var_decl: Ast.full.VarDecl,
) !Self {
    const node_idx = if (var_decl.ast.type_node != 0)
        var_decl.ast.type_node
    else
        var_decl.ast.init_node;
    return try init(import_solver, store, AstNode.init(context, node_idx));
}

pub fn fromParam(
    import_solver: ImportSolver,
    store: *DocumentStore,
    context: *const AstContext,
    param: Ast.full.FnProto.Param,
) !Self {
    const node_idx = param.type_expr;
    return try init(import_solver, store, AstNode.init(context, node_idx));
}

pub fn fromFnProtoReturn(
    import_solver: ImportSolver,
    store: *DocumentStore,
    context: *const AstContext,
    fn_proto: Ast.full.FnProto,
) !Self {
    const node_idx = fn_proto.ast.return_type;
    return try init(import_solver, store, AstNode.init(context, node_idx));
}

pub fn fromContainerField(
    import_solver: ImportSolver,
    store: *DocumentStore,
    context: *const AstContext,
    field: Ast.full.ContainerField,
) !Self {
    const node_idx = field.ast.type_expr;
    const node = AstNode.init(context, node_idx);
    if (node_idx == 0) {
        // enum member ?
        return Self{
            .node = node,
            .kind = .enum_literal,
        };
    } else {
        return try init(import_solver, store, node);
    }
}

pub fn getMember(
    self: Self,
    import_solver: ImportSolver,
    store: *DocumentStore,
    name: []const u8,
) anyerror!?AstNode.Member {
    switch (self.kind) {
        .container => {
            var buf: [2]u32 = undefined;
            return self.node.getMember(name, &buf);
        },
        .import => |import| {
            // resolve import
            if (import_solver.solve(self.node.context.path, import)) |path| {
                if (try store.getOrLoad(path)) |imported| {
                    const root = AstNode.init(imported.ast_context, 0);
                    var buf: [2]u32 = undefined;
                    return root.getMember(name, &buf);
                } else {
                    return null;
                }
            } else {
                return null;
            }
        },
        else => {
            logger.err("getMember: unknown {s}", .{@tagName(self.kind)});
            return null;
        },
    }
}

pub fn allocPrint(self: Self, allocator: std.mem.Allocator) ![]const u8 {
    var buffer = std.ArrayList(u8).init(allocator);
    const w = buffer.writer();

    switch (self.kind) {
        // .import=> {
        // },
        // .this=> {},
        // .builtin=> {},
        // .call=> {},
        // .array=> {},
        // .ptr=> {},
        // .container=> {},
        // .primitive=> {}: Primitive,
        // .ref=> {}: Declaration,
        .unknown => {
            try w.print("unknown: node tag = {s}", .{@tagName(self.node.getTag())});
        },
        .import => |import| {
            switch (import) {
                .pkg => |pkg| {
                    try w.print("@import pkg {s}", .{pkg});
                },
                .file => |file| {
                    try w.print("@import file {s}", .{file});
                },
            }
        },
        else => {
            try w.print("else {s}", .{@tagName(self.kind)});
        },
    }

    return buffer.items;
}
