const std = @import("std");
const Ast = std.zig.Ast;
const AstToken = @import("./AstToken.zig");
const AstNode = @import("./AstNode.zig");
const AstContext = @import("./AstContext.zig");
const Declaration = @import("./Declaration.zig");
const PrimitiveType = @import("./primitives.zig").PrimitiveType;
const Project = @import("./Project.zig");
const ImportSolver = @import("./ImportSolver.zig");
const logger = std.log.scoped(.VarType);

const Self = @This();

node: AstNode,
kind: union(enum) {
    import: []const u8,
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
    primitive: PrimitiveType,
    unknown,
} = .unknown,

pub fn init(project: ?Project, node: AstNode) anyerror!Self {
    var buf: [2]u32 = undefined;
    const children = node.getChildren(&buf);
    switch (children) {
        .builtin_call => |builtin_call| {
            const builtin = node.getMainToken().getText();
            if (std.mem.eql(u8, builtin, "@import")) {
                const param = AstNode.init(node.context, builtin_call.ast.params[0]);
                const text = param.getMainToken().getText();
                return Self{
                    .node = node,
                    .kind = .{
                        .import = text,                            
                    },
                };
            } else if (std.mem.eql(u8, builtin, "@This")) {
                if (node.getContainerNodeForThis()) |container_node| {
                    return try init(project, container_node);
                }
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
            return fromVarDecl(project, node.context, var_decl);
        },
        .container_decl => |container_decl| {
            if (node.index == 0) {
                // root
                return Self{
                    .node = node,
                    .kind = .container,
                };
            }

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
                    if (PrimitiveType.fromName(token.getText())) |primitive| {
                        return Self{
                            .node = node,
                            .kind = .{ .primitive = primitive },
                        };
                    } else if (Declaration.findFromBlock(node)) |decl| {
                        return init(project, AstNode.fromTokenIndex(node.context, decl.token.index));
                    } else if (Declaration.findFromContainer(node)) |decl| {
                        return init(project, AstNode.fromTokenIndex(node.context, decl.token.index));
                    } else {
                        logger.warn("no ref: {s}", .{node.getMainToken().getText()});
                    }
                },
                .optional_type => {
                    return Self{
                        .node = node,
                        .kind = .optional,
                    };
                },
                .field_access => {
                    var data = node.getData();
                    // lhs
                    var lhs = AstNode.init(node.context, data.lhs);
                    var var_type = try init(project, lhs);
                    // rhs
                    var rhs = AstToken.init(&node.context.tree, data.rhs);
                    if (try var_type.getMember(project, rhs.getText())) |member| {
                        return init(project, member);
                    } else {
                        logger.err("fail to field_access.getMember", .{});
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
    project: ?Project,
    context: *const AstContext,
    var_decl: Ast.full.VarDecl,
) !Self {
    const node_idx = if (var_decl.ast.type_node != 0)
        var_decl.ast.type_node
    else
        var_decl.ast.init_node;
    return try init(project, AstNode.init(context, node_idx));
}

pub fn fromParam(
    project: ?Project,
    context: *const AstContext,
    param: Ast.full.FnProto.Param,
) !Self {
    const node_idx = param.type_expr;
    return try init(project, AstNode.init(context, node_idx));
}

pub fn fromFnProtoReturn(
    project: ?Project,
    context: *const AstContext,
    fn_proto: Ast.full.FnProto,
) !Self {
    const node_idx = fn_proto.ast.return_type;
    return try init(project, AstNode.init(context, node_idx));
}

pub fn fromContainerField(
    project: ?Project,
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
        return try init(project, node);
    }
}

pub fn getMember(
    self: Self,
    project: ?Project,
    name: []const u8,
) anyerror!?AstNode {
    switch (self.kind) {
        .container => {
            return self.node.getMember(name);
        },
        .import => |import| {
            // resolve import
            if (project) |p| {
                if (p.import_solver.solve(self.node.context.path, import)) |path| {
                    if (try p.store.getOrLoad(path)) |imported| {
                        const root = AstNode.init(imported.ast_context, 0);
                        return root.getMember(name);
                    } else {
                        return null;
                    }
                }
            }
            return null;
        },
        else => {
            logger.err(
                "getMember: {s} => {s}: {s}",
                .{
                    @tagName(self.kind),
                    @tagName(self.node.getTag()),
                    self.node.getMainToken().getText(),
                },
            );
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
            try w.print("@import {s}", .{import});            
        },
        else => {
            try w.print("{s}", .{@tagName(self.kind)});
        },
    }

    return buffer.items;
}

test "@This" {
    const source =
        \\const Self = @This();
        \\
        \\value: u32 = 0,
        \\
        \\fn init() Self
        \\{
        \\    return .{};
        \\}
        \\
        \\fn get(self: Self) u32
        \\{
        \\    return self.value;
        \\}
    ;

    const allocator = std.testing.allocator;
    const text: [:0]const u8 = try allocator.dupeZ(u8, source);
    defer allocator.free(text);

    const context = try AstContext.new(allocator, .{}, text);
    defer context.delete();

    {
        const node = AstNode.fromTokenIndex(context, 3);
        var buf: [2]u32 = undefined;
        try std.testing.expect(node.getChildren(&buf) == .builtin_call);

        var var_type = try Self.init(null, AstNode.fromTokenIndex(context, 1));
        try std.testing.expectEqual(var_type.kind, .container);
        try std.testing.expectEqual(var_type.node.index, 0);
    }

    {
        const token = AstToken.init(&context.tree, context.tree.tokens.len - 6);
        try std.testing.expectEqualStrings(token.getText(), "self");
        var var_type = try Self.init(null, AstNode.fromTokenIndex(context, token.index));
        try std.testing.expectEqual(var_type.kind, .container);
    }

    {
        const token = AstToken.init(&context.tree, context.tree.tokens.len - 4);
        try std.testing.expectEqualStrings(token.getText(), "value");
        var var_type = try Self.init(null, AstNode.fromTokenIndex(context, token.index));
        try std.testing.expectEqual(var_type.kind, .{ .primitive = .{ .kind = .u32 } });
    }
}
