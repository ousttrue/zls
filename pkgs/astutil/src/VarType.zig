const std = @import("std");
const Ast = std.zig.Ast;
const AstToken = @import("./AstToken.zig");
const AstNode = @import("./AstNode.zig");
const AstContext = @import("./AstContext.zig");
const Declaration = @import("./Declaration.zig");
const Primitive = @import("./Primitive.zig");

pub const ContainerType = enum {
    Struct,
    Enum,
};

pub const ImportType = union(enum) {
    Pkg: []const u8,
    File: []const u8,
};

const Self = @This();

node: AstNode,
kind: union(enum) {
    import: ImportType,
    this,
    builtin,
    call,
    array,
    ptr,
    optional,
    field_access,
    error_union,
    enum_literal,
    container: ContainerType,
    primitive: Primitive,
    unknown,
} = .unknown,

pub fn init(node: AstNode) Self {
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
                                .File = text,
                            },
                        },
                    };
                } else {
                    return Self{
                        .node = node,
                        .kind = .{
                            .import = .{
                                .Pkg = text,
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
            return fromVarDecl(node.context, var_decl);
        },
        .container_decl => |container_decl| {
            const token = AstToken.init(&node.context.tree, container_decl.ast.main_token);
            if (std.mem.eql(u8, token.getText(), "enum")) {
                return Self{
                    .node = node,
                    .kind = .{ .container = .Enum },
                };
            } else {
                return Self{
                    .node = node,
                    .kind = .{ .container = .Struct },
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
                        return init(AstNode.fromTokenIndex(node.context, decl.token.index));
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
                    return Self{
                        .node = node,
                        .kind = .field_access,
                    };
                },
                .error_union => {
                    return Self{
                        .node = node,
                        .kind = .error_union,
                    };
                },
                else => {
                    // try w.print("node [{s}]: {s}", .{ node.getMainToken().getText(), @tagName(node.getTag()) });
                },
            }
        },
    }

    return Self{
        .node = node,
    };
}

pub fn fromVarDecl(context: *const AstContext, var_decl: Ast.full.VarDecl) Self {
    const node_idx = if (var_decl.ast.type_node != 0)
        var_decl.ast.type_node
    else
        var_decl.ast.init_node;
    return init(AstNode.init(context, node_idx));
}

pub fn fromParam(context: *const AstContext, param: Ast.full.FnProto.Param) Self {
    const node_idx = param.type_expr;
    return init(AstNode.init(context, node_idx));
}

pub fn fromFnProtoReturn(context: *const AstContext, fn_proto: Ast.full.FnProto) Self {
    const node_idx = fn_proto.ast.return_type;
    return init(AstNode.init(context, node_idx));
}

pub fn fromContainerField(context: *const AstContext, field: Ast.full.ContainerField) Self {
    const node_idx = field.ast.type_expr;
    const node = AstNode.init(context, node_idx);
    if (node_idx == 0) {
        // enum member ?
        return Self{
            .node = node,
            .kind = .enum_literal,
        };
    } else {
        return init(node);
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
                .Pkg => |pkg| {
                    try w.print("@import pkg {s}", .{pkg});
                },
                .File => |file| {
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
