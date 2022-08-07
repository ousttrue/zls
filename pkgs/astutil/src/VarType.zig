const std = @import("std");
const Ast = std.zig.Ast;
const AstToken = @import("./AstToken.zig");
const AstNode = @import("./AstNode.zig");
const AstContext = @import("./AstContext.zig");
const Declaration = @import("./Declaration.zig");
const Self = @This();

node: AstNode,
kind: union(enum) {
    import,
    this,
    builtin,
    call,
    array,
    ptr,
    container,
    ref: Declaration,
    unknown,
} = .unknown,

pub fn init(node: AstNode) Self {
    var buf: [2]u32 = undefined;
    switch (node.getChildren(&buf)) {
        .builtin_call => {
            const builtin = node.getMainToken().getText();
            if (std.mem.eql(u8, builtin, "@import")) {
                return Self{
                    .node = node,
                    .kind = .import,
                };
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
        .container_decl => {
            return Self{
                .node = node,
                .kind = .container,
            };
        },
        else => {
            switch (node.getTag()) {
                .identifier => {
                    if (Declaration.fromToken(node.context, node.getMainToken())) |decl| {
                        // deref
                        return Self{
                            .node = node,
                            .kind = .{ .ref = decl },
                        };
                    } else {
                        // try w.print("no ref: {s}", .{node.getMainToken().getText()});
                    }
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

pub fn fromContainerField(context: *const AstContext, field: Ast.full.ContainerField) Self
{
    const node_idx = field.ast.type_expr;
    return init(AstNode.init(context, node_idx));
}

pub fn allocPrint(self: Self, allocator: std.mem.Allocator) ![]const u8 {
    var buffer = std.ArrayList(u8).init(allocator);
    const w = buffer.writer();

    var buf: [2]u32 = undefined;
    switch (self.node.getChildren(&buf)) {
        .builtin_call => {
            const builtin = self.node.getMainToken().getText();
            if (std.mem.eql(u8, builtin, "@import")) {
                try w.print("struct @import", .{});
            } else if (std.mem.eql(u8, builtin, "@This")) {
                // TODO: eval
                try w.print("struct @This", .{});
            } else {
                try w.print("{s}", .{builtin});
            }
        },
        .call => {
            // TODO: eval
            try w.print("call result value type", .{});
        },
        .array_type => |array_type| {
            const element_count = AstNode.init(self.node.context, array_type.ast.elem_count);
            // TODO: deref
            const element_type = AstNode.init(self.node.context, array_type.ast.elem_type);
            try w.print("[{s}]{s}", .{
                element_count.getMainToken().getText(),
                element_type.getMainToken().getText(),
            });
        },
        .ptr_type => |ptr_type| {
            // TODO: deref
            const child_type = AstNode.init(self.node.context, ptr_type.ast.child_type);
            try w.print("*{s}", .{
                child_type.getMainToken().getText(),
            });
        },
        .container_decl => {
            try w.print("struct", .{});
        },
        else => {
            switch (self.node.getTag()) {
                .identifier => {
                    if (Declaration.fromToken(self.node.context, self.node.getMainToken())) |decl| {
                        // deref
                        const info = try decl.allocPrint(allocator);
                        defer allocator.free(info);
                        try w.print("{s}", .{info});
                    } else {
                        try w.print("no ref: {s}", .{self.node.getMainToken().getText()});
                    }
                },
                else => {
                    try w.print("node [{s}]: {s}", .{ self.node.getMainToken().getText(), @tagName(self.node.getTag()) });
                },
            }
        },
    }
    return buffer.items;
}
