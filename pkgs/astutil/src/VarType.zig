const std = @import("std");
const Ast = std.zig.Ast;
const AstToken = @import("./AstToken.zig");
const AstNode = @import("./AstNode.zig");
const AstContext = @import("./AstContext.zig");
const Declaration = @import("./Declaration.zig");
const Self = @This();

node: AstNode,

pub fn fromVarDecl(context: *const AstContext, var_decl: Ast.full.VarDecl) Self {
    const node_idx = if (var_decl.ast.type_node != 0)
        var_decl.ast.type_node
    else
        var_decl.ast.init_node;
    const node = AstNode.init(context, node_idx);
    return Self{
        .node = node,
    };
}

pub fn fromParam(context: *const AstContext, param: Ast.full.FnProto.Param) Self {
    const node_idx = param.type_expr;
    const node = AstNode.init(context, node_idx);
    return Self{
        .node = node,
    };
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
                    try w.print("node [{s}]: {s}", .{self.node.getMainToken().getText(), @tagName(self.node.getTag())});
                },
            }
        },
    }
    return buffer.items;
}
