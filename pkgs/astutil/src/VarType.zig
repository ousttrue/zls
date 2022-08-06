const std = @import("std");
const Ast = std.zig.Ast;
const AstToken = @import("./AstToken.zig");
const AstNode = @import("./AstNode.zig");
const AstContext = @import("./AstContext.zig");
const Self = @This();

data: union(enum) {
    type_expr: AstNode, // resolve to container or primitive or call result
    init_expr: AstNode, // resolve to container or primitive or call result
},

pub fn fromVarDecl(context: *const AstContext, var_decl: Ast.full.VarDecl) Self {
    if (var_decl.ast.type_node != 0) {
        return Self{
            .data = .{
                .type_expr = AstNode.init(context, var_decl.ast.type_node),
            },
        };
    } else {
        return Self{
            .data = .{
                .init_expr = AstNode.init(context, var_decl.ast.init_node),
            },
        };
    }
}

pub fn allocPrint(self: Self, allocator: std.mem.Allocator) ![]const u8 {
    var buffer = std.ArrayList(u8).init(allocator);
    const w = buffer.writer();
    switch (self.data) {
        .type_expr => |type_expr| {
            try w.print("{s}", .{@tagName(type_expr.getTag())});
        },
        .init_expr => |init_expr| {
            try w.print("{s}", .{@tagName(init_expr.getTag())});
        },
    }
    return buffer.items;
}
