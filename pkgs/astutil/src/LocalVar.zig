const std = @import("std");
const Ast = std.zig.Ast;
const AstToken = @import("./AstToken.zig");
const AstNode = @import("./AstNode.zig");
const Self = @This();

full: union(enum) {
    var_decl: Ast.full.VarDecl,
    if_payload: Ast.full.If,
},

/// var_decl, if_payload, while_payload, switch_case_payload, fn_param
pub fn find(node: AstNode) ?Self {
    std.debug.assert(node.getTag() == .identifier);
    const tree = &node.context.tree;
    const symbol = node.getMainToken().getText();
    var it = node.parentIterator();
    while (it.next()) |current| {
        var buffer: [2]u32 = undefined;
        switch (current.getChildren(&buffer)) {
            .block => |block| {
                for (block.ast.statements) |statement| {
                    const statement_node = AstNode.init(node.context, statement);
                    var buffer2: [2]u32 = undefined;
                    switch (statement_node.getChildren(&buffer2)) {
                        .var_decl => |var_decl| {
                            if (std.mem.eql(u8, statement_node.getMainToken().next().getText(), symbol)) {
                                return Self{
                                    .full = .{ .var_decl = var_decl },
                                };
                            }
                        },
                        else => {},
                    }
                }
            },
            .@"if" => |if_full| {
                if (if_full.payload_token) |payload_token| {
                    const token = AstToken.init(tree, payload_token);
                    if (std.mem.eql(u8, token.getText(), symbol)) {
                        return Self{
                            .full = .{ .if_payload = if_full },
                        };
                    }
                }
            },
            else => {
                switch (current.getTag()) {
                    .fn_decl => {
                        break;
                    },
                    else => {},
                }
            },
        }
    }
    return null;
}

pub fn allocPrint(self: Self, allocator: std.mem.Allocator) ![]const u8 {
    var buffer = std.ArrayList(u8).init(allocator);
    const w = buffer.writer();

    switch (self.full) {
        .var_decl => |var_decl| {
            // getType: var decl type part => eval expression
            _ = var_decl;
            try w.print("[local] var_decl", .{});
        },
        .if_payload => |if_full| {
            // getType: eval expression
            _ = if_full;
            try w.print("[local] if_payload", .{});
        },
    }

    return buffer.items;
}
