const std = @import("std");
const Ast = std.zig.Ast;
const AstToken = @import("./AstToken.zig");
const AstNode = @import("./AstNode.zig");
const logger = std.log.scoped(.Declaration);
const Self = @This();

token: AstToken,
full: union(enum) {
    var_decl: Ast.full.VarDecl,
    fn_decl: AstNode,
},

/// Container スコープを遡って Declaration を探す
pub fn find(node: AstNode) ?Self {
    std.debug.assert(node.getTag() == .identifier);
    const tree = &node.context.tree;
    const symbol = node.getMainToken().getText();
    var it = node.parentIterator();
    while (it.next()) |current| {
        var buffer: [2]u32 = undefined;
        switch (current.getChildren(&buffer)) {
            .container_decl => |container_decl| {
                for (container_decl.ast.members) |member| {
                    const member_node = AstNode.init(node.context, member);
                    var buf2: [2]u32 = undefined;
                    switch (member_node.getChildren(&buf2)) {
                        .var_decl => |full| {
                            const token = member_node.getMainToken().next();
                            if (std.mem.eql(u8, token.getText(), symbol)) {
                                return Self{
                                    .token = token,
                                    .full = .{ .var_decl = full },
                                };
                            }
                        },
                        .container_field => {},
                        else => {
                            switch (member_node.getTag()) {
                                .fn_decl => {
                                    const fn_proto_node = AstNode.init(node.context, member_node.getData().lhs);
                                    var buf3: [2]u32 = undefined;
                                    if (fn_proto_node.getFnProto(&buf3)) |fn_proto| {
                                        if (fn_proto.name_token) |name_token| {
                                            const token = AstToken.init(tree, name_token);
                                            if (std.mem.eql(u8, token.getText(), symbol)) {
                                                return Self{
                                                    .token = token,
                                                    .full = .{ .fn_decl = member_node },
                                                };
                                            }
                                        }
                                    }
                                },
                                else => {
                                    logger.debug("unknown: {}", .{member_node.getTag()});
                                },
                            }
                        },
                    }
                }
            },
            else => {},
        }
    }
    return null;
}

pub fn allocPrint(self: Self, allocator: std.mem.Allocator) ![]const u8 {
    var buffer = std.ArrayList(u8).init(allocator);
    const w = buffer.writer();
    switch (self.full) {
        .var_decl => {
            try w.print("[container] var_decl", .{});
        },
        .fn_decl => {
            try w.print("[container] fn_decl", .{});
        },
    }
    return buffer.items;
}
