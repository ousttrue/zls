const std = @import("std");
const Ast = std.zig.Ast;
const AstToken = @import("./AstToken.zig");
const AstNode = @import("./AstNode.zig");
const AstContext = @import("./AstContext.zig");
const VarType = @import("./VarType.zig");
const Self = @This();

context: *const AstContext,
token: AstToken,
full: union(enum) {
    var_decl: Ast.full.VarDecl,
    if_payload: Ast.full.If,
    while_payload: Ast.full.While,
    switch_case_payload: Ast.full.SwitchCase,
    param: Ast.full.FnProto.Param,
},

/// block スコープを遡ってローカル変数を探す
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
                        .var_decl => |full| {
                            const token = statement_node.getMainToken().next();
                            if (std.mem.eql(u8, token.getText(), symbol)) {
                                return Self{
                                    .context = node.context,
                                    .token = token,
                                    .full = .{ .var_decl = full },
                                };
                            }
                        },
                        else => {},
                    }
                }
            },
            .@"if" => |full| {
                if (full.payload_token) |payload_token| {
                    const token = AstToken.init(tree, payload_token);
                    if (std.mem.eql(u8, token.getText(), symbol)) {
                        return Self{
                            .context = node.context,
                            .token = token,
                            .full = .{ .if_payload = full },
                        };
                    }
                }
            },
            .@"while" => |full| {
                if (full.payload_token) |payload_token| {
                    const token = AstToken.init(tree, payload_token);
                    if (std.mem.eql(u8, token.getText(), symbol)) {
                        return Self{
                            .context = node.context,
                            .token = token,
                            .full = .{ .while_payload = full },
                        };
                    }
                }
            },
            .switch_case => |full| {
                if (full.payload_token) |payload_token| {
                    const token = AstToken.init(tree, payload_token);
                    if (std.mem.eql(u8, token.getText(), symbol)) {
                        return Self{
                            .context = node.context,
                            .token = token,
                            .full = .{ .switch_case_payload = full },
                        };
                    }
                }
            },
            else => {
                switch (current.getTag()) {
                    .fn_decl => {
                        const fn_proto_node = AstNode.init(node.context, current.getData().lhs);
                        var buffer2: [2]u32 = undefined;
                        if (fn_proto_node.getFnProto(&buffer2)) |fn_proto| {
                            var params = fn_proto.iterate(tree);
                            while (params.next()) |param| {
                                if (param.name_token) |name_token| {
                                    const token = AstToken.init(tree, name_token);
                                    if (std.mem.eql(u8, token.getText(), symbol)) {
                                        return Self{
                                            .context = node.context,
                                            .token = token,
                                            .full = .{ .param = param },
                                        };
                                    }
                                }
                            }
                        } else {
                            unreachable;
                        }
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
        .var_decl => |full| {
            // getType: var decl type part => eval expression
            const var_type = VarType.fromVarDecl(self.context, full);
            const info = try var_type.allocPrint(allocator);
            defer allocator.free(info);
            try w.print("[local] var_decl: {s}", .{info});
        },
        .if_payload => |full| {
            // getType: eval expression
            _ = full;
            try w.print("[local] if_payload", .{});
        },
        .while_payload => |full| {
            // getType: eval expression
            _ = full;
            try w.print("[local] while_payload", .{});
        },
        .switch_case_payload => |full| {
            // getType: union type part
            _ = full;
            try w.print("[local] swtich_case_payload", .{});
        },
        .param => |full| {
            // getType: param decl
            _ = full;
            try w.print("[local] param", .{});
        },
    }

    return buffer.items;
}
