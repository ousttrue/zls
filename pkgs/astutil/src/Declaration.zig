const std = @import("std");
const Ast = std.zig.Ast;
const AstToken = @import("./AstToken.zig");
const AstNode = @import("./AstNode.zig");
const AstContext = @import("./AstContext.zig");
const VarType = @import("./VarType.zig");
const logger = std.log.scoped(.Declaration);
const Self = @This();

context: *const AstContext,
token: AstToken,
scope: union(enum) {
    block: AstNode,
    container: AstNode,
},
full: union(enum) {
    var_decl: Ast.full.VarDecl,
    if_payload: Ast.full.If,
    while_payload: Ast.full.While,
    switch_case_payload: Ast.full.SwitchCase,
    param: Ast.full.FnProto.Param,
    fn_decl: AstNode,
},

/// find local variable in from block scope
pub fn findFromBlockNode(scope: AstNode, symbol: []const u8) ?Self {
    const tree = &scope.context.tree;
    var buffer: [2]u32 = undefined;
    switch (scope.getChildren(&buffer)) {
        .block => |block| {
            for (block.ast.statements) |statement| {
                const statement_node = AstNode.init(scope.context, statement);
                var buffer2: [2]u32 = undefined;
                switch (statement_node.getChildren(&buffer2)) {
                    .var_decl => |full| {
                        const token = statement_node.getMainToken().getNext();
                        if (std.mem.eql(u8, token.getText(), symbol)) {
                            return Self{
                                .context = scope.context,
                                .token = token,
                                .scope = .{ .block = scope },
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
                        .context = scope.context,
                        .token = token,
                        .scope = .{ .block = scope },
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
                        .context = scope.context,
                        .token = token,
                        .scope = .{ .block = scope },
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
                        .context = scope.context,
                        .token = token,
                        .scope = .{ .block = scope },
                        .full = .{ .switch_case_payload = full },
                    };
                }
            }
        },
        else => {
            switch (scope.getTag()) {
                .fn_decl => {
                    const fn_proto_node = AstNode.init(scope.context, scope.getData().lhs);
                    var buffer2: [2]u32 = undefined;
                    if (fn_proto_node.getFnProto(&buffer2)) |fn_proto| {
                        var params = fn_proto.iterate(tree);
                        while (params.next()) |param| {
                            if (param.name_token) |name_token| {
                                const token = AstToken.init(tree, name_token);
                                if (std.mem.eql(u8, token.getText(), symbol)) {
                                    return Self{
                                        .context = scope.context,
                                        .token = token,
                                        .scope = .{ .block = scope },
                                        .full = .{ .param = param },
                                    };
                                }
                            }
                        }
                    } else {
                        unreachable;
                    }
                },
                else => {},
            }
        },
    }
    return null;
}

/// find declaration from container scope
pub fn findFromContainerNode(scope: AstNode, symbol: []const u8) ?Self {
    const tree = &scope.context.tree;
    var buffer: [2]u32 = undefined;
    switch (scope.getChildren(&buffer)) {
        .container_decl => |container_decl| {
            for (container_decl.ast.members) |member| {
                const member_node = AstNode.init(scope.context, member);
                var buf2: [2]u32 = undefined;
                switch (member_node.getChildren(&buf2)) {
                    .var_decl => |full| {
                        const token = member_node.getMainToken().getNext();
                        if (std.mem.eql(u8, token.getText(), symbol)) {
                            return Self{
                                .context = scope.context,
                                .token = token,
                                .scope = .{ .container = scope },
                                .full = .{ .var_decl = full },
                            };
                        }
                    },
                    .container_field => {},
                    else => {
                        switch (member_node.getTag()) {
                            .fn_decl => {
                                const fn_proto_node = AstNode.init(scope.context, member_node.getData().lhs);
                                var buf3: [2]u32 = undefined;
                                if (fn_proto_node.getFnProto(&buf3)) |fn_proto| {
                                    if (fn_proto.name_token) |name_token| {
                                        const token = AstToken.init(tree, name_token);
                                        if (std.mem.eql(u8, token.getText(), symbol)) {
                                            return Self{
                                                .context = scope.context,
                                                .token = token,
                                                .scope = .{ .container = scope },
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
    return null;
}

pub fn findFromContainer(node: AstNode) ?Self {
    if(node.getTag() != .identifier)
    {
        return null;        
    }
    const symbol = node.getMainToken().getText();
    var it = node.parentIterator();
    while (it.current) |current| : (it.next()) {
        if (findFromContainerNode(current, symbol)) |decl| {
            return decl;
        }
    }
    return null;
}

pub fn findFromBlock(node: AstNode) ?Self {
    if (node.getTag() != .identifier) {
        return null;
    }
    const symbol = node.getMainToken().getText();
    var it = node.parentIterator();
    while (it.current) |current| : (it.next()) {
        if (findFromBlockNode(current, symbol)) |local| {
            return local;
        }
        if (current.getTag() == .fn_decl) {
            break;
        }
        var buf: [2]u32 = undefined;
        if (current.getChildren(&buf) == .container_decl) {
            break;
        }
    }
    return null;
}

pub fn allocPrint(self: Self, allocator: std.mem.Allocator) anyerror![]const u8 {
    var buffer = std.ArrayList(u8).init(allocator);
    const w = buffer.writer();

    switch (self.full) {
        .var_decl => |full| {
            // getType: var decl type part => eval expression
            _ = full;
            // const var_type = VarType.fromVarDecl(self.context, full);
            // const info = try var_type.allocPrint(allocator);
            // defer allocator.free(info);
            // try w.print("{s}", .{info});
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
            // const var_type = VarType.fromParam(self.context, full);
            // const info = try var_type.allocPrint(allocator);
            // defer allocator.free(info);
            // try w.print("{s}: {s}", .{ self.token.getText(), info });
        },
        .fn_decl => {
            try w.print("[container] fn", .{});
        },
    }

    return buffer.items;
}
