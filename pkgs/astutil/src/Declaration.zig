//! search name_token in the source file.
const std = @import("std");
const Ast = std.zig.Ast;
const AstToken = @import("./AstToken.zig");
const AstNode = @import("./AstNode.zig");
const AstContext = @import("./AstContext.zig");
const Project = @import("./Project.zig");
const PrimitiveType = @import("./primitives.zig").PrimitiveType;
const logger = std.log.scoped(.Declaration);

pub const LocalVariable = struct {
    block: AstNode,
    variable: union(enum) {
        var_decl: AstNode,
        if_payload,
        while_payload,
        switch_case_payload,
        param: AstNode,
    },
    name_token: AstToken,
};

pub const ContainerDecl = struct {
    container: AstNode,
    member: union(enum) {
        var_decl: AstNode,
        field: AstNode,
        fn_decl: AstNode,
        // test_decl: AstNode,
    },
    name_token: AstToken,
};

pub const Declaration = union(enum) {
    const Self = @This();
    // var | const, payload, param
    local: LocalVariable,
    // container member. var | const, field, fn
    container: ContainerDecl,
    // u32, bool, ... etc
    primitive: PrimitiveType,

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
                        .var_decl => {
                            const name_token = statement_node.getMainToken().getNext();
                            if (std.mem.eql(u8, name_token.getText(), symbol)) {
                                return Self{ .local = .{
                                    .block = scope,
                                    .variable = .{ .var_decl = statement_node },
                                    .name_token = name_token,
                                } };
                            }
                        },
                        else => {},
                    }
                }
            },
            .@"if" => |full| {
                if (full.payload_token) |payload_token| {
                    const name_token = AstToken.init(tree, payload_token);
                    if (std.mem.eql(u8, name_token.getText(), symbol)) {
                        return Self{
                            .local = .{
                                .block = scope,
                                .variable = .if_payload,
                                .name_token = name_token,
                            },
                        };
                    }
                }
            },
            .@"while" => |full| {
                if (full.payload_token) |payload_token| {
                    const name_token = AstToken.init(tree, payload_token);
                    if (std.mem.eql(u8, name_token.getText(), symbol)) {
                        return Self{
                            .local = .{
                                .block = scope,
                                .variable = .while_payload,
                                .name_token = name_token,
                            },
                        };
                    }
                }
            },
            .switch_case => |full| {
                if (full.payload_token) |payload_token| {
                    const name_token = AstToken.init(tree, payload_token);
                    if (std.mem.eql(u8, name_token.getText(), symbol)) {
                        return Self{
                            .local = .{
                                .block = scope,
                                .variable = .switch_case_payload,
                                .name_token = name_token,
                            },
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
                                if (param.name_token) |name_token_index| {
                                    const name_token = AstToken.init(tree, name_token_index);
                                    if (std.mem.eql(u8, name_token.getText(), symbol)) {
                                        const param_type = AstNode.init(scope.context, param.type_expr);
                                        // param
                                        return Self{
                                            .local = .{
                                                .block = scope,
                                                .variable = .{ .param = param_type },
                                                .name_token = name_token,
                                            },
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
                        .var_decl => {
                            const name_token = member_node.getMainToken().getNext();
                            if (std.mem.eql(u8, name_token.getText(), symbol)) {
                                return Self{
                                    .container = .{
                                        .container = scope,
                                        .member = .{ .var_decl = member_node },
                                        .name_token = name_token,
                                    },
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
                                        if (fn_proto.name_token) |name_token_index| {
                                            const name_token = AstToken.init(tree, name_token_index);
                                            if (std.mem.eql(u8, name_token.getText(), symbol)) {
                                                return Self{
                                                    .container = .{
                                                        .container = scope,
                                                        .member = .{ .fn_decl = member_node },
                                                        .name_token = name_token,
                                                    },
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

    pub fn find(node: AstNode) ?Self {
        if (node.getTag() != .identifier) {
            return null;
        }
        const symbol = node.getMainToken().getText();

        // from block
        {
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
        }

        // from container
        {
            var it = node.parentIterator();
            while (it.current) |current| : (it.next()) {
                if (findFromContainerNode(current, symbol)) |decl| {
                    return decl;
                }
            }
        }

        return null;
    }

    pub fn allocPrint(
        self: Self,
        allocator: std.mem.Allocator,
    ) anyerror![]const u8 {
        var buffer = std.ArrayList(u8).init(allocator);
        const w = buffer.writer();

        switch (self) {
            .local => {
                try w.print("[local]", .{});
            },
            .container => {
                try w.print("[container]", .{});
            },
            .primitive => {
                try w.print("[primitive]", .{});
            },
            // .var_decl => |full| {
            //     // getType: var decl type part => eval expression
            //     _ = full;
            //     // const var_type = VarType.fromVarDecl(self.context, full);
            //     // const info = try var_type.allocPrint(allocator);
            //     // defer allocator.free(info);
            //     try w.print("[var_decl]", .{});
            // },
            // .if_payload => |full| {
            //     // getType: eval expression
            //     _ = full;
            //     try w.print("[if_payload]", .{});
            // },
            // .while_payload => |full| {
            //     // getType: eval expression
            //     _ = full;
            //     try w.print("[while_payload]", .{});
            // },
            // .switch_case_payload => |full| {
            //     // getType: union type part
            //     _ = full;
            //     try w.print("[swtich_case_payload]", .{});
            // },
            // .param => |full| {
            //     _ = full;
            //     // getType: param decl
            //     // const var_type = try VarType.fromParam(project, self.context, full);
            //     // const info = try var_type.allocPrint(allocator);
            //     // defer allocator.free(info);
            //     // try w.print("[param] {s}", .{info});
            //     try w.print("[param]", .{});
            // },
            // .fn_decl => {
            //     try w.print("[global] fn", .{});
            // },
        }

        return buffer.items;
    }
};
