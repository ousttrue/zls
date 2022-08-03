const std = @import("std");
const lsp = @import("lsp");
const Ast = std.zig.Ast;
const ast = @import("./ast.zig");
const Scope = @import("./Scope.zig");
const Declaration = Scope.Declaration;
const TypeWithHandle = @import("./TypeWithHandle.zig");
const logger = std.log.scoped(.DocumentScope);

fn nodeSourceRange(tree: Ast, node: Ast.Node.Index) std.zig.Token.Loc {
    const loc_start = ast.tokenLocation(tree, tree.firstToken(node));
    const loc_end = ast.tokenLocation(tree, ast.lastToken(tree, node));

    return .{
        .start = loc_start.start,
        .end = loc_end.end,
    };
}

const CompletionContext = struct {
    pub fn hash(self: @This(), item: lsp.CompletionItem) u32 {
        _ = self;
        return @truncate(u32, std.hash.Wyhash.hash(0, item.label));
    }

    pub fn eql(self: @This(), a: lsp.CompletionItem, b: lsp.CompletionItem, b_index: usize) bool {
        _ = self;
        _ = b_index;
        return std.mem.eql(u8, a.label, b.label);
    }
};

pub const CompletionSet = std.ArrayHashMapUnmanaged(
    lsp.CompletionItem,
    void,
    CompletionContext,
    false,
);
comptime {
    std.debug.assert(@sizeOf(lsp.CompletionItem) == @sizeOf(CompletionSet.Data));
}

const Self = @This();

allocator: std.mem.Allocator,
/// Whether we have already visited the root node.
had_root: bool = false,
scopes: std.ArrayList(Scope),
errors: CompletionSet = .{},
enums: CompletionSet = .{},

pub fn init(allocator: std.mem.Allocator, tree: Ast) !Self {
    var self = Self{
        .allocator = allocator,
        .scopes = std.ArrayList(Scope).init(allocator),
    };
    try self.makeScopeInternal(tree, 0);
    return self;
}

pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
    for (self.scopes.items) |*scope| {
        scope.decls.deinit();
        allocator.free(scope.uses);
        allocator.free(scope.tests);
    }
    self.scopes.deinit();

    for (self.errors.entries.items(.key)) |item| {
        if (item.documentation) |doc| allocator.free(doc.value);
    }
    self.errors.deinit(allocator);

    for (self.enums.entries.items(.key)) |item| {
        if (item.documentation) |doc| allocator.free(doc.value);
    }
    self.enums.deinit(allocator);
}

fn makeScopeInternal(
    self: *Self,
    tree: Ast,
    /// decl
    node_idx: Ast.Node.Index,
) error{OutOfMemory}!void {
    const allocator = self.allocator;
    const tags = tree.nodes.items(.tag);
    const token_tags = tree.tokens.items(.tag);
    const data = tree.nodes.items(.data);
    const main_tokens = tree.nodes.items(.main_token);
    const node_tag = tags[node_idx];

    if (node_idx == 0) {
        if (self.had_root)
            return
        else
            self.had_root = true;
    }

    switch (node_tag) {
        .container_decl,
        .container_decl_trailing,
        .container_decl_arg,
        .container_decl_arg_trailing,
        .container_decl_two,
        .container_decl_two_trailing,
        .tagged_union,
        .tagged_union_trailing,
        .tagged_union_two,
        .tagged_union_two_trailing,
        .tagged_union_enum_tag,
        .tagged_union_enum_tag_trailing,
        .root,
        .error_set_decl,
        => {
            var buf: [2]Ast.Node.Index = undefined;
            const ast_decls = ast.declMembers(tree, node_idx, &buf);

            // new Scope
            (try self.scopes.addOne()).* = .{
                .range = nodeSourceRange(tree, node_idx),
                .decls = std.StringHashMap(Declaration).init(allocator),
                .data = .{ .container = node_idx },
            };
            const scope_idx = self.scopes.items.len - 1;
            var uses = std.ArrayListUnmanaged(*const Ast.Node.Index){};
            var tests = std.ArrayListUnmanaged(Ast.Node.Index){};

            errdefer {
                self.scopes.items[scope_idx].decls.deinit();
                uses.deinit(allocator);
                tests.deinit(allocator);
            }

            if (node_tag == .error_set_decl) {
                // All identifiers in main_token..data.lhs are error fields.
                var i = main_tokens[node_idx];
                while (i < data[node_idx].rhs) : (i += 1) {
                    if (token_tags[i] == .identifier) {
                        try self.errors.put(allocator, .{
                            .label = tree.tokenSlice(i),
                            .kind = .Constant,
                            .insertText = tree.tokenSlice(i),
                            .insertTextFormat = .PlainText,
                        }, {});
                    }
                }
            }

            const container_decl = switch (node_tag) {
                .container_decl, .container_decl_trailing => tree.containerDecl(node_idx),
                .container_decl_arg, .container_decl_arg_trailing => tree.containerDeclArg(node_idx),
                .container_decl_two, .container_decl_two_trailing => blk: {
                    var buffer: [2]Ast.Node.Index = undefined;
                    break :blk tree.containerDeclTwo(&buffer, node_idx);
                },
                .tagged_union, .tagged_union_trailing => tree.taggedUnion(node_idx),
                .tagged_union_enum_tag, .tagged_union_enum_tag_trailing => tree.taggedUnionEnumTag(node_idx),
                .tagged_union_two, .tagged_union_two_trailing => blk: {
                    var buffer: [2]Ast.Node.Index = undefined;
                    break :blk tree.taggedUnionTwo(&buffer, node_idx);
                },
                else => null,
            };

            // Only tagged unions and enums should pass this
            const can_have_enums = if (container_decl) |container| blk: {
                const kind = token_tags[container.ast.main_token];
                break :blk kind != .keyword_struct and
                    (kind != .keyword_union or container.ast.enum_token != null or container.ast.arg != 0);
            } else false;

            for (ast_decls) |*ptr_decl| {
                const decl = ptr_decl.*;
                if (tags[decl] == .@"usingnamespace") {
                    try uses.append(allocator, ptr_decl);
                    continue;
                }

                try self.makeScopeInternal(tree, decl);
                const name = TypeWithHandle.getDeclName(tree, decl) orelse continue;

                if (tags[decl] == .test_decl) {
                    try tests.append(allocator, decl);
                    continue;
                }
                if (try self.scopes.items[scope_idx].decls.fetchPut(name, .{ .ast_node = decl })) |existing| {
                    _ = existing;
                    // TODO Record a redefinition error.
                }

                if (!can_have_enums)
                    continue;

                const container_field = switch (tags[decl]) {
                    .container_field => tree.containerField(decl),
                    .container_field_align => tree.containerFieldAlign(decl),
                    .container_field_init => tree.containerFieldInit(decl),
                    else => null,
                };

                if (container_field) |_| {
                    if (!std.mem.eql(u8, name, "_")) {
                        try self.enums.put(allocator, .{
                            .label = name,
                            .kind = .Constant,
                            .insertText = name,
                            .insertTextFormat = .PlainText,
                            .documentation = if (try ast.getDocComments(allocator, tree, decl, .Markdown)) |docs|
                                lsp.MarkupContent{ .kind = .Markdown, .value = docs }
                            else
                                null,
                        }, {});
                    }
                }
            }

            self.scopes.items[scope_idx].tests = tests.toOwnedSlice(allocator);
            self.scopes.items[scope_idx].uses = uses.toOwnedSlice(allocator);
        },
        .array_type_sentinel => {
            // TODO: ???
            return;
        },
        .fn_proto,
        .fn_proto_one,
        .fn_proto_simple,
        .fn_proto_multi,
        .fn_decl,
        => |fn_tag| {
            var buf: [1]Ast.Node.Index = undefined;
            const func = ast.fnProto(tree, node_idx, &buf).?;

            (try self.scopes.addOne()).* = .{
                .range = nodeSourceRange(tree, node_idx),
                .decls = std.StringHashMap(Declaration).init(allocator),
                .data = .{ .function = node_idx },
            };
            var scope_idx = self.scopes.items.len - 1;
            errdefer self.scopes.items[scope_idx].decls.deinit();

            var it = func.iterate(&tree);
            while (it.next()) |param| {
                // Add parameter decls
                if (param.name_token) |name_token| {
                    if (try self.scopes.items[scope_idx].decls.fetchPut(
                        tree.tokenSlice(name_token),
                        .{ .param_decl = param },
                    )) |existing| {
                        _ = existing;
                        // TODO record a redefinition error
                    }
                }
                // Visit parameter types to pick up any error sets and enum
                //   completions
                try self.makeScopeInternal(tree, param.type_expr);
            }

            if (fn_tag == .fn_decl) blk: {
                if (data[node_idx].lhs == 0) break :blk;
                const return_type_node = data[data[node_idx].lhs].rhs;

                // Visit the return type
                try self.makeScopeInternal(tree, return_type_node);
            }

            // Visit the function body
            try self.makeScopeInternal(tree, data[node_idx].rhs);
        },
        .test_decl => {
            return try self.makeScopeInternal(tree, data[node_idx].rhs);
        },
        .block,
        .block_semicolon,
        .block_two,
        .block_two_semicolon,
        => {
            const first_token = tree.firstToken(node_idx);
            const last_token = ast.lastToken(tree, node_idx);

            // if labeled block
            if (token_tags[first_token] == .identifier) {
                const scope = try self.scopes.addOne();
                scope.* = .{
                    .range = .{
                        .start = ast.tokenLocation(tree, main_tokens[node_idx]).start,
                        .end = ast.tokenLocation(tree, last_token).start,
                    },
                    .decls = std.StringHashMap(Declaration).init(allocator),
                    .data = .other,
                };
                errdefer scope.decls.deinit();
                try scope.decls.putNoClobber(tree.tokenSlice(first_token), .{ .label_decl = first_token });
            }

            (try self.scopes.addOne()).* = .{
                .range = nodeSourceRange(tree, node_idx),
                .decls = std.StringHashMap(Declaration).init(allocator),
                .data = .{ .block = node_idx },
            };
            var scope_idx = self.scopes.items.len - 1;
            var uses = std.ArrayList(*const Ast.Node.Index).init(allocator);

            errdefer {
                self.scopes.items[scope_idx].decls.deinit();
                uses.deinit();
            }

            const statements: []const Ast.Node.Index = switch (node_tag) {
                .block, .block_semicolon => tree.extra_data[data[node_idx].lhs..data[node_idx].rhs],
                .block_two, .block_two_semicolon => blk: {
                    const statements = &[_]Ast.Node.Index{ data[node_idx].lhs, data[node_idx].rhs };
                    const len: usize = if (data[node_idx].lhs == 0)
                        @as(usize, 0)
                    else if (data[node_idx].rhs == 0)
                        @as(usize, 1)
                    else
                        @as(usize, 2);
                    break :blk statements[0..len];
                },
                else => unreachable,
            };

            for (statements) |*ptr_stmt| {
                const idx = ptr_stmt.*;
                if (tags[idx] == .@"usingnamespace") {
                    try uses.append(ptr_stmt);
                    continue;
                }

                try self.makeScopeInternal(tree, idx);
                if (ast.varDecl(tree, idx)) |var_decl| {
                    const name = tree.tokenSlice(var_decl.ast.mut_token + 1);
                    if (try self.scopes.items[scope_idx].decls.fetchPut(name, .{ .ast_node = idx })) |existing| {
                        _ = existing;
                        // TODO record a redefinition error.
                    }
                }
            }

            self.scopes.items[scope_idx].uses = uses.toOwnedSlice();
            return;
        },
        .@"if",
        .if_simple,
        => {
            const if_node = ast.ifFull(tree, node_idx);

            if (if_node.payload_token) |payload| {
                var scope = try self.scopes.addOne();
                scope.* = .{
                    .range = .{
                        .start = ast.tokenLocation(tree, payload).start,
                        .end = ast.tokenLocation(tree, ast.lastToken(tree, if_node.ast.then_expr)).end,
                    },
                    .decls = std.StringHashMap(Declaration).init(allocator),
                    .data = .other,
                };
                errdefer scope.decls.deinit();

                const name_token = payload + @boolToInt(token_tags[payload] == .asterisk);
                std.debug.assert(token_tags[name_token] == .identifier);

                const name = tree.tokenSlice(name_token);
                try scope.decls.putNoClobber(name, .{
                    .pointer_payload = .{
                        .name = name_token,
                        .condition = if_node.ast.cond_expr,
                    },
                });
            }

            try self.makeScopeInternal(tree, if_node.ast.then_expr);

            if (if_node.ast.else_expr != 0) {
                if (if_node.error_token) |err_token| {
                    std.debug.assert(token_tags[err_token] == .identifier);
                    var scope = try self.scopes.addOne();
                    scope.* = .{
                        .range = .{
                            .start = ast.tokenLocation(tree, err_token).start,
                            .end = ast.tokenLocation(tree, ast.lastToken(tree, if_node.ast.else_expr)).end,
                        },
                        .decls = std.StringHashMap(Declaration).init(allocator),
                        .data = .other,
                    };
                    errdefer scope.decls.deinit();

                    const name = tree.tokenSlice(err_token);
                    try scope.decls.putNoClobber(name, .{ .ast_node = if_node.ast.else_expr });
                }
                try self.makeScopeInternal(tree, if_node.ast.else_expr);
            }
        },
        .@"catch" => {
            try self.makeScopeInternal(tree, data[node_idx].lhs);

            const catch_token = main_tokens[node_idx];
            const catch_expr = data[node_idx].rhs;

            var scope = try self.scopes.addOne();
            scope.* = .{
                .range = .{
                    .start = ast.tokenLocation(tree, tree.firstToken(catch_expr)).start,
                    .end = ast.tokenLocation(tree, ast.lastToken(tree, catch_expr)).end,
                },
                .decls = std.StringHashMap(Declaration).init(allocator),
                .data = .other,
            };
            errdefer scope.decls.deinit();

            if (token_tags.len > catch_token + 2 and
                token_tags[catch_token + 1] == .pipe and
                token_tags[catch_token + 2] == .identifier)
            {
                const name = tree.tokenSlice(catch_token + 2);
                try scope.decls.putNoClobber(name, .{ .ast_node = catch_expr });
            }
            try self.makeScopeInternal(tree, catch_expr);
        },
        .@"while",
        .while_simple,
        .while_cont,
        .@"for",
        .for_simple,
        => {
            const while_node = ast.whileAst(tree, node_idx).?;
            const is_for = node_tag == .@"for" or node_tag == .for_simple;

            if (while_node.label_token) |label| {
                std.debug.assert(token_tags[label] == .identifier);
                var scope = try self.scopes.addOne();
                scope.* = .{
                    .range = .{
                        .start = ast.tokenLocation(tree, while_node.ast.while_token).start,
                        .end = ast.tokenLocation(tree, ast.lastToken(tree, node_idx)).end,
                    },
                    .decls = std.StringHashMap(Declaration).init(allocator),
                    .data = .other,
                };
                errdefer scope.decls.deinit();

                try scope.decls.putNoClobber(tree.tokenSlice(label), .{ .label_decl = label });
            }

            if (while_node.payload_token) |payload| {
                var scope = try self.scopes.addOne();
                scope.* = .{
                    .range = .{
                        .start = ast.tokenLocation(tree, payload).start,
                        .end = ast.tokenLocation(tree, ast.lastToken(tree, while_node.ast.then_expr)).end,
                    },
                    .decls = std.StringHashMap(Declaration).init(allocator),
                    .data = .other,
                };
                errdefer scope.decls.deinit();

                const name_token = payload + @boolToInt(token_tags[payload] == .asterisk);
                std.debug.assert(token_tags[name_token] == .identifier);

                const name = tree.tokenSlice(name_token);
                try scope.decls.putNoClobber(name, if (is_for) .{
                    .array_payload = .{
                        .identifier = name_token,
                        .array_expr = while_node.ast.cond_expr,
                    },
                } else .{
                    .pointer_payload = .{
                        .name = name_token,
                        .condition = while_node.ast.cond_expr,
                    },
                });

                // for loop with index as well
                if (token_tags[name_token + 1] == .comma) {
                    const index_token = name_token + 2;
                    std.debug.assert(token_tags[index_token] == .identifier);
                    if (try scope.decls.fetchPut(
                        tree.tokenSlice(index_token),
                        .{ .array_index = index_token },
                    )) |existing| {
                        _ = existing;
                        // TODO Record a redefinition error
                    }
                }
            }
            try self.makeScopeInternal(tree, while_node.ast.then_expr);

            if (while_node.ast.else_expr != 0) {
                if (while_node.error_token) |err_token| {
                    std.debug.assert(token_tags[err_token] == .identifier);
                    var scope = try self.scopes.addOne();
                    scope.* = .{
                        .range = .{
                            .start = ast.tokenLocation(tree, err_token).start,
                            .end = ast.tokenLocation(tree, ast.lastToken(tree, while_node.ast.else_expr)).end,
                        },
                        .decls = std.StringHashMap(Declaration).init(allocator),
                        .data = .other,
                    };
                    errdefer scope.decls.deinit();

                    const name = tree.tokenSlice(err_token);
                    try scope.decls.putNoClobber(name, .{ .ast_node = while_node.ast.else_expr });
                }
                try self.makeScopeInternal(tree, while_node.ast.else_expr);
            }
        },
        .@"switch",
        .switch_comma,
        => {
            const cond = data[node_idx].lhs;
            const extra = tree.extraData(data[node_idx].rhs, Ast.Node.SubRange);
            const cases = tree.extra_data[extra.start..extra.end];

            for (cases) |case| {
                const switch_case: Ast.full.SwitchCase = switch (tags[case]) {
                    .switch_case => tree.switchCase(case),
                    .switch_case_one => tree.switchCaseOne(case),
                    else => continue,
                };

                if (switch_case.payload_token) |payload| {
                    var scope = try self.scopes.addOne();
                    scope.* = .{
                        .range = .{
                            .start = ast.tokenLocation(tree, payload).start,
                            .end = ast.tokenLocation(tree, ast.lastToken(tree, switch_case.ast.target_expr)).end,
                        },
                        .decls = std.StringHashMap(Declaration).init(allocator),
                        .data = .other,
                    };
                    errdefer scope.decls.deinit();

                    // if payload is *name than get next token
                    const name_token = payload + @boolToInt(token_tags[payload] == .asterisk);
                    const name = tree.tokenSlice(name_token);

                    try scope.decls.putNoClobber(name, .{
                        .switch_payload = .{
                            .node = name_token,
                            .switch_expr = cond,
                            .items = switch_case.ast.values,
                        },
                    });
                }

                try self.makeScopeInternal(tree, switch_case.ast.target_expr);
            }
        },
        .switch_case,
        .switch_case_one,
        .switch_range,
        => {
            return;
        },
        .global_var_decl,
        .local_var_decl,
        .aligned_var_decl,
        .simple_var_decl,
        => {
            const var_decl = ast.varDecl(tree, node_idx).?;
            if (var_decl.ast.type_node != 0) {
                try self.makeScopeInternal(tree, var_decl.ast.type_node);
            }

            if (var_decl.ast.init_node != 0) {
                try self.makeScopeInternal(tree, var_decl.ast.init_node);
            }
        },
        .call,
        .call_comma,
        .call_one,
        .call_one_comma,
        .async_call,
        .async_call_comma,
        .async_call_one,
        .async_call_one_comma,
        => {
            var buf: [1]Ast.Node.Index = undefined;
            const call = ast.callFull(tree, node_idx, &buf).?;

            try self.makeScopeInternal(tree, call.ast.fn_expr);
            for (call.ast.params) |param|
                try self.makeScopeInternal(tree, param);
        },
        .struct_init,
        .struct_init_comma,
        .struct_init_dot,
        .struct_init_dot_comma,
        .struct_init_dot_two,
        .struct_init_dot_two_comma,
        .struct_init_one,
        .struct_init_one_comma,
        => {
            var buf: [2]Ast.Node.Index = undefined;
            const struct_init: Ast.full.StructInit = switch (node_tag) {
                .struct_init, .struct_init_comma => tree.structInit(node_idx),
                .struct_init_dot, .struct_init_dot_comma => tree.structInitDot(node_idx),
                .struct_init_dot_two, .struct_init_dot_two_comma => tree.structInitDotTwo(&buf, node_idx),
                .struct_init_one, .struct_init_one_comma => tree.structInitOne(buf[0..1], node_idx),
                else => unreachable,
            };

            if (struct_init.ast.type_expr != 0)
                try self.makeScopeInternal(tree, struct_init.ast.type_expr);

            for (struct_init.ast.fields) |field| {
                try self.makeScopeInternal(tree, field);
            }
        },
        .array_init,
        .array_init_comma,
        .array_init_dot,
        .array_init_dot_comma,
        .array_init_dot_two,
        .array_init_dot_two_comma,
        .array_init_one,
        .array_init_one_comma,
        => {
            var buf: [2]Ast.Node.Index = undefined;
            const array_init: Ast.full.ArrayInit = switch (node_tag) {
                .array_init, .array_init_comma => tree.arrayInit(node_idx),
                .array_init_dot, .array_init_dot_comma => tree.arrayInitDot(node_idx),
                .array_init_dot_two, .array_init_dot_two_comma => tree.arrayInitDotTwo(&buf, node_idx),
                .array_init_one, .array_init_one_comma => tree.arrayInitOne(buf[0..1], node_idx),
                else => unreachable,
            };

            if (array_init.ast.type_expr != 0)
                try self.makeScopeInternal(tree, array_init.ast.type_expr);
            for (array_init.ast.elements) |elem| {
                try self.makeScopeInternal(tree, elem);
            }
        },
        .container_field,
        .container_field_align,
        .container_field_init,
        => {
            const field = ast.containerField(tree, node_idx).?;

            try self.makeScopeInternal(tree, field.ast.type_expr);
            try self.makeScopeInternal(tree, field.ast.align_expr);
            try self.makeScopeInternal(tree, field.ast.value_expr);
        },
        .builtin_call,
        .builtin_call_comma,
        .builtin_call_two,
        .builtin_call_two_comma,
        => {
            const b_data = data[node_idx];
            const params = switch (node_tag) {
                .builtin_call, .builtin_call_comma => tree.extra_data[b_data.lhs..b_data.rhs],
                .builtin_call_two, .builtin_call_two_comma => if (b_data.lhs == 0)
                    &[_]Ast.Node.Index{}
                else if (b_data.rhs == 0)
                    &[_]Ast.Node.Index{b_data.lhs}
                else
                    &[_]Ast.Node.Index{ b_data.lhs, b_data.rhs },
                else => unreachable,
            };

            for (params) |param| {
                try self.makeScopeInternal(tree, param);
            }
        },
        .ptr_type,
        .ptr_type_aligned,
        .ptr_type_bit_range,
        .ptr_type_sentinel,
        => {
            const ptr_type: Ast.full.PtrType = ast.ptrType(tree, node_idx).?;

            try self.makeScopeInternal(tree, ptr_type.ast.sentinel);
            try self.makeScopeInternal(tree, ptr_type.ast.align_node);
            try self.makeScopeInternal(tree, ptr_type.ast.child_type);
        },
        .slice,
        .slice_open,
        .slice_sentinel,
        => {
            const slice: Ast.full.Slice = switch (node_tag) {
                .slice => tree.slice(node_idx),
                .slice_open => tree.sliceOpen(node_idx),
                .slice_sentinel => tree.sliceSentinel(node_idx),
                else => unreachable,
            };
            try self.makeScopeInternal(tree, slice.ast.sliced);
            try self.makeScopeInternal(tree, slice.ast.start);
            try self.makeScopeInternal(tree, slice.ast.end);
            try self.makeScopeInternal(tree, slice.ast.sentinel);
        },
        .@"errdefer" => {
            const expr = data[node_idx].rhs;
            if (data[node_idx].lhs != 0) {
                const payload_token = data[node_idx].lhs;
                var scope = try self.scopes.addOne();
                scope.* = .{
                    .range = .{
                        .start = ast.tokenLocation(tree, payload_token).start,
                        .end = ast.tokenLocation(tree, ast.lastToken(tree, expr)).end,
                    },
                    .decls = std.StringHashMap(Declaration).init(allocator),
                    .data = .other,
                };
                errdefer scope.decls.deinit();

                const name = tree.tokenSlice(payload_token);
                try scope.decls.putNoClobber(name, .{ .ast_node = expr });
            }

            try self.makeScopeInternal(tree, expr);
        },

        .@"asm",
        .asm_simple,
        .asm_output,
        .asm_input,
        .error_value,
        .multiline_string_literal,
        .string_literal,
        .enum_literal,
        .identifier,
        .anyframe_type,
        .anyframe_literal,
        .char_literal,
        .integer_literal,
        .float_literal,
        .unreachable_literal,
        .@"continue",
        => {},
        .@"break", .@"defer" => {
            try self.makeScopeInternal(tree, data[node_idx].rhs);
        },

        .@"return",
        .@"resume",
        .field_access,
        .@"suspend",
        .deref,
        .@"try",
        .@"await",
        .optional_type,
        .@"comptime",
        .@"nosuspend",
        .bool_not,
        .negation,
        .bit_not,
        .negation_wrap,
        .address_of,
        .grouped_expression,
        .unwrap_optional,
        .@"usingnamespace",
        => {
            try self.makeScopeInternal(tree, data[node_idx].lhs);
        },

        .equal_equal,
        .bang_equal,
        .less_than,
        .greater_than,
        .less_or_equal,
        .greater_or_equal,
        .assign_mul,
        .assign_div,
        .assign_mod,
        .assign_add,
        .assign_sub,
        .assign_shl,
        .assign_shr,
        .assign_bit_and,
        .assign_bit_xor,
        .assign_bit_or,
        .assign_mul_wrap,
        .assign_add_wrap,
        .assign_sub_wrap,
        .assign_mul_sat,
        .assign_add_sat,
        .assign_sub_sat,
        .assign_shl_sat,
        .assign,
        .merge_error_sets,
        .mul,
        .div,
        .mod,
        .array_mult,
        .mul_wrap,
        .mul_sat,
        .add,
        .sub,
        .array_cat,
        .add_wrap,
        .sub_wrap,
        .add_sat,
        .sub_sat,
        .shl,
        .shl_sat,
        .shr,
        .bit_and,
        .bit_xor,
        .bit_or,
        .@"orelse",
        .bool_and,
        .bool_or,
        .array_type,
        .array_access,
        .error_union,
        => {
            try self.makeScopeInternal(tree, data[node_idx].lhs);
            try self.makeScopeInternal(tree, data[node_idx].rhs);
        },
    }
}

pub fn debugPrint(self: Self) void {
    for (self.scopes) |scope| {
        logger.debug(
            \\--------------------------
            \\Scope {}, range: [{d}, {d})
            \\ {d} usingnamespaces
            \\Decls:
        , .{
            scope.data,
            scope.range.start,
            scope.range.end,
            scope.uses.len,
        });

        var decl_it = scope.decls.iterator();
        var idx: usize = 0;
        while (decl_it.next()) |_| : (idx += 1) {
            if (idx != 0) logger.debug(", ", .{});
        }
        // logger.debug("{s}", .{name_decl.key});
        logger.debug("\n--------------------------\n", .{});
    }
}

pub fn innermostBlockScopeIndex(self: Self, source_index: usize) usize {
    if (self.scopes.items.len == 1) return 0;

    var current: usize = 0;
    for (self.scopes.items[1..]) |*scope, idx| {
        if (source_index >= scope.range.start and source_index <= scope.range.end) {
            switch (scope.data) {
                .container, .function, .block => current = idx + 1,
                else => {},
            }
        }
        if (scope.range.start > source_index) break;
    }
    return current;
}

pub fn innermostBlockScope(self: Self, source_index: usize) Ast.Node.Index {
    return self.scopes.items[self.innermostBlockScopeIndex(source_index)].toNodeIndex().?;
}

pub fn findContainerScope(self: Self, container: Ast.Node.Index) ?*Scope {
    // if (!ast.isContainer(handle.ast_context.tree, container)) return null;

    // Find the container scope.
    return for (self.scopes.items) |*scope| {
        switch (scope.data) {
            .container => |node| if (node == container) {
                break scope;
            },
            else => {},
        }
    } else null;
}
