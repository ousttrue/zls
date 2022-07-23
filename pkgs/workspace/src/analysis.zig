const std = @import("std");
const Scope = @import("./Scope.zig");
const Declaration = Scope.Declaration;
const Workspace = @import("./Workspace.zig");
const Document = @import("./Document.zig");
const Config = @import("./Config.zig");
const ClientCapabilities = @import("./ClientCapabilities.zig");
const Ast = std.zig.Ast;
const TypeWithHandle = @import("./TypeWithHandle.zig");
const lsp = @import("lsp");
const offsets = @import("./offsets.zig");
const logger = std.log.scoped(.analysis);
const ast = @import("./ast.zig");
const DeclWithHandle = @import("./DeclWithHandle.zig");

/// Gets a function's keyword, name, arguments and return value.
pub fn getFunctionSignature(tree: Ast, func: Ast.full.FnProto) []const u8 {
    const start = offsets.tokenLocation(tree, func.ast.fn_token);

    const end = if (func.ast.return_type != 0)
        offsets.tokenLocation(tree, ast.lastToken(tree, func.ast.return_type))
    else
        start;
    return tree.source[start.start..end.end];
}

pub fn getVariableSignature(tree: Ast, var_decl: Ast.full.VarDecl) []const u8 {
    const start = offsets.tokenLocation(tree, var_decl.ast.mut_token).start;
    const end = offsets.tokenLocation(tree, ast.lastToken(tree, var_decl.ast.init_node)).end;
    return tree.source[start..end];
}

pub fn getContainerFieldSignature(tree: Ast, field: Ast.full.ContainerField) []const u8 {
    const start = offsets.tokenLocation(tree, field.ast.name_token).start;
    const end_node = if (field.ast.value_expr != 0) field.ast.value_expr else field.ast.type_expr;
    const end = offsets.tokenLocation(tree, ast.lastToken(tree, end_node)).end;
    return tree.source[start..end];
}

// ANALYSIS ENGINE
fn isContainerDecl(decl_handle: DeclWithHandle) bool {
    return switch (decl_handle.decl.*) {
        .ast_node => |inner_node| ast.isContainer(decl_handle.handle.tree.nodes.items(.tag)[inner_node]),
        else => false,
    };
}

pub const BoundTypeParams = std.AutoHashMap(Ast.full.FnProto.Param, TypeWithHandle);

pub const FieldAccessReturn = struct {
    original: TypeWithHandle,
    unwrapped: ?TypeWithHandle = null,
};

pub fn getFieldAccessType(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    handle: *Document,
    source_index: usize,
    tokenizer: *std.zig.Tokenizer,
) !?FieldAccessReturn {
    var current_type = TypeWithHandle.typeVal(handle, undefined);

    var bound_type_params = BoundTypeParams.init(arena.allocator());

    while (true) {
        const tok = tokenizer.next();
        switch (tok.tag) {
            .eof => return FieldAccessReturn{
                .original = current_type,
                .unwrapped = try TypeWithHandle.resolveDerefType(arena, workspace, current_type, &bound_type_params),
            },
            .identifier => {
                if (try workspace.lookupSymbolGlobal(
                    arena,
                    current_type.handle,
                    tokenizer.buffer[tok.loc.start..tok.loc.end],
                    source_index,
                )) |child| {
                    if (try child.resolveType(arena, workspace, &bound_type_params)) |child_type| {
                        current_type = child_type;
                    } else {
                        logger.warn("fail to child.resolveType: {}", .{child.decl});
                        return null;
                    }
                } else {
                    logger.warn("fail to lookupSymbolGlobal: {}", .{tok});
                    return null;
                }
            },
            .period => {
                const after_period = tokenizer.next();
                switch (after_period.tag) {
                    .eof => {
                        // function labels cannot be dot accessed
                        if (current_type.isFunc()) return null;
                        return FieldAccessReturn{
                            .original = current_type,
                            .unwrapped = try TypeWithHandle.resolveDerefType(arena, workspace, current_type, &bound_type_params),
                        };
                    },
                    .identifier => {
                        if (after_period.loc.end == tokenizer.buffer.len) {
                            return FieldAccessReturn{
                                .original = current_type,
                                .unwrapped = try TypeWithHandle.resolveDerefType(arena, workspace, current_type, &bound_type_params),
                            };
                        }

                        current_type = try TypeWithHandle.resolveFieldAccessLhsType(arena, workspace, current_type, &bound_type_params);
                        const current_type_node = switch (current_type.type.data) {
                            .other => |n| n,
                            else => return null,
                        };

                        if (try DeclWithHandle.lookupSymbolContainer(
                            arena,
                            workspace,
                            current_type.handle,
                            current_type_node,
                            tokenizer.buffer[after_period.loc.start..after_period.loc.end],
                            !current_type.type.is_type_val,
                        )) |child| {
                            current_type = (try child.resolveType(
                                arena,
                                workspace,
                                &bound_type_params,
                            )) orelse return null;
                        } else return null;
                    },
                    .question_mark => {
                        current_type = (try TypeWithHandle.resolveUnwrapOptionalType(
                            arena,
                            workspace,
                            current_type,
                            &bound_type_params,
                        )) orelse return null;
                    },
                    else => {
                        logger.debug("Unrecognized token {} after period.", .{after_period.tag});
                        return null;
                    },
                }
            },
            .period_asterisk => {
                current_type = (try TypeWithHandle.resolveDerefType(
                    arena,
                    workspace,
                    current_type,
                    &bound_type_params,
                )) orelse return null;
            },
            .l_paren => {
                const current_type_node = switch (current_type.type.data) {
                    .other => |n| n,
                    else => return null,
                };

                // Can't call a function type, we need a function type instance.
                if (current_type.type.is_type_val) return null;
                const cur_tree = current_type.handle.tree;
                var buf: [1]Ast.Node.Index = undefined;
                if (ast.fnProto(cur_tree, current_type_node, &buf)) |func| {
                    // Check if the function has a body and if so, pass it
                    // so the type can be resolved if it's a generic function returning
                    // an anonymous struct
                    const has_body = cur_tree.nodes.items(.tag)[current_type_node] == .fn_decl;
                    const body = cur_tree.nodes.items(.data)[current_type_node].rhs;

                    // TODO Actually bind params here when calling functions instead of just skipping args.
                    if (try TypeWithHandle.resolveReturnType(arena, workspace, func, current_type.handle, &bound_type_params, if (has_body) body else null)) |ret| {
                        current_type = ret;
                        // Skip to the right paren
                        var paren_count: usize = 1;
                        var next = tokenizer.next();
                        while (next.tag != .eof) : (next = tokenizer.next()) {
                            if (next.tag == .r_paren) {
                                paren_count -= 1;
                                if (paren_count == 0) break;
                            } else if (next.tag == .l_paren) {
                                paren_count += 1;
                            }
                        } else return null;
                    } else return null;
                } else return null;
            },
            .l_bracket => {
                var brack_count: usize = 1;
                var next = tokenizer.next();
                var is_range = false;
                while (next.tag != .eof) : (next = tokenizer.next()) {
                    if (next.tag == .r_bracket) {
                        brack_count -= 1;
                        if (brack_count == 0) break;
                    } else if (next.tag == .l_bracket) {
                        brack_count += 1;
                    } else if (next.tag == .ellipsis2 and brack_count == 1) {
                        is_range = true;
                    }
                } else return null;

                current_type = (try TypeWithHandle.resolveBracketAccessType(arena, workspace, current_type, if (is_range) .Range else .Single, &bound_type_params)) orelse return null;
            },
            else => {
                logger.debug("Unimplemented token: {}", .{tok.tag});
                return null;
            },
        }
    }

    return FieldAccessReturn{
        .original = current_type,
        .unwrapped = try TypeWithHandle.resolveDerefType(arena, current_type, &bound_type_params),
    };
}

pub fn nodeToString(tree: Ast, node: Ast.Node.Index) ?[]const u8 {
    const data = tree.nodes.items(.data);
    const main_token = tree.nodes.items(.main_token)[node];
    var buf: [1]Ast.Node.Index = undefined;
    switch (tree.nodes.items(.tag)[node]) {
        .container_field => return tree.tokenSlice(tree.containerField(node).ast.name_token),
        .container_field_init => return tree.tokenSlice(tree.containerFieldInit(node).ast.name_token),
        .container_field_align => return tree.tokenSlice(tree.containerFieldAlign(node).ast.name_token),
        .error_value => return tree.tokenSlice(data[node].rhs),
        .identifier => return tree.tokenSlice(main_token),
        .fn_proto,
        .fn_proto_multi,
        .fn_proto_one,
        .fn_proto_simple,
        .fn_decl,
        => if (ast.fnProto(tree, node, &buf).?.name_token) |name|
            return tree.tokenSlice(name),
        .field_access => return tree.tokenSlice(data[node].rhs),
        .call,
        .call_comma,
        .async_call,
        .async_call_comma,
        => return tree.tokenSlice(tree.callFull(node).ast.lparen - 1),
        .call_one,
        .call_one_comma,
        .async_call_one,
        .async_call_one_comma,
        => return tree.tokenSlice(tree.callOne(&buf, node).ast.lparen - 1),
        .test_decl => if (data[node].lhs != 0)
            return tree.tokenSlice(data[node].lhs),
        else => |tag| logger.debug("INVALID: {}", .{tag}),
    }

    return null;
}

pub const SourceRange = std.zig.Token.Loc;

fn iterateSymbolsContainerInternal(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    handle: *Document,
    container: Ast.Node.Index,
    orig_handle: *Document,
    comptime callback: anytype,
    context: anytype,
    instance_access: bool,
    use_trail: *std.ArrayList(*const Ast.Node.Index),
    config: *Config,
    client_capabilities: *ClientCapabilities,
) error{OutOfMemory}!void {
    const tree = handle.tree;
    const node_tags = tree.nodes.items(.tag);
    const token_tags = tree.tokens.items(.tag);
    const main_token = tree.nodes.items(.main_token)[container];

    const is_enum = token_tags[main_token] == .keyword_enum;

    const container_scope = Scope.findContainerScope(handle, container) orelse return;

    var decl_it = container_scope.decls.iterator();
    while (decl_it.next()) |entry| {
        switch (entry.value_ptr.*) {
            .ast_node => |node| {
                if (node_tags[node].isContainerField()) {
                    if (!instance_access and !is_enum) continue;
                    if (instance_access and is_enum) continue;
                } else if (node_tags[node] == .global_var_decl or
                    node_tags[node] == .local_var_decl or
                    node_tags[node] == .simple_var_decl or
                    node_tags[node] == .aligned_var_decl)
                {
                    if (instance_access) continue;
                }
            },
            .label_decl => continue,
            else => {},
        }

        const decl = DeclWithHandle{ .decl = entry.value_ptr, .handle = handle };
        if (handle != orig_handle and !decl.isPublic()) continue;
        try callback(arena, workspace, context, decl, config, client_capabilities);
    }

    for (container_scope.uses) |use| {
        const use_token = tree.nodes.items(.main_token)[use.*];
        const is_pub = use_token > 0 and token_tags[use_token - 1] == .keyword_pub;
        if (handle != orig_handle and !is_pub) continue;
        if (std.mem.indexOfScalar(*const Ast.Node.Index, use_trail.items, use) != null) continue;
        try use_trail.append(use);

        const lhs = tree.nodes.items(.data)[use.*].lhs;
        const use_expr = (try TypeWithHandle.resolveTypeOfNode(arena, workspace, handle, lhs)) orelse continue;

        const use_expr_node = switch (use_expr.type.data) {
            .other => |n| n,
            else => continue,
        };
        try iterateSymbolsContainerInternal(
            arena,
            workspace,
            use_expr.handle,
            use_expr_node,
            orig_handle,
            callback,
            context,
            false,
            use_trail,
            config,
            client_capabilities,
        );
    }
}

pub fn iterateSymbolsContainer(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    handle: *Document,
    container: Ast.Node.Index,
    orig_handle: *Document,
    comptime callback: anytype,
    context: anytype,
    instance_access: bool,
    config: *Config,
    client_capabilities: *ClientCapabilities,
) error{OutOfMemory}!void {
    var use_trail = std.ArrayList(*const Ast.Node.Index).init(arena.allocator());
    return try iterateSymbolsContainerInternal(
        arena,
        workspace,
        handle,
        container,
        orig_handle,
        callback,
        context,
        instance_access,
        &use_trail,
        config,
        client_capabilities,
    );
}

pub fn iterateLabels(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    handle: *Document,
    source_index: usize,
    comptime callback: anytype,
    context: anytype,
    config: *Config,
    client_capabilities: *ClientCapabilities,
) error{OutOfMemory}!void {
    for (handle.document_scope.scopes) |scope| {
        if (source_index >= scope.range.start and source_index < scope.range.end) {
            var decl_it = scope.decls.iterator();
            while (decl_it.next()) |entry| {
                switch (entry.value_ptr.*) {
                    .label_decl => {},
                    else => continue,
                }
                try callback(arena, workspace, context, DeclWithHandle{ .decl = entry.value_ptr, .handle = handle }, config, client_capabilities);
            }
        }
        if (scope.range.start >= source_index) return;
    }
}

fn iterateSymbolsGlobalInternal(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    handle: *Document,
    source_index: usize,
    comptime callback: anytype,
    context: anytype,
    use_trail: *std.ArrayList(*const Ast.Node.Index),
    config: *Config,
    client_capabilities: *ClientCapabilities,
) error{OutOfMemory}!void {
    for (handle.document_scope.scopes) |scope| {
        if (source_index >= scope.range.start and source_index <= scope.range.end) {
            var decl_it = scope.decls.iterator();
            while (decl_it.next()) |entry| {
                if (entry.value_ptr.* == .ast_node and
                    handle.tree.nodes.items(.tag)[entry.value_ptr.*.ast_node].isContainerField()) continue;
                if (entry.value_ptr.* == .label_decl) continue;
                try callback(arena, workspace, context, DeclWithHandle{ .decl = entry.value_ptr, .handle = handle }, config, client_capabilities);
            }

            for (scope.uses) |use| {
                if (std.mem.indexOfScalar(*const Ast.Node.Index, use_trail.items, use) != null) continue;
                try use_trail.append(use);

                const use_expr = (try TypeWithHandle.resolveTypeOfNode(
                    arena,
                    workspace,
                    handle,
                    handle.tree.nodes.items(.data)[use.*].lhs,
                )) orelse continue;
                const use_expr_node = switch (use_expr.type.data) {
                    .other => |n| n,
                    else => continue,
                };
                try iterateSymbolsContainerInternal(
                    arena,
                    workspace,
                    use_expr.handle,
                    use_expr_node,
                    handle,
                    callback,
                    context,
                    false,
                    use_trail,
                    config,
                    client_capabilities,
                );
            }
        }

        if (scope.range.start >= source_index) return;
    }
}

pub fn iterateSymbolsGlobal(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    handle: *Document,
    source_index: usize,
    comptime callback: anytype,
    context: anytype,
    config: *Config,
    client_capabilities: *ClientCapabilities,
) error{OutOfMemory}!void {
    var use_trail = std.ArrayList(*const Ast.Node.Index).init(arena.allocator());
    return try iterateSymbolsGlobalInternal(arena, workspace, handle, source_index, callback, context, &use_trail, config, client_capabilities);
}

pub fn innermostBlockScopeIndex(handle: Document, source_index: usize) usize {
    if (handle.document_scope.scopes.len == 1) return 0;

    var current: usize = 0;
    for (handle.document_scope.scopes[1..]) |*scope, idx| {
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

pub fn innermostBlockScope(handle: Document, source_index: usize) Ast.Node.Index {
    return handle.document_scope.scopes[innermostBlockScopeIndex(handle, source_index)].toNodeIndex().?;
}

pub fn lookupLabel(handle: *Document, symbol: []const u8, source_index: usize) error{OutOfMemory}!?DeclWithHandle {
    for (handle.document_scope.scopes) |scope| {
        if (source_index >= scope.range.start and source_index < scope.range.end) {
            if (scope.decls.getEntry(symbol)) |candidate| {
                switch (candidate.value_ptr.*) {
                    .label_decl => {},
                    else => continue,
                }
                return DeclWithHandle{
                    .decl = candidate.value_ptr,
                    .handle = handle,
                };
            }
        }
        if (scope.range.start > source_index) return null;
    }
    return null;
}
