const std = @import("std");
const lsp = @import("lsp");
const Workspace = @import("./Workspace.zig");
const Document = @import("./Document.zig");
const DocumentScope = @import("./DocumentScope.zig");
const Scope = @import("./Scope.zig");
const Config = @import("./Config.zig");
const ClientCapabilities = @import("./ClientCapabilities.zig");
const TypeWithHandle = @import("./TypeWithHandle.zig");
const DeclWithHandle = @import("./DeclWithHandle.zig");
const FieldAccessReturn = @import("./FieldAccessReturn.zig");
const ast = @import("./ast.zig");
const Ast = std.zig.Ast;
const builtin_completions = @import("./builtin_completions.zig");
const logger = std.log.scoped(.Completion);

fn typeToCompletion(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    list: *std.ArrayList(lsp.CompletionItem),
    field_access: FieldAccessReturn,
    orig_handle: *Document,
    config: *Config,
    client_capabilities: *ClientCapabilities,
) error{OutOfMemory}!void {
    const type_handle = field_access.original;
    switch (type_handle.type.data) {
        .slice => {
            if (!type_handle.type.is_type_val) {
                try list.append(.{
                    .label = "len",
                    .kind = .Field,
                    .insertText = "len",
                    .insertTextFormat = .PlainText,
                });
                try list.append(.{
                    .label = "ptr",
                    .kind = .Field,
                    .insertText = "ptr",
                    .insertTextFormat = .PlainText,
                });
            }
        },
        .error_union => {},
        .pointer => |n| {
            if (config.operator_completions) {
                try list.append(.{
                    .label = "*",
                    .kind = .Operator,
                    .insertText = "*",
                    .insertTextFormat = .PlainText,
                });
            }
            try nodeToCompletion(
                arena,
                workspace,
                type_handle.handle,
                n,
                list,
                null,
                orig_handle,
                type_handle.type.is_type_val,
                null,
                config,
                client_capabilities,
            );
        },
        .other => |n| try nodeToCompletion(
            arena,
            workspace,
            type_handle.handle,
            n,
            list,
            field_access.unwrapped,
            orig_handle,
            type_handle.type.is_type_val,
            null,
            config,
            client_capabilities,
        ),
        .primitive => {},
    }
}

const DeclToCompletionContext = struct {
    completions: *std.ArrayList(lsp.CompletionItem),
    config: *const Config,
    arena: *std.heap.ArenaAllocator,
    orig_handle: *Document,
    parent_is_type_val: ?bool = null,
};

/// Creates snippet insert text for a function. Caller owns returned memory.
pub fn getFunctionSnippet(allocator: std.mem.Allocator, tree: Ast, func: Ast.full.FnProto, skip_self_param: bool) ![]const u8 {
    const name_index = func.name_token.?;

    var buffer = std.ArrayList(u8).init(allocator);
    try buffer.ensureTotalCapacity(128);

    try buffer.appendSlice(tree.tokenSlice(name_index));
    try buffer.append('(');

    var buf_stream = buffer.writer();

    const token_tags = tree.tokens.items(.tag);

    var it = func.iterate(&tree);
    var i: usize = 0;
    while (it.next()) |param| : (i += 1) {
        if (skip_self_param and i == 0) continue;
        if (i != @boolToInt(skip_self_param))
            try buffer.appendSlice(", ${")
        else
            try buffer.appendSlice("${");

        try buf_stream.print("{d}:", .{i + 1});

        if (param.comptime_noalias) |token_index| {
            if (token_tags[token_index] == .keyword_comptime)
                try buffer.appendSlice("comptime ")
            else
                try buffer.appendSlice("noalias ");
        }

        if (param.name_token) |name_token| {
            try buffer.appendSlice(tree.tokenSlice(name_token));
            try buffer.appendSlice(": ");
        }

        if (param.anytype_ellipsis3) |token_index| {
            if (token_tags[token_index] == .keyword_anytype)
                try buffer.appendSlice("anytype")
            else
                try buffer.appendSlice("...");
        } else if (param.type_expr != 0) {
            var curr_token = tree.firstToken(param.type_expr);
            var end_token = ast.lastToken(tree, param.type_expr);
            while (curr_token <= end_token) : (curr_token += 1) {
                const tag = token_tags[curr_token];
                const is_comma = tag == .comma;

                if (curr_token == end_token and is_comma) continue;
                try buffer.appendSlice(tree.tokenSlice(curr_token));
                if (is_comma or tag == .keyword_const) try buffer.append(' ');
            }
        } else unreachable;

        try buffer.append('}');
    }
    try buffer.append(')');

    return buffer.toOwnedSlice();
}

pub fn iterateSymbolsContainerInternal(
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

fn nodeToCompletion(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    handle: *Document,
    node: Ast.Node.Index,
    list: *std.ArrayList(lsp.CompletionItem),
    unwrapped: ?TypeWithHandle,
    orig_handle: *Document,
    is_type_val: bool,
    parent_is_type_val: ?bool,
    config: *Config,
    client_capabilities: *ClientCapabilities,
) error{OutOfMemory}!void {
    const tree = handle.tree;
    const node_tags = tree.nodes.items(.tag);
    const token_tags = tree.tokens.items(.tag);

    const doc_kind: ast.MarkupFormat = if (client_capabilities.completion_doc_supports_md)
        .Markdown
    else
        .PlainText;

    const doc = if (try ast.getDocComments(
        list.allocator,
        handle.tree,
        node,
        doc_kind,
    )) |doc_comments|
        lsp.MarkupContent.init(client_capabilities.completion_doc_supports_md, doc_comments)
    else
        null;

    if (ast.isContainer(handle.tree, node)) {
        const context = DeclToCompletionContext{
            .completions = list,
            .config = config,
            .arena = arena,
            .orig_handle = orig_handle,
            .parent_is_type_val = is_type_val,
        };
        try iterateSymbolsContainer(
            arena,
            workspace,
            handle,
            node,
            orig_handle,
            declToCompletion,
            context,
            !is_type_val,
            config,
            client_capabilities,
        );
    }

    if (is_type_val) return;

    switch (node_tags[node]) {
        .fn_proto,
        .fn_proto_multi,
        .fn_proto_one,
        .fn_proto_simple,
        .fn_decl,
        => {
            var buf: [1]Ast.Node.Index = undefined;
            const func = ast.fnProto(tree, node, &buf).?;
            if (func.name_token) |name_token| {
                const use_snippets = config.enable_snippets and client_capabilities.supports_snippets;
                const insert_text = if (use_snippets) blk: {
                    const skip_self_param = !(parent_is_type_val orelse true) and
                        try TypeWithHandle.hasSelfParam(arena, workspace, handle, func);
                    break :blk try getFunctionSnippet(arena.allocator(), tree, func, skip_self_param);
                } else tree.tokenSlice(func.name_token.?);

                const is_type_function = TypeWithHandle.isTypeFunction(handle.tree, func);

                try list.append(.{
                    .label = handle.tree.tokenSlice(name_token),
                    .kind = if (is_type_function) .Struct else .Function,
                    .documentation = doc,
                    .detail = ast.getFunctionSignature(handle.tree, func),
                    .insertText = insert_text,
                    .insertTextFormat = if (use_snippets) .Snippet else .PlainText,
                });
            }
        },
        .global_var_decl,
        .local_var_decl,
        .aligned_var_decl,
        .simple_var_decl,
        => {
            const var_decl = ast.varDecl(tree, node).?;
            const is_const = token_tags[var_decl.ast.mut_token] == .keyword_const;

            if (try DeclWithHandle.resolveVarDeclAlias(arena, workspace, handle, node)) |result| {
                const context = DeclToCompletionContext{
                    .completions = list,
                    .config = config,
                    .arena = arena,
                    .orig_handle = orig_handle,
                };
                return try declToCompletion(arena, workspace, context, result, config, client_capabilities);
            }

            try list.append(.{
                .label = handle.tree.tokenSlice(var_decl.ast.mut_token + 1),
                .kind = if (is_const) .Constant else .Variable,
                .documentation = doc,
                .detail = ast.getVariableSignature(tree, var_decl),
                .insertText = tree.tokenSlice(var_decl.ast.mut_token + 1),
                .insertTextFormat = .PlainText,
            });
        },
        .container_field,
        .container_field_align,
        .container_field_init,
        => {
            const field = ast.containerField(tree, node).?;
            try list.append(.{
                .label = handle.tree.tokenSlice(field.ast.name_token),
                .kind = .Field,
                .documentation = doc,
                .detail = ast.getContainerFieldSignature(handle.tree, field),
                .insertText = tree.tokenSlice(field.ast.name_token),
                .insertTextFormat = .PlainText,
            });
        },
        .array_type,
        .array_type_sentinel,
        => {
            try list.append(.{
                .label = "len",
                .kind = .Field,
                .insertText = "len",
                .insertTextFormat = .PlainText,
            });
        },
        .ptr_type,
        .ptr_type_aligned,
        .ptr_type_bit_range,
        .ptr_type_sentinel,
        => {
            const ptr_type = ast.ptrType(tree, node).?;

            switch (ptr_type.size) {
                .One, .C, .Many => if (config.operator_completions) {
                    try list.append(.{
                        .label = "*",
                        .kind = .Operator,
                        .insertText = "*",
                        .insertTextFormat = .PlainText,
                    });
                },
                .Slice => {
                    try list.append(.{
                        .label = "ptr",
                        .kind = .Field,
                        .insertText = "ptr",
                        .insertTextFormat = .PlainText,
                    });
                    try list.append(.{
                        .label = "len",
                        .kind = .Field,
                        .insertText = "len",
                        .insertTextFormat = .PlainText,
                    });
                    return;
                },
            }

            if (unwrapped) |actual_type| {
                try typeToCompletion(
                    arena,
                    workspace,
                    list,
                    .{ .original = actual_type },
                    orig_handle,
                    config,
                    client_capabilities,
                );
            }
            return;
        },
        .optional_type => {
            if (config.operator_completions) {
                try list.append(.{
                    .label = "?",
                    .kind = .Operator,
                    .insertText = "?",
                    .insertTextFormat = .PlainText,
                });
            }
            return;
        },
        .string_literal => {
            try list.append(.{
                .label = "len",
                .kind = .Field,
                .insertText = "len",
                .insertTextFormat = .PlainText,
            });
        },
        else => if (ast.nodeToString(tree, node)) |string| {
            try list.append(.{
                .label = string,
                .kind = .Field,
                .documentation = doc,
                .detail = tree.getNodeSource(node),
                .insertText = string,
                .insertTextFormat = .PlainText,
            });
        },
    }
}

fn declToCompletion(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    context: DeclToCompletionContext,
    decl_handle: DeclWithHandle,
    config: *Config,
    client_capabilities: *ClientCapabilities,
) !void {
    const tree = decl_handle.handle.tree;
    switch (decl_handle.decl.*) {
        .ast_node => |node| try nodeToCompletion(
            arena,
            workspace,
            decl_handle.handle,
            node,
            context.completions,
            null,
            context.orig_handle,
            false,
            context.parent_is_type_val,
            config,
            client_capabilities,
        ),
        .param_decl => |param| {
            const doc_kind: ast.MarkupFormat = if (client_capabilities.completion_doc_supports_md) .Markdown else .PlainText;
            const doc = if (param.first_doc_comment) |doc_comments|
                lsp.MarkupContent.init(
                    client_capabilities.completion_doc_supports_md,
                    try ast.collectDocComments(context.arena.allocator(), tree, doc_comments, doc_kind, false),
                )
            else
                null;

            const first_token = param.first_doc_comment orelse
                param.comptime_noalias orelse
                param.name_token orelse
                tree.firstToken(param.type_expr);
            const last_token = param.anytype_ellipsis3 orelse tree.lastToken(param.type_expr);

            try context.completions.append(.{
                .label = tree.tokenSlice(param.name_token.?),
                .kind = .Constant,
                .documentation = doc,
                .detail = tree.source[ast.tokenLocation(tree, first_token).start..ast.tokenLocation(tree, last_token).end],
                .insertText = tree.tokenSlice(param.name_token.?),
                .insertTextFormat = .PlainText,
            });
        },
        .pointer_payload => |payload| {
            try context.completions.append(.{
                .label = tree.tokenSlice(payload.name),
                .kind = .Variable,
                .insertText = tree.tokenSlice(payload.name),
                .insertTextFormat = .PlainText,
            });
        },
        .array_payload => |payload| {
            try context.completions.append(.{
                .label = tree.tokenSlice(payload.identifier),
                .kind = .Variable,
                .insertText = tree.tokenSlice(payload.identifier),
                .insertTextFormat = .PlainText,
            });
        },
        .array_index => |payload| {
            try context.completions.append(.{
                .label = tree.tokenSlice(payload),
                .kind = .Variable,
                .insertText = tree.tokenSlice(payload),
                .insertTextFormat = .PlainText,
            });
        },
        .switch_payload => |payload| {
            try context.completions.append(.{
                .label = tree.tokenSlice(payload.node),
                .kind = .Variable,
                .insertText = tree.tokenSlice(payload.node),
                .insertTextFormat = .PlainText,
            });
        },
        .label_decl => |label_decl| {
            try context.completions.append(.{
                .label = tree.tokenSlice(label_decl),
                .kind = .Variable,
                .insertText = tree.tokenSlice(label_decl),
                .insertTextFormat = .PlainText,
            });
        },
    }
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

fn completeLabel(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    pos_index: usize,
    handle: *Document,
    config: *Config,
    client_capabilities: *ClientCapabilities,
) ![]lsp.CompletionItem {
    var completions = std.ArrayList(lsp.CompletionItem).init(arena.allocator());

    const context = DeclToCompletionContext{
        .completions = &completions,
        .config = config,
        .arena = arena,
        .orig_handle = handle,
    };
    try iterateLabels(arena, workspace, handle, pos_index, declToCompletion, context, config, client_capabilities);
    builtin_completions.truncateCompletions(completions.items, config.max_detail_length);
    return completions.items;
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

fn completeGlobal(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    pos_index: usize,
    handle: *Document,
    config: *Config,
    client_capabilities: *ClientCapabilities,
) ![]lsp.CompletionItem {
    var completions = std.ArrayList(lsp.CompletionItem).init(arena.allocator());

    const context = DeclToCompletionContext{
        .completions = &completions,
        .config = config,
        .arena = arena,
        .orig_handle = handle,
    };
    try iterateSymbolsGlobal(arena, workspace, handle, pos_index, declToCompletion, context, config, client_capabilities);
    builtin_completions.truncateCompletions(completions.items, config.max_detail_length);
    return completions.items;
}

fn completeFieldAccess(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    doc: *Document,
    byte_position: u32,
    range: std.zig.Token.Loc,
    config: *Config,
    client_capabilities: *ClientCapabilities,
) ![]lsp.CompletionItem {
    const allocator = arena.allocator();
    var copy = try allocator.dupeZ(u8, doc.utf8_buffer.text[range.start..range.end]);
    defer allocator.free(copy);
    var tokenizer = std.zig.Tokenizer.init(copy);

    var completions = std.ArrayList(lsp.CompletionItem).init(arena.allocator());
    if (try FieldAccessReturn.getFieldAccessType(arena, workspace, doc, byte_position, &tokenizer)) |result| {
        try typeToCompletion(arena, workspace, &completions, result, doc, config, client_capabilities);
        builtin_completions.truncateCompletions(completions.items, config.max_detail_length);
    }
    return completions.items;
}

pub fn tagStoreCompletionItems(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    base: *Document,
    comptime name: []const u8,
) ![]lsp.CompletionItem {
    // TODO Better solution for deciding what tags to include
    var max_len: usize = @field(base.document_scope, name).count();
    for (base.imports_used.items) |uri| {
        max_len += @field(workspace.handles.get(uri).?.document_scope, name).count();
    }

    var result_set = DocumentScope.CompletionSet{};
    try result_set.ensureTotalCapacity(arena.allocator(), max_len);
    for (@field(base.document_scope, name).entries.items(.key)) |completion| {
        result_set.putAssumeCapacityNoClobber(completion, {});
    }

    for (base.imports_used.items) |uri| {
        const curr_set = &@field(workspace.handles.get(uri).?.document_scope, name);
        for (curr_set.entries.items(.key)) |completion| {
            result_set.putAssumeCapacity(completion, {});
        }
    }
    return result_set.entries.items(.key);
}

fn completeError(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    doc: *Document,
    config: *Config,
) ![]lsp.CompletionItem {
    const completions = try tagStoreCompletionItems(arena, workspace, doc, "error_completions");
    builtin_completions.truncateCompletions(completions, config.max_detail_length);
    logger.debug("Completing error:", .{});
    return completions;
}

fn completeDot(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    doc: *Document,
    config: *Config,
) ![]lsp.CompletionItem {
    var completions = try tagStoreCompletionItems(arena, workspace, doc, "enum_completions");
    builtin_completions.truncateCompletions(completions, config.max_detail_length);
    return completions;
}

pub fn process(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    doc: *Document,
    byte_position: u32,
    config: *Config,
    client_capabilities: *ClientCapabilities,
) ![]const lsp.CompletionItem {
    const pos_context = doc.getPositionContext(byte_position);
    switch (pos_context) {
        .builtin => {
            logger.debug("[completion][builtin]", .{});
            return builtin_completions.completeBuiltin();
        },
        .var_access, .empty => {
            logger.debug("[completion][global]", .{});
            return try completeGlobal(arena, workspace, byte_position, doc, config, client_capabilities);
        },
        .field_access => |range| {
            logger.debug("[completion][field_access]", .{});
            return try completeFieldAccess(arena, workspace, doc, byte_position, range, config, client_capabilities);
        },
        .global_error_set => {
            logger.debug("[completion][global_error_set]", .{});
            return try completeError(arena, workspace, doc, config);
        },
        .enum_literal => {
            logger.debug("[completion][enum_literal]", .{});
            return try completeDot(arena, workspace, doc, config);
        },
        .label => {
            logger.debug("[completion][label]", .{});
            return try completeLabel(arena, workspace, byte_position, doc, config, client_capabilities);
        },
        else => {
            return &[_]lsp.CompletionItem{};
        },
    }
}
