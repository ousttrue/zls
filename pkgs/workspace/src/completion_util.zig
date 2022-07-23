const std = @import("std");
const lsp = @import("lsp");
const Workspace = @import("./Workspace.zig");
const Document = @import("./Document.zig");
const DocumentPosition = @import("./DocumentPosition.zig");
const Config = @import("./Config.zig");
const ClientCapabilities = @import("./ClientCapabilities.zig");
const analysis = @import("./analysis.zig");
const TypeWithHandle = @import("./TypeWithHandle.zig");
const DeclWithHandle = @import("./DeclWithHandle.zig");
const ast = @import("./ast.zig");
const Ast = std.zig.Ast;
const offsets = @import("./offsets.zig");
const position_context = @import("./position_context.zig");
const builtin_completions = @import("./builtin_completions.zig");
const logger = std.log.scoped(.Completion);

fn typeToCompletion(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    list: *std.ArrayList(lsp.CompletionItem),
    field_access: analysis.FieldAccessReturn,
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
        try analysis.iterateSymbolsContainer(
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
                    .detail = analysis.getFunctionSignature(handle.tree, func),
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
                .detail = analysis.getVariableSignature(tree, var_decl),
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
                .detail = analysis.getContainerFieldSignature(handle.tree, field),
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
        else => if (analysis.nodeToString(tree, node)) |string| {
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
                .detail = tree.source[offsets.tokenLocation(tree, first_token).start..offsets.tokenLocation(tree, last_token).end],
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

fn completeLabel(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    id: i64,
    pos_index: usize,
    handle: *Document,
    config: *Config,
    client_capabilities: *ClientCapabilities,
) !lsp.Response {
    var completions = std.ArrayList(lsp.CompletionItem).init(arena.allocator());

    const context = DeclToCompletionContext{
        .completions = &completions,
        .config = config,
        .arena = arena,
        .orig_handle = handle,
    };
    try analysis.iterateLabels(arena, workspace, handle, pos_index, declToCompletion, context, config, client_capabilities);
    builtin_completions.truncateCompletions(completions.items, config.max_detail_length);

    return lsp.Response{
        .id = id,
        .result = .{
            .CompletionList = .{
                .isIncomplete = false,
                .items = completions.items,
            },
        },
    };
}

fn completeGlobal(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    id: i64,
    pos_index: usize,
    handle: *Document,
    config: *Config,
    client_capabilities: *ClientCapabilities,
) !lsp.Response {
    var completions = std.ArrayList(lsp.CompletionItem).init(arena.allocator());

    const context = DeclToCompletionContext{
        .completions = &completions,
        .config = config,
        .arena = arena,
        .orig_handle = handle,
    };
    try analysis.iterateSymbolsGlobal(arena, workspace, handle, pos_index, declToCompletion, context, config, client_capabilities);
    builtin_completions.truncateCompletions(completions.items, config.max_detail_length);

    return lsp.Response{
        .id = id,
        .result = .{
            .CompletionList = .{
                .isIncomplete = false,
                .items = completions.items,
            },
        },
    };
}

fn completeFieldAccess(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    id: i64,
    handle: *Document,
    position: DocumentPosition,
    range: analysis.SourceRange,
    config: *Config,
    client_capabilities: *ClientCapabilities,
) !lsp.Response {
    var completions = std.ArrayList(lsp.CompletionItem).init(arena.allocator());

    const line_mem_start = @ptrToInt(position.line.ptr) - @ptrToInt(handle.utf8_buffer.mem.ptr);
    var held_range = handle.utf8_buffer.borrowNullTerminatedSlice(line_mem_start + range.start, line_mem_start + range.end);
    errdefer held_range.release();
    var tokenizer = std.zig.Tokenizer.init(held_range.data());

    if (try analysis.getFieldAccessType(arena, workspace, handle, position.absolute_index, &tokenizer)) |result| {
        held_range.release();
        try typeToCompletion(arena, workspace, &completions, result, handle, config, client_capabilities);
        builtin_completions.truncateCompletions(completions.items, config.max_detail_length);
    }

    return lsp.Response{
        .id = id,
        .result = .{
            .CompletionList = .{
                .isIncomplete = false,
                .items = completions.items,
            },
        },
    };
}

fn completeError(arena: *std.heap.ArenaAllocator, workspace: *Workspace, id: i64, handle: *Document, config: *Config) !lsp.Response {
    const completions = try workspace.errorCompletionItems(arena, handle);
    builtin_completions.truncateCompletions(completions, config.max_detail_length);
    logger.debug("Completing error:", .{});

    return lsp.Response{
        .id = id,
        .result = .{
            .CompletionList = .{
                .isIncomplete = false,
                .items = completions,
            },
        },
    };
}

fn completeDot(arena: *std.heap.ArenaAllocator, workspace: *Workspace, id: i64, handle: *Document, config: *Config) !lsp.Response {
    var completions = try workspace.enumCompletionItems(arena, handle);
    builtin_completions.truncateCompletions(completions, config.max_detail_length);

    return lsp.Response{
        .id = id,
        .result = .{
            .CompletionList = .{
                .isIncomplete = false,
                .items = completions,
            },
        },
    };
}

pub fn process(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    id: i64,
    handle: *Document,
    doc_position: DocumentPosition,
    config: *Config,
    client_capabilities: *ClientCapabilities,
) !lsp.Response {
    const pos_context = position_context.documentPositionContext(arena, doc_position);
    switch (pos_context) {
        .builtin => {
            logger.debug("[completion][builtin]", .{});
            return builtin_completions.completeBuiltin(id);
        },
        .var_access, .empty => {
            logger.debug("[completion][global]", .{});
            return try completeGlobal(arena, workspace, id, doc_position.absolute_index, handle, config, client_capabilities);
        },
        .field_access => |range| {
            logger.debug("[completion][field_access]", .{});
            return try completeFieldAccess(arena, workspace, id, handle, doc_position, range, config, client_capabilities);
        },
        .global_error_set => {
            logger.debug("[completion][global_error_set]", .{});
            return try completeError(arena, workspace, id, handle, config);
        },
        .enum_literal => {
            logger.debug("[completion][enum_literal]", .{});
            return try completeDot(arena, workspace, id, handle, config);
        },
        .label => {
            logger.debug("[completion][label]", .{});
            return try completeLabel(arena, workspace, id, doc_position.absolute_index, handle, config, client_capabilities);
        },
        else => {
            logger.debug("[completion][{s}]", .{@tagName(pos_context)});
            return lsp.Response{
                .id = id,
                .result = lsp.ResponseParams{
                    .CompletionList = .{
                        .isIncomplete = false,
                        .items = &.{},
                    },
                },
            };
        },
    }
}
