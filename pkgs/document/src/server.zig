const std = @import("std");
const Config = @import("./Config.zig");
const Workspace = @import("./Workspace.zig");
const Document = @import("./Document.zig");
const requests = @import("lsp").requests;
const lsp = @import("lsp");
const analysis = @import("./analysis.zig");
const ast = @import("./ast.zig");
const references = @import("./references.zig");
const rename = @import("./rename.zig");
const offsets = @import("./offsets.zig");
const semantic_tokens = @import("./semantic_tokens.zig");
const shared = @import("./shared.zig");
const Ast = std.zig.Ast;
const getSignatureInfo = @import("signature_help.zig").getSignatureInfo;
const Session = @import("./Session.zig");
const builtin_completions = @import("./builtin_completions.zig");
const position_context = @import("./position_context.zig");
const DocumentPosition = @import("./document_position.zig").DocumentPosition;

const logger = std.log.scoped(.main);

const ClientCapabilities = struct {
    supports_snippets: bool = false,
    supports_semantic_tokens: bool = false,
    hover_supports_md: bool = false,
    completion_doc_supports_md: bool = false,
};

var client_capabilities = ClientCapabilities{};

const no_completions_response = lsp.ResponseParams{
    .CompletionList = .{
        .isIncomplete = false,
        .items = &.{},
    },
};

const no_signatures_response = lsp.ResponseParams{
    .SignatureHelp = .{
        .signatures = &.{},
        .activeSignature = null,
        .activeParameter = null,
    },
};

const no_semantic_tokens_response = lsp.ResponseParams{
    .SemanticTokensFull = .{
        .data = &.{},
    },
};

// TODO: Is this correct or can we get a better end?
fn astLocationToRange(loc: Ast.Location) lsp.Range {
    return .{
        .start = .{
            .line = @intCast(i64, loc.line),
            .character = @intCast(i64, loc.column),
        },
        .end = .{
            .line = @intCast(i64, loc.line),
            .character = @intCast(i64, loc.column),
        },
    };
}

fn createNotifyDiagnostics(session: *Session, handle: *const Document) !lsp.Notification {
    const tree = handle.tree;

    var diagnostics = std.ArrayList(lsp.Diagnostic).init(session.arena.allocator());

    for (tree.errors) |err| {
        const loc = tree.tokenLocation(0, err.token);

        var mem_buffer: [256]u8 = undefined;
        var fbs = std.io.fixedBufferStream(&mem_buffer);
        try tree.renderError(err, fbs.writer());

        try diagnostics.append(.{
            .range = astLocationToRange(loc),
            .severity = .Error,
            .code = @tagName(err.tag),
            .source = "zls",
            .message = try session.arena.allocator().dupe(u8, fbs.getWritten()),
            // .relatedInformation = undefined
        });
    }

    // TODO: style warnings for types, values and declarations below root scope
    if (tree.errors.len == 0) {
        for (tree.rootDecls()) |decl_idx| {
            const decl = tree.nodes.items(.tag)[decl_idx];
            switch (decl) {
                .fn_proto,
                .fn_proto_multi,
                .fn_proto_one,
                .fn_proto_simple,
                .fn_decl,
                => blk: {
                    var buf: [1]Ast.Node.Index = undefined;
                    const func = ast.fnProto(tree, decl_idx, &buf).?;
                    if (func.extern_export_inline_token != null) break :blk;

                    if (session.config.warn_style) {
                        if (func.name_token) |name_token| {
                            const loc = tree.tokenLocation(0, name_token);

                            const is_type_function = analysis.isTypeFunction(tree, func);

                            const func_name = tree.tokenSlice(name_token);
                            if (!is_type_function and !analysis.isCamelCase(func_name)) {
                                try diagnostics.append(.{
                                    .range = astLocationToRange(loc),
                                    .severity = .Information,
                                    .code = "BadStyle",
                                    .source = "zls",
                                    .message = "Functions should be camelCase",
                                });
                            } else if (is_type_function and !analysis.isPascalCase(func_name)) {
                                try diagnostics.append(.{
                                    .range = astLocationToRange(loc),
                                    .severity = .Information,
                                    .code = "BadStyle",
                                    .source = "zls",
                                    .message = "Type functions should be PascalCase",
                                });
                            }
                        }
                    }
                },
                else => {},
            }
        }
    }

    return lsp.Notification{
        .method = "textDocument/publishDiagnostics",
        .params = .{
            .PublishDiagnostics = .{
                .uri = handle.document.uri,
                .diagnostics = diagnostics.items,
            },
        },
    };

    // try notifyQueue.insert(0, notification);
}

fn typeToCompletion(session: *Session, list: *std.ArrayList(lsp.CompletionItem), field_access: analysis.FieldAccessReturn, orig_handle: *Document) error{OutOfMemory}!void {
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
            if (session.config.operator_completions) {
                try list.append(.{
                    .label = "*",
                    .kind = .Operator,
                    .insertText = "*",
                    .insertTextFormat = .PlainText,
                });
            }
            try nodeToCompletion(session, list, .{ .node = n, .handle = type_handle.handle }, null, orig_handle, type_handle.type.is_type_val, null);
        },
        .other => |n| try nodeToCompletion(session, list, .{ .node = n, .handle = type_handle.handle }, field_access.unwrapped, orig_handle, type_handle.type.is_type_val, null),
        .primitive => {},
    }
}

fn nodeToCompletion(session: *Session, list: *std.ArrayList(lsp.CompletionItem), node_handle: analysis.NodeWithHandle, unwrapped: ?analysis.TypeWithHandle, orig_handle: *Document, is_type_val: bool, parent_is_type_val: ?bool) error{OutOfMemory}!void {
    const arena = session.arena;
    const node = node_handle.node;
    const handle = node_handle.handle;
    const tree = handle.tree;
    const node_tags = tree.nodes.items(.tag);
    const token_tags = tree.tokens.items(.tag);

    const doc_kind: lsp.MarkupContent.Kind = if (client_capabilities.completion_doc_supports_md)
        .Markdown
    else
        .PlainText;

    const doc = if (try analysis.getDocComments(
        list.allocator,
        handle.tree,
        node,
        doc_kind,
    )) |doc_comments|
        lsp.MarkupContent{
            .kind = doc_kind,
            .value = doc_comments,
        }
    else
        null;

    if (ast.isContainer(handle.tree, node)) {
        const context = DeclToCompletionContext{
            .completions = list,
            .config = session.config,
            .arena = arena,
            .orig_handle = orig_handle,
            .parent_is_type_val = is_type_val,
        };
        try analysis.iterateSymbolsContainer(
            session,
            node_handle,
            orig_handle,
            declToCompletion,
            context,
            !is_type_val,
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
                const use_snippets = session.config.enable_snippets and client_capabilities.supports_snippets;
                const insert_text = if (use_snippets) blk: {
                    const skip_self_param = !(parent_is_type_val orelse true) and
                        try analysis.hasSelfParam(session, handle, func);
                    break :blk try analysis.getFunctionSnippet(arena.allocator(), tree, func, skip_self_param);
                } else tree.tokenSlice(func.name_token.?);

                const is_type_function = analysis.isTypeFunction(handle.tree, func);

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

            if (try analysis.resolveVarDeclAlias(session, node_handle)) |result| {
                const context = DeclToCompletionContext{
                    .completions = list,
                    .config = session.config,
                    .arena = arena,
                    .orig_handle = orig_handle,
                };
                return try declToCompletion(session, context, result);
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
                .One, .C, .Many => if (session.config.operator_completions) {
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
                try typeToCompletion(session, list, .{ .original = actual_type }, orig_handle);
            }
            return;
        },
        .optional_type => {
            if (session.config.operator_completions) {
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

fn hoverSymbol(session: *Session, id: i64, decl_handle: analysis.DeclWithHandle) (std.os.WriteError || error{OutOfMemory})!lsp.Response {
    const handle = decl_handle.handle;
    const tree = handle.tree;
    const arena = session.arena;

    const hover_kind: lsp.MarkupContent.Kind = if (client_capabilities.hover_supports_md) .Markdown else .PlainText;
    var doc_str: ?[]const u8 = null;

    const def_str = switch (decl_handle.decl.*) {
        .ast_node => |node| def: {
            if (try analysis.resolveVarDeclAlias(session, .{ .node = node, .handle = handle })) |result| {
                return try hoverSymbol(session, id, result);
            }
            doc_str = try analysis.getDocComments(arena.allocator(), tree, node, hover_kind);

            var buf: [1]Ast.Node.Index = undefined;

            if (ast.varDecl(tree, node)) |var_decl| {
                break :def analysis.getVariableSignature(tree, var_decl);
            } else if (ast.fnProto(tree, node, &buf)) |fn_proto| {
                break :def analysis.getFunctionSignature(tree, fn_proto);
            } else if (ast.containerField(tree, node)) |field| {
                break :def analysis.getContainerFieldSignature(tree, field);
            } else {
                break :def analysis.nodeToString(tree, node) orelse
                    return lsp.Response.createNull(id);
            }
        },
        .param_decl => |param| def: {
            if (param.first_doc_comment) |doc_comments| {
                doc_str = try analysis.collectDocComments(arena.allocator(), handle.tree, doc_comments, hover_kind, false);
            }

            const first_token = param.first_doc_comment orelse
                param.comptime_noalias orelse
                param.name_token orelse
                tree.firstToken(param.type_expr); // extern fn
            const last_token = param.anytype_ellipsis3 orelse tree.lastToken(param.type_expr);

            const start = offsets.tokenLocation(tree, first_token).start;
            const end = offsets.tokenLocation(tree, last_token).end;
            break :def tree.source[start..end];
        },
        .pointer_payload => |payload| tree.tokenSlice(payload.name),
        .array_payload => |payload| handle.tree.tokenSlice(payload.identifier),
        .array_index => |payload| handle.tree.tokenSlice(payload),
        .switch_payload => |payload| tree.tokenSlice(payload.node),
        .label_decl => |label_decl| tree.tokenSlice(label_decl),
    };

    var hover_text: []const u8 = undefined;
    if (hover_kind == .Markdown) {
        hover_text =
            if (doc_str) |doc|
            try std.fmt.allocPrint(arena.allocator(), "```zig\n{s}\n```\n{s}", .{ def_str, doc })
        else
            try std.fmt.allocPrint(arena.allocator(), "```zig\n{s}\n```", .{def_str});
    } else {
        hover_text =
            if (doc_str) |doc|
            try std.fmt.allocPrint(arena.allocator(), "{s}\n{s}", .{ def_str, doc })
        else
            def_str;
    }

    return lsp.Response{
        .id = id,
        .result = .{
            .Hover = .{
                .contents = .{ .value = hover_text },
            },
        },
    };
}

fn hoverDefinitionBuiltin(session: *Session, id: i64, pos_index: usize, handle: *Document) !lsp.Response {
    const name = try offsets.identifierFromPosition(pos_index, handle.document.text);
    for (builtin_completions.data()) |builtin| {
        if (std.mem.eql(u8, builtin.name[1..], name)) {
            return lsp.Response{
                .id = id,
                .result = .{
                    .Hover = .{
                        .contents = .{
                            .value = try std.fmt.allocPrint(
                                session.arena.allocator(),
                                "```zig\n{s}\n```\n{s}",
                                .{ builtin.signature, builtin.documentation },
                            ),
                        },
                    },
                },
            };
        }
    }

    unreachable;
}

fn renameDefinitionGlobal(session: *Session, id: i64, handle: *Document, pos_index: usize, new_name: []const u8) !lsp.Response {
    const decl = try offsets.getSymbolGlobal(session, pos_index, handle);

    var workspace_edit = lsp.WorkspaceEdit{
        .changes = std.StringHashMap([]lsp.TextEdit).init(session.arena.allocator()),
    };
    try rename.renameSymbol(session, decl, new_name, &workspace_edit.changes.?, offsets.offset_encoding);
    return lsp.Response{
        .id = id,
        .result = .{ .WorkspaceEdit = workspace_edit },
    };
}

fn renameDefinitionFieldAccess(session: *Session, id: i64, handle: *Document, position: DocumentPosition, range: analysis.SourceRange, new_name: []const u8) !lsp.Response {
    const decl = try offsets.getSymbolFieldAccess(session, handle, position, range);

    var workspace_edit = lsp.WorkspaceEdit{
        .changes = std.StringHashMap([]lsp.TextEdit).init(session.arena.allocator()),
    };
    try rename.renameSymbol(session, decl, new_name, &workspace_edit.changes.?, offsets.offset_encoding);
    return lsp.Response{
        .id = id,
        .result = .{ .WorkspaceEdit = workspace_edit },
    };
}

fn renameDefinitionLabel(session: *Session, id: i64, handle: *Document, pos_index: usize, new_name: []const u8) !lsp.Response {
    const decl = (try offsets.getLabelGlobal(pos_index, handle)) orelse return lsp.Response.createNull(id);

    var workspace_edit = lsp.WorkspaceEdit{
        .changes = std.StringHashMap([]lsp.TextEdit).init(session.arena.allocator()),
    };
    try rename.renameLabel(session, decl, new_name, &workspace_edit.changes.?, offsets.offset_encoding);
    return lsp.Response{
        .id = id,
        .result = .{ .WorkspaceEdit = workspace_edit },
    };
}

fn referencesDefinitionGlobal(session: *Session, id: i64, handle: *Document, pos_index: usize, include_decl: bool, skip_std_references: bool) !lsp.Response {
    const decl = try offsets.getSymbolGlobal(session, pos_index, handle);
    var locs = std.ArrayList(lsp.Location).init(session.arena.allocator());
    try references.symbolReferences(
        session,
        decl,
        offsets.offset_encoding,
        include_decl,
        &locs,
        std.ArrayList(lsp.Location).append,
        skip_std_references,
    );
    return lsp.Response{
        .id = id,
        .result = .{ .Locations = locs.items },
    };
}

fn referencesDefinitionFieldAccess(session: *Session, id: i64, handle: *Document, position: DocumentPosition, range: analysis.SourceRange, include_decl: bool) !lsp.Response {
    const decl = try offsets.getSymbolFieldAccess(session, handle, position, range);
    var locs = std.ArrayList(lsp.Location).init(session.arena.allocator());
    try references.symbolReferences(session, decl, offsets.offset_encoding, include_decl, &locs, std.ArrayList(lsp.Location).append, session.config.skip_std_references);
    return lsp.Response{
        .id = id,
        .result = .{ .Locations = locs.items },
    };
}

fn referencesDefinitionLabel(session: *Session, id: i64, handle: *Document, pos_index: usize, include_decl: bool) !lsp.Response {
    const decl = (try offsets.getLabelGlobal(pos_index, handle)) orelse return lsp.Response.createNull(id);
    var locs = std.ArrayList(lsp.Location).init(session.arena.allocator());
    try references.labelReferences(decl, offsets.offset_encoding, include_decl, &locs, std.ArrayList(lsp.Location).append);
    return lsp.Response{
        .id = id,
        .result = .{ .Locations = locs.items },
    };
}

fn hasComment(tree: Ast.Tree, start_token: Ast.TokenIndex, end_token: Ast.TokenIndex) bool {
    const token_starts = tree.tokens.items(.start);

    const start = token_starts[start_token];
    const end = token_starts[end_token];

    return std.mem.indexOf(u8, tree.source[start..end], "//") != null;
}

const DeclToCompletionContext = struct {
    completions: *std.ArrayList(lsp.CompletionItem),
    config: *const Config,
    arena: *std.heap.ArenaAllocator,
    orig_handle: *Document,
    parent_is_type_val: ?bool = null,
};

fn declToCompletion(session: *Session, context: DeclToCompletionContext, decl_handle: analysis.DeclWithHandle) !void {
    const tree = decl_handle.handle.tree;
    switch (decl_handle.decl.*) {
        .ast_node => |node| try nodeToCompletion(
            session,
            context.completions,
            .{ .node = node, .handle = decl_handle.handle },
            null,
            context.orig_handle,
            false,
            context.parent_is_type_val,
        ),
        .param_decl => |param| {
            const doc_kind: lsp.MarkupContent.Kind = if (client_capabilities.completion_doc_supports_md) .Markdown else .PlainText;
            const doc = if (param.first_doc_comment) |doc_comments|
                lsp.MarkupContent{
                    .kind = doc_kind,
                    .value = try analysis.collectDocComments(context.arena.allocator(), tree, doc_comments, doc_kind, false),
                }
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

fn completeLabel(session: *Session, id: i64, pos_index: usize, handle: *Document) !lsp.Response {
    var completions = std.ArrayList(lsp.CompletionItem).init(session.arena.allocator());

    const context = DeclToCompletionContext{
        .completions = &completions,
        .config = session.config,
        .arena = session.arena,
        .orig_handle = handle,
    };
    try analysis.iterateLabels(session, handle, pos_index, declToCompletion, context);
    builtin_completions.truncateCompletions(completions.items, session.config.max_detail_length);

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

fn completeGlobal(session: *Session, id: i64, pos_index: usize, handle: *Document) !lsp.Response {
    var completions = std.ArrayList(lsp.CompletionItem).init(session.arena.allocator());

    const context = DeclToCompletionContext{
        .completions = &completions,
        .config = session.config,
        .arena = session.arena,
        .orig_handle = handle,
    };
    try analysis.iterateSymbolsGlobal(session, handle, pos_index, declToCompletion, context);
    builtin_completions.truncateCompletions(completions.items, session.config.max_detail_length);

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

fn completeFieldAccess(session: *Session, id: i64, handle: *Document, position: DocumentPosition, range: analysis.SourceRange) !lsp.Response {
    var completions = std.ArrayList(lsp.CompletionItem).init(session.arena.allocator());

    const line_mem_start = @ptrToInt(position.line.ptr) - @ptrToInt(handle.document.mem.ptr);
    var held_range = handle.document.borrowNullTerminatedSlice(line_mem_start + range.start, line_mem_start + range.end);
    errdefer held_range.release();
    var tokenizer = std.zig.Tokenizer.init(held_range.data());

    if (try analysis.getFieldAccessType(session, handle, position.absolute_index, &tokenizer)) |result| {
        held_range.release();
        try typeToCompletion(session, &completions, result, handle);
        builtin_completions.truncateCompletions(completions.items, session.config.max_detail_length);
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

fn completeError(session: *Session, id: i64, handle: *Document) !lsp.Response {
    const completions = try session.workspace.errorCompletionItems(session.arena, handle);
    builtin_completions.truncateCompletions(completions, session.config.max_detail_length);
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

fn completeDot(session: *Session, id: i64, handle: *Document) !lsp.Response {
    var completions = try session.workspace.enumCompletionItems(session.arena, handle);
    builtin_completions.truncateCompletions(completions, session.config.max_detail_length);

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

pub fn initializeHandler(session: *Session, id: i64, req: requests.Initialize) !lsp.Response {
    _ = session;

    for (req.params.capabilities.offsetEncoding.value) |encoding| {
        if (std.mem.eql(u8, encoding, "utf-8")) {
            offsets.offset_encoding = .utf8;
        }
    }

    if (req.params.capabilities.textDocument) |textDocument| {
        client_capabilities.supports_semantic_tokens = textDocument.semanticTokens.exists;
        if (textDocument.hover) |hover| {
            for (hover.contentFormat.value) |format| {
                if (std.mem.eql(u8, "markdown", format)) {
                    client_capabilities.hover_supports_md = true;
                }
            }
        }
        if (textDocument.completion) |completion| {
            if (completion.completionItem) |completionItem| {
                client_capabilities.supports_snippets = completionItem.snippetSupport.value;
                for (completionItem.documentationFormat.value) |documentationFormat| {
                    if (std.mem.eql(u8, "markdown", documentationFormat)) {
                        client_capabilities.completion_doc_supports_md = true;
                    }
                }
            }
        }
    }

    logger.info("zls initialized", .{});
    logger.info("{}", .{client_capabilities});
    logger.info("Using offset encoding: {s}", .{@tagName(offsets.offset_encoding)});
    return lsp.Response{
        .id = id,
        .result = .{
            .InitializeResult = .{
                .offsetEncoding = if (offsets.offset_encoding == .utf8)
                    @as([]const u8, "utf-8")
                else
                    "utf-16",
                .serverInfo = .{
                    .name = "zls",
                    .version = "0.1.0",
                },
                .capabilities = .{
                    .signatureHelpProvider = .{
                        .triggerCharacters = &.{"("},
                        .retriggerCharacters = &.{","},
                    },
                    .textDocumentSync = .Full,
                    .renameProvider = true,
                    .completionProvider = .{
                        .resolveProvider = false,
                        .triggerCharacters = &[_][]const u8{ ".", ":", "@" },
                    },
                    .documentHighlightProvider = false,
                    .hoverProvider = true,
                    .codeActionProvider = false,
                    .declarationProvider = true,
                    .definitionProvider = true,
                    .typeDefinitionProvider = true,
                    .implementationProvider = false,
                    .referencesProvider = true,
                    .documentSymbolProvider = true,
                    .colorProvider = false,
                    .documentFormattingProvider = true,
                    .documentRangeFormattingProvider = false,
                    .foldingRangeProvider = false,
                    .selectionRangeProvider = false,
                    .workspaceSymbolProvider = false,
                    .rangeProvider = false,
                    .documentProvider = true,
                    .workspace = .{
                        .workspaceFolders = .{
                            .supported = false,
                            .changeNotifications = false,
                        },
                    },
                    .semanticTokensProvider = .{
                        .full = true,
                        .range = false,
                        .legend = .{
                            .tokenTypes = comptime block: {
                                const tokTypeFields = std.meta.fields(lsp.SemanticTokenType);
                                var names: [tokTypeFields.len][]const u8 = undefined;
                                for (tokTypeFields) |field, i| {
                                    names[i] = field.name;
                                }
                                break :block &names;
                            },
                            .tokenModifiers = comptime block: {
                                const tokModFields = std.meta.fields(lsp.SemanticTokenModifiers);
                                var names: [tokModFields.len][]const u8 = undefined;
                                for (tokModFields) |field, i| {
                                    names[i] = field.name;
                                }
                                break :block &names;
                            },
                        },
                    },
                },
            },
        },
    };
}

pub fn openDocumentHandler(session: *Session, req: requests.OpenDocument) !void {
    const handle = try session.workspace.openDocument(req.params.textDocument.uri, req.params.textDocument.text);
    if (createNotifyDiagnostics(session, handle)) |notification| {
        session.transport.sendToJson(notification);
    } else |_| {}
}

pub fn changeDocumentHandler(session: *Session, req: requests.ChangeDocument) !void {
    const handle = try session.workspace.getHandle(req.params.textDocument.uri);
    try session.workspace.applyChanges(handle, req.params.contentChanges.Array, offsets.offset_encoding);
    if (createNotifyDiagnostics(session, handle)) |notification| {
        session.transport.sendToJson(notification);
    } else |_| {}
}

pub fn saveDocumentHandler(session: *Session, req: requests.SaveDocument) !void {
    const handle = try session.workspace.getHandle(req.params.textDocument.uri);
    try session.workspace.applySave(handle);
}

pub fn closeDocumentHandler(session: *Session, req: requests.CloseDocument) !void {
    session.workspace.closeDocument(req.params.textDocument.uri);
}

pub fn semanticTokensFullHandler(session: *Session, id: i64, req: requests.SemanticTokensFull) !lsp.Response {
    if (!session.config.enable_semantic_tokens) {
        return lsp.Response{ .id = id, .result = no_semantic_tokens_response };
    }

    const handle = try session.workspace.getHandle(req.params.textDocument.uri);
    const token_array = try semantic_tokens.writeAllSemanticTokens(session, handle, offsets.offset_encoding);
    return lsp.Response{
        .id = id,
        .result = .{ .SemanticTokensFull = .{ .data = token_array } },
    };
}

pub fn completionHandler(session: *Session, id: i64, req: requests.Completion) !lsp.Response {
    const handle = try session.workspace.getHandle(req.params.textDocument.uri);
    const doc_position = try offsets.documentPosition(handle.document, req.params.position, offsets.offset_encoding);
    const pos_context = position_context.documentPositionContext(session.arena, doc_position);

    switch (pos_context) {
        .builtin => {
            logger.debug("[completion][builtin]", .{});
            return builtin_completions.completeBuiltin(id);
        },
        .var_access, .empty => {
            logger.debug("[completion][global]", .{});
            return try completeGlobal(session, id, doc_position.absolute_index, handle);
        },
        .field_access => |range| {
            logger.debug("[completion][field_access]", .{});
            return try completeFieldAccess(session, id, handle, doc_position, range);
        },
        .global_error_set => {
            logger.debug("[completion][global_error_set]", .{});
            return try completeError(session, id, handle);
        },
        .enum_literal => {
            logger.debug("[completion][enum_literal]", .{});
            return try completeDot(session, id, handle);
        },
        .label => {
            logger.debug("[completion][label]", .{});
            return try completeLabel(session, id, doc_position.absolute_index, handle);
        },
        else => {
            logger.debug("[completion][{s}]", .{@tagName(pos_context)});
            return lsp.Response{ .id = id, .result = no_completions_response };
        },
    }
}

fn getSignature(session: *Session, id: i64, req: requests.SignatureHelp) !lsp.Response {
    if (req.params.position.character == 0) {
        return lsp.Response{ .id = id, .result = no_signatures_response };
    }

    const handle = try session.workspace.getHandle(req.params.textDocument.uri);
    const doc_position = try offsets.documentPosition(handle.document, req.params.position, offsets.offset_encoding);
    if (try getSignatureInfo(
        session,
        handle,
        doc_position.absolute_index,
        builtin_completions.data(),
    )) |sig_info| {
        return lsp.Response{
            .id = id,
            .result = .{
                .SignatureHelp = .{
                    .signatures = &[1]lsp.SignatureInformation{sig_info},
                    .activeSignature = 0,
                    .activeParameter = sig_info.activeParameter,
                },
            },
        };
    }

    return lsp.Response{ .id = id, .result = no_signatures_response };
}

pub fn signatureHelpHandler(session: *Session, id: i64, req: requests.SignatureHelp) !lsp.Response {
    return try getSignature(session, id, req);
}

pub fn gotoDefinitionHandler(session: *Session, id: i64, req: requests.GotoDefinition) !lsp.Response {
    return try offsets.gotoHandler(session, id, req, true);
}

pub fn gotoDeclarationHandler(session: *Session, id: i64, req: requests.GotoDefinition) !lsp.Response {
    return try offsets.gotoHandler(session, id, req, false);
}

pub fn hoverHandler(session: *Session, id: i64, req: requests.Hover) !lsp.Response {
    logger.debug("[hover]{s} {}", .{ req.params.textDocument.uri, req.params.position });
    const handle = try session.workspace.getHandle(req.params.textDocument.uri);
    const doc_position = try offsets.documentPosition(handle.document, req.params.position, offsets.offset_encoding);
    const pos_context = position_context.documentPositionContext(session.arena, doc_position);
    switch (pos_context) {
        .builtin => {
            logger.debug("[hover][builtin]", .{});
            return try hoverDefinitionBuiltin(session, id, doc_position.absolute_index, handle);
        },
        .var_access => {
            logger.debug("[hover][var_access]", .{});
            const decl = try offsets.getSymbolGlobal(session, doc_position.absolute_index, handle);
            return try hoverSymbol(session, id, decl);
        },
        .field_access => |range| {
            logger.debug("[hover][field_access]", .{});
            const decl = try offsets.getSymbolFieldAccess(session, handle, doc_position, range);
            return try hoverSymbol(session, id, decl);
        },
        .label => {
            logger.debug("[hover][label_access]", .{});
            const decl = (try offsets.getLabelGlobal(doc_position.absolute_index, handle)) orelse return lsp.Response.createNull(id);
            return try hoverSymbol(session, id, decl);
        },
        else => {
            logger.debug("[hover][{s}]", .{@tagName(pos_context)});
            return lsp.Response.createNull(id);
        },
    }
}

fn getDocumentSymbol(session: *Session, id: i64, req: requests.DocumentSymbols) !lsp.Response {
    const handle = try session.workspace.getHandle(req.params.textDocument.uri);

    return lsp.Response{
        .id = id,
        .result = .{ .DocumentSymbols = try analysis.getDocumentSymbols(session.arena.allocator(), handle.tree, offsets.offset_encoding) },
    };
}

pub fn documentSymbolsHandler(session: *Session, id: i64, req: requests.DocumentSymbols) !lsp.Response {
    return try getDocumentSymbol(session, id, req);
}

fn doFormat(session: *Session, id: i64, req: requests.Formatting) !lsp.Response {
    const zig_exe_path = session.config.zig_exe_path orelse {
        logger.warn("no zig_exe_path", .{});
        return lsp.Response.createNull(id);
    };

    const handle = try session.workspace.getHandle(req.params.textDocument.uri);
    var process = std.ChildProcess.init(&[_][]const u8{ zig_exe_path, "fmt", "--stdin" }, session.arena.allocator());
    process.stdin_behavior = .Pipe;
    process.stdout_behavior = .Pipe;
    process.spawn() catch |err| {
        logger.warn("Failed to spawn zig fmt process, error: {}", .{err});
        return lsp.Response.createNull(id);
    };
    try process.stdin.?.writeAll(handle.document.text);
    process.stdin.?.close();
    process.stdin = null;

    const stdout_bytes = try process.stdout.?.reader().readAllAlloc(session.arena.allocator(), std.math.maxInt(usize));

    var edits = try session.arena.allocator().alloc(lsp.TextEdit, 1);
    edits[0] = .{
        .range = try offsets.documentRange(handle.document, offsets.offset_encoding),
        .newText = stdout_bytes,
    };

    switch (try process.wait()) {
        .Exited => |code| if (code == 0) {
            return lsp.Response{
                .id = id,
                .result = .{
                    .TextEdits = edits,
                },
            };
        },
        else => {},
    }

    return lsp.Response.createNull(id);
}

pub fn formattingHandler(session: *Session, id: i64, req: requests.Formatting) !lsp.Response {
    return try doFormat(session, id, req);
}

fn doRename(session: *Session, id: i64, req: requests.Rename) !lsp.Response {
    const handle = try session.workspace.getHandle(req.params.textDocument.uri);
    const doc_position = try offsets.documentPosition(handle.document, req.params.position, offsets.offset_encoding);
    const pos_context = position_context.documentPositionContext(session.arena, doc_position);

    return switch (pos_context) {
        .var_access => try renameDefinitionGlobal(session, id, handle, doc_position.absolute_index, req.params.newName),
        .field_access => |range| try renameDefinitionFieldAccess(session, id, handle, doc_position, range, req.params.newName),
        .label => try renameDefinitionLabel(session, id, handle, doc_position.absolute_index, req.params.newName),
        else => lsp.Response.createNull(id),
    };
}

pub fn renameHandler(session: *Session, id: i64, req: requests.Rename) !lsp.Response {
    return try doRename(session, id, req);
}

fn getReference(session: *Session, id: i64, req: requests.References) !lsp.Response {
    const handle = try session.workspace.getHandle(req.params.textDocument.uri);
    const doc_position = try offsets.documentPosition(handle.document, req.params.position, offsets.offset_encoding);
    const pos_context = position_context.documentPositionContext(session.arena, doc_position);

    const include_decl = req.params.context.includeDeclaration;
    return switch (pos_context) {
        .var_access => try referencesDefinitionGlobal(session, id, handle, doc_position.absolute_index, include_decl, session.config.skip_std_references),
        .field_access => |range| try referencesDefinitionFieldAccess(session, id, handle, doc_position, range, include_decl),
        .label => try referencesDefinitionLabel(session, id, handle, doc_position.absolute_index, include_decl),
        else => lsp.Response.createNull(id),
    };
}

pub fn referencesHandler(session: *Session, id: i64, req: requests.References) !lsp.Response {
    return try getReference(session, id, req);
}
