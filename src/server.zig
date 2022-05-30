const std = @import("std");
const build_options = @import("build_options");
const Config = @import("./Config.zig");
const DocumentStore = @import("./DocumentStore.zig");
const requests = @import("./requests.zig");
const types = @import("./types.zig");
const analysis = @import("./analysis.zig");
const ast = @import("./ast.zig");
const references = @import("./references.zig");
const rename = @import("./rename.zig");
const offsets = @import("./offsets.zig");
const semantic_tokens = @import("./semantic_tokens.zig");
const shared = @import("./shared.zig");
const Ast = std.zig.Ast;

const data = switch (build_options.data_version) {
    .master => @import("data/master.zig"),
    .@"0.7.0" => @import("data/0.7.0.zig"),
    .@"0.7.1" => @import("data/0.7.1.zig"),
    .@"0.8.0" => @import("data/0.8.0.zig"),
    .@"0.8.1" => @import("data/0.8.1.zig"),
    .@"0.9.0" => @import("data/0.9.0.zig"),
};

const logger = std.log.scoped(.main);

// Code is largely based off of https://github.com/andersfr/zig-lsp/blob/master/server.zig
var stdout: std.io.BufferedWriter(4096, std.fs.File.Writer) = undefined;
var allocator: std.mem.Allocator = undefined;

var document_store: DocumentStore = undefined;

const ClientCapabilities = struct {
    supports_snippets: bool = false,
    supports_semantic_tokens: bool = false,
    hover_supports_md: bool = false,
    completion_doc_supports_md: bool = false,
};

var client_capabilities = ClientCapabilities{};
var offset_encoding = offsets.Encoding.utf16;

const not_implemented_response = types.ResponseError{
    .code = -32601,
    .message = "NotImplemented",
};

const null_result_response = types.ResponseParams{
    .Null = null,
};

const no_completions_response = types.ResponseParams{
    .CompletionList = .{
        .isIncomplete = false,
        .items = &.{},
    },
};

const no_signatures_response = types.ResponseParams{
    .SignatureHelp = .{
        .signatures = &.{},
        .activeSignature = null,
        .activeParameter = null,
    },
};

const no_semantic_tokens_response = types.ResponseParams{
    .SemanticTokensFull = .{
        .data = &.{},
    },
};

/// Sends a request or response
pub fn send(arena: *std.heap.ArenaAllocator, reqOrRes: anytype) !void {
    var arr = std.ArrayList(u8).init(arena.allocator());
    try std.json.stringify(reqOrRes, .{}, arr.writer());

    const stdout_stream = stdout.writer();
    try stdout_stream.print("Content-Length: {}\r\n\r\n", .{arr.items.len});
    try stdout_stream.writeAll(arr.items);
    try stdout.flush();
}

fn truncateCompletions(list: []types.CompletionItem, max_detail_length: usize) void {
    for (list) |*item| {
        if (item.detail) |det| {
            if (det.len > max_detail_length) {
                item.detail = det[0..max_detail_length];
            }
        }
    }
}

fn respondError(arena: *std.heap.ArenaAllocator, id: types.RequestId, e: types.ResponseError) !void {
    try send(arena, types.Response{ .id = id, .result = null_result_response, .@"error" = e });
}

fn respondGeneric(arena: *std.heap.ArenaAllocator, id: types.RequestId, result: types.ResponseParams) !void {
    try send(arena, types.Response{ .id = id, .result = result });
}

fn showMessage(message_type: types.MessageType, message: []const u8) !void {
    try send(types.Notification{
        .method = "window/showMessage",
        .params = .{
            .ShowMessageParams = .{
                .type = message_type,
                .message = message,
            },
        },
    });
}

// TODO: Is this correct or can we get a better end?
fn astLocationToRange(loc: Ast.Location) types.Range {
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

fn publishDiagnostics(arena: *std.heap.ArenaAllocator, handle: DocumentStore.Handle, config: Config) !void {
    const tree = handle.tree;

    var diagnostics = std.ArrayList(types.Diagnostic).init(arena.allocator());

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
            .message = try arena.allocator().dupe(u8, fbs.getWritten()),
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

                    if (config.warn_style) {
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

    try send(arena, types.Notification{
        .method = "textDocument/publishDiagnostics",
        .params = .{
            .PublishDiagnostics = .{
                .uri = handle.uri(),
                .diagnostics = diagnostics.items,
            },
        },
    });
}

fn typeToCompletion(arena: *std.heap.ArenaAllocator, list: *std.ArrayList(types.CompletionItem), field_access: analysis.FieldAccessReturn, orig_handle: *DocumentStore.Handle, config: Config) error{OutOfMemory}!void {
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
                list,
                .{ .node = n, .handle = type_handle.handle },
                null,
                orig_handle,
                type_handle.type.is_type_val,
                null,
                config,
            );
        },
        .other => |n| try nodeToCompletion(
            arena,
            list,
            .{ .node = n, .handle = type_handle.handle },
            field_access.unwrapped,
            orig_handle,
            type_handle.type.is_type_val,
            null,
            config,
        ),
        .primitive => {},
    }
}

fn nodeToCompletion(arena: *std.heap.ArenaAllocator, list: *std.ArrayList(types.CompletionItem), node_handle: analysis.NodeWithHandle, unwrapped: ?analysis.TypeWithHandle, orig_handle: *DocumentStore.Handle, is_type_val: bool, parent_is_type_val: ?bool, config: Config) error{OutOfMemory}!void {
    const node = node_handle.node;
    const handle = node_handle.handle;
    const tree = handle.tree;
    const node_tags = tree.nodes.items(.tag);
    const token_tags = tree.tokens.items(.tag);

    const doc_kind: types.MarkupContent.Kind = if (client_capabilities.completion_doc_supports_md)
        .Markdown
    else
        .PlainText;

    const doc = if (try analysis.getDocComments(
        list.allocator,
        handle.tree,
        node,
        doc_kind,
    )) |doc_comments|
        types.MarkupContent{
            .kind = doc_kind,
            .value = doc_comments,
        }
    else
        null;

    if (ast.isContainer(handle.tree, node)) {
        const context = DeclToCompletionContext{
            .completions = list,
            .config = &config,
            .arena = arena,
            .orig_handle = orig_handle,
            .parent_is_type_val = is_type_val,
        };
        try analysis.iterateSymbolsContainer(
            &document_store,
            arena,
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
                const use_snippets = config.enable_snippets and client_capabilities.supports_snippets;
                const insert_text = if (use_snippets) blk: {
                    const skip_self_param = !(parent_is_type_val orelse true) and
                        try analysis.hasSelfParam(arena, &document_store, handle, func);
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

            if (try analysis.resolveVarDeclAlias(&document_store, arena, node_handle)) |result| {
                const context = DeclToCompletionContext{
                    .completions = list,
                    .config = &config,
                    .arena = arena,
                    .orig_handle = orig_handle,
                };
                return try declToCompletion(context, result);
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
                try typeToCompletion(arena, list, .{ .original = actual_type }, orig_handle, config);
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

pub fn identifierFromPosition(pos_index: usize, handle: DocumentStore.Handle) []const u8 {
    const text = handle.document.text;

    if (pos_index + 1 >= text.len) return "";
    var start_idx = pos_index;

    while (start_idx > 0 and isSymbolChar(text[start_idx - 1])) {
        start_idx -= 1;
    }

    var end_idx = pos_index;
    while (end_idx < handle.document.text.len and isSymbolChar(text[end_idx])) {
        end_idx += 1;
    }

    if (end_idx <= start_idx) return "";
    return text[start_idx..end_idx];
}

fn isSymbolChar(char: u8) bool {
    return std.ascii.isAlNum(char) or char == '_';
}

fn gotoDefinitionSymbol(id: types.RequestId, arena: *std.heap.ArenaAllocator, decl_handle: analysis.DeclWithHandle, resolve_alias: bool) !void {
    var handle = decl_handle.handle;

    const location = switch (decl_handle.decl.*) {
        .ast_node => |node| block: {
            if (resolve_alias) {
                if (try analysis.resolveVarDeclAlias(&document_store, arena, .{ .node = node, .handle = handle })) |result| {
                    handle = result.handle;
                    break :block result.location(offset_encoding) catch return;
                }
            }

            const name_token = analysis.getDeclNameToken(handle.tree, node) orelse
                return try respondGeneric(arena, id, null_result_response);
            break :block offsets.tokenRelativeLocation(handle.tree, 0, handle.tree.tokens.items(.start)[name_token], offset_encoding) catch return;
        },
        else => decl_handle.location(offset_encoding) catch return,
    };

    try send(arena, types.Response{
        .id = id,
        .result = .{
            .Location = .{
                .uri = handle.document.uri,
                .range = .{
                    .start = .{
                        .line = @intCast(i64, location.line),
                        .character = @intCast(i64, location.column),
                    },
                    .end = .{
                        .line = @intCast(i64, location.line),
                        .character = @intCast(i64, location.column),
                    },
                },
            },
        },
    });
}

fn hoverSymbol(id: types.RequestId, arena: *std.heap.ArenaAllocator, decl_handle: analysis.DeclWithHandle) (std.os.WriteError || error{OutOfMemory})!void {
    const handle = decl_handle.handle;
    const tree = handle.tree;

    const hover_kind: types.MarkupContent.Kind = if (client_capabilities.hover_supports_md) .Markdown else .PlainText;
    var doc_str: ?[]const u8 = null;

    const def_str = switch (decl_handle.decl.*) {
        .ast_node => |node| def: {
            if (try analysis.resolveVarDeclAlias(&document_store, arena, .{ .node = node, .handle = handle })) |result| {
                return try hoverSymbol(id, arena, result);
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
                    return try respondGeneric(arena, id, null_result_response);
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

    try send(arena, types.Response{
        .id = id,
        .result = .{
            .Hover = .{
                .contents = .{ .value = hover_text },
            },
        },
    });
}

fn getLabelGlobal(pos_index: usize, handle: *DocumentStore.Handle) !?analysis.DeclWithHandle {
    const name = identifierFromPosition(pos_index, handle.*);
    if (name.len == 0) return null;

    return try analysis.lookupLabel(handle, name, pos_index);
}

fn getSymbolGlobal(arena: *std.heap.ArenaAllocator, pos_index: usize, handle: *DocumentStore.Handle) !?analysis.DeclWithHandle {
    const name = identifierFromPosition(pos_index, handle.*);
    if (name.len == 0) return null;

    return try analysis.lookupSymbolGlobal(&document_store, arena, handle, name, pos_index);
}

fn gotoDefinitionLabel(arena: *std.heap.ArenaAllocator, id: types.RequestId, pos_index: usize, handle: *DocumentStore.Handle, config: Config) !void {
    _ = config;

    const decl = (try getLabelGlobal(pos_index, handle)) orelse return try respondGeneric(arena, id, null_result_response);
    return try gotoDefinitionSymbol(id, arena, decl, false);
}

fn gotoDefinitionGlobal(arena: *std.heap.ArenaAllocator, id: types.RequestId, pos_index: usize, handle: *DocumentStore.Handle, config: Config, resolve_alias: bool) !void {
    _ = config;

    const decl = (try getSymbolGlobal(arena, pos_index, handle)) orelse return try respondGeneric(arena, id, null_result_response);
    return try gotoDefinitionSymbol(id, arena, decl, resolve_alias);
}

fn hoverDefinitionLabel(arena: *std.heap.ArenaAllocator, id: types.RequestId, pos_index: usize, handle: *DocumentStore.Handle, config: Config) !void {
    _ = config;

    const decl = (try getLabelGlobal(pos_index, handle)) orelse return try respondGeneric(arena, id, null_result_response);
    return try hoverSymbol(id, arena, decl);
}

fn hoverDefinitionBuiltin(arena: *std.heap.ArenaAllocator, id: types.RequestId, pos_index: usize, handle: *DocumentStore.Handle) !void {
    const name = identifierFromPosition(pos_index, handle.*);
    if (name.len == 0) return try respondGeneric(arena, id, null_result_response);

    inline for (data.builtins) |builtin| {
        if (std.mem.eql(u8, builtin.name[1..], name)) {
            try send(arena, types.Response{
                .id = id,
                .result = .{
                    .Hover = .{
                        .contents = .{
                            .value = try std.fmt.allocPrint(
                                arena.allocator(),
                                "```zig\n{s}\n```\n{s}",
                                .{ builtin.signature, builtin.documentation },
                            ),
                        },
                    },
                },
            });
        }
    }
}

fn hoverDefinitionGlobal(arena: *std.heap.ArenaAllocator, id: types.RequestId, pos_index: usize, handle: *DocumentStore.Handle, config: Config) !void {
    _ = config;

    const decl = (try getSymbolGlobal(arena, pos_index, handle)) orelse return try respondGeneric(arena, id, null_result_response);
    return try hoverSymbol(id, arena, decl);
}

fn getSymbolFieldAccess(handle: *DocumentStore.Handle, arena: *std.heap.ArenaAllocator, position: offsets.DocumentPosition, range: analysis.SourceRange, config: Config) !?analysis.DeclWithHandle {
    _ = config;

    const name = identifierFromPosition(position.absolute_index, handle.*);
    if (name.len == 0) return null;

    const line_mem_start = @ptrToInt(position.line.ptr) - @ptrToInt(handle.document.mem.ptr);
    var held_range = handle.document.borrowNullTerminatedSlice(line_mem_start + range.start, line_mem_start + range.end);
    var tokenizer = std.zig.Tokenizer.init(held_range.data());

    errdefer held_range.release();
    if (try analysis.getFieldAccessType(&document_store, arena, handle, position.absolute_index, &tokenizer)) |result| {
        held_range.release();
        const container_handle = result.unwrapped orelse result.original;
        const container_handle_node = switch (container_handle.type.data) {
            .other => |n| n,
            else => return null,
        };
        return try analysis.lookupSymbolContainer(
            &document_store,
            arena,
            .{ .node = container_handle_node, .handle = container_handle.handle },
            name,
            true,
        );
    }
    return null;
}

fn gotoDefinitionFieldAccess(arena: *std.heap.ArenaAllocator, id: types.RequestId, handle: *DocumentStore.Handle, position: offsets.DocumentPosition, range: analysis.SourceRange, config: Config, resolve_alias: bool) !void {
    const decl = (try getSymbolFieldAccess(handle, arena, position, range, config)) orelse return try respondGeneric(arena, id, null_result_response);
    return try gotoDefinitionSymbol(id, arena, decl, resolve_alias);
}

fn hoverDefinitionFieldAccess(arena: *std.heap.ArenaAllocator, id: types.RequestId, handle: *DocumentStore.Handle, position: offsets.DocumentPosition, range: analysis.SourceRange, config: Config) !void {
    const decl = (try getSymbolFieldAccess(handle, arena, position, range, config)) orelse return try respondGeneric(arena, id, null_result_response);
    return try hoverSymbol(id, arena, decl);
}

fn gotoDefinitionString(arena: *std.heap.ArenaAllocator, id: types.RequestId, pos_index: usize, handle: *DocumentStore.Handle, config: Config) !void {
    _ = config;

    const tree = handle.tree;

    const import_str = analysis.getImportStr(tree, 0, pos_index) orelse return try respondGeneric(arena, id, null_result_response);
    const uri = (try document_store.uriFromImportStr(
        arena.allocator(),
        handle.*,
        import_str,
    )) orelse return try respondGeneric(arena, id, null_result_response);

    try send(arena, types.Response{
        .id = id,
        .result = .{
            .Location = .{
                .uri = uri,
                .range = .{
                    .start = .{ .line = 0, .character = 0 },
                    .end = .{ .line = 0, .character = 0 },
                },
            },
        },
    });
}

fn renameDefinitionGlobal(arena: *std.heap.ArenaAllocator, id: types.RequestId, handle: *DocumentStore.Handle, pos_index: usize, new_name: []const u8) !void {
    const decl = (try getSymbolGlobal(arena, pos_index, handle)) orelse return try respondGeneric(arena, id, null_result_response);

    var workspace_edit = types.WorkspaceEdit{
        .changes = std.StringHashMap([]types.TextEdit).init(arena.allocator()),
    };
    try rename.renameSymbol(arena, &document_store, decl, new_name, &workspace_edit.changes.?, offset_encoding);
    try send(arena, types.Response{
        .id = id,
        .result = .{ .WorkspaceEdit = workspace_edit },
    });
}

fn renameDefinitionFieldAccess(arena: *std.heap.ArenaAllocator, id: types.RequestId, handle: *DocumentStore.Handle, position: offsets.DocumentPosition, range: analysis.SourceRange, new_name: []const u8, config: Config) !void {
    const decl = (try getSymbolFieldAccess(handle, arena, position, range, config)) orelse return try respondGeneric(arena, id, null_result_response);

    var workspace_edit = types.WorkspaceEdit{
        .changes = std.StringHashMap([]types.TextEdit).init(arena.allocator()),
    };
    try rename.renameSymbol(arena, &document_store, decl, new_name, &workspace_edit.changes.?, offset_encoding);
    try send(arena, types.Response{
        .id = id,
        .result = .{ .WorkspaceEdit = workspace_edit },
    });
}

fn renameDefinitionLabel(arena: *std.heap.ArenaAllocator, id: types.RequestId, handle: *DocumentStore.Handle, pos_index: usize, new_name: []const u8) !void {
    const decl = (try getLabelGlobal(pos_index, handle)) orelse return try respondGeneric(arena, id, null_result_response);

    var workspace_edit = types.WorkspaceEdit{
        .changes = std.StringHashMap([]types.TextEdit).init(arena.allocator()),
    };
    try rename.renameLabel(arena, decl, new_name, &workspace_edit.changes.?, offset_encoding);
    try send(arena, types.Response{
        .id = id,
        .result = .{ .WorkspaceEdit = workspace_edit },
    });
}

fn referencesDefinitionGlobal(arena: *std.heap.ArenaAllocator, id: types.RequestId, handle: *DocumentStore.Handle, pos_index: usize, include_decl: bool, skip_std_references: bool) !void {
    const decl = (try getSymbolGlobal(arena, pos_index, handle)) orelse return try respondGeneric(arena, id, null_result_response);
    var locs = std.ArrayList(types.Location).init(arena.allocator());
    try references.symbolReferences(
        arena,
        &document_store,
        decl,
        offset_encoding,
        include_decl,
        &locs,
        std.ArrayList(types.Location).append,
        skip_std_references,
    );
    try send(arena, types.Response{
        .id = id,
        .result = .{ .Locations = locs.items },
    });
}

fn referencesDefinitionFieldAccess(arena: *std.heap.ArenaAllocator, id: types.RequestId, handle: *DocumentStore.Handle, position: offsets.DocumentPosition, range: analysis.SourceRange, include_decl: bool, config: Config) !void {
    const decl = (try getSymbolFieldAccess(handle, arena, position, range, config)) orelse return try respondGeneric(arena, id, null_result_response);
    var locs = std.ArrayList(types.Location).init(arena.allocator());
    try references.symbolReferences(arena, &document_store, decl, offset_encoding, include_decl, &locs, std.ArrayList(types.Location).append, config.skip_std_references);
    try send(arena, types.Response{
        .id = id,
        .result = .{ .Locations = locs.items },
    });
}

fn referencesDefinitionLabel(arena: *std.heap.ArenaAllocator, id: types.RequestId, handle: *DocumentStore.Handle, pos_index: usize, include_decl: bool) !void {
    const decl = (try getLabelGlobal(pos_index, handle)) orelse return try respondGeneric(arena, id, null_result_response);
    var locs = std.ArrayList(types.Location).init(arena.allocator());
    try references.labelReferences(arena, decl, offset_encoding, include_decl, &locs, std.ArrayList(types.Location).append);
    try send(arena, types.Response{
        .id = id,
        .result = .{ .Locations = locs.items },
    });
}

fn hasComment(tree: Ast.Tree, start_token: Ast.TokenIndex, end_token: Ast.TokenIndex) bool {
    const token_starts = tree.tokens.items(.start);

    const start = token_starts[start_token];
    const end = token_starts[end_token];

    return std.mem.indexOf(u8, tree.source[start..end], "//") != null;
}

const DeclToCompletionContext = struct {
    completions: *std.ArrayList(types.CompletionItem),
    config: *const Config,
    arena: *std.heap.ArenaAllocator,
    orig_handle: *DocumentStore.Handle,
    parent_is_type_val: ?bool = null,
};

fn declToCompletion(context: DeclToCompletionContext, decl_handle: analysis.DeclWithHandle) !void {
    const tree = decl_handle.handle.tree;
    switch (decl_handle.decl.*) {
        .ast_node => |node| try nodeToCompletion(
            context.arena,
            context.completions,
            .{ .node = node, .handle = decl_handle.handle },
            null,
            context.orig_handle,
            false,
            context.parent_is_type_val,
            context.config.*,
        ),
        .param_decl => |param| {
            const doc_kind: types.MarkupContent.Kind = if (client_capabilities.completion_doc_supports_md) .Markdown else .PlainText;
            const doc = if (param.first_doc_comment) |doc_comments|
                types.MarkupContent{
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

fn completeLabel(arena: *std.heap.ArenaAllocator, id: types.RequestId, pos_index: usize, handle: *DocumentStore.Handle, config: Config) !void {
    var completions = std.ArrayList(types.CompletionItem).init(arena.allocator());

    const context = DeclToCompletionContext{
        .completions = &completions,
        .config = &config,
        .arena = arena,
        .orig_handle = handle,
    };
    try analysis.iterateLabels(handle, pos_index, declToCompletion, context);
    truncateCompletions(completions.items, config.max_detail_length);

    try send(arena, types.Response{
        .id = id,
        .result = .{
            .CompletionList = .{
                .isIncomplete = false,
                .items = completions.items,
            },
        },
    });
}

var builtin_completions: ?[]types.CompletionItem = null;
fn completeBuiltin(arena: *std.heap.ArenaAllocator, id: types.RequestId, config: Config) !void {
    if (builtin_completions == null) {
        builtin_completions = try allocator.alloc(types.CompletionItem, data.builtins.len);
        for (data.builtins) |builtin, idx| {
            builtin_completions.?[idx] = types.CompletionItem{
                .label = builtin.name,
                .kind = .Function,
                .filterText = builtin.name[1..],
                .detail = builtin.signature,
                .documentation = .{
                    .kind = .Markdown,
                    .value = builtin.documentation,
                },
            };

            var insert_text: []const u8 = undefined;
            if (config.enable_snippets) {
                insert_text = builtin.snippet;
                builtin_completions.?[idx].insertTextFormat = .Snippet;
            } else {
                insert_text = builtin.name;
            }
            builtin_completions.?[idx].insertText =
                if (config.include_at_in_builtins)
                insert_text
            else
                insert_text[1..];
        }
        truncateCompletions(builtin_completions.?, config.max_detail_length);
    }

    try send(arena, types.Response{
        .id = id,
        .result = .{
            .CompletionList = .{
                .isIncomplete = false,
                .items = builtin_completions.?,
            },
        },
    });
}

fn completeGlobal(arena: *std.heap.ArenaAllocator, id: types.RequestId, pos_index: usize, handle: *DocumentStore.Handle, config: Config) !void {
    var completions = std.ArrayList(types.CompletionItem).init(arena.allocator());

    const context = DeclToCompletionContext{
        .completions = &completions,
        .config = &config,
        .arena = arena,
        .orig_handle = handle,
    };
    try analysis.iterateSymbolsGlobal(&document_store, arena, handle, pos_index, declToCompletion, context);
    truncateCompletions(completions.items, config.max_detail_length);

    try send(arena, types.Response{
        .id = id,
        .result = .{
            .CompletionList = .{
                .isIncomplete = false,
                .items = completions.items,
            },
        },
    });
}

fn completeFieldAccess(arena: *std.heap.ArenaAllocator, id: types.RequestId, handle: *DocumentStore.Handle, position: offsets.DocumentPosition, range: analysis.SourceRange, config: Config) !void {
    var completions = std.ArrayList(types.CompletionItem).init(arena.allocator());

    const line_mem_start = @ptrToInt(position.line.ptr) - @ptrToInt(handle.document.mem.ptr);
    var held_range = handle.document.borrowNullTerminatedSlice(line_mem_start + range.start, line_mem_start + range.end);
    errdefer held_range.release();
    var tokenizer = std.zig.Tokenizer.init(held_range.data());

    if (try analysis.getFieldAccessType(&document_store, arena, handle, position.absolute_index, &tokenizer)) |result| {
        held_range.release();
        try typeToCompletion(arena, &completions, result, handle, config);
        truncateCompletions(completions.items, config.max_detail_length);
    }

    try send(arena, types.Response{
        .id = id,
        .result = .{
            .CompletionList = .{
                .isIncomplete = false,
                .items = completions.items,
            },
        },
    });
}

fn completeError(arena: *std.heap.ArenaAllocator, id: types.RequestId, handle: *DocumentStore.Handle, config: Config) !void {
    const completions = try document_store.errorCompletionItems(arena, handle);
    truncateCompletions(completions, config.max_detail_length);
    logger.debug("Completing error:", .{});

    try send(arena, types.Response{
        .id = id,
        .result = .{
            .CompletionList = .{
                .isIncomplete = false,
                .items = completions,
            },
        },
    });
}

fn completeDot(arena: *std.heap.ArenaAllocator, id: types.RequestId, handle: *DocumentStore.Handle, config: Config) !void {
    var completions = try document_store.enumCompletionItems(arena, handle);
    truncateCompletions(completions, config.max_detail_length);

    try send(arena, types.Response{
        .id = id,
        .result = .{
            .CompletionList = .{
                .isIncomplete = false,
                .items = completions,
            },
        },
    });
}

fn documentSymbol(arena: *std.heap.ArenaAllocator, id: types.RequestId, handle: *DocumentStore.Handle) !void {
    try send(arena, types.Response{
        .id = id,
        .result = .{ .DocumentSymbols = try analysis.getDocumentSymbols(arena.allocator(), handle.tree, offset_encoding) },
    });
}

fn initializeHandler(arena: *std.heap.ArenaAllocator, config: Config, tree: std.json.ValueTree, id: types.RequestId) !void {
    _ = config;

    const req = try requests.fromDynamicTree(arena, requests.Initialize, tree.root);

    for (req.params.capabilities.offsetEncoding.value) |encoding| {
        if (std.mem.eql(u8, encoding, "utf-8")) {
            offset_encoding = .utf8;
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

    try send(arena, types.Response{
        .id = id,
        .result = .{
            .InitializeResult = .{
                .offsetEncoding = if (offset_encoding == .utf8)
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
                                const tokTypeFields = std.meta.fields(semantic_tokens.TokenType);
                                var names: [tokTypeFields.len][]const u8 = undefined;
                                for (tokTypeFields) |field, i| {
                                    names[i] = field.name;
                                }
                                break :block &names;
                            },
                            .tokenModifiers = comptime block: {
                                const tokModFields = std.meta.fields(semantic_tokens.TokenModifiers);
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
    });

    logger.info("zls initialized", .{});
    logger.info("{}", .{client_capabilities});
    logger.info("Using offset encoding: {s}", .{std.meta.tagName(offset_encoding)});
}

pub var keep_running = true;
fn shutdownHandler(arena: *std.heap.ArenaAllocator, config: Config, _: std.json.ValueTree, id: types.RequestId) !void {
    _ = config;
    _ = arena;

    logger.info("Server closing...", .{});

    keep_running = false;
    // Technically we should deinitialize first and send possible errors to the client
    try respondGeneric(arena, id, null_result_response);
}

fn openDocumentHandler(arena: *std.heap.ArenaAllocator, config: Config, tree: std.json.ValueTree, id: types.RequestId) !void {

    const req = try requests.fromDynamicTree(arena, requests.OpenDocument, tree.root);

    const handle = try document_store.openDocument(req.params.textDocument.uri, req.params.textDocument.text);
    try publishDiagnostics(arena, handle.*, config);

    // TODO:
    if (client_capabilities.supports_semantic_tokens)
        try semanticTokensFullHandlerReq(arena, config, id, .{ .params = .{ .textDocument = .{ .uri = req.params.textDocument.uri } } });
}

fn changeDocumentHandler(arena: *std.heap.ArenaAllocator, config: Config, tree: std.json.ValueTree, id: types.RequestId) !void {
    _ = id;

    const req = try requests.fromDynamicTree(arena, requests.ChangeDocument, tree.root);

    const handle = document_store.getHandle(req.params.textDocument.uri) orelse {
        logger.debug("Trying to change non existent document {s}", .{req.params.textDocument.uri});
        return;
    };

    try document_store.applyChanges(handle, req.params.contentChanges.Array, offset_encoding);
    try publishDiagnostics(arena, handle.*, config);
}

fn saveDocumentHandler(arena: *std.heap.ArenaAllocator, config: Config, tree: std.json.ValueTree, id: types.RequestId) !void {
    _ = config;
    _ = id;
    _ = arena;

    const req = try requests.fromDynamicTree(arena, requests.SaveDocument, tree.root);

    const handle = document_store.getHandle(req.params.textDocument.uri) orelse {
        logger.warn("Trying to save non existent document {s}", .{req.params.textDocument.uri});
        return;
    };
    try document_store.applySave(handle);
}

fn closeDocumentHandler(arena: *std.heap.ArenaAllocator, config: Config, tree: std.json.ValueTree, id: types.RequestId) !void {
    _ = config;
    _ = id;
    _ = arena;
    const req = try requests.fromDynamicTree(arena, requests.CloseDocument, tree.root);
    document_store.closeDocument(req.params.textDocument.uri);
}

fn semanticTokensFullHandler(arena: *std.heap.ArenaAllocator, config: Config, tree: std.json.ValueTree, id: types.RequestId) !void {
    const req = try requests.fromDynamicTree(arena, requests.SemanticTokensFull, tree.root);
    try semanticTokensFullHandlerReq(arena, config, id, req);
}

fn semanticTokensFullHandlerReq(arena: *std.heap.ArenaAllocator, config: Config, id: types.RequestId, req: requests.SemanticTokensFull) !void {
    if (config.enable_semantic_tokens) blk: {
        const handle = document_store.getHandle(req.params.textDocument.uri) orelse {
            logger.warn("Trying to get semantic tokens of non existent document {s}", .{req.params.textDocument.uri});
            break :blk;
        };

        const token_array = try semantic_tokens.writeAllSemanticTokens(arena, &document_store, handle, offset_encoding);
        defer allocator.free(token_array);

        return try send(arena, types.Response{
            .id = id,
            .result = .{ .SemanticTokensFull = .{ .data = token_array } },
        });
    }
    return try respondGeneric(arena, id, no_semantic_tokens_response);
}

fn completionHandler(arena: *std.heap.ArenaAllocator, config: Config, tree: std.json.ValueTree, id: types.RequestId) !void {
    const req = try requests.fromDynamicTree(arena, requests.Completion, tree.root);
    const handle = document_store.getHandle(req.params.textDocument.uri) orelse {
        logger.warn("Trying to complete in non existent document {s}", .{req.params.textDocument.uri});
        return try respondGeneric(arena, id, no_completions_response);
    };

    if (req.params.position.character == 0)
        return try respondGeneric(arena, id, no_completions_response);

    const doc_position = try offsets.documentPosition(handle.document, req.params.position, offset_encoding);
    const pos_context = try analysis.documentPositionContext(arena, handle.document, doc_position);

    switch (pos_context) {
        .builtin => try completeBuiltin(arena, id, config),
        .var_access, .empty => try completeGlobal(arena, id, doc_position.absolute_index, handle, config),
        .field_access => |range| try completeFieldAccess(arena, id, handle, doc_position, range, config),
        .global_error_set => try completeError(arena, id, handle, config),
        .enum_literal => try completeDot(arena, id, handle, config),
        .label => try completeLabel(arena, id, doc_position.absolute_index, handle, config),
        else => try respondGeneric(arena, id, no_completions_response),
    }
}

fn signatureHelpHandler(arena: *std.heap.ArenaAllocator, config: Config, tree: std.json.ValueTree, id: types.RequestId) !void {
    _ = config;
    const req = try requests.fromDynamicTree(arena, requests.SignatureHelp, tree.root);

    const getSignatureInfo = @import("signature_help.zig").getSignatureInfo;
    const handle = document_store.getHandle(req.params.textDocument.uri) orelse {
        logger.warn("Trying to get signature help in non existent document {s}", .{req.params.textDocument.uri});
        return try respondGeneric(arena, id, no_signatures_response);
    };

    if (req.params.position.character == 0)
        return try respondGeneric(arena, id, no_signatures_response);

    const doc_position = try offsets.documentPosition(handle.document, req.params.position, offset_encoding);
    if (try getSignatureInfo(
        &document_store,
        arena,
        handle,
        doc_position.absolute_index,
        data,
    )) |sig_info| {
        return try send(arena, types.Response{
            .id = id,
            .result = .{
                .SignatureHelp = .{
                    .signatures = &[1]types.SignatureInformation{sig_info},
                    .activeSignature = 0,
                    .activeParameter = sig_info.activeParameter,
                },
            },
        });
    }
    return try respondGeneric(arena, id, no_signatures_response);
}

fn gotoHandler(arena: *std.heap.ArenaAllocator, id: types.RequestId, req: requests.GotoDefinition, config: Config, resolve_alias: bool) !void {
    const handle = document_store.getHandle(req.params.textDocument.uri) orelse {
        logger.warn("Trying to go to definition in non existent document {s}", .{req.params.textDocument.uri});
        return try respondGeneric(arena, id, null_result_response);
    };

    if (req.params.position.character >= 0) {
        const doc_position = try offsets.documentPosition(handle.document, req.params.position, offset_encoding);
        const pos_context = try analysis.documentPositionContext(arena, handle.document, doc_position);

        switch (pos_context) {
            .var_access => try gotoDefinitionGlobal(arena, id, doc_position.absolute_index, handle, config, resolve_alias),
            .field_access => |range| try gotoDefinitionFieldAccess(arena, id, handle, doc_position, range, config, resolve_alias),
            .string_literal => try gotoDefinitionString(arena, id, doc_position.absolute_index, handle, config),
            .label => try gotoDefinitionLabel(arena, id, doc_position.absolute_index, handle, config),
            else => try respondGeneric(arena, id, null_result_response),
        }
    } else {
        try respondGeneric(arena, id, null_result_response);
    }
}

fn gotoDefinitionHandler(arena: *std.heap.ArenaAllocator, config: Config, tree: std.json.ValueTree, id: types.RequestId) !void {
    const req = try requests.fromDynamicTree(arena, requests.GotoDefinition, tree.root);
    try gotoHandler(arena, id, req, config, true);
}

fn gotoDeclarationHandler(arena: *std.heap.ArenaAllocator, config: Config, tree: std.json.ValueTree, id: types.RequestId) !void {
    const req = try requests.fromDynamicTree(arena, requests.GotoDeclaration, tree.root);
    try gotoHandler(arena, id, req, config, false);
}

fn hoverHandler(arena: *std.heap.ArenaAllocator, config: Config, tree: std.json.ValueTree, id: types.RequestId) !void {
    const req = try requests.fromDynamicTree(arena, requests.Hover, tree.root);
    const handle = document_store.getHandle(req.params.textDocument.uri) orelse {
        logger.warn("Trying to get hover in non existent document {s}", .{req.params.textDocument.uri});
        return try respondGeneric(arena, id, null_result_response);
    };

    if (req.params.position.character >= 0) {
        const doc_position = try offsets.documentPosition(handle.document, req.params.position, offset_encoding);
        const pos_context = try analysis.documentPositionContext(arena, handle.document, doc_position);
        switch (pos_context) {
            .builtin => try hoverDefinitionBuiltin(arena, id, doc_position.absolute_index, handle),
            .var_access => try hoverDefinitionGlobal(arena, id, doc_position.absolute_index, handle, config),
            .field_access => |range| try hoverDefinitionFieldAccess(arena, id, handle, doc_position, range, config),
            .label => try hoverDefinitionLabel(arena, id, doc_position.absolute_index, handle, config),
            else => try respondGeneric(arena, id, null_result_response),
        }
    } else {
        try respondGeneric(arena, id, null_result_response);
    }
}

fn documentSymbolsHandler(arena: *std.heap.ArenaAllocator, config: Config, tree: std.json.ValueTree, id: types.RequestId) !void {
    _ = config;
    const req = try requests.fromDynamicTree(arena, requests.DocumentSymbols, tree.root);
    const handle = document_store.getHandle(req.params.textDocument.uri) orelse {
        logger.warn("Trying to get document symbols in non existent document {s}", .{req.params.textDocument.uri});
        return try respondGeneric(arena, id, null_result_response);
    };
    try documentSymbol(arena, id, handle);
}

fn formattingHandler(arena: *std.heap.ArenaAllocator, config: Config, tree: std.json.ValueTree, id: types.RequestId) !void {
    const req = try requests.fromDynamicTree(arena, requests.Formatting, tree.root);
    if (config.zig_exe_path) |zig_exe_path| {
        const handle = document_store.getHandle(req.params.textDocument.uri) orelse {
            logger.warn("Trying to got to definition in non existent document {s}", .{req.params.textDocument.uri});
            return try respondGeneric(arena, id, null_result_response);
        };

        var process = std.ChildProcess.init(&[_][]const u8{ zig_exe_path, "fmt", "--stdin" }, allocator);
        process.stdin_behavior = .Pipe;
        process.stdout_behavior = .Pipe;

        process.spawn() catch |err| {
            logger.warn("Failed to spawn zig fmt process, error: {}", .{err});
            return try respondGeneric(arena, id, null_result_response);
        };
        try process.stdin.?.writeAll(handle.document.text);
        process.stdin.?.close();
        process.stdin = null;

        const stdout_bytes = try process.stdout.?.reader().readAllAlloc(allocator, std.math.maxInt(usize));
        defer allocator.free(stdout_bytes);

        switch (try process.wait()) {
            .Exited => |code| if (code == 0) {
                return try send(arena, types.Response{
                    .id = id,
                    .result = .{
                        .TextEdits = &[1]types.TextEdit{
                            .{
                                .range = try offsets.documentRange(handle.document, offset_encoding),
                                .newText = stdout_bytes,
                            },
                        },
                    },
                });
            },
            else => {},
        }
    }
    return try respondGeneric(arena, id, null_result_response);
}

fn renameHandler(arena: *std.heap.ArenaAllocator, config: Config, tree: std.json.ValueTree, id: types.RequestId) !void {
    const req = try requests.fromDynamicTree(arena, requests.Rename, tree.root);
    const handle = document_store.getHandle(req.params.textDocument.uri) orelse {
        logger.warn("Trying to rename in non existent document {s}", .{req.params.textDocument.uri});
        return try respondGeneric(arena, id, null_result_response);
    };

    if (req.params.position.character >= 0) {
        const doc_position = try offsets.documentPosition(handle.document, req.params.position, offset_encoding);
        const pos_context = try analysis.documentPositionContext(arena, handle.document, doc_position);

        switch (pos_context) {
            .var_access => try renameDefinitionGlobal(arena, id, handle, doc_position.absolute_index, req.params.newName),
            .field_access => |range| try renameDefinitionFieldAccess(arena, id, handle, doc_position, range, req.params.newName, config),
            .label => try renameDefinitionLabel(arena, id, handle, doc_position.absolute_index, req.params.newName),
            else => try respondGeneric(arena, id, null_result_response),
        }
    } else {
        try respondGeneric(arena, id, null_result_response);
    }
}

fn referencesHandler(arena: *std.heap.ArenaAllocator, config: Config, tree: std.json.ValueTree, id: types.RequestId) !void {
    const req = try requests.fromDynamicTree(arena, requests.References, tree.root);
    const handle = document_store.getHandle(req.params.textDocument.uri) orelse {
        logger.warn("Trying to get references in non existent document {s}", .{req.params.textDocument.uri});
        return try respondGeneric(arena, id, null_result_response);
    };

    if (req.params.position.character >= 0) {
        const doc_position = try offsets.documentPosition(handle.document, req.params.position, offset_encoding);
        const pos_context = try analysis.documentPositionContext(arena, handle.document, doc_position);

        const include_decl = req.params.context.includeDeclaration;
        switch (pos_context) {
            .var_access => try referencesDefinitionGlobal(arena, id, handle, doc_position.absolute_index, include_decl, config.skip_std_references),
            .field_access => |range| try referencesDefinitionFieldAccess(arena, id, handle, doc_position, range, include_decl, config),
            .label => try referencesDefinitionLabel(arena, id, handle, doc_position.absolute_index, include_decl),
            else => try respondGeneric(arena, id, null_result_response),
        }
    } else {
        try respondGeneric(arena, id, null_result_response);
    }
}

// Needed for the hack seen below.
fn extractErr(val: anytype) anyerror {
    val catch |e| return e;
    return error.HackDone;
}

pub fn processJsonRpc(arena: *std.heap.ArenaAllocator, config: Config, tree: std.json.ValueTree) !void {
    const id = if (tree.root.Object.get("id")) |id| switch (id) {
        .Integer => |int| types.RequestId{ .Integer = int },
        .String => |str| types.RequestId{ .String = str },
        else => types.RequestId{ .Integer = 0 },
    } else types.RequestId{ .Integer = 0 };

    std.debug.assert(tree.root.Object.get("method") != null);
    const method = tree.root.Object.get("method").?.String;
    const unimplemented_map = std.ComptimeStringMap(void, .{
        .{"textDocument/documentHighlight"},
        .{"textDocument/codeAction"},
        .{"textDocument/codeLens"},
        .{"textDocument/documentLink"},
        .{"textDocument/rangeFormatting"},
        .{"textDocument/onTypeFormatting"},
        .{"textDocument/prepareRename"},
        .{"textDocument/foldingRange"},
        .{"textDocument/selectionRange"},
        .{"textDocument/semanticTokens/range"},
        .{"workspace/didChangeWorkspaceFolders"},
    });
    if (unimplemented_map.has(method)) {
        // TODO: Unimplemented methods, implement them and add them to server capabilities.
        return try respondGeneric(arena, id, null_result_response);
    }

    const start_time = std.time.milliTimestamp();
    defer {
        const end_time = std.time.milliTimestamp();
        logger.debug("Took {}ms to process method {s}", .{ end_time - start_time, method });
    }

    const method_map = .{
        // .{"initialized"},
        // .{"$/cancelRequest"},
        // .{"textDocument/willSave"},
        .{ "initialize", initializeHandler },
        .{ "shutdown", shutdownHandler },
        .{ "textDocument/didOpen", openDocumentHandler },
        .{ "textDocument/didChange", changeDocumentHandler },
        .{ "textDocument/didSave", saveDocumentHandler },
        .{ "textDocument/didClose", closeDocumentHandler },
        .{ "textDocument/semanticTokens/full", semanticTokensFullHandler },
        .{ "textDocument/completion", completionHandler },
        .{ "textDocument/signatureHelp", signatureHelpHandler },
        .{ "textDocument/definition", gotoDefinitionHandler },
        .{ "textDocument/typeDefinition", gotoDefinitionHandler },
        .{ "textDocument/implementation", gotoDefinitionHandler },
        .{ "textDocument/declaration", gotoDeclarationHandler },
        .{ "textDocument/hover", hoverHandler },
        .{ "textDocument/documentSymbol", documentSymbolsHandler },
        .{ "textDocument/formatting", formattingHandler },
        .{ "textDocument/rename", renameHandler },
        .{ "textDocument/references", referencesHandler },
    };

    // Hack to avoid `return`ing in the inline for, which causes bugs.
    inline for (method_map) |method_info| {
        if (std.mem.eql(u8, method, method_info[0])) {
            try method_info[1](arena, config, tree, id);
            return;
        }
    }

    if (tree.root.Object.get("id")) |_| {
        return try respondError(arena, id, not_implemented_response);
    }
    logger.debug("Method without return value not implemented: {s}", .{method});
    return try respondGeneric(arena, id, null_result_response);
}

pub fn init(a: std.mem.Allocator, config: Config, build_runner_path: []const u8, build_runner_cache_path: []const u8) anyerror!void {
    allocator = a;
    analysis.init(allocator);
    stdout = std.io.bufferedWriter(std.io.getStdOut().writer());
    try document_store.init(
        allocator,
        config.zig_exe_path,
        build_runner_path,
        build_runner_cache_path,
        config.zig_lib_path,
        // TODO make this configurable
        // We can't figure it out ourselves since we don't know what arguments
        // the user will use to run "zig build"
        "zig-cache",
        // Since we don't compile anything and no packages should put their
        // files there this path can be ignored
        "ZLS_DONT_CARE",
        config.builtin_path,
    );
}

pub fn deinit() void {
    keep_running = false;
    analysis.deinit();
    document_store.deinit();
    if (builtin_completions) |compls| {
        allocator.free(compls);
    }
}
