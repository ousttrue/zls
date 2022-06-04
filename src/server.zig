const std = @import("std");
const build_options = @import("build_options");
const Config = @import("./Config.zig");
const DocumentStore = @import("./DocumentStore.zig");
const requests = @import("./requests.zig");
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

const data = switch (build_options.data_version) {
    .master => @import("data/master.zig"),
    .@"0.7.0" => @import("data/0.7.0.zig"),
    .@"0.7.1" => @import("data/0.7.1.zig"),
    .@"0.8.0" => @import("data/0.8.0.zig"),
    .@"0.8.1" => @import("data/0.8.1.zig"),
    .@"0.9.0" => @import("data/0.9.0.zig"),
};

const logger = std.log.scoped(.main);
pub var config = Config{};

// Code is largely based off of https://github.com/andersfr/zig-lsp/blob/master/server.zig
var allocator: std.mem.Allocator = undefined;
pub var notifyQueue: std.ArrayList(lsp.Notification) = undefined;

var document_store: DocumentStore = undefined;

const ClientCapabilities = struct {
    supports_snippets: bool = false,
    supports_semantic_tokens: bool = false,
    hover_supports_md: bool = false,
    completion_doc_supports_md: bool = false,
};

var client_capabilities = ClientCapabilities{};
var offset_encoding = offsets.Encoding.utf16;

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

fn truncateCompletions(list: []lsp.CompletionItem, max_detail_length: usize) void {
    for (list) |*item| {
        if (item.detail) |det| {
            if (det.len > max_detail_length) {
                item.detail = det[0..max_detail_length];
            }
        }
    }
}

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

fn notifyDiagnostics(arena: *std.heap.ArenaAllocator, handle: DocumentStore.Handle) !void {
    const tree = handle.tree;

    var diagnostics = std.ArrayList(lsp.Diagnostic).init(arena.allocator());

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

    const notification = lsp.Notification{
        .method = "textDocument/publishDiagnostics",
        .params = .{
            .PublishDiagnostics = .{
                .uri = handle.uri(),
                .diagnostics = diagnostics.items,
            },
        },
    };

    try notifyQueue.insert(0, notification);
}

fn typeToCompletion(arena: *std.heap.ArenaAllocator, list: *std.ArrayList(lsp.CompletionItem), field_access: analysis.FieldAccessReturn, orig_handle: *DocumentStore.Handle) error{OutOfMemory}!void {
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
            try nodeToCompletion(arena, list, .{ .node = n, .handle = type_handle.handle }, null, orig_handle, type_handle.type.is_type_val, null);
        },
        .other => |n| try nodeToCompletion(arena, list, .{ .node = n, .handle = type_handle.handle }, field_access.unwrapped, orig_handle, type_handle.type.is_type_val, null),
        .primitive => {},
    }
}

fn nodeToCompletion(arena: *std.heap.ArenaAllocator, list: *std.ArrayList(lsp.CompletionItem), node_handle: analysis.NodeWithHandle, unwrapped: ?analysis.TypeWithHandle, orig_handle: *DocumentStore.Handle, is_type_val: bool, parent_is_type_val: ?bool) error{OutOfMemory}!void {
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
                try typeToCompletion(arena, list, .{ .original = actual_type }, orig_handle);
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

fn gotoDefinitionSymbol(id: i64, arena: *std.heap.ArenaAllocator, decl_handle: analysis.DeclWithHandle, resolve_alias: bool) !lsp.Response {
    var handle = decl_handle.handle;

    const location = switch (decl_handle.decl.*) {
        .ast_node => |node| block: {
            if (resolve_alias) {
                if (try analysis.resolveVarDeclAlias(&document_store, arena, .{ .node = node, .handle = handle })) |result| {
                    handle = result.handle;
                    break :block try result.location(offset_encoding);
                }
            }

            const name_token = analysis.getDeclNameToken(handle.tree, node) orelse
                return lsp.Response.createNull(id);
            break :block try offsets.tokenRelativeLocation(handle.tree, 0, handle.tree.tokens.items(.start)[name_token], offset_encoding);
        },
        else => try decl_handle.location(offset_encoding),
    };

    return lsp.Response{
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
    };
}

fn hoverSymbol(id: i64, arena: *std.heap.ArenaAllocator, decl_handle: analysis.DeclWithHandle) (std.os.WriteError || error{OutOfMemory})!lsp.Response {
    const handle = decl_handle.handle;
    const tree = handle.tree;

    const hover_kind: lsp.MarkupContent.Kind = if (client_capabilities.hover_supports_md) .Markdown else .PlainText;
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

fn gotoDefinitionLabel(arena: *std.heap.ArenaAllocator, id: i64, pos_index: usize, handle: *DocumentStore.Handle) !lsp.Response {
    const decl = (try getLabelGlobal(pos_index, handle)) orelse return lsp.Response.createNull(id);
    return try gotoDefinitionSymbol(id, arena, decl, false);
}

fn gotoDefinitionGlobal(arena: *std.heap.ArenaAllocator, id: i64, pos_index: usize, handle: *DocumentStore.Handle, resolve_alias: bool) !lsp.Response {
    const decl = (try getSymbolGlobal(arena, pos_index, handle)) orelse return lsp.Response.createNull(id);
    return try gotoDefinitionSymbol(id, arena, decl, resolve_alias);
}

fn hoverDefinitionLabel(arena: *std.heap.ArenaAllocator, id: i64, pos_index: usize, handle: *DocumentStore.Handle) !lsp.Response {
    const decl = (try getLabelGlobal(pos_index, handle)) orelse return lsp.Response.createNull(id);
    return try hoverSymbol(id, arena, decl);
}

fn hoverDefinitionBuiltin(arena: *std.heap.ArenaAllocator, id: i64, pos_index: usize, handle: *DocumentStore.Handle) !lsp.Response {
    const name = identifierFromPosition(pos_index, handle.*);
    if (name.len == 0) return lsp.Response.createNull(id);

    inline for (data.builtins) |builtin| {
        if (std.mem.eql(u8, builtin.name[1..], name)) {
            return lsp.Response{
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
            };
        }
    }

    unreachable;
}

fn hoverDefinitionGlobal(arena: *std.heap.ArenaAllocator, id: i64, pos_index: usize, handle: *DocumentStore.Handle) !lsp.Response {
    _ = config;

    const decl = (try getSymbolGlobal(arena, pos_index, handle)) orelse return lsp.Response.createNull(id);
    return try hoverSymbol(id, arena, decl);
}

fn getSymbolFieldAccess(handle: *DocumentStore.Handle, arena: *std.heap.ArenaAllocator, position: offsets.DocumentPosition, range: analysis.SourceRange) !?analysis.DeclWithHandle {
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

fn gotoDefinitionFieldAccess(arena: *std.heap.ArenaAllocator, id: i64, handle: *DocumentStore.Handle, position: offsets.DocumentPosition, range: analysis.SourceRange, resolve_alias: bool) !lsp.Response {
    const decl = (try getSymbolFieldAccess(handle, arena, position, range)) orelse return lsp.Response.createNull(id);
    return try gotoDefinitionSymbol(id, arena, decl, resolve_alias);
}

fn hoverDefinitionFieldAccess(arena: *std.heap.ArenaAllocator, id: i64, handle: *DocumentStore.Handle, position: offsets.DocumentPosition, range: analysis.SourceRange) !lsp.Response {
    const decl = (try getSymbolFieldAccess(handle, arena, position, range)) orelse return lsp.Response.createNull(id);
    return try hoverSymbol(id, arena, decl);
}

fn gotoDefinitionString(arena: *std.heap.ArenaAllocator, id: i64, pos_index: usize, handle: *DocumentStore.Handle) !lsp.Response {
    _ = config;

    const tree = handle.tree;

    const import_str = analysis.getImportStr(tree, 0, pos_index) orelse return lsp.Response.createNull(id);
    const uri = (try document_store.uriFromImportStr(
        arena.allocator(),
        handle.*,
        import_str,
    )) orelse return lsp.Response.createNull(id);

    return lsp.Response{
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
    };
}

fn renameDefinitionGlobal(arena: *std.heap.ArenaAllocator, id: i64, handle: *DocumentStore.Handle, pos_index: usize, new_name: []const u8) !lsp.Response {
    const decl = (try getSymbolGlobal(arena, pos_index, handle)) orelse return lsp.Response.createNull(id);

    var workspace_edit = lsp.WorkspaceEdit{
        .changes = std.StringHashMap([]lsp.TextEdit).init(arena.allocator()),
    };
    try rename.renameSymbol(arena, &document_store, decl, new_name, &workspace_edit.changes.?, offset_encoding);
    return lsp.Response{
        .id = id,
        .result = .{ .WorkspaceEdit = workspace_edit },
    };
}

fn renameDefinitionFieldAccess(arena: *std.heap.ArenaAllocator, id: i64, handle: *DocumentStore.Handle, position: offsets.DocumentPosition, range: analysis.SourceRange, new_name: []const u8) !lsp.Response {
    const decl = (try getSymbolFieldAccess(handle, arena, position, range)) orelse return lsp.Response.createNull(id);

    var workspace_edit = lsp.WorkspaceEdit{
        .changes = std.StringHashMap([]lsp.TextEdit).init(arena.allocator()),
    };
    try rename.renameSymbol(arena, &document_store, decl, new_name, &workspace_edit.changes.?, offset_encoding);
    return lsp.Response{
        .id = id,
        .result = .{ .WorkspaceEdit = workspace_edit },
    };
}

fn renameDefinitionLabel(arena: *std.heap.ArenaAllocator, id: i64, handle: *DocumentStore.Handle, pos_index: usize, new_name: []const u8) !lsp.Response {
    const decl = (try getLabelGlobal(pos_index, handle)) orelse return lsp.Response.createNull(id);

    var workspace_edit = lsp.WorkspaceEdit{
        .changes = std.StringHashMap([]lsp.TextEdit).init(arena.allocator()),
    };
    try rename.renameLabel(arena, decl, new_name, &workspace_edit.changes.?, offset_encoding);
    return lsp.Response{
        .id = id,
        .result = .{ .WorkspaceEdit = workspace_edit },
    };
}

fn referencesDefinitionGlobal(arena: *std.heap.ArenaAllocator, id: i64, handle: *DocumentStore.Handle, pos_index: usize, include_decl: bool, skip_std_references: bool) !lsp.Response {
    const decl = (try getSymbolGlobal(arena, pos_index, handle)) orelse return lsp.Response.createNull(id);
    var locs = std.ArrayList(lsp.Location).init(arena.allocator());
    try references.symbolReferences(
        arena,
        &document_store,
        decl,
        offset_encoding,
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

fn referencesDefinitionFieldAccess(arena: *std.heap.ArenaAllocator, id: i64, handle: *DocumentStore.Handle, position: offsets.DocumentPosition, range: analysis.SourceRange, include_decl: bool) !lsp.Response {
    const decl = (try getSymbolFieldAccess(handle, arena, position, range)) orelse return lsp.Response.createNull(id);
    var locs = std.ArrayList(lsp.Location).init(arena.allocator());
    try references.symbolReferences(arena, &document_store, decl, offset_encoding, include_decl, &locs, std.ArrayList(lsp.Location).append, config.skip_std_references);
    return lsp.Response{
        .id = id,
        .result = .{ .Locations = locs.items },
    };
}

fn referencesDefinitionLabel(arena: *std.heap.ArenaAllocator, id: i64, handle: *DocumentStore.Handle, pos_index: usize, include_decl: bool) !lsp.Response {
    const decl = (try getLabelGlobal(pos_index, handle)) orelse return lsp.Response.createNull(id);
    var locs = std.ArrayList(lsp.Location).init(arena.allocator());
    try references.labelReferences(arena, decl, offset_encoding, include_decl, &locs, std.ArrayList(lsp.Location).append);
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

fn completeLabel(arena: *std.heap.ArenaAllocator, id: i64, pos_index: usize, handle: *DocumentStore.Handle) !lsp.Response {
    var completions = std.ArrayList(lsp.CompletionItem).init(arena.allocator());

    const context = DeclToCompletionContext{
        .completions = &completions,
        .config = &config,
        .arena = arena,
        .orig_handle = handle,
    };
    try analysis.iterateLabels(handle, pos_index, declToCompletion, context);
    truncateCompletions(completions.items, config.max_detail_length);

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

var builtin_completions: ?[]lsp.CompletionItem = null;
fn completeBuiltin(id: i64) !lsp.Response {
    if (builtin_completions == null) {
        builtin_completions = try allocator.alloc(lsp.CompletionItem, data.builtins.len);
        for (data.builtins) |builtin, idx| {
            builtin_completions.?[idx] = lsp.CompletionItem{
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

    return lsp.Response{
        .id = id,
        .result = .{
            .CompletionList = .{
                .isIncomplete = false,
                .items = builtin_completions.?,
            },
        },
    };
}

fn completeGlobal(arena: *std.heap.ArenaAllocator, id: i64, pos_index: usize, handle: *DocumentStore.Handle) !lsp.Response {
    var completions = std.ArrayList(lsp.CompletionItem).init(arena.allocator());

    const context = DeclToCompletionContext{
        .completions = &completions,
        .config = &config,
        .arena = arena,
        .orig_handle = handle,
    };
    try analysis.iterateSymbolsGlobal(&document_store, arena, handle, pos_index, declToCompletion, context);
    truncateCompletions(completions.items, config.max_detail_length);

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

fn completeFieldAccess(arena: *std.heap.ArenaAllocator, id: i64, handle: *DocumentStore.Handle, position: offsets.DocumentPosition, range: analysis.SourceRange) !lsp.Response {
    var completions = std.ArrayList(lsp.CompletionItem).init(arena.allocator());

    const line_mem_start = @ptrToInt(position.line.ptr) - @ptrToInt(handle.document.mem.ptr);
    var held_range = handle.document.borrowNullTerminatedSlice(line_mem_start + range.start, line_mem_start + range.end);
    errdefer held_range.release();
    var tokenizer = std.zig.Tokenizer.init(held_range.data());

    if (try analysis.getFieldAccessType(&document_store, arena, handle, position.absolute_index, &tokenizer)) |result| {
        held_range.release();
        try typeToCompletion(arena, &completions, result, handle);
        truncateCompletions(completions.items, config.max_detail_length);
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

fn completeError(arena: *std.heap.ArenaAllocator, id: i64, handle: *DocumentStore.Handle) !lsp.Response {
    const completions = try document_store.errorCompletionItems(arena, handle);
    truncateCompletions(completions, config.max_detail_length);
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

fn completeDot(arena: *std.heap.ArenaAllocator, id: i64, handle: *DocumentStore.Handle) !lsp.Response {
    var completions = try document_store.enumCompletionItems(arena, handle);
    truncateCompletions(completions, config.max_detail_length);

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

pub fn initializeHandler(arena: *std.heap.ArenaAllocator, id: i64, req: requests.Initialize) !lsp.Response {
    _ = arena;
    _ = config;
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

    logger.info("zls initialized", .{});
    logger.info("{}", .{client_capabilities});
    logger.info("Using offset encoding: {s}", .{std.meta.tagName(offset_encoding)});
    return lsp.Response{
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
    };
}

pub fn openDocumentHandler(arena: *std.heap.ArenaAllocator, req: requests.OpenDocument) !void {
    _ = config;
    const handle = try document_store.openDocument(req.params.textDocument.uri, req.params.textDocument.text);
    _ = handle;

    try notifyDiagnostics(arena, handle.*);
}

pub fn changeDocumentHandler(arena: *std.heap.ArenaAllocator, req: requests.ChangeDocument) !void {
    _ = config;
    if (document_store.getHandle(req.params.textDocument.uri)) |handle| {
        try document_store.applyChanges(handle, req.params.contentChanges.Array, offset_encoding);
        try notifyDiagnostics(arena, handle.*);
    } else {
        logger.debug("Trying to change non existent document {s}", .{req.params.textDocument.uri});
    }
}

pub fn saveDocumentHandler(arena: *std.heap.ArenaAllocator, req: requests.SaveDocument) !void {
    _ = config;
    _ = arena;
    if (document_store.getHandle(req.params.textDocument.uri)) |handle| {
        try document_store.applySave(handle);
    } else {
        logger.warn("Trying to save non existent document {s}", .{req.params.textDocument.uri});
    }
}

pub fn closeDocumentHandler(arena: *std.heap.ArenaAllocator, req: requests.CloseDocument) !void {
    _ = config;
    _ = arena;
    document_store.closeDocument(req.params.textDocument.uri);
}

pub fn semanticTokensFullHandler(arena: *std.heap.ArenaAllocator, tree: std.json.ValueTree, id: i64) !lsp.Response {
    const req = try requests.fromDynamicTree(arena, requests.SemanticTokensFull, tree.root);
    return try semanticTokensFullHandlerReq(arena, id, req);
}

fn semanticTokensFullHandlerReq(arena: *std.heap.ArenaAllocator, id: i64, req: requests.SemanticTokensFull) !lsp.Response {
    if (config.enable_semantic_tokens) {
        if (document_store.getHandle(req.params.textDocument.uri)) |handle| {
            const token_array = try semantic_tokens.writeAllSemanticTokens(arena, &document_store, handle, offset_encoding);
            return lsp.Response{
                .id = id,
                .result = .{ .SemanticTokensFull = .{ .data = token_array } },
            };
        } else {
            logger.warn("Trying to get semantic tokens of non existent document {s}", .{req.params.textDocument.uri});
        }
    }
    return lsp.Response{ .id = id, .result = no_semantic_tokens_response };
}

fn getCompletion(arena: *std.heap.ArenaAllocator, id: i64, req: requests.Completion) !lsp.Response {
    if (req.params.position.character < 0) {
        return lsp.Response{ .id = id, .result = no_completions_response };
    }

    if (document_store.getHandle(req.params.textDocument.uri)) |handle| {
        const doc_position = try offsets.documentPosition(handle.document, req.params.position, offset_encoding);
        const pos_context = try analysis.documentPositionContext(arena, handle.document, doc_position);

        return switch (pos_context) {
            .builtin => try completeBuiltin(id),
            .var_access, .empty => try completeGlobal(arena, id, doc_position.absolute_index, handle),
            .field_access => |range| try completeFieldAccess(arena, id, handle, doc_position, range),
            .global_error_set => try completeError(arena, id, handle),
            .enum_literal => try completeDot(arena, id, handle),
            .label => try completeLabel(arena, id, doc_position.absolute_index, handle),
            else => lsp.Response{ .id = id, .result = no_completions_response },
        };
    }

    logger.warn("Trying to complete in non existent document {s}", .{req.params.textDocument.uri});
    return lsp.Response{ .id = id, .result = no_completions_response };
}

pub fn completionHandler(arena: *std.heap.ArenaAllocator, tree: std.json.ValueTree, id: i64) !lsp.Response {
    const req = try requests.fromDynamicTree(arena, requests.Completion, tree.root);
    return try getCompletion(arena, id, req);
}

fn getSignature(arena: *std.heap.ArenaAllocator, id: i64, req: requests.SignatureHelp) !lsp.Response {
    if (req.params.position.character == 0)
        return lsp.Response{ .id = id, .result = no_signatures_response };

    if (document_store.getHandle(req.params.textDocument.uri)) |handle| {
        const doc_position = try offsets.documentPosition(handle.document, req.params.position, offset_encoding);
        if (try getSignatureInfo(
            &document_store,
            arena,
            handle,
            doc_position.absolute_index,
            data,
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
    } else {
        logger.warn("Trying to get signature help in non existent document {s}", .{req.params.textDocument.uri});
    }

    return lsp.Response{ .id = id, .result = no_signatures_response };
}

pub fn signatureHelpHandler(arena: *std.heap.ArenaAllocator, tree: std.json.ValueTree, id: i64) !lsp.Response {
    _ = config;
    const req = try requests.fromDynamicTree(arena, requests.SignatureHelp, tree.root);
    return try getSignature(arena, id, req);
}

pub fn gotoHandler(arena: *std.heap.ArenaAllocator, id: i64, req: requests.GotoDefinition, resolve_alias: bool) !lsp.Response {
    const handle = document_store.getHandle(req.params.textDocument.uri) orelse {
        logger.warn("Trying to go to definition in non existent document {s}", .{req.params.textDocument.uri});
        return lsp.Response.createNull(id);
    };

    if (req.params.position.character < 0) {
        return lsp.Response.createNull(id);
    }

    const doc_position = try offsets.documentPosition(handle.document, req.params.position, offset_encoding);
    const pos_context = try analysis.documentPositionContext(arena, handle.document, doc_position);

    return switch (pos_context) {
        .var_access => try gotoDefinitionGlobal(arena, id, doc_position.absolute_index, handle, resolve_alias),
        .field_access => |range| try gotoDefinitionFieldAccess(arena, id, handle, doc_position, range, resolve_alias),
        .string_literal => try gotoDefinitionString(arena, id, doc_position.absolute_index, handle),
        .label => try gotoDefinitionLabel(arena, id, doc_position.absolute_index, handle),
        else => lsp.Response.createNull(id),
    };
}

pub fn gotoDefinitionHandler(arena: *std.heap.ArenaAllocator, tree: std.json.ValueTree, id: i64) !lsp.Response {
    const req = try requests.fromDynamicTree(arena, requests.GotoDefinition, tree.root);
    return try gotoHandler(arena, id, req, true);
}

pub fn gotoDeclarationHandler(arena: *std.heap.ArenaAllocator, tree: std.json.ValueTree, id: i64) !lsp.Response {
    const req = try requests.fromDynamicTree(arena, requests.GotoDeclaration, tree.root);
    return try gotoHandler(arena, id, req, false);
}

fn getHover(arena: *std.heap.ArenaAllocator, id: i64, req: requests.Hover) !lsp.Response {
    const handle = document_store.getHandle(req.params.textDocument.uri) orelse {
        logger.warn("Trying to get hover in non existent document {s}", .{req.params.textDocument.uri});
        return lsp.Response.createNull(id);
    };

    if (req.params.position.character < 0) {
        return lsp.Response.createNull(id);
    }

    const doc_position = try offsets.documentPosition(handle.document, req.params.position, offset_encoding);
    const pos_context = try analysis.documentPositionContext(arena, handle.document, doc_position);
    return switch (pos_context) {
        .builtin => try hoverDefinitionBuiltin(arena, id, doc_position.absolute_index, handle),
        .var_access => try hoverDefinitionGlobal(arena, id, doc_position.absolute_index, handle),
        .field_access => |range| try hoverDefinitionFieldAccess(arena, id, handle, doc_position, range),
        .label => try hoverDefinitionLabel(arena, id, doc_position.absolute_index, handle),
        else => lsp.Response.createNull(id),
    };
}

pub fn hoverHandler(arena: *std.heap.ArenaAllocator, tree: std.json.ValueTree, id: i64) !lsp.Response {
    const req = try requests.fromDynamicTree(arena, requests.Hover, tree.root);
    return try getHover(arena, id, req);
}

fn getDocumentSymbol(arena: *std.heap.ArenaAllocator, id: i64, req: requests.DocumentSymbols) !lsp.Response {
    const handle = document_store.getHandle(req.params.textDocument.uri) orelse {
        logger.warn("Trying to get document symbols in non existent document {s}", .{req.params.textDocument.uri});
        return lsp.Response.createNull(id);
    };

    return lsp.Response{
        .id = id,
        .result = .{ .DocumentSymbols = try analysis.getDocumentSymbols(arena.allocator(), handle.tree, offset_encoding) },
    };
}

pub fn documentSymbolsHandler(arena: *std.heap.ArenaAllocator, tree: std.json.ValueTree, id: i64) !lsp.Response {
    _ = config;
    const req = try requests.fromDynamicTree(arena, requests.DocumentSymbols, tree.root);
    return try getDocumentSymbol(arena, id, req);
}

fn doFormat(arena: *std.heap.ArenaAllocator, id: i64, req: requests.Formatting) !lsp.Response {
    const zig_exe_path = config.zig_exe_path orelse {
        logger.warn("no zig_exe_path", .{});
        return lsp.Response.createNull(id);
    };

    const handle = document_store.getHandle(req.params.textDocument.uri) orelse {
        logger.warn("Trying to got to definition in non existent document {s}", .{req.params.textDocument.uri});
        return lsp.Response.createNull(id);
    };

    var process = std.ChildProcess.init(&[_][]const u8{ zig_exe_path, "fmt", "--stdin" }, allocator);
    process.stdin_behavior = .Pipe;
    process.stdout_behavior = .Pipe;
    process.spawn() catch |err| {
        logger.warn("Failed to spawn zig fmt process, error: {}", .{err});
        return lsp.Response.createNull(id);
    };
    try process.stdin.?.writeAll(handle.document.text);
    process.stdin.?.close();
    process.stdin = null;

    const stdout_bytes = try process.stdout.?.reader().readAllAlloc(arena.allocator(), std.math.maxInt(usize));

    var edits = try arena.allocator().alloc(lsp.TextEdit, 1);
    edits[0] = .{
        .range = try offsets.documentRange(handle.document, offset_encoding),
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

pub fn formattingHandler(arena: *std.heap.ArenaAllocator, tree: std.json.ValueTree, id: i64) !lsp.Response {
    const req = try requests.fromDynamicTree(arena, requests.Formatting, tree.root);
    return try doFormat(arena, id, req);
}

fn doRename(arena: *std.heap.ArenaAllocator, id: i64, req: requests.Rename) !lsp.Response {
    const handle = document_store.getHandle(req.params.textDocument.uri) orelse {
        logger.warn("Trying to rename in non existent document {s}", .{req.params.textDocument.uri});
        return lsp.Response.createNull(id);
    };

    if (req.params.position.character < 0) {
        return lsp.Response.createNull(id);
    }

    const doc_position = try offsets.documentPosition(handle.document, req.params.position, offset_encoding);
    const pos_context = try analysis.documentPositionContext(arena, handle.document, doc_position);

    return switch (pos_context) {
        .var_access => try renameDefinitionGlobal(arena, id, handle, doc_position.absolute_index, req.params.newName),
        .field_access => |range| try renameDefinitionFieldAccess(arena, id, handle, doc_position, range, req.params.newName),
        .label => try renameDefinitionLabel(arena, id, handle, doc_position.absolute_index, req.params.newName),
        else => lsp.Response.createNull(id),
    };
}

pub fn renameHandler(arena: *std.heap.ArenaAllocator, tree: std.json.ValueTree, id: i64) !lsp.Response {
    const req = try requests.fromDynamicTree(arena, requests.Rename, tree.root);
    return try doRename(arena, id, req);
}

fn getReference(arena: *std.heap.ArenaAllocator, id: i64, req: requests.References) !lsp.Response {
    const handle = document_store.getHandle(req.params.textDocument.uri) orelse {
        logger.warn("Trying to get references in non existent document {s}", .{req.params.textDocument.uri});
        return lsp.Response.createNull(id);
    };

    if (req.params.position.character < 0) {
        return lsp.Response.createNull(id);
    }

    const doc_position = try offsets.documentPosition(handle.document, req.params.position, offset_encoding);
    const pos_context = try analysis.documentPositionContext(arena, handle.document, doc_position);

    const include_decl = req.params.context.includeDeclaration;
    return switch (pos_context) {
        .var_access => try referencesDefinitionGlobal(arena, id, handle, doc_position.absolute_index, include_decl, config.skip_std_references),
        .field_access => |range| try referencesDefinitionFieldAccess(arena, id, handle, doc_position, range, include_decl),
        .label => try referencesDefinitionLabel(arena, id, handle, doc_position.absolute_index, include_decl),
        else => lsp.Response.createNull(id),
    };
}

pub fn referencesHandler(arena: *std.heap.ArenaAllocator, tree: std.json.ValueTree, id: i64) !lsp.Response {
    const req = try requests.fromDynamicTree(arena, requests.References, tree.root);
    return try getReference(arena, id, req);
}

pub fn init(a: std.mem.Allocator, build_runner_path: []const u8, build_runner_cache_path: []const u8) anyerror!void {
    allocator = a;
    analysis.init(allocator);
    notifyQueue = std.ArrayList(lsp.Notification).init(allocator);
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
    analysis.deinit();
    notifyQueue.deinit();
    document_store.deinit();
    if (builtin_completions) |compls| {
        allocator.free(compls);
    }
}
