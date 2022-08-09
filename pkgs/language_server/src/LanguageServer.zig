//! A LanguageServer frontend, registered to a JsonRPC dispatcher.
const std = @import("std");
const lsp = @import("lsp");
const astutil = @import("astutil");
const ws = @import("workspace");
const FixedPath = astutil.FixedPath;
const Config = ws.Config;
const ZigEnv = ws.ZigEnv;
const Workspace = ws.Workspace;
const Document = ws.Document;
const Line = ws.Line;
const semantic_tokens = ws.semantic_tokens;
const SemanticTokensBuilder = ws.SemanticTokensBuilder;
const AstNodeIterator = astutil.AstNodeIterator;
const AstToken = astutil.AstToken;
const Project = astutil.Project;
const completion_util = ws.completion_util;
const ClientCapabilities = @import("./ClientCapabilities.zig");
pub const URI = @import("./uri.zig");
const builtin_completions = ws.builtin_completions;
const getSignatureInfo = ws.signature_help.getSignatureInfo;
const textdocument = @import("./textdocument.zig");
const textdocument_position = @import("./textdocument_position.zig");
const logger = std.log.scoped(.LanguageServer);
pub var keep_running: bool = true;

fn logJson(arena: *std.heap.ArenaAllocator, json: ?std.json.Value) void {
    var json_buffer = std.ArrayList(u8).init(arena.allocator());
    if (json) |value| {
        value.jsonStringify(
            .{ .emit_null_optional_fields = false, .whitespace = .{ .indent_level = 1 } },
            json_buffer.writer(),
        ) catch @panic("stringify");
        logger.debug("{s}", .{json_buffer.items});
    } else {
        logger.debug("null", .{});
    }
}

const Self = @This();

allocator: std.mem.Allocator,
config: *Config,
zigenv: ZigEnv,
workspace: ?*Workspace = null,
client_capabilities: ClientCapabilities = .{},
encoding: Line.Encoding = .utf16,
server_capabilities: lsp.initialize.ServerCapabilities = .{
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
    // .codeLensProvider = .{
    //     .resolveProvider = true,
    // },
    .declarationProvider = false,
    .definitionProvider = true,
    // .typeDefinitionProvider = true,
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
            .tokenTypes = block: {
                const tokTypeFields = std.meta.fields(semantic_tokens.SemanticTokenType);
                var names: [tokTypeFields.len][]const u8 = undefined;
                for (tokTypeFields) |field, i| {
                    names[i] = field.name;
                }
                break :block &names;
            },
            .tokenModifiers = block: {
                const tokModFields = std.meta.fields(semantic_tokens.SemanticTokenModifiers);
                var names: [tokModFields.len][]const u8 = undefined;
                for (tokModFields) |field, i| {
                    names[i] = field.name;
                }
                break :block &names;
            },
        },
    },
},
notification_queue: std.ArrayList(lsp.Notification),

pub fn init(allocator: std.mem.Allocator, config: *Config, zigenv: ZigEnv) Self {
    return Self{
        .allocator = allocator,
        .config = config,
        .zigenv = zigenv,
        .notification_queue = std.ArrayList(lsp.Notification).init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    if (self.workspace) |workspace| {
        workspace.delete();
    }
    self.notification_queue.deinit();
}

/// # base protocol
/// * https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#cancelRequest
pub fn @"$/cancelRequest"(self: *Self, arena: *std.heap.ArenaAllocator, jsonParams: ?std.json.Value) !void {
    _ = self;
    _ = arena;
    _ = jsonParams;
}

/// # lifecycle
/// * https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#initialize
pub fn initialize(self: *Self, arena: *std.heap.ArenaAllocator, id: i64, jsonParams: ?std.json.Value) !lsp.Response {
    // logJson(arena, jsonParams);

    const params = try lsp.fromDynamicTree(arena, lsp.initialize.InitializeParams, jsonParams.?);
    for (params.capabilities.offsetEncoding.value) |encoding| {
        if (std.mem.eql(u8, encoding, "utf-8")) {
            self.encoding = .utf8;
        }
    }

    if (params.capabilities.textDocument) |textDocument| {
        self.client_capabilities.supports_semantic_tokens = textDocument.semanticTokens.exists;
        if (textDocument.hover) |hover| {
            for (hover.contentFormat.value) |format| {
                if (std.mem.eql(u8, "markdown", format)) {
                    self.client_capabilities.hover_supports_md = true;
                }
            }
        }
        if (textDocument.completion) |completion| {
            if (completion.completionItem) |completionItem| {
                self.client_capabilities.supports_snippets = completionItem.snippetSupport.value;
                for (completionItem.documentationFormat.value) |documentationFormat| {
                    if (std.mem.eql(u8, "markdown", documentationFormat)) {
                        self.client_capabilities.completion_doc_supports_md = true;
                    }
                }
            }
        }
    }

    if (params.rootUri) |uri| {
        self.workspace = try Workspace.new(self.allocator, self.zigenv, try FixedPath.fromUri(uri));
    } else if (params.rootPath) |path| {
        self.workspace = try Workspace.new(self.allocator, self.zigenv, FixedPath.fromFullpath(path));
    } else {
        return error.NoWorkspaceRoot;
    }

    return lsp.Response{
        .id = id,
        .result = .{ .InitializeResult = .{
            .offsetEncoding = self.encoding.toString(),
            .serverInfo = .{
                .name = "zls",
                .version = "0.1.0",
            },
            .capabilities = self.server_capabilities,
        } },
    };
}

/// # lifecycle
/// * https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#initialized
pub fn initialized(self: *Self, arena: *std.heap.ArenaAllocator, jsonParams: ?std.json.Value) !void {
    _ = self;
    _ = arena;
    _ = jsonParams;
}

/// # lifecycle
/// * https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#shutdown
pub fn shutdown(self: *Self, arena: *std.heap.ArenaAllocator, id: i64, jsonParams: ?std.json.Value) !lsp.Response {
    _ = self;
    _ = arena;
    _ = jsonParams;
    keep_running = false;
    return lsp.Response.createNull(id);
}

/// # document sync
/// * https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_didOpen
pub fn @"textDocument/didOpen"(self: *Self, arena: *std.heap.ArenaAllocator, jsonParams: ?std.json.Value) !void {
    var workspace = self.workspace orelse return error.WorkspaceNotInitialized;
    const params = try lsp.fromDynamicTree(arena, lsp.requests.OpenDocument, jsonParams.?);
    const doc = try workspace.openDocument(try FixedPath.fromUri(params.textDocument.uri), params.textDocument.text);
    _ = doc;
    // if (textdocument_diagnostics.createNotifyDiagnostics(arena, doc, self.config)) |notification| {
    //     try self.notification_queue.append(notification);
    // } else |err| {
    //     logger.err("createNotifyDiagnostics: {}", .{err});
    // }
}

/// # document sync
/// * https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_didChange
pub fn @"textDocument/didChange"(self: *Self, arena: *std.heap.ArenaAllocator, jsonParams: ?std.json.Value) !void {
    var workspace = self.workspace orelse return error.WorkspaceNotInitialized;
    const params = try lsp.fromDynamicTree(arena, lsp.requests.ChangeDocument, jsonParams.?);
    const doc = workspace.getDocument(try FixedPath.fromUri(params.textDocument.uri)) orelse return error.DocumentNotFound;
    try doc.applyChanges(params.contentChanges.Array, self.encoding);
    // if (textdocument_diagnostics.createNotifyDiagnostics(arena, doc, self.config)) |notification| {
    //     try self.notification_queue.append(notification);
    // } else |err| {
    //     logger.err("createNotifyDiagnostics: {}", .{err});
    // }
}

/// # document sync
/// * https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_didSave
pub fn @"textDocument/didSave"(self: *Self, arena: *std.heap.ArenaAllocator, jsonParams: ?std.json.Value) !void {
    var workspace = self.workspace orelse return error.WorkspaceNotInitialized;
    const params = try lsp.fromDynamicTree(arena, lsp.requests.SaveDocument, jsonParams.?);
    const doc = workspace.getDocument(try FixedPath.fromUri(params.textDocument.uri)) orelse return error.DocumentNotFound;
    _ = doc;
    // try doc.applySave(self.zigenv);
}

/// # document sync
/// * https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_didClose
pub fn @"textDocument/didClose"(self: *Self, arena: *std.heap.ArenaAllocator, jsonParams: ?std.json.Value) !void {
    var workspace = self.workspace orelse return error.WorkspaceNotInitialized;
    const params = try lsp.fromDynamicTree(arena, lsp.requests.CloseDocument, jsonParams.?);
    const doc = workspace.getDocument(try FixedPath.fromUri(params.textDocument.uri)) orelse return error.DocumentNotFound;
    _ = doc;
}

/// # language feature
/// ## document request
/// * https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_formatting
pub fn @"textDocument/formatting"(self: *Self, arena: *std.heap.ArenaAllocator, id: i64, jsonParams: ?std.json.Value) !lsp.Response {
    var workspace = self.workspace orelse return error.WorkspaceNotInitialized;
    const params = try lsp.fromDynamicTree(arena, lsp.requests.Formatting, jsonParams.?);
    const doc = workspace.getDocument(try FixedPath.fromUri(params.textDocument.uri)) orelse return error.DocumentNotFound;

    const stdout_bytes = self.zigenv.spawnZigFmt(arena.allocator(), doc.utf8_buffer.text) catch |err|
        {
        logger.err("zig fmt: {}", .{err});
        return lsp.Response.createNull(id);
    };

    const end = doc.utf8_buffer.text.len;
    const position = try doc.utf8_buffer.getPositionFromBytePosition(end, self.encoding);
    const range = lsp.Range{
        .start = .{
            .line = 0,
            .character = 0,
        },
        .end = .{
            .line = position.line,
            .character = position.x,
        },
    };

    var edits = try arena.allocator().alloc(lsp.TextEdit, 1);
    edits[0] = .{
        .range = range,
        .newText = stdout_bytes,
    };
    return lsp.Response{
        .id = id,
        .result = .{
            .TextEdits = edits,
        },
    };
}

/// # language feature
/// ## document request
/// * https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_documentSymbol
pub fn @"textDocument/documentSymbol"(self: *Self, arena: *std.heap.ArenaAllocator, id: i64, jsonParams: ?std.json.Value) !lsp.Response {
    var workspace = self.workspace orelse return error.WorkspaceNotInitialized;
    const params = try lsp.fromDynamicTree(arena, lsp.requests.DocumentSymbols, jsonParams.?);
    const doc = workspace.getDocument(try FixedPath.fromUri(params.textDocument.uri)) orelse return error.DocumentNotFound;
    const symbols = try textdocument.to_symbols(arena, Project.init(workspace.build_file.import_solver, &workspace.store), doc, self.encoding);
    return lsp.Response{
        .id = id,
        .result = .{
            .DocumentSymbols = symbols,
        },
    };
}

/// # language feature
/// ## document request
/// * https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_semanticTokens
pub fn @"textDocument/semanticTokens/full"(self: *Self, arena: *std.heap.ArenaAllocator, id: i64, jsonParams: ?std.json.Value) !lsp.Response {
    var workspace = self.workspace orelse return error.WorkspaceNotInitialized;
    if (!self.config.enable_semantic_tokens) {
        return lsp.Response{
            .id = id,
            .result = lsp.ResponseParams{ .SemanticTokensFull = .{
                .data = &.{},
            } },
        };
    }

    const params = try lsp.fromDynamicTree(arena, lsp.requests.SemanticTokensFull, jsonParams.?);
    const doc = workspace.getDocument(try FixedPath.fromUri(params.textDocument.uri)) orelse return error.DocumentNotFound;

    var token_array = try SemanticTokensBuilder.writeAllSemanticTokens(arena, doc);
    var array = try std.ArrayList(u32).initCapacity(arena.allocator(), token_array.len * 5);
    for (token_array) |token| {
        const start = try doc.utf8_buffer.getPositionFromBytePosition(token.start, self.encoding);
        const end = try doc.utf8_buffer.getPositionFromBytePosition(token.end - 1, self.encoding);
        std.debug.assert(start.line == end.line);
        try array.appendSlice(&.{
            start.line,
            start.x,
            @truncate(u32, end.x - start.x + 1),
            @enumToInt(token.token_type),
            token.token_modifiers.toInt(),
        });
    }
    // convert to delta
    var data = array.items;
    {
        var prev_line: u32 = 0;
        var prev_character: u32 = 0;
        var i: u32 = 0;
        while (i < data.len) : (i += 5) {
            const current_line = data[i];
            const current_character = data[i + 1];

            data[i] = current_line - prev_line;
            data[i + 1] = current_character - if (current_line == prev_line) prev_character else 0;

            prev_line = current_line;
            prev_character = current_character;
        }
    }
    // logger.debug("semantic tokens: {}", .{data.len});
    return lsp.Response{
        .id = id,
        .result = .{ .SemanticTokensFull = .{ .data = data } },
    };
}

/// # language feature
/// ## document request
/// * https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_signatureHelp
pub fn @"textDocument/codeLens"(self: *Self, arena: *std.heap.ArenaAllocator, id: i64, jsonParams: ?std.json.Value) !lsp.Response {
    var workspace = self.workspace orelse return error.WorkspaceNotInitialized;
    const params = try lsp.fromDynamicTree(arena, lsp.requests.TextDocumentIdentifierRequest, jsonParams.?);
    const doc = workspace.getDocument(try FixedPath.fromUri(params.textDocument.uri)) orelse return error.DocumentNotFound;

    // const tree = doc.ast_context.tree;
    var data = std.ArrayList(lsp.types.CodeLens).init(arena.allocator());
    const tag = doc.ast_context.tree.nodes.items(.tag);
    // var i: u32 = 0;
    var buffer: [2]u32 = undefined;
    const allocator = arena.allocator();
    // while (i < tree.nodes.len) : (i += 1) {
    for (tag) |_, i| {
        const children = AstNodeIterator.NodeChildren.init(doc.ast_context.tree, @intCast(u32, i), &buffer);
        switch (children) {
            .fn_proto => |fn_proto| {
                const token_idx = fn_proto.ast.fn_token;
                const token = AstToken.init(&doc.ast_context.tree, token_idx);
                const n = if (try textdocument_position.getRenferences(arena, workspace, doc, token, true)) |refs| refs.len else 0;
                const loc = token.getLoc();
                const start = try doc.utf8_buffer.getPositionFromBytePosition(loc.start, self.encoding);
                const end = try doc.utf8_buffer.getPositionFromBytePosition(loc.end, self.encoding);
                const text = try std.fmt.allocPrint(allocator, "references {}", .{n});
                _ = text;
                // const arg = try std.fmt.allocPrint(allocator, "{}", .{token_idx});
                // logger.debug("{s}", .{text});
                try data.append(.{
                    .range = .{
                        .start = .{
                            .line = start.line,
                            .character = 0,
                        },
                        .end = .{
                            .line = end.line,
                            .character = 1,
                        },
                    },
                    .command = .{
                        .title = text,
                        .command = "zls.refrences",
                    },
                });
            },
            else => {},
        }
    }

    return lsp.Response{
        .id = id,
        .result = .{ .CodeLens = data.items },
    };
}

pub fn @"codeLens/resolve"(self: *Self, arena: *std.heap.ArenaAllocator, id: i64, jsonParams: ?std.json.Value) !lsp.Response {
    var workspace = self.workspace orelse return error.WorkspaceNotInitialized;
    _ = workspace;
    logJson(arena, jsonParams);
    // const params = try lsp.fromDynamicTree(arena, lsp.requests.TextDocumentIdentifierRequest, jsonParams.?);
    // const doc = workspace.getDocument(try FixedPath.fromUri(params.textDocument.uri)) orelse return error.DocumentNotFound;
    return lsp.Response.createNull(id);
}

/// # language feature
/// ## document position request
/// * https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_hover
pub fn @"textDocument/hover"(self: *Self, arena: *std.heap.ArenaAllocator, id: i64, jsonParams: ?std.json.Value) !lsp.Response {
    var workspace = self.workspace orelse return error.WorkspaceNotInitialized;
    const params = try lsp.fromDynamicTree(arena, lsp.requests.Hover, jsonParams.?);
    const doc = workspace.getDocument(try FixedPath.fromUri(params.textDocument.uri)) orelse return error.DocumentNotFound;
    const position = params.position;
    const line = try doc.utf8_buffer.getLine(@intCast(u32, position.line));
    const byte_position = try line.getBytePosition(@intCast(u32, position.character), self.encoding);
    const token = AstToken.fromBytePosition(&doc.ast_context.tree, byte_position) orelse {
        return lsp.Response.createNull(id);
    };

    const hover_or_null: ?textdocument_position.Hover = try textdocument_position.getHover(
        arena,
        Project.init(workspace.build_file.import_solver, &workspace.store),
        doc,
        token,
    );

    const hover = hover_or_null orelse {
        return lsp.Response.createNull(id);
    };

    var range: ?lsp.types.Range = null;
    if (hover.loc) |loc| {
        const start = try doc.utf8_buffer.getPositionFromBytePosition(loc.start, self.encoding);
        const end = try doc.utf8_buffer.getPositionFromBytePosition(loc.end, self.encoding);
        range = lsp.types.Range{
            .start = .{
                .line = start.line,
                .character = start.x,
            },
            .end = .{
                .line = end.line,
                .character = end.x,
            },
        };
    }

    return lsp.Response{
        .id = id,
        .result = .{
            .Hover = .{
                .contents = .{ .value = hover.text },
                .range = range,
            },
        },
    };
}

/// # language feature
/// ## document position request
/// * https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_definition
pub fn @"textDocument/definition"(self: *Self, arena: *std.heap.ArenaAllocator, id: i64, jsonParams: ?std.json.Value) !lsp.Response {
    var workspace = self.workspace orelse return error.WorkspaceNotInitialized;
    const params = try lsp.fromDynamicTree(arena, lsp.requests.GotoDefinition, jsonParams.?);
    const doc = workspace.getDocument(try FixedPath.fromUri(params.textDocument.uri)) orelse return error.DocumentNotFound;
    const position = params.position;
    const line = try doc.utf8_buffer.getLine(@intCast(u32, position.line));
    const byte_position = try line.getBytePosition(@intCast(u32, position.character), self.encoding);
    const token = AstToken.fromBytePosition(&doc.ast_context.tree, byte_position) orelse {
        return lsp.Response.createNull(id);
    };

    if (try textdocument_position.getGoto(arena, Project.init(workspace.build_file.import_solver, &workspace.store), doc, token)) |location| {
        const goto_doc = (try workspace.getOrLoadDocument(location.path)) orelse return error.DocumentNotFound;
        const goto = try goto_doc.utf8_buffer.getPositionFromBytePosition(location.loc.start, self.encoding);
        const goto_pos = lsp.Position{ .line = goto.line, .character = goto.x };

        const uri = try URI.fromPath(arena.allocator(), location.path.slice());
        return lsp.Response{
            .id = id,
            .result = .{
                .Location = .{
                    .uri = uri,
                    .range = .{
                        .start = goto_pos,
                        .end = goto_pos,
                    },
                },
            },
        };
    } else {
        return lsp.Response.createNull(id);
    }
}

/// # language feature
/// ## document position request
/// * https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_completion
pub fn @"textDocument/completion"(self: *Self, arena: *std.heap.ArenaAllocator, id: i64, jsonParams: ?std.json.Value) !lsp.Response {
    var workspace = self.workspace orelse return error.WorkspaceNotInitialized;
    // var tmp = std.ArrayList(u8).init(arena.allocator());
    // try jsonParams.?.jsonStringify(.{}, tmp.writer());
    // logger.debug("{s}", .{tmp.items});

    const params = try lsp.fromDynamicTree(arena, lsp.completion.Completion, jsonParams.?);
    const doc = workspace.getDocument(try FixedPath.fromUri(params.textDocument.uri)) orelse return error.DocumentNotFound;
    const position = params.position;
    const line = try doc.utf8_buffer.getLine(@intCast(u32, position.line));
    const byte_position = try line.getBytePosition(@intCast(u32, position.character), self.encoding);

    // get token for completion context
    //
    // std.
    //     ^ cursor is here. no token. get previous token
    const token_with_index = doc.ast_context.prevTokenFromBytePos(byte_position) orelse {
        // token not found. return no hover.
        return lsp.Response.createNull(id);
    };

    const completions = try completion_util.process(
        arena,
        workspace,
        doc,
        params.context.triggerCharacter,
        AstToken.init(&doc.ast_context.tree, token_with_index.index),
        self.config,
        if (self.client_capabilities.hover_supports_md) .Markdown else .PlainText,
        self.encoding,
    );

    return lsp.Response{
        .id = id,
        .result = .{
            .CompletionItems = completions,
        },
    };
}

/// # language feature
/// ## document position request
/// * https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_rename
pub fn @"textDocument/rename"(self: *Self, arena: *std.heap.ArenaAllocator, id: i64, jsonParams: ?std.json.Value) !lsp.Response {
    var workspace = self.workspace orelse return error.WorkspaceNotInitialized;
    const params = try lsp.fromDynamicTree(arena, lsp.requests.Rename, jsonParams.?);
    const doc = workspace.getDocument(try FixedPath.fromUri(params.textDocument.uri)) orelse return error.DocumentNotFound;
    const position = params.position;
    const line = try doc.utf8_buffer.getLine(@intCast(u32, position.line));
    const byte_position = try line.getBytePosition(@intCast(u32, position.character), self.encoding);
    const token = AstToken.fromBytePosition(&doc.ast_context.tree, byte_position) orelse {
        return lsp.Response.createNull(id);
    };

    if (try textdocument_position.getRename(arena, workspace, doc, token)) |locations| {
        var changes = std.StringHashMap([]lsp.TextEdit).init(arena.allocator());
        const allocator = arena.allocator();
        for (locations) |location| {
            const uri = try URI.fromPath(arena.allocator(), location.path.slice());
            var text_edits = if (changes.get(uri)) |slice|
                std.ArrayList(lsp.TextEdit).fromOwnedSlice(allocator, slice)
            else
                std.ArrayList(lsp.TextEdit).init(allocator);

            var start = try doc.utf8_buffer.getPositionFromBytePosition(location.loc.start, self.encoding);
            var end = try doc.utf8_buffer.getPositionFromBytePosition(location.loc.end, self.encoding);

            (try text_edits.addOne()).* = .{
                .range = .{
                    .start = .{ .line = start.line, .character = start.x },
                    .end = .{ .line = end.line, .character = end.x },
                },
                .newText = params.newName,
            };
            try changes.put(uri, text_edits.toOwnedSlice());
        }

        return lsp.Response{
            .id = id,
            .result = .{ .WorkspaceEdit = .{ .changes = changes } },
        };
    } else {
        return lsp.Response.createNull(id);
    }
}

/// # language feature
/// ## document position request
/// * https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_references
pub fn @"textDocument/references"(self: *Self, arena: *std.heap.ArenaAllocator, id: i64, jsonParams: ?std.json.Value) !lsp.Response {
    var workspace = self.workspace orelse return error.WorkspaceNotInitialized;
    const params = try lsp.fromDynamicTree(arena, lsp.requests.References, jsonParams.?);
    const doc = workspace.getDocument(try FixedPath.fromUri(params.textDocument.uri)) orelse return error.DocumentNotFound;
    const position = params.position;
    const line = try doc.utf8_buffer.getLine(@intCast(u32, position.line));
    const byte_position = try line.getBytePosition(@intCast(u32, position.character), self.encoding);
    const token = AstToken.fromBytePosition(&doc.ast_context.tree, byte_position) orelse {
        return lsp.Response.createNull(id);
    };

    if (try textdocument_position.getRenferences(
        arena,
        workspace,
        doc,
        token,
        params.context.includeDeclaration,
    )) |src| {
        var locations = std.ArrayList(lsp.Location).init(arena.allocator());
        for (src) |location| {
            const uri = try URI.fromPath(arena.allocator(), location.path.slice());
            var start = try doc.utf8_buffer.getPositionFromBytePosition(location.loc.start, self.encoding);
            var end = try doc.utf8_buffer.getPositionFromBytePosition(location.loc.end, self.encoding);
            if (self.encoding == .utf16) {
                start = try doc.utf8_buffer.utf8PositionToUtf16(start);
                end = try doc.utf8_buffer.utf8PositionToUtf16(end);
            }
            try locations.append(.{
                .uri = uri,
                .range = .{
                    .start = .{ .line = start.line, .character = start.x },
                    .end = .{ .line = end.line, .character = end.x },
                },
            });
        }

        return lsp.Response{
            .id = id,
            .result = .{ .Locations = locations.items },
        };
    } else {
        return lsp.Response.createNull(id);
    }
}

/// # language feature
/// ## document position request
/// * https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_signatureHelp
pub fn @"textDocument/signatureHelp"(self: *Self, arena: *std.heap.ArenaAllocator, id: i64, jsonParams: ?std.json.Value) !lsp.Response {
    var workspace = self.workspace orelse return error.WorkspaceNotInitialized;
    const params = try lsp.fromDynamicTree(arena, lsp.requests.SignatureHelp, jsonParams.?);
    const doc = workspace.getDocument(try FixedPath.fromUri(params.textDocument.uri)) orelse return error.DocumentNotFound;
    const position = params.position;
    const line = try doc.utf8_buffer.getLine(@intCast(u32, position.line));
    const byte_position = try line.getBytePosition(@intCast(u32, position.character), self.encoding);
    const token_with_index = doc.ast_context.tokenFromBytePos(byte_position) orelse {
        // token not found. return no hover.
        return lsp.Response.createNull(id);
    };

    const sig_info = (try getSignatureInfo(
        arena,
        workspace,
        doc,
        token_with_index.index,
        builtin_completions.data(),
    )) orelse {
        logger.warn("no signature info", .{});
        return lsp.Response{
            .id = id,
            .result = .{
                .SignatureHelp = .{
                    .signatures = &.{},
                    .activeSignature = null,
                    .activeParameter = null,
                },
            },
        };
    };

    var signatures = std.ArrayList(lsp.SignatureInformation).init(arena.allocator());
    try signatures.append(sig_info);

    return lsp.Response{
        .id = id,
        .result = .{
            .SignatureHelp = .{
                .signatures = signatures.items,
                .activeSignature = 0,
                .activeParameter = sig_info.activeParameter,
            },
        },
    };
}
