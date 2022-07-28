//! A LanguageServer frontend, registered to a JsonRPC dispatcher.
const std = @import("std");
const lsp = @import("lsp");
const ws = @import("workspace");
const Config = ws.Config;
const ZigEnv = ws.ZigEnv;
const Workspace = ws.Workspace;
const LinePosition = ws.LinePosition;
const Line = ws.Line;
const semantic_tokens = ws.semantic_tokens;
const SemanticTokensBuilder = ws.SemanticTokensBuilder;
const SymbolTree = ws.SymbolTree;
const hover_util = ws.hover_util;
const completion_util = ws.completion_util;
const ClientCapabilities = ws.ClientCapabilities;
const builtin_completions = ws.builtin_completions;
const rename_util = @import("./rename_util.zig");
const references_util = @import("./references_util.zig");
const getSignatureInfo = ws.signature_help.getSignatureInfo;

const textdocument_symbol = @import("./textdocument_symbol.zig");
const textdocument_diagnostics = @import("./textdocument_diagnostics.zig");
const textdocument_goto = @import("./textdocument_goto.zig");

const logger = std.log.scoped(.LanguageServer);
pub var keep_running: bool = true;
const Self = @This();

config: *Config,
zigenv: ZigEnv,
workspace: Workspace = undefined,
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
    .declarationProvider = false,
    .definitionProvider = true,
    .typeDefinitionProvider = false,
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
        .config = config,
        .zigenv = zigenv,
        .workspace = Workspace.init(allocator, zigenv),
        .notification_queue = std.ArrayList(lsp.Notification).init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    self.notification_queue.deinit();
    self.workspace.deinit();
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

    // logger.info("zls initialized", .{});
    // logger.info("{}", .{self.client_capabilities});
    // logger.info("Using offset encoding: {s}", .{@tagName(self.encoding)});

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
    const params = try lsp.fromDynamicTree(arena, lsp.requests.OpenDocument, jsonParams.?);
    const doc = try self.workspace.openDocument(params.textDocument.uri, params.textDocument.text);
    if (textdocument_diagnostics.createNotifyDiagnostics(arena, doc, self.config)) |notification| {
        try self.notification_queue.append(notification);
    } else |err| {
        logger.err("createNotifyDiagnostics: {}", .{err});
    }
}

/// # document sync
/// * https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_didChange
pub fn @"textDocument/didChange"(self: *Self, arena: *std.heap.ArenaAllocator, jsonParams: ?std.json.Value) !void {
    const params = try lsp.fromDynamicTree(arena, lsp.requests.ChangeDocument, jsonParams.?);
    const doc = self.workspace.getDocument(params.textDocument.uri) orelse return error.DocumentNotFound;
    try doc.applyChanges(params.contentChanges.Array, self.encoding, self.zigenv);
    if (textdocument_diagnostics.createNotifyDiagnostics(arena, doc, self.config)) |notification| {
        try self.notification_queue.append(notification);
    } else |err| {
        logger.err("createNotifyDiagnostics: {}", .{err});
    }
}

/// # document sync
/// * https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_didSave
pub fn @"textDocument/didSave"(self: *Self, arena: *std.heap.ArenaAllocator, jsonParams: ?std.json.Value) !void {
    const params = try lsp.fromDynamicTree(arena, lsp.requests.SaveDocument, jsonParams.?);
    const doc = self.workspace.getDocument(params.textDocument.uri) orelse return error.DocumentNotFound;
    try doc.applySave(self.zigenv);
}

/// # document sync
/// * https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_didClose
pub fn @"textDocument/didClose"(self: *Self, arena: *std.heap.ArenaAllocator, jsonParams: ?std.json.Value) !void {
    const params = try lsp.fromDynamicTree(arena, lsp.requests.CloseDocument, jsonParams.?);
    const doc = self.workspace.getDocument(params.textDocument.uri) orelse return error.DocumentNotFound;
    _ = doc;
}

/// # language feature
/// ## document request
/// * https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_formatting
pub fn @"textDocument/formatting"(self: *Self, arena: *std.heap.ArenaAllocator, id: i64, jsonParams: ?std.json.Value) !lsp.Response {
    const params = try lsp.fromDynamicTree(arena, lsp.requests.Formatting, jsonParams.?);
    const doc = self.workspace.getDocument(params.textDocument.uri) orelse return error.DocumentNotFound;

    const stdout_bytes = self.zigenv.spawnZigFmt(arena.allocator(), doc.utf8_buffer.text) catch |err|
        {
        logger.err("zig fmt: {}", .{err});
        return lsp.Response.createNull(id);
    };

    const end = doc.utf8_buffer.text.len;
    const position = try doc.line_position.getPositionFromBytePosition(end, self.encoding);
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
    const params = try lsp.fromDynamicTree(arena, lsp.requests.DocumentSymbols, jsonParams.?);
    const doc = self.workspace.getDocument(params.textDocument.uri) orelse return error.DocumentNotFound;
    var symbol_tree = SymbolTree.init(arena.allocator(), doc.tree);
    try symbol_tree.traverse(0);
    // logger.debug("{} symbols", .{symbol_tree.symbols.items.len});
    const symbols = try textdocument_symbol.to_symbols(arena.allocator(), doc, self.encoding, symbol_tree.symbols.items, 0);
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
    if (!self.config.enable_semantic_tokens) {
        return lsp.Response{
            .id = id,
            .result = lsp.ResponseParams{ .SemanticTokensFull = .{
                .data = &.{},
            } },
        };
    }
    const params = try lsp.fromDynamicTree(arena, lsp.requests.SemanticTokensFull, jsonParams.?);
    const doc = self.workspace.getDocument(params.textDocument.uri) orelse return error.DocumentNotFound;
    var token_array = try SemanticTokensBuilder.writeAllSemanticTokens(arena, doc);
    var array = try std.ArrayList(u32).initCapacity(arena.allocator(), token_array.len * 5);
    for (token_array) |token| {
        const start = try doc.line_position.getPositionFromBytePosition(token.start, self.encoding);
        const end = try doc.line_position.getPositionFromBytePosition(token.end - 1, self.encoding);
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
/// ## document position request
/// * https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_hover
pub fn @"textDocument/hover"(self: *Self, arena: *std.heap.ArenaAllocator, id: i64, jsonParams: ?std.json.Value) !lsp.Response {
    const params = try lsp.fromDynamicTree(arena, lsp.requests.Hover, jsonParams.?);
    const doc = self.workspace.getDocument(params.textDocument.uri) orelse return error.DocumentNotFound;
    const position = params.position;
    const line = try doc.line_position.getLine(@intCast(u32, position.line));
    const byte_position = try line.getBytePosition(@intCast(u32, position.character), self.encoding);

    if (hover_util.process(arena, &self.workspace, doc, byte_position, &self.client_capabilities)) |hover_or_null| {
        if (hover_or_null) |hover_contents| {
            return lsp.Response{
                .id = id,
                .result = .{
                    .Hover = .{
                        .contents = .{ .value = hover_contents },
                    },
                },
            };
        } else {
            return lsp.Response.createNull(id);
        }
    } else |err| {
        const hover_contents = try std.fmt.allocPrint(arena.allocator(), "{}", .{err});
        return lsp.Response{
            .id = id,
            .result = .{
                .Hover = .{
                    .contents = .{ .value = hover_contents },
                },
            },
        };
    }
}

/// # language feature
/// ## document position request
/// * https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_definition
pub fn @"textDocument/definition"(self: *Self, arena: *std.heap.ArenaAllocator, id: i64, jsonParams: ?std.json.Value) !lsp.Response {
    const params = try lsp.fromDynamicTree(arena, lsp.requests.GotoDefinition, jsonParams.?);
    logger.debug("[definition]{s} {}", .{ params.textDocument.uri, params.position });
    const doc = self.workspace.getDocument(params.textDocument.uri) orelse return error.DocumentNotFound;
    const position = params.position;
    const line = try doc.line_position.getLine(@intCast(u32, position.line));
    const byte_position = try line.getBytePosition(@intCast(u32, position.character), self.encoding);

    if (try textdocument_goto.gotoHandler(arena, &self.workspace, doc, @intCast(u32, byte_position), true)) |location| {
        const goto_doc = self.workspace.getDocument(location.uri) orelse return error.DocumentNotFound;
        const goto = try goto_doc.line_position.getPositionFromBytePosition(location.loc.start, self.encoding);
        const goto_pos = lsp.Position{ .line = goto.line, .character = goto.x };

        return lsp.Response{
            .id = id,
            .result = .{
                .Location = .{
                    .uri = location.uri,
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
    // var tmp = std.ArrayList(u8).init(arena.allocator());
    // try jsonParams.?.jsonStringify(.{}, tmp.writer());
    // logger.debug("{s}", .{tmp.items});

    const params = try lsp.fromDynamicTree(arena, lsp.completion.Completion, jsonParams.?);
    const doc = self.workspace.getDocument(params.textDocument.uri) orelse return error.DocumentNotFound;
    const position = params.position;
    const line = try doc.line_position.getLine(@intCast(u32, position.line));
    const byte_position = try line.getBytePosition(@intCast(u32, position.character), self.encoding);
    // _ = byte_position;

    const completions = try completion_util.process(
        arena,
        &self.workspace,
        doc,
        params.context.triggerCharacter,
        byte_position,
        self.config,
        &self.client_capabilities,
    );
    // logger.debug("{} completions", .{completions.len});
    // logger.debug("{s}", .{completions[0]});

    // const allocator = arena.allocator();
    // var completions = std.ArrayList(lsp.CompletionItem).init(allocator);
    // try completions.append(.{
    //     .label = "lajfkdjkla",
    // });

    const response = lsp.Response{
        .id = id,
        .result = .{
            .CompletionItems = completions,
        },
    };

    // try tmp.resize(0);
    // std.json.stringify(response, .{.emit_null_optional_fields=false}, tmp.writer()) catch unreachable;
    // logger.debug("{s}", .{tmp.items});

    return response;
}

/// # language feature
/// ## document position request
/// * https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_rename
pub fn @"textDocument/rename"(self: *Self, arena: *std.heap.ArenaAllocator, id: i64, jsonParams: ?std.json.Value) !lsp.Response {
    const params = try lsp.fromDynamicTree(arena, lsp.requests.Rename, jsonParams.?);
    const doc = self.workspace.getDocument(params.textDocument.uri) orelse return error.DocumentNotFound;
    const position = params.position;
    const line = try doc.line_position.getLine(@intCast(u32, position.line));
    const byte_position = try line.getBytePosition(@intCast(u32, position.character), self.encoding);

    if (try rename_util.process(arena, &self.workspace, doc, byte_position)) |locations| {
        var changes = std.StringHashMap([]lsp.TextEdit).init(arena.allocator());
        const allocator = arena.allocator();
        for (locations) |loc| {
            var text_edits = if (changes.get(loc.uri)) |slice|
                std.ArrayList(lsp.TextEdit).fromOwnedSlice(allocator, slice)
            else
                std.ArrayList(lsp.TextEdit).init(allocator);

            var start = try doc.line_position.getPositionFromBytePosition(loc.loc.start, self.encoding);
            var end = try doc.line_position.getPositionFromBytePosition(loc.loc.end, self.encoding);

            (try text_edits.addOne()).* = .{
                .range = .{
                    .start = .{ .line = start.line, .character = start.x },
                    .end = .{ .line = end.line, .character = end.x },
                },
                .newText = params.newName,
            };
            try changes.put(loc.uri, text_edits.toOwnedSlice());
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
    const params = try lsp.fromDynamicTree(arena, lsp.requests.References, jsonParams.?);
    const doc = self.workspace.getDocument(params.textDocument.uri) orelse return error.DocumentNotFound;
    const position = params.position;
    const line = try doc.line_position.getLine(@intCast(u32, position.line));
    const byte_position = try line.getBytePosition(@intCast(u32, position.character), self.encoding);

    if (try references_util.process(
        arena,
        &self.workspace,
        doc,
        byte_position,
        params.context.includeDeclaration,
        self.config,
    )) |src| {
        var locations = std.ArrayList(lsp.Location).init(arena.allocator());
        for (src) |loc| {
            var start = try doc.line_position.getPositionFromBytePosition(loc.loc.start, self.encoding);
            var end = try doc.line_position.getPositionFromBytePosition(loc.loc.end, self.encoding);
            if (self.encoding == .utf16) {
                start = try doc.line_position.utf8PositionToUtf16(start);
                end = try doc.line_position.utf8PositionToUtf16(end);
            }
            try locations.append(.{
                .uri = loc.uri,
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
    const params = try lsp.fromDynamicTree(arena, lsp.requests.SignatureHelp, jsonParams.?);
    const doc = self.workspace.getDocument(params.textDocument.uri) orelse return error.DocumentNotFound;
    const position = params.position;
    const line = try doc.line_position.getLine(@intCast(u32, position.line));
    const byte_position = try line.getBytePosition(@intCast(u32, position.character), self.encoding);

    const sig_info = (try getSignatureInfo(
        arena,
        &self.workspace,
        doc,
        byte_position,
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
