const std = @import("std");
const lsp = @import("lsp");
const Config = @import("./Config.zig");
const Workspace = @import("./Workspace.zig");
const analysis = @import("./analysis.zig");
const semantic_tokens = @import("./semantic_tokens.zig");
const offsets = @import("./offsets.zig");
const document_symbols = @import("./document_symbols.zig");
const hover_util = @import("./hover_util.zig");
const completion_util = @import("./completion_util.zig");
const Self = @This();
const root = @import("root");
pub var keep_running: bool = true;
const logger = std.log.scoped(.LanguageServer);

const ClientCapabilities = @import("./ClientCapabilities.zig");

config: *Config,
workspace: Workspace = undefined,
client_capabilities: ClientCapabilities = .{},

pub fn init(allocator: std.mem.Allocator, config: *Config) Self {
    var self = Self{
        .config = config,
    };

    self.workspace.init(
        allocator,
        config.zig_exe_path,
        config.build_runner_path orelse @panic("no build_runner_path"),
        config.build_runner_cache_path orelse @panic("build_runner_cache_path"),
        config.zig_lib_path,
        // TODO make this configurable
        // We can't figure it out ourselves since we don't know what arguments
        // the user will use to run "zig build"
        "zig-cache",
        // Since we don't compile anything and no packages should put their
        // files there this path can be ignored
        "ZLS_DONT_CARE",
        config.builtin_path,
    ) catch @panic("Workspace.init");

    return self;
}

pub fn deinit(self: *Self) void {
    self.workspace.deinit();
}

pub fn initialize(self: *Self, arena: *std.heap.ArenaAllocator, id: i64, jsonParams: ?std.json.Value) !lsp.Response {
    _ = self;
    const params = try lsp.fromDynamicTree(arena, lsp.requests.Initialize, jsonParams.?);
    for (params.capabilities.offsetEncoding.value) |encoding| {
        if (std.mem.eql(u8, encoding, "utf-8")) {
            offsets.offset_encoding = .utf8;
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

    logger.info("zls initialized", .{});
    logger.info("{}", .{self.client_capabilities});
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

pub fn shutdown(self: *Self, arena: *std.heap.ArenaAllocator, id: i64, jsonParams: ?std.json.Value) !lsp.Response {
    _ = self;
    _ = arena;
    _ = jsonParams;
    keep_running = false;
    return lsp.Response.createNull(id);
}

pub fn @"textDocument/didOpen"(self: *Self, arena: *std.heap.ArenaAllocator, jsonParams: ?std.json.Value) !void {
    const params = try lsp.fromDynamicTree(arena, lsp.requests.OpenDocument, jsonParams.?);
    _ = try self.workspace.openDocument(params.textDocument.uri, params.textDocument.text);
    // if (createNotifyDiagnostics(self, handle)) |notification| {
    //     self.transport.sendToJson(notification);
    // } else |_| {}
}

pub fn @"textDocument/didChange"(self: *Self, arena: *std.heap.ArenaAllocator, jsonParams: ?std.json.Value) !void {
    const params = try lsp.fromDynamicTree(arena, lsp.requests.ChangeDocument, jsonParams.?);
    const handle = try self.workspace.getHandle(params.textDocument.uri);
    try self.workspace.applyChanges(handle, params.contentChanges.Array, offsets.offset_encoding);
    // if (createNotifyDiagnostics(self, handle)) |notification| {
    //     self.transport.sendToJson(notification);
    // } else |_| {}
}

pub fn @"textDocument/didSave"(self: *Self, arena: *std.heap.ArenaAllocator, jsonParams: ?std.json.Value) !void {
    const params = try lsp.fromDynamicTree(arena, lsp.requests.SaveDocument, jsonParams.?);
    const handle = try self.workspace.getHandle(params.textDocument.uri);
    try self.workspace.applySave(handle);
}

pub fn @"textDocument/didClose"(self: *Self, arena: *std.heap.ArenaAllocator, jsonParams: ?std.json.Value) !void {
    const params = try lsp.fromDynamicTree(arena, lsp.requests.CloseDocument, jsonParams.?);
    self.workspace.closeDocument(params.textDocument.uri);
}

pub fn @"textDocument/documentSymbol"(self: *Self, arena: *std.heap.ArenaAllocator, id: i64, jsonParams: ?std.json.Value) !lsp.Response {
    const params = try lsp.fromDynamicTree(arena, lsp.requests.DocumentSymbols, jsonParams.?);
    const handle = try self.workspace.getHandle(params.textDocument.uri);
    return lsp.Response{
        .id = id,
        .result = .{
            .DocumentSymbols = try document_symbols.getDocumentSymbols(arena.allocator(), handle.tree, offsets.offset_encoding),
        },
    };
}

pub fn @"textDocument/hover"(self: *Self, arena: *std.heap.ArenaAllocator, id: i64, jsonParams: ?std.json.Value) !lsp.Response {
    const params = try lsp.fromDynamicTree(arena, lsp.requests.Hover, jsonParams.?);
    const handle = try self.workspace.getHandle(params.textDocument.uri);
    logger.debug("[hover]{s} {}", .{ params.textDocument.uri, params.position });
    const doc_position = try offsets.documentPosition(handle.document, params.position, offsets.offset_encoding);
    return try hover_util.process(arena, &self.workspace, id, handle, doc_position, &self.client_capabilities);
}

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
    const handle = try self.workspace.getHandle(params.textDocument.uri);
    const token_array = try semantic_tokens.writeAllSemanticTokens(arena, &self.workspace, handle, offsets.offset_encoding);
    return lsp.Response{
        .id = id,
        .result = .{ .SemanticTokensFull = .{ .data = token_array } },
    };
}

pub fn @"textDocument/formatting"(self: *Self, arena: *std.heap.ArenaAllocator, id: i64, jsonParams: ?std.json.Value) !lsp.Response {
    const zig_exe_path = self.config.zig_exe_path orelse {
        logger.warn("no zig_exe_path", .{});
        return lsp.Response.createNull(id);
    };

    const params = try lsp.fromDynamicTree(arena, lsp.requests.Formatting, jsonParams.?);
    const handle = try self.workspace.getHandle(params.textDocument.uri);
    var process = std.ChildProcess.init(&[_][]const u8{ zig_exe_path, "fmt", "--stdin" }, arena.allocator());
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

pub fn @"textDocument/definition"(self: *Self, arena: *std.heap.ArenaAllocator, id: i64, jsonParams: ?std.json.Value) !lsp.Response {
    const params = try lsp.fromDynamicTree(arena, lsp.requests.GotoDefinition, jsonParams.?);
    logger.debug("[definition]{s} {}", .{ params.textDocument.uri, params.position });
    const handle = try self.workspace.getHandle(params.textDocument.uri);
    const doc_position = try offsets.documentPosition(handle.document, params.position, offsets.offset_encoding);
    return try offsets.gotoHandler(arena, &self.workspace, id, handle, doc_position, true);
}

pub fn @"$/cancelRequest"(self: *Self, arena: *std.heap.ArenaAllocator, jsonParams: ?std.json.Value) !void {
    _ = self;
    _ = arena;
    _ = jsonParams;
}

pub fn @"textDocument/completion"(self: *Self, arena: *std.heap.ArenaAllocator, id: i64, jsonParams: ?std.json.Value) !lsp.Response {
    const params = try lsp.fromDynamicTree(arena, lsp.requests.Completion, jsonParams.?);
    const handle = try self.workspace.getHandle(params.textDocument.uri);
    const doc_position = try offsets.documentPosition(handle.document, params.position, offsets.offset_encoding);
    return completion_util.process(arena, &self.workspace, id, handle, doc_position, self.config, &self.client_capabilities);
}
