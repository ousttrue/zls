const std = @import("std");
const lsp = @import("lsp");
const Config = @import("./Config.zig");
const Workspace = @import("./Workspace.zig");
const server = @import("./server.zig");
const analysis = @import("./analysis.zig");
const semantic_tokens = @import("./semantic_tokens.zig");
const offsets = @import("./offsets.zig");
const document_symbols = @import("./document_symbols.zig");
const Self = @This();
const root = @import("root");
pub var keep_running: bool = true;

config: *Config,
workspace: Workspace = undefined,

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
    return try server.initializeHandler(id, params);
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
        .result = .{ .DocumentSymbols = try document_symbols.getDocumentSymbols(arena.allocator(), handle.tree, offsets.offset_encoding) ,},
    };
}

const no_semantic_tokens_response = lsp.ResponseParams{
    .SemanticTokensFull = .{
        .data = &.{},
    },
};

pub fn @"textDocument/semanticTokens/full"(self: *Self, arena: *std.heap.ArenaAllocator, id: i64, jsonParams: ?std.json.Value) !lsp.Response {
    if (!self.config.enable_semantic_tokens) {
        return lsp.Response{ .id = id, .result = no_semantic_tokens_response };
    }
    const params = try lsp.fromDynamicTree(arena, lsp.requests.SemanticTokensFull, jsonParams.?);
    const handle = try self.workspace.getHandle(params.textDocument.uri);
    const token_array = try semantic_tokens.writeAllSemanticTokens(arena, &self.workspace, handle, offsets.offset_encoding);
    return lsp.Response{
        .id = id,
        .result = .{ .SemanticTokensFull = .{ .data = token_array } },
    };
}
