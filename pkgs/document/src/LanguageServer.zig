const std = @import("std");
const lsp = @import("lsp");
const Config = @import("./Config.zig");
const Workspace = @import("./Workspace.zig");
const server = @import("./server.zig");
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

pub fn initialize(self: *Self, arena: *std.heap.ArenaAllocator, id: i64, params: ?std.json.Value) !lsp.Response {
    _ = self;
    const initializeParams = try lsp.fromDynamicTree(arena, lsp.requests.Initialize, params.?);
    return try server.initializeHandler(id, initializeParams);
}

pub fn shutdown(self: *Self, arena: *std.heap.ArenaAllocator, id: i64, params: ?std.json.Value) !lsp.Response {
    _ = self;
    _ = arena;
    _ = params;
    keep_running = false;
    return lsp.Response.createNull(id);
}
