const std = @import("std");
const lsp = @import("lsp");
const Workspace = @import("./Workspace.zig");
const Document = @import("./Document.zig");
const builtin_completions = @import("./builtin_completions.zig");
const Config = @import("./Config.zig");
const Stdio = @import("./Stdio.zig");

const Self = @This();

// global(not deinit)
allocator: std.mem.Allocator,
config: *Config,
workspace: *Workspace,
transport: *Stdio,

// par request session(deinit each session)
// Arena used for temporary allocations while handling a request
arena: *std.heap.ArenaAllocator,
tree: std.json.ValueTree,

pub fn init(
    allocator: std.mem.Allocator,
    arena: *std.heap.ArenaAllocator,
    config: *Config,
    workspace: *Workspace,
    transport: *Stdio,
    tree: std.json.ValueTree,
) Self {

    return .{
        .allocator = allocator,
        .config = config,
        .workspace = workspace,
        .transport = transport,
        .arena = arena,
        .tree = tree,
    };
}

pub fn deinit(self: *Self) void {
    self.tree.deinit();
    self.arena.deinit();
    self.arena.state = .{};
}

pub fn getId(self: *Self) ?i64 {
    if (self.tree.root.Object.get("id")) |child| {
        switch (child) {
            .Integer => |int| return int,
            else => {},
        }
    }
    return null;
}

pub fn getMethod(self: *Self) ?[]const u8 {
    if (self.tree.root.Object.get("method")) |child| {
        switch (child) {
            .String => |str| return str,
            else => {},
        }
    }
    return null;
}

pub fn getParam(self: *Self, comptime ParamType: type) !ParamType {
    return lsp.fromDynamicTree(self.arena, ParamType, self.tree.root);
}
