const std = @import("std");
const lsp = @import("lsp");
const Workspace = @import("./Workspace.zig");
const Completion = @import("./builtin_completions.zig").Completion;
const Config = @import("./Config.zig");
const Stdio = @import("./Stdio.zig");

const SessionError = error{
    DocumentNotExists,
};

const Self = @This();

// global(not deinit)
allocator: std.mem.Allocator,
config: *Config,
document_store: *Workspace,
completion: *Completion,
transport: *Stdio,

// par request session(deinit each session)
// Arena used for temporary allocations while handling a request
arena: *std.heap.ArenaAllocator,
tree: std.json.ValueTree,

pub fn init(
    allocator: std.mem.Allocator,
    arena: *std.heap.ArenaAllocator,
    config: *Config,
    document_store: *Workspace,
    completion: *Completion,
    transport: *Stdio,
    tree: std.json.ValueTree,
) Self {

    return .{
        .allocator = allocator,
        .config = config,
        .document_store = document_store,
        .completion = completion,
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

/// Sends a request or response
fn showMessage(self: *Self, message_type: lsp.MessageType, message: []const u8) void {
    self.transport.sendToJson(lsp.Notification{
        .method = "window/showMessage",
        .params = .{
            .ShowMessageParams = .{
                .type = message_type,
                .message = message,
            },
        },
    });
}

pub fn getHandle(self: *Self, uri: []const u8) SessionError!*Workspace.Handle {
    if (self.document_store.getHandle(uri)) |handle| {
        return handle;
    } else {
        return SessionError.DocumentNotExists;
    }
}
