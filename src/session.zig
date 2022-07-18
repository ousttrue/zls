const std = @import("std");
const lsp = @import("lsp");
const readRequestHeader = @import("./header.zig").readRequestHeader;
const DocumentStore = @import("./DocumentStore.zig");
const Completion = @import("./builtin_completions.zig").Completion;
const Config = @import("./Config.zig");

const SessionError = error{
    DocumentNotExists,
};

pub const Session = struct {
    const Self = @This();

    // global(not deinit)
    allocator: std.mem.Allocator,
    config: *Config,
    document_store: *DocumentStore,
    completion: *Completion,
    writer: std.io.BufferedWriter(4096, std.fs.File.Writer),

    // par request session(deinit each session)
    // Arena used for temporary allocations while handling a request
    arena: *std.heap.ArenaAllocator,
    tree: std.json.ValueTree,

    pub fn init(allocator: std.mem.Allocator, config: *Config, document_store: *DocumentStore, completion: *Completion, writer: anytype, arena: *std.heap.ArenaAllocator, reader: anytype, json_parser: *std.json.Parser) Self {
        // read
        const headers = readRequestHeader(arena.allocator(), reader) catch @panic("readRequestHeader");
        const buf = arena.allocator().alloc(u8, headers.content_length) catch @panic("arena.alloc");
        reader.readNoEof(buf) catch @panic("readNoEof");
        // parse
        const tree = json_parser.parse(buf) catch @panic("parseError");
        defer json_parser.reset();

        return .{
            .allocator = allocator,
            .config = config,
            .document_store = document_store,
            .completion = completion,
            .writer = writer,

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
    pub fn send(self: *Self, reqOrRes: anytype) void {
        var arr = std.ArrayList(u8).init(self.arena.allocator());
        std.json.stringify(reqOrRes, .{}, arr.writer()) catch @panic("stringify");

        const stdout_stream = self.writer.writer();
        stdout_stream.print("Content-Length: {}\r\n\r\n", .{arr.items.len}) catch @panic("send");
        stdout_stream.writeAll(arr.items) catch @panic("send");
        self.writer.flush() catch @panic("send");
    }

    fn showMessage(self: *Self, message_type: lsp.MessageType, message: []const u8) void {
        self.send(lsp.Notification{
            .method = "window/showMessage",
            .params = .{
                .ShowMessageParams = .{
                    .type = message_type,
                    .message = message,
                },
            },
        });
    }

    pub fn getHandle(self: *Self, uri: []const u8) SessionError!*DocumentStore.Handle {
        if (self.document_store.getHandle(uri)) |handle| {
            return handle;
        } else {
            return SessionError.DocumentNotExists;
        }
    }
};
