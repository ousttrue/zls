const std = @import("std");
const types = @import("./types.zig");
const readRequestHeader = @import("./header.zig").readRequestHeader;

const logger = std.log.scoped(.jsonrpc);
var stdout: std.io.BufferedWriter(4096, std.fs.File.Writer) = undefined;

pub const RpcError = error{
    Format,
    NotImplemented,
};

pub const RequestProto = fn (arena: *std.heap.ArenaAllocator, tree: std.json.ValueTree, id: types.RequestId) anyerror!types.Response;
pub const NotifyProto = fn (arena: *std.heap.ArenaAllocator, tree: std.json.ValueTree) anyerror!void;

pub var request_map: std.StringHashMap(RequestProto) = undefined;
pub var notify_map: std.StringHashMap(NotifyProto) = undefined;

/// Sends a request or response
pub fn send(arena: *std.heap.ArenaAllocator, reqOrRes: anytype) void {
    var arr = std.ArrayList(u8).init(arena.allocator());
    std.json.stringify(reqOrRes, .{}, arr.writer()) catch @panic("stringify");

    const stdout_stream = stdout.writer();
    stdout_stream.print("Content-Length: {}\r\n\r\n", .{arr.items.len}) catch @panic("send");
    stdout_stream.writeAll(arr.items) catch @panic("send");
    stdout.flush() catch @panic("send");
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

fn request(arena: *std.heap.ArenaAllocator, tree: std.json.ValueTree, id: types.RequestId, method: []const u8) void {
    if (request_map.get(method)) |handler| {
        const start_time = std.time.milliTimestamp();
        if (handler(arena, tree, id)) |res| {
            send(arena, res);
            const end_time = std.time.milliTimestamp();
            logger.debug("id[{}] {s} => {}ms", .{ id.toInt(i64), method, end_time - start_time });
        } else |err| {
            logger.warn("id[{}] {s} => {s}", .{ id.toInt(i64), method, @errorName(err) });
            const res = types.Response.createErrorNotImplemented(id);
            send(arena, res);
        }
    } else {
        // no method
        logger.warn("id[{}] {s} => unknown request", .{ id.toInt(i64), method });
        const res = types.Response.createNull(id);
        send(arena, res);
    }
}

fn notify(arena: *std.heap.ArenaAllocator, tree: std.json.ValueTree, method: []const u8) void {
    if (notify_map.get(method)) |handler| {
        const start_time = std.time.milliTimestamp();
        if (handler(arena, tree)) {
            const end_time = std.time.milliTimestamp();
            logger.debug("{s} => {}ms", .{ method, end_time - start_time });
        } else |err| {
            logger.warn("{s} => {s}", .{ method, @errorName(err) });
        }
    } else {
        logger.warn("{s} => unknown notify", .{method});
    }
}

pub fn process(arena: *std.heap.ArenaAllocator, tree: std.json.ValueTree) RpcError!void {
    // request: id, method, ?params
    // reponse: id, ?result, ?error
    // notify: method, ?params
    if (tree.root.Object.get("id")) |idValue| {
        if (tree.root.Object.get("method")) |method| {
            // request
            if (types.RequestId.fromJson(idValue)) |id| {
                request(arena, tree, id, method.String);
            } else {
                return RpcError.Format;
            }
        } else {
            // response
            return RpcError.NotImplemented;
        }
    } else {
        if (tree.root.Object.get("method")) |method| {
            // notify
            notify(arena, tree, method.String);
        } else {
            // invalid
            return RpcError.Format;
        }
    }
}

pub var keep_running = false;
pub fn shutdownHandler(arena: *std.heap.ArenaAllocator, tree: std.json.ValueTree, id: types.RequestId) !types.Response {
    _ = arena;
    _ = tree;
    logger.info("Server closing...", .{});
    keep_running = false;
    // Technically we should deinitialize first and send possible errors to the client
    return types.Response.createNull(id);
}

pub fn readloop(allocator: std.mem.Allocator, r: std.fs.File, w: std.fs.File, notifyQueue: *std.ArrayList(types.Notification)) void {
    stdout = std.io.bufferedWriter(w.writer());
    keep_running = true;
    const reader = r.reader();

    // Arena used for temporary allocations while handling a request
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    // This JSON parser is passed to processJsonRpc and reset.
    var json_parser = std.json.Parser.init(allocator, false);
    defer json_parser.deinit();

    while (keep_running) {
        const headers = readRequestHeader(arena.allocator(), reader) catch @panic("readRequestHeader");
        const buf = arena.allocator().alloc(u8, headers.content_length) catch @panic("arena.alloc");
        reader.readNoEof(buf) catch @panic("readNoEof");

        var tree = json_parser.parse(buf) catch @panic("jason.parse");
        defer {
            tree.deinit();
            json_parser.reset();
            arena.deinit();
            arena.state = .{};
        }

        process(&arena, tree) catch |err|
            {
            switch (err) {
                RpcError.Format => @panic("jsonrpc: format"),
                RpcError.NotImplemented => @panic("jsonrpc: not implemented"),
            }
        };

        for (notifyQueue.items) |*message| {
            send(&arena, message.*);
            logger.debug("notify...", .{});
        }
        notifyQueue.resize(0) catch @panic("resize");
    }
}
