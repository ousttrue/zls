const std = @import("std");
const lsp = @import("lsp");
const readRequestHeader = @import("./header.zig").readRequestHeader;

const logger = std.log.scoped(.jsonrpc);
var stdout: std.io.BufferedWriter(4096, std.fs.File.Writer) = undefined;

pub const RpcError = error{
    // Parse,
    InvalidRequest,
    MethodNotFound,
    InvalidParams,
    InternalError,
};

pub const RequestProto = fn (arena: *std.heap.ArenaAllocator, tree: std.json.ValueTree, id: lsp.RequestId) anyerror!lsp.Response;
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

fn showMessage(message_type: lsp.MessageType, message: []const u8) !void {
    try send(lsp.Notification{
        .method = "window/showMessage",
        .params = .{
            .ShowMessageParams = .{
                .type = message_type,
                .message = message,
            },
        },
    });
}

fn request(arena: *std.heap.ArenaAllocator, tree: std.json.ValueTree, id: lsp.RequestId, method: []const u8) RpcError!void {
    if (request_map.get(method)) |handler| {
        const start_time = std.time.milliTimestamp();
        if (handler(arena, tree, id)) |res| {
            const end_time = std.time.milliTimestamp();
            logger.info("id[{}] {s} => {}ms", .{ id.toInt(i64), method, end_time - start_time });
            send(arena, res);
        } else |err| {
            logger.err("id[{}] {s} => {s}", .{ id.toInt(i64), method, @errorName(err) });
            return RpcError.InternalError;
        }
    } else {
        // no method
        logger.err("id[{}] {s} => unknown request", .{ id.toInt(i64), method });
        return RpcError.MethodNotFound;
    }
}

fn notify(arena: *std.heap.ArenaAllocator, tree: std.json.ValueTree, method: []const u8) RpcError!void {
    if (notify_map.get(method)) |handler| {
        const start_time = std.time.milliTimestamp();
        if (handler(arena, tree)) {
            const end_time = std.time.milliTimestamp();
            logger.info("{s} => {}ms", .{ method, end_time - start_time });
        } else |err| {
            logger.err("{s} => {s}", .{ method, @errorName(err) });
            return RpcError.InternalError;
        }
    } else {
        logger.err("{s} => unknown notify", .{method});
        return RpcError.MethodNotFound;
    }
}

pub fn process(arena: *std.heap.ArenaAllocator, tree: std.json.ValueTree) void {
    // request: id, method, ?params
    // reponse: id, ?result, ?error
    // notify: method, ?params
    if (tree.root.Object.get("id")) |idValue| {
        if (tree.root.Object.get("method")) |method| {
            // request
            if (lsp.RequestId.fromJson(idValue)) |id| {
                request(arena, tree, id, method.String) catch |err| switch (err) {
                    RpcError.InvalidRequest => send(arena, lsp.Response.createInvalidRequest(id)),
                    RpcError.MethodNotFound => send(arena, lsp.Response.createMethodNotFound(id)),
                    RpcError.InvalidParams => send(arena, lsp.Response.createInvalidParams(id)),
                    RpcError.InternalError => send(arena, lsp.Response.createInternalError(id)),
                };
            } else {
                // invalid
                send(arena, lsp.Response.createParseError());
            }
        } else {
            // response
            @panic("NotImplemented");
        }
    } else {
        if (tree.root.Object.get("method")) |method| {
            // notify
            notify(arena, tree, method.String) catch |err| switch (err) {
                RpcError.InvalidRequest => send(arena, lsp.Response.createInvalidRequest(.{ .Null = null })),
                RpcError.MethodNotFound => send(arena, lsp.Response.createMethodNotFound(.{ .Null = null })),
                RpcError.InvalidParams => send(arena, lsp.Response.createInvalidParams(.{ .Null = null })),
                RpcError.InternalError => send(arena, lsp.Response.createInternalError(.{ .Null = null })),
            };
        } else {
            // invalid
            send(arena, lsp.Response.createParseError());
        }
    }
}

pub var keep_running = false;
pub fn shutdownHandler(arena: *std.heap.ArenaAllocator, tree: std.json.ValueTree, id: lsp.RequestId) !lsp.Response {
    _ = arena;
    _ = tree;
    logger.info("Server closing...", .{});
    keep_running = false;
    // Technically we should deinitialize first and send possible errors to the client
    return lsp.Response.createNull(id);
}

pub fn readloop(allocator: std.mem.Allocator, r: std.fs.File, w: std.fs.File, notifyQueue: *std.ArrayList(lsp.Notification)) void {
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

        var tree = json_parser.parse(buf) catch {
            send(&arena, lsp.Response.createParseError());
            return;
        };

        defer {
            tree.deinit();
            json_parser.reset();
            arena.deinit();
            arena.state = .{};
        }

        process(&arena, tree);

        for (notifyQueue.items) |*message| {
            send(&arena, message.*);
            logger.debug("notify...", .{});
        }
        notifyQueue.resize(0) catch @panic("resize");
    }
}
