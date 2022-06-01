const std = @import("std");
const Config = @import("./Config.zig");
const types = @import("./types.zig");

const logger = std.log.scoped(.jsonrpc);
pub var stdout: std.io.BufferedWriter(4096, std.fs.File.Writer) = undefined;

pub const RpcError = error{
    Format,
    NotImplemented,
};

pub const RequestProto = fn (arena: *std.heap.ArenaAllocator, config: Config, tree: std.json.ValueTree, id: types.RequestId) anyerror!types.Response;
pub const NotifyProto = fn (arena: *std.heap.ArenaAllocator, config: Config, tree: std.json.ValueTree) anyerror!void;

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

fn request(arena: *std.heap.ArenaAllocator, config: Config, tree: std.json.ValueTree, id: types.RequestId, method: []const u8) void {
    if (request_map.get(method)) |handler| {
        const start_time = std.time.milliTimestamp();
        if (handler(arena, config, tree, id)) |res| {
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

fn notify(arena: *std.heap.ArenaAllocator, config: Config, tree: std.json.ValueTree, method: []const u8) void {
    if (notify_map.get(method)) |handler| {
        const start_time = std.time.milliTimestamp();
        if (handler(arena, config, tree)) {
            const end_time = std.time.milliTimestamp();
            logger.debug("{s} => {}ms", .{ method, end_time - start_time });
        } else |err| {
            logger.warn("{s} => {s}", .{ method, @errorName(err) });
        }
    } else {
        logger.warn("{s} => unknown notify", .{method});
    }
}

pub fn process(arena: *std.heap.ArenaAllocator, config: Config, tree: std.json.ValueTree) RpcError!void {
    // request: id, method, ?params
    // reponse: id, ?result, ?error
    // notify: method, ?params
    if (tree.root.Object.get("id")) |idValue| {
        if (tree.root.Object.get("method")) |method| {
            // request
            if (types.RequestId.fromJson(idValue)) |id| {
                request(arena, config, tree, id, method.String);
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
            notify(arena, config, tree, method.String);
        } else {
            // invalid
            return RpcError.Format;
        }
    }
}
