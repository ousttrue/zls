const std = @import("std");
const lsp = @import("lsp");
const Session = @import("./Session.zig");
const logger = std.log.scoped(.jsonrpc);

const RequestProto = fn (session: *Session, id: i64) anyerror!lsp.Response;
const NotifyProto = fn (session: *Session) anyerror!void;
const Self = @This();

pub const Error = error{
    InvalidRequest,
    MethodNotFound,
    InvalidParams,
    InternalError,
};

request_map: std.StringHashMap(RequestProto),
notify_map: std.StringHashMap(NotifyProto),

pub fn init(allocator: std.mem.Allocator) Self {
    return .{
        .request_map = std.StringHashMap(RequestProto).init(allocator),
        .notify_map = std.StringHashMap(NotifyProto).init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    self.request_map.deinit();
    self.notify_map.deinit();
}

pub fn registerRequest(
    self: *Self,
    method: []const u8,
    comptime ParamType: type,
    comptime callback: fn (session: *Session, id: i64, req: ParamType) anyerror!lsp.Response,
) void {
    if (ParamType == void) {
        const T = struct {
            pub fn request(session: *Session, id: i64) anyerror!lsp.Response {
                return callback(session, id, .{});
            }
        };
        self.request_map.put(method, T.request) catch @panic("put");
    } else {
        const T = struct {
            pub fn request(session: *Session, id: i64) anyerror!lsp.Response {
                if (session.getParam(ParamType)) |req| {
                    return try callback(session, id, req);
                } else |_| {
                    return Error.InvalidParams;
                }
            }
        };
        self.request_map.put(method, T.request) catch @panic("put");
    }
}

pub fn registerNotify(
    self: *Self,
    method: []const u8,
    comptime ParamType: type,
    comptime callback: fn (session: *Session, req: ParamType) anyerror!void,
) void {
    const T = struct {
        pub fn notify(session: *Session) anyerror!void {
            if (session.getParam(ParamType)) |req| {
                try callback(session, req);
            } else |_| {
                return Error.InvalidParams;
            }
        }
    };
    self.notify_map.put(method, T.notify) catch @panic("put");
}

pub fn dispatchRequest(self: Self, session: *Session, id: i64, method: []const u8) !lsp.Response {
    if (self.request_map.get(method)) |handler| {
        const start_time = std.time.milliTimestamp();
        if (handler(session, id)) |res| {
            const end_time = std.time.milliTimestamp();
            logger.info("id[{}] {s} => {}ms", .{ id, method, end_time - start_time });
            return res;
        } else |err| {
            logger.err("id[{}] {s} => {s}", .{ id, method, @errorName(err) });
            return Error.InternalError;
        }
    } else {
        // no method
        logger.err("id[{}] {s} => unknown request", .{ id, method });
        return Error.MethodNotFound;
    }
}

pub fn dispatchNotify(self: *Self, session: *Session, method: []const u8) !void {
    if (self.notify_map.get(method)) |handler| {
        const start_time = std.time.milliTimestamp();
        if (handler(session)) {
            const end_time = std.time.milliTimestamp();
            logger.info("{s} => {}ms", .{ method, end_time - start_time });
        } else |err| {
            logger.err("{s} => {s}", .{ method, @errorName(err) });
            return Error.InternalError;
        }
    } else {
        logger.err("{s} => unknown notify", .{method});
        return Error.MethodNotFound;
    }
}
