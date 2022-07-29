const std = @import("std");
const lsp = @import("lsp");
const language_server = @import("language_server");
const logger = std.log.scoped(.Didpatcher);

const RequestProto = fn (self: *language_server.LanguageServer, arena: *std.heap.ArenaAllocator, id: i64, params: ?std.json.Value) anyerror!lsp.Response;
const RequestFunctor = struct {
    ls: *language_server.LanguageServer,
    proto: RequestProto,
    pub fn call(self: RequestFunctor, arena: *std.heap.ArenaAllocator, id: i64, params: ?std.json.Value) anyerror!lsp.Response {
        return self.proto(self.ls, arena, id, params);
    }
};

const NotifyProto = fn (self: *language_server.LanguageServer, arena: *std.heap.ArenaAllocator, prams: ?std.json.Value) anyerror!void;
const NotifyFunctor = struct {
    ls: *language_server.LanguageServer,
    proto: NotifyProto,
    pub fn call(self: NotifyFunctor, arena: *std.heap.ArenaAllocator, params: ?std.json.Value) anyerror!void {
        return self.proto(self.ls, arena, params);
    }
};

const Self = @This();

pub const Error = error{
    InvalidRequest,
    MethodNotFound,
    InvalidParams,
    InternalError,
};

request_map: std.StringHashMap(RequestFunctor),
notify_map: std.StringHashMap(NotifyFunctor),

pub fn init(allocator: std.mem.Allocator) Self {
    return .{
        .request_map = std.StringHashMap(RequestFunctor).init(allocator),
        .notify_map = std.StringHashMap(NotifyFunctor).init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    self.request_map.deinit();
    self.notify_map.deinit();
}

pub fn registerRequest(
    self: *Self,
    ls: *language_server.LanguageServer,
    comptime method: []const u8,
) void {
    const field = @field(language_server.LanguageServer, method);
    const S = struct {
        fn call(ptr: *language_server.LanguageServer, arena: *std.heap.ArenaAllocator, id: i64, params: ?std.json.Value) anyerror!lsp.Response {
            return @call(.{}, field, .{ ptr, arena, id, params });
        }
    };
    self.request_map.put(method, RequestFunctor{
        .ls = ls,
        .proto = S.call,
    }) catch @panic("put");
}

pub fn registerNotify(
    self: *Self,
    ls: *language_server.LanguageServer,
    comptime method: []const u8,
) void {
    const field = @field(language_server.LanguageServer, method);
    const S = struct {
        fn call(ptr: *language_server.LanguageServer, arena: *std.heap.ArenaAllocator, params: ?std.json.Value) anyerror!void {
            return @call(.{}, field, .{ ptr, arena, params });
        }
    };
    self.notify_map.put(method, NotifyFunctor{
        .ls = ls,
        .proto = S.call,
    }) catch @panic("put");
}

pub fn dispatchRequest(self: Self, arena: *std.heap.ArenaAllocator, id: i64, method: []const u8, params: ?std.json.Value) !lsp.Response {
    if (self.request_map.get(method)) |functor| {
        const start_time = std.time.milliTimestamp();
        if (functor.call(arena, id, params)) |res| {
            const end_time = std.time.milliTimestamp();
            logger.info("({}){s} => {}ms", .{ id, method, end_time - start_time });
            return res;
        } else |err| {
            logger.err("({}){s} => {s}", .{ id, method, @errorName(err) });
            return Error.InternalError;
        }
    } else {
        // no method
        logger.err("({}){s} => unknown request", .{ id, method });
        return Error.MethodNotFound;
    }
}

pub fn dispatchNotify(self: *Self, arena: *std.heap.ArenaAllocator, method: []const u8, params: ?std.json.Value) !void {
    if (self.notify_map.get(method)) |functor| {
        const start_time = std.time.milliTimestamp();
        if (functor.call(arena, params)) {
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
