const std = @import("std");
const lsp = @import("lsp");
const requests = lsp.requests;
const Session = @import("./session.zig").Session;
const DocumentStore = @import("./DocumentStore.zig");
const Config = @import("./Config.zig");
const Completion = @import("./builtin_completions.zig").Completion;

const logger = std.log.scoped(.jsonrpc);

pub const RpcError = error{
    // Parse,
    InvalidRequest,
    MethodNotFound,
    InvalidParams,
    InternalError,
};

const RequestProto = fn (session: *Session, id: i64) anyerror!lsp.Response;
const NotifyProto = fn (session: *Session) anyerror!void;
var request_map: std.StringHashMap(RequestProto) = undefined;
var notify_map: std.StringHashMap(NotifyProto) = undefined;

pub fn init(allocator: std.mem.Allocator) void {
    request_map = std.StringHashMap(RequestProto).init(allocator);
    notify_map = std.StringHashMap(NotifyProto).init(allocator);
}

pub fn deinit() void {
    request_map.deinit();
    notify_map.deinit();
}

pub fn registerRequest(method: []const u8, comptime ParamType: type, comptime callback: fn (session: *Session, id: i64, req: ParamType) anyerror!lsp.Response) void {
    if (ParamType == void) {
        const T = struct {
            pub fn request(session: *Session, id: i64) anyerror!lsp.Response {
                return callback(session, id, .{});
            }
        };
        request_map.put(method, T.request) catch @panic("put");
    } else {
        const T = struct {
            pub fn request(session: *Session, id: i64) anyerror!lsp.Response {
                if (session.getParam(ParamType)) |req| {
                    return try callback(session, id, req);
                } else |_| {
                    return RpcError.InvalidParams;
                }
            }
        };
        request_map.put(method, T.request) catch @panic("put");
    }
}

pub fn registerNotify(method: []const u8, comptime ParamType: type, comptime callback: fn (session: *Session, req: ParamType) anyerror!void) void {
    const T = struct {
        pub fn notify(session: *Session) anyerror!void {
            if (session.getParam(ParamType)) |req| {
                try callback(session, req);
            } else |_| {
                return RpcError.InvalidParams;
            }
        }
    };
    notify_map.put(method, T.notify) catch @panic("put");
}

fn dispatchRequest(session: *Session, id: i64, method: []const u8) RpcError!lsp.Response {
    if (request_map.get(method)) |handler| {
        const start_time = std.time.milliTimestamp();
        if (handler(session, id)) |res| {
            const end_time = std.time.milliTimestamp();
            logger.info("id[{}] {s} => {}ms", .{ id, method, end_time - start_time });
            return res;
        } else |err| {
            logger.err("id[{}] {s} => {s}", .{ id, method, @errorName(err) });
            return RpcError.InternalError;
        }
    } else {
        // no method
        logger.err("id[{}] {s} => unknown request", .{ id, method });
        return RpcError.MethodNotFound;
    }
}

fn dispatchNotify(session: *Session, method: []const u8) RpcError!void {
    if (notify_map.get(method)) |handler| {
        const start_time = std.time.milliTimestamp();
        if (handler(session)) {
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

pub fn dispatch(session: *Session) void {
    // request: id, method, ?params
    // reponse: id, ?result, ?error
    // notify: method, ?params
    if (session.getId()) |id| {
        if (session.getMethod()) |method| {
            // request
            if (dispatchRequest(session, id, method)) |res| {
                session.send(res);
            } else |err| switch (err) {
                RpcError.InvalidRequest => session.send(lsp.Response.createInvalidRequest(id)),
                RpcError.MethodNotFound => session.send(lsp.Response.createMethodNotFound(id)),
                RpcError.InvalidParams => session.send(lsp.Response.createInvalidParams(id)),
                RpcError.InternalError => session.send(lsp.Response.createInternalError(id)),
            }
        } else {
            // response
            @panic("jsonrpc response is not implemented(not send request)");
        }
    } else {
        if (session.getMethod()) |method| {
            // notify
            dispatchNotify(session, method) catch |err| switch (err) {
                RpcError.InvalidRequest => session.send(lsp.Response.createInvalidRequest(null)),
                RpcError.MethodNotFound => session.send(lsp.Response.createMethodNotFound(null)),
                RpcError.InvalidParams => session.send(lsp.Response.createInvalidParams(null)),
                RpcError.InternalError => session.send(lsp.Response.createInternalError(null)),
            };
        } else {
            // invalid
            session.send(lsp.Response.createParseError());
        }
    }
}

pub var keep_running = false;
pub fn shutdownHandler(session: *Session, id: i64, _: void) !lsp.Response {
    _ = session;
    keep_running = false;
    return lsp.Response.createNull(id);
}

pub fn readloop(allocator: std.mem.Allocator, r: std.fs.File, stdout: anytype, config: *Config) void {
    keep_running = true;
    const reader = r.reader();

    // This JSON parser is passed to processJsonRpc and reset.
    var json_parser = std.json.Parser.init(allocator, false);
    defer json_parser.deinit();

    var arena = std.heap.ArenaAllocator.init(allocator);

    var document_store: DocumentStore = undefined;
    document_store.init(
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
    ) catch @panic("DocumentStore.init");
    defer document_store.deinit();

    var completion = Completion.init(allocator);
    defer completion.deinit();

    while (keep_running) {
        var session = Session.init(allocator, config, &document_store, &completion, stdout, &arena, reader, &json_parser);
        defer session.deinit();

        dispatch(&session);
    }
}
