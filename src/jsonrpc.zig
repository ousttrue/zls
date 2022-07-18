const std = @import("std");
const lsp = @import("lsp");
const requests = lsp.requests;
const Session = @import("./Session.zig");
const DocumentStore = @import("./DocumentStore.zig");
const Config = @import("./Config.zig");
const Completion = @import("./builtin_completions.zig").Completion;
const Dispatcher = @import("./Dispatcher.zig");

pub var keep_running = false;
pub fn shutdownHandler(session: *Session, id: i64, _: void) !lsp.Response {
    _ = session;
    keep_running = false;
    return lsp.Response.createNull(id);
}

pub fn readloop(allocator: std.mem.Allocator, r: std.fs.File, stdout: anytype, config: *Config, dispatcher: *Dispatcher) void {
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

        // request: id, method, ?params
        // reponse: id, ?result, ?error
        // notify: method, ?params
        if (session.getId()) |id| {
            if (session.getMethod()) |method| {
                // request
                if (dispatcher.dispatchRequest(&session, id, method)) |res| {
                    session.send(res);
                } else |err| switch (err) {
                    Dispatcher.Error.InvalidRequest => session.send(lsp.Response.createInvalidRequest(id)),
                    Dispatcher.Error.MethodNotFound => session.send(lsp.Response.createMethodNotFound(id)),
                    Dispatcher.Error.InvalidParams => session.send(lsp.Response.createInvalidParams(id)),
                    Dispatcher.Error.InternalError => session.send(lsp.Response.createInternalError(id)),
                }
            } else {
                // response
                @panic("jsonrpc response is not implemented(not send request)");
            }
        } else {
            if (session.getMethod()) |method| {
                // notify
                dispatcher.dispatchNotify(&session, method) catch |err| switch (err) {
                    Dispatcher.Error.InvalidRequest => session.send(lsp.Response.createInvalidRequest(null)),
                    Dispatcher.Error.MethodNotFound => session.send(lsp.Response.createMethodNotFound(null)),
                    Dispatcher.Error.InvalidParams => session.send(lsp.Response.createInvalidParams(null)),
                    Dispatcher.Error.InternalError => session.send(lsp.Response.createInternalError(null)),
                };
            } else {
                // invalid
                session.send(lsp.Response.createParseError());
            }
        }
    }
}
