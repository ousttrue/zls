const std = @import("std");
const lsp = @import("lsp");
const document = @import("document");
const Dispatcher = @import("./Dispatcher.zig");
const requests = lsp.requests;
const Session = document.Session;
const Workspace = document.Workspace;
const Config = document.Config;
const Stdio = document.Stdio;

const logger = std.log.scoped(.jsonrpc);

pub var keep_running = false;
pub fn shutdownHandler(session: *Session, id: i64, _: void) !lsp.Response {
    _ = session;
    keep_running = false;
    return lsp.Response.createNull(id);
}

pub fn readloop(allocator: std.mem.Allocator, transport: *Stdio, config: *Config, dispatcher: *Dispatcher) void {
    keep_running = true;

    // This JSON parser is passed to processJsonRpc and reset.
    var json_parser = std.json.Parser.init(allocator, false);
    defer json_parser.deinit();

    var arena = std.heap.ArenaAllocator.init(allocator);

    var document_store: Workspace = undefined;
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
    ) catch @panic("Workspace.init");
    defer document_store.deinit();

    while (keep_running) {
        if (transport.readNext()) |content| {
            // parse
            json_parser.reset();
            if (json_parser.parse(content)) |tree| {
                var session = Session.init(allocator, &arena, config, &document_store, transport, tree);
                defer session.deinit();

                // request: id, method, ?params
                // reponse: id, ?result, ?error
                // notify: method, ?params
                if (session.getId()) |id| {
                    if (session.getMethod()) |method| {
                        // request
                        if (dispatcher.dispatchRequest(&session, id, method)) |res| {
                            transport.sendToJson(res);
                        } else |err| switch (err) {
                            Dispatcher.Error.InvalidRequest => transport.sendToJson(lsp.Response.createInvalidRequest(id)),
                            Dispatcher.Error.MethodNotFound => transport.sendToJson(lsp.Response.createMethodNotFound(id)),
                            Dispatcher.Error.InvalidParams => transport.sendToJson(lsp.Response.createInvalidParams(id)),
                            Dispatcher.Error.InternalError => transport.sendToJson(lsp.Response.createInternalError(id)),
                        }
                    } else {
                        // response
                        @panic("jsonrpc response is not implemented(not send request)");
                    }
                } else {
                    if (session.getMethod()) |method| {
                        // notify
                        dispatcher.dispatchNotify(&session, method) catch |err| switch (err) {
                            Dispatcher.Error.InvalidRequest => transport.sendToJson(lsp.Response.createInvalidRequest(null)),
                            Dispatcher.Error.MethodNotFound => transport.sendToJson(lsp.Response.createMethodNotFound(null)),
                            Dispatcher.Error.InvalidParams => transport.sendToJson(lsp.Response.createInvalidParams(null)),
                            Dispatcher.Error.InternalError => transport.sendToJson(lsp.Response.createInternalError(null)),
                        };
                    } else {
                        // invalid
                        transport.sendToJson(lsp.Response.createParseError());
                    }
                }
            } else |err| {
                logger.err("{s}", .{@errorName(err)});
            }
        } else |err| {
            logger.err("{s}", .{@errorName(err)});
            keep_running = false;
            break;
        }
    }
}
