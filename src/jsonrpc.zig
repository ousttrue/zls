const std = @import("std");
const lsp = @import("lsp");
const ws = @import("workspace");
const language_server = @import("language_server");
const Dispatcher = @import("./Dispatcher.zig");
const requests = lsp.requests;
const Session = ws.Session;
const Workspace = ws.Workspace;
const Config = ws.Config;
const Stdio = ws.Stdio;

const logger = std.log.scoped(.jsonrpc);

fn getId(tree: std.json.ValueTree) ?i64 {
    if (tree.root.Object.get("id")) |child| {
        switch (child) {
            .Integer => |int| return int,
            else => {},
        }
    }
    return null;
}

pub fn getMethod(tree: std.json.ValueTree) ?[]const u8 {
    if (tree.root.Object.get("method")) |child| {
        switch (child) {
            .String => |str| return str,
            else => {},
        }
    }
    return null;
}

pub fn getParams(tree: std.json.ValueTree) ?std.json.Value {
    return tree.root.Object.get("params");
}

pub fn readloop(allocator: std.mem.Allocator, transport: *Stdio, dispatcher: *Dispatcher) void {
    // This JSON parser is passed to processJsonRpc and reset.
    var json_parser = std.json.Parser.init(allocator, false);
    defer json_parser.deinit();

    var arena = std.heap.ArenaAllocator.init(allocator);

    while (language_server.LanguageServer.keep_running) {
        if (transport.readNext()) |content| {
            // parse
            json_parser.reset();
            var tree = json_parser.parse(content) catch |err| {
                logger.err("{s}", .{@errorName(err)});
                transport.sendToJson(lsp.Response.createInvalidRequest(null));
                continue;
            };
            defer tree.deinit();
            // var session = Session.init(allocator, &arena, config, &document_store, transport, tree);
            // defer session.deinit();

            // request: id, method, ?params
            // reponse: id, ?result, ?error
            // notify: method, ?params
            if (getId(tree)) |id| {
                if (getMethod(tree)) |method| {
                    // request
                    if (dispatcher.dispatchRequest(&arena, id, method, getParams(tree))) |res| {
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
                if (getMethod(tree)) |method| {
                    // notify
                    dispatcher.dispatchNotify(&arena, method, getParams(tree)) catch |err| switch (err) {
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
            language_server.LanguageServer.keep_running = false;
            break;
        }
    }
}
