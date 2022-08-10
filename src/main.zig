const std = @import("std");
const zig_builtin = @import("builtin");
const build_options = @import("build_options");
const known_folders = @import("known-folders");
const astutil = @import("astutil");
const FixedPath = astutil.FixedPath;
const ls = @import("language_server");
const LanguageServer = ls.LanguageServer;
const Config = ls.Config;
const ZigEnv = ls.ZigEnv;
const lsp = @import("lsp");
const requests = lsp.requests;
const setup = @import("./setup.zig");
const jsonrpc = @import("./jsonrpc.zig");
const Dispatcher = @import("./Dispatcher.zig");
const Stdio = @import("./Stdio.zig");

const logger = std.log.scoped(.main);

pub const data = switch (build_options.data_version) {
    .master => @import("data/master.zig"),
    .@"0.7.0" => @import("data/0.7.0.zig"),
    .@"0.7.1" => @import("data/0.7.1.zig"),
    .@"0.8.0" => @import("data/0.8.0.zig"),
    .@"0.8.1" => @import("data/0.8.1.zig"),
    .@"0.9.0" => @import("data/0.9.0.zig"),
};

// Always set this to debug to make std.log call into our handler, then control the runtime
// value in the definition below.
pub const log_level = .debug;
var transport: Stdio = undefined;

var actual_log_level: std.log.Level = switch (zig_builtin.mode) {
    .Debug => .debug,
    else => @intToEnum(std.log.Level, @enumToInt(build_options.log_level)), //temporary fix to build failing on release-safe due to a Zig bug
};

pub fn log(
    comptime message_level: std.log.Level,
    comptime scope: @Type(.EnumLiteral),
    comptime format: []const u8,
    args: anytype,
) void {
    if (@enumToInt(message_level) > @enumToInt(actual_log_level)) {
        return;
    }
    // After shutdown, pipe output to stderr
    if (!LanguageServer.keep_running) {
        std.debug.print("[{s}-{s}] " ++ format ++ "\n", .{ @tagName(message_level), @tagName(scope) } ++ args);
        return;
    }

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    var message = std.fmt.allocPrint(arena.allocator(), "{s}> " ++ format, .{@tagName(scope)} ++ args) catch {
        std.debug.print("Failed to allocPrint message.\n", .{});
        return;
    };

    const notification = lsp.Notification{
        .method = "window/logMessage",
        .params = lsp.NotificationParams{
            .LogMessage = .{
                .type = switch (message_level) {
                    .debug => .Log,
                    .info => .Info,
                    .warn => .Warning,
                    .err => .Error,
                },
                .message = message,
            },
        },
    };

    transport.sendToJson(notification);
}

fn initialize(allocator: std.mem.Allocator, config: *Config) !?FixedPath {
    transport = Stdio.init(allocator);
    logger.info("######## [ZLS MODIFIED] ########", .{});

    // Check arguments.
    var args_it = try std.process.ArgIterator.initWithAllocator(allocator);
    defer args_it.deinit();
    if (!args_it.skip()) @panic("Could not find self argument");

    var config_dir: ?FixedPath = null;
    var next_arg_config_path = false;
    while (args_it.next()) |arg| {
        if (next_arg_config_path) {
            if (Config.load(allocator, arg)) |c| {
                logger.info("arg: {s}", .{arg});
                config.* = c;
                if (std.fs.path.dirname(arg)) |dir| {
                    config_dir = FixedPath.fromFullpath(dir);
                } else {
                    unreachable;
                }
            }
            next_arg_config_path = false;
            continue;
        }

        if (std.mem.eql(u8, arg, "--debug-log")) {
            // actual_log_level = .debug;
            std.debug.print("Enabled debug logging\n", .{});
        } else if (std.mem.eql(u8, arg, "--config-path")) {
            next_arg_config_path = true;
            continue;
        } else if (std.mem.eql(u8, arg, "config") or std.mem.eql(u8, arg, "configure")) {
            try setup.wizard(allocator);
            return null;
        } else {
            std.debug.print("Unrecognized argument {s}\n", .{arg});
            std.os.exit(1);
        }
    }

    if (next_arg_config_path) {
        std.debug.print("Expected configuration file path after --config-path argument\n", .{});
        return null;
    }

    if (config_dir == null) {
        if (known_folders.getPath(allocator, .local_configuration) catch unreachable) |dir| {
            defer allocator.free(dir);
            if (Config.loadInFolder(allocator, dir)) |c| {
                logger.info("local_configuration: {s}", .{dir});
                config.* = c;
                config_dir = FixedPath.fromFullpath(dir);
            }
        }
    }

    if (config_dir == null) {
        if (known_folders.getPath(allocator, .global_configuration) catch unreachable) |dir| {
            defer allocator.free(dir);
            if (Config.loadInFolder(allocator, dir)) |c| {
                logger.info("global_configuration: {s}", .{dir});
                config.* = c;
                config_dir = FixedPath.fromFullpath(dir);
            }
        }
    }

    return config_dir;
}

pub fn main() anyerror!void {
    var config = Config{};
    const config_dir = (try initialize(std.heap.page_allocator, &config)) orelse
        {
        return;
    };

    {
        var gpa = std.heap.GeneralPurposeAllocator(.{}){};
        const allocator = gpa.allocator();
        defer std.debug.assert(!gpa.deinit());

        var zigenv = try ZigEnv.init(
            allocator,
            config_dir,
            config.zig_exe_path,
            config.zig_lib_path,
            config.builtin_path,
            config.build_runner_path,
            config.build_runner_cache_path,
            // TODO make this configurable
            // We can't figure it out ourselves since we don't know what arguments
            // the user will use to run "zig build"
            "zig-cache",
            // Since we don't compile anything and no packages should put their
            // files there this path can be ignored
            "ZLS_DONT_CARE",
        );

        ls.init(allocator, &data.builtins, &config);
        defer ls.deinit();

        var dispatcher = Dispatcher.init(allocator);
        defer dispatcher.deinit();

        var language_server = LanguageServer.init(allocator, &config, zigenv);
        defer language_server.deinit();

        // life cycle
        dispatcher.registerRequest(&language_server, "initialize");
        dispatcher.registerNotify(&language_server, "initialized");
        dispatcher.registerRequest(&language_server, "shutdown");
        // document sync
        dispatcher.registerNotify(&language_server, "textDocument/didOpen");
        dispatcher.registerNotify(&language_server, "textDocument/didChange");
        dispatcher.registerNotify(&language_server, "textDocument/didSave");
        dispatcher.registerNotify(&language_server, "textDocument/didClose");
        // document request
        dispatcher.registerRequest(&language_server, "textDocument/semanticTokens/full");
        dispatcher.registerRequest(&language_server, "textDocument/documentSymbol");
        // dispatcher.registerRequest(&language_server, "textDocument/codeLens");
        // dispatcher.registerRequest(&language_server, "codeLens/resolve");
        // document position request
        dispatcher.registerRequest(&language_server, "textDocument/hover");
        dispatcher.registerRequest(&language_server, "textDocument/formatting");
        dispatcher.registerRequest(&language_server, "textDocument/definition");
        dispatcher.registerNotify(&language_server, "$/cancelRequest");
        dispatcher.registerRequest(&language_server, "textDocument/completion");
        // dispatcher.registerRequest(&language_server, "textDocument/rename");
        // dispatcher.registerRequest(&language_server, "textDocument/references");
        dispatcher.registerRequest(&language_server, "textDocument/signatureHelp");

        // start
        jsonrpc.readloop(allocator, &transport, &dispatcher, &language_server.notification_queue);
    }
}
