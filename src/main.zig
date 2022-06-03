const std = @import("std");
const zig_builtin = @import("builtin");
const build_options = @import("build_options");
// const types = @import("./types.zig");
const server = @import("server.zig");
const Config = @import("./Config.zig");
const setup = @import("./setup.zig");
const known_folders = @import("known-folders");
const jsonrpc = @import("./jsonrpc.zig");
const lsp = @import("lsp");

const logger = std.log.scoped(.main);

// Always set this to debug to make std.log call into our handler, then control the runtime
// value in the definition below.
pub const log_level = .debug;

var actual_log_level: std.log.Level = switch (zig_builtin.mode) {
    .Debug => .debug,
    else => @intToEnum(std.log.Level, @enumToInt(build_options.log_level)), //temporary fix to build failing on release-safe due to a Zig bug
};

pub fn log(comptime message_level: std.log.Level, comptime scope: @Type(.EnumLiteral), comptime format: []const u8, args: anytype) void {
    if (@enumToInt(message_level) > @enumToInt(actual_log_level)) {
        return;
    }
    // After shutdown, pipe output to stderr
    if (!jsonrpc.keep_running) {
        std.debug.print("[{s}-{s}] " ++ format ++ "\n", .{ @tagName(message_level), @tagName(scope) } ++ args);
        return;
    }

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    var message = std.fmt.allocPrint(arena.allocator(), "[{s}-{s}] " ++ format, .{ @tagName(message_level), @tagName(scope) } ++ args) catch {
        std.debug.print("Failed to allocPrint message.\n", .{});
        return;
    };

    jsonrpc.send(&arena, lsp.Notification{
        .method = "window/logMessage",
        .params = lsp.Notification.Params{
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
    });
}

fn loadConfigFile(allocator: std.mem.Allocator, file_path: []const u8) ?Config {
    var file = std.fs.cwd().openFile(file_path, .{}) catch |err| {
        if (err != error.FileNotFound)
            logger.warn("Error while reading configuration file: {}", .{err});
        return null;
    };

    defer file.close();

    const file_buf = file.readToEndAlloc(allocator, 0x1000000) catch return null;
    defer allocator.free(file_buf);
    @setEvalBranchQuota(3000);
    // TODO: Better errors? Doesn't seem like std.json can provide us positions or context.
    var config = std.json.parse(Config, &std.json.TokenStream.init(file_buf), std.json.ParseOptions{ .allocator = allocator }) catch |err| {
        logger.warn("Error while parsing configuration file: {}", .{err});
        return null;
    };

    if (config.zig_lib_path) |zig_lib_path| {
        if (!std.fs.path.isAbsolute(zig_lib_path)) {
            logger.warn("zig library path is not absolute, defaulting to null.", .{});
            allocator.free(zig_lib_path);
            config.zig_lib_path = null;
        }
    }

    return config;
}

fn loadConfigInFolder(allocator: std.mem.Allocator, folder_path: []const u8) ?Config {
    const full_path = std.fs.path.resolve(allocator, &.{ folder_path, "zls.json" }) catch return null;
    defer allocator.free(full_path);
    return loadConfigFile(allocator, full_path);
}

pub fn main() anyerror!void {
    // allocator = &gpa_state.allocator;
    // @TODO Using the GPA here, realloc calls hang currently for some reason
    const allocator = std.heap.page_allocator;

    // Check arguments.
    var args_it = try std.process.ArgIterator.initWithAllocator(allocator);
    defer args_it.deinit();
    if (!args_it.skip()) @panic("Could not find self argument");

    var config_path: ?[]const u8 = null;
    var next_arg_config_path = false;
    while (args_it.next()) |arg| {
        if (next_arg_config_path) {
            config_path = try allocator.dupe(u8, arg);
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
            return;
        } else {
            std.debug.print("Unrecognized argument {s}\n", .{arg});
            std.os.exit(1);
        }
    }

    if (next_arg_config_path) {
        std.debug.print("Expected configuration file path after --config-path argument\n", .{});
        return;
    }

    // Read the configuration, if any.
    const config_parse_options = std.json.ParseOptions{ .allocator = allocator };
    defer std.json.parseFree(Config, server.config, config_parse_options);

    config_read: {
        if (config_path) |path| {
            defer allocator.free(path);
            if (loadConfigFile(allocator, path)) |conf| {
                server.config = conf;
                break :config_read;
            }
            std.debug.print("Could not open configuration file '{s}'\n", .{path});
            std.debug.print("Falling back to a lookup in the local and global configuration folders\n", .{});
        }
        if (try known_folders.getPath(allocator, .local_configuration)) |path| {
            config_path = path;
            if (loadConfigInFolder(allocator, path)) |conf| {
                server.config = conf;
                break :config_read;
            }
        }
        if (try known_folders.getPath(allocator, .global_configuration)) |path| {
            config_path = path;
            if (loadConfigInFolder(allocator, path)) |conf| {
                server.config = conf;
                break :config_read;
            }
        }
        logger.info("No config file zls.json found.", .{});
        config_path = null;
    }

    // Find the zig executable in PATH
    find_zig: {
        if (server.config.zig_exe_path) |exe_path| {
            if (std.fs.path.isAbsolute(exe_path)) not_valid: {
                std.fs.cwd().access(exe_path, .{}) catch break :not_valid;
                break :find_zig;
            }
            logger.debug("zig path `{s}` is not absolute, will look in path", .{exe_path});
            allocator.free(exe_path);
        }
        server.config.zig_exe_path = try setup.findZig(allocator);
    }

    if (server.config.zig_exe_path) |exe_path| {
        logger.info("Using zig executable {s}", .{exe_path});

        if (server.config.zig_lib_path == null) find_lib_path: {
            // Use `zig env` to find the lib path
            const zig_env_result = try std.ChildProcess.exec(.{
                .allocator = allocator,
                .argv = &[_][]const u8{ exe_path, "env" },
            });

            defer {
                allocator.free(zig_env_result.stdout);
                allocator.free(zig_env_result.stderr);
            }

            switch (zig_env_result.term) {
                .Exited => |exit_code| {
                    if (exit_code == 0) {
                        const Env = struct {
                            zig_exe: []const u8,
                            lib_dir: ?[]const u8,
                            std_dir: []const u8,
                            global_cache_dir: []const u8,
                            version: []const u8,
                        };

                        var json_env = std.json.parse(
                            Env,
                            &std.json.TokenStream.init(zig_env_result.stdout),
                            .{ .allocator = allocator },
                        ) catch {
                            logger.err("Failed to parse zig env JSON result", .{});
                            break :find_lib_path;
                        };
                        defer std.json.parseFree(Env, json_env, .{ .allocator = allocator });
                        // We know this is allocated with `allocator`, we just steal it!
                        server.config.zig_lib_path = json_env.lib_dir.?;
                        json_env.lib_dir = null;
                        logger.info("Using zig lib path '{s}'", .{server.config.zig_lib_path});
                    }
                },
                else => logger.err("zig env invocation failed", .{}),
            }
        }
    } else {
        logger.warn("Zig executable path not specified in zls.json and could not be found in PATH", .{});
    }

    if (server.config.zig_lib_path == null) {
        logger.warn("Zig standard library path not specified in zls.json and could not be resolved from the zig executable", .{});
    }

    if (server.config.builtin_path == null and server.config.zig_exe_path != null and config_path != null) blk: {
        const result = try std.ChildProcess.exec(.{
            .allocator = allocator,
            .argv = &.{
                server.config.zig_exe_path.?,
                "build-exe",
                "--show-builtin",
            },
            .max_output_bytes = 1024 * 1024 * 50,
        });
        defer allocator.free(result.stdout);
        defer allocator.free(result.stderr);

        var d = try std.fs.cwd().openDir(config_path.?, .{});
        defer d.close();

        const f = d.createFile("builtin.zig", .{}) catch |err| switch (err) {
            error.AccessDenied => break :blk,
            else => |e| return e,
        };
        defer f.close();

        try f.writer().writeAll(result.stdout);

        server.config.builtin_path = try std.fs.path.join(allocator, &.{ config_path.?, "builtin.zig" });
    }

    const build_runner_path = if (server.config.build_runner_path) |p|
        try allocator.dupe(u8, p)
    else blk: {
        var exe_dir_bytes: [std.fs.MAX_PATH_BYTES]u8 = undefined;
        const exe_dir_path = try std.fs.selfExeDirPath(&exe_dir_bytes);
        break :blk try std.fs.path.resolve(allocator, &[_][]const u8{ exe_dir_path, "build_runner.zig" });
    };

    const build_runner_cache_path = if (server.config.build_runner_cache_path) |p|
        try allocator.dupe(u8, p)
    else blk: {
        const cache_dir_path = (try known_folders.getPath(allocator, .cache)) orelse {
            logger.warn("Known-folders could not fetch the cache path", .{});
            return;
        };
        defer allocator.free(cache_dir_path);
        break :blk try std.fs.path.resolve(allocator, &[_][]const u8{ cache_dir_path, "zls" });
    };

    try server.init(allocator, build_runner_path, build_runner_cache_path);
    defer server.deinit();

    jsonrpc.request_map = std.StringHashMap(jsonrpc.RequestProto).init(allocator);
    defer jsonrpc.request_map.deinit();
    try jsonrpc.request_map.put("initialize", server.initializeHandler);
    try jsonrpc.request_map.put("shutdown", jsonrpc.shutdownHandler);
    try jsonrpc.request_map.put("textDocument/semanticTokens/full", server.semanticTokensFullHandler);
    try jsonrpc.request_map.put("textDocument/completion", server.completionHandler);
    try jsonrpc.request_map.put("textDocument/signatureHelp", server.signatureHelpHandler);
    try jsonrpc.request_map.put("textDocument/definition", server.gotoDefinitionHandler);
    try jsonrpc.request_map.put("textDocument/typeDefinition", server.gotoDefinitionHandler);
    try jsonrpc.request_map.put("textDocument/implementation", server.gotoDefinitionHandler);
    try jsonrpc.request_map.put("textDocument/declaration", server.gotoDeclarationHandler);
    try jsonrpc.request_map.put("textDocument/hover", server.hoverHandler);
    try jsonrpc.request_map.put("textDocument/documentSymbol", server.documentSymbolsHandler);
    try jsonrpc.request_map.put("textDocument/formatting", server.formattingHandler);
    try jsonrpc.request_map.put("textDocument/rename", server.renameHandler);
    try jsonrpc.request_map.put("textDocument/references", server.referencesHandler);

    jsonrpc.notify_map = std.StringHashMap(jsonrpc.NotifyProto).init(allocator);
    defer jsonrpc.notify_map.deinit();
    try jsonrpc.notify_map.put("textDocument/didOpen", server.openDocumentHandler);
    try jsonrpc.notify_map.put("textDocument/didSave", server.saveDocumentHandler);
    try jsonrpc.notify_map.put("textDocument/didChange", server.changeDocumentHandler);
    try jsonrpc.notify_map.put("textDocument/didClose", server.closeDocumentHandler);

    jsonrpc.readloop(allocator, std.io.getStdIn(), std.io.getStdOut(), &server.notifyQueue);
}
