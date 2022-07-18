const std = @import("std");
const zig_builtin = @import("builtin");
const build_options = @import("build_options");
const known_folders = @import("known-folders");
const document = @import("document");
const lsp = @import("lsp");
const setup = @import("./setup.zig");
const jsonrpc = @import("./jsonrpc.zig");
const Dispatcher = @import("./Dispatcher.zig");
const requests = lsp.requests;
const Config = document.Config;
const Stdio = document.Stdio;
const server = document.server;

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
        logger.warn("Error while parsing configuration file: {s} {}", .{ file_path, err });
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
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer std.debug.assert(!gpa.deinit());

    transport = Stdio.init(allocator);

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
    // const config_parse_options = std.json.ParseOptions{ .allocator = allocator };
    // defer std.json.parseFree(Config, config, config_parse_options);

    var config = Config{};
    config_read: {
        if (config_path) |path| {
            defer allocator.free(path);
            if (loadConfigFile(allocator, path)) |conf| {
                config = conf;
                break :config_read;
            }
            std.debug.print("Could not open configuration file '{s}'\n", .{path});
            std.debug.print("Falling back to a lookup in the local and global configuration folders\n", .{});
        }
        if (try known_folders.getPath(allocator, .local_configuration)) |path| {
            config_path = path;
            if (loadConfigInFolder(allocator, path)) |conf| {
                config = conf;
                break :config_read;
            }
        }
        if (try known_folders.getPath(allocator, .global_configuration)) |path| {
            config_path = path;
            if (loadConfigInFolder(allocator, path)) |conf| {
                config = conf;
                break :config_read;
            }
        }
        logger.info("No config file zls.json found.", .{});
        config_path = null;
    }

    // Find the zig executable in PATH
    find_zig: {
        if (config.zig_exe_path) |exe_path| {
            if (std.fs.path.isAbsolute(exe_path)) not_valid: {
                std.fs.cwd().access(exe_path, .{}) catch break :not_valid;
                break :find_zig;
            }
            logger.debug("zig path `{s}` is not absolute, will look in path", .{exe_path});
            allocator.free(exe_path);
        }
        config.zig_exe_path = try setup.findZig(allocator);
    }

    if (config.zig_exe_path) |exe_path| {
        logger.info("Using zig executable {s}", .{exe_path});

        if (config.zig_lib_path == null) find_lib_path: {
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
                        config.zig_lib_path = json_env.lib_dir.?;
                        json_env.lib_dir = null;
                        logger.info("Using zig lib path '{s}'", .{config.zig_lib_path});
                    }
                },
                else => logger.err("zig env invocation failed", .{}),
            }
        }
    } else {
        logger.warn("Zig executable path not specified in zls.json and could not be found in PATH", .{});
    }

    if (config.zig_lib_path == null) {
        logger.warn("Zig standard library path not specified in zls.json and could not be resolved from the zig executable", .{});
    }

    if (config.builtin_path == null and config.zig_exe_path != null and config_path != null) blk: {
        const result = try std.ChildProcess.exec(.{
            .allocator = allocator,
            .argv = &.{
                config.zig_exe_path.?,
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

        config.builtin_path = try std.fs.path.join(allocator, &.{ config_path.?, "builtin.zig" });
    }

    if (config.build_runner_path == null) {
        var exe_dir_bytes: [std.fs.MAX_PATH_BYTES]u8 = undefined;
        const exe_dir_path = try std.fs.selfExeDirPath(&exe_dir_bytes);
        config.build_runner_path = try std.fs.path.resolve(allocator, &[_][]const u8{ exe_dir_path, "build_runner.zig" });
    }

    if (config.build_runner_cache_path == null) {
        const cache_dir_path = (try known_folders.getPath(allocator, .cache)) orelse {
            logger.warn("Known-folders could not fetch the cache path", .{});
            return;
        };
        defer allocator.free(cache_dir_path);
        config.build_runner_cache_path = try std.fs.path.resolve(allocator, &[_][]const u8{ cache_dir_path, "zls" });
    }

    document.init(allocator, &data.builtins, &config);
    defer document.deinit();

    var dispatcher = Dispatcher.init(allocator);
    defer dispatcher.deinit();
    dispatcher.registerRequest("initialize", requests.Initialize, server.initializeHandler);
    dispatcher.registerRequest("shutdown", void, jsonrpc.shutdownHandler);
    dispatcher.registerRequest("textDocument/semanticTokens/full", requests.SemanticTokensFull, server.semanticTokensFullHandler);
    dispatcher.registerRequest("textDocument/completion", requests.Completion, server.completionHandler);
    dispatcher.registerRequest("textDocument/signatureHelp", requests.SignatureHelp, server.signatureHelpHandler);
    dispatcher.registerRequest("textDocument/definition", requests.GotoDefinition, server.gotoDefinitionHandler);
    dispatcher.registerRequest("textDocument/typeDefinition", requests.GotoDefinition, server.gotoDefinitionHandler);
    dispatcher.registerRequest("textDocument/implementation", requests.GotoDefinition, server.gotoDefinitionHandler);
    dispatcher.registerRequest("textDocument/declaration", requests.GotoDeclaration, server.gotoDeclarationHandler);
    dispatcher.registerRequest("textDocument/hover", requests.Hover, server.hoverHandler);
    dispatcher.registerRequest("textDocument/documentSymbol", requests.DocumentSymbols, server.documentSymbolsHandler);
    dispatcher.registerRequest("textDocument/formatting", requests.Formatting, server.formattingHandler);
    dispatcher.registerRequest("textDocument/rename", requests.Rename, server.renameHandler);
    dispatcher.registerRequest("textDocument/references", requests.References, server.referencesHandler);
    dispatcher.registerNotify("textDocument/didOpen", requests.OpenDocument, server.openDocumentHandler);
    dispatcher.registerNotify("textDocument/didSave", requests.SaveDocument, server.saveDocumentHandler);
    dispatcher.registerNotify("textDocument/didChange", requests.ChangeDocument, server.changeDocumentHandler);
    dispatcher.registerNotify("textDocument/didClose", requests.CloseDocument, server.closeDocumentHandler);

    jsonrpc.readloop(allocator, &transport, &config, &dispatcher);
}
