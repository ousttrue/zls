const std = @import("std");
const builtin = @import("builtin");
const known_folders = @import("known-folders");
const URI = @import("./uri.zig");
const logger = std.log.scoped(.Config);

const Self = @This();

allocator: std.mem.Allocator,
exe_path: []const u8,
std_uri: []const u8,
builtin_path: []const u8,
// config.build_runner_path orelse @panic("no build_runner_path"),
// config.build_runner_cache_path orelse @panic("build_runner_cache_path"),
build_runner_path: []const u8,
build_runner_cache_path: []const u8,
cache_root: []const u8,
global_cache_root: []const u8,

fn stdUriFromLibPath(allocator: std.mem.Allocator, zpath: []const u8) ![]const u8 {
    const std_path = try std.fs.path.resolve(allocator, &[_][]const u8{
        zpath, "./std/std.zig",
    });
    defer allocator.free(std_path);
    // Get the std_path as a URI, so we can just append to it!
    return try URI.fromPath(allocator, std_path);
}

pub fn findZig(allocator: std.mem.Allocator) !?[]const u8 {
    const env_path = std.process.getEnvVarOwned(allocator, "PATH") catch |err| switch (err) {
        error.EnvironmentVariableNotFound => {
            return null;
        },
        else => return err,
    };
    defer allocator.free(env_path);

    const exe_extension = builtin.target.exeFileExt();
    const zig_exe = try std.fmt.allocPrint(allocator, "zig{s}", .{exe_extension});
    defer allocator.free(zig_exe);

    var it = std.mem.tokenize(u8, env_path, &[_]u8{std.fs.path.delimiter});
    while (it.next()) |path| {
        if (builtin.os.tag == .windows) {
            if (std.mem.indexOfScalar(u8, path, '/') != null) continue;
        }
        const full_path = try std.fs.path.join(allocator, &[_][]const u8{ path, zig_exe });
        defer allocator.free(full_path);

        if (!std.fs.path.isAbsolute(full_path)) continue;

        const file = std.fs.openFileAbsolute(full_path, .{}) catch continue;
        defer file.close();
        const stat = file.stat() catch continue;
        if (stat.kind == .Directory) continue;

        return try allocator.dupe(u8, full_path);
    }
    return null;
}

fn isAbsoluteExists(config_zig_exe_path: ?[]const u8) ?[]const u8 {
    if (config_zig_exe_path) |exe_path| {
        if (std.fs.path.isAbsolute(exe_path)) {
            if (std.fs.cwd().access(exe_path, .{})) {
                return exe_path;
            } else |_| {}
        }
        logger.debug("zig path `{s}` is not absolute, will look in path", .{exe_path});
    }
    return null;
}

fn getZigLibAlloc(allocator: std.mem.Allocator, zig_exe_path: []const u8) ![]const u8 {
    // Use `zig env` to find the lib path
    const zig_env_result = try std.ChildProcess.exec(.{
        .allocator = allocator,
        .argv = &[_][]const u8{ zig_exe_path, "env" },
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
                    unreachable;
                };
                defer std.json.parseFree(Env, json_env, .{ .allocator = allocator });
                // We know this is allocated with `allocator`, we just steal it!
                defer json_env.lib_dir = null;
                return json_env.lib_dir.?;
            }
        },
        else => {
            logger.err("zig env invocation failed", .{});
        },
    }
    unreachable;
}

fn getZigBuiltinAlloc(allocator: std.mem.Allocator, zig_exe_path: []const u8, config_dir: []const u8) ![]const u8 {
    logger.info("getZigBuiltinAlloc: {s}", .{config_dir});
    const result = try std.ChildProcess.exec(.{
        .allocator = allocator,
        .argv = &.{
            zig_exe_path,
            "build-exe",
            "--show-builtin",
        },
        .max_output_bytes = 1024 * 1024 * 50,
    });
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    var d = try std.fs.cwd().openDir(config_dir, .{});
    defer d.close();

    const f = try d.createFile("builtin.zig", .{});
    defer f.close();

    try f.writer().writeAll(result.stdout);

    return try std.fs.path.join(allocator, &.{ config_dir, "builtin.zig" });
}

pub fn init(
    allocator: std.mem.Allocator,
    config_dir: ?[]const u8,
    config_zig_exe_path: ?[]const u8,
    config_zig_lib_path: ?[]const u8,
    config_builtin_path: ?[]const u8,
    config_build_runner_path: ?[]const u8,
    config_build_runner_cache_path: ?[]const u8,
    cache_root: []const u8,
    global_cache_root: []const u8,
) !Self {
    const zig_exe_path = if (isAbsoluteExists(config_zig_exe_path)) |path|
        try allocator.dupe(u8, path)
    else
        (try findZig(allocator)).?;
    logger.info("Using zig executable: {s}", .{zig_exe_path});

    const zig_lib_path = if (config_zig_lib_path) |path|
        try allocator.dupe(u8, path)
    else
        try getZigLibAlloc(allocator, zig_exe_path);
    logger.info("Using zig lib path: {s}", .{zig_lib_path});

    const builtin_path = if (config_builtin_path) |path|
        try allocator.dupe(u8, path)
    else blk: {
        if (config_dir) |dir| {
            break :blk try getZigBuiltinAlloc(allocator, zig_exe_path, dir);
        } else {
            logger.info("no config_dir", .{});
            return error.NoConfigDir;
        }
    };
    logger.info("Using builtin_path: {s}", .{builtin_path});

    const build_runner_path = if (config_build_runner_path) |path|
        try allocator.dupe(u8, path)
    else blk: {
        var exe_dir_bytes: [std.fs.MAX_PATH_BYTES]u8 = undefined;
        const exe_dir_path = try std.fs.selfExeDirPath(&exe_dir_bytes);
        break :blk try std.fs.path.resolve(allocator, &[_][]const u8{ exe_dir_path, "build_runner.zig" });
    };

    const build_runner_cache_path = if (config_build_runner_cache_path) |path|
        try allocator.dupe(u8, path)
    else blk: {
        const cache_dir_path = (try known_folders.getPath(allocator, .cache)) orelse {
            logger.warn("Known-folders could not fetch the cache path", .{});
            unreachable;
        };
        defer allocator.free(cache_dir_path);
        break :blk try std.fs.path.resolve(allocator, &[_][]const u8{ cache_dir_path, "zls" });
    };

    const std_uri = try stdUriFromLibPath(allocator, zig_lib_path);

    return Self{
        .allocator = allocator,
        .exe_path = zig_exe_path,
        .std_uri = std_uri,
        .builtin_path = builtin_path,
        .build_runner_path = build_runner_path,
        .build_runner_cache_path = build_runner_cache_path,
        .cache_root = try allocator.dupe(u8, cache_root),
        .global_cache_root = try allocator.dupe(u8, global_cache_root),
    };
}

pub fn deinit(self: *Self) void {
    self.allocator.free(self.cache_root);
    self.allocator.free(self.global_cache_root);
    self.allocator.free(self.build_runner_cache_path);
    self.allocator.free(self.build_runner_path);
    self.allocator.free(self.builtin_path);
    self.allocator.free(self.std_uri);
    self.allocator.free(self.exe_path);
}
