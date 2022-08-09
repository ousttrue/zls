const std = @import("std");
const builtin = @import("builtin");
const astutil = @import("astutil");
const known_folders = @import("known-folders");
const FixedPath = astutil.FixedPath;
const ImportSolver = astutil.ImportSolver;
const logger = std.log.scoped(.ZigEnv);

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

fn getZigLibAlloc(allocator: std.mem.Allocator, zig_exe_path: FixedPath) !FixedPath {
    // Use `zig env` to find the lib path
    const zig_env_result = try zig_exe_path.exec(allocator, &.{"env"});
    defer allocator.free(zig_env_result.stdout);
    defer allocator.free(zig_env_result.stderr);

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
                return FixedPath.fromFullpath(json_env.lib_dir.?);
            }
        },
        else => {
            logger.err("zig env invocation failed", .{});
        },
    }
    unreachable;
}

fn getZigBuiltinAlloc(
    allocator: std.mem.Allocator,
    zig_exe_path: FixedPath,
    config_dir: FixedPath,
) !FixedPath {
    const result = try zig_exe_path.exec(allocator, &.{
        "build-exe",
        "--show-builtin",
    });
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    var d = try std.fs.cwd().openDir(config_dir.slice(), .{});
    defer d.close();

    const f = try d.createFile("builtin.zig", .{});
    defer f.close();
    try f.writer().writeAll(result.stdout);

    var path = FixedPath.fromFullpath(config_dir.slice());
    path = path.child("builtin.zig");
    logger.info("{s}", .{path.slice()});

    return path;
}

const Self = @This();

exe: FixedPath,
lib: FixedPath,
std_path: FixedPath,
builtin_path: FixedPath,
build_runner_path: FixedPath,
build_runner_cache_path: FixedPath,
cache_root: FixedPath,
global_cache_root: FixedPath,

pub fn init(
    allocator: std.mem.Allocator,
    config_dir: ?FixedPath,
    config_zig_exe_path: ?[]const u8,
    config_zig_lib_path: ?[]const u8,
    config_builtin_path: ?[]const u8,
    config_build_runner_path: ?[]const u8,
    config_build_runner_cache_path: ?[]const u8,
    cache_root: []const u8,
    global_cache_root: []const u8,
) !Self {
    // exe
    var zig_exe_path: FixedPath = .{};
    if (config_zig_exe_path) |not_null| {
        const exe = FixedPath.fromFullpath(not_null);
        if (exe.isAbsoluteExists()) {
            zig_exe_path = exe;
        }
    }
    if (zig_exe_path.len() == 0) {
        if (try findZig(allocator)) |exe| {
            zig_exe_path = FixedPath.fromFullpath(exe);
        }
    }
    logger.info("Using zig executable: {s}", .{zig_exe_path.slice()});

    // lib
    var zig_lib_path: FixedPath = .{};
    if (config_zig_lib_path) |not_null| {
        zig_lib_path = FixedPath.fromFullpath(not_null);
    } else {
        zig_lib_path = try getZigLibAlloc(allocator, zig_exe_path);
    }
    logger.info("Using zig lib path: {s}", .{zig_lib_path.slice()});

    // builtin_path
    var builtin_path: FixedPath = .{};
    if (config_builtin_path) |not_null| {
        builtin_path = FixedPath.fromFullpath(not_null);
    } else {
        if (config_dir) |dir| {
            builtin_path = try getZigBuiltinAlloc(allocator, zig_exe_path, dir);
        } else {
            logger.err("no config_dir", .{});
            return error.NoConfigDir;
        }
    }
    logger.info("Using builtin_path: {s}", .{builtin_path.slice()});

    // build_runner_path
    var build_runner_path = FixedPath{};
    if (config_build_runner_path) |not_null| {
        build_runner_path = FixedPath.fromFullpath(not_null);
    } else {
        const exe_dir_path = try FixedPath.fromSelfExe();
        build_runner_path = exe_dir_path.child("build_runner.zig");
    }
    logger.info("Using build_runner_path: {s}", .{build_runner_path.slice()});

    // build_runner_cache_path
    var build_runner_cache_path = FixedPath{};
    if (config_build_runner_cache_path) |not_null| {
        build_runner_cache_path = FixedPath.fromFullpath(not_null);
    } else {
        const cache_dir_path = (try known_folders.getPath(allocator, .cache)) orelse {
            logger.warn("Known-folders could not fetch the cache path", .{});
            unreachable;
        };
        defer allocator.free(cache_dir_path);
        build_runner_cache_path = FixedPath.fromFullpath(cache_dir_path).child("zls");
    }
    logger.info("Using build_cache_runner_path: {s}", .{build_runner_cache_path.slice()});

    return Self{
        .exe = zig_exe_path,
        .lib = zig_lib_path,
        .std_path = zig_lib_path.child("std/std.zig"),
        .builtin_path = builtin_path,
        .build_runner_path = build_runner_path,
        .build_runner_cache_path = build_runner_cache_path,
        .cache_root = FixedPath.fromFullpath(cache_root),
        .global_cache_root = FixedPath.fromFullpath(global_cache_root),
    };
}

pub fn spawnZigFmt(self: Self, allocator: std.mem.Allocator, src: []const u8) ![]const u8 {
    var process = std.ChildProcess.init(&[_][]const u8{ self.exe.slice(), "fmt", "--stdin" }, allocator);
    process.stdin_behavior = .Pipe;
    process.stdout_behavior = .Pipe;
    try process.spawn();
    try process.stdin.?.writeAll(src);
    process.stdin.?.close();
    process.stdin = null;
    const bytes = try process.stdout.?.reader().readAllAlloc(allocator, std.math.maxInt(usize));
    switch (try process.wait()) {
        .Exited => |code| if (code == 0) {
            return bytes;
        } else {
            return error.ExitedNonZero;
        },
        else => {
            return error.ProcessError;
        },
    }
}

pub fn runBuildRunner(self: Self, allocator: std.mem.Allocator, build_file_path: FixedPath) ![]const u8 {
    const directory_path = build_file_path.parent().?;
    const zig_run_result = try self.exe.exec(allocator, &.{
        "run",
        self.build_runner_path.slice(),
        "--cache-dir",
        self.build_runner_cache_path.slice(),
        "--pkg-begin",
        "@build@",
        build_file_path.slice(),
        "--pkg-end",
        "--",
        self.exe.slice(),
        directory_path.slice(),
        self.cache_root.slice(),
        self.global_cache_root.slice(),
    });
    defer allocator.free(zig_run_result.stderr);
    return switch (zig_run_result.term) {
        .Exited => |exit_code| if (exit_code == 0)
            zig_run_result.stdout
        else
            return error.RunFailed,
        else => return error.RunFailed,
    };
}

// json types
const NamePath = struct {
    name: []const u8,
    path: []const u8,
};

const Project = struct {
    // LibExeObjStep
    objects: []const NamePath,
    // Pkg
    packages: []const NamePath,
};

// build file is project_root/build.zig
pub fn loadPackages(self: Self, allocator: std.mem.Allocator, import_solver: *ImportSolver, root: FixedPath) !void {  
    const zig_run_result = try self.runBuildRunner(allocator, root.child("build.zig"));
    defer allocator.free(zig_run_result);

    var stream = std.json.TokenStream.init(zig_run_result);
    const options = std.json.ParseOptions{ .allocator = allocator, .ignore_unknown_fields = true };
    const project = try std.json.parse(Project, &stream, options);
    defer std.json.parseFree(Project, project, options);

    for (project.packages) |pkg| {
        try import_solver.push(pkg.name, root.child(pkg.path));
    }
}
