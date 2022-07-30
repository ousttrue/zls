const std = @import("std");
const BuildAssociatedConfig = @import("./BuildAssociatedConfig.zig");
const URI = @import("./uri.zig");
const ZigEnv = @import("./ZigEnv.zig");
const logger = std.log.scoped(.BuildFile);
const Pkg = struct {
    name: []const u8,
    uri: []const u8,
};
const Self = @This();

allocator: std.mem.Allocator,
refs: usize,
uri: []const u8,
packages: std.ArrayListUnmanaged(Pkg),
builtin_uri: ?[]const u8 = null,

pub fn new(allocator: std.mem.Allocator, uri: []const u8) !*Self {
    var self = try allocator.create(Self);
    self.* = Self{
        .allocator = allocator,
        .refs = 1,
        .uri = try allocator.dupe(u8, uri),
        .packages = .{},
    };
    return self;
}

pub fn delete(self: *Self) void {
    if (self.builtin_uri) |builtin_uri| {
        self.allocator.free(builtin_uri);
    }

    for (self.packages.items) |pkg| {
        self.allocator.free(pkg.name);
        self.allocator.free(pkg.uri);
    }
    self.packages.deinit(self.allocator);

    self.allocator.free(self.uri);
    self.allocator.destroy(self);
}

pub fn decrement(self: *Self) void {
    self.refs -= 1;
    if (self.refs == 0) {
        logger.debug("Freeing build file {s}", .{self.uri});
        self.delete();
    }
}

fn loadBuildAssociatedConfiguration(build_file: *Self, allocator: std.mem.Allocator, build_file_path: []const u8) !void {
    const directory_path = build_file_path[0 .. build_file_path.len - "build.zig".len];

    const options = std.json.ParseOptions{ .allocator = allocator };
    const build_associated_config = blk: {
        const config_file_path = try std.fs.path.join(allocator, &[_][]const u8{ directory_path, "zls.build.json" });
        defer allocator.free(config_file_path);

        var config_file = std.fs.cwd().openFile(config_file_path, .{}) catch |err| {
            if (err == error.FileNotFound) return;
            return err;
        };
        defer config_file.close();

        const file_buf = try config_file.readToEndAlloc(allocator, 0x1000000);
        defer allocator.free(file_buf);

        break :blk try std.json.parse(BuildAssociatedConfig, &std.json.TokenStream.init(file_buf), options);
    };
    defer std.json.parseFree(BuildAssociatedConfig, build_associated_config, options);

    if (build_associated_config.relative_builtin_path) |relative_builtin_path| {
        var absolute_builtin_path = try std.mem.concat(allocator, u8, &.{ directory_path, relative_builtin_path });
        defer allocator.free(absolute_builtin_path);
        build_file.builtin_uri = try URI.fromPath(allocator, absolute_builtin_path);
    }
}

pub fn loadPackages(build_file: *Self, allocator: std.mem.Allocator, _build_file_path: ?[]const u8, zigenv: ZigEnv) !void {
    const build_file_path = _build_file_path orelse try URI.parse(allocator, build_file.uri);
    defer if (_build_file_path == null) allocator.free(build_file_path);
    const directory_path = build_file_path[0 .. build_file_path.len - "build.zig".len];

    const zig_run_result = try std.ChildProcess.exec(.{
        .allocator = allocator,
        .argv = &[_][]const u8{
            zigenv.exe_path.slice(),
            "run",
            zigenv.build_runner_path,
            "--cache-dir",
            zigenv.build_runner_cache_path,
            "--pkg-begin",
            "@build@",
            build_file_path,
            "--pkg-end",
            "--",
            zigenv.exe_path.slice(),
            directory_path,
            zigenv.cache_root,
            zigenv.global_cache_root,
        },
    });

    defer {
        allocator.free(zig_run_result.stdout);
        allocator.free(zig_run_result.stderr);
    }

    switch (zig_run_result.term) {
        .Exited => |exit_code| {
            if (exit_code == 0) {
                logger.debug("Finished zig run for build file {s}", .{build_file.uri});

                for (build_file.packages.items) |old_pkg| {
                    allocator.free(old_pkg.name);
                    allocator.free(old_pkg.uri);
                }

                build_file.packages.shrinkAndFree(allocator, 0);
                var line_it = std.mem.split(u8, zig_run_result.stdout, "\n");
                while (line_it.next()) |line| {
                    if (std.mem.indexOfScalar(u8, line, '\x00')) |zero_byte_idx| {
                        const name = line[0..zero_byte_idx];
                        const rel_path = line[zero_byte_idx + 1 ..];

                        const pkg_abs_path = try std.fs.path.resolve(allocator, &[_][]const u8{ directory_path, rel_path });
                        defer allocator.free(pkg_abs_path);

                        const pkg_uri = try URI.fromPath(allocator, pkg_abs_path);
                        errdefer allocator.free(pkg_uri);

                        const duped_name = try allocator.dupe(u8, name);
                        errdefer allocator.free(duped_name);

                        (try build_file.packages.addOne(allocator)).* = .{
                            .name = duped_name,
                            .uri = pkg_uri,
                        };
                    }
                }
            } else {
                logger.debug("{s} => {s}", .{ build_file.uri, zig_run_result.stderr });
                return error.RunFailed;
            }
        },
        else => return error.RunFailed,
    }
}

pub fn extractPackages(allocator: std.mem.Allocator, uri: []const u8, zigenv: ZigEnv) !*Self {
    logger.debug("{s} => extracting packages...", .{uri});

    // This is a build file.
    var build_file = try Self.new(allocator, uri);
    errdefer build_file.delete();

    const build_file_path = try URI.parse(allocator, build_file.uri);
    defer allocator.free(build_file_path);

    if (build_file.loadBuildAssociatedConfiguration(allocator, build_file_path)) {
        logger.info("{s} => loadBuildAssociatedConfiguration", .{build_file.uri});
    } else |err| {
        logger.debug("Failed to load config associated with build file {s} (error: {})", .{ build_file.uri, err });
    }

    if (build_file.builtin_uri == null) {
        build_file.builtin_uri = try URI.fromPath(allocator, zigenv.builtin_path.slice());
        logger.info("builtin config not found, falling back to default: {s}", .{build_file.builtin_uri});
    }

    // TODO: Do this in a separate thread?
    // It can take quite long.
    if (build_file.loadPackages(allocator, build_file_path, zigenv)) {
        logger.info("{s} => loadPackages", .{build_file.uri});
    } else |err| {
        logger.debug("{s} => {}", .{ build_file.uri, err });
    }

    return build_file;
}
