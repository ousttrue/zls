const std = @import("std");
const BuildAssociatedConfig = @import("./BuildAssociatedConfig.zig");
const URI = @import("./uri.zig");
const FixedPath = @import("./FixedPath.zig");
const ZigEnv = @import("./ZigEnv.zig");
const logger = std.log.scoped(.BuildFile);
const Pkg = struct {
    name: []const u8,
    uri: []const u8,
};
const Self = @This();

allocator: std.mem.Allocator,
path: FixedPath,
packages: std.ArrayListUnmanaged(Pkg),
builtin_uri: ?[]const u8 = null,

pub fn new(allocator: std.mem.Allocator, path: FixedPath) !*Self {
    var self = try allocator.create(Self);
    self.* = Self{
        .allocator = allocator,
        .path = path,
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
    self.allocator.destroy(self);
}

fn loadBuildAssociatedConfiguration(self: *Self, allocator: std.mem.Allocator) !void {
    const directory_path = self.path.parent().?;

    const options = std.json.ParseOptions{ .allocator = allocator };
    const build_associated_config = blk: {
        const config_file_path = try std.fs.path.join(allocator, &[_][]const u8{ directory_path.slice(), "zls.build.json" });
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
        var absolute_builtin_path = try std.mem.concat(allocator, u8, &.{ directory_path.slice(), relative_builtin_path });
        defer allocator.free(absolute_builtin_path);
        self.builtin_uri = try URI.fromPath(allocator, absolute_builtin_path);
    }
}

pub fn loadPackages(self: *Self, allocator: std.mem.Allocator, zigenv: ZigEnv) !void {
    // const build_file_path = _build_file_path orelse try URI.parse(allocator, self.uri);
    // defer if (_build_file_path == null) allocator.free(build_file_path);
    const directory_path = self.path.parent().?;

    const zig_run_result = try zigenv.exe.exec(allocator, &.{
        "run",
        zigenv.build_runner_path.slice(),
        "--cache-dir",
        zigenv.build_runner_cache_path.slice(),
        "--pkg-begin",
        "@build@",
        self.path.slice(),
        "--pkg-end",
        "--",
        zigenv.exe.slice(),
        directory_path.slice(),
        zigenv.cache_root.slice(),
        zigenv.global_cache_root.slice(),
    });
    defer {
        allocator.free(zig_run_result.stdout);
        allocator.free(zig_run_result.stderr);
    }

    switch (zig_run_result.term) {
        .Exited => |exit_code| {
            if (exit_code == 0) {
                for (self.packages.items) |old_pkg| {
                    allocator.free(old_pkg.name);
                    allocator.free(old_pkg.uri);
                }

                self.packages.shrinkAndFree(allocator, 0);
                var line_it = std.mem.split(u8, zig_run_result.stdout, "\n");
                while (line_it.next()) |line| {
                    if (std.mem.indexOfScalar(u8, line, '\x00')) |zero_byte_idx| {
                        const name = line[0..zero_byte_idx];
                        const rel_path = line[zero_byte_idx + 1 ..];

                        const pkg_abs_path = try std.fs.path.resolve(allocator, &[_][]const u8{ self.path.parent().?.slice(), rel_path });
                        defer allocator.free(pkg_abs_path);

                        const pkg_uri = try URI.fromPath(allocator, pkg_abs_path);
                        errdefer allocator.free(pkg_uri);

                        const duped_name = try allocator.dupe(u8, name);
                        errdefer allocator.free(duped_name);

                        (try self.packages.addOne(allocator)).* = .{
                            .name = duped_name,
                            .uri = pkg_uri,
                        };
                    }
                }
            } else {
                logger.debug("{s} => {s}", .{ self.path.slice(), zig_run_result.stderr });
                return error.RunFailed;
            }
        },
        else => return error.RunFailed,
    }
}

pub fn extractPackages(allocator: std.mem.Allocator, path: FixedPath, zigenv: ZigEnv) !*Self {
    logger.debug("extracting packages: {s} ...", .{path.slice()});

    // This is a build file.
    var build_file = try Self.new(allocator, path);
    errdefer build_file.delete();

    if (build_file.loadBuildAssociatedConfiguration(allocator)) {
        logger.info("loadBuildAssociatedConfiguration: {s} ok", .{build_file.path.slice()});
    } else |err| {
        logger.debug("loadBuildAssociatedConfiguration: {s} {}", .{ build_file.path.slice(), err });
    }

    if (build_file.builtin_uri == null) {
        build_file.builtin_uri = try URI.fromPath(allocator, zigenv.builtin_path.slice());
        logger.info("builtin config not found, falling back to default: {s}", .{build_file.builtin_uri});
    }

    // TODO: Do this in a separate thread?
    // It can take quite long.
    if (build_file.loadPackages(allocator, zigenv)) {
        logger.info("loadPackages: {s} => ok", .{build_file.path.slice()});
    } else |err| {
        logger.debug("loadPackages: {s} => {}", .{ build_file.path.slice(), err });
    }

    return build_file;
}
