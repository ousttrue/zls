const std = @import("std");
const astutil = @import("astutil");
const BuildAssociatedConfig = @import("./BuildAssociatedConfig.zig");
const FixedPath = astutil.FixedPath;
const ZigEnv = @import("./ZigEnv.zig");
const logger = std.log.scoped(.BuildFile);
const Self = @This();

allocator: std.mem.Allocator,
path: FixedPath,
packages: std.StringHashMap(FixedPath),
builtin_path: ?FixedPath = null,

pub fn new(allocator: std.mem.Allocator, path: FixedPath) !*Self {
    var self = try allocator.create(Self);
    self.* = Self{
        .allocator = allocator,
        .path = path,
        .packages = std.StringHashMap(FixedPath).init(allocator),
    };
    return self;
}

pub fn delete(self: *Self) void {
    self.packages.deinit();
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
        self.builtin_path = FixedPath.fromFullpath(absolute_builtin_path);
    }
}

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

pub fn loadPackages(self: *Self, zigenv: ZigEnv) !void {
    const zig_run_result = try zigenv.runBuildRunner(self.allocator, self.path);
    defer self.allocator.free(zig_run_result);
    self.packages.deinit();
    self.packages = std.StringHashMap(FixedPath).init(self.allocator);

    var stream = std.json.TokenStream.init(zig_run_result);
    const options = std.json.ParseOptions{ .allocator = self.allocator, .ignore_unknown_fields = true };
    const project = try std.json.parse(Project, &stream, options);
    defer std.json.parseFree(Project, project, options);

    const base_dir = self.path.parent().?;
    for (project.packages) |pkg| {
        const copy = try self.allocator.dupe(u8, pkg.name);
        logger.debug("{s}: {s}", .{pkg.name, pkg.path});
        try self.packages.put(copy, base_dir.child(pkg.path));
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

    if (build_file.builtin_path == null) {
        build_file.builtin_path = zigenv.builtin_path;
        logger.info("builtin config not found, falling back to default: {s}", .{build_file.builtin_path.?.slice()});
    }

    // TODO: Do this in a separate thread?
    // It can take quite long.
    if (build_file.loadPackages(zigenv)) {
        logger.info("loadPackages: {s} => ok", .{build_file.path.slice()});
    } else |err| {
        logger.debug("loadPackages: {s} => {}", .{ build_file.path.slice(), err });
    }

    return build_file;
}
