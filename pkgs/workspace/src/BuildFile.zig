const std = @import("std");
const astutil = @import("astutil");
const FixedPath = astutil.FixedPath;
const ZigEnv = @import("./ZigEnv.zig");
const logger = std.log.scoped(.BuildFile);
const Self = @This();

allocator: std.mem.Allocator,
path: FixedPath,
packages: std.StringHashMap(FixedPath),

pub fn new(allocator: std.mem.Allocator, path: FixedPath, zigenv: ZigEnv) !*Self {
    var self = try allocator.create(Self);
    self.* = Self{
        .allocator = allocator,
        .path = path,
        .packages = std.StringHashMap(FixedPath).init(allocator),
    };

    // TODO: Do this in a separate thread?
    // It can take quite long.
    if (self.loadPackages(zigenv)) {
        logger.info("loadPackages: {s} => ok", .{path.slice()});
    } else |err| {
        logger.debug("loadPackages: {s} => {}", .{path.slice(), err });
    }

    return self;
}

pub fn delete(self: *Self) void {
    var it = self.packages.iterator();
    while (it.next()) |entry| {
        self.allocator.free(entry.key_ptr.*);
    }
    self.packages.deinit();
    self.allocator.destroy(self);
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

fn loadPackages(self: *Self, zigenv: ZigEnv) !void {
    const zig_run_result = try zigenv.runBuildRunner(self.allocator, self.path);
    defer self.allocator.free(zig_run_result);

    var stream = std.json.TokenStream.init(zig_run_result);
    const options = std.json.ParseOptions{ .allocator = self.allocator, .ignore_unknown_fields = true };
    const project = try std.json.parse(Project, &stream, options);
    defer std.json.parseFree(Project, project, options);

    const base_dir = self.path.parent().?;
    for (project.packages) |pkg| {
        const copy = try self.allocator.dupe(u8, pkg.name);
        logger.debug("{s}: {s}", .{ pkg.name, pkg.path });
        try self.packages.put(copy, base_dir.child(pkg.path));
    }
}
