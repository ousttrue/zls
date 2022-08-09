const std = @import("std");
const astutil = @import("astutil");
const FixedPath = astutil.FixedPath;
const ImportSolver = astutil.ImportSolver;
const ZigEnv = @import("./ZigEnv.zig");
const logger = std.log.scoped(.BuildFile);
var STD = "std";
const Self = @This();

allocator: std.mem.Allocator,
path: FixedPath,
import_solver: ImportSolver,

pub fn new(allocator: std.mem.Allocator, path: FixedPath, zigenv: ZigEnv) !*Self {
    var self = try allocator.create(Self);
    self.* = Self{
        .allocator = allocator,
        .path = path,
        .import_solver = ImportSolver.init(allocator),
    };

    try self.import_solver.push(STD, zigenv.std_path);

    // } else if (std.mem.eql(u8, import_str, "builtin")) {
    //     // special path
    //     return self.zigenv.builtin_path;

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
    self.import_solver.deinit();
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
        try self.import_solver.push(pkg.name, base_dir.child(pkg.path));
    }
}
