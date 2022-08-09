///
/// * Workspace
///    * uri: Document
///
const std = @import("std");
const astutil = @import("astutil");
const Ast = std.zig.Ast;
const ast = astutil.ast;
const FixedPath = astutil.FixedPath;
const Document = astutil.Document;
const ZigEnv = @import("./ZigEnv.zig");
const DocumentScope = @import("./DocumentScope.zig");
const PathPosition = astutil.PathPosition;
const ImportSolver = astutil.ImportSolver;
const DocumentStore = astutil.DocumentStore;
const Project = astutil.Project;
const logger = std.log.scoped(.Workspace);
const Self = @This();

allocator: std.mem.Allocator,
zigenv: ZigEnv,
root: FixedPath,
import_solver : ImportSolver,
store: DocumentStore,
handles: std.AutoHashMap(*Document, *DocumentScope),

pub fn new(
    allocator: std.mem.Allocator,
    zigenv: ZigEnv,
    root: FixedPath,
) !*Self {
    var self = try allocator.create(Self);

    self.* = Self{
        .allocator = allocator,
        .zigenv = zigenv,
        .root = root,
        .import_solver = ImportSolver.init(allocator),
        .store = DocumentStore.init(allocator),
        .handles = std.AutoHashMap(*Document, *DocumentScope).init(allocator),
    };

    // initialize import_solver
    try self.import_solver.push("std", zigenv.std_path);
    try zigenv.loadPackages(allocator, &self.import_solver, root);

    return self;
}

pub fn delete(self: *Self) void {
    self.store.deinit();
    self.import_solver.deinit();
    self.handles.deinit();
    self.allocator.destroy(self);
}

pub fn project(self: *Self) Project
{
    return .{
        .import_solver = self.import_solver,
        .store = &self.store,
    };
}
