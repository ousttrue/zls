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
const BuildFile = @import("./BuildFile.zig");
const ZigEnv = @import("./ZigEnv.zig");
const DocumentScope = @import("./DocumentScope.zig");
const PathPosition = astutil.PathPosition;
const ImportSolver = astutil.ImportSolver;
const DocumentStore = astutil.DocumentStore;
const logger = std.log.scoped(.Workspace);
const Self = @This();

allocator: std.mem.Allocator,
zigenv: ZigEnv,
root: FixedPath,
store: DocumentStore,
handles: std.AutoHashMap(*Document, *DocumentScope),
build_file: *BuildFile,

pub fn new(
    allocator: std.mem.Allocator,
    zigenv: ZigEnv,
    root: FixedPath,
) !*Self {
    var self = try allocator.create(Self);

    // build file is project_root/build.zig
    var build_file = try BuildFile.new(allocator, root.child("build.zig"), zigenv);

    self.* = Self{
        .allocator = allocator,
        .zigenv = zigenv,
        .root = root,
        .store = DocumentStore.init(allocator),
        .handles = std.AutoHashMap(*Document, *DocumentScope).init(allocator),
        .build_file = build_file,
    };
    return self;
}

pub fn delete(self: *Self) void {
    self.store.deinit();
    // var entry_iterator = self.handles.iterator();
    // while (entry_iterator.next()) |entry| {
    //     self.allocator.free(entry.key_ptr.*);
    //     entry.value_ptr.*.delete();
    // }
    self.handles.deinit();
    self.build_file.delete();
    self.allocator.destroy(self);
}

pub fn openDocument(self: *Self, path: FixedPath, text: []const u8) !*Document {
    if (self.store.get(path)) |doc| {
        // update document
        try doc.update(text);
        return doc;
    } else {
        // new document
        logger.debug("new {s}", .{path.slice()});
        const doc = try Document.new(self.allocator, path, text);
        errdefer doc.delete();
        try self.store.put(doc);
        try self.handles.putNoClobber(doc, try DocumentScope.new(self.allocator, &doc.ast_context.tree));
        return doc;
    }
}

pub fn getDocument(self: *Self, path: FixedPath) ?*Document {
    if (self.store.get(path)) |found| {
        return found;
    }

    logger.warn("not found: {s}", .{path.slice()});
    return null;
}

pub fn getOrLoadDocument(self: *Self, path: FixedPath) !?*Document {
    if (self.store.get(path)) |found| {
        return found;
    }

    return try self.store.getOrLoad(path);
}

pub fn resolveImport(self: *Self, doc: *Document, text: []const u8) !?*Document {
    return if (self.build_file.import_solver.solve(doc.path, ImportSolver.ImportType.fromText(text))) |path|
        try self.getOrLoadDocument(path)
    else
        null;
}
