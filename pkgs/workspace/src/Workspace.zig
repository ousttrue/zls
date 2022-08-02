///
/// * Workspace
///    * uri: Document
///
const std = @import("std");
const Ast = std.zig.Ast;
const ast = @import("./ast.zig");
const FixedPath = @import("./FixedPath.zig");
const Document = @import("./Document.zig");
const BuildFile = @import("./BuildFile.zig");
const ZigEnv = @import("./ZigEnv.zig");
const UriBytePosition = @import("./UriBytePosition.zig");
const logger = std.log.scoped(.Workspace);
const Self = @This();

allocator: std.mem.Allocator,
zigenv: ZigEnv,
root: FixedPath,
handles: std.StringHashMap(*Document),
build_file: *BuildFile,

pub fn new(
    allocator: std.mem.Allocator,
    zigenv: ZigEnv,
    root: FixedPath,
) !*Self {
    logger.info("{s}", .{root.slice()});
    var self = try allocator.create(Self);
    self.* = Self{
        .allocator = allocator,
        .zigenv = zigenv,
        .root = root,
        .handles = std.StringHashMap(*Document).init(allocator),
        .build_file = try BuildFile.extractPackages(self.allocator, root.child("build.zig"), zigenv),
    };
    return self;
}

pub fn delete(self: *Self) void {
    var entry_iterator = self.handles.iterator();
    while (entry_iterator.next()) |entry| {
        self.allocator.free(entry.key_ptr.*);
        entry.value_ptr.*.delete();
    }
    self.handles.deinit();
    self.build_file.delete();
    self.allocator.destroy(self);
}

pub fn openDocument(self: *Self, path: FixedPath, text: []const u8) !*Document {
    const duped_text = try self.allocator.dupeZ(u8, text);
    errdefer self.allocator.free(duped_text);
    if (self.getDocument(path)) |doc| {
        // update document
        try doc.update(duped_text);
        try doc.refreshDocument();
        return doc;
    } else {
        // new document
        const doc = try Document.new(self.allocator, path, duped_text);
        errdefer doc.delete();
        logger.debug("new document: {s}", .{path.slice()});
        try self.handles.putNoClobber(doc.path.slice(), doc);
        return doc;
    }
}

pub fn getDocument(self: *Self, path: FixedPath) ?*Document {
    if (self.handles.get(path.slice())) |found| {
        return found;
    }

    logger.warn("not found: {s}", .{path.slice()});
    return null;
}

pub fn getOrLoadDocument(self: *Self, path: FixedPath) ?*Document {
    if (self.handles.get(path.slice())) |found| {
        return found;
    }

    const contents = path.readContents(self.allocator) catch return null;
    defer self.allocator.free(contents);
    return self.openDocument(path, contents) catch unreachable;
}

pub fn resolveImportPath(
    self: Self,
    doc: *Document,
    import_str: []const u8,
) !FixedPath {
    if (std.mem.eql(u8, import_str, "std")) {
        // special path
        return self.zigenv.std_path;
    } else if (std.mem.eql(u8, import_str, "builtin")) {
        // special path
        if (self.build_file.builtin_path) |builtin_path| {
            return builtin_path;
        }
        return self.zigenv.builtin_path;
    } else if (!std.mem.endsWith(u8, import_str, ".zig")) {
        // std.build.Pkg
        for (self.build_file.packages.items) |pkg| {
            if (std.mem.eql(u8, import_str, pkg.name)) {
                return pkg.path;
            }
        }
        return error.PkgNotFound;
    } else {
        // relative path
        const resolved = doc.path.parent().?.child(import_str);
        logger.debug("{s}", .{resolved.slice()});
        return resolved;
    }
}

pub fn resolveImport(self: *Self, doc: *Document, import_str: []const u8) ?*Document {
    const path = self.resolveImportPath(doc, import_str) catch return null;
    return self.getOrLoadDocument(path);
}
