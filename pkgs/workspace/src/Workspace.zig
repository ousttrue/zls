///
/// * Workspace
///    * uri: Document
///
const std = @import("std");
const Ast = std.zig.Ast;
const ast = @import("./ast.zig");
const FixedPath = @import("./FixedPath.zig");
const URI = @import("./uri.zig");
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

pub fn uriFromImportStrAlloc(
    self: Self,
    allocator: std.mem.Allocator,
    doc: *Document,
    import_str: []const u8,
) ![]const u8 {
    if (std.mem.eql(u8, import_str, "std")) {
        // special path
        return try allocator.dupe(u8, self.zigenv.std_uri);
    } else if (std.mem.eql(u8, import_str, "builtin")) {
        // special path
        if (self.build_file.builtin_uri) |builtin_uri| {
            return try allocator.dupe(u8, builtin_uri);
        }
        return try URI.fromPath(allocator, self.zigenv.builtin_path.slice());
    } else if (!std.mem.endsWith(u8, import_str, ".zig")) {
        // std.build.Pkg
        for (self.build_file.packages.items) |pkg| {
            if (std.mem.eql(u8, import_str, pkg.name)) {
                return try allocator.dupe(u8, pkg.uri);
            }
        }
        return error.PkgNotFound;
    } else {
        // "./relative/path_to.zig"
        // const base = doc.uri;
        // var base_len = base.len;
        // while (base[base_len - 1] != '/' and base_len > 0) {
        //     base_len -= 1;
        // }
        // base_len -= 1;
        // if (base_len <= 0) {
        //     return error.UriBadScheme;
        // }
        const path = doc.path.parent().?.child(import_str);
        return try URI.fromPath(allocator, path.slice());
    }
}

pub fn resolveImport(self: *Self, doc: *Document, import_str: []const u8) ?*Document {
    const allocator = self.allocator;
    const final_uri = self.uriFromImportStrAlloc(allocator, doc, import_str) catch return null;
    defer allocator.free(final_uri);
    const path = FixedPath.fromUri(final_uri) catch return null;
    return self.getOrLoadDocument(path);
}
