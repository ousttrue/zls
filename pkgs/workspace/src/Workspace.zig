///
/// * Workspace
///    * uri: Document
///
const std = @import("std");
const FixedPath = @import("./FixedPath.zig");
const URI = @import("./uri.zig");
const Document = @import("./Document.zig");
const BuildFile = @import("./BuildFile.zig");
const ZigEnv = @import("./ZigEnv.zig");
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

// fn findBuildFile(self: *Self, uri: []const u8) !?*BuildFile {
//     // Look into build files and keep the one that lives closest to the document in the directory structure
//     var candidate: ?*BuildFile = null;
//     {
//         var uri_chars_matched: usize = 0;
//         for (self.build_files.items) |build_file| {
//             const build_file_base_uri = build_file.uri[0 .. std.mem.lastIndexOfScalar(u8, build_file.uri, '/').? + 1];

//             if (build_file_base_uri.len > uri_chars_matched and std.mem.startsWith(u8, uri, build_file_base_uri)) {
//                 uri_chars_matched = build_file_base_uri.len;
//                 candidate = build_file;
//             }
//         }
//     }

//     // Then, try to find the closest build file.
//     var curr_path = try URI.parse(self.allocator, uri);
//     defer self.allocator.free(curr_path);
//     while (true) {
//         if (curr_path.len == 0) break;

//         if (std.mem.lastIndexOfScalar(u8, curr_path[0 .. curr_path.len - 1], std.fs.path.sep)) |idx| {
//             // This includes the last separator
//             curr_path = curr_path[0 .. idx + 1];

//             // Try to open the folder, then the file.
//             var folder = std.fs.cwd().openDir(curr_path, .{}) catch |err| switch (err) {
//                 error.FileNotFound => continue,
//                 else => return err,
//             };
//             defer folder.close();

//             var build_file = folder.openFile("build.zig", .{}) catch |err| switch (err) {
//                 error.FileNotFound, error.AccessDenied => continue,
//                 else => return err,
//             };
//             defer build_file.close();

//             // Calculate build file's URI
//             var candidate_path = try std.mem.concat(self.allocator, u8, &.{ curr_path, "build.zig" });
//             defer self.allocator.free(candidate_path);
//             const build_file_uri = try URI.fromPath(self.allocator, candidate_path);
//             errdefer self.allocator.free(build_file_uri);

//             if (candidate) |candidate_build_file| {
//                 // Check if it is the same as the current candidate we got from the existing build files.
//                 // If it isn't, we need to read the file and make a new build file.
//                 if (std.mem.eql(u8, candidate_build_file.uri, build_file_uri)) {
//                     self.allocator.free(build_file_uri);
//                     break;
//                 }
//             }

//             // Check if the build file already exists
//             if (self.handles.get(build_file_uri)) |build_file_handle| {
//                 candidate = build_file_handle.is_build_file.?;
//                 break;
//             }

//             // Read the build file, create a new document, set the candidate to the new build file.
//             const build_file_text = try build_file.readToEndAllocOptions(
//                 self.allocator,
//                 std.math.maxInt(usize),
//                 null,
//                 @alignOf(u8),
//                 0,
//             );
//             errdefer self.allocator.free(build_file_text);

//             const build_file_handle = try self.newDocument(build_file_uri, build_file_text);
//             candidate = build_file_handle.is_build_file.?;
//             break;
//         } else break;
//     }

//     return candidate;
// }

/// This function asserts the document is not open yet and takes ownership
/// of the uri and text passed in.
fn newDocument(self: *Self, uri: []const u8, text: [:0]u8) anyerror!*Document {
    const doc = try Document.new(self.allocator, uri, text);
    errdefer doc.delete();

    // // TODO: Better logic for detecting std or subdirectories?
    // if (std.mem.indexOf(u8, uri, "/std/") != null) {
    //     // in std
    // } else if (std.mem.endsWith(u8, uri, "/build.zig")) {
    //     var build_file = try BuildFile.extractPackages(self.allocator, uri, self.zigenv);
    //     try self.build_files.append(build_file);
    //     doc.is_build_file = build_file;
    // } else {
    //     if (try self.findBuildFile(uri)) |build_file| {
    //         build_file.refs += 1;
    //         doc.associated_build_file = build_file;
    //     }
    // }

    try self.handles.putNoClobber(uri, doc);
    return doc;
}

pub fn openDocument(self: *Self, uri: []const u8, text: []const u8) !*Document {
    if (self.handles.getEntry(uri)) |entry| {
        try entry.value_ptr.*.refreshDocument();
        return entry.value_ptr.*;
    } else {
        const duped_text = try self.allocator.dupeZ(u8, text);
        errdefer self.allocator.free(duped_text);
        const duped_uri = try self.allocator.dupeZ(u8, uri);
        errdefer self.allocator.free(duped_uri);
        return try self.newDocument(duped_uri, duped_text);
    }
}

pub fn getDocument(self: *Self, uri: []const u8) ?*Document {
    if (self.handles.getEntry(uri)) |entry| {
        return entry.value_ptr.*;
    } else {}
    return null;
}

pub fn uriFromImportStrAlloc(
    self: Self,
    allocator: std.mem.Allocator,
    doc: *Document,
    import_str: []const u8,
) !?[]const u8 {
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
        return null;
    } else {
        // "./relative/path_to.zig"
        const base = doc.uri;
        var base_len = base.len;
        while (base[base_len - 1] != '/' and base_len > 0) {
            base_len -= 1;
        }
        base_len -= 1;
        if (base_len <= 0) {
            return error.UriBadScheme;
        }
        return try URI.pathRelative(allocator, base[0..base_len], import_str);
    }
}

pub fn resolveImport(self: *Self, doc: *Document, import_str: []const u8) !?*Document {
    const allocator = self.allocator;
    const final_uri = (try self.uriFromImportStrAlloc(allocator, doc, import_str)) orelse return null;
    defer allocator.free(final_uri);

    const file_path = try URI.parse(allocator, final_uri);
    defer allocator.free(file_path);
    if (self.handles.get(final_uri)) |found| {
        return found;
    }

    // New document, read the file then call into openDocument.
    logger.debug("{s} + {s} => {s}", .{ doc.uri, import_str, final_uri });
    var file = std.fs.cwd().openFile(file_path, .{}) catch {
        logger.debug("Cannot open import file {s}", .{file_path});
        return null;
    };
    defer file.close();

    const file_contents = file.readToEndAllocOptions(
        allocator,
        std.math.maxInt(usize),
        null,
        @alignOf(u8),
        0,
    ) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        else => {
            logger.debug("Could not read from file {s}", .{file_path});
            return null;
        },
    };
    errdefer allocator.free(file_contents);

    // Swap handles.
    // This takes ownership of the passed uri and text.
    const duped_final_uri = try allocator.dupe(u8, final_uri);
    errdefer allocator.free(duped_final_uri);
    return try self.newDocument(duped_final_uri, file_contents);
}
