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
build_files: std.ArrayList(*BuildFile),

pub fn init(
    allocator: std.mem.Allocator,
    zigenv: ZigEnv,
    root: FixedPath,
) Self {
    logger.info("{s}", .{root.slice()});
    return Self{
        .allocator = allocator,
        .zigenv = zigenv,
        .root = root,
        .handles = std.StringHashMap(*Document).init(allocator),
        .build_files = std.ArrayList(*BuildFile).init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    var entry_iterator = self.handles.iterator();
    while (entry_iterator.next()) |entry| {
        self.allocator.free(entry.key_ptr.*);
        entry.value_ptr.*.delete();
    }
    self.handles.deinit();

    for (self.build_files.items) |build_file| {
        build_file.delete();
    }
    self.build_files.deinit();
}

fn findBuildFile(self: *Self, uri: []const u8) !?*BuildFile {
    // Look into build files and keep the one that lives closest to the document in the directory structure
    var candidate: ?*BuildFile = null;
    {
        var uri_chars_matched: usize = 0;
        for (self.build_files.items) |build_file| {
            const build_file_base_uri = build_file.uri[0 .. std.mem.lastIndexOfScalar(u8, build_file.uri, '/').? + 1];

            if (build_file_base_uri.len > uri_chars_matched and std.mem.startsWith(u8, uri, build_file_base_uri)) {
                uri_chars_matched = build_file_base_uri.len;
                candidate = build_file;
            }
        }
    }

    // Then, try to find the closest build file.
    var curr_path = try URI.parse(self.allocator, uri);
    defer self.allocator.free(curr_path);
    while (true) {
        if (curr_path.len == 0) break;

        if (std.mem.lastIndexOfScalar(u8, curr_path[0 .. curr_path.len - 1], std.fs.path.sep)) |idx| {
            // This includes the last separator
            curr_path = curr_path[0 .. idx + 1];

            // Try to open the folder, then the file.
            var folder = std.fs.cwd().openDir(curr_path, .{}) catch |err| switch (err) {
                error.FileNotFound => continue,
                else => return err,
            };
            defer folder.close();

            var build_file = folder.openFile("build.zig", .{}) catch |err| switch (err) {
                error.FileNotFound, error.AccessDenied => continue,
                else => return err,
            };
            defer build_file.close();

            // Calculate build file's URI
            var candidate_path = try std.mem.concat(self.allocator, u8, &.{ curr_path, "build.zig" });
            defer self.allocator.free(candidate_path);
            const build_file_uri = try URI.fromPath(self.allocator, candidate_path);
            errdefer self.allocator.free(build_file_uri);

            if (candidate) |candidate_build_file| {
                // Check if it is the same as the current candidate we got from the existing build files.
                // If it isn't, we need to read the file and make a new build file.
                if (std.mem.eql(u8, candidate_build_file.uri, build_file_uri)) {
                    self.allocator.free(build_file_uri);
                    break;
                }
            }

            // Check if the build file already exists
            if (self.handles.get(build_file_uri)) |build_file_handle| {
                candidate = build_file_handle.is_build_file.?;
                break;
            }

            // Read the build file, create a new document, set the candidate to the new build file.
            const build_file_text = try build_file.readToEndAllocOptions(
                self.allocator,
                std.math.maxInt(usize),
                null,
                @alignOf(u8),
                0,
            );
            errdefer self.allocator.free(build_file_text);

            const build_file_handle = try self.newDocument(build_file_uri, build_file_text);
            candidate = build_file_handle.is_build_file.?;
            break;
        } else break;
    }

    return candidate;
}

/// This function asserts the document is not open yet and takes ownership
/// of the uri and text passed in.
fn newDocument(self: *Self, uri: []const u8, text: [:0]u8) anyerror!*Document {
    const doc = try Document.new(self.allocator, uri, text);
    errdefer doc.delete();
    doc.import_uris = try doc.collectImportUris(self.zigenv);

    // TODO: Better logic for detecting std or subdirectories?
    if (std.mem.indexOf(u8, uri, "/std/") != null) {
        // in std
    } else if (std.mem.endsWith(u8, uri, "/build.zig")) {
        var build_file = try BuildFile.extractPackages(self.allocator, uri, self.zigenv);
        try self.build_files.append(build_file);
        doc.is_build_file = build_file;
    } else {
        if (try self.findBuildFile(uri)) |build_file| {
            build_file.refs += 1;
            doc.associated_build_file = build_file;
        }
    }

    try self.handles.putNoClobber(uri, doc);
    return doc;
}

pub fn openDocument(self: *Self, uri: []const u8, text: []const u8) !*Document {
    if (self.handles.getEntry(uri)) |entry| {
        try entry.value_ptr.*.refreshDocument(self.zigenv);
        if (entry.value_ptr.*.is_build_file) |build_file| {
            build_file.refs += 1;
        }
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
    }
    return null;
}

pub fn resolveImport(self: *Self, doc: *Document, import_str: []const u8) !?*Document {
    const allocator = self.allocator;
    const final_uri = (try doc.uriFromImportStrAlloc(allocator, import_str, self.zigenv)) orelse return null;
    defer allocator.free(final_uri);

    for (doc.imports_used.items) |uri| {
        if (std.mem.eql(u8, uri, final_uri)) {
            if (self.getDocument(final_uri)) |found| {
                return found;
            }
        }
    }

    // The URI must be somewhere in the import_uris or the package uris
    const handle_uri = find_uri: {
        for (doc.import_uris) |uri| {
            if (std.mem.eql(u8, uri, final_uri)) {
                break :find_uri uri;
            }
        }
        if (doc.associated_build_file) |bf| {
            for (bf.packages.items) |pkg| {
                if (std.mem.eql(u8, pkg.uri, final_uri)) {
                    break :find_uri pkg.uri;
                }
            }
        }
        return null;
    };

    // New import.
    // Check if the import is already opened by others.
    if (self.getDocument(final_uri)) |new_handle| {
        // If it is, append it to our imports, increment the count, set our new handle
        // and return the parsed tree root node.
        try doc.imports_used.append(self.allocator, handle_uri);
        return new_handle;
    }

    // New document, read the file then call into openDocument.
    const file_path = try URI.parse(allocator, final_uri);
    defer allocator.free(file_path);

    var file = std.fs.cwd().openFile(file_path, .{}) catch {
        logger.debug("Cannot open import file {s}", .{file_path});
        return null;
    };

    defer file.close();
    {
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

        // Add to import table of current handle.
        try doc.imports_used.append(self.allocator, handle_uri);
        // Swap handles.
        // This takes ownership of the passed uri and text.
        const duped_final_uri = try allocator.dupe(u8, final_uri);
        errdefer allocator.free(duped_final_uri);
        return try self.newDocument(duped_final_uri, file_contents);
    }
}
