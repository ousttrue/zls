const std = @import("std");
const lsp = @import("lsp");
const URI = @import("./uri.zig");
const analysis = @import("./analysis.zig");
const offsets = @import("./offsets.zig");
const Document = @import("./Document.zig");
const Utf8Buffer = @import("./Utf8Buffer.zig");
const BuildFile = @import("./BuildFile.zig");
const ZigEnv = @import("./ZigEnv.zig");
const logger = std.log.scoped(.Workspace);

const Self = @This();

allocator: std.mem.Allocator,
zigenv: ZigEnv,
handles: std.StringHashMap(*Document),
build_files: std.ArrayListUnmanaged(*BuildFile),

pub fn init(
    allocator: std.mem.Allocator,
    zigenv: ZigEnv,
) Self {
    return Self{
        .allocator = allocator,
        .zigenv = zigenv,
        .handles = std.StringHashMap(*Document).init(allocator),
        .build_files = .{},
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
        for (build_file.packages.items) |pkg| {
            self.allocator.free(pkg.name);
            self.allocator.free(pkg.uri);
        }
        build_file.packages.deinit(self.allocator);
        self.allocator.free(build_file.uri);
        build_file.destroy(self.allocator);
    }
    self.build_files.deinit(self.allocator);
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
        // if (candidate) |build_file| {
        //     logger.debug("Found a candidate associated build file: `{s}`", .{build_file.uri});
        // }
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

    // TODO: Better logic for detecting std or subdirectories?
    if (std.mem.indexOf(u8, uri, "/std/") != null) {
        // in std
    } else if (std.mem.endsWith(u8, uri, "/build.zig")) {
        var build_file = try BuildFile.extractPackages(self.allocator, uri, self.zigenv);
        try self.build_files.append(self.allocator, build_file);
        doc.is_build_file = build_file;
    } else {
        if (try self.findBuildFile(uri)) |build_file| {
            build_file.refs += 1;
            doc.associated_build_file = build_file;
            // logger.debug("Associated build file `{s}` to document `{s}`", .{ build_file.uri, handle.uri() });
        }
    }

    doc.import_uris = try self.collectImportUris(doc);
    try self.handles.putNoClobber(uri, doc);
    return doc;
}

pub fn openDocument(self: *Self, uri: []const u8, text: []const u8) !*Document {
    if (self.handles.getEntry(uri)) |entry| {
        // logger.debug("Document already open: {s}, incrementing count", .{uri});
        entry.value_ptr.*.count += 1;
        if (entry.value_ptr.*.is_build_file) |build_file| {
            build_file.refs += 1;
        }
        // logger.debug("New count: {}", .{entry.value_ptr.*.count});
        return entry.value_ptr.*;
    }

    const duped_text = try self.allocator.dupeZ(u8, text);
    errdefer self.allocator.free(duped_text);
    const duped_uri = try self.allocator.dupeZ(u8, uri);
    errdefer self.allocator.free(duped_uri);

    return try self.newDocument(duped_uri, duped_text);
}

fn decrementBuildFileRefs(self: *Self, build_file: *BuildFile) void {
    build_file.refs -= 1;
    if (build_file.refs == 0) {
        logger.debug("Freeing build file {s}", .{build_file.uri});
        for (build_file.packages.items) |pkg| {
            self.allocator.free(pkg.name);
            self.allocator.free(pkg.uri);
        }
        build_file.packages.deinit(self.allocator);

        // Decrement count of the document since one count comes
        // from the build file existing.
        self.decrementCount(build_file.uri);
        self.allocator.free(build_file.uri);

        // Remove the build file from the array list
        _ = self.build_files.swapRemove(std.mem.indexOfScalar(*BuildFile, self.build_files.items, build_file).?);
        build_file.destroy(self.allocator);
    }
}

fn decrementCount(self: *Self, uri: []const u8) void {
    if (self.handles.getEntry(uri)) |entry| {
        const handle = entry.value_ptr.*;
        if (handle.count == 0) return;
        handle.count -= 1;

        if (handle.count > 0)
            return;

        logger.debug("Freeing document: {s}", .{uri});

        if (handle.associated_build_file) |build_file| {
            self.decrementBuildFileRefs(build_file);
        }

        if (handle.is_build_file) |build_file| {
            self.decrementBuildFileRefs(build_file);
        }

        handle.tree.deinit(self.allocator);
        self.allocator.free(handle.utf8_buffer.mem);

        for (handle.imports_used.items) |import_uri| {
            self.decrementCount(import_uri);
        }

        for (handle.import_uris) |import_uri| {
            self.allocator.free(import_uri);
        }

        handle.document_scope.deinit(self.allocator);
        handle.imports_used.deinit(self.allocator);
        self.allocator.free(handle.import_uris);
        self.allocator.destroy(handle);
        const uri_key = entry.key_ptr.*;
        std.debug.assert(self.handles.remove(uri));
        self.allocator.free(uri_key);
    }
}

pub fn closeDocument(self: *Self, uri: []const u8) void {
    self.decrementCount(uri);
}

pub fn getDocument(self: *Self, uri: []const u8) !*Document {
    return self.handles.get(uri) orelse return error.NoDocument;
}

fn collectImportUris(self: *Self, handle: *Document) ![]const []const u8 {
    var new_imports = std.ArrayList([]const u8).init(self.allocator);
    errdefer {
        for (new_imports.items) |imp| {
            self.allocator.free(imp);
        }
        new_imports.deinit();
    }
    try analysis.collectImports(&new_imports, handle.tree);

    // Convert to URIs
    var i: usize = 0;
    while (i < new_imports.items.len) {
        if (try self.uriFromImportStr(self.allocator, handle.*, new_imports.items[i])) |uri| {
            // The raw import strings are owned by the document and do not need to be freed here.
            new_imports.items[i] = uri;
            i += 1;
        } else {
            _ = new_imports.swapRemove(i);
        }
    }
    return new_imports.toOwnedSlice();
}

fn refreshDocument(self: *Self, handle: *Document) !void {
    logger.debug("New text for document {s}", .{handle.utf8_buffer.uri});
    handle.tree.deinit(self.allocator);
    handle.tree = try std.zig.parse(self.allocator, handle.utf8_buffer.text);

    handle.document_scope.deinit(self.allocator);
    handle.document_scope = try analysis.makeDocumentScope(self.allocator, handle.tree);

    const new_imports = try self.collectImportUris(handle);
    errdefer {
        for (new_imports) |imp| {
            self.allocator.free(imp);
        }
        self.allocator.free(new_imports);
    }

    const old_imports = handle.import_uris;
    handle.import_uris = new_imports;
    defer {
        for (old_imports) |uri| {
            self.allocator.free(uri);
        }
        self.allocator.free(old_imports);
    }

    var i: usize = 0;
    while (i < handle.imports_used.items.len) {
        const old = handle.imports_used.items[i];
        still_exists: {
            for (new_imports) |new| {
                if (std.mem.eql(u8, new, old)) {
                    handle.imports_used.items[i] = new;
                    break :still_exists;
                }
            }
            logger.debug("Import removed: {s}", .{old});
            self.decrementCount(old);
            _ = handle.imports_used.swapRemove(i);
            continue;
        }
        i += 1;
    }
}

pub fn applySave(self: *Self, handle: *Document) !void {
    if (handle.is_build_file) |build_file| {
        build_file.loadPackages(self.allocator, null, self.zigenv) catch |err| {
            logger.debug("Failed to load packages of build file {s} (error: {})", .{ build_file.uri, err });
        };
    }
}

pub fn applyChanges(self: *Self, handle: *Document, content_changes: std.json.Array, offset_encoding: offsets.Encoding) !void {
    const document = &handle.utf8_buffer;

    for (content_changes.items) |change| {
        if (change.Object.get("range")) |range| {
            std.debug.assert(@ptrCast([*]const u8, document.text.ptr) == document.mem.ptr);

            // TODO: add tests and validate the JSON
            const start_obj = range.Object.get("start").?.Object;
            const start_pos = lsp.Position{
                .line = start_obj.get("line").?.Integer,
                .character = start_obj.get("character").?.Integer,
            };
            const end_obj = range.Object.get("end").?.Object;
            const end_pos = lsp.Position{
                .line = end_obj.get("line").?.Integer,
                .character = end_obj.get("character").?.Integer,
            };

            const change_text = change.Object.get("text").?.String;
            const start_index = (try offsets.documentPosition(document.*, start_pos, offset_encoding)).absolute_index;
            const end_index = (try offsets.documentPosition(document.*, end_pos, offset_encoding)).absolute_index;

            const old_len = document.text.len;
            const new_len = old_len - (end_index - start_index) + change_text.len;
            if (new_len >= document.mem.len) {
                // We need to reallocate memory.
                // We reallocate twice the current filesize or the new length, if it's more than that
                // so that we can reduce the amount of realloc calls.
                // We can tune this to find a better size if needed.
                const realloc_len = std.math.max(2 * old_len, new_len + 1);
                document.mem = try self.allocator.realloc(document.mem, realloc_len);
            }

            // The first part of the string, [0 .. start_index] need not be changed.
            // We then copy the last part of the string, [end_index ..] to its
            //    new position, [start_index + change_len .. ]
            if (new_len < old_len) {
                std.mem.copy(u8, document.mem[start_index + change_text.len ..][0 .. old_len - end_index], document.mem[end_index..old_len]);
            } else {
                std.mem.copyBackwards(u8, document.mem[start_index + change_text.len ..][0 .. old_len - end_index], document.mem[end_index..old_len]);
            }
            // Finally, we copy the changes over.
            std.mem.copy(u8, document.mem[start_index..][0..change_text.len], change_text);

            // Reset the text substring.
            document.mem[new_len] = 0;
            document.text = document.mem[0..new_len :0];
        } else {
            const change_text = change.Object.get("text").?.String;
            const old_len = document.text.len;

            if (change_text.len >= document.mem.len) {
                // Like above.
                const realloc_len = std.math.max(2 * old_len, change_text.len + 1);
                document.mem = try self.allocator.realloc(document.mem, realloc_len);
            }

            std.mem.copy(u8, document.mem[0..change_text.len], change_text);
            document.mem[change_text.len] = 0;
            document.text = document.mem[0..change_text.len :0];
        }
    }

    try self.refreshDocument(handle);
}

pub fn uriFromImportStr(self: *Self, allocator: std.mem.Allocator, handle: Document, import_str: []const u8) !?[]const u8 {
    if (std.mem.eql(u8, import_str, "std")) {
        return try allocator.dupe(u8, self.zigenv.std_uri);
    } else if (std.mem.eql(u8, import_str, "builtin")) {
        if (handle.associated_build_file) |build_file| {
            if (build_file.builtin_uri) |builtin_uri| {
                return try allocator.dupe(u8, builtin_uri);
            }
        }
        return try URI.fromPath(allocator, self.zigenv.builtin_path);
    } else if (!std.mem.endsWith(u8, import_str, ".zig")) {
        if (handle.associated_build_file) |build_file| {
            for (build_file.packages.items) |pkg| {
                if (std.mem.eql(u8, import_str, pkg.name)) {
                    return try allocator.dupe(u8, pkg.uri);
                }
            }
        }
        return null;
    } else {
        const base = handle.utf8_buffer.uri;
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

pub fn resolveImport(self: *Self, handle: *Document, import_str: []const u8) !?*Document {
    const allocator = self.allocator;
    const final_uri = (try self.uriFromImportStr(
        self.allocator,
        handle.*,
        import_str,
    )) orelse return null;
    defer allocator.free(final_uri);

    for (handle.imports_used.items) |uri| {
        if (std.mem.eql(u8, uri, final_uri)) {
            return self.getDocument(final_uri);
        }
    }
    // The URI must be somewhere in the import_uris or the package uris
    const handle_uri = find_uri: {
        for (handle.import_uris) |uri| {
            if (std.mem.eql(u8, uri, final_uri)) {
                break :find_uri uri;
            }
        }
        if (handle.associated_build_file) |bf| {
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
        try handle.imports_used.append(self.allocator, handle_uri);
        new_handle.count += 1;
        return new_handle;
    } else |_| {
        //
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
        try handle.imports_used.append(self.allocator, handle_uri);
        // Swap handles.
        // This takes ownership of the passed uri and text.
        const duped_final_uri = try allocator.dupe(u8, final_uri);
        errdefer allocator.free(duped_final_uri);
        return try self.newDocument(duped_final_uri, file_contents);
    }
}

fn tagStoreCompletionItems(self: Self, arena: *std.heap.ArenaAllocator, base: *Self.Document, comptime name: []const u8) ![]lsp.CompletionItem {
    // TODO Better solution for deciding what tags to include
    var max_len: usize = @field(base.document_scope, name).count();
    for (base.imports_used.items) |uri| {
        max_len += @field(self.handles.get(uri).?.document_scope, name).count();
    }

    var result_set = analysis.CompletionSet{};
    try result_set.ensureTotalCapacity(arena.allocator(), max_len);
    for (@field(base.document_scope, name).entries.items(.key)) |completion| {
        result_set.putAssumeCapacityNoClobber(completion, {});
    }

    for (base.imports_used.items) |uri| {
        const curr_set = &@field(self.handles.get(uri).?.document_scope, name);
        for (curr_set.entries.items(.key)) |completion| {
            result_set.putAssumeCapacity(completion, {});
        }
    }
    return result_set.entries.items(.key);
}

pub fn errorCompletionItems(self: Self, arena: *std.heap.ArenaAllocator, base: *Self.Document) ![]lsp.CompletionItem {
    return try self.tagStoreCompletionItems(arena, base, "error_completions");
}

pub fn enumCompletionItems(self: Self, arena: *std.heap.ArenaAllocator, base: *Self.Document) ![]lsp.CompletionItem {
    return try self.tagStoreCompletionItems(arena, base, "enum_completions");
}
