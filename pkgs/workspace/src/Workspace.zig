const std = @import("std");
const lsp = @import("lsp");
const URI = @import("./uri.zig");
const DeclWithHandle = @import("./DeclWithHandle.zig");
const Document = @import("./Document.zig");
const DocumentScope = @import("./DocumentScope.zig");
const BuildFile = @import("./BuildFile.zig");
const Location = @import("./Location.zig");
const DocumentPosition = @import("./DocumentPosition.zig");
const position_context = @import("./position_context.zig");
const ZigEnv = @import("./ZigEnv.zig");
const ast = @import("./ast.zig");
const logger = std.log.scoped(.Workspace);
const TokenLocation = @import("./TokenLocation.zig");

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
        build_file.delete();
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

    doc.import_uris = try doc.collectImportUris(self.zigenv);
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

// fn decrementCount(self: *Self, uri: []const u8) void {
//     if (self.handles.getEntry(uri)) |entry| {
//         const handle = entry.value_ptr.*;
//         if (handle.count > 0) {
//             if (handle.associated_build_file) |build_file| {
//                 self.decrementCount(build_file.uri);
//                 // Remove the build file from the array list
//                 _ = self.build_files.swapRemove(std.mem.indexOfScalar(*BuildFile, self.build_files.items, build_file).?);
//             }

//             if (handle.is_build_file) |build_file| {
//                 self.decrementCount(build_file.uri);
//                 // Remove the build file from the array list
//                 _ = self.build_files.swapRemove(std.mem.indexOfScalar(*BuildFile, self.build_files.items, build_file).?);
//             }

//             for (handle.imports_used.items) |import_uri| {
//                 self.decrementCount(import_uri);
//             }

//             if (handle.decrement() == 0) {
//                 logger.debug("Freeing document: {s}", .{uri});
//             }

//             const uri_key = entry.key_ptr.*;
//             std.debug.assert(self.handles.remove(uri));
//             self.allocator.free(uri_key);
//         }
//     }
// }

// pub fn closeDocument(self: *Self, uri: []const u8) void {
//     self.decrementCount(uri);
// }

pub fn getDocument(self: *Self, uri: []const u8) !*Document {
    if (self.handles.getEntry(uri)) |entry| {
        var doc = entry.value_ptr.*;
        if (doc.count == 0) {
            // remove entry
            const uri_key = entry.key_ptr.*;
            std.debug.assert(self.handles.remove(uri));
            self.allocator.free(uri_key);
        } else {
            return doc;
        }
    }
    return error.NoDocument;
}

pub fn resolveImport(self: *Self, handle: *Document, import_str: []const u8) !?*Document {
    const allocator = self.allocator;
    const final_uri = (try handle.uriFromImportStrAlloc(allocator, import_str, self.zigenv)) orelse return null;
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

    var result_set = DocumentScope.CompletionSet{};
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

pub fn gotoHandler(
    self: *Self,
    arena: *std.heap.ArenaAllocator,
    doc: *Document,
    doc_position: DocumentPosition,
    resolve_alias: bool,
) !?Location {
    // const pos_context = position_context.documentPositionContext(arena, doc_position);
    const pos_context = doc.getPositionContext(doc_position.absolute_index);
    switch (pos_context) {
        .var_access => {
            if (try self.getSymbolGlobal(arena, doc, doc_position.absolute_index)) |decl| {
                return self.gotoDefinitionSymbol(arena, decl, resolve_alias);
            } else {
                return null;
            }
        },
        .field_access => |range| {
            const decl = try DeclWithHandle.getSymbolFieldAccess(arena, self, doc, doc_position.absolute_index, range);
            return self.gotoDefinitionSymbol(arena, decl, resolve_alias);
        },
        .string_literal => {
            return doc.gotoDefinitionString(arena, doc_position.absolute_index, self.zigenv);
        },
        .label => {
            return self.gotoDefinitionLabel(arena, doc, doc_position.absolute_index);
        },
        else => {
            logger.debug("PositionContext.{s} is not implemented", .{@tagName(pos_context)});
            return null;
        },
    }
}

pub fn lookupSymbolGlobal(
    workspace: *Self,
    arena: *std.heap.ArenaAllocator,
    handle: *Document,
    symbol: []const u8,
    source_index: usize,
) error{OutOfMemory}!?DeclWithHandle {
    const innermost_scope_idx = handle.innermostBlockScopeIndex(source_index);

    var curr = innermost_scope_idx;
    while (curr >= 0) : (curr -= 1) {
        const scope = &handle.document_scope.scopes[curr];
        if (source_index >= scope.range.start and source_index <= scope.range.end) blk: {
            if (scope.decls.getEntry(symbol)) |candidate| {
                switch (candidate.value_ptr.*) {
                    .ast_node => |node| {
                        if (handle.tree.nodes.items(.tag)[node].isContainerField()) break :blk;
                    },
                    .label_decl => break :blk,
                    else => {},
                }
                return DeclWithHandle{
                    .decl = candidate.value_ptr,
                    .handle = handle,
                };
            }
            if (try DeclWithHandle.resolveUse(arena, workspace, scope.uses, symbol, handle)) |result| return result;
        }
        if (curr == 0) break;
    }
    return null;
}

pub fn getSymbolGlobal(self: *Self, arena: *std.heap.ArenaAllocator, handle: *Document, pos_index: usize) !?DeclWithHandle {
    if (handle.identifierFromPosition(pos_index)) |name| {
        return self.lookupSymbolGlobal(arena, handle, name, pos_index);
    } else {
        return null;
    }
}

fn gotoDefinitionSymbol(
    workspace: *Self,
    arena: *std.heap.ArenaAllocator,
    decl_handle: DeclWithHandle,
    resolve_alias: bool,
) !?Location {
    var handle = decl_handle.handle;

    const location = switch (decl_handle.decl.*) {
        .ast_node => |node| block: {
            if (resolve_alias) {
                if (try DeclWithHandle.resolveVarDeclAlias(arena, workspace, handle, node)) |result| {
                    handle = result.handle;
                    break :block try result.location();
                }
            }

            const name_token = ast.getDeclNameToken(handle.tree, node) orelse
                return null;
            break :block try TokenLocation.tokenRelativeLocation(handle.tree, 0, handle.tree.tokens.items(.start)[name_token]);
        },
        else => try decl_handle.location(),
    };

    return Location.init(
        arena.allocator(),
        handle.utf8_buffer.uri,
        .{ .row = @intCast(u32, location.line), .col = @intCast(u32, location.column) },
    );
}

fn gotoDefinitionLabel(
    self: *Self,
    arena: *std.heap.ArenaAllocator,
    handle: *Document,
    pos_index: usize,
) !?Location {
    if (try handle.getLabelGlobal(pos_index)) |decl| {
        return self.gotoDefinitionSymbol(arena, decl, false);
    } else {
        return null;
    }
}
