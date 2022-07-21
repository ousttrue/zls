const std = @import("std");
const lsp = @import("lsp");
const URI = @import("./uri.zig");
const ZigEnv = @import("./ZigEnv.zig");
const Ast = std.zig.Ast;
const analysis = @import("./analysis.zig");
const Utf8Buffer = @import("./Utf8Buffer.zig");
const BuildFile = @import("./BuildFile.zig");
const DocumentPosition = @import("./DocumentPosition.zig");
const PositionContext = @import("./position_context.zig").PositionContext;
const AstContext = @import("./AstContext.zig");
const offsets = @import("./offsets.zig");
const Self = @This();
const logger = std.log.scoped(.Document);

allocator: std.mem.Allocator,
utf8_buffer: Utf8Buffer,
count: usize,
/// Contains one entry for every import in the document
import_uris: []const []const u8,
/// Items in this array list come from `import_uris`
imports_used: std.ArrayListUnmanaged([]const u8),
tree: Ast,
ast_context: *AstContext,
document_scope: analysis.DocumentScope,

associated_build_file: ?*BuildFile,
is_build_file: ?*BuildFile,

pub fn new(allocator: std.mem.Allocator, uri: []const u8, text: [:0]u8) !*Self {
    var self = try allocator.create(Self);
    errdefer allocator.destroy(self);

    var tree = try std.zig.parse(allocator, text);
    errdefer tree.deinit(allocator);

    var document_scope = try analysis.makeDocumentScope(allocator, tree);
    errdefer document_scope.deinit(allocator);

    self.* = Self{
        .allocator = allocator,
        .count = 1,
        .import_uris = &.{},
        .imports_used = .{},
        .utf8_buffer = Utf8Buffer.init(uri, text),
        .tree = tree,
        .ast_context = undefined,
        .document_scope = document_scope,
        .associated_build_file = null,
        .is_build_file = null,
    };

    self.ast_context = AstContext.new(allocator, &self.tree);

    return self;
}

pub fn delete(self: *Self) void {
    for (self.import_uris) |imp_uri| {
        self.allocator.free(imp_uri);
    }
    self.allocator.free(self.import_uris);
    self.imports_used.deinit(self.allocator);
    self.document_scope.deinit(self.allocator);
    self.ast_context.delete();
    self.tree.deinit(self.allocator);
    self.allocator.free(self.utf8_buffer.mem);
    self.allocator.destroy(self);
}

pub fn decrement(self: *Self) usize {
    self.count -= 1;
    if (self.count == 0) {
        if (self.associated_build_file) |build_file| {
            build_file.decrement();
        }

        if (self.is_build_file) |build_file| {
            build_file.decrement();
        }

        self.tree.deinit(self.allocator);
        self.allocator.free(self.utf8_buffer.mem);

        for (self.import_uris) |import_uri| {
            self.allocator.free(import_uri);
        }

        self.document_scope.deinit(self.allocator);
        self.imports_used.deinit(self.allocator);
        self.allocator.free(self.import_uris);
        self.allocator.destroy(self);
    }
    return self.count;
}

pub fn getPositionContext(self: Self, byte_pos: usize) PositionContext {
    const token_with_index = self.ast_context.tokenFromBytePos(byte_pos) orelse {
        return .empty;
    };
    const token = token_with_index.token;
    const token_index = token_with_index.index;
    const token_text = self.ast_context.getTokenText(token);
    const node_index = self.ast_context.tokens_node[token_index];
    const tag = self.tree.nodes.items(.tag);
    const node_tag = tag[node_index];
    logger.info("{s}: {s} => {s}", .{ @tagName(token.tag), token_text, @tagName(node_tag) });

    return switch (token_with_index.token.tag) {
        .builtin => .{ .builtin = token.loc },
        .string_literal => .{ .string_literal = token.loc },
        // field_access: SourceRange,
        // var_access: SourceRange,
        // global_error_set,
        // enum_literal,
        // // pre_label,
        // label: bool,
        // // other,
        .keyword_addrspace,
        .keyword_align,
        .keyword_allowzero,
        .keyword_and,
        .keyword_anyframe,
        .keyword_anytype,
        .keyword_asm,
        .keyword_async,
        .keyword_await,
        .keyword_break,
        .keyword_callconv,
        .keyword_catch,
        .keyword_comptime,
        .keyword_const,
        .keyword_continue,
        .keyword_defer,
        .keyword_else,
        .keyword_enum,
        .keyword_errdefer,
        .keyword_error,
        .keyword_export,
        .keyword_extern,
        .keyword_fn,
        .keyword_for,
        .keyword_if,
        .keyword_inline,
        .keyword_noalias,
        .keyword_noinline,
        .keyword_nosuspend,
        .keyword_opaque,
        .keyword_or,
        .keyword_orelse,
        .keyword_packed,
        .keyword_pub,
        .keyword_resume,
        .keyword_return,
        .keyword_linksection,
        .keyword_struct,
        .keyword_suspend,
        .keyword_switch,
        .keyword_test,
        .keyword_threadlocal,
        .keyword_try,
        .keyword_union,
        .keyword_unreachable,
        .keyword_usingnamespace,
        .keyword_var,
        .keyword_volatile,
        .keyword_while,
        => .keyword,

        else => .empty,
    };
}

pub fn uriFromImportStrAlloc(self: *Self, allocator: std.mem.Allocator, import_str: []const u8, zigenv: ZigEnv) !?[]const u8 {
    if (std.mem.eql(u8, import_str, "std")) {
        return try allocator.dupe(u8, zigenv.std_uri);
    } else if (std.mem.eql(u8, import_str, "builtin")) {
        if (self.associated_build_file) |build_file| {
            if (build_file.builtin_uri) |builtin_uri| {
                return try allocator.dupe(u8, builtin_uri);
            }
        }
        return try URI.fromPath(allocator, zigenv.builtin_path);
    } else if (!std.mem.endsWith(u8, import_str, ".zig")) {
        if (self.associated_build_file) |build_file| {
            for (build_file.packages.items) |pkg| {
                if (std.mem.eql(u8, import_str, pkg.name)) {
                    return try allocator.dupe(u8, pkg.uri);
                }
            }
        }
        return null;
    } else {
        const base = self.utf8_buffer.uri;
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

pub fn collectImportUris(self: *Self, zigenv: ZigEnv) ![]const []const u8 {
    var new_imports = std.ArrayList([]const u8).init(self.allocator);
    errdefer {
        for (new_imports.items) |imp| {
            self.allocator.free(imp);
        }
        new_imports.deinit();
    }
    try analysis.collectImports(&new_imports, self.tree);

    // Convert to URIs
    var i: usize = 0;
    while (i < new_imports.items.len) {
        if (try self.uriFromImportStrAlloc(self.allocator, new_imports.items[i], zigenv)) |uri| {
            // The raw import strings are owned by the document and do not need to be freed here.
            new_imports.items[i] = uri;
            i += 1;
        } else {
            _ = new_imports.swapRemove(i);
        }
    }
    return new_imports.toOwnedSlice();
}

fn refreshDocument(self: *Self, zigenv: ZigEnv) !void {
    logger.debug("New text for document {s}", .{self.utf8_buffer.uri});
    self.tree.deinit(self.allocator);
    self.tree = try std.zig.parse(self.allocator, self.utf8_buffer.text);

    self.document_scope.deinit(self.allocator);
    self.document_scope = try analysis.makeDocumentScope(self.allocator, self.tree);

    const new_imports = try self.collectImportUris(zigenv);
    errdefer {
        for (new_imports) |imp| {
            self.allocator.free(imp);
        }
        self.allocator.free(new_imports);
    }

    const old_imports = self.import_uris;
    self.import_uris = new_imports;
    defer {
        for (old_imports) |uri| {
            self.allocator.free(uri);
        }
        self.allocator.free(old_imports);
    }

    var i: usize = 0;
    while (i < self.imports_used.items.len) {
        const old = self.imports_used.items[i];
        still_exists: {
            for (new_imports) |new_import| {
                if (std.mem.eql(u8, new_import, old)) {
                    self.imports_used.items[i] = new_import;
                    break :still_exists;
                }
            }
            logger.debug("Import removed: {s}", .{old});
            // self.decrementCount(old);
            _ = self.imports_used.swapRemove(i);
            continue;
        }
        i += 1;
    }
}

pub fn applyChanges(self: *Self, content_changes: std.json.Array, offset_encoding: offsets.Encoding, zigenv: ZigEnv) !void {
    const document = &self.utf8_buffer;

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

    try self.refreshDocument(zigenv);
}
