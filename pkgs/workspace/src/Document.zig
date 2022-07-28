const std = @import("std");
const URI = @import("./uri.zig");
const ZigEnv = @import("./ZigEnv.zig");
const Ast = std.zig.Ast;
const DocumentScope = @import("./DocumentScope.zig");
const Line = @import("./Line.zig");
const DeclWithHandle = @import("./DeclWithHandle.zig");
const ast = @import("./ast.zig");
const Utf8Buffer = @import("./Utf8Buffer.zig");
const BuildFile = @import("./BuildFile.zig");
const AstContext = @import("./AstContext.zig");
const LinePosition = @import("./LinePosition.zig");
const UriBytePosition = @import("./UriBytePosition.zig");
const logger = std.log.scoped(.Document);

pub const PositionContext = union(enum) {
    builtin: std.zig.Token.Loc,
    string_literal: std.zig.Token.Loc,
    field_access: std.zig.Token.Loc,
    var_access: std.zig.Token.Loc,
    global_error_set,
    enum_literal,
    // pre_label,
    label: bool,
    // other,
    keyword,
    empty,
};

const Self = @This();
allocator: std.mem.Allocator,
utf8_buffer: Utf8Buffer,
count: usize,
/// Contains one entry for every import in the document
import_uris: []const []const u8,
/// Items in this array list come from `import_uris`
imports_used: std.ArrayListUnmanaged([]const u8),
tree: Ast,
ast_context: *AstContext,
document_scope: DocumentScope,
line_position: LinePosition,

associated_build_file: ?*BuildFile,
is_build_file: ?*BuildFile,

pub fn new(allocator: std.mem.Allocator, uri: []const u8, text: [:0]u8) !*Self {
    var self = try allocator.create(Self);
    errdefer allocator.destroy(self);

    var tree = try std.zig.parse(allocator, text);
    errdefer tree.deinit(allocator);

    self.* = Self{
        .allocator = allocator,
        .count = 1,
        .import_uris = &.{},
        .imports_used = .{},
        .utf8_buffer = Utf8Buffer.init(uri, text),
        .tree = tree,
        .ast_context = AstContext.new(allocator, &self.tree),
        .document_scope = try DocumentScope.init(allocator, tree),
        .associated_build_file = null,
        .is_build_file = null,
        .line_position = try LinePosition.init(allocator, text),
    };

    return self;
}

pub fn delete(self: *Self) void {
    self.line_position.deinit();
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

fn getPositionContextFromIdentifier(self: Self, token_idx: u32, node_idx: u32, node_tag: Ast.Node.Tag) PositionContext {
    _ = token_idx;
    return switch (node_tag) {
        .field_access => {
            const first = self.ast_context.tokens.items[self.tree.firstToken(node_idx)];
            const last = self.ast_context.tokens.items[self.tree.lastToken(node_idx)];
            return .{ .field_access = .{ .start = first.loc.start, .end = last.loc.end } };
        },
        .enum_literal => {
            return .enum_literal;
        },
        else => {
            const token = self.ast_context.tokens.items[token_idx];
            return .{ .var_access = token.loc };
        },
    };
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
        .identifier => self.getPositionContextFromIdentifier(token_index, node_index, node_tag),
        .period => .{ .field_access = token.loc },
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

/// Collects all imports we can find into a slice of import paths (without quotes).
pub fn collectImports(import_arr: *std.ArrayList([]const u8), tree: Ast) !void {
    const tags = tree.tokens.items(.tag);

    var i: usize = 0;
    while (i < tags.len) : (i += 1) {
        if (tags[i] != .builtin)
            continue;
        const text = tree.tokenSlice(@intCast(u32, i));

        if (std.mem.eql(u8, text, "@import")) {
            if (i + 3 >= tags.len)
                break;
            if (tags[i + 1] != .l_paren)
                continue;
            if (tags[i + 2] != .string_literal)
                continue;
            if (tags[i + 3] != .r_paren)
                continue;

            const str = tree.tokenSlice(@intCast(u32, i + 2));
            try import_arr.append(str[1 .. str.len - 1]);
        }
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
    try collectImports(&new_imports, self.tree);

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

pub fn refreshDocument(self: *Self, zigenv: ZigEnv) !void {
    // logger.debug("New text for document {s}", .{self.utf8_buffer.uri});
    self.tree.deinit(self.allocator);
    self.tree = try std.zig.parse(self.allocator, self.utf8_buffer.text);

    self.ast_context.delete();
    self.ast_context = AstContext.new(self.allocator, &self.tree);

    self.line_position.deinit();
    self.line_position = try LinePosition.init(self.allocator, self.utf8_buffer.text);

    self.document_scope.deinit(self.allocator);
    self.document_scope = try DocumentScope.init(self.allocator, self.tree);

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

pub fn applyChanges(self: *Self, content_changes: std.json.Array, encoding: Line.Encoding, zigenv: ZigEnv) !void {
    const document = &self.utf8_buffer;

    for (content_changes.items) |change| {
        if (change.Object.get("range")) |range| {
            std.debug.assert(@ptrCast([*]const u8, document.text.ptr) == document.mem.ptr);

            // TODO: add tests and validate the JSON
            const start_obj = range.Object.get("start").?;
            const end_obj = range.Object.get("end").?;

            const change_text = change.Object.get("text").?.String;
            const start_line = try self.line_position.getLine(@intCast(u32, start_obj.Object.get("line").?.Integer));
            const start_index = try start_line.getBytePosition(@intCast(u32, start_obj.Object.get("character").?.Integer), encoding);
            const end_line = try self.line_position.getLine(@intCast(u32, end_obj.Object.get("line").?.Integer));
            const end_index = try end_line.getBytePosition(@intCast(u32, end_obj.Object.get("character").?.Integer), encoding);

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

pub fn applySave(self: *Self, zigenv: ZigEnv) !void {
    if (self.is_build_file) |build_file| {
        build_file.loadPackages(self.allocator, null, zigenv) catch |err| {
            logger.debug("Failed to load packages of build file {s} (error: {})", .{ build_file.uri, err });
        };
    }
}

const ImportStrIterator = struct {
    const Payload = union(enum) {
        decl: Ast.Node.Index,
        decls: []const Ast.Node.Index,
    };
    const Item = struct {
        payload: Payload,
        pos: usize = 0,

        fn node(self: Item) Ast.Node.Index {
            return switch (self.payload) {
                .decls => |decls| return decls[self.pos],
                .decl => |decl| return decl,
            };
        }

        fn next(self: Item) ?Item {
            switch (self.payload) {
                .decls => |decls| {
                    if (self.pos + 1 < decls.len) {
                        return Item{ .payload = .{ .decls = decls }, .pos = self.pos + 1 };
                    }
                    return null;
                },
                .decl => return null,
            }
        }
    };

    tree: Ast,
    stack: [256]Item = undefined,
    depth: usize,

    pub fn init(tree: Ast) @This() {
        var self = @This(){
            .tree = tree,
            .depth = 0,
        };
        self.push(.{ .payload = .{ .decl = 0 } });
        return self;
    }

    fn push(self: *@This(), item: Item) void {
        self.stack[self.depth] = item;
        self.depth += 1;
    }

    fn pop(self: *@This()) Item {
        const node = self.stack[self.depth - 1];
        self.depth -= 1;
        return node;
    }

    pub fn next(self: *@This()) ?u32 {
        const node_tags = self.tree.nodes.items(.tag);
        var buf: [2]Ast.Node.Index = undefined;
        while (self.depth > 0) {
            const item = self.pop();
            if (item.next()) |next_item| {
                self.push(next_item);
            }

            const node = item.node();

            if (ast.isContainer(self.tree, node)) {
                const decls = ast.declMembers(self.tree, node, &buf);
                if (decls.len > 0) {
                    self.push(.{ .payload = .{ .decls = decls } });
                }
            } else if (ast.varDecl(self.tree, node)) |var_decl| {
                self.push(.{ .payload = .{ .decl = var_decl.ast.init_node } });
            } else if (node_tags[node] == .@"usingnamespace") {
                self.push(.{ .payload = .{ .decl = self.tree.nodes.items(.data)[node].lhs } });
            }

            if (ast.isBuiltinCall(self.tree, node)) {
                const builtin_token = self.tree.nodes.items(.main_token)[node];
                const call_name = self.tree.tokenSlice(builtin_token);
                if (std.mem.eql(u8, call_name, "@import")) {
                    return node;
                }
            }
        }

        return null;
    }
};

fn nodeContainsSourceIndex(tree: Ast, node: Ast.Node.Index, source_index: usize) bool {
    const first_token = ast.tokenLocation(tree, tree.firstToken(node)).start;
    const last_token = ast.tokenLocation(tree, ast.lastToken(tree, node)).end;
    return source_index >= first_token and source_index <= last_token;
}

fn importStr(tree: std.zig.Ast, node: usize) ?[]const u8 {
    const node_tags = tree.nodes.items(.tag);
    const data = tree.nodes.items(.data)[node];
    const params = switch (node_tags[node]) {
        .builtin_call, .builtin_call_comma => tree.extra_data[data.lhs..data.rhs],
        .builtin_call_two, .builtin_call_two_comma => if (data.lhs == 0)
            &[_]Ast.Node.Index{}
        else if (data.rhs == 0)
            &[_]Ast.Node.Index{data.lhs}
        else
            &[_]Ast.Node.Index{ data.lhs, data.rhs },
        else => unreachable,
    };

    if (params.len != 1) return null;

    const import_str = tree.tokenSlice(tree.nodes.items(.main_token)[params[0]]);
    return import_str[1 .. import_str.len - 1];
}

pub fn gotoDefinitionString(
    handle: *Self,
    arena: *std.heap.ArenaAllocator,
    pos_index: usize,
    zigenv: ZigEnv,
) !?UriBytePosition {
    var it = ImportStrIterator.init(handle.tree);
    while (it.next()) |node| {
        if (nodeContainsSourceIndex(handle.tree, node, pos_index)) {
            if (importStr(handle.tree, node)) |import_str| {
                if (try handle.uriFromImportStrAlloc(arena.allocator(), import_str, zigenv)) |uri| {
                    logger.debug("gotoDefinitionString: {s}", .{uri});
                    return UriBytePosition{ .uri = uri, .loc = .{ .start = 0, .end = 0 } };
                }
            }
        }
    }
    return null;
}

fn isSymbolChar(char: u8) bool {
    return std.ascii.isAlNum(char) or char == '_';
}

pub fn identifierFromPosition(self: Self, pos_index: usize) ?[]const u8 {
    const text: []const u8 = self.utf8_buffer.text;
    if (pos_index + 1 >= text.len) {
        return null;
    }
    if (!isSymbolChar(text[pos_index])) {
        return null;
    }

    var start_idx = pos_index;
    while (start_idx >= 0) : (start_idx -= 1) {
        if (!isSymbolChar(text[start_idx])) {
            start_idx += 1;
            break;
        }
    }

    var end_idx = pos_index;
    while (end_idx < text.len and isSymbolChar(text[end_idx])) {
        end_idx += 1;
    }

    const id = text[start_idx..end_idx];
    // std.debug.print("{}, {}:{s}", .{start_idx, end_idx, id});
    return id;
}

test "identifierFromPosition" {
    try std.testing.expectEqualStrings("abc", try identifierFromPosition(1, " abc cde"));
    try std.testing.expectEqualStrings("abc", try identifierFromPosition(2, " abc cde"));
    // try std.testing.expectEqualStrings("", try identifierFromPosition(3, "abc cde"));
}

pub fn lookupLabel(handle: *Self, symbol: []const u8, source_index: usize) error{OutOfMemory}!?DeclWithHandle {
    for (handle.document_scope.scopes) |scope| {
        if (source_index >= scope.range.start and source_index < scope.range.end) {
            if (scope.decls.getEntry(symbol)) |candidate| {
                switch (candidate.value_ptr.*) {
                    .label_decl => {},
                    else => continue,
                }
                return DeclWithHandle{
                    .decl = candidate.value_ptr,
                    .handle = handle,
                };
            }
        }
        if (scope.range.start > source_index) return null;
    }
    return null;
}

pub fn getLabelGlobal(self: *Self, pos_index: usize) !?DeclWithHandle {
    if (self.identifierFromPosition(pos_index)) |name| {
        return try lookupLabel(self, name, pos_index);
    } else {
        return null;
    }
}

pub fn innermostBlockScopeIndex(handle: Self, source_index: usize) usize {
    if (handle.document_scope.scopes.len == 1) return 0;

    var current: usize = 0;
    for (handle.document_scope.scopes[1..]) |*scope, idx| {
        if (source_index >= scope.range.start and source_index <= scope.range.end) {
            switch (scope.data) {
                .container, .function, .block => current = idx + 1,
                else => {},
            }
        }
        if (scope.range.start > source_index) break;
    }
    return current;
}

pub fn innermostBlockScope(self: Self, source_index: usize) Ast.Node.Index {
    return self.document_scope.scopes[self.innermostBlockScopeIndex(source_index)].toNodeIndex().?;
}

pub fn tokenReference(self: Self, token_idx: Ast.TokenIndex) UriBytePosition {
    const token = self.ast_context.tokens.items[token_idx];
    return UriBytePosition
    {
        .uri = self.utf8_buffer.uri,
        .loc = token.loc,
    };
}
