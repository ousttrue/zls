const std = @import("std");
const URI = @import("./uri.zig");
const ZigEnv = @import("./ZigEnv.zig");
const Ast = std.zig.Ast;
const Line = @import("./Line.zig");
const ast = @import("./ast.zig");
const Utf8Buffer = @import("./Utf8Buffer.zig");
const BuildFile = @import("./BuildFile.zig");
const AstContext = @import("./AstContext.zig");
const UriBytePosition = @import("./UriBytePosition.zig");
const logger = std.log.scoped(.Document);

const Self = @This();
allocator: std.mem.Allocator,
uri: []const u8,
utf8_buffer: Utf8Buffer,
ast_context: *AstContext,
/// Contains one entry for every import in the document
import_uris: []const []const u8,
/// Items in this array list come from `import_uris`
imports_used: std.ArrayListUnmanaged([]const u8),
associated_build_file: ?*BuildFile,
is_build_file: ?*BuildFile,

pub fn new(allocator: std.mem.Allocator, uri: []const u8, text: [:0]u8) !*Self {
    var self = try allocator.create(Self);
    errdefer allocator.destroy(self);
    self.* = Self{
        .allocator = allocator,
        .uri = uri,
        .import_uris = &.{},
        .imports_used = .{},
        .utf8_buffer = try Utf8Buffer.init(allocator, text),
        .ast_context = try AstContext.new(allocator, text),
        .associated_build_file = null,
        .is_build_file = null,
    };
    return self;
}

pub fn delete(self: *Self) void {
    for (self.import_uris) |imp_uri| {
        self.allocator.free(imp_uri);
    }
    self.allocator.free(self.import_uris);
    self.imports_used.deinit(self.allocator);
    self.ast_context.delete();
    self.allocator.destroy(self);
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
        const base = self.uri;
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
    try self.ast_context.collectImports(&new_imports);

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
    self.ast_context.delete();
    self.ast_context = try AstContext.new(self.allocator, self.utf8_buffer.text);
    errdefer self.ast_context.delete();

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
    try self.utf8_buffer.applyChanges(content_changes, encoding);
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
    self: *Self,
    arena: *std.heap.ArenaAllocator,
    pos_index: usize,
    zigenv: ZigEnv,
) !?UriBytePosition {
    const tree = self.ast_context.tree;
    var it = ImportStrIterator.init(tree);
    while (it.next()) |node| {
        if (nodeContainsSourceIndex(tree, node, pos_index)) {
            if (importStr(tree, node)) |import_str| {
                if (try self.uriFromImportStrAlloc(arena.allocator(), import_str, zigenv)) |uri| {
                    // logger.debug("gotoDefinitionString: {s}", .{uri});
                    return UriBytePosition{ .uri = uri, .loc = .{ .start = 0, .end = 0 } };
                }
            }
        }
    }
    return null;
}

pub fn tokenReference(self: Self, token_idx: Ast.TokenIndex) UriBytePosition {
    const token = self.ast_context.tokens.items[token_idx];
    return UriBytePosition{
        .uri = self.uri,
        .loc = token.loc,
    };
}
