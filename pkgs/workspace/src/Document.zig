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

pub const FixedImport = struct
{
    str: [std.fs.MAX_PATH_BYTES]u8,
};

const Self = @This();
allocator: std.mem.Allocator,
uri: []const u8,
utf8_buffer: Utf8Buffer,
ast_context: *AstContext,

pub fn new(allocator: std.mem.Allocator, uri: []const u8, text: [:0]u8) !*Self {
    var self = try allocator.create(Self);
    errdefer allocator.destroy(self);
    self.* = Self{
        .allocator = allocator,
        .uri = uri,
        .utf8_buffer = try Utf8Buffer.init(allocator, text),
        .ast_context = try AstContext.new(allocator, text),
    };
    return self;
}

pub fn delete(self: *Self) void {
    self.ast_context.delete();
    self.allocator.destroy(self);
}

pub fn refreshDocument(self: *Self) !void {
    self.ast_context.delete();
    self.ast_context = try AstContext.new(self.allocator, self.utf8_buffer.text);
    errdefer self.ast_context.delete();
}

pub fn applyChanges(self: *Self, content_changes: std.json.Array, encoding: Line.Encoding) !void {
    try self.utf8_buffer.applyChanges(content_changes, encoding);
    try self.refreshDocument();
}

// pub fn applySave(self: *Self, zigenv: ZigEnv) !void {
//     if (self.is_build_file) |build_file| {
//         build_file.loadPackages(self.allocator, null, zigenv) catch |err| {
//             logger.debug("Failed to load packages of build file {s} (error: {})", .{ build_file.uri, err });
//         };
//     }
// }

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

pub fn tokenReference(self: Self, token_idx: Ast.TokenIndex) UriBytePosition {
    const token = self.ast_context.tokens.items[token_idx];
    return UriBytePosition{
        .uri = self.uri,
        .loc = token.loc,
    };
}
