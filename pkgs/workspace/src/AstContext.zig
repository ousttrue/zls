const std = @import("std");
const Ast = std.zig.Ast;
const UriBytePosition = @import("./UriBytePosition.zig");
const AstGetChildren = @import("./AstGetChildren.zig");
const ast = @import("./ast.zig");
const ZigEnv = @import("./ZigEnv.zig");
const Self = @This();

fn getAllTokens(allocator: std.mem.Allocator, source: [:0]const u8) std.ArrayList(std.zig.Token) {
    var tokens = std.ArrayList(std.zig.Token).init(allocator);
    var tokenizer: std.zig.Tokenizer = .{
        .buffer = source,
        .index = 0,
        .pending_invalid_token = null,
    };

    while (true) {
        const token = tokenizer.next();
        if (token.tag == .eof) {
            break;
        }
        tokens.append(token) catch unreachable;
    }

    return tokens;
}

pub fn traverse(context: *Self, stack: *std.ArrayList(u32)) void {
    const tree = context.tree;
    const idx = stack.items[stack.items.len - 1];
    std.debug.assert(stack.items.len > 1);
    const parent_idx = stack.items[stack.items.len - 2];
    context.nodes_parent[idx] = parent_idx;
    const token_start = tree.firstToken(idx);
    const token_last = tree.lastToken(idx);
    var token_idx = token_start;
    while (token_idx <= token_last) : (token_idx += 1) {
        context.tokens_node[token_idx] = idx;
    }

    var children = AstGetChildren.init(context.allocator);
    defer children.deinit();
    for (children.getChildren(context.tree, idx)) |child| {
        if (child >= context.nodes_parent.len) {
            const tags = tree.nodes.items(.tag);
            const node_tag = tags[idx];
            std.log.err("{}: {}>=nodes_parent.len", .{ node_tag, child });
            unreachable;
        }
        stack.append(child) catch unreachable;
        traverse(context, stack);
        _ = stack.pop();
    }
}

pub const AstPath = struct {};

allocator: std.mem.Allocator,
tree: *const std.zig.Ast,
nodes_parent: []u32,
tokens: std.ArrayList(std.zig.Token),
tokens_node: []u32,

pub fn new(allocator: std.mem.Allocator, tree: *const std.zig.Ast) *Self {
    // const tree: std.zig.Ast = std.zig.parse(allocator, src) catch unreachable;
    var self = allocator.create(Self) catch unreachable;
    self.* = Self{
        .allocator = allocator,
        .tree = tree,
        .nodes_parent = allocator.alloc(u32, tree.nodes.len) catch unreachable,
        .tokens = getAllTokens(allocator, tree.source),
        .tokens_node = allocator.alloc(u32, tree.tokens.len) catch unreachable,
    };
    for (self.nodes_parent) |*x| {
        x.* = 0;
    }
    for (self.tokens_node) |*x| {
        x.* = 0;
    }

    var stack = std.ArrayList(u32).init(allocator);
    defer stack.deinit();

    // root
    stack.append(0) catch unreachable;
    for (tree.rootDecls()) |decl| {
        // top level
        stack.append(decl) catch unreachable;
        traverse(self, &stack);
        _ = stack.pop();
    }

    return self;
}

pub fn delete(self: *Self) void {
    self.allocator.free(self.tokens_node);
    self.allocator.free(self.nodes_parent);
    self.allocator.destroy(self);
}

pub fn getText(self: Self, loc: std.zig.Token.Loc) []const u8 {
    return self.tree.source[loc.start..loc.end];
}

pub fn getTokenText(self: Self, token: std.zig.Token) []const u8 {
    return self.getText(token.loc);
}

pub fn getTokens(self: Self, start: usize, last: usize) []const std.zig.Token {
    var end = last;
    if (end < self.tokens.items.len) {
        end += 1;
    }
    return self.tokens.items[start..end];
}

pub fn getNodeTokens(self: Self, idx: u32) []const std.zig.Token {
    return self.getTokens(self.tree.firstToken(idx), self.tree.lastToken(idx));
}

pub fn getParentNode(self: Self, idx: u32) ?u32 {
    if (idx == 0) {
        return null;
    }
    return self.nodes_parent[idx];
}

pub fn getNodeTag(self: Self, idx: u32) std.zig.Ast.Node.Tag {
    const tag = self.tree.nodes.items(.tag);
    return tag[idx];
}

pub fn getMainToken(self: Self, idx: u32) std.zig.Token {
    const main_token = self.tree.nodes.items(.main_token);
    const token_idx = main_token[idx];
    return self.tokens.items[token_idx];
}

pub fn getAstPath(self: Self, token_idx: usize) ?AstPath {
    const tag = self.tree.nodes.items(.tag);
    var idx = self.tokens_node[token_idx];
    while (self.getParentNode(idx)) |parent| : (idx = parent) {
        std.debug.print(", {}[{s}]", .{ idx, @tagName(tag[idx]) });
    }
    std.debug.print("\n", .{});

    return null;
}

pub fn findAncestor(self: Self, idx: u32, target: u32) bool {
    var current = self.nodes_parent[idx];
    while (current != 0) : (current = self.nodes_parent[current]) {
        if (current == target) {
            return true;
        }
    }
    return false;
}

pub fn isInToken(pos: usize, token: std.zig.Token) bool {
    return pos >= token.loc.start and pos <= token.loc.end - 1;
}

pub const TokenWithIndex = struct { token: std.zig.Token, index: u32 };

pub fn tokenFromBytePos(self: Self, byte_pos: usize) ?TokenWithIndex {
    for (self.tokens.items) |token, i| {
        if (isInToken(byte_pos, token)) {
            return TokenWithIndex{ .token = token, .index = @intCast(u32, i) };
        }
    }
    return null;
}

fn writePath(self: Self, buffer: *std.ArrayList(u8), node_idx: u32) anyerror!void {
    const parent = self.nodes_parent[node_idx];
    const w = buffer.writer();
    if (parent != 0) {
        try self.writePath(buffer, parent);
        try w.print("/", .{});
    } else {}
    const tag = self.tree.nodes.items(.tag);
    const node_tag = tag[node_idx];
    try w.print("{s}", .{@tagName(node_tag)});
}

///
/// AST: a/b/c/d => TAG: TAG_TEXT
///
pub fn getTokenIndexContext(self: Self, allocator: std.mem.Allocator, token_idx: usize) ![]const u8 {
    var buffer = std.ArrayList(u8).init(allocator);

    // ast path
    const w = buffer.writer();
    try w.print("", .{});
    const node_idx = self.tokens_node[token_idx];
    try self.writePath(&buffer, node_idx);
    try w.print(" => ", .{});

    // token
    const token = self.tokens.items[token_idx];
    const name = self.getTokenText(token);
    try w.print("{s}: {s}", .{
        @tagName(token.tag),
        name,
    });

    return buffer.items;
}

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

fn getPositionContextFromIdentifier(self: Self, token_idx: u32, node_idx: u32, node_tag: std.zig.Ast.Node.Tag) PositionContext {
    _ = token_idx;
    return switch (node_tag) {
        .field_access => {
            const first = self.tokens.items[self.tree.firstToken(node_idx)];
            const last = self.tokens.items[self.tree.lastToken(node_idx)];
            return .{ .field_access = .{ .start = first.loc.start, .end = last.loc.end } };
        },
        .enum_literal => {
            return .enum_literal;
        },
        else => {
            const token = self.tokens.items[token_idx];
            return .{ .var_access = token.loc };
        },
    };
}

pub fn getPositionContext(self: Self, byte_pos: usize) PositionContext {
    const token_with_index = self.tokenFromBytePos(byte_pos) orelse {
        return .empty;
    };
    const token = token_with_index.token;
    const token_index = token_with_index.index;
    // const token_text = self.getTokenText(token);
    const node_index = self.tokens_node[token_index];
    const tag = self.tree.nodes.items(.tag);
    const node_tag = tag[node_index];

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

fn isSymbolChar(char: u8) bool {
    return std.ascii.isAlNum(char) or char == '_';
}

pub fn identifierFromPosition(self: Self, pos_index: usize) ?[]const u8 {
    const text: []const u8 = self.tree.source;
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
