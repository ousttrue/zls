const std = @import("std");
const Self = @This();
const PositionContext = @import("./position_context.zig").PositionContext;

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

pub fn getChildren(children: *std.ArrayList(u32), tree: *const std.zig.Ast, idx: u32) void {
    const tag = tree.nodes.items(.tag);
    const node_tag = tag[idx];
    const data = tree.nodes.items(.data);
    const node_data = data[idx];

    switch (node_tag) {
        .simple_var_decl => {
            const var_decl = tree.simpleVarDecl(idx);
            if (var_decl.ast.type_node != 0) {
                children.append(var_decl.ast.type_node) catch unreachable;
            }
            if (var_decl.ast.init_node != 0) {
                children.append(var_decl.ast.init_node) catch unreachable;
            }
        },
        .fn_decl => {
            // fn_proto
            children.append(node_data.lhs) catch unreachable;

            // body
            children.append(node_data.rhs) catch unreachable;
        },
        .builtin_call_two => {
            if (node_data.lhs != 0) {
                children.append(node_data.lhs) catch unreachable;
            }
            if (node_data.rhs != 0) {
                children.append(node_data.rhs) catch unreachable;
            }
        },
        .field_access => {
            children.append(node_data.lhs) catch unreachable;
        },
        .string_literal => {
            // leaf. no children
        },
        .block, .block_semicolon => {
            for (tree.extra_data[node_data.lhs..node_data.rhs]) |child| {
                children.append(child) catch unreachable;
            }
        },
        .block_two, .block_two_semicolon => {
            if (node_data.lhs != 0) {
                children.append(node_data.lhs) catch unreachable;
            }
            if (node_data.rhs != 0) {
                children.append(node_data.rhs) catch unreachable;
            }
        },
        else => {
            // std.debug.print("unknown node: {s}\n", .{@tagName(node_tag)});
        },
    }
}

pub fn traverse(context: *Self, stack: *std.ArrayList(u32)) void {
    const tree = context.tree;
    const idx = stack.items[stack.items.len - 1];
    context.nodes_parent[idx] = stack.items[stack.items.len - 2];
    const token_start = tree.firstToken(idx);
    const token_last = tree.lastToken(idx);
    var token_idx = token_start;
    while (token_idx <= token_last) : (token_idx += 1) {
        context.tokens_node[token_idx] = idx;
    }

    var children = std.ArrayList(u32).init(context.allocator);
    defer children.deinit();
    getChildren(&children, context.tree, idx);
    for (children.items) |child| {
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

pub fn getTokenText(self: Self, token: std.zig.Token) []const u8 {
    return self.tree.source[token.loc.start..token.loc.end];
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
