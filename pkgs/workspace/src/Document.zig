const std = @import("std");
const ZigEnv = @import("./ZigEnv.zig");
const Ast = std.zig.Ast;
const Line = @import("./Line.zig");
const ast = @import("./ast.zig");
const Utf8Buffer = @import("./Utf8Buffer.zig");
const AstContext = @import("./AstContext.zig");
const UriBytePosition = @import("./UriBytePosition.zig");
const logger = std.log.scoped(.Document);

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
    self.utf8_buffer.deinit();
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

pub fn update(self: *Self, text: [:0]u8) !void {
    self.utf8_buffer.deinit();
    self.utf8_buffer = try Utf8Buffer.init(self.allocator, text);
}

// pub fn applySave(self: *Self, zigenv: ZigEnv) !void {
//     if (self.is_build_file) |build_file| {
//         build_file.loadPackages(self.allocator, null, zigenv) catch |err| {
//             logger.debug("Failed to load packages of build file {s} (error: {})", .{ build_file.uri, err });
//         };
//     }
// }

pub fn tokenReference(self: Self, token_idx: Ast.TokenIndex) UriBytePosition {
    const token = self.ast_context.tokens.items[token_idx];
    return UriBytePosition{
        .uri = self.uri,
        .loc = token.loc,
    };
}
