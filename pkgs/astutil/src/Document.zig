const std = @import("std");
const FixedPath = @import("././FixedPath.zig");
// const astutil = @import("astutil");
// const ZigEnv = @import("./ZigEnv.zig");
// const Ast = std.zig.Ast;
const Line = @import("./Line.zig");
// const ast = astutil.ast;
const Utf8Buffer = @import("./Utf8Buffer.zig");
const AstContext = @import("./AstContext.zig");
// const PathPosition = astutil.PathPosition;
// const FixedPath = astutil.FixedPath;
// const DocumentScope = @import("./DocumentScope.zig");
const logger = std.log.scoped(.astutil_Document);

const Self = @This();
allocator: std.mem.Allocator,
path: FixedPath,
utf8_buffer: Utf8Buffer,
ast_context: *AstContext,

pub fn new(allocator: std.mem.Allocator, path: FixedPath, text: []const u8) !*Self {
    const utf8_buffer = try Utf8Buffer.init(allocator, text);

    var self = try allocator.create(Self);
    errdefer allocator.destroy(self);
    self.* = Self{
        .allocator = allocator,
        .path = path,
        .utf8_buffer = utf8_buffer,
        .ast_context = try AstContext.new(allocator, utf8_buffer.text),
    };
    return self;
}

pub fn delete(self: *Self) void {
    self.ast_context.delete();
    self.utf8_buffer.deinit();
    self.allocator.destroy(self);
}

fn refreshDocument(self: *Self) !void {
    self.ast_context.delete();
    self.ast_context = try AstContext.new(self.allocator, self.utf8_buffer.text);
    errdefer self.ast_context.delete();
}

pub fn applyChanges(self: *Self, content_changes: std.json.Array, encoding: Line.Encoding) !void {
    try self.utf8_buffer.applyChanges(content_changes, encoding);
    try self.refreshDocument();
}

pub fn update(self: *Self, text: []const u8) !void {
    self.utf8_buffer.deinit();
    self.utf8_buffer = try Utf8Buffer.init(self.allocator, text);
    try self.refreshDocument();
}
