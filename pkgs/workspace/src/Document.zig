const std = @import("std");
const astutil = @import("astutil");
const ZigEnv = @import("./ZigEnv.zig");
const Ast = std.zig.Ast;
const Line = @import("./Line.zig");
const ast = astutil.ast;
const Utf8Buffer = @import("./Utf8Buffer.zig");
const AstContext = astutil.AstContext;
const PathPosition = astutil.PathPosition;
const FixedPath = astutil.FixedPath;
const DocumentScope = @import("./DocumentScope.zig");
const logger = std.log.scoped(.Document);

const Self = @This();
allocator: std.mem.Allocator,
path: FixedPath,
utf8_buffer: Utf8Buffer,
ast_context: *AstContext,
document_scope: DocumentScope = undefined,

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
    self.document_scope = try DocumentScope.init(allocator, self.ast_context.tree);
    return self;
}

pub fn delete(self: *Self) void {
    self.document_scope.deinit();
    self.ast_context.delete();
    self.utf8_buffer.deinit();
    self.allocator.destroy(self);
}

fn refreshDocument(self: *Self) !void {
    self.document_scope.deinit();
    self.ast_context.delete();
    self.ast_context = try AstContext.new(self.allocator, self.utf8_buffer.text);
    self.document_scope = try DocumentScope.init(self.allocator, self.ast_context.tree);
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

// pub fn applySave(self: *Self, zigenv: ZigEnv) !void {
//     if (self.is_build_file) |build_file| {
//         build_file.loadPackages(self.allocator, null, zigenv) catch |err| {
//             logger.debug("Failed to load packages of build file {s} (error: {})", .{ build_file.uri, err });
//         };
//     }
// }

pub fn tokenReference(self: Self, token_idx: Ast.TokenIndex) PathPosition {
    const token = self.ast_context.tokens[token_idx];
    return PathPosition{
        .path = self.path,
        .loc = token.loc,
    };
}
