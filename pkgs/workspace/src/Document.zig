const std = @import("std");
const Ast = std.zig.Ast;
const analysis = @import("./analysis.zig");
const Utf8Buffer = @import("./Utf8Buffer.zig");
const BuildFile = @import("./BuildFile.zig");
const DocumentPosition = @import("./DocumentPosition.zig");
const PositionContext = @import("./position_context.zig").PositionContext;
const AstContext = @import("./AstContext.zig");
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

pub fn getPositionContext(self: Self, byte_pos: usize) PositionContext {
    if (self.ast_context.tokenIndexFromBytePos(byte_pos)) |token_index| {
        const node_idx = self.ast_context.tokens_node[token_index];
        _ = node_idx;
        return .empty;
    } else {
        return .empty;
    }
}
