const std = @import("std");
const Ast = std.zig.Ast;
const analysis = @import("./analysis.zig");
const Utf8Buffer = @import("./Utf8Buffer.zig");
const BuildFile = @import("./BuildFile.zig");
const DocumentPosition = @import("./DocumentPosition.zig");
// const PositionContext = @import("./PositionContext.zig");
const AstContext = @import("./AstContext.zig");
const Self = @This();

utf8_buffer: Utf8Buffer,
count: usize,
/// Contains one entry for every import in the document
import_uris: []const []const u8,
/// Items in this array list come from `import_uris`
imports_used: std.ArrayListUnmanaged([]const u8),
tree: Ast,
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
        .count = 1,
        .import_uris = &.{},
        .imports_used = .{},
        .utf8_buffer = Utf8Buffer.init(uri, text),
        .tree = tree,
        .document_scope = document_scope,
        .associated_build_file = null,
        .is_build_file = null,
    };

    return self;
}

// pub fn getPositionContext(self: Self, doc_position: DocumentPosition) PositionContext {
//     unreachable;
// }
