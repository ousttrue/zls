const std = @import("std");
const Ast = std.zig.Ast;
const analysis = @import("./analysis.zig");
const TextDocument = @import("./TextDocument.zig");
const Self = @This();

pub const BuildFile = struct {
    const Pkg = struct {
        name: []const u8,
        uri: []const u8,
    };

    refs: usize,
    uri: []const u8,
    packages: std.ArrayListUnmanaged(Pkg),

    builtin_uri: ?[]const u8 = null,

    pub fn destroy(self: *BuildFile, allocator: std.mem.Allocator) void {
        if (self.builtin_uri) |builtin_uri| allocator.free(builtin_uri);
        allocator.destroy(self);
    }
};

document: TextDocument,
count: usize,
/// Contains one entry for every import in the document
import_uris: []const []const u8,
/// Items in this array list come from `import_uris`
imports_used: std.ArrayListUnmanaged([]const u8),
tree: Ast,
document_scope: analysis.DocumentScope,

associated_build_file: ?*BuildFile,
is_build_file: ?*BuildFile,
