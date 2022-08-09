const std = @import("std");
const FixedPath = @import("./FixedPath.zig");
const Document = @import("./Document.zig");
const Utf8Buffer = @import("./Utf8Buffer.zig");
const Self = @This();

allocator: std.mem.Allocator,
path_document_map: std.StringHashMap(*Document),

pub fn init(allocator: std.mem.Allocator) Self {
    return Self{
        .allocator = allocator,
        .path_document_map = std.StringHashMap(*Document).init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    self.path_document_map.deinit();
}

pub fn put(self: *Self, doc: *Document) !void
{
    try self.path_document_map.put(doc.path.slice(), doc);
}

pub fn get(self: Self, path: FixedPath) ?*Document {
    return self.path_document_map.get(path.slice());
}

pub fn getOrLoad(self: *Self, path: FixedPath) !?*Document {
    if (self.get(path)) |document| {
        return document;
    }

    // load
    const text = path.readContents(self.allocator) catch return null;
    defer self.allocator.free(text);

    const new_document = try Document.new(self.allocator, path, text);
    try self.path_document_map.put(new_document.path.slice(), new_document);
    return new_document;
}
