const std = @import("std");
const FixedPath = @import("./FixedPath.zig");
const AstContext = @import("./AstContext.zig");
const Utf8Buffer = @import("./Utf8Buffer.zig");
const Self = @This();

allocator: std.mem.Allocator,
path_context_map: std.StringHashMap(*AstContext),

pub fn init(allocator: std.mem.Allocator) Self {
    return Self{
        .allocator = allocator,
        .path_context_map = std.StringHashMap(*AstContext).init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    self.path_context_map.deinit();
}

pub fn getOrLoad(self: *Self, path: FixedPath) !?*AstContext {
    if (self.path_context_map.get(path.slice())) |context| {
        return context;
    }

    // load
    const text = path.readContents(self.allocator) catch return null;
    defer self.allocator.free(text);

    const utf8_buffer = try Utf8Buffer.init(self.allocator, text);

    var new_context = try AstContext.new(self.allocator, path, utf8_buffer.text);
    try self.path_context_map.put(new_context.path.slice(), new_context);
    return new_context;
}
