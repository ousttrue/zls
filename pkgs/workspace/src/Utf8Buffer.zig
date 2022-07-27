const std = @import("std");
const Self = @This();

uri: []const u8,
// This is a substring of mem starting at 0
text: [:0]const u8,
// This holds the memory that we have actually allocated.
mem: []u8,

pub fn init(uri: []const u8, text: [:0] u8) Self {
    return .{
        .uri = uri,
        .text = text,
        // Extra +1 to include the null terminator
        .mem = text.ptr[0 .. text.len + 1],
    };
}

const Held = struct {
    document: *const Self,
    popped: u8,
    start_index: usize,
    end_index: usize,

    pub fn data(self: @This()) [:0]const u8 {
        return self.document.mem[self.start_index..self.end_index :0];
    }

    pub fn release(self: *@This()) void {
        self.document.mem[self.end_index] = self.popped;
    }
};
