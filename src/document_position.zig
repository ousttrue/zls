pub const DocumentPosition = struct {
    const Self = @This();

    row: usize,
    col: usize,
    line: []const u8,
    absolute_index: usize,

    pub fn init_line(row: usize, line: []const u8, absolute_index: usize) Self {
        return .{
            .row = row,
            .col = 0,
            .line = line,
            .absolute_index = absolute_index,
        };
    }

    pub fn advance(self: Self, delta: usize) DocumentPosition {
        return DocumentPosition{
            .row = self.row,
            .col = self.col + delta,
            .line = self.line,
            .absolute_index = self.absolute_index + delta,
        };
    }
};
