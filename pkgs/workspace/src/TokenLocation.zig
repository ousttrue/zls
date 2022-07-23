const std = @import("std");
const Ast = std.zig.Ast;

const Self = @This();

line: usize,
column: usize,
offset: usize,

pub fn add(lhs: Self, rhs: Self) Self {
    return .{
        .line = lhs.line + rhs.line,
        .column = if (rhs.line == 0)
            lhs.column + rhs.column
        else
            rhs.column,
        .offset = rhs.offset,
    };
}

pub fn tokenRelativeLocation(tree: Ast, start_index: usize, token_start: usize) !Self {
    std.debug.assert(token_start >= start_index);
    var loc = Self{
        .line = 0,
        .column = 0,
        .offset = 0,
    };

    const source = tree.source[start_index..];
    var i: usize = 0;
    while (i + start_index < token_start) {
        const c = source[i];
        if (c == '\n') {
            loc.line += 1;
            loc.column = 0;
            i += 1;
        } else {
            loc.column += 1;
            i += 1;
        }
    }
    loc.offset = i + start_index;
    return loc;
}
