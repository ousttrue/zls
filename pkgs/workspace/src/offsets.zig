const std = @import("std");
const Ast = std.zig.Ast;
const Document = @import("./Document.zig");
const Workspace = @import("./Workspace.zig");
const Utf8Buffer = @import("./Utf8Buffer.zig");
const FieldAccessReturn = @import("./FieldAccessReturn.zig");
const DeclWithHandle = @import("./DeclWithHandle.zig");
const ast = @import("./ast.zig");
const position_context = @import("./position_context.zig");
const Location = @import("./Location.zig");
const logger = std.log.scoped(.offset);

pub const OffsetError = error{
    LineNotFound,
    NoIdentifier,
    NoFieldAccessType,
    GlobalSymbolNotFound,
    ContainerSymbolNotFound,
    NodeNotFound,
    OutOfRange,
    NotImplemented,
};

pub const Encoding = enum {
    utf8,
    utf16,

    pub fn toString(self: Encoding) []const u8 {
        return if (self == .utf8)
            @as([]const u8, "utf-8")
        else
            "utf-16";
    }
};

pub const TokenLocation = struct {
    line: usize,
    column: usize,
    offset: usize,

    pub fn add(lhs: TokenLocation, rhs: TokenLocation) TokenLocation {
        return .{
            .line = lhs.line + rhs.line,
            .column = if (rhs.line == 0)
                lhs.column + rhs.column
            else
                rhs.column,
            .offset = rhs.offset,
        };
    }
};

pub fn tokenRelativeLocation(tree: Ast, start_index: usize, token_start: usize, encoding: Encoding) !TokenLocation {
    std.debug.assert(token_start >= start_index);
    var loc = TokenLocation{
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
            if (encoding == .utf16) {
                const n = try std.unicode.utf8ByteSequenceLength(c);
                if (i + n >= source.len)
                    return error.CodepointTooLong;

                const codepoint = try std.unicode.utf8Decode(source[i .. i + n]);
                loc.column += 1 + @as(usize, @boolToInt(codepoint >= 0x10000));
                i += n;
            } else {
                loc.column += 1;
                i += 1;
            }
        }
    }
    loc.offset = i + start_index;
    return loc;
}
