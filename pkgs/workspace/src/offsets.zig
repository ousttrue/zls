const std = @import("std");
const Ast = std.zig.Ast;
const Document = @import("./Document.zig");
const Workspace = @import("./Workspace.zig");
const Utf8Buffer = @import("./Utf8Buffer.zig");
const analysis = @import("./analysis.zig");
const DeclWithHandle = @import("./DeclWithHandle.zig");
const offsets = @import("./offsets.zig");
const ast = @import("./ast.zig");
const DocumentPosition = @import("./DocumentPosition.zig");
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

fn getUtf8Length(utf8: []const u8, utf16Characters: i64) usize {
    var utf8_idx: usize = 0;
    var utf16_idx: usize = 0;
    while (utf16_idx < utf16Characters) {
        if (utf8_idx > utf8.len) {
            unreachable;
            // return error.InvalidParams;
        }

        const n = std.unicode.utf8ByteSequenceLength(utf8[utf8_idx]) catch unreachable;
        const next_utf8_idx = utf8_idx + n;
        const codepoint = std.unicode.utf8Decode(utf8[utf8_idx..next_utf8_idx]) catch unreachable;
        if (codepoint < 0x10000) {
            utf16_idx += 1;
        } else {
            utf16_idx += 2;
        }
        utf8_idx = next_utf8_idx;
    }
    return utf8_idx;
}

pub fn documentPosition(text: []const u8, pos: struct { line: u32, x: u32 = 0 }, encoding: Encoding) OffsetError!DocumentPosition {
    const line = DocumentPosition.getLine(text, pos.line) orelse {
        return OffsetError.LineNotFound;
    };

    if (encoding == .utf8) {
        return line.advance(pos.x);
    } else {
        const utf8 = text[line.absolute_index..];
        const utf8_idx = getUtf8Length(utf8, pos.x);
        return line.advance(utf8_idx);
    }
}

pub fn lineSectionLength(tree: Ast, start_index: usize, end_index: usize, encoding: Encoding) !usize {
    const source = tree.source[start_index..];
    std.debug.assert(end_index >= start_index and source.len >= end_index - start_index);
    if (encoding == .utf8) {
        return end_index - start_index;
    }

    var result: usize = 0;
    var i: usize = 0;
    while (i + start_index < end_index) {
        std.debug.assert(source[i] != '\n');

        const n = try std.unicode.utf8ByteSequenceLength(source[i]);
        if (i + n >= source.len)
            return error.CodepointTooLong;

        const codepoint = try std.unicode.utf8Decode(source[i .. i + n]);

        result += 1 + @as(usize, @boolToInt(codepoint >= 0x10000));
        i += n;
    }
    return result;
}

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

/// Asserts the token is comprised of valid utf8
pub fn tokenLength(tree: Ast, token: Ast.TokenIndex, encoding: Encoding) usize {
    const token_loc = tokenLocation(tree, token);
    if (encoding == .utf8)
        return token_loc.end - token_loc.start;

    var i: usize = token_loc.start;
    var utf16_len: usize = 0;
    while (i < token_loc.end) {
        const n = std.unicode.utf8ByteSequenceLength(tree.source[i]) catch unreachable;
        const codepoint = std.unicode.utf8Decode(tree.source[i .. i + n]) catch unreachable;
        if (codepoint < 0x10000) {
            utf16_len += 1;
        } else {
            utf16_len += 2;
        }
        i += n;
    }
    return utf16_len;
}

/// Token location inside source
pub const Loc = struct {
    start: usize,
    end: usize,
};

pub fn tokenLocation(tree: Ast, token_index: Ast.TokenIndex) Loc {
    const start = tree.tokens.items(.start)[token_index];
    const tag = tree.tokens.items(.tag)[token_index];

    // For some tokens, re-tokenization is needed to find the end.
    var tokenizer: std.zig.Tokenizer = .{
        .buffer = tree.source,
        .index = start,
        .pending_invalid_token = null,
    };

    const token = tokenizer.next();
    std.debug.assert(token.tag == tag);
    return .{ .start = token.loc.start, .end = token.loc.end };
}

pub fn getSymbolFieldAccess(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    handle: *Document,
    position: DocumentPosition,
    range: std.zig.Token.Loc,
) !DeclWithHandle {
    const name = handle.identifierFromPosition(position.absolute_index) orelse return error.NoIdentifier;
    const line_mem_start = @ptrToInt(position.line.ptr) - @ptrToInt(handle.utf8_buffer.mem.ptr);
    var held_range = handle.utf8_buffer.borrowNullTerminatedSlice(line_mem_start + range.start, line_mem_start + range.end);
    var tokenizer = std.zig.Tokenizer.init(held_range.data());

    errdefer held_range.release();
    const result = (try analysis.getFieldAccessType(arena, workspace, handle, position.absolute_index, &tokenizer)) orelse return OffsetError.NoFieldAccessType;
    held_range.release();
    const container_handle = result.unwrapped orelse result.original;
    const container_handle_node = switch (container_handle.type.data) {
        .other => |n| n,
        else => return OffsetError.NodeNotFound,
    };
    return (try DeclWithHandle.lookupSymbolContainer(
        arena,
        workspace,
        container_handle.handle,
        container_handle_node,
        name,
        true,
    )) orelse return OffsetError.ContainerSymbolNotFound;
}
