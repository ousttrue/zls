const std = @import("std");
const lsp = @import("lsp");
const Ast = std.zig.Ast;
const Session = @import("./session.zig").Session;
const DocumentStore = @import("./DocumentStore.zig");
const analysis = @import("./analysis.zig");
const DocumentPosition = @import("./document_position.zig").DocumentPosition;
const position_context = @import("./position_context.zig");

const logger = std.log.scoped(.offset);

pub const OffsetError = error{
    LineNotFound,
    PositionNegativeCharacter,
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
};
pub var offset_encoding = Encoding.utf16;

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

pub fn documentPosition(doc: lsp.TextDocument, position: lsp.Position, encoding: Encoding) OffsetError!DocumentPosition {
    if (position.character < 0) {
        return OffsetError.PositionNegativeCharacter;
    }

    const line = DocumentPosition.getLine(doc.text, @intCast(usize, position.line)) orelse {
        return OffsetError.LineNotFound;
    };

    if (encoding == .utf8) {
        return line.advance(@intCast(usize, position.character));
    } else {
        const utf8 = doc.text[line.absolute_index..];
        const utf8_idx = getUtf8Length(utf8, position.character);
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

pub fn documentRange(doc: lsp.TextDocument, encoding: Encoding) !lsp.Range {
    var line_idx: i64 = 0;
    var curr_line: []const u8 = doc.text;

    var split_iterator = std.mem.split(u8, doc.text, "\n");
    while (split_iterator.next()) |line| : (line_idx += 1) {
        curr_line = line;
    }

    if (encoding == .utf8) {
        return lsp.Range{
            .start = .{
                .line = 0,
                .character = 0,
            },
            .end = .{
                .line = line_idx,
                .character = @intCast(i64, curr_line.len),
            },
        };
    } else {
        var utf16_len: usize = 0;
        var line_utf8_idx: usize = 0;
        while (line_utf8_idx < curr_line.len) {
            const n = try std.unicode.utf8ByteSequenceLength(curr_line[line_utf8_idx]);
            const codepoint = try std.unicode.utf8Decode(curr_line[line_utf8_idx .. line_utf8_idx + n]);
            if (codepoint < 0x10000) {
                utf16_len += 1;
            } else {
                utf16_len += 2;
            }
            line_utf8_idx += n;
        }
        return lsp.Range{
            .start = .{
                .line = 0,
                .character = 0,
            },
            .end = .{
                .line = line_idx,
                .character = @intCast(i64, utf16_len),
            },
        };
    }
}

fn isSymbolChar(char: u8) bool {
    return std.ascii.isAlNum(char) or char == '_';
}

pub fn identifierFromPosition(pos_index: usize, text: []const u8) OffsetError![]const u8 {
    if (pos_index + 1 >= text.len) {
        return OffsetError.NoIdentifier;
    }
    if (!isSymbolChar(text[pos_index])) {
        return OffsetError.NoIdentifier;
    }

    var start_idx = pos_index;
    while (start_idx >= 0) : (start_idx -= 1) {
        if (!isSymbolChar(text[start_idx])) {
            start_idx += 1;
            break;
        }
    }

    var end_idx = pos_index;
    while (end_idx < text.len and isSymbolChar(text[end_idx])) {
        end_idx += 1;
    }

    const id = text[start_idx..end_idx];
    // std.debug.print("{}, {}:{s}", .{start_idx, end_idx, id});
    return id;
}

test "identifierFromPosition" {
    try std.testing.expectEqualStrings("abc", identifierFromPosition(1, " abc cde"));
    try std.testing.expectEqualStrings("abc", identifierFromPosition(2, " abc cde"));
    try std.testing.expectEqualStrings("", identifierFromPosition(3, "abc cde"));
}

pub fn getSymbolGlobal(session: *Session, pos_index: usize, handle: *DocumentStore.Handle) !analysis.DeclWithHandle {
    const name = try identifierFromPosition(pos_index, handle.document.text);
    return (try analysis.lookupSymbolGlobal(session, handle, name, pos_index)) orelse return OffsetError.GlobalSymbolNotFound;
}

pub fn getLabelGlobal(pos_index: usize, handle: *DocumentStore.Handle) !?analysis.DeclWithHandle {
    const name = try identifierFromPosition(pos_index, handle.document.text);
    return try analysis.lookupLabel(handle, name, pos_index);
}

fn gotoDefinitionSymbol(session: *Session, id: i64, decl_handle: analysis.DeclWithHandle, resolve_alias: bool) !lsp.Response {
    var handle = decl_handle.handle;

    const location = switch (decl_handle.decl.*) {
        .ast_node => |node| block: {
            if (resolve_alias) {
                if (try analysis.resolveVarDeclAlias(session, .{ .node = node, .handle = handle })) |result| {
                    handle = result.handle;
                    break :block try result.location(offset_encoding);
                }
            }

            const name_token = analysis.getDeclNameToken(handle.tree, node) orelse
                return lsp.Response.createNull(id);
            break :block try tokenRelativeLocation(handle.tree, 0, handle.tree.tokens.items(.start)[name_token], offset_encoding);
        },
        else => try decl_handle.location(offset_encoding),
    };

    return lsp.Response{
        .id = id,
        .result = .{
            .Location = .{
                .uri = handle.document.uri,
                .range = .{
                    .start = .{
                        .line = @intCast(i64, location.line),
                        .character = @intCast(i64, location.column),
                    },
                    .end = .{
                        .line = @intCast(i64, location.line),
                        .character = @intCast(i64, location.column),
                    },
                },
            },
        },
    };
}

pub fn getSymbolFieldAccess(session: *Session, handle: *DocumentStore.Handle, position: DocumentPosition, range: analysis.SourceRange) !analysis.DeclWithHandle {
    const name = try identifierFromPosition(position.absolute_index, handle.document.text);
    const line_mem_start = @ptrToInt(position.line.ptr) - @ptrToInt(handle.document.mem.ptr);
    var held_range = handle.document.borrowNullTerminatedSlice(line_mem_start + range.start, line_mem_start + range.end);
    var tokenizer = std.zig.Tokenizer.init(held_range.data());

    errdefer held_range.release();
    const result = (try analysis.getFieldAccessType(session, handle, position.absolute_index, &tokenizer)) orelse return OffsetError.NoFieldAccessType;
    held_range.release();
    const container_handle = result.unwrapped orelse result.original;
    const container_handle_node = switch (container_handle.type.data) {
        .other => |n| n,
        else => return OffsetError.NodeNotFound,
    };
    return (try analysis.lookupSymbolContainer(
        session,
        .{ .node = container_handle_node, .handle = container_handle.handle },
        name,
        true,
    )) orelse return OffsetError.ContainerSymbolNotFound;
}

fn gotoDefinitionString(session: *Session, id: i64, pos_index: usize, handle: *DocumentStore.Handle) !lsp.Response {
    const tree = handle.tree;

    const import_str = analysis.getImportStr(tree, 0, pos_index) orelse return lsp.Response.createNull(id);
    const uri = (try session.document_store.uriFromImportStr(
        session.arena.allocator(),
        handle.*,
        import_str,
    )) orelse return lsp.Response.createNull(id);

    return lsp.Response{
        .id = id,
        .result = .{
            .Location = .{
                .uri = uri,
                .range = .{
                    .start = .{ .line = 0, .character = 0 },
                    .end = .{ .line = 0, .character = 0 },
                },
            },
        },
    };
}

fn gotoDefinitionLabel(session: *Session, id: i64, pos_index: usize, handle: *DocumentStore.Handle) !lsp.Response {
    const decl = (try getLabelGlobal(pos_index, handle)) orelse return lsp.Response.createNull(id);
    return try gotoDefinitionSymbol(session, id, decl, false);
}

pub fn gotoHandler(session: *Session, id: i64, req: lsp.requests.GotoDefinition, resolve_alias: bool) !lsp.Response {
    logger.debug("[definition]{s} {}", .{ req.params.textDocument.uri, req.params.position });
    const handle = try session.getHandle(req.params.textDocument.uri);
    const doc_position = try documentPosition(handle.document, req.params.position, offset_encoding);
    const pos_context = position_context.documentPositionContext(session.arena, doc_position);

    switch (pos_context) {
        .var_access => {
            const decl = try getSymbolGlobal(session, doc_position.absolute_index, handle);
            return try gotoDefinitionSymbol(session, id, decl, resolve_alias);
        },
        .field_access => |range| {
            const decl = try getSymbolFieldAccess(session, handle, doc_position, range);
            return try gotoDefinitionSymbol(session, id, decl, resolve_alias);
        },
        .string_literal => {
            return try gotoDefinitionString(session, id, doc_position.absolute_index, handle);
        },
        .label => {
            return try gotoDefinitionLabel(session, id, doc_position.absolute_index, handle);
        },
        else => {
            logger.debug("PositionContext.{s} is not implemented", .{@tagName(pos_context)});
            return OffsetError.NotImplemented;
        },
    }
}
