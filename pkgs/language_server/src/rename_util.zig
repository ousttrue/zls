const std = @import("std");
const ws = @import("workspace");
const UriBytePosition = ws.UriBytePosition;
const DeclWithHandle = ws.DeclWithHandle;
const offsets = ws.offsets;
const Workspace = ws.Workspace;
const Document = ws.Document;
const analysis = ws.analysis;

fn renameDefinitionGlobal(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    handle: *Document,
    pos_index: usize,
) !?[]const UriBytePosition {
    if (try DeclWithHandle.getSymbolGlobal(arena, workspace, handle, pos_index)) |decl| {
        return try decl.renameSymbol(arena, workspace);
    } else {
        return null;
    }
}

fn renameDefinitionFieldAccess(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    doc: *Document,
    byte_position: u32,
    range: std.zig.Token.Loc,
) ![]const UriBytePosition {
    const decl = try DeclWithHandle.getSymbolFieldAccess(arena, workspace, doc, byte_position, range);
    return try decl.renameSymbol(arena, workspace);
}

fn renameDefinitionLabel(
    arena: *std.heap.ArenaAllocator,
    handle: *Document,
    pos_index: usize,
) !?[]const UriBytePosition {
    if (try handle.getLabelGlobal(pos_index)) |decl| {
        return try decl.renameLabel(arena);
    } else {
        return null;
    }
}

pub fn process(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    doc: *Document,
    byte_position: u32,
) !?[]const UriBytePosition {
    const pos_context = doc.getPositionContext(byte_position);
    return switch (pos_context) {
        .var_access => try renameDefinitionGlobal(arena, workspace, doc, byte_position),
        .field_access => |range| try renameDefinitionFieldAccess(arena, workspace, doc, byte_position, range),
        .label => try renameDefinitionLabel(arena, doc, byte_position),
        else => null,
    };
}
