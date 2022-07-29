const std = @import("std");
const ws = @import("workspace");
const UriBytePosition = ws.UriBytePosition;
const DeclWithHandle = ws.DeclWithHandle;
const offsets = ws.offsets;
const Workspace = ws.Workspace;
const Document = ws.Document;
const analysis = ws.analysis;

fn renameDefinitionFieldAccess(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    doc: *Document,
    byte_position: u32,
) ![]const UriBytePosition {
    const decl = try DeclWithHandle.getSymbolFieldAccess(arena, workspace, doc, byte_position);
    return try decl.renameSymbol(arena, workspace);
}

fn renameDefinitionLabel(
    arena: *std.heap.ArenaAllocator,
    handle: *Document,
    pos_index: usize,
) !?[]const UriBytePosition {
    if (try DeclWithHandle.getLabelGlobal(handle, pos_index)) |decl| {
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
    const pos_context = doc.ast_context.getPositionContext(byte_position);
    return switch (pos_context) {
        .var_access => if (try DeclWithHandle.lookupSymbolGlobal(arena, workspace, doc, byte_position)) |decl|
            try decl.renameSymbol(arena, workspace)
        else
            null,
        .field_access => |_| try renameDefinitionFieldAccess(arena, workspace, doc, byte_position),
        .label => try renameDefinitionLabel(arena, doc, byte_position),
        else => null,
    };
}
