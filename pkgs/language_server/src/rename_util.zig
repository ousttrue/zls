const std = @import("std");
const ws = @import("workspace");
const UriBytePosition = ws.UriBytePosition;
const DeclWithHandle = ws.DeclWithHandle;
const offsets = ws.offsets;
const Workspace = ws.Workspace;
const Document = ws.Document;
const analysis = ws.analysis;

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
        .field_access => |_| if (DeclWithHandle.getSymbolFieldAccess(arena, workspace, doc, byte_position)) |decl|
            try decl.renameSymbol(arena, workspace)
        else |_|
            null,
        .label => if (try DeclWithHandle.lookupLabel(doc, byte_position)) |decl|
            try decl.renameLabel(arena)
        else
            null,
        else => null,
    };
}
