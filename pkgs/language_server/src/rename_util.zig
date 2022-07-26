const std = @import("std");
const lsp = @import("lsp");
const ws = @import("workspace");
const position_context = ws.position_context;
const DocumentPosition = ws.DocumentPosition;
const UriBytePosition = ws.UriBytePosition;
const DeclWithHandle = ws.DeclWithHandle;
const offsets = ws.offsets;
const Workspace = ws.Workspace;
const Document = ws.Document;
const rename = ws.rename;
const analysis = ws.analysis;

fn renameDefinitionGlobal(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    handle: *Document,
    pos_index: usize,
) !?[]const UriBytePosition {
    if (try workspace.getSymbolGlobal(arena, handle, pos_index)) |decl| {
        return try rename.renameSymbol(arena, workspace, decl);
    } else {
        return null;
    }
}

fn renameDefinitionFieldAccess(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    handle: *Document,
    position: DocumentPosition,
    range: std.zig.Token.Loc,
) ![]const UriBytePosition {
    const decl = try DeclWithHandle.getSymbolFieldAccess(arena, workspace, handle, position.absolute_index, range);
    return try rename.renameSymbol(arena, workspace, decl);
}

fn renameDefinitionLabel(
    arena: *std.heap.ArenaAllocator,
    handle: *Document,
    pos_index: usize,
) !?[]const UriBytePosition {
    if (try handle.getLabelGlobal(pos_index)) |decl| {
        return try rename.renameLabel(arena, decl);
    } else {
        return null;
    }
}

pub fn process(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    handle: *Document,
    doc_position: DocumentPosition,
) !?[]const UriBytePosition {
    const pos_context = position_context.documentPositionContext(arena, doc_position);
    return switch (pos_context) {
        .var_access => try renameDefinitionGlobal(arena, workspace, handle, doc_position.absolute_index),
        .field_access => |range| try renameDefinitionFieldAccess(arena, workspace, handle, doc_position, range),
        .label => try renameDefinitionLabel(arena, handle, doc_position.absolute_index),
        else => null,
    };
}
