const std = @import("std");
const lsp = @import("lsp");
const ws = @import("workspace");
const position_context = ws.position_context;
const DocumentPosition = ws.DocumentPosition;
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
    new_name: []const u8,
) !?lsp.WorkspaceEdit {
    if (try workspace.getSymbolGlobal(arena, handle, pos_index)) |decl| {
        var workspace_edit = lsp.WorkspaceEdit{
            .changes = std.StringHashMap([]lsp.TextEdit).init(arena.allocator()),
        };
        try rename.renameSymbol(arena, workspace, decl, new_name, &workspace_edit.changes.?);
        return workspace_edit;
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
    new_name: []const u8,
) !lsp.WorkspaceEdit {
    const decl = try DeclWithHandle.getSymbolFieldAccess(arena, workspace, handle, position, range);
    var workspace_edit = lsp.WorkspaceEdit{
        .changes = std.StringHashMap([]lsp.TextEdit).init(arena.allocator()),
    };
    try rename.renameSymbol(arena, workspace, decl, new_name, &workspace_edit.changes.?);
    return workspace_edit;
}

fn renameDefinitionLabel(
    arena: *std.heap.ArenaAllocator,
    handle: *Document,
    pos_index: usize,
    new_name: []const u8,
) !?lsp.WorkspaceEdit {
    if (try handle.getLabelGlobal(pos_index)) |decl| {
        var workspace_edit = lsp.WorkspaceEdit{
            .changes = std.StringHashMap([]lsp.TextEdit).init(arena.allocator()),
        };
        try rename.renameLabel(arena, decl, new_name, &workspace_edit.changes.?);
        return workspace_edit;
    } else {
        return null;
    }
}

pub fn process(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    handle: *Document,
    doc_position: DocumentPosition,
    new_name: []const u8,
) !?lsp.WorkspaceEdit {
    const pos_context = position_context.documentPositionContext(arena, doc_position);
    return switch (pos_context) {
        .var_access => try renameDefinitionGlobal(arena, workspace, handle, doc_position.absolute_index, new_name),
        .field_access => |range| try renameDefinitionFieldAccess(arena, workspace, handle, doc_position, range, new_name),
        .label => try renameDefinitionLabel(arena, handle, doc_position.absolute_index, new_name),
        else => null,
    };
}

// lsp.Response.createNull(id),
// return lsp.Response{
//     .id = id,
//     .result = .{ .WorkspaceEdit = workspace_edit },
// };
