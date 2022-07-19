const std = @import("std");
const lsp = @import("lsp");
const ws = @import("workspace");
const position_context = ws.position_context;
const DocumentPosition = ws.DocumentPosition;
const offsets = ws.offsets;
const Workspace = ws.Workspace;
const Document = ws.Document;
const rename = ws.rename;
const analysis = ws.analysis;

fn renameDefinitionGlobal(arena: *std.heap.ArenaAllocator, workspace: *Workspace, id: i64, handle: *Document, pos_index: usize, new_name: []const u8) !lsp.Response {
    const decl = try offsets.getSymbolGlobal(arena, workspace, pos_index, handle);

    var workspace_edit = lsp.WorkspaceEdit{
        .changes = std.StringHashMap([]lsp.TextEdit).init(arena.allocator()),
    };
    try rename.renameSymbol(arena, workspace, decl, new_name, &workspace_edit.changes.?, offsets.offset_encoding);
    return lsp.Response{
        .id = id,
        .result = .{ .WorkspaceEdit = workspace_edit },
    };
}

fn renameDefinitionFieldAccess(arena: *std.heap.ArenaAllocator, workspace: *Workspace, id: i64, handle: *Document, position: DocumentPosition, range: analysis.SourceRange, new_name: []const u8) !lsp.Response {
    const decl = try offsets.getSymbolFieldAccess(arena, workspace, handle, position, range);

    var workspace_edit = lsp.WorkspaceEdit{
        .changes = std.StringHashMap([]lsp.TextEdit).init(arena.allocator()),
    };
    try rename.renameSymbol(arena, workspace, decl, new_name, &workspace_edit.changes.?, offsets.offset_encoding);
    return lsp.Response{
        .id = id,
        .result = .{ .WorkspaceEdit = workspace_edit },
    };
}

fn renameDefinitionLabel(arena: *std.heap.ArenaAllocator, id: i64, handle: *Document, pos_index: usize, new_name: []const u8) !lsp.Response {
    const decl = (try offsets.getLabelGlobal(pos_index, handle)) orelse return lsp.Response.createNull(id);

    var workspace_edit = lsp.WorkspaceEdit{
        .changes = std.StringHashMap([]lsp.TextEdit).init(arena.allocator()),
    };
    try rename.renameLabel(arena, decl, new_name, &workspace_edit.changes.?, offsets.offset_encoding);
    return lsp.Response{
        .id = id,
        .result = .{ .WorkspaceEdit = workspace_edit },
    };
}

pub fn process(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    id: i64,
    handle: *Document,
    doc_position: DocumentPosition,
    new_name: []const u8,
) !lsp.Response {
    const pos_context = position_context.documentPositionContext(arena, doc_position);
    return switch (pos_context) {
        .var_access => try renameDefinitionGlobal(arena, workspace, id, handle, doc_position.absolute_index, new_name),
        .field_access => |range| try renameDefinitionFieldAccess(arena, workspace, id, handle, doc_position, range, new_name),
        .label => try renameDefinitionLabel(arena, id, handle, doc_position.absolute_index, new_name),
        else => lsp.Response.createNull(id),
    };
}
