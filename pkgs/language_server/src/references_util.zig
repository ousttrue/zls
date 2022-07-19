const std = @import("std");
const lsp = @import("lsp");
const ws = @import("workspace");
const position_context = ws.position_context;
const Workspace = ws.Workspace;
const Document = ws.Document;
const DocumentPosition = ws.DocumentPosition;
const offsets = ws.offsets;
const analysis = ws.analysis;
const references = ws.references;
const Config = ws.Config;

fn referencesDefinitionGlobal(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    id: i64,
    handle: *Document,
    pos_index: usize,
    include_decl: bool,
    skip_std_references: bool,
) !lsp.Response {
    const decl = try offsets.getSymbolGlobal(arena, workspace, pos_index, handle);
    var locs = std.ArrayList(lsp.Location).init(arena.allocator());
    try references.symbolReferences(
        arena,
        workspace,
        decl,
        offsets.offset_encoding,
        include_decl,
        &locs,
        std.ArrayList(lsp.Location).append,
        skip_std_references,
    );
    return lsp.Response{
        .id = id,
        .result = .{ .Locations = locs.items },
    };
}

fn referencesDefinitionFieldAccess(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    id: i64,
    handle: *Document,
    position: DocumentPosition,
    range: analysis.SourceRange,
    include_decl: bool,
    config: *Config,
) !lsp.Response {
    const decl = try offsets.getSymbolFieldAccess(arena, workspace, handle, position, range);
    var locs = std.ArrayList(lsp.Location).init(arena.allocator());
    try references.symbolReferences(arena, workspace, decl, offsets.offset_encoding, include_decl, &locs, std.ArrayList(lsp.Location).append, config.skip_std_references);
    return lsp.Response{
        .id = id,
        .result = .{ .Locations = locs.items },
    };
}

fn referencesDefinitionLabel(arena: *std.heap.ArenaAllocator, id: i64, handle: *Document, pos_index: usize, include_decl: bool) !lsp.Response {
    const decl = (try offsets.getLabelGlobal(pos_index, handle)) orelse return lsp.Response.createNull(id);
    var locs = std.ArrayList(lsp.Location).init(arena.allocator());
    try references.labelReferences(decl, offsets.offset_encoding, include_decl, &locs, std.ArrayList(lsp.Location).append);
    return lsp.Response{
        .id = id,
        .result = .{ .Locations = locs.items },
    };
}

pub fn process(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    id: i64,
    doc: *Document,
    doc_position: DocumentPosition,
    include_decl: bool,
    config: *Config,
) !lsp.Response {
    const pos_context = position_context.documentPositionContext(arena, doc_position);
    return switch (pos_context) {
        .var_access => try referencesDefinitionGlobal(arena, workspace, id, doc, doc_position.absolute_index, include_decl, config.skip_std_references),
        .field_access => |range| try referencesDefinitionFieldAccess(arena, workspace, id, doc, doc_position, range, include_decl, config),
        .label => try referencesDefinitionLabel(arena, id, doc, doc_position.absolute_index, include_decl),
        else => lsp.Response.createNull(id),
    };
}
