const std = @import("std");
const ws = @import("workspace");
const Workspace = ws.Workspace;
const Document = ws.Document;
const DeclWithHandle = ws.DeclWithHandle;
const UriBytePosition = ws.UriBytePosition;
const references = ws.references;
const Config = ws.Config;

fn referencesDefinitionGlobal(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    handle: *Document,
    pos_index: usize,
    include_decl: bool,
    skip_std_references: bool,
) !?[]UriBytePosition {
    if (try workspace.getSymbolGlobal(arena, handle, pos_index)) |decl| {
        var locs = std.ArrayList(UriBytePosition).init(arena.allocator());
        try references.symbolReferences(arena, workspace, decl, include_decl, &locs, skip_std_references);
        return locs.items;
    } else {
        return null;
    }
}

fn referencesDefinitionFieldAccess(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    doc: *Document,
    byte_position: u32,
    range: std.zig.Token.Loc,
    include_decl: bool,
    config: *Config,
) ![]UriBytePosition {
    const decl = try DeclWithHandle.getSymbolFieldAccess(arena, workspace, doc, byte_position, range);
    var locs = std.ArrayList(UriBytePosition).init(arena.allocator());
    try references.symbolReferences(arena, workspace, decl, include_decl, &locs, config.skip_std_references);
    return locs.items;
}

fn referencesDefinitionLabel(
    arena: *std.heap.ArenaAllocator,
    handle: *Document,
    pos_index: usize,
    include_decl: bool,
) !?[]UriBytePosition {
    if ((try handle.getLabelGlobal(pos_index))) |decl| {
        var locs = std.ArrayList(UriBytePosition).init(arena.allocator());
        try references.labelReferences(decl, include_decl, &locs);
        return locs.items;
    } else {
        return null;
    }
}

pub fn process(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    doc: *Document,
    byte_position: u32,
    include_decl: bool,
    config: *Config,
) !?[]UriBytePosition {
    const pos_context = doc.getPositionContext(byte_position);
    return switch (pos_context) {
        .var_access => try referencesDefinitionGlobal(arena, workspace, doc, byte_position, include_decl, config.skip_std_references),
        .field_access => |range| try referencesDefinitionFieldAccess(arena, workspace, doc, byte_position, range, include_decl, config),
        .label => try referencesDefinitionLabel(arena, doc, byte_position, include_decl),
        else => return null,
    };
}
