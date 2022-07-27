const std = @import("std");
const Workspace = @import("./Workspace.zig");
const DeclWithHandle = @import("./DeclWithHandle.zig");
const UriBytePosition = @import("./UriBytePosition.zig");
const references = @import("./references.zig");

pub fn renameSymbol(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    decl_handle: DeclWithHandle,
) ![]UriBytePosition {
    std.debug.assert(decl_handle.decl.* != .label_decl);
    var locations = std.ArrayList(UriBytePosition).init(arena.allocator());
    try references.symbolReferences(arena, workspace, decl_handle, true, &locations, true);
    return locations.items;
}

pub fn renameLabel(
    arena: *std.heap.ArenaAllocator,
    decl_handle: DeclWithHandle,
) ![]UriBytePosition {
    std.debug.assert(decl_handle.decl.* == .label_decl);
    var locations = std.ArrayList(UriBytePosition).init(arena.allocator());
    try references.labelReferences(decl_handle, true, &locations);
    return locations.items;
}
