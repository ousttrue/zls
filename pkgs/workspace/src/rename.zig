const std = @import("std");
const Workspace = @import("./Workspace.zig");
const DeclWithHandle = @import("./DeclWithHandle.zig");
const references = @import("./references.zig");
const lsp = @import("lsp");
const offsets = @import("./offsets.zig");

// TODO Use a map to array lists and collect at the end instead?
const RefHandlerContext = struct {
    edits: *std.StringHashMap([]lsp.TextEdit),
    allocator: std.mem.Allocator,
    new_name: []const u8,
};

fn refHandler(context: RefHandlerContext, loc: lsp.Location) !void {
    var text_edits = if (context.edits.get(loc.uri)) |slice|
        std.ArrayList(lsp.TextEdit).fromOwnedSlice(context.allocator, slice)
    else
        std.ArrayList(lsp.TextEdit).init(context.allocator);

    (try text_edits.addOne()).* = .{
        .range = loc.range,
        .newText = context.new_name,
    };
    try context.edits.put(loc.uri, text_edits.toOwnedSlice());
}

pub fn renameSymbol(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    decl_handle: DeclWithHandle,
    new_name: []const u8,
    edits: *std.StringHashMap([]lsp.TextEdit),
    encoding: offsets.Encoding,
) !void {
    std.debug.assert(decl_handle.decl.* != .label_decl);
    try references.symbolReferences(arena, workspace, decl_handle, encoding, true, RefHandlerContext{
        .edits = edits,
        .allocator = arena.allocator(),
        .new_name = new_name,
    }, refHandler, true);
}

pub fn renameLabel(arena: *std.heap.ArenaAllocator, decl_handle: DeclWithHandle, new_name: []const u8, edits: *std.StringHashMap([]lsp.TextEdit), encoding: offsets.Encoding) !void {
    std.debug.assert(decl_handle.decl.* == .label_decl);
    try references.labelReferences(decl_handle, encoding, true, RefHandlerContext{
        .edits = edits,
        .allocator = arena.allocator(),
        .new_name = new_name,
    }, refHandler);
}
