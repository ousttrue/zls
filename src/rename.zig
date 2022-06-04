const std = @import("std");
const DocumentStore = @import("./DocumentStore.zig");
const analysis = @import("./analysis.zig");
const references = @import("./references.zig");
const lsp = @import("lsp");
const offsets = @import("./offsets.zig");
const Session = @import("./session.zig").Session;

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

pub fn renameSymbol(session: *Session, decl_handle: analysis.DeclWithHandle, new_name: []const u8, edits: *std.StringHashMap([]lsp.TextEdit), encoding: offsets.Encoding) !void {
    std.debug.assert(decl_handle.decl.* != .label_decl);
    try references.symbolReferences(session, decl_handle, encoding, true, RefHandlerContext{
        .edits = edits,
        .allocator = session.arena.allocator(),
        .new_name = new_name,
    }, refHandler, true);
}

pub fn renameLabel(session: *Session, decl_handle: analysis.DeclWithHandle, new_name: []const u8, edits: *std.StringHashMap([]lsp.TextEdit), encoding: offsets.Encoding) !void {
    std.debug.assert(decl_handle.decl.* == .label_decl);
    try references.labelReferences(decl_handle, encoding, true, RefHandlerContext{
        .edits = edits,
        .allocator = session.arena.allocator(),
        .new_name = new_name,
    }, refHandler);
}
