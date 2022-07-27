const std = @import("std");
const ws = @import("workspace");
const Workspace = ws.Workspace;
const Document = ws.Document;
const DeclWithHandle = ws.DeclWithHandle;
const UriBytePosition = ws.UriBytePosition;

pub fn gotoHandler(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    doc: *Document,
    byte_position: u32,
    resolve_alias: bool,
) !?UriBytePosition {
    const pos_context = doc.getPositionContext(byte_position);
    switch (pos_context) {
        .var_access => {
            if (try DeclWithHandle.getSymbolGlobal(arena, workspace, doc, byte_position)) |decl| {
                return decl.gotoDefinitionSymbol(workspace, arena, resolve_alias);
            } else {
                return null;
            }
        },
        .field_access => |range| {
            const decl = try DeclWithHandle.getSymbolFieldAccess(arena, workspace, doc, byte_position, range);
            return decl.gotoDefinitionSymbol(workspace, arena, resolve_alias);
        },
        .string_literal => {
            return doc.gotoDefinitionString(arena, byte_position, workspace.zigenv);
        },
        .label => {
            // return self.gotoDefinitionLabel(arena, doc, byte_position);
            if (try doc.getLabelGlobal(byte_position)) |decl| {
                return decl.gotoDefinitionSymbol(workspace, arena, false);
            } else {
                return null;
            }
        },
        else => {
            // logger.debug("PositionContext.{s} is not implemented", .{@tagName(pos_context)});
            return null;
        },
    }
}
