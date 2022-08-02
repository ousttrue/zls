const std = @import("std");
const Ast = std.zig.Ast;
const Workspace = @import("./Workspace.zig");
const Document = @import("./Document.zig");
const DeclWithHandle = @import("./DeclWithHandle.zig");
const TypeWithHandle = @import("./TypeWithHandle.zig");
const Scope = @import("./Scope.zig");
const Self = @This();

using_trail: std.ArrayList([*]const u8),

pub fn init(allocator: std.mem.Allocator) Self {
    return Self{
        .using_trail = std.ArrayList([*]const u8).init(allocator),
    };
}

pub fn deinit(self: Self) void {
    self.using_trail.deinit();
}

pub fn resolveUse(
    self: *Self,
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    uses: []const *const Ast.Node.Index,
    symbol: []const u8,
    handle: *Document,
) error{OutOfMemory}!?DeclWithHandle {
    // If we were asked to resolve this symbol before,
    // it is self-referential and we cannot resolve it.
    if (std.mem.indexOfScalar([*]const u8, self.using_trail.items, symbol.ptr) != null)
        return null;
    try self.using_trail.append(symbol.ptr);
    defer _ = self.using_trail.pop();

    for (uses) |use| {
        const index = use.*;

        if (handle.ast_context.tree.nodes.items(.data).len <= index) continue;

        const expr_type_node = (try TypeWithHandle.resolveTypeOfNode(
            arena,
            workspace,
            handle,
            handle.ast_context.tree.nodes.items(.data)[index].lhs,
        )) orelse
            continue;

        const node = switch (expr_type_node.type.data) {
            .other => |n| n,
            else => continue,
        };

        if (try self.lookupSymbolContainer(
            arena,
            workspace,
            expr_type_node.handle,
            node,
            symbol,
            false,
        )) |candidate| {
            if (candidate.handle == handle or candidate.isPublic()) {
                return candidate;
            }
        }
    }
    return null;
}

pub fn lookupSymbolContainer(
    self: *Self,
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    handle: *Document,
    container: Ast.Node.Index,
    symbol: []const u8,
    /// If true, we are looking up the symbol like we are accessing through a field access
    /// of an instance of the type, otherwise as a field access of the type value itself.
    instance_access: bool,
) error{OutOfMemory}!?DeclWithHandle {
    const tree = handle.ast_context.tree;
    const node_tags = tree.nodes.items(.tag);
    const token_tags = tree.tokens.items(.tag);
    const main_token = tree.nodes.items(.main_token)[container];

    const is_enum = token_tags[main_token] == .keyword_enum;

    if (Scope.findContainerScope(handle, container)) |container_scope| {
        if (container_scope.decls.getEntry(symbol)) |candidate| {
            switch (candidate.value_ptr.*) {
                .ast_node => |node| {
                    if (node_tags[node].isContainerField()) {
                        if (!instance_access and !is_enum) return null;
                        if (instance_access and is_enum) return null;
                    }
                },
                .label_decl => unreachable,
                else => {},
            }
            return DeclWithHandle{ .decl = candidate.value_ptr, .handle = handle };
        }

        if (try self.resolveUse(arena, workspace, container_scope.uses, symbol, handle)) |result| return result;
        return null;
    }

    return null;
}
