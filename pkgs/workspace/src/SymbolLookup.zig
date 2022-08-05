const std = @import("std");
const astutil = @import("astutil");
const Ast = std.zig.Ast;
const Workspace = @import("./Workspace.zig");
const Document = @import("./Document.zig");
const DeclWithHandle = @import("./DeclWithHandle.zig");
const TypeWithHandle = @import("./TypeWithHandle.zig");
const FieldAccessReturn = @import("./FieldAccessReturn.zig");
const Scope = @import("./Scope.zig");
const AstToken = astutil.AstToken;
const AstNode = astutil.AstNode;
const ast = astutil.ast;

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
) ?DeclWithHandle {
    // If we were asked to resolve this symbol before,
    // it is self-referential and we cannot resolve it.
    if (std.mem.indexOfScalar([*]const u8, self.using_trail.items, symbol.ptr) != null)
        return null;
    self.using_trail.append(symbol.ptr) catch unreachable;
    defer _ = self.using_trail.pop();

    for (uses) |use| {
        const index = use.*;

        if (handle.ast_context.tree.nodes.items(.data).len <= index) continue;

        const expr_type_node = TypeWithHandle.resolveTypeOfNode(
            arena,
            workspace,
            handle,
            handle.ast_context.tree.nodes.items(.data)[index].lhs,
        ) orelse {
            continue;
        };

        const node = switch (expr_type_node.type.data) {
            .other => |n| n,
            else => continue,
        };

        if (self.lookupSymbolContainer(
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
    doc: *Document,
    container: Ast.Node.Index,
    symbol: []const u8,
    /// If true, we are looking up the symbol like we are accessing through a field access
    /// of an instance of the type, otherwise as a field access of the type value itself.
    instance_access: bool,
) ?DeclWithHandle {
    const tree = doc.ast_context.tree;
    const node_tags = tree.nodes.items(.tag);
    const token_tags = tree.tokens.items(.tag);
    const main_token = tree.nodes.items(.main_token)[container];

    const is_enum = token_tags[main_token] == .keyword_enum;

    if (doc.document_scope.findContainerScope(container)) |container_scope| {
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
            return DeclWithHandle{ .decl = candidate.value_ptr, .handle = doc };
        }

        if (self.resolveUse(arena, workspace, container_scope.uses, symbol, doc)) |result| {
            return result;
        }
        return null;
    }

    return null;
}

pub fn lookupSymbolGlobalTokenIndex(
    self: *Self,
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    handle: *Document,
    token_idx: u32,
) ?DeclWithHandle {
    // const token_with_index = handle.ast_context.tokenFromBytePos(source_index) orelse return null;
    const token = handle.ast_context.tokens[token_idx];
    const symbol = handle.ast_context.getTokenText(token);
    const innermost_scope_idx = handle.document_scope.innermostBlockScopeIndex(token.loc.start);

    var curr = innermost_scope_idx;
    while (curr >= 0) : (curr -= 1) {
        const scope = &handle.document_scope.scopes.items[curr];
        if (token.loc.start >= scope.range.start and token.loc.end <= scope.range.end) blk: {
            if (scope.decls.getEntry(symbol)) |candidate| {
                switch (candidate.value_ptr.*) {
                    .ast_node => |node| {
                        if (handle.ast_context.tree.nodes.items(.tag)[node].isContainerField()) break :blk;
                    },
                    .label_decl => break :blk,
                    else => {},
                }
                return DeclWithHandle{
                    .decl = candidate.value_ptr,
                    .handle = handle,
                };
            }

            if (self.resolveUse(arena, workspace, scope.uses, symbol, handle)) |result| {
                return result;
            }
        }
        if (curr == 0) break;
    }
    return null;
}

fn resolveVarDeclAliasInternal(
    self: *Self,
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    handle: *Document,
    node: Ast.Node.Index,
    root: bool,
) ?DeclWithHandle {
    _ = root;
    const tree = handle.ast_context.tree;
    const node_tags = tree.nodes.items(.tag);
    const main_tokens = tree.nodes.items(.main_token);
    const datas = tree.nodes.items(.data);

    if (node_tags[node] == .identifier) {
        const token = main_tokens[node];
        return self.lookupSymbolGlobalTokenIndex(
            arena,
            workspace,
            handle,
            token,
        );
    }

    if (node_tags[node] == .field_access) {
        const lhs = datas[node].lhs;

        var container_node: Ast.Node.Index = undefined;
        var container_handle: *Document = undefined;
        if (ast.isBuiltinCall(tree, lhs)) {
            if (!std.mem.eql(u8, tree.tokenSlice(main_tokens[lhs]), "@import")) {
                return null;
            }

            const inner_node = TypeWithHandle.resolveTypeOfNode(arena, workspace, handle, lhs) orelse {
                return null;
            };
            // assert root node
            std.debug.assert(inner_node.type.data.other == 0);
            container_handle = inner_node.handle;
            container_node = inner_node.type.data.other;
        } else if (self.resolveVarDeclAliasInternal(arena, workspace, handle, lhs, false)) |decl_handle| {
            if (decl_handle.decl.* != .ast_node) return null;
            const resolved = TypeWithHandle.resolveTypeOfNode(arena, workspace, decl_handle.handle, decl_handle.decl.ast_node) orelse {
                return null;
            };
            const resolved_node = switch (resolved.type.data) {
                .other => |n| n,
                else => return null,
            };
            if (!ast.isContainer(resolved.handle.ast_context.tree, resolved_node)) return null;
            container_handle = resolved.handle;
            container_node = resolved_node;
        } else {
            return null;
        }

        return self.lookupSymbolContainer(
            arena,
            workspace,
            container_handle,
            container_node,
            tree.tokenSlice(datas[node].rhs),
            false,
        );
    }
    return null;
}

/// Resolves variable declarations consisting of chains of imports and field accesses of containers, ending with the same name as the variable decl's name
/// Examples:
///```zig
/// const decl = @import("decl-file.zig").decl;
/// const other = decl.middle.other;
///```
pub fn resolveVarDeclAlias(
    self: *Self,
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    handle: *Document,
    decl: Ast.Node.Index,
) ?DeclWithHandle {
    const tree = handle.ast_context.tree;
    const token_tags = tree.tokens.items(.tag);
    const node_tags = tree.nodes.items(.tag);

    if (ast.varDecl(handle.ast_context.tree, decl)) |var_decl| {
        if (var_decl.ast.init_node == 0) return null;
        const base_exp = var_decl.ast.init_node;
        if (token_tags[var_decl.ast.mut_token] != .keyword_const) return null;

        if (node_tags[base_exp] == .field_access) {
            const name = tree.tokenSlice(tree.nodes.items(.data)[base_exp].rhs);
            if (!std.mem.eql(u8, tree.tokenSlice(var_decl.ast.mut_token + 1), name))
                return null;

            return self.resolveVarDeclAliasInternal(arena, workspace, handle, base_exp, true);
        }
    }

    return null;
}

pub fn getSymbolFieldAccess(
    self: *Self,
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    doc: *Document,
    token_idx: u32,
) ?DeclWithHandle {
    const idx = doc.ast_context.tokens_node[token_idx];
    const tag = doc.ast_context.tree.nodes.items(.tag);
    std.debug.assert(tag[idx] == .field_access);
    const result = FieldAccessReturn.getFieldAccessType(
        arena,
        workspace,
        doc,
        token_idx,
    ) orelse {
        return null;
    };

    const container_handle = result.unwrapped orelse result.original;
    const container_handle_node = switch (container_handle.type.data) {
        .other => |n| n,
        else => return null,
    };
    const token = doc.ast_context.tokens[token_idx];
    const name = doc.ast_context.getTokenText(token);

    return self.lookupSymbolContainer(
        arena,
        workspace,
        container_handle.handle,
        container_handle_node,
        name,
        true,
    );
}

pub fn lookupIdentifier(
    self: *Self,
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    doc: *Document,
    token_index: u32,
) ?DeclWithHandle {
    const node = AstNode.fromTokenIndex(doc.ast_context, token_index);
    _ = node;
    const tag = doc.ast_context.tree.nodes.items(.tag);
    const idx = doc.ast_context.tokens_node[token_index];
    const node_tag = tag[idx];
    switch (node_tag) {
        .identifier => {
            return self.lookupSymbolGlobalTokenIndex(arena, workspace, doc, token_index);
        },
        .field_access => {
            return self.getSymbolFieldAccess(arena, workspace, doc, token_index);
        },
        else => {
            return null;
        },
    }
}
