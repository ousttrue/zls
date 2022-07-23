const std = @import("std");
const Ast = std.zig.Ast;
const Workspace = @import("./Workspace.zig");
const Document = @import("./Document.zig");
const Scope = @import("./Scope.zig");
const Declaration = Scope.Declaration;
const offsets = @import("./offsets.zig");
const ast = @import("./ast.zig");
const Self = @This();
const TypeWithHandle = @import("./TypeWithHandle.zig");
const BoundTypeParams = std.AutoHashMap(Ast.full.FnProto.Param, TypeWithHandle);

decl: *Declaration,
handle: *Document,

pub fn nameToken(self: Self) Ast.TokenIndex {
    const tree = self.handle.tree;
    return switch (self.decl.*) {
        .ast_node => |n| ast.getDeclNameToken(tree, n).?,
        .param_decl => |p| p.name_token.?,
        .pointer_payload => |pp| pp.name,
        .array_payload => |ap| ap.identifier,
        .array_index => |ai| ai,
        .switch_payload => |sp| sp.node,
        .label_decl => |ld| ld,
    };
}

pub fn location(self: Self, encoding: offsets.Encoding) !offsets.TokenLocation {
    const tree = self.handle.tree;
    return try offsets.tokenRelativeLocation(tree, 0, tree.tokens.items(.start)[self.nameToken()], encoding);
}

pub fn isNodePublic(tree: Ast, node: Ast.Node.Index) bool {
    var buf: [1]Ast.Node.Index = undefined;
    return switch (tree.nodes.items(.tag)[node]) {
        .global_var_decl,
        .local_var_decl,
        .simple_var_decl,
        .aligned_var_decl,
        => ast.varDecl(tree, node).?.visib_token != null,
        .fn_proto,
        .fn_proto_multi,
        .fn_proto_one,
        .fn_proto_simple,
        .fn_decl,
        => ast.fnProto(tree, node, &buf).?.visib_token != null,
        else => true,
    };
}

pub fn isPublic(self: Self) bool {
    return switch (self.decl.*) {
        .ast_node => |node| isNodePublic(self.handle.tree, node),
        else => true,
    };
}

pub fn resolveType(self: Self, arena: *std.heap.ArenaAllocator, workspace: *Workspace, bound_type_params: *BoundTypeParams) !?TypeWithHandle {
    const tree = self.handle.tree;
    const node_tags = tree.nodes.items(.tag);
    const main_tokens = tree.nodes.items(.main_token);
    return switch (self.decl.*) {
        .ast_node => |node| try TypeWithHandle.resolveTypeOfNodeInternal(
            arena,
            workspace,
            self.handle,
            node,
            bound_type_params,
        ),
        .param_decl => |param_decl| {
            if (TypeWithHandle.isMetaType(self.handle.tree, param_decl.type_expr)) {
                var bound_param_it = bound_type_params.iterator();
                while (bound_param_it.next()) |entry| {
                    if (std.meta.eql(entry.key_ptr.*, param_decl)) return entry.value_ptr.*;
                }
                return null;
            } else if (node_tags[param_decl.type_expr] == .identifier) {
                if (param_decl.name_token) |name_tok| {
                    if (std.mem.eql(u8, tree.tokenSlice(main_tokens[param_decl.type_expr]), tree.tokenSlice(name_tok)))
                        return null;
                }
            }
            return ((try TypeWithHandle.resolveTypeOfNodeInternal(
                arena,
                workspace,
                self.handle,
                param_decl.type_expr,
                bound_type_params,
            )) orelse return null).instanceTypeVal();
        },
        .pointer_payload => |pay| try TypeWithHandle.resolveUnwrapOptionalType(
            arena,
            workspace,
            (try TypeWithHandle.resolveTypeOfNodeInternal(
                arena,
                workspace,
                self.handle,
                pay.condition,
                bound_type_params,
            )) orelse return null,
            bound_type_params,
        ),
        .array_payload => |pay| try TypeWithHandle.resolveBracketAccessType(
            arena,
            workspace,
            (try TypeWithHandle.resolveTypeOfNodeInternal(
                arena,
                workspace,
                self.handle,
                pay.array_expr,
                bound_type_params,
            )) orelse return null,
            .Single,
            bound_type_params,
        ),
        .array_index => TypeWithHandle{
            .type = .{ .data = .primitive, .is_type_val = false },
            .handle = self.handle,
        },
        .label_decl => return null,
        .switch_payload => |pay| {
            if (pay.items.len == 0) return null;
            // TODO Peer type resolution, we just use the first item for now.
            const switch_expr_type = (try TypeWithHandle.resolveTypeOfNodeInternal(arena, workspace, self.handle, pay.switch_expr, bound_type_params)) orelse return null;
            if (!switch_expr_type.isUnionType())
                return null;

            if (node_tags[pay.items[0]] == .enum_literal) {
                const scope = Scope.findContainerScope(switch_expr_type.handle, switch_expr_type.type.data.other) orelse return null;
                if (scope.decls.getEntry(tree.tokenSlice(main_tokens[pay.items[0]]))) |candidate| {
                    switch (candidate.value_ptr.*) {
                        .ast_node => |node| {
                            if (ast.containerField(switch_expr_type.handle.tree, node)) |container_field| {
                                if (container_field.ast.type_expr != 0) {
                                    return ((try TypeWithHandle.resolveTypeOfNodeInternal(
                                        arena,
                                        workspace,
                                        switch_expr_type.handle,
                                        container_field.ast.type_expr,
                                        bound_type_params,
                                    )) orelse return null).instanceTypeVal();
                                }
                            }
                        },
                        else => {},
                    }
                    return null;
                }
            }
            return null;
        },
    };
}

var using_trail: std.ArrayList([*]const u8) = undefined;
pub fn init(allocator: std.mem.Allocator) void {
    using_trail = std.ArrayList([*]const u8).init(allocator);
}
pub fn deinit() void {
    using_trail.deinit();
}

pub fn resolveUse(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    uses: []const *const Ast.Node.Index,
    symbol: []const u8,
    handle: *Document,
) error{OutOfMemory}!?Self {
    // If we were asked to resolve this symbol before,
    // it is self-referential and we cannot resolve it.
    if (std.mem.indexOfScalar([*]const u8, using_trail.items, symbol.ptr) != null)
        return null;
    try using_trail.append(symbol.ptr);
    defer _ = using_trail.pop();

    for (uses) |use| {
        const index = use.*;

        if (handle.tree.nodes.items(.data).len <= index) continue;

        const expr_type_node = (try TypeWithHandle.resolveTypeOfNode(
            arena,
            workspace,
            handle,
            handle.tree.nodes.items(.data)[index].lhs,
        )) orelse
            continue;

        const node = switch (expr_type_node.type.data) {
            .other => |n| n,
            else => continue,
        };

        if (try lookupSymbolContainer(
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
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    handle: *Document,
    container: Ast.Node.Index,
    symbol: []const u8,
    /// If true, we are looking up the symbol like we are accessing through a field access
    /// of an instance of the type, otherwise as a field access of the type value itself.
    instance_access: bool,
) error{OutOfMemory}!?Self {
    const tree = handle.tree;
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
            return Self{ .decl = candidate.value_ptr, .handle = handle };
        }

        if (try resolveUse(arena, workspace, container_scope.uses, symbol, handle)) |result| return result;
        return null;
    }

    return null;
}

fn resolveVarDeclAliasInternal(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    handle: *Document,
    node: Ast.Node.Index,
    root: bool,
) error{OutOfMemory}!?Self {
    _ = root;
    const tree = handle.tree;
    const node_tags = tree.nodes.items(.tag);
    const main_tokens = tree.nodes.items(.main_token);
    const datas = tree.nodes.items(.data);

    if (node_tags[node] == .identifier) {
        const token = main_tokens[node];
        return try workspace.lookupSymbolGlobal(
            arena,
            handle,
            tree.tokenSlice(token),
            tree.tokens.items(.start)[token],
        );
    }

    if (node_tags[node] == .field_access) {
        const lhs = datas[node].lhs;

        var container_node: Ast.Node.Index = undefined;
        var container_handle: *Document = undefined;
        if (ast.isBuiltinCall(tree, lhs)) {
            if (!std.mem.eql(u8, tree.tokenSlice(main_tokens[lhs]), "@import"))
                return null;

            const inner_node = (try TypeWithHandle.resolveTypeOfNode(arena, workspace, handle, lhs)) orelse return null;
            // assert root node
            std.debug.assert(inner_node.type.data.other == 0);
            container_handle = inner_node.handle;
            container_node = inner_node.type.data.other;
        } else if (try resolveVarDeclAliasInternal(arena, workspace, handle, lhs, false)) |decl_handle| {
            if (decl_handle.decl.* != .ast_node) return null;
            const resolved = (try TypeWithHandle.resolveTypeOfNode(arena, workspace, decl_handle.handle, decl_handle.decl.ast_node)) orelse return null;
            const resolved_node = switch (resolved.type.data) {
                .other => |n| n,
                else => return null,
            };
            if (!ast.isContainer(resolved.handle.tree, resolved_node)) return null;
            container_handle = resolved.handle;
            container_node = resolved_node;
        } else {
            return null;
        }

        return try lookupSymbolContainer(
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
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    handle: *Document,
    decl: Ast.Node.Index,
) !?Self {
    const tree = handle.tree;
    const token_tags = tree.tokens.items(.tag);
    const node_tags = tree.nodes.items(.tag);

    if (ast.varDecl(handle.tree, decl)) |var_decl| {
        if (var_decl.ast.init_node == 0) return null;
        const base_exp = var_decl.ast.init_node;
        if (token_tags[var_decl.ast.mut_token] != .keyword_const) return null;

        if (node_tags[base_exp] == .field_access) {
            const name = tree.tokenSlice(tree.nodes.items(.data)[base_exp].rhs);
            if (!std.mem.eql(u8, tree.tokenSlice(var_decl.ast.mut_token + 1), name))
                return null;

            return try resolveVarDeclAliasInternal(arena, workspace, handle, base_exp, true);
        }
    }

    return null;
}
