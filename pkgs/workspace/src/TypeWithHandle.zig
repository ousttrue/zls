const std = @import("std");
const Ast = std.zig.Ast;
const Document = @import("./Document.zig");
const ast = @import("./ast.zig");

pub const Type = struct {
    data: union(enum) {
        pointer: Ast.Node.Index,
        slice: Ast.Node.Index,
        error_union: Ast.Node.Index,
        other: Ast.Node.Index,
        primitive,
    },
    /// If true, the type `type`, the attached data is the value of the type value.
    is_type_val: bool,
};

pub const NodeWithHandle = struct {
    node: Ast.Node.Index,
    handle: *Document,
};

const Self = @This();

type: Type,
handle: *Document,

pub fn typeVal(node_handle: NodeWithHandle) Self {
    return .{
        .type = .{
            .data = .{ .other = node_handle.node },
            .is_type_val = true,
        },
        .handle = node_handle.handle,
    };
}

pub fn instanceTypeVal(self: Self) ?Self {
    if (!self.type.is_type_val) return null;
    return Self{
        .type = .{ .data = self.type.data, .is_type_val = false },
        .handle = self.handle,
    };
}

fn isRoot(self: Self) bool {
    switch (self.type.data) {
        // root is always index 0
        .other => |n| return n == 0,
        else => return false,
    }
}

fn isContainerKind(self: Self, container_kind_tok: std.zig.Token.Tag) bool {
    const tree = self.handle.tree;
    const main_tokens = tree.nodes.items(.main_token);
    const tags = tree.tokens.items(.tag);
    switch (self.type.data) {
        .other => |n| return tags[main_tokens[n]] == container_kind_tok,
        else => return false,
    }
}

pub fn isStructType(self: Self) bool {
    return self.isContainerKind(.keyword_struct) or self.isRoot();
}

pub fn isNamespace(self: Self) bool {
    if (!self.isStructType()) return false;
    const tree = self.handle.tree;
    const node = self.type.data.other;
    const tags = tree.nodes.items(.tag);
    if (ast.isContainer(tree, node)) {
        var buf: [2]Ast.Node.Index = undefined;
        for (ast.declMembers(tree, node, &buf)) |child| {
            if (tags[child].isContainerField()) return false;
        }
    }
    return true;
}

pub fn isEnumType(self: Self) bool {
    return self.isContainerKind(.keyword_enum);
}

pub fn isUnionType(self: Self) bool {
    return self.isContainerKind(.keyword_union);
}

pub fn isOpaqueType(self: Self) bool {
    return self.isContainerKind(.keyword_opaque);
}

/// The node is the meta-type `type`
pub fn isMetaType(tree: Ast, node: Ast.Node.Index) bool {
    if (tree.nodes.items(.tag)[node] == .identifier) {
        return std.mem.eql(u8, tree.tokenSlice(tree.nodes.items(.main_token)[node]), "type");
    }
    return false;
}

pub fn isTypeFunction(tree: Ast, func: Ast.full.FnProto) bool {
    return isMetaType(tree, func.ast.return_type);
}

pub fn isTypeFunc(self: Self) bool {
    var buf: [1]Ast.Node.Index = undefined;
    const tree = self.handle.tree;
    return switch (self.type.data) {
        .other => |n| if (ast.fnProto(tree, n, &buf)) |fn_proto| blk: {
            break :blk isTypeFunction(tree, fn_proto);
        } else false,
        else => false,
    };
}

pub fn isGenericFunction(tree: Ast, func: Ast.full.FnProto) bool {
    var it = func.iterate(&tree);
    while (it.next()) |param| {
        if (param.anytype_ellipsis3 != null or param.comptime_noalias != null) {
            return true;
        }
    }
    return false;
}

pub fn isGenericFunc(self: Self) bool {
    var buf: [1]Ast.Node.Index = undefined;
    const tree = self.handle.tree;
    return switch (self.type.data) {
        .other => |n| if (ast.fnProto(tree, n, &buf)) |fn_proto| blk: {
            break :blk isGenericFunction(tree, fn_proto);
        } else false,
        else => false,
    };
}

pub fn isFunc(self: Self) bool {
    const tree = self.handle.tree;
    const tags = tree.nodes.items(.tag);
    return switch (self.type.data) {
        .other => |n| switch (tags[n]) {
            .fn_proto,
            .fn_proto_multi,
            .fn_proto_one,
            .fn_proto_simple,
            .fn_decl,
            => true,
            else => false,
        },
        else => false,
    };
}
