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
