const std = @import("std");
const astutil = @import("astutil");
const Ast = std.zig.Ast;
const Document = astutil.Document;
const ast = astutil.ast;

pub const Declaration = union(enum) {
    /// Index of the ast node
    ast_node: Ast.Node.Index,
    /// Function parameter
    param_decl: Ast.full.FnProto.Param,
    pointer_payload: struct {
        name: Ast.TokenIndex,
        condition: Ast.Node.Index,
    },
    array_payload: struct {
        identifier: Ast.TokenIndex,
        array_expr: Ast.Node.Index,
    },
    array_index: Ast.TokenIndex,
    switch_payload: struct {
        node: Ast.TokenIndex,
        switch_expr: Ast.Node.Index,
        items: []const Ast.Node.Index,
    },
    label_decl: Ast.TokenIndex,
};

pub const Data = union(enum) {
    container: Ast.Node.Index, // .tag is ContainerDecl or Root or ErrorSetDecl
    function: Ast.Node.Index, // .tag is FnProto
    block: Ast.Node.Index, // .tag is Block
    other,
};

const Self = @This();

range: std.zig.Token.Loc,
decls: std.StringHashMap(Declaration),
tests: []const Ast.Node.Index = &.{},
uses: []const *const Ast.Node.Index = &.{},

data: Data,
pub fn toNodeIndex(self: Self) ?Ast.Node.Index {
    return switch (self.data) {
        .container, .function, .block => |idx| idx,
        else => null,
    };
}
