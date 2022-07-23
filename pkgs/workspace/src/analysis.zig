const std = @import("std");
const Scope = @import("./Scope.zig");
const Declaration = Scope.Declaration;
const Workspace = @import("./Workspace.zig");
const Document = @import("./Document.zig");
const Config = @import("./Config.zig");
const ClientCapabilities = @import("./ClientCapabilities.zig");
const Ast = std.zig.Ast;
const TypeWithHandle = @import("./TypeWithHandle.zig");
const lsp = @import("lsp");
const offsets = @import("./offsets.zig");
const ast = @import("./ast.zig");
const DeclWithHandle = @import("./DeclWithHandle.zig");

/// Gets a function's keyword, name, arguments and return value.
pub fn getFunctionSignature(tree: Ast, func: Ast.full.FnProto) []const u8 {
    const start = offsets.tokenLocation(tree, func.ast.fn_token);

    const end = if (func.ast.return_type != 0)
        offsets.tokenLocation(tree, ast.lastToken(tree, func.ast.return_type))
    else
        start;
    return tree.source[start.start..end.end];
}

pub fn getVariableSignature(tree: Ast, var_decl: Ast.full.VarDecl) []const u8 {
    const start = offsets.tokenLocation(tree, var_decl.ast.mut_token).start;
    const end = offsets.tokenLocation(tree, ast.lastToken(tree, var_decl.ast.init_node)).end;
    return tree.source[start..end];
}

pub fn getContainerFieldSignature(tree: Ast, field: Ast.full.ContainerField) []const u8 {
    const start = offsets.tokenLocation(tree, field.ast.name_token).start;
    const end_node = if (field.ast.value_expr != 0) field.ast.value_expr else field.ast.type_expr;
    const end = offsets.tokenLocation(tree, ast.lastToken(tree, end_node)).end;
    return tree.source[start..end];
}

// ANALYSIS ENGINE
fn isContainerDecl(decl_handle: DeclWithHandle) bool {
    return switch (decl_handle.decl.*) {
        .ast_node => |inner_node| ast.isContainer(decl_handle.handle.tree.nodes.items(.tag)[inner_node]),
        else => false,
    };
}
