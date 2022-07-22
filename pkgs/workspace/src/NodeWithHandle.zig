const std = @import("std");
const Ast = std.zig.Ast;
const Document = @import("./Document.zig");
const Self = @This();

node: Ast.Node.Index,
handle: *Document,
