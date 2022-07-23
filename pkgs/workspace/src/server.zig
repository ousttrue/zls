const std = @import("std");
const Document = @import("./Document.zig");
const requests = @import("lsp").requests;
const lsp = @import("lsp");
const analysis = @import("./analysis.zig");
const ast = @import("./ast.zig");
const offsets = @import("./offsets.zig");
const Ast = std.zig.Ast;
const builtin_completions = @import("./builtin_completions.zig");

const logger = std.log.scoped(.server);

// // STYLE

// pub fn isCamelCase(name: []const u8) bool {
//     return !std.ascii.isUpper(name[0]) and !isSnakeCase(name);
// }

// pub fn isPascalCase(name: []const u8) bool {
//     return std.ascii.isUpper(name[0]) and !isSnakeCase(name);
// }

// pub fn isSnakeCase(name: []const u8) bool {
//     return std.mem.indexOf(u8, name, "_") != null;
// }

