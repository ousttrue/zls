const std = @import("std");
pub const Workspace = @import("./Workspace.zig");
pub const Document = @import("./Document.zig");
pub const Line = @import("./Line.zig");
pub const UriBytePosition = @import("./UriBytePosition.zig");
pub const Config = @import("./Config.zig");
pub const ZigEnv = @import("./ZigEnv.zig");
pub const Stdio = @import("./Stdio.zig");
pub const Builtin = @import("./Builtin.zig");
pub const ClientCapabilities = @import("./ClientCapabilities.zig");
pub const semantic_tokens = @import("./semantic_tokens.zig");
pub const SemanticTokensBuilder = @import("./SemanticTokensBuilder.zig");
pub const SymbolTree = @import("./SymbolTree.zig");
pub const hover_util = @import("./hover_util.zig");
pub const completion_util = @import("./completion_util.zig");
pub const ast = @import("./ast.zig");
pub const signature_help = @import("./signature_help.zig");
pub const builtin_completions = @import("./builtin_completions.zig");
pub const DeclWithHandle = @import("./DeclWithHandle.zig");
pub const TypeWithHandle = @import("./TypeWithHandle.zig");

pub fn init(allocator: std.mem.Allocator, builtins: []const Builtin, config: *Config) void {
    TypeWithHandle.init(allocator);
    DeclWithHandle.init(allocator);
    builtin_completions.init(allocator, builtins, config);
}

pub fn deinit() void {
    builtin_completions.deinit();
    TypeWithHandle.deinit();
    DeclWithHandle.deinit();
}
