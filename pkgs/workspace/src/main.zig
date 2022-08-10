const std = @import("std");
const astutil = @import("astutil");
pub const Workspace = @import("./Workspace.zig");
pub const Line = astutil.Line;
pub const Config = @import("./Config.zig");
pub const ZigEnv = @import("./ZigEnv.zig");
pub const Builtin = @import("./Builtin.zig");
pub const semantic_tokens = @import("./semantic_tokens.zig");
pub const SemanticTokensBuilder = @import("./SemanticTokensBuilder.zig");
pub const ast = astutil.ast;
pub const builtin_completions = @import("./builtin_completions.zig");

pub fn init(allocator: std.mem.Allocator, builtins: []const Builtin, config: *Config) void {
    builtin_completions.init(allocator, builtins, config);
}

pub fn deinit() void {
    builtin_completions.deinit();
}
