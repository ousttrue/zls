const std = @import("std");
pub const Workspace = @import("./Workspace.zig");
pub const Document = @import("./Document.zig");
pub const DocumentPosition = @import("./DocumentPosition.zig");
pub const position_context = @import("./position_context.zig");
pub const Config = @import("./Config.zig");
pub const Stdio = @import("./Stdio.zig");
pub const Builtin = @import("./Builtin.zig");
pub const ClientCapabilities = @import("./ClientCapabilities.zig");
pub const semantic_tokens = @import("./semantic_tokens.zig");
pub const offsets = @import("./offsets.zig");
pub const document_symbols = @import("./document_symbols.zig");
pub const hover_util = @import("./hover_util.zig");
pub const completion_util = @import("./completion_util.zig");
pub const rename = @import("./rename.zig");
pub const analysis = @import("./analysis.zig");
const builtin_completions = @import("./builtin_completions.zig");

pub fn init(allocator: std.mem.Allocator, builtins: []const Builtin, config: *Config) void {
    analysis.init(allocator);
    builtin_completions.init(allocator, builtins, config);
}

pub fn deinit() void {
    builtin_completions.deinit();
    analysis.deinit();
}
