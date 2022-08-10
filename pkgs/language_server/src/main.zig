const std = @import("std");
const builtin_completions = @import("./builtin_completions.zig");
pub const Builtin = @import("./Builtin.zig");
pub const Config = @import("./Config.zig");
pub const LanguageServer = @import("./LanguageServer.zig");
pub const ZigEnv = @import("./ZigEnv.zig");

pub fn init(allocator: std.mem.Allocator, builtins: []const Builtin, config: *Config) void {
    builtin_completions.init(allocator, builtins, config);
}

pub fn deinit() void {
    builtin_completions.deinit();
}
