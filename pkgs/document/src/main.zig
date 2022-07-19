const std = @import("std");
pub const Session = @import("./Session.zig");
pub const Workspace = @import("./Workspace.zig");
pub const Config = @import("./Config.zig");
pub const Stdio = @import("./Stdio.zig");
pub const Builtin = @import("./Builtin.zig");
// pub const server = @import("server.zig");
pub const LanguageServer = @import("./LanguageServer.zig");

const analysis = @import("./analysis.zig");
const builtin_completions = @import("./builtin_completions.zig");

pub fn init(allocator: std.mem.Allocator, builtins: []const Builtin, config: *Config) void {
    analysis.init(allocator);
    builtin_completions.init(allocator, builtins, config);
}

pub fn deinit() void {
    builtin_completions.deinit();
    analysis.deinit();
}
