const std = @import("std");
const zig_builtin = @import("builtin");
const build_options = @import("build_options");
const types = @import("./types.zig");
const server = @import("server.zig");

const allocator = std.heap.page_allocator;

// Always set this to debug to make std.log call into our handler, then control the runtime
// value in the definition below.
pub const log_level = .debug;

var actual_log_level: std.log.Level = switch (zig_builtin.mode) {
    .Debug => .debug,
    else => @intToEnum(std.log.Level, @enumToInt(build_options.log_level)), //temporary fix to build failing on release-safe due to a Zig bug
};

pub fn log(comptime message_level: std.log.Level, comptime scope: @Type(.EnumLiteral), comptime format: []const u8, args: anytype) void {
    if (@enumToInt(message_level) > @enumToInt(actual_log_level)) {
        return;
    }
    // After shutdown, pipe output to stderr
    if (!server.keep_running) {
        std.debug.print("[{s}-{s}] " ++ format ++ "\n", .{ @tagName(message_level), @tagName(scope) } ++ args);
        return;
    }

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    var message = std.fmt.allocPrint(arena.allocator(), "[{s}-{s}] " ++ format, .{ @tagName(message_level), @tagName(scope) } ++ args) catch {
        std.debug.print("Failed to allocPrint message.\n", .{});
        return;
    };

    const message_type: types.MessageType = switch (message_level) {
        .debug => .Log,
        .info => .Info,
        .warn => .Warning,
        .err => .Error,
    };
    server.send(&arena, types.Notification{
        .method = "window/logMessage",
        .params = types.Notification.Params{
            .LogMessage = .{
                .type = message_type,
                .message = message,
            },
        },
    }) catch |err| {
        std.debug.print("Failed to send show message notification (error: {}).\n", .{err});
    };
}

pub fn main() anyerror!void {
    try server.run();
}
