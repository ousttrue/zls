//! Configuration options for zls.
//! zls.json for std.json.parse

const std = @import("std");
const logger = std.log.scoped(.Config);
const Self = @This();

/// Whether to enable snippet completions
enable_snippets: bool = false,

/// zig library path
zig_lib_path: ?[]const u8 = null,

/// zig executable path used to run the custom build runner.
/// May be used to find a lib path if none is provided.
zig_exe_path: ?[]const u8 = null,

/// Whether to pay attention to style issues. This is opt-in since the style
/// guide explicitly states that the style info provided is a guideline only.
warn_style: bool = false,

/// Path to the build_runner.zig file.
build_runner_path: ?[]const u8 = null,

/// Path to a directory that will be used as cache when `zig run`ning the build runner
build_runner_cache_path: ?[]const u8 = null,

/// Semantic token support
enable_semantic_tokens: bool = true,

/// Whether to enable `*` and `?` operators in completion lists
operator_completions: bool = true,

/// Whether the @ sign should be part of the completion of builtins
include_at_in_builtins: bool = false,

/// The detail field of completions is truncated to be no longer than this (in bytes).
max_detail_length: usize = 1024 * 1024,

/// Skips references to std. This will improve lookup speeds.
/// Going to definition however will continue to work
skip_std_references: bool = false,

builtin_path: ?[]const u8 = null,

pub fn load(allocator: std.mem.Allocator, path: []const u8) ?Self {
    logger.info("config_path: {s}", .{path});
    var file = std.fs.cwd().openFile(path, .{}) catch |err| {
        if (err != error.FileNotFound) {
            logger.warn("Error while reading configuration file: {}", .{err});
        }
        return null;
    };
    defer file.close();

    const file_buf = file.readToEndAlloc(allocator, 0x1000000) catch return null;
    defer allocator.free(file_buf);
    @setEvalBranchQuota(3000);
    // TODO: Better errors? Doesn't seem like std.json can provide us positions or context.
    var config = std.json.parse(Self, &std.json.TokenStream.init(file_buf), .{ .allocator = allocator }) catch |err| {
        logger.warn("Error while parsing configuration file: {s} {}", .{ path, err });
        return null;
    };

    if (config.zig_lib_path) |zig_lib_path| {
        if (!std.fs.path.isAbsolute(zig_lib_path)) {
            logger.warn("zig library path is not absolute, defaulting to null.", .{});
            allocator.free(zig_lib_path);
            config.zig_lib_path = null;
        }
    }

    return config;
}

pub fn loadInFolder(allocator: std.mem.Allocator, folder_path: []const u8) ?Self {
    const full_path = std.fs.path.resolve(allocator, &.{ folder_path, "zls.json" }) catch return null;
    defer allocator.free(full_path);
    return load(allocator, full_path);
}
