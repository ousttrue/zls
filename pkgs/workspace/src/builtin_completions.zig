const std = @import("std");
const lsp = @import("lsp");
const Config = @import("./Config.zig");
const shared = @import("./shared.zig");
const Builtin = @import("./Builtin.zig");

pub fn truncateCompletions(list: []lsp.CompletionItem, max_detail_length: usize) void {
    for (list) |*item| {
        if (item.detail) |det| {
            if (det.len > max_detail_length) {
                item.detail = det[0..max_detail_length];
            }
        }
    }
}

var g_builtins: []const Builtin = undefined;
var g_allocator: std.mem.Allocator = undefined;
var g_builtin_completions: []lsp.CompletionItem = undefined;

pub fn data() []const Builtin {
    return g_builtins;
}

pub fn init(allocator: std.mem.Allocator, builtins: []const Builtin, config: *Config) void {
    g_allocator = allocator;
    g_builtins = builtins;
    g_builtin_completions = allocator.alloc(lsp.CompletionItem, g_builtins.len) catch unreachable;
    for (g_builtins) |builtin, idx| {
        g_builtin_completions[idx] = lsp.CompletionItem{
            .label = builtin.name,
            .kind = .Function,
            .filterText = builtin.name[1..],
            .detail = builtin.signature,
            .documentation = .{
                .kind = .Markdown,
                .value = builtin.documentation,
            },
        };

        var insert_text: []const u8 = undefined;
        if (config.enable_snippets) {
            insert_text = builtin.snippet;
            g_builtin_completions[idx].insertTextFormat = .Snippet;
        } else {
            insert_text = builtin.name;
        }
        g_builtin_completions[idx].insertText =
            if (config.include_at_in_builtins)
            insert_text
        else
            insert_text[1..];
    }
    truncateCompletions(g_builtin_completions, config.max_detail_length);
}

pub fn deinit() void {
    g_allocator.free(g_builtin_completions);
}

pub fn completeBuiltin() []lsp.CompletionItem {
    return g_builtin_completions;
}

pub fn find(name: []const u8) ?Builtin {
    for (data()) |builtin| {
        if (std.mem.eql(u8, builtin.name, name)) {
            return builtin;
        }
    }
    return null;
}
