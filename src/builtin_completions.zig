const std = @import("std");
const lsp = @import("lsp");
const build_options = @import("build_options");
const Config = @import("./Config.zig");

pub const data = switch (build_options.data_version) {
    .master => @import("data/master.zig"),
    .@"0.7.0" => @import("data/0.7.0.zig"),
    .@"0.7.1" => @import("data/0.7.1.zig"),
    .@"0.8.0" => @import("data/0.8.0.zig"),
    .@"0.8.1" => @import("data/0.8.1.zig"),
    .@"0.9.0" => @import("data/0.9.0.zig"),
};

pub fn truncateCompletions(list: []lsp.CompletionItem, max_detail_length: usize) void {
    for (list) |*item| {
        if (item.detail) |det| {
            if (det.len > max_detail_length) {
                item.detail = det[0..max_detail_length];
            }
        }
    }
}

pub const Completion = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    builtin_completions: ?[]lsp.CompletionItem = null,

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Self) void {
        if (self.builtin_completions) |compls| {
            self.allocator.free(compls);
        }
    }

    pub fn completeBuiltin(self: *Self, id: i64, config: *Config) !lsp.Response {        
        if (self.builtin_completions == null) {
            var builtin_completions = try self.allocator.alloc(lsp.CompletionItem, data.builtins.len);
            self.builtin_completions = builtin_completions;
            for (data.builtins) |builtin, idx| {
                builtin_completions[idx] = lsp.CompletionItem{
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
                    builtin_completions[idx].insertTextFormat = .Snippet;
                } else {
                    insert_text = builtin.name;
                }
                builtin_completions[idx].insertText =
                    if (config.include_at_in_builtins)
                    insert_text
                else
                    insert_text[1..];
            }
            truncateCompletions(builtin_completions, config.max_detail_length);
        }

        return lsp.Response{
            .id = id,
            .result = .{
                .CompletionList = .{
                    .isIncomplete = false,
                    .items = self.builtin_completions.?,
                },
            },
        };
    }
};
