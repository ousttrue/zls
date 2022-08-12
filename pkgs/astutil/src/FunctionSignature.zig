const std = @import("std");
const AstToken = @import("./AstToken.zig");
const AstNode = @import("./AstNode.zig");

const Arg = struct {
    name: []const u8,
    document: []const u8,
};

const Self = @This();

name: []const u8,
document: []const u8,
args: std.ArrayList(Arg),
return_type: []const u8,
param_count: u32 = 0,

pub fn init(allocator: std.mem.Allocator, name: []const u8, document: []const u8, return_type: []const u8) Self {
    return Self{
        .name = name,
        .document = document,
        .args = std.ArrayList(Arg).init(allocator),
        .return_type = return_type,
    };
}

pub fn deinit(self: Self) void {
    self.args.deinit();
}

pub fn fromFnDecl(allocator: std.mem.Allocator, node: AstNode) !Self {
    std.debug.assert(node.getTag() == .fn_decl);
    const fn_proto_node = AstNode.init(node.context, node.getData().lhs);
    var buf: [2]u32 = undefined;
    const fn_proto = fn_proto_node.getFnProto(&buf) orelse return error.NoFnProto;
    const name_token = AstToken.init(&node.context.tree, fn_proto.name_token.?);
    const return_type_node = AstNode.init(node.context, fn_proto.ast.return_type);

    var self = init(allocator, name_token.getText(), "", return_type_node.getText());
    var it = fn_proto.iterate(&fn_proto_node.context.tree);
    while (it.next()) |param| {
        try self.args.append(.{
            .name = AstToken.init(&fn_proto_node.context.tree, param.name_token.?).getText(),
            .document = AstNode.init(fn_proto_node.context, param.type_expr).getText(),
        });
    }
    return self;
}

pub fn allocPrint(self: Self, allocator: std.mem.Allocator) ![]const u8 {
    var buf = std.ArrayList(u8).init(allocator);
    const w = buf.writer();

    try w.print("fn {s}(", .{self.name});
    for (self.args.items) |arg, i| {
        if (i > 0) {
            try w.print(", ", .{});
        }
        try w.print("{s}: {s}", .{ arg.name, arg.document });
    }
    try w.print(") {s}\n\n", .{self.return_type});

    if (self.document.len > 0) {
        try w.print("{s}", .{self.document});
    }

    return buf.toOwnedSlice();
}
