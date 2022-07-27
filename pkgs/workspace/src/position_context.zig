const std = @import("std");

pub const PositionContext = union(enum) {
    builtin: std.zig.Token.Loc,
    string_literal: std.zig.Token.Loc,
    field_access: std.zig.Token.Loc,
    var_access: std.zig.Token.Loc,
    global_error_set,
    enum_literal,
    // pre_label,
    label: bool,
    // other,
    keyword,
    empty,
};
