const std = @import("std");
const astutil = @import("astutil");
const FixedPath = astutil.FixedPath;
const Self = @This();

path: FixedPath,
loc: std.zig.Token.Loc,
