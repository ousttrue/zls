const astutil = @import("astutil");
const Project = astutil.Project;
const AstNode = astutil.AstNode;
const Self = @This();

pub fn init(project: ?Project, node: AstNode) ?Self {
    _ = project;
    _ = node;

    return null;
}

pub fn next(self: *Self) ?AstNode {
    _ = self;
    return null;
}
