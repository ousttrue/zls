const astutil = @import("astutil");
const Project = astutil.Project;
const AstNode = astutil.AstNode;
const Self = @This();

pub fn init(project: ?Project, node: AstNode) ?Self {
    _ = project;
    _ = node;

    var buf: [2]u32 = undefined;
    switch (node.getChildren(&buf)) {
        .var_decl => {

            // rhs: init_node
            // get container_decl

        },
        .container_field => {

            // type_expr
            // get container_decl

        },
        else => {},
    }

    return null;
}

pub fn next(self: *Self) ?AstNode {
    _ = self;
    return null;
}
