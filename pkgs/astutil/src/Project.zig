const ImportSolver = @import("./ImportSolver.zig");
const DocumentStore = @import("./DocumentStore.zig");
const Self = @This();

import_solver: ImportSolver,
store: *DocumentStore,

pub fn init(import_solver: ImportSolver, store: *DocumentStore) Self
{
    return Self{
        .import_solver = import_solver,
        .store = store,
    };
}
