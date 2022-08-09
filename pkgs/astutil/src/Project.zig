const ImportSolver = @import("./ImportSolver.zig");
const DocumentStore = @import("./DocumentStore.zig");
const Document = @import("./Document.zig");
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

pub fn resolveImport(self: *Self, doc: *Document, text: []const u8) !?*Document {
    return if (self.import_solver.solve(doc.path, ImportSolver.ImportType.fromText(text))) |path|
        try self.store.getOrLoad(path)
    else
        null;
}
