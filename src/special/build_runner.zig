///! This is a modified build runner to extract information out of build.zig
///! Modified from the std.special.build_runner
const root = @import("@build@");
const std = @import("std");
const fmt = std.fmt;
const io = std.io;
const log = std.log;
const process = std.process;
const Builder = std.build.Builder;
const Pkg = std.build.Pkg;
const InstallArtifactStep = std.build.InstallArtifactStep;
const LibExeObjStep = std.build.LibExeObjStep;
const ArrayList = std.ArrayList;

const NamePath = struct {
    name: []const u8,
    path: []const u8,
};

const Project = struct {
    // LibExeObjStep
    objects: []const NamePath,
    // Pkg
    packages: []const NamePath,
};

///
/// path_to_zig/zig.exe run path_to_zls/zig-out/bin/build_runner.zig --pkg-begin @build@ path_to_project/build.zig --pkg-end -- arg1 arg2 arg3 arg4
///
pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    var args = try process.argsAlloc(allocator);
    defer process.argsFree(allocator, args);

    // skip my own exe name
    var arg_idx: usize = 1;

    const zig_exe = nextArg(args, &arg_idx) orelse {
        log.warn("Expected first argument to be path to zig compiler\n", .{});
        return error.InvalidArgs;
    };
    const build_root = nextArg(args, &arg_idx) orelse {
        log.warn("Expected second argument to be build root directory path\n", .{});
        return error.InvalidArgs;
    };
    const cache_root = nextArg(args, &arg_idx) orelse {
        log.warn("Expected third argument to be cache root directory path\n", .{});
        return error.InvalidArgs;
    };
    const global_cache_root = nextArg(args, &arg_idx) orelse {
        log.warn("Expected third argument to be global cache root directory path\n", .{});
        return error.InvalidArgs;
    };

    const builder = try Builder.create(
        allocator,
        zig_exe,
        build_root,
        cache_root,
        global_cache_root,
    );

    defer builder.destroy();

    builder.resolveInstallPrefix(null, Builder.DirList{});
    try runBuild(builder);

    // TODO: We currently add packages from every LibExeObj step that the install step depends on.
    //       Should we error out or keep one step or something similar?
    // We also flatten them, we should probably keep the nested structure.
    var objects = std.ArrayList(NamePath).init(allocator);
    defer objects.deinit();
    var packages = std.ArrayList(NamePath).init(allocator);
    defer packages.deinit();
    for (builder.top_level_steps.items) |tls| {
        for (tls.step.dependencies.items) |step| {
            try processStep(step, &objects, &packages);
        }
    }

    // write json
    try std.json.stringify(Project{
        .objects = objects.items,
        .packages = packages.items,
    }, .{ .emit_null_optional_fields = false }, io.getStdOut().writer());
}

fn fileSourcePath(source: std.build.FileSource) ?[]const u8 {
    return switch (source) {
        .path => |path| path,
        .generated => |generated| generated.path,
    };
}

fn processStep(
    step: *std.build.Step,
    objects: *std.ArrayList(NamePath),
    packages: *std.ArrayList(NamePath),
) anyerror!void {
    if (step.cast(InstallArtifactStep)) |install_exe| {
        const exe = install_exe.artifact;
        if (exe.root_src) |root_src| {
            if (fileSourcePath(root_src)) |path| {
                try objects.append(.{
                    .name = exe.name,
                    .path = path,
                });
            }
        }       
        for (install_exe.artifact.packages.items) |pkg| {
            try processPackage(pkg, packages);
        }
    } else if (step.cast(LibExeObjStep)) |exe| {
        if (exe.root_src) |root_src| {
            if (fileSourcePath(root_src)) |path| {
                try objects.append(.{
                    .name = exe.name,
                    .path = path,
                });
            }
        }
        for (exe.packages.items) |pkg| {
            try processPackage(pkg, packages);
        }
    } else {
        for (step.dependencies.items) |unknown_step| {
            try processStep(unknown_step, objects, packages);
        }
    }
}

fn processPackage(pkg: Pkg, packages: *std.ArrayList(NamePath)) anyerror!void {
    if (fileSourcePath(pkg.source)) |path| {
        try packages.append(.{
            .name = pkg.name,
            .path = path,
        });
    }
    if (pkg.dependencies) |dependencies| {
        for (dependencies) |dep| {
            try processPackage(dep, packages);
        }
    }
}

fn runBuild(builder: *Builder) anyerror!void {
    switch (@typeInfo(@typeInfo(@TypeOf(root.build)).Fn.return_type.?)) {
        .Void => root.build(builder),
        .ErrorUnion => try root.build(builder),
        else => @compileError("expected return type of build to be 'void' or '!void'"),
    }
}

fn nextArg(args: [][]const u8, idx: *usize) ?[]const u8 {
    if (idx.* >= args.len) return null;
    defer idx.* += 1;
    return args[idx.*];
}
