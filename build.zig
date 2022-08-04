const std = @import("std");
const builtin = @import("builtin");
const shared = @import("./pkgs/workspace/src/shared.zig");

const known_folders_pkg = std.build.Pkg{
    .name = "known-folders",
    .source = .{ .path = "src/known-folders/known-folders.zig" },
};

const lsp_pkg = std.build.Pkg{
    .name = "lsp",
    .source = .{ .path = "pkgs/lsp/src/main.zig" },
};

const workspace_pkg = std.build.Pkg{
    .name = "workspace",
    .source = .{ .path = "pkgs/workspace/src/main.zig" },
    .dependencies = &.{lsp_pkg, known_folders_pkg},
};

const ls_pkg = std.build.Pkg{
    .name = "language_server",
    .source = .{ .path = "pkgs/language_server/src/main.zig" },
    .dependencies = &.{ lsp_pkg, workspace_pkg },
};

pub fn build(b: *std.build.Builder) !void {
    const target = b.standardTargetOptions(.{});

    const mode = b.standardReleaseOptions();
    const exe = b.addExecutable("zls", "src/main.zig");
    const exe_options = b.addOptions();
    exe.addOptions("build_options", exe_options);

    exe_options.addOption(
        shared.ZigVersion,
        "data_version",
        b.option(shared.ZigVersion, "data_version", "The Zig version your compiler is.") orelse .master,
    );

    exe_options.addOption(
        std.log.Level,
        "log_level",
        b.option(std.log.Level, "log_level", "The Log Level to be used.") orelse .info,
    );

    exe.addPackage(known_folders_pkg);
    exe.addPackage(.{ .name = "zinput", .source = .{ .path = "src/zinput/src/main.zig" } });
    exe.addPackage(lsp_pkg);
    exe.addPackage(workspace_pkg);
    exe.addPackage(ls_pkg);

    exe.setTarget(target);
    exe.setBuildMode(mode);
    exe.install();

    b.installFile("src/special/build_runner.zig", "bin/build_runner.zig");

    // const test_step = b.step("test", "Run all the tests");
    // test_step.dependOn(&tests.step);
    // test_step.dependOn(b.getInstallStep());

    // var unit_tests = b.addTest("src/unit_tests.zig");
    // unit_tests.setBuildMode(.Debug);
    // test_step.dependOn(&unit_tests.step);

    // var session_tests = b.addTest("tests/sessions.zig");
    // session_tests.addPackage(.{ .name = "header", .source = .{ .path = "src/header.zig" } });
    // session_tests.setBuildMode(.Debug);
    // test_step.dependOn(&session_tests.step);

    const tests = b.addTest("src/main.zig");
    tests.addPackage(lsp_pkg);
    tests.setTarget(target);
    tests.setBuildMode(mode);
    // tests.install();
    const test_step = b.step("test", "Run all the tests");
    test_step.dependOn(&tests.step);
    test_step.dependOn(b.getInstallStep());
}
