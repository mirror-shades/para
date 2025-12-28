const std = @import("std");

pub fn build(b: *std.Build) void {
    const exe = b.addExecutable(.{
        .name = "para",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = b.graph.host,
        }),
    });
    b.installArtifact(exe);

    // Add test step
    const test_exe = b.addTest(.{
        .name = "test",
        .root_module = b.createModule(.{
            .root_source_file = b.path("test/test.zig"),
            .target = b.graph.host,
        }),
    });

    const run_test = b.addRunArtifact(test_exe);
    const test_step = b.step("test", "Run the tests");

    // Tests spawn the `para` executable by path; install it to a dedicated
    // subdirectory so `zig build test` doesn't fail if `zig-out/bin/para.exe`
    // is currently in use on Windows.
    const install_test_para = b.addInstallArtifact(exe, .{
        .dest_sub_path = b.fmt("test-bin/{s}", .{exe.out_filename}),
    });
    test_step.dependOn(&install_test_para.step);
    run_test.step.dependOn(&exe.step);
    test_step.dependOn(&run_test.step);
}
