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

    const test_exe = b.addTest(.{
        .name = "test",
        .root_module = b.createModule(.{
            .root_source_file = b.path("test/test.zig"),
            .target = b.graph.host,
        }),
    });

    const run_test = b.addRunArtifact(test_exe);
    const test_step = b.step("test", "Run the tests");

    run_test.step.dependOn(&exe.step);
    test_step.dependOn(&run_test.step);
}
