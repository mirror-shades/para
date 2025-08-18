const std = @import("std");

pub fn build(b: *std.Build) void {
    const optimize = b.standardOptimizeOption(.{});
    const target = b.standardTargetOptions(.{});

    const exe = b.addExecutable(.{
        .name = "para",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    b.installArtifact(exe);

    // Add test step
    const test_exe = b.addTest(.{
        .name = "test",
        .root_source_file = b.path("test/test.zig"),
    });
    const run_test = b.addRunArtifact(test_exe);
    const test_step = b.step("test", "Run the tests");
    test_step.dependOn(&run_test.step);
}
