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
    test_exe.root_module.addImport("backend_escape", b.createModule(.{
        .root_source_file = b.path("src/backend/escape.zig"),
        .target = b.graph.host,
    }));

    const run_test = b.addRunArtifact(test_exe);
    const test_step = b.step("test", "Run the tests");

    const para_exe_name = if (b.graph.host.result.os.tag == .windows) "para.exe" else "para";
    const para_install_rel = b.pathJoin(&.{ "zig-out", "bin", para_exe_name });

    run_test.setCwd(b.path("."));
    run_test.setEnvironmentVariable("PARA_BIN", para_install_rel);
    run_test.step.dependOn(b.getInstallStep());
    test_step.dependOn(&run_test.step);
}
