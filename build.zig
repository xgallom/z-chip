const std = @import("std");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const zengine = b.dependency("zengine", .{});
    const z = @import("zengine");
    const options = z.getOptions(b);

    const exe = b.addExecutable(.{
        .name = "z-chip",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "zengine", .module = zengine.module("zengine") },
            },
        }),
    });
    b.installArtifact(exe);

    {
        const install_shaders_dir = try z.addCompileShaders(b, .{
            .b = zengine.builder,
            .module = zengine.module("zengine"),
            .options = options,
            .optimize = optimize,
        });
        b.getInstallStep().dependOn(&install_shaders_dir.step);
    }
    {
        const install_shaders_dir = try z.addCompileShaders(b, .{
            .b = zengine.builder,
            .src = b.path("shaders"),
            .module = zengine.module("zengine"),
            .options = options,
            .optimize = optimize,
        });
        b.getInstallStep().dependOn(&install_shaders_dir.step);
    }

    {
        const compiler_exe = b.addExecutable(.{
            .name = "z-chip-c",
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/compiler.zig"),
                .target = target,
                .optimize = optimize,
                .imports = &.{
                    .{ .name = "zengine", .module = zengine.module("zengine") },
                },
            }),
        });
        b.installArtifact(compiler_exe);

        const compiler_step = b.step("compiler", "Run the compiler");
        const compiler_cmd = b.addRunArtifact(compiler_exe);
        compiler_step.dependOn(&compiler_cmd.step);

        if (b.args) |args| {
            compiler_cmd.addArgs(args);
        }
    }

    const run_step = b.step("run", "Run the emulator");
    const run_cmd = b.addRunArtifact(exe);
    run_step.dependOn(&run_cmd.step);
    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const exe_tests = b.addTest(.{
        .root_module = exe.root_module,
    });
    const run_tests = b.addRunArtifact(exe_tests);

    const test_step = b.step("test", "Run tests");
    test_step.dependOn(&run_tests.step);
}
