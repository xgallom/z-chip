const std = @import("std");

pub const ProgMeta = struct {
    name: []const u8,
    sources: []const []const u8,
};

const programs: []const ProgMeta = &.{
    .{
        .name = "editor",
        .sources = &.{
            "src/stddef.zc8",
            "src/editor/main.zc8",
            "src/editor/update.zc8",
            "src/editor/data.zc8",
        },
    },
};

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const zengine = b.dependency("zengine", .{});
    const z = @import("zengine");
    const options = z.getOptions(b);

    const exe_mod = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
        .imports = &.{
            .{ .name = "zengine", .module = zengine.module("zengine") },
        },
        .pic = true,
    });

    const check_exe = b.addExecutable(.{
        .name = "z-chip",
        .root_module = exe_mod,
    });

    const exe = b.addExecutable(.{
        .name = "z-chip",
        .root_module = exe_mod,
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

        const compiler_step = b.step("compiler", "Run the zchip compiler");
        const compiler_cmd = b.addRunArtifact(compiler_exe);
        compiler_step.dependOn(&compiler_cmd.step);

        if (b.args) |args| {
            compiler_cmd.addArgs(args);
        }

        const prog_step = b.step("compile", "Compile the zchip programs");

        for (programs) |prog| {
            const prog_cmd = b.addRunArtifact(compiler_exe);
            prog_step.dependOn(&prog_cmd.step);

            const prog_output = prog_cmd.addOutputFileArg(
                try std.fmt.allocPrint(b.allocator, "bin/{s}.ch8", .{prog.name}),
            );
            for (prog.sources) |src_path| _ = prog_cmd.addFileArg(b.path(
                try std.fs.path.join(b.allocator, &.{ "assets/prog", src_path }),
            ));

            const install_prog = b.addInstallFile(
                prog_output,
                try std.fmt.allocPrint(b.allocator, "prog/{s}.ch8", .{prog.name}),
            );
            prog_step.dependOn(&install_prog.step);
        }
    }

    const check_step = b.step("check", "Check sources");
    check_step.dependOn(&check_exe.step);

    const run_step = b.step("run", "Run the zchip emulator");
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
