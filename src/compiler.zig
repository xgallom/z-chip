const std = @import("std");

const zengine = @import("zengine");
const Zengine = zengine.Zengine;
const allocators = zengine.allocators;
const global = zengine.global;

const Mem = @import("emul/Mem.zig");
const Prog = @import("emul/Prog.zig");
const Syntax = @import("emul/Syntax.zig");
const storage = @import("emul/storage.zig");

const log = std.log.scoped(.main);

pub const std_options: std.Options = .{
    .log_level = .info,
    .log_scope_levels = &.{
        // .{ .scope = .main, .level = .debug },
        // .{ .scope = .cpu, .level = .debug },
        // .{ .scope = .inst, .level = .debug },
        // .{ .scope = .mem, .level = .debug },
        .{ .scope = .prog, .level = .debug },
        .{ .scope = .syntax, .level = .debug },
    },
};

pub const zengine_options: zengine.Options = .{
    .has_renderer = false,
    .has_scene = false,
    .has_ui = false,
};

var stderr_buf: [1 << 8]u8 = undefined;
var stderr_writer = std.fs.File.stderr().writer(&stderr_buf);
const stderr = &stderr_writer.interface;

pub fn main() !void {
    allocators.init(1_000_000_000);
    defer allocators.deinit();

    var engine = try Zengine.createHeadless(.{ .run = &run });
    defer engine.deinit();

    const args = global.args();
    if (args.len < 2) return error.NotEnoughArguments;

    return engine.run();
}

fn run(self: *Zengine) !void {
    _ = self;
    const gpa = allocators.gpa();

    const buf = try allocators.scratch().alloc(u8, 1 << 10);
    defer allocators.scratch().free(buf);
    var syntax: Syntax = .{};
    defer syntax.deinit(gpa);

    const args = global.args();
    for (args[1..]) |in_path| {
        const in_f = try std.fs.cwd().openFile(in_path, .{});
        defer in_f.close();
        var in_reader = in_f.reader(buf);
        try syntax.read(gpa, &in_reader.interface);
    }

    var prog = try syntax.parse(gpa);
    defer prog.deinit(gpa);
    var resolved = try prog.compile(gpa);
    defer resolved.deinit(gpa);
    try resolved.resolve();

    const mem = try gpa.create(Mem.Data);
    defer gpa.destroy(mem);
    mem.reset();

    var r = std.Io.Reader.fixed(resolved.code.items);
    try mem.load(&r);
    try mem.dump(stderr);
    try stderr.flush();
    try storage.Program.store(resolved.code.items);
}
