const std = @import("std");
const assert = std.debug.assert;

const zengine = @import("zengine");
const allocators = zengine.allocators;
const global = zengine.global;

const Mem = @import("Mem.zig");

const log = std.log.scoped(.storage);

pub const Program = struct {
    pub fn load(mem: *Mem) !void {
        const prog_path = global.arg(0);
        log.info("loading program \"{s}\"", .{prog_path});
        const src = try std.fs.cwd().openFile(prog_path, .{});
        defer src.close();
        var buf: [256]u8 = undefined;
        var reader = src.reader(&buf);
        try mem.data.load(&reader.interface);
        if (!reader.atEnd()) return error.FileTooLong;
    }
};

pub const ProgramRegisters = extern struct {
    data: [len]u8 = @splat(0),

    const Self = @This();
    pub const init: Self = .{};
    pub const len = 16;

    pub fn load() !Self {
        errdefer |err| log.info("error loading program registers: {t}", .{err});
        const prog_reg_path = try path();
        defer allocators.scratch().free(prog_reg_path);
        const src = try std.fs.cwd().openFile(prog_reg_path, .{});
        defer src.close();
        var buf: [@sizeOf(Self)]u8 = undefined;
        var reader = src.reader(&buf);
        const result = try reader.interface.takeStruct(Self, .little);
        if (reader.interface.seek != try src.getEndPos()) return error.FileTooLong;
        return result;
    }

    pub fn store(data: []const u8) !void {
        var result: Self = load() catch .init;
        @memcpy(result.data[0..data.len], data);
        const prog_reg_path = try path();
        defer allocators.scratch().free(prog_reg_path);
        const src = try std.fs.cwd().createFile(prog_reg_path, .{ .truncate = true });
        defer src.close();
        var buf: [@sizeOf(Self)]u8 = undefined;
        var reader = src.writer(&buf);
        try reader.interface.writeStruct(result, .little);
        try reader.interface.flush();
    }

    pub fn path() ![]const u8 {
        const prog_path = global.arg(0);
        const prog_ext = std.fs.path.extension(prog_path);
        const prog_basename = prog_path[0 .. prog_path.len - prog_ext.len];
        return std.fmt.allocPrint(allocators.scratch(), "{s}.bin", .{prog_basename});
    }
};
