const std = @import("std");
const assert = std.debug.assert;

const font = @import("font.zig");
const Inst = @import("Inst.zig");

const log = std.log.scoped(.mem);

pub const regs_v_len = 16;
pub const stack_size = 16;
pub const data_size = 0x1000;
pub const prog_begin = 0x200;
pub const prog_end = data_size;
pub const prog_size = prog_end - prog_begin;
pub const scr_w = 64;
pub const scr_h = 32;
pub const scr_size = scr_w * scr_h;
pub const scr_hi_w = 128;
pub const scr_hi_h = 64;
pub const scr_hi_size = scr_hi_w * scr_hi_h;
pub const scr_w_mul = @divExact(scr_hi_w, scr_w);
pub const scr_h_mul = @divExact(scr_hi_h, scr_h);
pub const key_size = 16;

comptime {
    assert(scr_w_mul == 2);
    assert(scr_h_mul == 2);
}

regs: Regs,
stack: Stack,
data: Data,
scr: Screen,
key: Keyboard,

const Self = @This();
pub const Error = error{ OutOfRange, BadAlignment };

pub const Regs = struct {
    pc: u16 = prog_begin,
    i: u16 = 0,
    v: [regs_v_len]u8 = @splat(0),
    sp: u8 = 0,
    dt: u8 = 0,
    st: u8 = 0,

    pub fn reset(self: *Regs) void {
        self.* = .{};
    }

    pub fn dump(self: *const Regs) void {
        std.debug.print(
            \\registers:
            \\pc: {X:04}
            \\i: {X:04}
            \\
        , .{ self.pc, self.i });

        for (0..regs_v_len) |n| std.debug.print("v[{:2}]: {X:02}\n", .{ n, self.v[n] });

        std.debug.print(
            \\sp: {X:02}
            \\dt: {X:02}
            \\st: {X:02}
            \\
        , .{ self.sp, self.dt, self.st });
    }
};

pub const Stack = struct {
    data: [stack_size]u16,

    pub fn clear(self: *Stack) void {
        @memset(&self.data, 0);
    }

    pub fn get(self: *const Stack, idx: u8) u16 {
        assert(idx < stack_size);
        return self.data[idx];
    }

    pub fn set(self: *Stack, idx: u8, val: u16) void {
        assert(idx < stack_size);
        self.data[idx] = val;
    }

    pub fn dump(self: *const Stack) void {
        std.debug.print("stack:\n", .{});
        for (0..stack_size) |n| std.debug.print(
            "stack[{:2}]: {X:04}\n",
            .{ n, self.get(@intCast(n)) },
        );
    }
};

pub const Data = struct {
    data: [data_size]u8,

    pub fn reset(self: *Data) void {
        @memset(&self.data, 0);
        @memcpy(self.data[0..font.size], &font.data);
    }

    pub fn load(self: *Data, prog: *std.Io.Reader) !void {
        _ = try prog.readSliceShort(self.data[prog_begin..]);
    }

    pub fn slice(self: *Data, start: u16, end: u16) []u8 {
        assert(end <= data_size);
        assert(start <= end);
        return self.data[start..end];
    }

    pub fn sliceConst(self: *const Data, start: u16, end: u16) []const u8 {
        assert(end <= data_size);
        assert(start <= end);
        return self.data[start..end];
    }

    pub fn read(self: *const Data, addr: u16) !u8 {
        if (!isValid(.any, addr)) {
            log.err("read @{X:04} out of range", .{addr});
            return Error.OutOfRange;
        }
        return self.uncheckedRead(addr);
    }

    pub fn uncheckedRead(self: *const Data, addr: u16) u8 {
        assert(addr < data_size);
        return self.data[addr];
    }

    pub fn write(self: *Data, addr: u16, val: u8) !void {
        if (!isValid(.prog, addr)) {
            log.err("write @{X:04} out of range", .{addr});
            return Error.OutOfRange;
        }
        self.data[addr] = val;
    }

    pub fn decode(self: *const Data, addr: u16) !Inst {
        if (!isValid(.prog, addr)) {
            log.err("decode @{X:04} out of range", .{addr});
            return Error.OutOfRange;
        }
        if (addr & 0x1 != 0) {
            log.err("decode @{X:04} bad alignment", .{addr});
            return Error.BadAlignment;
        }
        return .fromData(.{ self.data[addr], self.data[addr + 1] });
    }

    fn isValid(comptime section: enum { font, prog, any }, addr: u16) bool {
        return switch (comptime section) {
            .font => addr < font.size,
            .prog => addr >= prog_begin and addr < prog_end,
            .any => addr < prog_end,
        };
    }

    pub fn dump(self: *const Data) void {
        std.debug.print("data:\n", .{});
        {
            var n: u16 = 0;
            comptime assert(data_size % 0x20 == 0);
            while (n < data_size) : (n += 0x20) {
                const line = self.sliceConst(n, n + 0x20);
                if (std.mem.allEqual(u8, line, 0)) continue;
                std.debug.print("[{X:4}]:", .{n});
                for (line, 0..) |c, i| {
                    std.debug.print(" {X:02}", .{c});
                    if (i % 2 == 1) std.debug.print(" ", .{});
                    if (i == 0xF) std.debug.print("|", .{});
                }
                std.debug.print("\n", .{});
            }
        }
    }
};

pub const Screen = struct {
    data: std.StaticBitSet(scr_hi_size),

    pub fn clear(self: *Screen) void {
        self.data = .initEmpty();
    }

    fn index(y: u16, x: u16) usize {
        assert(y < scr_h);
        assert(x < scr_w);
        return y * scr_h_mul * scr_hi_w + x * scr_w_mul;
    }

    fn indexHi(y: u16, x: u16) usize {
        assert(y < scr_hi_h);
        assert(x < scr_hi_w);
        return y * scr_hi_w + x;
    }

    pub fn read(self: *const Screen, y: u16, x: u16) bool {
        const n = index(y, x);
        return self.data.isSet(n);
    }

    pub fn readHi(self: *const Screen, y: u16, x: u16) bool {
        const n = indexHi(y, x);
        return self.data.isSet(n);
    }

    pub fn write(self: *Screen, y: u16, x: u16, val: bool) void {
        const n = index(y, x);
        self.data.setValue(n, val);
        self.data.setValue(n + 1, val);
        self.data.setValue(n + scr_hi_w, val);
        self.data.setValue(n + scr_hi_w + 1, val);
    }

    pub fn writeHi(self: *Screen, y: u16, x: u16, val: bool) void {
        const n = indexHi(y, x);
        self.data.setValue(n, val);
    }

    const border_v = blk: {
        var result: [scr_hi_w + 2]u8 = @splat('-');
        result[0] = '+';
        result[scr_hi_w + 1] = '+';
        break :blk result;
    };

    pub fn dump(self: *const Screen) void {
        std.debug.print("screen:\n", .{});
        std.debug.print("{s}\n", .{&border_v});
        for (0..scr_hi_h) |y| {
            std.debug.print("|", .{});
            for (0..scr_hi_w) |x| {
                const p = self.readHi(@intCast(y), @intCast(x));
                const c: u8 = if (p) '#' else ' ';
                std.debug.print("{c}", .{c});
            }
            std.debug.print("|\n", .{});
        }
        std.debug.print("{s}\n", .{&border_v});
    }
};

pub const Keyboard = struct {
    data: std.StaticBitSet(key_size) = .initEmpty(),

    pub fn clear(self: *Keyboard) void {
        self.data = .initEmpty();
    }

    pub fn down(self: *Keyboard, key: u8) void {
        assert(key < key_size);
        self.data.set(key);
    }

    pub fn up(self: *Keyboard, key: u8) void {
        assert(key < key_size);
        self.data.unset(key);
    }

    pub fn isDown(self: *const Keyboard, key: u8) !bool {
        if (key >= key_size) {
            log.err("key is down {X} out of range", .{key});
            return Error.OutOfRange;
        }
        return self.data.isSet(key);
    }

    pub fn dump(self: *const Keyboard) void {
        std.debug.print("keyboard:\n", .{});
        for (0..key_size) |n| std.debug.print("key[{X}]: {}\n", .{
            n,
            self.isDown(@intCast(n)) catch unreachable,
        });
    }
};

pub fn init(gpa: std.mem.Allocator) !*Self {
    const self = try gpa.create(Self);
    self.reset();
    return self;
}

pub fn deinit(self: *Self, gpa: std.mem.Allocator) void {
    gpa.destroy(self);
}

pub fn reset(self: *Self) void {
    self.regs.reset();
    self.stack.clear();
    self.data.reset();
    self.scr.clear();
    self.key.clear();
}

pub fn dump(self: *const Self) void {
    std.debug.print("memory dump:\n", .{});
    self.regs.dump();
    self.stack.dump();
    self.data.dump();
}
