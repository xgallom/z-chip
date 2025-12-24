const std = @import("std");
const assert = std.debug.assert;

const zengine = @import("zengine");

const font = @import("font.zig");
const Inst = @import("Inst.zig");
const Mem = @import("Mem.zig");
const storage = @import("storage.zig");

const log = std.log.scoped(.cpu);
pub const sections = zengine.perf.sections(@This(), &.{.run});

rnd: std.Random.DefaultPrng,
inst: Inst = .invalid,
device: Flags.Device = .default,
run_flags: RunFlags = .{},
key: ?u4 = null,
jmp: JumpTable = initJumpTable(),

const Self = @This();
const InstHandler = *const fn (self: *Self, mem: *Mem, args: Inst.Args) Inst.Error!void;
const JumpTable = std.EnumArray(Inst.OpCode, InstHandler);

pub const Flags = packed struct {
    vf_reset: bool,
    i_increment: bool,
    drw_sync: bool,
    drw_clip_bottom: bool,
    shift_vx: bool,
    jp_v0a_n: bool,

    pub const default = device_flags.get(.default);

    pub const Device = enum {
        chip8,
        schip,
        xochip,

        pub const default: Device = .xochip;

        pub fn next(self: *Device) void {
            self.* = @enumFromInt((@intFromEnum(self.*) + 1) % DeviceFlags.Indexer.count);
        }
    };

    pub const DeviceFlags = std.EnumArray(Device, Flags);
    pub const device_flags: DeviceFlags = .init(.{
        .chip8 = .{
            .vf_reset = true,
            .i_increment = true,
            .drw_sync = true,
            .drw_clip_bottom = true,
            .shift_vx = false,
            .jp_v0a_n = false,
        },
        .schip = .{
            .vf_reset = false,
            .i_increment = false,
            .drw_sync = true,
            .drw_clip_bottom = true,
            .shift_vx = true,
            .jp_v0a_n = true,
        },
        .xochip = .{
            .vf_reset = false,
            .i_increment = true,
            .drw_sync = true,
            .drw_clip_bottom = true,
            .shift_vx = false,
            .jp_v0a_n = false,
        },
    });
};

pub const RunFlags = packed struct {
    is_drw_sync: bool = false,
    is_scr_hi: bool = false,
    drw_plane: u2 = Mem.Screen.plane_mask_default.bits.mask,

    pub fn drawPlane(rf: RunFlags) Mem.Screen.PlaneMask {
        return .{ .bits = .{ .mask = rf.drw_plane } };
    }
};

pub fn register() !void {
    try sections.register();
    try sections.sub(.run)
        .sections(&.{ .step, .exec }).register();
}

pub fn beginSection() void {
    const section = sections.sub(.run);
    section.begin();
    section.sub(.step).beginPaused();
    section.sub(.exec).beginPaused();
}

pub fn endSection() void {
    const section = sections.sub(.run);
    section.end();
    section.sub(.step).end();
    section.sub(.exec).end();
}

pub fn init(gpa: std.mem.Allocator) !*Self {
    const self = try gpa.create(Self);
    self.* = .{ .rnd = .init(@intCast(std.time.microTimestamp())) };
    return self;
}

pub fn deinit(self: *Self, gpa: std.mem.Allocator) void {
    gpa.destroy(self);
}

pub fn reset(self: *Self) void {
    self.inst = .invalid;
    self.run_flags = .{};
    self.key = null;
}

pub fn step(self: *Self, mem: *Mem) !void {
    sections.sub(.run).sub(.step).unpause();
    defer sections.sub(.run).sub(.step).pause();
    self.inst = .invalid;
    self.inst = .fromWord(try mem.data.readWord(mem.regs.pc));
    log.debug("step: {t} ({X:04}) @{X:04}", .{ self.inst.op, self.inst.args.data, mem.regs.pc });
    mem.regs.pc += 2;
    try self.exec(mem);
}

pub fn exec(self: *Self, mem: *Mem) !void {
    sections.sub(.run).sub(.exec).unpause();
    defer sections.sub(.run).sub(.exec).pause();
    try self.jmp.get(self.inst.op)(
        self,
        mem,
        .{ .data = self.inst.args.data },
    );
}

pub fn dump(self: *const Self, comptime config: enum { small, big }, w: *std.Io.Writer) !void {
    try w.print(
        \\cpu dump:
        \\inst: {t} ({X:04})
        \\device: {t}
        \\run flags: {any}
        \\key: {?X}
        \\
    , .{
        self.inst.op,
        self.inst.args.data,
        self.device,
        self.run_flags,
        self.key,
    });
    if (comptime config == .big) {
        try w.print("jmp:\n", .{});
        inline for (0..JumpTable.Indexer.count) |n| try w.print(
            "[{X:2}]({t:9}): @{X:08}\n",
            .{ n, JumpTable.Indexer.keyForIndex(n), @intFromPtr(self.jmp.values[n]) },
        );
    }
}

fn initJumpTable() JumpTable {
    comptime {
        var result: JumpTable = undefined;
        for (0..JumpTable.Indexer.count) |n| {
            result.values[n] = @ptrCast(&@field(
                inst_handler,
                @tagName(JumpTable.Indexer.keyForIndex(n)),
            ));
        }
        return result;
    }
}

pub fn updateJumpTable(self: *Self) void {
    switch (self.device) {
        .chip8 => {
            self.jmp.set(.ld_nnnn, &inst_handler.invalid);
            self.jmp.set(.ld_plane, &inst_handler.invalid);
            self.jmp.set(.ld_audio, &inst_handler.invalid);
            self.jmp.set(.ld_pitch, &inst_handler.invalid);
            self.jmp.set(.ld_str, &inst_handler.invalid);
            self.jmp.set(.ld_rst, &inst_handler.invalid);
            self.jmp.set(.ld_iarr, &inst_handler.invalid);
            self.jmp.set(.ld_rria, &inst_handler.invalid);
            self.jmp.set(.exit, &inst_handler.invalid);
            self.jmp.set(.scroll_dn, &inst_handler.invalid);
            self.jmp.set(.scroll_un, &inst_handler.invalid);
            self.jmp.set(.scroll_r, &inst_handler.invalid);
            self.jmp.set(.scroll_l, &inst_handler.invalid);
            self.jmp.set(.scr_lo, &inst_handler.invalid);
            self.jmp.set(.scr_hi, &inst_handler.invalid);
            self.run_flags.drw_plane = Mem.Screen.plane_mask_default.bits.mask;
        },
        .schip => {
            self.jmp.set(.ld_nnnn, &inst_handler.invalid);
            self.jmp.set(.ld_plane, &inst_handler.invalid);
            self.jmp.set(.ld_audio, &inst_handler.invalid);
            self.jmp.set(.ld_pitch, &inst_handler.invalid);
            self.jmp.set(.ld_str, @ptrCast(&inst_handler.ld_str));
            self.jmp.set(.ld_rst, @ptrCast(&inst_handler.ld_rst));
            self.jmp.set(.ld_iarr, &inst_handler.invalid);
            self.jmp.set(.ld_rria, &inst_handler.invalid);
            self.jmp.set(.exit, &inst_handler.exit);
            self.jmp.set(.scroll_dn, &inst_handler.scroll_dn);
            self.jmp.set(.scroll_un, &inst_handler.invalid);
            self.jmp.set(.scroll_r, &inst_handler.scroll_r);
            self.jmp.set(.scroll_l, &inst_handler.scroll_l);
            self.jmp.set(.scr_lo, &inst_handler.scr_lo);
            self.jmp.set(.scr_hi, &inst_handler.scr_hi);
        },
        .xochip => {
            self.jmp.set(.ld_nnnn, @ptrCast(&inst_handler.ld_nnnn));
            self.jmp.set(.ld_plane, @ptrCast(&inst_handler.ld_plane));
            self.jmp.set(.ld_audio, &inst_handler.ld_audio);
            self.jmp.set(.ld_pitch, &inst_handler.ld_pitch);
            self.jmp.set(.ld_str, @ptrCast(&inst_handler.ld_str));
            self.jmp.set(.ld_rst, @ptrCast(&inst_handler.ld_rst));
            self.jmp.set(.ld_iarr, @ptrCast(&inst_handler.ld_iarr));
            self.jmp.set(.ld_rria, @ptrCast(&inst_handler.ld_rria));
            self.jmp.set(.exit, &inst_handler.exit);
            self.jmp.set(.scroll_dn, &inst_handler.scroll_dn);
            self.jmp.set(.scroll_un, &inst_handler.scroll_un);
            self.jmp.set(.scroll_r, &inst_handler.scroll_r);
            self.jmp.set(.scroll_l, &inst_handler.scroll_l);
            self.jmp.set(.scr_lo, &inst_handler.scr_lo);
            self.jmp.set(.scr_hi, &inst_handler.scr_hi);
        },
    }

    const flags = Flags.device_flags.get(self.device);
    switch (flags.vf_reset) {
        inline else => |vf_reset| {
            self.jmp.set(.or_rr, inst_handler.impl.or_rr(vf_reset));
            self.jmp.set(.and_rr, inst_handler.impl.and_rr(vf_reset));
            self.jmp.set(.xor_rr, inst_handler.impl.xor_rr(vf_reset));
        },
    }
    switch (flags.shift_vx) {
        inline else => |shift_vx| {
            self.jmp.set(.shr_rr, inst_handler.impl.shr_rr(shift_vx));
            self.jmp.set(.shl_rr, inst_handler.impl.shl_rr(shift_vx));
        },
    }
    switch (flags.jp_v0a_n) {
        inline else => |jp_v0a_n| {
            self.jmp.set(.jp_v0a, inst_handler.impl.jp_v0a(jp_v0a_n));
        },
    }
    switch (flags.drw_sync) {
        inline else => |drw_sync| switch (flags.drw_clip_bottom) {
            inline else => |drw_clip_bottom| {
                self.jmp.set(.drw_rrc, inst_handler.impl.drw_rrc(drw_sync, drw_clip_bottom));
            },
        },
    }
    switch (flags.i_increment) {
        inline else => |i_increment| {
            self.jmp.set(.ld_iar, inst_handler.impl.ld_iar(i_increment));
            self.jmp.set(.ld_ria, inst_handler.impl.ld_ria(i_increment));
        },
    }
}

pub const inst_handler = struct {
    const Args = Inst.Args;

    pub fn invalid(self: *Self, mem: *Mem, args: Args) !void {
        _ = self;
        _ = mem;
        _ = args;
        return Inst.Error.InvalidInstruction;
    }

    pub fn cls(self: *Self, mem: *Mem, args: Args) !void {
        _ = args;
        mem.scr.clear(self.run_flags.drawPlane());
        log.debug("CLS", .{});
    }

    pub fn ret(self: *Self, mem: *Mem, args: Args) !void {
        _ = self;
        _ = args;
        mem.regs.pc = mem.stack.get(mem.regs.sp);
        mem.regs.sp -= 1;
        log.debug("RET @{X:04} SP{X}", .{ mem.regs.pc, mem.regs.sp });
    }

    pub fn sys_a(self: *Self, mem: *Mem, args: Args) !void {
        impl.notImplemented(self, mem, args);
    }

    pub fn jp_a(self: *Self, mem: *Mem, args: Args.nnn) !void {
        _ = self;
        const annn = args.nnn();
        mem.regs.pc = annn;
        log.debug("JP @{X:04}", .{annn});
    }

    pub fn call_a(self: *Self, mem: *Mem, args: Args.nnn) !void {
        _ = self;
        const a = args.nnn();
        mem.regs.sp += 1;
        mem.stack.set(mem.regs.sp, mem.regs.pc);
        mem.regs.pc = a;
        log.debug("CALL @{X:04}", .{a});
    }

    pub fn se_rc(self: *Self, mem: *Mem, args: Args.xkk) !void {
        _ = self;
        const ax = args.x();
        const akk = args.kk();
        if (mem.regs.v[ax] == akk) {
            const inst: Inst = .decode(try mem.data.readWord(mem.regs.pc));
            mem.regs.pc += inst.op.byteSize();
        }
        log.debug("SE V{X} #{X:02}", .{ ax, akk });
    }

    pub fn sne_rc(self: *Self, mem: *Mem, args: Args.xkk) !void {
        _ = self;
        const ax = args.x();
        const akk = args.kk();
        if (mem.regs.v[ax] != akk) {
            const inst: Inst = .decode(try mem.data.readWord(mem.regs.pc));
            mem.regs.pc += inst.op.byteSize();
        }
        log.debug("SNE V{X} #{X:02}", .{ ax, akk });
    }

    pub fn se_rr(self: *Self, mem: *Mem, args: Args.xy) !void {
        _ = self;
        const ax = args.x();
        const ay = args.y();
        if (mem.regs.v[ax] == mem.regs.v[ay]) {
            const inst: Inst = .decode(try mem.data.readWord(mem.regs.pc));
            mem.regs.pc += inst.op.byteSize();
        }
        log.debug("SNE V{X} V{X}", .{ ax, ay });
    }

    pub fn ld_rc(self: *Self, mem: *Mem, args: Args.xkk) !void {
        _ = self;
        const ax = args.x();
        const akk = args.kk();
        mem.regs.v[ax] = akk;
        log.debug("LD V{X} #{X:02}", .{ ax, akk });
    }

    pub fn add_rc(self: *Self, mem: *Mem, args: Args.xkk) !void {
        _ = self;
        const ax = args.x();
        const akk = args.kk();
        mem.regs.v[ax] +%= akk;
        log.debug("ADD V{X} #{X:02}", .{ ax, akk });
    }

    pub fn ld_rr(self: *Self, mem: *Mem, args: Args.xy) !void {
        _ = self;
        const ax = args.x();
        const ay = args.y();
        mem.regs.v[ax] = mem.regs.v[ay];
        log.debug("LD V{X} V{X}", .{ ax, ay });
    }

    pub fn or_rr(self: *Self, mem: *Mem, args: Args) !void {
        try impl.or_rr(Flags.default.vf_reset)(self, mem, args);
    }

    pub fn and_rr(self: *Self, mem: *Mem, args: Args) !void {
        try impl.and_rr(Flags.default.vf_reset)(self, mem, args);
    }

    pub fn xor_rr(self: *Self, mem: *Mem, args: Args) !void {
        try impl.xor_rr(Flags.default.vf_reset)(self, mem, args);
    }

    pub fn add_rr(self: *Self, mem: *Mem, args: Args.xy) !void {
        _ = self;
        const ax = args.x();
        const ay = args.y();
        const x = @as(u16, mem.regs.v[ax]) + mem.regs.v[ay];
        mem.regs.v[ax] = @truncate(x);
        mem.regs.v[0xf] = @intCast(x >> 8);
        log.debug("ADD V{X} V{X}", .{ ax, ay });
    }

    pub fn sub_rr(self: *Self, mem: *Mem, args: Args.xy) !void {
        _ = self;
        const ax = args.x();
        const ay = args.y();
        const x = @as(u16, mem.regs.v[ax]) -% mem.regs.v[ay];
        mem.regs.v[ax] = @truncate(x);
        mem.regs.v[0xf] = @intFromBool(x <= 0xFF);
        log.debug("SUB V{X} V{X}", .{ ax, ay });
    }

    pub fn shr_rr(self: *Self, mem: *Mem, args: Args) !void {
        try impl.shr_rr(Flags.default.shift_vx)(self, mem, args);
    }

    pub fn subn_rr(self: *Self, mem: *Mem, args: Args.xy) !void {
        _ = self;
        const ax = args.x();
        const ay = args.y();
        const x = @as(u16, mem.regs.v[ay]) -% mem.regs.v[ax];
        mem.regs.v[ax] = @truncate(x);
        mem.regs.v[0xf] = @intFromBool(x <= 0xFF);
        log.debug("SUBN V{X} V{X}", .{ ax, ay });
    }

    pub fn shl_rr(self: *Self, mem: *Mem, args: Args) !void {
        try impl.shl_rr(Flags.default.shift_vx)(self, mem, args);
    }

    pub fn sne_rr(self: *Self, mem: *Mem, args: Args.xy) !void {
        _ = self;
        const ax = args.x();
        const ay = args.y();
        if (mem.regs.v[ax] != mem.regs.v[ay]) {
            const inst: Inst = .decode(try mem.data.readWord(mem.regs.pc));
            mem.regs.pc += inst.op.byteSize();
        }
        log.debug("SNE V{X} V{X}", .{ ax, ay });
    }

    pub fn ld_ia(self: *Self, mem: *Mem, args: Args.nnn) !void {
        _ = self;
        const annn = args.nnn();
        mem.regs.i = annn;
        log.debug("LD I @{X:04}", .{annn});
    }

    pub fn jp_v0a(self: *Self, mem: *Mem, args: Args) !void {
        try impl.jp_v0a(Flags.default.jp_v0a_n)(self, mem, args);
    }

    pub fn rnd_rc(self: *Self, mem: *Mem, args: Args.xkk) !void {
        const ax = args.x();
        const akk = args.kk();
        mem.regs.v[ax] = self.rnd.random().int(u8) & akk;
        log.debug("RND V{X} #{X:02}", .{ ax, akk });
    }

    pub fn drw_rrc(self: *Self, mem: *Mem, args: Args) !void {
        try impl.drw_rrc(
            Flags.default.drw_sync,
            Flags.default.drw_clip_bottom,
        )(self, mem, args);
    }

    pub fn skp_r(self: *Self, mem: *Mem, args: Args.x) !void {
        _ = self;
        const ax = args.x();
        if (try mem.key.isDown(mem.regs.v[ax])) {
            const inst: Inst = .decode(try mem.data.readWord(mem.regs.pc));
            mem.regs.pc += inst.op.byteSize();
        }
        log.debug("SKP V{X}", .{ax});
    }

    pub fn sknp_r(self: *Self, mem: *Mem, args: Args.x) !void {
        _ = self;
        const ax = args.x();
        if (!try mem.key.isDown(mem.regs.v[ax])) {
            const inst: Inst = .decode(try mem.data.readWord(mem.regs.pc));
            mem.regs.pc += inst.op.byteSize();
        }
        log.debug("SKNP V{X}", .{ax});
    }

    pub fn ld_nnnn(self: *Self, mem: *Mem, args: Args.nnnn) !void {
        _ = self;
        const annnn = args.nnnn();
        mem.regs.i = annnn;
        log.debug("LD I #{X:04}", .{annnn});
    }

    pub fn ld_plane(self: *Self, mem: *Mem, args: Args.x) !void {
        _ = mem;
        const ax = args.x();
        if (ax < 1 or ax > 2) return Inst.Error.InvalidArguments;
        self.run_flags.drw_plane = @intCast(ax);
        log.debug("LD PLN #{X}", .{ax});
    }

    pub fn ld_audio(self: *Self, mem: *Mem, args: Args) !void {
        return impl.notImplemented(self, mem, args);
    }

    pub fn ld_rd(self: *Self, mem: *Mem, args: Args.x) !void {
        _ = self;
        const ax = args.x();
        mem.regs.v[ax] = mem.regs.dt;
        log.debug("LD V{X} DT", .{ax});
    }

    pub fn ld_rk(self: *Self, mem: *Mem, args: Args.x) !void {
        const ax = args.x();
        if (self.key) |key| {
            mem.regs.v[ax] = key;
            self.key = null;
            log.debug("LD V{X} K", .{ax});
        } else return Inst.Error.WaitForKey;
    }

    pub fn ld_dr(self: *Self, mem: *Mem, args: Args.x) !void {
        _ = self;
        const ax = args.x();
        mem.regs.dt = mem.regs.v[ax];
        log.debug("LD DT V{X}", .{ax});
    }

    pub fn ld_sr(self: *Self, mem: *Mem, args: Args.x) !void {
        _ = self;
        const ax = args.x();
        mem.regs.st = mem.regs.v[ax];
        log.debug("LD ST V{X}", .{ax});
    }

    pub fn add_ir(self: *Self, mem: *Mem, args: Args.x) !void {
        _ = self;
        const ax = args.x();
        mem.regs.i += mem.regs.v[ax];
        log.debug("ADD I V{X}", .{ax});
    }

    pub fn ld_fr(self: *Self, mem: *Mem, args: Args.x) !void {
        _ = self;
        const ax = args.x();
        mem.regs.i = (mem.regs.v[ax] & 0xf) * font.height;
        log.debug("LD F V{X}", .{ax});
    }

    pub fn ld_fbr(self: *Self, mem: *Mem, args: Args.x) !void {
        _ = self;
        const ax = args.x();
        mem.regs.i = (mem.regs.v[ax] & 0xf) * font.height;
        log.debug("LD F V{X}", .{ax});
    }

    pub fn ld_br(self: *Self, mem: *Mem, args: Args.x) !void {
        _ = self;
        const ax = args.x();
        const x = mem.regs.v[ax];
        try mem.data.write(mem.regs.i, x / 100);
        try mem.data.write(mem.regs.i + 1, (x / 10) % 10);
        try mem.data.write(mem.regs.i + 2, x % 10);
        log.debug("LD B V{X}", .{ax});
    }

    pub fn ld_pitch(self: *Self, mem: *Mem, args: Args) !void {
        return impl.notImplemented(self, mem, args);
    }

    pub fn ld_iar(self: *Self, mem: *Mem, args: Args) !void {
        try impl.ld_iar(Flags.default.i_increment)(self, mem, args);
    }

    pub fn ld_ria(self: *Self, mem: *Mem, args: Args) !void {
        try impl.ld_ria(Flags.default.i_increment)(self, mem, args);
    }

    pub fn ld_str(self: *Self, mem: *Mem, args: Args.x) !void {
        _ = self;
        const ax: u5 = args.x();
        storage.ProgramRegisters.store(mem.regs.v[0 .. ax + 1]) catch |err| {
            log.err("failed writing {X} program registers to \"{s}\": {t}", .{
                ax + 1,
                storage.ProgramRegisters.path() catch unreachable,
                err,
            });
            return Inst.Error.StorageFailed;
        };
        log.debug("LD STR V0-V{X}", .{ax});
    }

    pub fn ld_rst(self: *Self, mem: *Mem, args: Args.x) !void {
        _ = self;
        const ax: u5 = args.x();
        const prog_regs = storage.ProgramRegisters.load() catch storage.ProgramRegisters.init;
        @memcpy(mem.regs.v[0 .. ax + 1], prog_regs.data[0 .. ax + 1]);
        log.debug("LD V0-V{X} STR", .{ax});
    }

    pub fn ld_iarr(self: *Self, mem: *Mem, args: Args.xy) !void {
        _ = self;
        const ax: u5 = args.x();
        const ay: u5 = args.y();
        var nx = ax;
        var ny = ay;
        if (ay < ax) {
            nx = ay;
            ny = ax;
        }
        for (nx..ny + 1, 0..) |vn, n| try mem.data.write(@intCast(mem.regs.i + n), mem.regs.v[vn]);
        log.debug("LD [I] V{X}-V{X}", .{ ax, ay });
    }

    pub fn ld_rria(self: *Self, mem: *Mem, args: Args.xy) !void {
        _ = self;
        const ax: u5 = args.x();
        const ay: u5 = args.y();
        var nx = ax;
        var ny = ay;
        if (ay < ax) {
            nx = ay;
            ny = ax;
        }
        for (nx..ny + 1, 0..) |vn, n| mem.regs.v[vn] = try mem.data.read(@intCast(mem.regs.i + n));
        log.debug("LD [I] V{X}-V{X}", .{ ax, ay });
    }

    pub fn exit(self: *Self, mem: *Mem, args: Args) !void {
        _ = self;
        _ = mem;
        _ = args;
        return Inst.Error.Exit;
    }

    pub fn scroll_dn(self: *Self, mem: *Mem, args: Args) !void {
        return impl.notImplemented(self, mem, args);
    }

    pub fn scroll_un(self: *Self, mem: *Mem, args: Args) !void {
        return impl.notImplemented(self, mem, args);
    }

    pub fn scroll_r(self: *Self, mem: *Mem, args: Args) !void {
        return impl.notImplemented(self, mem, args);
    }

    pub fn scroll_l(self: *Self, mem: *Mem, args: Args) !void {
        return impl.notImplemented(self, mem, args);
    }

    pub fn scr_lo(self: *Self, mem: *Mem, args: Args) !void {
        _ = mem;
        _ = args;
        assert(self.run_flags.is_scr_hi);
        self.run_flags.is_scr_hi = false;
    }

    pub fn scr_hi(self: *Self, mem: *Mem, args: Args) !void {
        _ = mem;
        _ = args;
        assert(!self.run_flags.is_scr_hi);
        self.run_flags.is_scr_hi = true;
    }

    pub const impl = struct {
        fn notImplemented(self: *Self, mem: *Mem, args: Args) void {
            log.warn("instruction not implemented: {t} ({X:04}) @{X:04}", .{
                self.inst.op,
                args.data,
                mem.regs.pc - 2,
            });
        }

        fn or_rr(comptime vf_reset: bool) InstHandler {
            const inner = struct {
                fn or_rr(self: *Self, mem: *Mem, args: Args.xy) !void {
                    _ = self;
                    const ax = args.x();
                    const ay = args.y();
                    mem.regs.v[ax] |= mem.regs.v[ay];
                    if (vf_reset) mem.regs.v[0xf] = 0;
                    log.debug("OR V{X} V{X}", .{ ax, ay });
                }
            };
            return @ptrCast(&inner.or_rr);
        }

        fn and_rr(comptime vf_reset: bool) InstHandler {
            const inner = struct {
                fn and_rr(self: *Self, mem: *Mem, args: Args.xy) !void {
                    _ = self;
                    const ax = args.x();
                    const ay = args.y();
                    mem.regs.v[ax] &= mem.regs.v[ay];
                    if (vf_reset) mem.regs.v[0xf] = 0;
                    log.debug("AND V{X} V{X}", .{ ax, ay });
                }
            };
            return @ptrCast(&inner.and_rr);
        }

        fn xor_rr(comptime vf_reset: bool) InstHandler {
            const inner = struct {
                fn xor_rr(self: *Self, mem: *Mem, args: Args.xy) !void {
                    _ = self;
                    const ax = args.x();
                    const ay = args.y();
                    mem.regs.v[ax] ^= mem.regs.v[ay];
                    if (vf_reset) mem.regs.v[0xf] = 0;
                    log.debug("XOR V{X} V{X}", .{ ax, ay });
                }
            };
            return @ptrCast(&inner.xor_rr);
        }

        fn shr_rr(comptime shift_vx: bool) InstHandler {
            const inner = struct {
                fn shr_rr(self: *Self, mem: *Mem, args: Args.xy) !void {
                    _ = self;
                    const ax = args.x();
                    var ay = args.y();
                    if (shift_vx) ay = ax;
                    const vf = mem.regs.v[ay] & 0x01;
                    mem.regs.v[ax] = mem.regs.v[ay] >> 1;
                    mem.regs.v[0xf] = vf;
                    log.debug("SHR V{X} V{X}", .{ ax, ay });
                }
            };
            return @ptrCast(&inner.shr_rr);
        }

        fn shl_rr(comptime shift_vx: bool) InstHandler {
            const inner = struct {
                fn shl_rr(self: *Self, mem: *Mem, args: Args.xy) !void {
                    _ = self;
                    const ax = args.x();
                    var ay = args.y();
                    if (shift_vx) ay = ax;
                    const vf = mem.regs.v[ay] >> 7;
                    mem.regs.v[ax] = mem.regs.v[ay] << 1;
                    mem.regs.v[0xf] = vf;
                    log.debug("SHL V{X} V{X}", .{ ax, ay });
                }
            };
            return @ptrCast(&inner.shl_rr);
        }

        fn jp_v0a(comptime jp_v0a_n: bool) InstHandler {
            const inner = struct {
                fn jp_v0a(self: *Self, mem: *Mem, args: Args.nnn) !void {
                    _ = self;
                    const annn = args.nnn();
                    if (jp_v0a_n) {
                        mem.regs.pc = mem.regs.v[(annn >> 8) & 0xF] + annn;
                    } else {
                        mem.regs.pc = mem.regs.v[0x0] + annn;
                    }
                    log.debug("JP V0 + @{X:04}", .{annn});
                }
            };
            return @ptrCast(&inner.jp_v0a);
        }

        fn drw_rrc(comptime drw_sync: bool, comptime drw_clip_bottom: bool) InstHandler {
            const inner = struct {
                fn drw_rrc(self: *Self, mem: *Mem, args: Args.xyn) !void {
                    if (drw_sync) {
                        if (!self.run_flags.is_drw_sync) return Inst.Error.WaitForDraw;
                    }

                    const ax = args.x();
                    const ay = args.y();
                    const an = args.n();
                    if (self.run_flags.is_scr_hi) {
                        for (0..an) |_y| {
                            const y: u4 = @intCast(_y);
                            const src = try mem.data.read(@intCast(mem.regs.i + y));
                            for (0..8) |_x| {
                                const x: u3 = @intCast(_x);
                                var mask: u1 = @truncate(src >> (7 - x));
                                if (drw_clip_bottom) {
                                    if (mem.regs.v[ay] % Mem.scr_hi_h + y >= Mem.scr_hi_h or
                                        mem.regs.v[ax] % Mem.scr_hi_w + x >= Mem.scr_hi_w) mask = 0;
                                }
                                if (mask == 0) continue;
                                const sy = (mem.regs.v[ay] + @as(u16, y)) % Mem.scr_hi_h;
                                const sx = (mem.regs.v[ax] + @as(u16, x)) % Mem.scr_hi_w;
                                var iter = self.run_flags.drawPlane().iterator();
                                mem.regs.v[0xf] = 0;
                                while (iter.next()) |p| {
                                    if (mem.scr.readHi(sy, sx, p)) {
                                        mem.scr.writeHi(sy, sx, false, p);
                                        mem.regs.v[0xf] |= 1;
                                    } else mem.scr.writeHi(sy, sx, true, p);
                                }
                            }
                        }
                    } else {
                        for (0..an) |_y| {
                            const y: u4 = @intCast(_y);
                            const src = try mem.data.read(@intCast(mem.regs.i + y));
                            for (0..8) |_x| {
                                const x: u3 = @intCast(_x);
                                var mask: u1 = @truncate(src >> (7 - x));
                                if (drw_clip_bottom) {
                                    if (mem.regs.v[ay] % Mem.scr_h + y >= Mem.scr_h or
                                        mem.regs.v[ax] % Mem.scr_w + x >= Mem.scr_w) mask = 0;
                                }
                                if (mask == 0) continue;
                                const sy = (mem.regs.v[ay] + @as(u16, y)) % Mem.scr_h;
                                const sx = (mem.regs.v[ax] + @as(u16, x)) % Mem.scr_w;
                                var iter = self.run_flags.drawPlane().iterator();
                                mem.regs.v[0xf] = 0;
                                while (iter.next()) |p| {
                                    if (mem.scr.read(sy, sx, p)) {
                                        mem.scr.write(sy, sx, false, p);
                                        mem.regs.v[0xf] |= 1;
                                    } else mem.scr.write(sy, sx, true, p);
                                }
                            }
                        }
                    }
                    log.debug("DRW V{X} V{X} #{X}", .{ ax, ay, an });
                }
            };
            return @ptrCast(&inner.drw_rrc);
        }

        fn ld_iar(comptime i_increment: bool) InstHandler {
            const inner = struct {
                fn ld_iar(self: *Self, mem: *Mem, args: Args.x) !void {
                    _ = self;
                    const ax: u5 = args.x();
                    for (0..ax + 1) |n| try mem.data.write(@intCast(mem.regs.i + n), mem.regs.v[n]);
                    if (i_increment) mem.regs.i +%= ax + 1;
                    log.debug("LD [I] V0-V{X}", .{ax});
                }
            };
            return @ptrCast(&inner.ld_iar);
        }

        fn ld_ria(comptime i_increment: bool) InstHandler {
            const inner = struct {
                fn ld_ria(self: *Self, mem: *Mem, args: Args.x) !void {
                    _ = self;
                    const ax: u5 = args.x();
                    for (0..ax + 1) |n| mem.regs.v[n] = try mem.data.read(@intCast(mem.regs.i + n));
                    if (i_increment) mem.regs.i +%= ax + 1;
                    log.debug("LD V0-V{X} [I]", .{ax});
                }
            };
            return @ptrCast(&inner.ld_ria);
        }
    };
};
