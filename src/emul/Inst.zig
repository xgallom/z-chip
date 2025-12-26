const std = @import("std");
const assert = std.debug.assert;

const CPU = @import("CPU.zig");
const Prog = @import("Prog.zig");

pub const byte_size = 2;

op: OpCode,
args: Args,

const Self = @This();
pub const Error = error{
    InvalidInstruction,
    InvalidArguments,
    StorageFailed,
    WaitForKey,
    WaitingForKey,
    WaitForDraw,
    WaitingForDraw,
    Exit,
};
pub const invalid: Self = .init(.invalid, 0);

pub const OpCode = enum(u8) {
    invalid,
    cls,
    ret,
    sys_a,
    jp_a,
    call_a,
    se_rc,
    sne_rc,
    se_rr,
    ld_rc,
    add_rc,
    ld_rr,
    or_rr,
    and_rr,
    xor_rr,
    add_rr,
    sub_rr,
    shr_rr,
    subn_rr,
    shl_rr,
    sne_rr,
    ld_ia,
    jp_v0a,
    rnd_rc,
    drw_rrc,
    skp_r,
    sknp_r,
    ld_nnnn,
    ld_plane,
    ld_audio,
    ld_rd,
    ld_rk,
    ld_dr,
    ld_sr,
    add_ir,
    ld_fr,
    ld_fbr,
    ld_br,
    ld_pitch,
    ld_iar,
    ld_ria,
    ld_str,
    ld_rst,
    ld_iarr,
    ld_rria,
    exit,
    scroll_dn,
    scroll_un,
    scroll_r,
    scroll_l,
    scr_lo,
    scr_hi,

    pub fn byteSize(op: OpCode) u16 {
        return switch (op) {
            .ld_nnnn => 2 * byte_size,
            else => byte_size,
        };
    }

    pub fn ArgsType(comptime op: OpCode) type {
        const info = @typeInfo(@TypeOf(@field(CPU.inst_handler, @tagName(op)))).@"fn";
        return info.params[2].type orelse unreachable;
    }
};

pub const Args = extern struct {
    data: u16 = 0,

    pub const args_type: Type = .none;
    pub const ArgsType = @This();
    pub const Type = enum {
        none,
        nnnn,
        nnn,
        xkk,
        xyn,
        xy,
        x,
    };

    pub fn init(a: ArgsType) @This() {
        return a;
    }

    pub fn add(self: *Args, args: anytype) void {
        self.data |= args.data;
    }

    pub const nnnn = extern struct {
        data: u16 = 0,
        pub const args_type: Type = .nnnn;
        pub const ArgsType = struct { nnnn: u16 = 0 };
        pub fn init(a: @This().ArgsType) @This() {
            return .{ .data = a.nnnn };
        }
        pub inline fn nnnn(args: @This()) u16 {
            return args.data;
        }
    };

    pub const nnn = extern struct {
        data: u16 = 0,
        pub const args_type: Type = .nnn;
        pub const ArgsType = struct { nnn: u12 = 0 };
        pub fn init(a: @This().ArgsType) @This() {
            return .{ .data = a.nnn };
        }
        pub inline fn nnn(args: @This()) u12 {
            return @truncate(args.data);
        }
    };

    pub const xkk = extern struct {
        data: u16 = 0,
        pub const args_type: Type = .xkk;
        pub const ArgsType = struct { x: u4 = 0, kk: u8 = 0 };
        pub fn init(a: @This().ArgsType) @This() {
            return .{ .data = @as(u16, a.x) << 8 | @as(u16, a.kk) };
        }
        pub inline fn x(args: @This()) u4 {
            return @truncate(args.data >> 8);
        }
        pub inline fn kk(args: @This()) u8 {
            return @truncate(args.data);
        }
    };

    pub const xyn = extern struct {
        data: u16 = 0,
        pub const args_type: Type = .xyn;
        pub const ArgsType = struct { x: u4 = 0, y: u4 = 0, n: u4 = 0 };
        pub fn init(a: @This().ArgsType) @This() {
            return .{ .data = @as(u16, a.x) << 8 | @as(u16, a.y) << 4 | @as(u16, a.n) };
        }
        pub inline fn x(args: @This()) u4 {
            return @truncate(args.data >> 8);
        }
        pub inline fn y(args: @This()) u4 {
            return @truncate(args.data >> 4);
        }
        pub inline fn n(args: @This()) u4 {
            return @truncate(args.data);
        }
    };

    pub const xy = extern struct {
        data: u16,
        pub const args_type: Type = .xy;
        pub const ArgsType = struct { x: u4 = 0, y: u4 = 0 };
        pub fn init(a: @This().ArgsType) @This() {
            return .{ .data = @as(u16, a.x) << 8 | @as(u16, a.y) << 4 };
        }
        pub inline fn x(args: @This()) u4 {
            return @truncate(args.data >> 8);
        }
        pub inline fn y(args: @This()) u4 {
            return @truncate(args.data >> 4);
        }
    };

    pub const x = extern struct {
        data: u16 = 0,
        pub const args_type: Type = .x;
        pub const ArgsType = struct { x: u4 = 0 };
        pub fn init(a: @This().ArgsType) @This() {
            return .{ .data = @as(u16, a.x) << 8 };
        }
        pub inline fn x(args: @This()) u4 {
            return @truncate(args.data >> 8);
        }
    };
};

test Args {
    const args: Args = .{ .data = 0xABCD };
    {
        const a: Args.nnnn = @bitCast(args);
        try std.testing.expectEqual(a.nnnn(), 0xABCD);
    }
    {
        const a: Args.nnn = @bitCast(args);
        try std.testing.expectEqual(a.nnn(), 0xBCD);
    }
    {
        const a: Args.xkk = @bitCast(args);
        try std.testing.expectEqual(a.x(), 0xB);
        try std.testing.expectEqual(a.kk(), 0xCD);
    }
    {
        const a: Args.xyn = @bitCast(args);
        try std.testing.expectEqual(a.x(), 0xB);
        try std.testing.expectEqual(a.y(), 0xC);
        try std.testing.expectEqual(a.n(), 0xD);
    }
    {
        const a: Args.xy = @bitCast(args);
        try std.testing.expectEqual(a.x(), 0xB);
        try std.testing.expectEqual(a.y(), 0xC);
    }
    {
        const a: Args.x = @bitCast(args);
        try std.testing.expectEqual(a.x(), 0xB);
    }
}

pub fn init(op: OpCode, i: u16) Self {
    return .{ .op = op, .args = .{ .data = i } };
}

pub fn fromWord(data: u16) Self {
    return decode(data);
}

pub fn fromData(data: [byte_size]u8) Self {
    return decode(wordFromData(data));
}

pub fn encode(
    comptime op: OpCode,
    a: OpCode.ArgsType(op).ArgsType,
    resolve: Prog.EncodedInst.Resolve,
) Prog.EncodedInst {
    var result: Self = switch (op) {
        .invalid => .init(op, 0xFFFF),
        .cls => .init(op, 0x00E0),
        .ret => .init(op, 0x00EE),
        .sys_a => .init(op, 0x0000),
        .jp_a => .init(op, 0x1000),
        .call_a => .init(op, 0x2000),
        .se_rc => .init(op, 0x3000),
        .sne_rc => .init(op, 0x4000),
        .se_rr => .init(op, 0x5000),
        .ld_rc => .init(op, 0x6000),
        .add_rc => .init(op, 0x7000),
        .ld_rr => .init(op, 0x8000),
        .or_rr => .init(op, 0x8001),
        .and_rr => .init(op, 0x8002),
        .xor_rr => .init(op, 0x8003),
        .add_rr => .init(op, 0x8004),
        .sub_rr => .init(op, 0x8005),
        .shr_rr => .init(op, 0x8006),
        .subn_rr => .init(op, 0x8007),
        .shl_rr => .init(op, 0x800E),
        .sne_rr => .init(op, 0x9000),
        .ld_ia => .init(op, 0xA000),
        .jp_v0a => .init(op, 0xB000),
        .rnd_rc => .init(op, 0xC000),
        .drw_rrc => .init(op, 0xD000),
        .skp_r => .init(op, 0xE09E),
        .sknp_r => .init(op, 0xE0A1),
        .ld_nnnn => .init(op, 0xF000),
        .ld_plane => .init(op, 0xF001),
        .ld_audio => .init(op, 0xF002),
        .ld_rd => .init(op, 0xF007),
        .ld_rk => .init(op, 0xF00A),
        .ld_dr => .init(op, 0xF015),
        .ld_sr => .init(op, 0xF018),
        .add_ir => .init(op, 0xF01E),
        .ld_fr => .init(op, 0xF029),
        .ld_fbr => .init(op, 0xF030),
        .ld_br => .init(op, 0xF033),
        .ld_pitch => .init(op, 0xF03A),
        .ld_iar => .init(op, 0xF055),
        .ld_ria => .init(op, 0xF065),
        .ld_str => .init(op, 0xF075),
        .ld_rst => .init(op, 0xF085),
        .ld_iarr => .init(op, 0x5002),
        .ld_rria => .init(op, 0x5003),
        .exit => .init(op, 0x00FD),
        .scroll_dn => .init(op, 0x00C0),
        .scroll_un => .init(op, 0x00D0),
        .scroll_r => .init(op, 0x00FB),
        .scroll_l => .init(op, 0x00FC),
        .scr_lo => .init(op, 0x00FE),
        .scr_hi => .init(op, 0x00FF),
    };
    const args: OpCode.ArgsType(op) = .init(a);
    result.args.add(args);
    var ei: Prog.EncodedInst = .init(result);
    if (op == .ld_nnnn) ei.nnnn.data = args.data;
    ei.resolve = resolve;
    return ei;
}

pub fn decode(i: u16) Self {
    return .init(switch (in(i, 0)) {
        0x0 => switch (i) {
            0x00E0 => .cls,
            0x00EE => .ret,
            0x00C0...0x00CF => .scroll_dn,
            0x00D0...0x00DF => .scroll_un,
            0x00FB => .scroll_r,
            0x00FC => .scroll_l,
            0x00FD => .exit,
            0x00FE => .scr_lo,
            0x00FF => .scr_hi,
            else => .sys_a,
        },
        0x1 => .jp_a,
        0x2 => .call_a,
        0x3 => .se_rc,
        0x4 => .sne_rc,
        0x5 => switch (in(i, 3)) {
            0x0 => .se_rr,
            0x2 => .ld_iarr,
            0x3 => .ld_rria,
            else => .invalid,
        },
        0x6 => .ld_rc,
        0x7 => .add_rc,
        0x8 => switch (in(i, 3)) {
            0x0 => .ld_rr,
            0x1 => .or_rr,
            0x2 => .and_rr,
            0x3 => .xor_rr,
            0x4 => .add_rr,
            0x5 => .sub_rr,
            0x6 => .shr_rr,
            0x7 => .subn_rr,
            0xE => .shl_rr,
            else => .invalid,
        },
        0x9 => if (in(i, 3) == 0x0) .sne_rr else .invalid,
        0xA => .ld_ia,
        0xB => .jp_v0a,
        0xC => .rnd_rc,
        0xD => .drw_rrc,
        0xE => switch (ib(i, 1)) {
            0x9E => .skp_r,
            0xA1 => .sknp_r,
            else => .invalid,
        },
        0xF => switch (ib(i, 1)) {
            0x00 => if (in(i, 1) == 0x0) .ld_nnnn else .invalid,
            0x01 => .ld_plane,
            0x02 => .ld_audio,
            0x07 => .ld_rd,
            0x0A => .ld_rk,
            0x15 => .ld_dr,
            0x18 => .ld_sr,
            0x1E => .add_ir,
            0x29 => .ld_fr,
            0x30 => .ld_fbr,
            0x33 => .ld_br,
            0x3A => .ld_pitch,
            0x55 => .ld_iar,
            0x65 => .ld_ria,
            0x75 => .ld_str,
            0x85 => .ld_rst,
            else => .invalid,
        },
    }, i);
}

pub inline fn wordFromData(data: [byte_size]u8) u16 {
    return (@as(u16, data[0]) << 8) | data[1];
}

test wordFromData {
    try std.testing.expectEqual(wordFromData(.{ 0xFF, 0xAA }), 0xFFAA);
}

inline fn in(i: u16, comptime n: u2) u4 {
    return @truncate(i >> (12 - 4 * @as(u4, n)));
}

test in {
    try std.testing.expectEqual(in(0xABCD, 0), 0xA);
    try std.testing.expectEqual(in(0xABCD, 1), 0xB);
    try std.testing.expectEqual(in(0xABCD, 2), 0xC);
    try std.testing.expectEqual(in(0xABCD, 3), 0xD);
}

inline fn ib(i: u16, comptime n: u1) u8 {
    return @truncate(i >> (8 - 8 * @as(u4, n)));
}

test ib {
    try std.testing.expectEqual(ib(0xABCD, 0), 0xAB);
    try std.testing.expectEqual(ib(0xABCD, 1), 0xCD);
}
