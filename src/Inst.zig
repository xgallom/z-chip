const std = @import("std");
const assert = std.debug.assert;

pub const byte_size = 2;

op: OpCode,
args: Args,

const Self = @This();
pub const Error = error{
    InvalidInstruction,
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
            .ld_nnnn => 4,
            else => 2,
        };
    }
};

pub const Args = extern struct {
    data: u16,

    pub const nnn = extern struct {
        data: u16,
        pub inline fn nnn(args: @This()) u12 {
            return @truncate(args.data);
        }
    };

    pub const xkk = extern struct {
        data: u16,
        pub inline fn x(args: @This()) u4 {
            return @truncate(args.data >> 8);
        }
        pub inline fn kk(args: @This()) u8 {
            return @truncate(args.data);
        }
    };

    pub const xyn = extern struct {
        data: u16,
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
        pub inline fn x(args: @This()) u4 {
            return @truncate(args.data >> 8);
        }
        pub inline fn y(args: @This()) u4 {
            return @truncate(args.data >> 4);
        }
    };

    pub const x = extern struct {
        data: u16,
        pub inline fn x(args: @This()) u4 {
            return @truncate(args.data >> 8);
        }
    };
};

test Args {
    const args: Args = .{ .data = 0xABCD };
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
