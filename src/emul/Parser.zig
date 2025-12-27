const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;

const zengine = @import("zengine");
const allocators = zengine.allocators;
const global = zengine.global;
const str = zengine.str;
const ArrayMap = zengine.containers.ArrayMap;

const Syntax = @import("Syntax.zig");
const Prog = @import("Prog.zig");
const Token = Syntax.Token;
const Node = Syntax.Node;

const log = std.log.scoped(.parser);

pub fn parse(self: *const Syntax, gpa: Allocator) !Prog {
    var p = try Parser.init(self, gpa);
    defer p.deinit(gpa);
    var result = try Prog.init(gpa, 32);
    errdefer result.deinit(gpa);
    log.info("parsing", .{});
    p.run(gpa, &result) catch |err| {
        if (p.f) |f| {
            var iter = result.fs.map.iterator();
            while (iter.next()) |e| {
                if (f == e.value_ptr) log.err("in fn {s}", .{e.key_ptr.*});
            }
        }
        if (p.v) |v| {
            var iter = result.vs.map.iterator();
            while (iter.next()) |e| {
                if (v == e.value_ptr) log.err("in var {s}", .{e.key_ptr.*});
            }
        }
        try p.dump(gpa);
        return err;
    };
    try p.dump(gpa);
    return result;
}

self: *const Syntax,
stack: std.ArrayList(State),
defs: ArrayMap(*Token),
idx: usize,
f_node: ?*Node = null,
f: ?*Prog.Function = null,
v_node: ?*Node = null,
v: ?*Prog.Variable = null,

const Parser = @This();

const State = enum {
    invalid,
    begin,
    end,

    @"fn",
    in_fn,

    @"var",
    in_var,

    def,
    undef,

    undefined_def,
    expected_label,
    expected_expr,
    expected_const,
    expected_register,
    expected_register_range,
    expected_i_register,
    expected_byte,
    expected_address,
    expected_v0_address,
    expected_ld_arg0,
    expected_ld_r_arg1,
    expected_ld_rr_arg1,
    expected_ld_i_arg1,
    expected_ld_ia_arg1,

    pub const default: State = .begin;
};

pub fn init(self: *const Syntax, gpa: Allocator) !Parser {
    var result: Parser = .{
        .self = self,
        .stack = try .initCapacity(gpa, 16),
        .defs = try .init(gpa, 16),
        .idx = 0,
    };
    result.stack.appendAssumeCapacity(.begin);
    return result;
}

pub fn deinit(p: *Parser, gpa: Allocator) void {
    p.stack.deinit(gpa);
    p.defs.deinit(gpa);
}

pub fn run(p: *Parser, gpa: Allocator, result: *Prog) !void {
    loop: switch (p.stack.getLast()) {
        .invalid => {
            const n = p.peek() orelse unreachable;
            const t = &n.token;
            log.err("invalid token \"{s}\" line {}:{}", .{
                p.self.sliceFor(t),
                t.line,
                t.line_pos,
            });
            return error.InvalidToken;
        },
        .begin => {
            const n = p.peek() orelse continue :loop .end;
            const t = &n.token;
            switch (t.tag.type) {
                .@"fn" => continue :loop .@"fn",
                .@"var" => continue :loop .@"var",
                .def => continue :loop .def,
                .undef => continue :loop .undef,
                else => {
                    log.err(
                        "expected fn, var, def or undef found \"{s}\"",
                        .{p.self.sliceFor(t)},
                    );
                    continue :loop .invalid;
                },
            }
        },
        .end => {
            if (p.stack.getLast() != .end) try p.stack.append(gpa, .end);
            return;
        },
        .@"fn" => {
            if (p.v != null) {
                while (p.stack.pop() != .in_var) {}
                p.v_node = null;
                p.v = null;
            }
            if (p.f != null) {
                while (p.stack.pop() != .in_fn) {}
                p.f_node = null;
                p.f = null;
            }

            const pn = p.take();
            const pt = &pn.token;
            assert(pt.tag.type == .@"fn");
            try p.stack.append(gpa, .in_fn);

            const n = p.peek() orelse continue :loop .expected_label;
            const t = &n.token;
            switch (t.tag.type) {
                .label => {
                    const l = p.self.sliceFor(t);
                    p.f = result.function(gpa, l) catch |err| switch (err) {
                        error.AlreadyExists => {
                            log.err("duplicate function \"{s}\" line {}:{}", .{
                                l, t.line, t.line_pos,
                            });
                            return error.DuplicateFunction;
                        },
                        else => return err,
                    };
                    _ = p.take();
                    p.f_node = pn;
                    pn.child_len = 1;
                    continue :loop .in_fn;
                },
                else => continue :loop .expected_label,
            }
        },
        .in_fn => {
            assert(p.f_node != null);
            assert(p.f != null);
            assert(p.v_node == null);
            assert(p.v == null);
            const f_node = p.f_node.?;
            const f = p.f.?;
            const n = p.peek() orelse continue :loop .end;
            const t = &n.token;
            switch (t.tag.type) {
                .@"fn" => continue :loop .@"fn",
                .@"var" => continue :loop .@"var",
                .def => continue :loop .def,
                .undef => continue :loop .undef,
                .expr_label => try f.label(gpa, p.self.sliceFor(t)[1..]),
                .inst_cls => try f.inst(gpa, .encode(.cls, .{}, .{})),
                .inst_ret => try f.inst(gpa, .encode(.ret, .{}, .{})),
                .inst_sys => {
                    continue :loop .invalid;
                    // _ = p.take();
                    // f_node.child_len += 1;
                    // const an = p.peek() orelse continue :loop .expected_address;
                    // const at = &an.token;
                    // switch (at.tag.type) {
                    //     .expr_c_hex, .expr_c_dec => try f.inst(gpa, .encode(.sys_a, .{
                    //         .nnn = @intCast(at.tag.data),
                    //     }, .{})),
                    //     .expr_ref => try f.inst(gpa, .encode(.sys_a, .{}, .{
                    //         .nnn = p.self.sliceFor(at)[1..],
                    //     })),
                    //     else => continue :loop .expected_address,
                    // }
                },
                .inst_jp => {
                    _ = p.take();
                    f_node.child_len += 1;
                    n.child_len += 1;
                    const xn = p.peek() orelse continue :loop .expected_v0_address;
                    var xt = &xn.token;
                    sx: switch (xt.tag.type) {
                        .expr_c_hex, .expr_c_dec => {
                            try f.inst(gpa, .encode(.jp_a, .{
                                .nnn = @intCast(xt.tag.data),
                            }, .{}));
                        },
                        .expr_ref => {
                            try f.inst(gpa, .encode(.jp_a, .{}, .{
                                .nnn = p.self.sliceFor(xt)[1..],
                            }));
                        },
                        .expr_r => {
                            _ = p.take();
                            f_node.child_len += 1;
                            n.child_len += 1;
                            assert(xt.tag.data == 0);
                            const an = p.peek() orelse continue :loop .expected_const;
                            var at = &an.token;
                            sa: switch (at.tag.type) {
                                .expr_c_hex, .expr_c_dec => try f.inst(gpa, .encode(.jp_v0a, .{
                                    .nnn = @intCast(at.tag.data),
                                }, .{})),
                                .expr_def => {
                                    const dl = p.self.sliceFor(at)[1..];
                                    if (!p.defs.contains(dl)) continue :loop .undefined_def;
                                    at = p.defs.get(dl);
                                    continue :sa at.tag.type;
                                },
                                else => continue :loop .expected_const,
                            }
                        },
                        .expr_def => {
                            const dl = p.self.sliceFor(xt)[1..];
                            if (!p.defs.contains(dl)) continue :loop .undefined_def;
                            xt = p.defs.get(dl);
                            continue :sx xt.tag.type;
                        },
                        else => continue :loop .expected_v0_address,
                    }
                },
                .inst_call => {
                    _ = p.take();
                    f_node.child_len += 1;
                    n.child_len = 1;
                    const an = p.peek() orelse continue :loop .expected_address;
                    var at = &an.token;
                    sa: switch (at.tag.type) {
                        .expr_c_hex, .expr_c_dec => try f.inst(gpa, .encode(.call_a, .{
                            .nnn = @intCast(at.tag.data),
                        }, .{})),
                        .expr_ref => try f.inst(gpa, .encode(.call_a, .{}, .{
                            .nnn = p.self.sliceFor(at)[1..],
                        })),
                        .expr_def => {
                            const dl = p.self.sliceFor(at)[1..];
                            if (!p.defs.contains(dl)) continue :loop .undefined_def;
                            at = p.defs.get(dl);
                            continue :sa at.tag.type;
                        },
                        else => continue :loop .expected_address,
                    }
                },
                .inst_se => {
                    _ = p.take();
                    f_node.child_len += 2;
                    n.child_len = 2;
                    const xn = p.peek() orelse continue :loop .expected_register;
                    var xt = &xn.token;
                    sx: switch (xt.tag.type) {
                        .expr_r => _ = p.take(),
                        .expr_def => {
                            const dl = p.self.sliceFor(xt)[1..];
                            if (!p.defs.contains(dl)) continue :loop .undefined_def;
                            xt = p.defs.get(dl);
                            continue :sx xt.tag.type;
                        },
                        else => continue :loop .expected_register,
                    }
                    const yn = p.peek() orelse continue :loop .expected_byte;
                    var yt = &yn.token;
                    sy: switch (yt.tag.type) {
                        .expr_r => try f.inst(gpa, .encode(.se_rr, .{
                            .x = @intCast(xt.tag.data),
                            .y = @intCast(yt.tag.data),
                        }, .{})),
                        .expr_c_hex, .expr_c_dec => try f.inst(gpa, .encode(.se_rc, .{
                            .x = @intCast(xt.tag.data),
                            .kk = @intCast(yt.tag.data),
                        }, .{})),
                        .expr_def => {
                            const dl = p.self.sliceFor(yt)[1..];
                            if (!p.defs.contains(dl)) continue :loop .undefined_def;
                            yt = p.defs.get(dl);
                            continue :sy yt.tag.type;
                        },
                        else => continue :loop .expected_byte,
                    }
                },
                .inst_sne => {
                    _ = p.take();
                    f_node.child_len += 2;
                    n.child_len = 2;
                    const xn = p.peek() orelse continue :loop .expected_register;
                    var xt = &xn.token;
                    sx: switch (xt.tag.type) {
                        .expr_r => _ = p.take(),
                        .expr_def => {
                            const dl = p.self.sliceFor(xt)[1..];
                            if (!p.defs.contains(dl)) continue :loop .undefined_def;
                            xt = p.defs.get(dl);
                            continue :sx xt.tag.type;
                        },
                        else => continue :loop .expected_register,
                    }
                    const yn = p.peek() orelse continue :loop .expected_byte;
                    var yt = &yn.token;
                    sy: switch (yt.tag.type) {
                        .expr_r => try f.inst(gpa, .encode(.sne_rr, .{
                            .x = @intCast(xt.tag.data),
                            .y = @intCast(yt.tag.data),
                        }, .{})),
                        .expr_c_hex, .expr_c_dec => try f.inst(gpa, .encode(.sne_rc, .{
                            .x = @intCast(xt.tag.data),
                            .kk = @intCast(yt.tag.data),
                        }, .{})),
                        .expr_def => {
                            const dl = p.self.sliceFor(yt)[1..];
                            if (!p.defs.contains(dl)) continue :loop .undefined_def;
                            yt = p.defs.get(dl);
                            continue :sy yt.tag.type;
                        },
                        else => continue :loop .expected_byte,
                    }
                },
                .inst_ld => {
                    // OpCode | arg0   | arg1   | arg2 |
                    //--------+--------+--------+------+
                    // ld_rc    v[x]     #kk
                    // ld_rr    v[x]     v[y]
                    // ld_rd    v[x]     dt
                    // ld_rk    v[x]     $key
                    // ld_ria   v[0]     i[]
                    // ld_rria  v[x]     i[]
                    // ld_ria   v[0...x] i[]
                    // ld_rria  v[x...y] i[]
                    // ld_rst   v[0...x] $store
                    // ld_ia    i        #nnn
                    // ld_ia    i        @ref
                    // ld_nnnn  i        #nnnn
                    // ld_fr    i        $font    v[x]
                    // ld_fbr   i        $fontbig v[x]
                    // ld_dr    dt       v[x]
                    // ld_sr    st       v[x]
                    // ld_iar   i[]      v[0]
                    // ld_iarr  i[]      v[x]
                    // ld_iar   i[]      v[0...x]
                    // ld_iarr  i[]      v[x...y]
                    // ld_br    i[]      $bcd     v[x]
                    // ld_plane $plane   #x
                    // ld_audio $audio
                    // ld_pitch $pitch
                    // ld_str   $store   v[0...x]
                    // register, const,
                    _ = p.take();
                    f_node.child_len += 2;
                    n.child_len = 2;
                    var an: [3]*Node = undefined;
                    var at: [3]*Token = undefined;
                    an[0] = p.peek() orelse continue :loop .expected_ld_arg0;
                    at[0] = &an[0].token;
                    s0: switch (at[0].tag.type) {
                        // ld_rc    v[x]     #kk
                        // ld_rr    v[x]     v[y]
                        // ld_rd    v[x]     dt
                        // ld_rk    v[x]     $key
                        // ld_ria   v[0]     i[]
                        // ld_rria  v[x]     i[]
                        .expr_r => {
                            _ = p.take();
                            an[1] = p.peek() orelse continue :loop .expected_ld_r_arg1;
                            at[1] = &an[1].token;
                            s1: switch (at[1].tag.type) {
                                .expr_c_hex, .expr_c_dec => try f.inst(gpa, .encode(.ld_rc, .{
                                    .x = @intCast(at[0].tag.data),
                                    .kk = @intCast(at[1].tag.data),
                                }, .{})),
                                .expr_r => try f.inst(gpa, .encode(.ld_rr, .{
                                    .x = @intCast(at[0].tag.data),
                                    .y = @intCast(at[1].tag.data),
                                }, .{})),
                                .expr_d => try f.inst(gpa, .encode(.ld_rd, .{
                                    .x = @intCast(at[0].tag.data),
                                }, .{})),
                                .expr_k => try f.inst(gpa, .encode(.ld_rk, .{
                                    .x = @intCast(at[0].tag.data),
                                }, .{})),
                                .expr_ia => if (at[0].tag.data == 0)
                                    try f.inst(gpa, .encode(.ld_ria, .{
                                        .x = 0,
                                    }, .{}))
                                else
                                    try f.inst(gpa, .encode(.ld_rria, .{
                                        .x = @intCast(at[0].tag.data),
                                        .y = @intCast(at[0].tag.data),
                                    }, .{})),
                                .expr_def => {
                                    const dl = p.self.sliceFor(at[1])[1..];
                                    if (!p.defs.contains(dl)) continue :loop .undefined_def;
                                    at[1] = p.defs.get(dl);
                                    continue :s1 at[1].tag.type;
                                },
                                else => continue :loop .expected_ld_r_arg1,
                            }
                        },
                        // ld_ria   v[0...x] i[]
                        // ld_rria  v[x...y] i[]
                        // ld_rst   v[0...x] $store
                        .expr_rr => {
                            _ = p.take();
                            an[1] = p.peek() orelse continue :loop .expected_ld_rr_arg1;
                            at[1] = &an[1].token;
                            const x0: u4 = @intCast(at[0].tag.data & 0xF);
                            const x1: u4 = @intCast((at[0].tag.data >> 4) & 0xF);
                            s1: switch (at[1].tag.type) {
                                .expr_ia => if (x0 == 0)
                                    try f.inst(gpa, .encode(.ld_ria, .{
                                        .x = x1,
                                    }, .{}))
                                else
                                    try f.inst(gpa, .encode(.ld_rria, .{
                                        .x = x0,
                                        .y = x1,
                                    }, .{})),
                                .expr_st => {
                                    assert(x0 == 0);
                                    try f.inst(gpa, .encode(.ld_rst, .{
                                        .x = x1,
                                    }, .{}));
                                },
                                .expr_def => {
                                    const dl = p.self.sliceFor(at[1])[1..];
                                    if (!p.defs.contains(dl)) continue :loop .undefined_def;
                                    at[1] = p.defs.get(dl);
                                    continue :s1 at[1].tag.type;
                                },
                                else => continue :loop .expected_ld_rr_arg1,
                            }
                        },
                        // ld_ia    i        #nnn
                        // ld_ia    i        @ref
                        // ld_nnnn  i        #nnnn
                        // ld_fr    i        $font    v[x]
                        // ld_fbr   i        $fontbig v[x]
                        .expr_i => {
                            _ = p.take();
                            an[1] = p.peek() orelse continue :loop .expected_ld_i_arg1;
                            at[1] = &an[1].token;
                            s1: switch (at[1].tag.type) {
                                .expr_c_hex, .expr_c_dec => if (at[1].tag.data & 0xF000 != 0)
                                    try f.inst(gpa, .encode(.ld_nnnn, .{
                                        .nnnn = at[1].tag.data,
                                    }, .{}))
                                else
                                    try f.inst(gpa, .encode(.ld_ia, .{
                                        .nnn = @intCast(at[1].tag.data),
                                    }, .{})),
                                .expr_f => {
                                    _ = p.take();
                                    f_node.child_len += 1;
                                    n.child_len += 1;
                                    an[2] = p.peek() orelse continue :loop .expected_register;
                                    at[2] = &an[2].token;
                                    s2: switch (at[2].tag.type) {
                                        .expr_r => try f.inst(gpa, .encode(.ld_fr, .{
                                            .x = @intCast(at[2].tag.data),
                                        }, .{})),
                                        .expr_def => {
                                            const dl = p.self.sliceFor(at[2])[1..];
                                            if (!p.defs.contains(dl)) continue :loop .undefined_def;
                                            at[2] = p.defs.get(dl);
                                            continue :s2 at[2].tag.type;
                                        },
                                        else => continue :loop .expected_register,
                                    }
                                },
                                .expr_fb => {
                                    _ = p.take();
                                    f_node.child_len += 1;
                                    n.child_len += 1;
                                    an[2] = p.peek() orelse continue :loop .expected_register;
                                    at[2] = &an[2].token;
                                    s2: switch (at[2].tag.type) {
                                        .expr_r => try f.inst(gpa, .encode(.ld_fbr, .{
                                            .x = @intCast(at[2].tag.data),
                                        }, .{})),
                                        .expr_def => {
                                            const dl = p.self.sliceFor(at[2])[1..];
                                            if (!p.defs.contains(dl)) continue :loop .undefined_def;
                                            at[2] = p.defs.get(dl);
                                            continue :s2 at[2].tag.type;
                                        },
                                        else => continue :loop .expected_register,
                                    }
                                },
                                .expr_ref => try f.inst(gpa, .encode(.ld_ia, .{}, .{
                                    .nnn = p.self.sliceFor(at[1])[1..],
                                })),
                                .expr_def => {
                                    const dl = p.self.sliceFor(at[1])[1..];
                                    if (!p.defs.contains(dl)) continue :loop .undefined_def;
                                    at[1] = p.defs.get(dl);
                                    log.info("sub def \"{s}\" {t} ({X:04}) \"{s}\"", .{
                                        dl,
                                        at[1].tag.type,
                                        at[1].tag.data,
                                        p.self.sliceFor(at[1]),
                                    });
                                    continue :s1 at[1].tag.type;
                                },
                                else => continue :loop .expected_ld_i_arg1,
                            }
                        },
                        // ld_dr    dt       v[x]
                        .expr_d => {
                            _ = p.take();
                            an[1] = p.peek() orelse continue :loop .expected_register;
                            at[1] = &an[1].token;
                            s1: switch (at[1].tag.type) {
                                .expr_r => try f.inst(gpa, .encode(.ld_dr, .{
                                    .x = @intCast(at[1].tag.data),
                                }, .{})),
                                .expr_def => {
                                    const dl = p.self.sliceFor(at[1])[1..];
                                    if (!p.defs.contains(dl)) continue :loop .undefined_def;
                                    at[1] = p.defs.get(dl);
                                    continue :s1 at[1].tag.type;
                                },
                                else => continue :loop .expected_register,
                            }
                        },
                        // ld_sr    st       v[x]
                        .expr_s => {
                            _ = p.take();
                            an[1] = p.peek() orelse continue :loop .expected_register;
                            at[1] = &an[1].token;
                            s1: switch (at[1].tag.type) {
                                .expr_r => try f.inst(gpa, .encode(.ld_dr, .{
                                    .x = @intCast(at[1].tag.data),
                                }, .{})),
                                .expr_def => {
                                    const dl = p.self.sliceFor(at[1])[1..];
                                    if (!p.defs.contains(dl)) continue :loop .undefined_def;
                                    at[1] = p.defs.get(dl);
                                    continue :s1 at[1].tag.type;
                                },
                                else => continue :loop .expected_register,
                            }
                        },
                        // ld_iar   i[]      v[0]
                        // ld_iarr  i[]      v[x]
                        // ld_iar   i[]      v[0...x]
                        // ld_iarr  i[]      v[x...y]
                        // ld_br    i[]      $bcd     v[x]
                        .expr_ia => {
                            _ = p.take();
                            an[1] = p.peek() orelse continue :loop .expected_ld_ia_arg1;
                            at[1] = &an[1].token;
                            s1: switch (at[1].tag.type) {
                                .expr_r => if (at[1].tag.data == 0)
                                    try f.inst(gpa, .encode(.ld_iar, .{
                                        .x = 0,
                                    }, .{}))
                                else
                                    try f.inst(gpa, .encode(.ld_iarr, .{
                                        .x = @intCast(at[1].tag.data),
                                        .y = @intCast(at[1].tag.data),
                                    }, .{})),
                                .expr_rr => {
                                    const y0: u4 = @intCast(at[1].tag.data & 0xF);
                                    const y1: u4 = @intCast((at[1].tag.data >> 4) & 0xF);
                                    if (y0 == 0) try f.inst(gpa, .encode(.ld_iar, .{
                                        .x = y1,
                                    }, .{})) else try f.inst(gpa, .encode(.ld_iarr, .{
                                        .x = y0,
                                        .y = y1,
                                    }, .{}));
                                },
                                .expr_b => {
                                    _ = p.take();
                                    f_node.child_len += 1;
                                    n.child_len += 1;
                                    an[2] = p.peek() orelse continue :loop .expected_register;
                                    at[2] = &an[2].token;
                                    s2: switch (at[2].tag.type) {
                                        .expr_r => try f.inst(gpa, .encode(.ld_br, .{
                                            .x = @intCast(at[2].tag.data),
                                        }, .{})),
                                        .expr_def => {
                                            const dl = p.self.sliceFor(at[2])[1..];
                                            if (!p.defs.contains(dl)) continue :loop .undefined_def;
                                            at[2] = p.defs.get(dl);
                                            continue :s2 at[2].tag.type;
                                        },
                                        else => continue :loop .expected_register,
                                    }
                                },
                                .expr_def => {
                                    const dl = p.self.sliceFor(at[1])[1..];
                                    if (!p.defs.contains(dl)) continue :loop .undefined_def;
                                    at[1] = p.defs.get(dl);
                                    continue :s1 at[1].tag.type;
                                },
                                else => continue :loop .expected_ld_ia_arg1,
                            }
                        },
                        // ld_plane $plane   #x
                        .expr_plane => {
                            _ = p.take();
                            an[1] = p.peek() orelse continue :loop .expected_const;
                            at[1] = &an[1].token;
                            s1: switch (at[1].tag.type) {
                                .expr_c_hex, .expr_c_dec => try f.inst(gpa, .encode(.ld_plane, .{
                                    .x = @intCast(at[1].tag.data),
                                }, .{})),
                                .expr_def => {
                                    const dl = p.self.sliceFor(at[1])[1..];
                                    if (!p.defs.contains(dl)) continue :loop .undefined_def;
                                    at[1] = p.defs.get(dl);
                                    continue :s1 at[1].tag.type;
                                },
                                else => continue :loop .expected_const,
                            }
                        },
                        // ld_audio $audio
                        .expr_audio => try f.inst(gpa, .encode(.ld_audio, .{}, .{})),
                        // ld_pitch $pitch
                        .expr_pitch => try f.inst(gpa, .encode(.ld_pitch, .{}, .{})),
                        // ld_str   $store   v[0...x]
                        .expr_st => {
                            _ = p.take();
                            an[1] = p.peek() orelse continue :loop .expected_register_range;
                            at[1] = &an[1].token;
                            s1: switch (at[1].tag.type) {
                                .expr_rr => {
                                    const y0: u4 = @intCast(at[1].tag.data & 0xF);
                                    const y1: u4 = @intCast((at[1].tag.data >> 4) & 0xF);
                                    assert(y0 == 0);
                                    try f.inst(gpa, .encode(.ld_str, .{
                                        .x = y1,
                                    }, .{}));
                                },
                                .expr_def => {
                                    const dl = p.self.sliceFor(at[1])[1..];
                                    if (!p.defs.contains(dl)) continue :loop .undefined_def;
                                    at[1] = p.defs.get(dl);
                                    continue :s1 at[1].tag.type;
                                },
                                else => continue :loop .expected_register_range,
                            }
                        },
                        .expr_def => {
                            const dl = p.self.sliceFor(at[0])[1..];
                            if (!p.defs.contains(dl)) continue :loop .undefined_def;
                            at[0] = p.defs.get(dl);
                            continue :s0 at[0].tag.type;
                        },
                        else => continue :loop .expected_ld_arg0,
                    }
                },
                .inst_add => {
                    _ = p.take();
                    f_node.child_len += 2;
                    n.child_len = 2;
                    const xn = p.peek() orelse continue :loop .expected_i_register;
                    var xt = &xn.token;
                    sx: switch (xt.tag.type) {
                        .expr_i => {
                            _ = p.take();
                            const yn = p.peek() orelse continue :loop .expected_register;
                            var yt = &yn.token;
                            sy: switch (yt.tag.type) {
                                .expr_r => try f.inst(gpa, .encode(.add_ir, .{
                                    .x = @intCast(yt.tag.data),
                                }, .{})),
                                .expr_def => {
                                    const dl = p.self.sliceFor(yt)[1..];
                                    if (!p.defs.contains(dl)) continue :loop .undefined_def;
                                    yt = p.defs.get(dl);
                                    continue :sy yt.tag.type;
                                },
                                else => continue :loop .expected_register,
                            }
                        },
                        .expr_r => {
                            _ = p.take();
                            const yn = p.peek() orelse continue :loop .expected_byte;
                            var yt = &yn.token;
                            sy: switch (yt.tag.type) {
                                .expr_c_hex, .expr_c_dec => try f.inst(gpa, .encode(.add_rc, .{
                                    .x = @intCast(xt.tag.data),
                                    .kk = @intCast(yt.tag.data),
                                }, .{})),
                                .expr_r => try f.inst(gpa, .encode(.add_rr, .{
                                    .x = @intCast(xt.tag.data),
                                    .y = @intCast(yt.tag.data),
                                }, .{})),
                                .expr_def => {
                                    const dl = p.self.sliceFor(yt)[1..];
                                    if (!p.defs.contains(dl)) continue :loop .undefined_def;
                                    yt = p.defs.get(dl);
                                    continue :sy yt.tag.type;
                                },
                                else => continue :loop .expected_byte,
                            }
                        },
                        .expr_def => {
                            const dl = p.self.sliceFor(xt)[1..];
                            if (!p.defs.contains(dl)) continue :loop .undefined_def;
                            xt = p.defs.get(dl);
                            continue :sx xt.tag.type;
                        },
                        else => continue :loop .expected_i_register,
                    }
                },
                .inst_or => {
                    _ = p.take();
                    f_node.child_len += 2;
                    n.child_len = 2;
                    const xn = p.peek() orelse continue :loop .expected_register;
                    var xt = &xn.token;
                    sx: switch (xt.tag.type) {
                        .expr_r => _ = p.take(),
                        .expr_def => {
                            const dl = p.self.sliceFor(xt)[1..];
                            if (!p.defs.contains(dl)) continue :loop .undefined_def;
                            xt = p.defs.get(dl);
                            continue :sx xt.tag.type;
                        },
                        else => continue :loop .expected_register,
                    }
                    const yn = p.peek() orelse continue :loop .expected_register;
                    var yt = &yn.token;
                    sy: switch (yt.tag.type) {
                        .expr_r => try f.inst(gpa, .encode(.or_rr, .{
                            .x = @intCast(xt.tag.data),
                            .y = @intCast(yt.tag.data),
                        }, .{})),
                        .expr_def => {
                            const dl = p.self.sliceFor(yt)[1..];
                            if (!p.defs.contains(dl)) continue :loop .undefined_def;
                            yt = p.defs.get(dl);
                            continue :sy yt.tag.type;
                        },
                        else => continue :loop .expected_register,
                    }
                },
                .inst_and => {
                    _ = p.take();
                    f_node.child_len += 2;
                    n.child_len = 2;
                    const xn = p.peek() orelse continue :loop .expected_register;
                    var xt = &xn.token;
                    sx: switch (xt.tag.type) {
                        .expr_r => _ = p.take(),
                        .expr_def => {
                            const dl = p.self.sliceFor(xt)[1..];
                            if (!p.defs.contains(dl)) continue :loop .undefined_def;
                            xt = p.defs.get(dl);
                            continue :sx xt.tag.type;
                        },
                        else => continue :loop .expected_register,
                    }
                    const yn = p.peek() orelse continue :loop .expected_register;
                    var yt = &yn.token;
                    sy: switch (yt.tag.type) {
                        .expr_r => try f.inst(gpa, .encode(.and_rr, .{
                            .x = @intCast(xt.tag.data),
                            .y = @intCast(yt.tag.data),
                        }, .{})),
                        .expr_def => {
                            const dl = p.self.sliceFor(yt)[1..];
                            if (!p.defs.contains(dl)) continue :loop .undefined_def;
                            yt = p.defs.get(dl);
                            continue :sy yt.tag.type;
                        },
                        else => continue :loop .expected_register,
                    }
                },
                .inst_xor => {
                    _ = p.take();
                    f_node.child_len += 2;
                    n.child_len = 2;
                    const xn = p.peek() orelse continue :loop .expected_register;
                    var xt = &xn.token;
                    sx: switch (xt.tag.type) {
                        .expr_r => _ = p.take(),
                        .expr_def => {
                            const dl = p.self.sliceFor(xt)[1..];
                            if (!p.defs.contains(dl)) continue :loop .undefined_def;
                            xt = p.defs.get(dl);
                            continue :sx xt.tag.type;
                        },
                        else => continue :loop .expected_register,
                    }
                    const yn = p.peek() orelse continue :loop .expected_register;
                    var yt = &yn.token;
                    sy: switch (yt.tag.type) {
                        .expr_r => try f.inst(gpa, .encode(.xor_rr, .{
                            .x = @intCast(xt.tag.data),
                            .y = @intCast(yt.tag.data),
                        }, .{})),
                        .expr_def => {
                            const dl = p.self.sliceFor(yt)[1..];
                            if (!p.defs.contains(dl)) continue :loop .undefined_def;
                            yt = p.defs.get(dl);
                            continue :sy yt.tag.type;
                        },
                        else => continue :loop .expected_register,
                    }
                },
                .inst_sub => {
                    _ = p.take();
                    f_node.child_len += 2;
                    n.child_len = 2;
                    const xn = p.peek() orelse continue :loop .expected_register;
                    var xt = &xn.token;
                    sx: switch (xt.tag.type) {
                        .expr_r => _ = p.take(),
                        .expr_def => {
                            const dl = p.self.sliceFor(xt)[1..];
                            if (!p.defs.contains(dl)) continue :loop .undefined_def;
                            xt = p.defs.get(dl);
                            continue :sx xt.tag.type;
                        },
                        else => continue :loop .expected_register,
                    }
                    const yn = p.peek() orelse continue :loop .expected_register;
                    var yt = &yn.token;
                    sy: switch (yt.tag.type) {
                        .expr_r => try f.inst(gpa, .encode(.sub_rr, .{
                            .x = @intCast(xt.tag.data),
                            .y = @intCast(yt.tag.data),
                        }, .{})),
                        .expr_def => {
                            const dl = p.self.sliceFor(yt)[1..];
                            if (!p.defs.contains(dl)) continue :loop .undefined_def;
                            yt = p.defs.get(dl);
                            continue :sy yt.tag.type;
                        },
                        else => continue :loop .expected_register,
                    }
                },
                .inst_shr => {
                    _ = p.take();
                    f_node.child_len += 2;
                    n.child_len = 2;
                    const xn = p.peek() orelse continue :loop .expected_register;
                    var xt = &xn.token;
                    sx: switch (xt.tag.type) {
                        .expr_r => _ = p.take(),
                        .expr_def => {
                            const dl = p.self.sliceFor(xt)[1..];
                            if (!p.defs.contains(dl)) continue :loop .undefined_def;
                            xt = p.defs.get(dl);
                            continue :sx xt.tag.type;
                        },
                        else => continue :loop .expected_register,
                    }
                    const yn = p.peek() orelse continue :loop .expected_register;
                    var yt = &yn.token;
                    sy: switch (yt.tag.type) {
                        .expr_r => try f.inst(gpa, .encode(.shr_rr, .{
                            .x = @intCast(xt.tag.data),
                            .y = @intCast(yt.tag.data),
                        }, .{})),
                        .expr_def => {
                            const dl = p.self.sliceFor(yt)[1..];
                            if (!p.defs.contains(dl)) continue :loop .undefined_def;
                            yt = p.defs.get(dl);
                            continue :sy yt.tag.type;
                        },
                        else => continue :loop .expected_register,
                    }
                },
                .inst_subn => {
                    _ = p.take();
                    f_node.child_len += 2;
                    n.child_len = 2;
                    const xn = p.peek() orelse continue :loop .expected_register;
                    var xt = &xn.token;
                    sx: switch (xt.tag.type) {
                        .expr_r => _ = p.take(),
                        .expr_def => {
                            const dl = p.self.sliceFor(xt)[1..];
                            if (!p.defs.contains(dl)) continue :loop .undefined_def;
                            xt = p.defs.get(dl);
                            continue :sx xt.tag.type;
                        },
                        else => continue :loop .expected_register,
                    }
                    const yn = p.peek() orelse continue :loop .expected_register;
                    var yt = &yn.token;
                    sy: switch (yt.tag.type) {
                        .expr_r => try f.inst(gpa, .encode(.subn_rr, .{
                            .x = @intCast(xt.tag.data),
                            .y = @intCast(yt.tag.data),
                        }, .{})),
                        .expr_def => {
                            const dl = p.self.sliceFor(yt)[1..];
                            if (!p.defs.contains(dl)) continue :loop .undefined_def;
                            yt = p.defs.get(dl);
                            continue :sy yt.tag.type;
                        },
                        else => continue :loop .expected_register,
                    }
                },
                .inst_shl => {
                    _ = p.take();
                    f_node.child_len += 2;
                    n.child_len = 2;
                    const xn = p.peek() orelse continue :loop .expected_register;
                    var xt = &xn.token;
                    sx: switch (xt.tag.type) {
                        .expr_r => _ = p.take(),
                        .expr_def => {
                            const dl = p.self.sliceFor(xt)[1..];
                            if (!p.defs.contains(dl)) continue :loop .undefined_def;
                            xt = p.defs.get(dl);
                            continue :sx xt.tag.type;
                        },
                        else => continue :loop .expected_register,
                    }
                    const yn = p.peek() orelse continue :loop .expected_register;
                    var yt = &yn.token;
                    sy: switch (yt.tag.type) {
                        .expr_r => try f.inst(gpa, .encode(.shl_rr, .{
                            .x = @intCast(xt.tag.data),
                            .y = @intCast(yt.tag.data),
                        }, .{})),
                        .expr_def => {
                            const dl = p.self.sliceFor(yt)[1..];
                            if (!p.defs.contains(dl)) continue :loop .undefined_def;
                            yt = p.defs.get(dl);
                            continue :sy yt.tag.type;
                        },
                        else => continue :loop .expected_register,
                    }
                },
                .inst_rnd => {
                    _ = p.take();
                    f_node.child_len += 2;
                    n.child_len = 2;
                    const xn = p.peek() orelse continue :loop .expected_register;
                    var xt = &xn.token;
                    sx: switch (xt.tag.type) {
                        .expr_r => _ = p.take(),
                        .expr_def => {
                            const dl = p.self.sliceFor(xt)[1..];
                            if (!p.defs.contains(dl)) continue :loop .undefined_def;
                            xt = p.defs.get(dl);
                            continue :sx xt.tag.type;
                        },
                        else => continue :loop .expected_register,
                    }
                    const an = p.peek() orelse continue :loop .expected_const;
                    var at = &an.token;
                    sa: switch (at.tag.type) {
                        .expr_c_hex, .expr_c_dec => try f.inst(gpa, .encode(.rnd_rc, .{
                            .x = @intCast(xt.tag.data),
                            .kk = @intCast(at.tag.data),
                        }, .{})),
                        .expr_def => {
                            const dl = p.self.sliceFor(at)[1..];
                            if (!p.defs.contains(dl)) continue :loop .undefined_def;
                            at = p.defs.get(dl);
                            continue :sa at.tag.type;
                        },
                        else => continue :loop .expected_const,
                    }
                },
                .inst_drw => {
                    _ = p.take();
                    f_node.child_len += 3;
                    n.child_len = 3;
                    const xn = p.peek() orelse continue :loop .expected_register;
                    var xt = &xn.token;
                    sx: switch (xt.tag.type) {
                        .expr_r => _ = p.take(),
                        .expr_def => {
                            const dl = p.self.sliceFor(xt)[1..];
                            if (!p.defs.contains(dl)) continue :loop .undefined_def;
                            xt = p.defs.get(dl);
                            continue :sx xt.tag.type;
                        },
                        else => continue :loop .expected_register,
                    }
                    const yn = p.peek() orelse continue :loop .expected_register;
                    var yt = &yn.token;
                    sy: switch (yt.tag.type) {
                        .expr_r => _ = p.take(),
                        .expr_def => {
                            const dl = p.self.sliceFor(yt)[1..];
                            if (!p.defs.contains(dl)) continue :loop .undefined_def;
                            yt = p.defs.get(dl);
                            continue :sy yt.tag.type;
                        },
                        else => continue :loop .expected_register,
                    }
                    const an = p.peek() orelse continue :loop .expected_const;
                    var at = &an.token;
                    sa: switch (at.tag.type) {
                        .expr_c_hex, .expr_c_dec => try f.inst(gpa, .encode(.drw_rrc, .{
                            .x = @intCast(xt.tag.data),
                            .y = @intCast(yt.tag.data),
                            .n = @intCast(at.tag.data),
                        }, .{})),
                        .expr_def => {
                            const dl = p.self.sliceFor(at)[1..];
                            if (!p.defs.contains(dl)) continue :loop .undefined_def;
                            at = p.defs.get(dl);
                            continue :sa at.tag.type;
                        },
                        else => continue :loop .expected_const,
                    }
                },
                .inst_skp => {
                    _ = p.take();
                    f_node.child_len += 1;
                    n.child_len = 1;
                    const xn = p.peek() orelse continue :loop .expected_register;
                    var xt = &xn.token;
                    sx: switch (xt.tag.type) {
                        .expr_r => try f.inst(gpa, .encode(.skp_r, .{
                            .x = @intCast(xt.tag.data),
                        }, .{})),
                        .expr_def => {
                            const dl = p.self.sliceFor(xt)[1..];
                            if (!p.defs.contains(dl)) continue :loop .undefined_def;
                            xt = p.defs.get(dl);
                            continue :sx xt.tag.type;
                        },
                        else => continue :loop .expected_register,
                    }
                },
                .inst_sknp => {
                    _ = p.take();
                    f_node.child_len += 1;
                    n.child_len = 1;
                    const xn = p.peek() orelse continue :loop .expected_register;
                    var xt = &xn.token;
                    sx: switch (xt.tag.type) {
                        .expr_r => try f.inst(gpa, .encode(.sknp_r, .{
                            .x = @intCast(xt.tag.data),
                        }, .{})),
                        .expr_def => {
                            const dl = p.self.sliceFor(xt)[1..];
                            if (!p.defs.contains(dl)) continue :loop .undefined_def;
                            xt = p.defs.get(dl);
                            continue :sx xt.tag.type;
                        },
                        else => continue :loop .expected_register,
                    }
                },
                .inst_exit => try f.inst(gpa, .encode(.exit, .{}, .{})),
                .inst_scroll_d => {
                    _ = p.take();
                    f_node.child_len += 1;
                    n.child_len = 1;
                    const an = p.peek() orelse continue :loop .expected_register;
                    var at = &an.token;
                    sa: switch (at.tag.type) {
                        .expr_c_hex, .expr_c_dec => try f.inst(gpa, .encode(.scroll_dn, .{
                            // .n = @intCast(at.tag.data),
                        }, .{})),
                        .expr_def => {
                            const dl = p.self.sliceFor(at)[1..];
                            if (!p.defs.contains(dl)) continue :loop .undefined_def;
                            at = p.defs.get(dl);
                            continue :sa at.tag.type;
                        },
                        else => continue :loop .expected_register,
                    }
                },
                .inst_scroll_u => {
                    _ = p.take();
                    f_node.child_len += 1;
                    n.child_len = 1;
                    const an = p.peek() orelse continue :loop .expected_register;
                    var at = &an.token;
                    sa: switch (at.tag.type) {
                        .expr_c_hex, .expr_c_dec => try f.inst(gpa, .encode(.scroll_un, .{
                            // .n = @intCast(at.tag.data),
                        }, .{})),
                        .expr_def => {
                            const dl = p.self.sliceFor(at)[1..];
                            if (!p.defs.contains(dl)) continue :loop .undefined_def;
                            at = p.defs.get(dl);
                            continue :sa at.tag.type;
                        },
                        else => continue :loop .expected_register,
                    }
                },
                .inst_scroll_r => try f.inst(gpa, .encode(.scroll_r, .{}, .{})),
                .inst_scroll_l => try f.inst(gpa, .encode(.scroll_l, .{}, .{})),
                .inst_scr_lo => try f.inst(gpa, .encode(.scr_lo, .{}, .{})),
                .inst_scr_hi => try f.inst(gpa, .encode(.scr_hi, .{}, .{})),
                else => continue :loop .invalid,
            }
            _ = p.take();
            f_node.child_len += 1;
            continue :loop .in_fn;
        },
        .@"var" => {
            if (p.v != null) {
                while (p.stack.pop() != .in_var) {}
                p.v = null;
                p.v_node = null;
            }
            if (p.f_node) |f_node| f_node.child_len += 1;

            const pn = p.take();
            const pt = &pn.token;
            assert(pt.tag.type == .@"var");
            try p.stack.append(gpa, .in_var);

            const n = p.peek() orelse continue :loop .expected_label;
            const t = &n.token;
            switch (t.tag.type) {
                .label => {
                    const l = p.self.sliceFor(t);
                    p.v = result.variable(gpa, l) catch |err| switch (err) {
                        error.AlreadyExists => {
                            log.err("duplicate variable \"{s}\" line {}:{}", .{
                                l, t.line, t.line_pos,
                            });
                            return error.DuplicateVariable;
                        },
                        else => return err,
                    };
                    _ = p.take();
                    p.v_node = pn;
                    pn.child_len = 1;
                    if (p.f_node) |f_node| f_node.child_len += 1;
                    continue :loop .in_var;
                },
                else => continue :loop .expected_label,
            }
        },
        .in_var => {
            assert(p.v != null);
            const v_node = p.v_node.?;
            const v = p.v.?;
            const n = p.peek() orelse continue :loop .end;
            var t = &n.token;
            st: switch (t.tag.type) {
                .expr_c_hex, .expr_c_dec => {
                    _ = p.take();
                    v_node.child_len += 1;
                    if (p.f_node) |f_node| f_node.child_len += 1;
                    try v.val.append(gpa, @intCast(t.tag.data));
                    continue :loop .in_var;
                },
                .expr_def => {
                    const dl = p.self.sliceFor(t)[1..];
                    if (!p.defs.contains(dl)) continue :loop .undefined_def;
                    t = p.defs.get(dl);
                    continue :st t.tag.type;
                },
                else => {
                    while (p.stack.pop() != .in_var) {}
                    p.v_node = null;
                    p.v = null;
                    continue :loop p.stack.getLast();
                },
            }
        },
        .def => {
            if (p.v != null) {
                while (p.stack.pop() != .in_var) {}
                p.v_node = null;
                p.v = null;
            }
            if (p.f_node) |f_node| f_node.child_len += 3;

            const pn = p.take();
            const pt = &pn.token;
            assert(pt.tag.type == .def);
            pn.child_len = 2;

            const ln = p.peek() orelse continue :loop .expected_label;
            const lt = &ln.token;
            switch (lt.tag.type) {
                .label => {
                    _ = p.take();
                    const an = p.peek() orelse continue :loop .expected_expr;
                    const at = &an.token;
                    switch (at.tag.type) {
                        .expr_i,
                        .expr_d,
                        .expr_k,
                        .expr_s,
                        .expr_f,
                        .expr_c_hex,
                        .expr_c_dec,
                        .expr_r,
                        .expr_rr,
                        .expr_fb,
                        .expr_ia,
                        .expr_b,
                        .expr_st,
                        .expr_plane,
                        .expr_audio,
                        .expr_pitch,
                        .expr_ref,
                        .expr_def,
                        => {
                            _ = p.take();
                            const l = p.self.sliceFor(lt);
                            _ = p.defs.tryInsert(gpa, l, at) catch |err| switch (err) {
                                error.AlreadyExists => {
                                    log.err("duplicate definition \"{s}\" line {}:{}", .{
                                        l, lt.line, lt.line_pos,
                                    });
                                    return error.DuplicateDefinition;
                                },
                                else => return err,
                            };
                            continue :loop p.stack.getLast();
                        },
                        else => continue :loop .expected_expr,
                    }
                },
                else => continue :loop .expected_label,
            }
        },
        .undef => {
            if (p.v != null) {
                while (p.stack.pop() != .in_var) {}
                p.v_node = null;
                p.v = null;
            }
            if (p.f_node) |f_node| f_node.child_len += 2;

            const pn = p.take();
            const pt = &pn.token;
            assert(pt.tag.type == .undef);
            pn.child_len = 1;

            const ln = p.peek() orelse continue :loop .expected_label;
            const lt = &ln.token;
            switch (lt.tag.type) {
                .label => {
                    _ = p.take();
                    const l = p.self.sliceFor(lt);
                    if (!p.defs.contains(l)) continue :loop .undefined_def;
                    p.defs.remove(l);
                    continue :loop p.stack.getLast();
                },
                else => continue :loop .expected_label,
            }
        },
        .undefined_def => {
            const n = p.peek() orelse unreachable;
            const t = &n.token;
            log.err("definition \"{s}\" is undefined line {}:{}", .{
                p.self.sliceFor(t)[1..],
                t.line,
                t.line_pos,
            });
            return error.UndefinedDefinition;
        },
        .expected_label => {
            const n = p.peek() orelse {
                log.err("expected label found end of stream", .{});
                return error.ExpectedLabel;
            };
            const t = &n.token;
            log.err("expected label found \"{s}\" ({t}) line {}:{}", .{
                p.self.sliceFor(t),
                t.tag.type,
                t.line,
                t.line_pos,
            });
            return error.ExpectedLabel;
        },
        .expected_expr => {
            const n = p.peek() orelse {
                log.err("expected expression found end of stream", .{});
                return error.ExpectedExpr;
            };
            const t = &n.token;
            log.err("expected expression found \"{s}\" ({t}) line {}:{}", .{
                p.self.sliceFor(t),
                t.tag.type,
                t.line,
                t.line_pos,
            });
            return error.ExpectedExpr;
        },
        .expected_const => {
            const n = p.peek() orelse {
                log.err("expected const found end of stream", .{});
                return error.ExpectedConst;
            };
            const t = &n.token;
            log.err("expected const found \"{s}\" ({t}) line {}:{}", .{
                p.self.sliceFor(t),
                t.tag.type,
                t.line,
                t.line_pos,
            });
            return error.ExpectedConst;
        },
        .expected_register => {
            const n = p.peek() orelse {
                log.err("expected register found end of stream", .{});
                return error.ExpectedRegister;
            };
            const t = &n.token;
            log.err("expected register found \"{s}\" ({t}) line {}:{}", .{
                p.self.sliceFor(t),
                t.tag.type,
                t.line,
                t.line_pos,
            });
            return error.ExpectedRegister;
        },
        .expected_register_range => {
            const n = p.peek() orelse {
                log.err("expected register range found end of stream", .{});
                return error.ExpectedRegisterRange;
            };
            const t = &n.token;
            log.err("expected register range found \"{s}\" ({t}) line {}:{}", .{
                p.self.sliceFor(t),
                t.tag.type,
                t.line,
                t.line_pos,
            });
            return error.ExpectedRegisterRange;
        },
        .expected_i_register => {
            const n = p.peek() orelse {
                log.err("expected i or register found end of stream", .{});
                return error.ExpectedIRegister;
            };
            const t = &n.token;
            log.err("expected i or register found \"{s}\" ({t}) line {}:{}", .{
                p.self.sliceFor(t),
                t.tag.type,
                t.line,
                t.line_pos,
            });
            return error.ExpectedIRegister;
        },
        .expected_byte => {
            const n = p.peek() orelse {
                log.err("expected constant or register found end of stream", .{});
                return error.ExpectedByte;
            };
            const t = &n.token;
            log.err("expected constant or register found \"{s}\" ({t}) line {}:{}", .{
                p.self.sliceFor(t),
                t.tag.type,
                t.line,
                t.line_pos,
            });
            return error.ExpectedByte;
        },
        .expected_address => {
            const n = p.peek() orelse {
                log.err("expected constant or ref found end of stream", .{});
                return error.ExpectedAddress;
            };
            const t = &n.token;
            log.err("expected constant or ref found \"{s}\" ({t}) line {}:{}", .{
                p.self.sliceFor(t),
                t.tag.type,
                t.line,
                t.line_pos,
            });
            return error.ExpectedAddress;
        },
        .expected_v0_address => {
            const n = p.peek() orelse {
                log.err("expected v[0], constant or ref found end of stream", .{});
                return error.ExpectedV0Address;
            };
            const t = &n.token;
            log.err("expected v[0], constant or ref found \"{s}\" ({t}) line {}:{}", .{
                p.self.sliceFor(t),
                t.tag.type,
                t.line,
                t.line_pos,
            });
            return error.ExpectedV0Address;
        },
        .expected_ld_arg0 => {
            const n = p.peek() orelse {
                log.err(
                    "expected register, register range, i, dt, st, i[], $plane, $audio, $pitch or $store found end of stream",
                    .{},
                );
                return error.ExpectedLdArg0;
            };
            const t = &n.token;
            log.err("expected register, register range, i, dt, st, i[], $plane, $audio, $pitch or $store found \"{s}\" ({t}) line {}:{}", .{
                p.self.sliceFor(t),
                t.tag.type,
                t.line,
                t.line_pos,
            });
            return error.ExpectedLdArg0;
        },
        .expected_ld_r_arg1 => {
            const n = p.peek() orelse {
                log.err("expected register, constant, dt, i[] or $key found end of stream", .{});
                return error.ExpectedLdRArg1;
            };
            const t = &n.token;
            log.err("expected register, constant, dt, i[] or $key found \"{s}\" ({t})  line {}:{}", .{
                p.self.sliceFor(t),
                t.tag.type,
                t.line,
                t.line_pos,
            });
            return error.ExpectedLdRArg1;
        },
        .expected_ld_rr_arg1 => {
            const n = p.peek() orelse {
                log.err("expected i[] or $store found end of stream", .{});
                return error.ExpectedLdRrArg1;
            };
            const t = &n.token;
            log.err("expected i[] or $store found \"{s}\" ({t}) line {}:{}", .{
                p.self.sliceFor(t),
                t.tag.type,
                t.line,
                t.line_pos,
            });
            return error.ExpectedLdRrArg1;
        },
        .expected_ld_i_arg1 => {
            const n = p.peek() orelse {
                log.err("expected constant, ref, $font or $fontbig found end of stream", .{});
                return error.ExpectedLdIArg1;
            };
            const t = &n.token;
            log.err("expected constant, ref, $font or $fontbig found \"{s}\" ({t}) line {}:{}", .{
                p.self.sliceFor(t),
                t.tag.type,
                t.line,
                t.line_pos,
            });
            return error.ExpectedLdIArg1;
        },
        .expected_ld_ia_arg1 => {
            const n = p.peek() orelse {
                log.err("expected register, register range, $bcd found end of stream", .{});
                return error.ExpectedLdIaArg1;
            };
            const t = &n.token;
            log.err("expected register, register range, $bcd found \"{s}\" ({t}) line {}:{}", .{
                p.self.sliceFor(t),
                t.tag.type,
                t.line,
                t.line_pos,
            });
            return error.ExpectedLdIaArg1;
        },
    }
    unreachable;
}

inline fn peek(p: *const Parser) ?*Node {
    if (p.idx >= p.self.nodes.items.len) return null;
    return &p.self.nodes.items[p.idx];
}

inline fn take(p: *Parser) *Node {
    assert(p.idx < p.self.nodes.items.len);
    defer p.idx += 1;
    const n = &p.self.nodes.items[p.idx];
    // log.debug("take {t} {X:04}", .{ n.token.tag.type, n.token.tag.data });
    return n;
}

pub fn dump(p: *const Parser, gpa: Allocator) !void {
    var stack: std.ArrayList(usize) = .empty;
    var to_remove: std.ArrayList(usize) = .empty;
    defer stack.deinit(gpa);
    defer to_remove.deinit(gpa);
    for (p.self.nodes.items) |*n| {
        const t = &n.token;
        log.debug("{s}{t} {X:04} \"{s}\"", .{
            global.spaces(stack.items.len * 2),
            t.tag.type,
            t.tag.data,
            p.self.sliceFor(t),
        });
        to_remove.clearRetainingCapacity();
        for (stack.items, 0..) |*s, sn| {
            s.* -= 1;
            if (s.* == 0) try to_remove.append(gpa, sn);
        }
        stack.orderedRemoveMany(to_remove.items);

        if (n.child_len != 0) try stack.append(gpa, n.child_len);
    }
}
