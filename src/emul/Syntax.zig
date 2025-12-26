const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;

const zengine = @import("zengine");
const allocators = zengine.allocators;
const global = zengine.global;
const str = zengine.str;

const Prog = @import("Prog.zig");
const log = std.log.scoped(.syntax);

code: std.ArrayList(u8) = .empty,
nodes: std.ArrayList(Node) = .empty,

const Self = @This();

pub fn deinit(self: *@This(), gpa: Allocator) void {
    self.code.deinit(gpa);
    self.nodes.deinit(gpa);
}

pub fn reset(self: *@This()) void {
    self.code.clearRetainingCapacity();
    self.nodes.clearRetainingCapacity();
}

pub fn sliceFor(self: *const @This(), token: *const Token) []u8 {
    return self.code.items[token.offset..token.end];
}

pub fn read(self: *@This(), gpa: Allocator, r: *std.Io.Reader) !void {
    const buf = try allocators.scratch().alloc(u8, 1 << 10);
    defer allocators.scratch().free(buf);
    var iter = self.tokenIterator();
    log.info("reading", .{});
    loop: while (true) {
        const buf_len = try r.readSliceShort(buf);
        if (buf_len == 0) break;
        log.debug("read {Bi} from reader", .{buf_len});
        try self.code.appendSlice(gpa, buf[0..buf_len]);
        while (iter.next() catch |err| switch (err) {
            error.StreamTooLong => {
                log.debug("pulling from reader", .{});
                continue :loop;
            },
            else => return err,
        }) |token| {
            log.debug("token[{}]: {t} {X:04} \"{s}\"", .{
                self.nodes.items.len,
                token.tag.type,
                token.tag.data,
                self.sliceFor(&token),
            });
            try self.nodes.append(gpa, .{ .token = token });
        }
    }
    const last_token = try iter.rest();
    if (last_token) |token| {
        log.debug("token[{}]: {t} {X:04} \"{s}\" rest", .{
            self.nodes.items.len,
            token.tag.type,
            token.tag.data,
            self.sliceFor(&token),
        });
        try self.nodes.append(gpa, .{ .token = token });
    }
}

pub const Node = struct {
    token: Token,
    child_len: usize = 0,
};

pub const Token = struct {
    tag: Tag = .invalid,
    offset: usize = 0,
    end: usize = 0,
    line: u32 = 0,
    line_pos: u32 = 0,

    pub const Tag = struct {
        type: Type = .invalid,
        data: u16 = 0,

        pub const invalid: Tag = .{};

        pub const Type = enum(u16) {
            invalid,
            @"fn",
            @"var",

            expr_i,
            expr_d,
            expr_k,
            expr_s,
            expr_f,
            expr_c_hex,
            expr_c_dec,
            expr_r,
            expr_rr,
            expr_fb,
            expr_ia,
            expr_b,
            expr_st,
            expr_plane,
            expr_audio,
            expr_pitch,
            expr_ref,
            expr_label,

            inst_invalid,
            inst_cls,
            inst_ret,
            inst_sys,
            inst_jp,
            inst_call,
            inst_se,
            inst_sne,
            inst_ld,
            inst_add,
            inst_or,
            inst_and,
            inst_xor,
            inst_sub,
            inst_shr,
            inst_subn,
            inst_shl,
            inst_rnd,
            inst_drw,
            inst_skp,
            inst_sknp,
            inst_exit,
            inst_scroll_d,
            inst_scroll_u,
            inst_scroll_r,
            inst_scroll_l,
            inst_scr_lo,
            inst_scr_hi,

            pub const inst_min: Type = .inst_invalid;
            pub const inst_max: Type = .inst_scr_hi;
        };

        fn parse(token: []const u8) Tag {
            if (str.eql(token, "fn")) return .{ .type = .@"fn" };
            if (str.eql(token, "var")) return .{ .type = .@"var" };
            if (Inst.parse(token)) |inst| return switch (inst) {
                inline else => |i| .{ .type = @field(Type, "inst_" ++ @tagName(i)) },
            };
            if (Expr.parse(token)) |expr| return switch (expr.type) {
                inline else => |e| .{ .type = @field(Type, "expr_" ++ @tagName(e)), .data = expr.data },
            };
            return .invalid;
        }
    };
};

pub const Expr = struct {
    type: Type,
    data: u16 = 0,

    pub const Type = enum(u8) {
        i,
        d,
        k,
        s,
        f,
        c_hex,
        c_dec,
        r,
        rr,
        fb,
        ia,
        b,
        st,
        plane,
        audio,
        pitch,
        ref,
        label,
    };

    pub fn parse(token: []const u8) ?Expr {
        if (token.len == 0) return null;

        var hex: u16 = 0;
        var is_hex = token.len <= 5 and token[0] == '#';
        if (is_hex) is_hex &= blk: for (1..token.len) |n| {
            hex <<= 4;
            hex |= switch (token[n]) {
                '0'...'9' => token[n] - '0',
                'a'...'f' => token[n] - 'a' + 0xA,
                'A'...'F' => token[n] - 'A' + 0xA,
                else => break :blk false,
            };
        } else true;

        var dec: u16 = 0;
        const is_dec = blk: for (0..token.len) |n| {
            dec *= 10;
            dec += switch (token[n]) {
                '0'...'9' => token[n] - '0',
                else => break :blk false,
            };
        } else true;
        log.debug("is_dec: {} {}", .{ is_dec, dec });

        var reg: u4 = 0;
        const is_reg = token.len == 4 and blk: {
            var is_valid = true;
            if (token[0] != 'v') is_valid = false;
            if (token[1] != '[') is_valid = false;
            switch (token[2]) {
                '0'...'9' => reg = @intCast(token[2] - '0'),
                'a'...'f' => reg = @intCast(token[2] - 'a' + 0xA),
                'A'...'F' => reg = @intCast(token[2] - 'A' + 0xA),
                else => is_valid = false,
            }
            if (token[3] != ']') is_valid = false;
            break :blk is_valid;
        };

        var reg_reg: u8 = 0;
        const is_reg_reg = token.len == 8 and blk: {
            var is_valid = true;
            if (token[0] != 'v') is_valid = false;
            if (token[1] != '[') is_valid = false;
            switch (token[2]) {
                '0'...'9' => reg_reg = @intCast(token[2] - '0'),
                'a'...'f' => reg_reg = @intCast(token[2] - 'a' + 0xA),
                'A'...'F' => reg_reg = @intCast(token[2] - 'A' + 0xA),
                else => is_valid = false,
            }
            if (token[3] != '.') is_valid = false;
            if (token[4] != '.') is_valid = false;
            if (token[5] != '.') is_valid = false;
            reg_reg <<= 4;
            switch (token[6]) {
                '0'...'9' => reg_reg |= @intCast(token[6] - '0'),
                'a'...'f' => reg_reg |= @intCast(token[6] - 'a' + 0xA),
                'A'...'F' => reg_reg |= @intCast(token[6] - 'A' + 0xA),
                else => is_valid = false,
            }
            if (token[7] != ']') is_valid = false;
            break :blk is_valid;
        };

        if (is_hex) return .{ .type = .c_hex, .data = hex };
        if (is_dec) return .{ .type = .c_dec, .data = dec };
        if (is_reg) return .{ .type = .r, .data = reg };
        if (is_reg_reg) return .{ .type = .rr, .data = reg_reg };
        if (token[0] == '@') return .{ .type = .ref };
        if (str.eql(token, "i")) return .{ .type = .i };
        if (str.eql(token, "i[]")) return .{ .type = .ia };
        if (str.eql(token, "dt")) return .{ .type = .d };
        if (str.eql(token, "st")) return .{ .type = .s };
        if (str.eql(token, "$key")) return .{ .type = .k };
        if (str.eql(token, "$font")) return .{ .type = .f };
        if (str.eql(token, "$fontbig")) return .{ .type = .fb };
        if (str.eql(token, "$bcd")) return .{ .type = .b };
        if (str.eql(token, "$store")) return .{ .type = .st };
        if (str.eql(token, "$plane")) return .{ .type = .plane };
        if (str.eql(token, "$audio")) return .{ .type = .audio };
        if (str.eql(token, "$pitch")) return .{ .type = .pitch };
        return .{ .type = .label };
    }
};

pub const Inst = enum(u8) {
    invalid,
    cls,
    ret,
    sys,
    jp,
    call,
    se,
    sne,
    ld,
    add,
    @"or",
    @"and",
    xor,
    sub,
    shr,
    subn,
    shl,
    rnd,
    drw,
    skp,
    sknp,
    exit,
    scroll_d,
    scroll_u,
    scroll_r,
    scroll_l,
    scr_lo,
    scr_hi,

    pub fn parse(token: []const u8) ?Inst {
        const Indexer = std.enums.EnumIndexer(Inst);
        inline for (0..Indexer.count) |n| {
            const key = Indexer.keyForIndex(n);
            if (str.eql(token, @tagName(key))) return key;
        }
        return null;
    }
};

fn tokenIterator(self: *@This()) TokenIterator {
    return .{
        .self = self,
        .idx = self.code.items.len,
    };
}

const TokenIterator = struct {
    self: *Self,
    idx: usize = 0,
    ln: usize = 1,
    ln_pos: usize = 0,

    fn next(i: *@This()) !?Token {
        const code = i.self.code.items;
        if (i.idx >= code.len) return null;
        while (std.ascii.isWhitespace(code[i.idx])) {
            if (code[i.idx] == '\n') {
                i.ln += 1;
                i.ln_pos = 0;
            }
            i.idx += 1;
            if (i.idx >= code.len) return error.StreamTooLong;
        }
        const offset = i.idx;
        var end = offset;
        while (!std.ascii.isWhitespace(code[end])) {
            end += 1;
            if (end >= code.len) return error.StreamTooLong;
        }
        const token = code[offset..end];
        const tag = Token.Tag.parse(token);
        if (tag.type == .invalid) {
            log.err("invalid token \"{s}\" at [{}..{}]", .{ token, offset, end });
            return error.InvalidToken;
        }

        defer {
            i.idx = end;
            i.ln_pos += 1;
        }

        return .{
            .tag = tag,
            .offset = offset,
            .end = end,
            .line = @intCast(i.ln),
            .line_pos = @intCast(i.ln_pos),
        };
    }

    fn rest(i: *@This()) !?Token {
        const code = i.self.code.items;
        if (i.idx >= code.len) return null;
        while (std.ascii.isWhitespace(code[i.idx])) {
            if (code[i.idx] == '\n') {
                i.ln += 1;
                i.ln_pos = 0;
            }
            i.idx += 1;
            if (i.idx >= code.len) return null;
        }
        const offset = i.idx;
        var end = offset;
        while (!std.ascii.isWhitespace(code[end])) {
            end += 1;
            if (end >= code.len) break;
        }
        const token = code[offset..end];
        const tag = Token.Tag.parse(token);
        if (tag.type == .invalid) {
            log.err("invalid token \"{s}\" at [{}..{}]", .{ token, offset, end });
            return error.InvalidToken;
        }

        defer {
            i.idx = end;
            i.ln_pos += 1;
        }

        return .{
            .tag = tag,
            .offset = offset,
            .end = end,
            .line = @intCast(i.ln),
            .line_pos = @intCast(i.ln_pos),
        };
    }
};

pub fn parse(self: *const @This(), gpa: Allocator) !Prog {
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
        return err;
    };
    {
        var stack: std.ArrayList(usize) = .empty;
        var to_remove: std.ArrayList(usize) = .empty;
        defer stack.deinit(gpa);
        defer to_remove.deinit(gpa);
        for (self.nodes.items) |*n| {
            const t = &n.token;
            log.debug("{s}{t} {X:04} \"{s}\"", .{
                global.spaces(stack.items.len * 2),
                t.tag.type,
                t.tag.data,
                self.sliceFor(t),
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
    return result;
}

const Parser = struct {
    self: *const Self,
    stack: std.ArrayList(State),
    idx: usize,
    f_node: ?*Node = null,
    f: ?*Prog.Function = null,
    v_node: ?*Node = null,
    v: ?*Prog.Variable = null,

    const State = enum {
        invalid,
        begin,
        end,

        @"fn",
        in_fn,

        @"var",
        in_var,

        expected_label,
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

    fn init(self: *const Self, gpa: Allocator) !@This() {
        var result: @This() = .{
            .self = self,
            .stack = try .initCapacity(gpa, 16),
            .idx = 0,
        };
        result.stack.appendAssumeCapacity(.begin);
        return result;
    }

    fn deinit(p: *Parser, gpa: Allocator) void {
        p.stack.deinit(gpa);
    }

    fn run(p: *Parser, gpa: Allocator, result: *Prog) !void {
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
                    else => {
                        log.err(
                            "expected fn or var, found \"{s}\"",
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
                    .expr_label => {
                        const l = p.self.sliceFor(t);
                        p.f = result.function(gpa, l) catch |err| switch (err) {
                            error.AlreadyExists => {
                                log.err("duplicate function declaration \"{s}\" line {}:{}", .{
                                    l, t.line_pos, t.line,
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
                    .expr_label => try f.label(gpa, p.self.sliceFor(t)),
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
                        const xt = &xn.token;
                        switch (xt.tag.type) {
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
                                const at = &an.token;
                                switch (at.tag.type) {
                                    .expr_c_hex, .expr_c_dec => try f.inst(gpa, .encode(.jp_v0a, .{
                                        .nnn = @intCast(at.tag.data),
                                    }, .{})),
                                    else => continue :loop .expected_const,
                                }
                            },
                            else => continue :loop .expected_v0_address,
                        }
                    },
                    .inst_call => {
                        _ = p.take();
                        f_node.child_len += 1;
                        n.child_len = 1;
                        const an = p.peek() orelse continue :loop .expected_address;
                        const at = &an.token;
                        switch (at.tag.type) {
                            .expr_c_hex, .expr_c_dec => try f.inst(gpa, .encode(.call_a, .{
                                .nnn = @intCast(at.tag.data),
                            }, .{})),
                            .expr_ref => try f.inst(gpa, .encode(.call_a, .{}, .{
                                .nnn = p.self.sliceFor(at)[1..],
                            })),
                            else => continue :loop .expected_address,
                        }
                    },
                    .inst_se => {
                        _ = p.take();
                        f_node.child_len += 2;
                        n.child_len = 2;
                        const xn = p.peek() orelse continue :loop .expected_register;
                        const xt = &xn.token;
                        switch (xt.tag.type) {
                            .expr_r => _ = p.take(),
                            else => continue :loop .expected_register,
                        }
                        const yn = p.peek() orelse continue :loop .expected_byte;
                        const yt = &yn.token;
                        switch (yt.tag.type) {
                            .expr_r => try f.inst(gpa, .encode(.se_rr, .{
                                .x = @intCast(xt.tag.data),
                                .y = @intCast(yt.tag.data),
                            }, .{})),
                            .expr_c_hex, .expr_c_dec => try f.inst(gpa, .encode(.se_rc, .{
                                .x = @intCast(xt.tag.data),
                                .kk = @intCast(yt.tag.data),
                            }, .{})),
                            else => continue :loop .expected_byte,
                        }
                    },
                    .inst_sne => {
                        _ = p.take();
                        f_node.child_len += 2;
                        n.child_len = 2;
                        const xn = p.peek() orelse continue :loop .expected_register;
                        const xt = &xn.token;
                        switch (xt.tag.type) {
                            .expr_r => _ = p.take(),
                            else => continue :loop .expected_register,
                        }
                        const yn = p.peek() orelse continue :loop .expected_byte;
                        const yt = &yn.token;
                        switch (yt.tag.type) {
                            .expr_r => try f.inst(gpa, .encode(.sne_rr, .{
                                .x = @intCast(xt.tag.data),
                                .y = @intCast(yt.tag.data),
                            }, .{})),
                            .expr_c_hex, .expr_c_dec => try f.inst(gpa, .encode(.sne_rc, .{
                                .x = @intCast(xt.tag.data),
                                .kk = @intCast(yt.tag.data),
                            }, .{})),
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
                        switch (at[0].tag.type) {
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
                                switch (at[1].tag.type) {
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
                                switch (at[1].tag.type) {
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
                                switch (at[1].tag.type) {
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
                                        switch (at[2].tag.type) {
                                            .expr_r => try f.inst(gpa, .encode(.ld_fr, .{
                                                .x = @intCast(at[2].tag.data),
                                            }, .{})),
                                            else => continue :loop .expected_register,
                                        }
                                    },
                                    .expr_fb => {
                                        _ = p.take();
                                        f_node.child_len += 1;
                                        n.child_len += 1;
                                        an[2] = p.peek() orelse continue :loop .expected_register;
                                        at[2] = &an[2].token;
                                        switch (at[2].tag.type) {
                                            .expr_r => try f.inst(gpa, .encode(.ld_fbr, .{
                                                .x = @intCast(at[2].tag.data),
                                            }, .{})),
                                            else => continue :loop .expected_register,
                                        }
                                    },
                                    .expr_ref => try f.inst(gpa, .encode(.ld_ia, .{}, .{
                                        .nnn = p.self.sliceFor(at[1])[1..],
                                    })),
                                    else => continue :loop .expected_ld_i_arg1,
                                }
                            },
                            // ld_dr    dt       v[x]
                            .expr_d => {
                                _ = p.take();
                                an[1] = p.peek() orelse continue :loop .expected_register;
                                at[1] = &an[1].token;
                                switch (at[1].tag.type) {
                                    .expr_r => try f.inst(gpa, .encode(.ld_dr, .{
                                        .x = @intCast(at[1].tag.data),
                                    }, .{})),
                                    else => continue :loop .expected_register,
                                }
                            },
                            // ld_sr    st       v[x]
                            .expr_s => {
                                _ = p.take();
                                an[1] = p.peek() orelse continue :loop .expected_register;
                                at[1] = &an[1].token;
                                switch (at[1].tag.type) {
                                    .expr_r => try f.inst(gpa, .encode(.ld_dr, .{
                                        .x = @intCast(at[1].tag.data),
                                    }, .{})),
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
                                switch (at[1].tag.type) {
                                    .expr_r => if (at[1].tag.data == 0)
                                        try f.inst(gpa, .encode(.ld_iar, .{
                                            .x = 0,
                                        }, .{}))
                                    else
                                        try f.inst(gpa, .encode(.ld_iarr, .{
                                            .x = @intCast(at[1].tag.data),
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
                                        switch (at[2].tag.type) {
                                            .expr_r => try f.inst(gpa, .encode(.ld_br, .{
                                                .x = @intCast(at[2].tag.data),
                                            }, .{})),
                                            else => continue :loop .expected_register,
                                        }
                                    },
                                    else => continue :loop .expected_ld_ia_arg1,
                                }
                            },
                            // ld_plane $plane   #x
                            .expr_plane => {
                                _ = p.take();
                                an[1] = p.peek() orelse continue :loop .expected_const;
                                at[1] = &an[1].token;
                                switch (at[1].tag.type) {
                                    .expr_c_hex, .expr_c_dec => try f.inst(gpa, .encode(.ld_plane, .{
                                        .x = @intCast(at[1].tag.data),
                                    }, .{})),
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
                                switch (at[1].tag.type) {
                                    .expr_rr => {
                                        const y0: u4 = @intCast(at[1].tag.data & 0xF);
                                        const y1: u4 = @intCast((at[1].tag.data >> 4) & 0xF);
                                        assert(y0 == 0);
                                        try f.inst(gpa, .encode(.ld_str, .{
                                            .x = y1,
                                        }, .{}));
                                    },
                                    else => continue :loop .expected_register_range,
                                }
                            },
                            else => continue :loop .expected_ld_arg0,
                        }
                    },
                    .inst_add => {
                        _ = p.take();
                        f_node.child_len += 2;
                        n.child_len = 2;
                        const xn = p.peek() orelse continue :loop .expected_i_register;
                        const xt = &xn.token;
                        switch (xt.tag.type) {
                            .expr_i => {
                                _ = p.take();
                                const yn = p.peek() orelse continue :loop .expected_register;
                                const yt = &yn.token;
                                switch (yt.tag.type) {
                                    .expr_r => try f.inst(gpa, .encode(.add_ir, .{
                                        .x = @intCast(yt.tag.data),
                                    }, .{})),
                                    else => continue :loop .expected_register,
                                }
                            },
                            .expr_r => {
                                _ = p.take();
                                const yn = p.peek() orelse continue :loop .expected_byte;
                                const yt = &yn.token;
                                switch (yt.tag.type) {
                                    .expr_c_hex, .expr_c_dec => try f.inst(gpa, .encode(.add_rc, .{
                                        .x = @intCast(xt.tag.data),
                                        .kk = @intCast(yt.tag.data),
                                    }, .{})),
                                    .expr_r => try f.inst(gpa, .encode(.add_rr, .{
                                        .x = @intCast(xt.tag.data),
                                        .y = @intCast(yt.tag.data),
                                    }, .{})),
                                    else => continue :loop .expected_byte,
                                }
                            },
                            else => continue :loop .expected_i_register,
                        }
                    },
                    .inst_or => {
                        _ = p.take();
                        f_node.child_len += 2;
                        n.child_len = 2;
                        const xn = p.peek() orelse continue :loop .expected_register;
                        const xt = &xn.token;
                        switch (xt.tag.type) {
                            .expr_r => _ = p.take(),
                            else => continue :loop .expected_register,
                        }
                        const yn = p.peek() orelse continue :loop .expected_register;
                        const yt = &yn.token;
                        switch (yt.tag.type) {
                            .expr_r => try f.inst(gpa, .encode(.or_rr, .{
                                .x = @intCast(xt.tag.data),
                                .y = @intCast(yt.tag.data),
                            }, .{})),
                            else => continue :loop .expected_register,
                        }
                    },
                    .inst_and => {
                        _ = p.take();
                        f_node.child_len += 2;
                        n.child_len = 2;
                        const xn = p.peek() orelse continue :loop .expected_register;
                        const xt = &xn.token;
                        switch (xt.tag.type) {
                            .expr_r => _ = p.take(),
                            else => continue :loop .expected_register,
                        }
                        const yn = p.peek() orelse continue :loop .expected_register;
                        const yt = &yn.token;
                        switch (yt.tag.type) {
                            .expr_r => try f.inst(gpa, .encode(.and_rr, .{
                                .x = @intCast(xt.tag.data),
                                .y = @intCast(yt.tag.data),
                            }, .{})),
                            else => continue :loop .expected_register,
                        }
                    },
                    .inst_xor => {
                        _ = p.take();
                        f_node.child_len += 2;
                        n.child_len = 2;
                        const xn = p.peek() orelse continue :loop .expected_register;
                        const xt = &xn.token;
                        switch (xt.tag.type) {
                            .expr_r => _ = p.take(),
                            else => continue :loop .expected_register,
                        }
                        const yn = p.peek() orelse continue :loop .expected_register;
                        const yt = &yn.token;
                        switch (yt.tag.type) {
                            .expr_r => try f.inst(gpa, .encode(.xor_rr, .{
                                .x = @intCast(xt.tag.data),
                                .y = @intCast(yt.tag.data),
                            }, .{})),
                            else => continue :loop .expected_register,
                        }
                    },
                    .inst_sub => {
                        _ = p.take();
                        f_node.child_len += 2;
                        n.child_len = 2;
                        const xn = p.peek() orelse continue :loop .expected_register;
                        const xt = &xn.token;
                        switch (xt.tag.type) {
                            .expr_r => _ = p.take(),
                            else => continue :loop .expected_register,
                        }
                        const yn = p.peek() orelse continue :loop .expected_register;
                        const yt = &yn.token;
                        switch (yt.tag.type) {
                            .expr_r => try f.inst(gpa, .encode(.sub_rr, .{
                                .x = @intCast(xt.tag.data),
                                .y = @intCast(yt.tag.data),
                            }, .{})),
                            else => continue :loop .expected_register,
                        }
                    },
                    .inst_shr => {
                        _ = p.take();
                        f_node.child_len += 2;
                        n.child_len = 2;
                        const xn = p.peek() orelse continue :loop .expected_register;
                        const xt = &xn.token;
                        switch (xt.tag.type) {
                            .expr_r => _ = p.take(),
                            else => continue :loop .expected_register,
                        }
                        const yn = p.peek() orelse continue :loop .expected_register;
                        const yt = &yn.token;
                        switch (yt.tag.type) {
                            .expr_r => try f.inst(gpa, .encode(.shr_rr, .{
                                .x = @intCast(xt.tag.data),
                                .y = @intCast(yt.tag.data),
                            }, .{})),
                            else => continue :loop .expected_register,
                        }
                    },
                    .inst_subn => {
                        _ = p.take();
                        f_node.child_len += 2;
                        n.child_len = 2;
                        const xn = p.peek() orelse continue :loop .expected_register;
                        const xt = &xn.token;
                        switch (xt.tag.type) {
                            .expr_r => _ = p.take(),
                            else => continue :loop .expected_register,
                        }
                        const yn = p.peek() orelse continue :loop .expected_register;
                        const yt = &yn.token;
                        switch (yt.tag.type) {
                            .expr_r => try f.inst(gpa, .encode(.subn_rr, .{
                                .x = @intCast(xt.tag.data),
                                .y = @intCast(yt.tag.data),
                            }, .{})),
                            else => continue :loop .expected_register,
                        }
                    },
                    .inst_shl => {
                        _ = p.take();
                        f_node.child_len += 2;
                        n.child_len = 2;
                        const xn = p.peek() orelse continue :loop .expected_register;
                        const xt = &xn.token;
                        switch (xt.tag.type) {
                            .expr_r => _ = p.take(),
                            else => continue :loop .expected_register,
                        }
                        const yn = p.peek() orelse continue :loop .expected_register;
                        const yt = &yn.token;
                        switch (yt.tag.type) {
                            .expr_r => try f.inst(gpa, .encode(.shl_rr, .{
                                .x = @intCast(xt.tag.data),
                                .y = @intCast(yt.tag.data),
                            }, .{})),
                            else => continue :loop .expected_register,
                        }
                    },
                    .inst_rnd => {
                        _ = p.take();
                        f_node.child_len += 2;
                        n.child_len = 2;
                        const xn = p.peek() orelse continue :loop .expected_register;
                        const xt = &xn.token;
                        switch (xt.tag.type) {
                            .expr_r => _ = p.take(),
                            else => continue :loop .expected_register,
                        }
                        const an = p.peek() orelse continue :loop .expected_const;
                        const at = &an.token;
                        switch (at.tag.type) {
                            .expr_c_hex, .expr_c_dec => try f.inst(gpa, .encode(.rnd_rc, .{
                                .x = @intCast(xt.tag.data),
                                .kk = @intCast(at.tag.data),
                            }, .{})),
                            else => continue :loop .expected_const,
                        }
                    },
                    .inst_drw => {
                        _ = p.take();
                        f_node.child_len += 3;
                        n.child_len = 3;
                        const xn = p.peek() orelse continue :loop .expected_register;
                        const xt = &xn.token;
                        switch (xt.tag.type) {
                            .expr_r => _ = p.take(),
                            else => continue :loop .expected_register,
                        }
                        const yn = p.peek() orelse continue :loop .expected_register;
                        const yt = &yn.token;
                        switch (yt.tag.type) {
                            .expr_r => _ = p.take(),
                            else => continue :loop .expected_register,
                        }
                        const an = p.peek() orelse continue :loop .expected_const;
                        const at = &an.token;
                        switch (at.tag.type) {
                            .expr_c_hex, .expr_c_dec => try f.inst(gpa, .encode(.drw_rrc, .{
                                .x = @intCast(xt.tag.data),
                                .y = @intCast(yt.tag.data),
                                .n = @intCast(at.tag.data),
                            }, .{})),
                            else => continue :loop .expected_const,
                        }
                    },
                    .inst_skp => {
                        _ = p.take();
                        f_node.child_len += 1;
                        n.child_len = 1;
                        const xn = p.peek() orelse continue :loop .expected_register;
                        const xt = &xn.token;
                        switch (xt.tag.type) {
                            .expr_r => try f.inst(gpa, .encode(.skp_r, .{
                                .x = @intCast(xt.tag.data),
                            }, .{})),
                            else => continue :loop .expected_register,
                        }
                    },
                    .inst_sknp => {
                        _ = p.take();
                        f_node.child_len += 1;
                        n.child_len = 1;
                        const xn = p.peek() orelse continue :loop .expected_register;
                        const xt = &xn.token;
                        switch (xt.tag.type) {
                            .expr_r => try f.inst(gpa, .encode(.sknp_r, .{
                                .x = @intCast(xt.tag.data),
                            }, .{})),
                            else => continue :loop .expected_register,
                        }
                    },
                    .inst_exit => try f.inst(gpa, .encode(.exit, .{}, .{})),
                    .inst_scroll_d => {
                        _ = p.take();
                        f_node.child_len += 1;
                        n.child_len = 1;
                        const an = p.peek() orelse continue :loop .expected_register;
                        const at = &an.token;
                        switch (at.tag.type) {
                            .expr_c_hex, .expr_c_dec => try f.inst(gpa, .encode(.scroll_dn, .{
                                // .n = @intCast(at.tag.data),
                            }, .{})),
                            else => continue :loop .expected_register,
                        }
                    },
                    .inst_scroll_u => {
                        _ = p.take();
                        f_node.child_len += 1;
                        n.child_len = 1;
                        const an = p.peek() orelse continue :loop .expected_register;
                        const at = &an.token;
                        switch (at.tag.type) {
                            .expr_c_hex, .expr_c_dec => try f.inst(gpa, .encode(.scroll_un, .{
                                // .n = @intCast(at.tag.data),
                            }, .{})),
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
                    .expr_label => {
                        const l = p.self.sliceFor(t);
                        p.v = result.variable(gpa, l) catch |err| switch (err) {
                            error.AlreadyExists => {
                                log.err("duplicate function declaration \"{s}\" line {}:{}", .{
                                    l, t.line_pos, t.line,
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
                const t = &n.token;
                switch (t.tag.type) {
                    .expr_c_hex, .expr_c_dec => {
                        _ = p.take();
                        v_node.child_len += 1;
                        if (p.f_node) |f_node| f_node.child_len += 1;
                        try v.val.append(gpa, @intCast(t.tag.data));
                        continue :loop .in_var;
                    },
                    else => {
                        while (p.stack.pop() != .in_var) {}
                        p.v_node = null;
                        p.v = null;
                        continue :loop p.stack.getLast();
                    },
                }
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
                    log.err("expected const or register found end of stream", .{});
                    return error.ExpectedByte;
                };
                const t = &n.token;
                log.err("expected const or register found \"{s}\" ({t}) line {}:{}", .{
                    p.self.sliceFor(t),
                    t.tag.type,
                    t.line,
                    t.line_pos,
                });
                return error.ExpectedByte;
            },
            .expected_address => {
                const n = p.peek() orelse {
                    log.err("expected const or ref found end of stream", .{});
                    return error.ExpectedAddress;
                };
                const t = &n.token;
                log.err("expected const or ref found \"{s}\" ({t}) line {}:{}", .{
                    p.self.sliceFor(t),
                    t.tag.type,
                    t.line,
                    t.line_pos,
                });
                return error.ExpectedAddress;
            },
            .expected_v0_address => {
                const n = p.peek() orelse {
                    log.err("expected v[0], const or ref found end of stream", .{});
                    return error.ExpectedV0Address;
                };
                const t = &n.token;
                log.err("expected v[0], const or ref found \"{s}\" ({t}) line {}:{}", .{
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
                    log.err("expected register, const, dt, i[] or $key found end of stream", .{});
                    return error.ExpectedLdRArg1;
                };
                const t = &n.token;
                log.err("expected register, const, dt, i[] or $key found \"{s}\" ({t})  line {}:{}", .{
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
                    log.err("expected const, ref, $font or $fontbig found end of stream", .{});
                    return error.ExpectedLdIArg1;
                };
                const t = &n.token;
                log.err("expected const, ref, $font or $fontbig found \"{s}\" ({t}) line {}:{}", .{
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

    inline fn peek(p: *const @This()) ?*Node {
        if (p.idx >= p.self.nodes.items.len) return null;
        return &p.self.nodes.items[p.idx];
    }

    inline fn take(p: *@This()) *Node {
        assert(p.idx < p.self.nodes.items.len);
        defer p.idx += 1;
        const n = &p.self.nodes.items[p.idx];
        log.debug("take {t} {X:04}", .{ n.token.tag.type, n.token.tag.data });
        return n;
    }
};
