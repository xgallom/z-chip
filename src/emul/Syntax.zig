const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;

const zengine = @import("zengine");
const allocators = zengine.allocators;
const global = zengine.global;
const str = zengine.str;
const ArrayMap = zengine.containers.ArrayMap;

const Prog = @import("Prog.zig");
const Parser = @import("Parser.zig");

const log = std.log.scoped(.syntax);

code: std.ArrayList(u8) = .empty,
nodes: std.ArrayList(Node) = .empty,

const Self = @This();

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
            comment,
            label,
            @"fn",
            @"var",
            def,
            undef,

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
            expr_def,
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
        };

        comptime {
            {
                // ensure token and expression enums match
                const TokenIndexer = std.enums.EnumIndexer(Type);
                const Indexer = std.enums.EnumIndexer(Expr.Type);
                assert(Indexer.indexOf(.i) == 0);
                const t0 = TokenIndexer.indexOf(.expr_i);
                for (0..Indexer.count) |i| {
                    const t = t0 + i;
                    const i_key = Indexer.keyForIndex(i);
                    const t_key = TokenIndexer.keyForIndex(t);
                    if (!str.eql(@tagName(t_key), "expr_" ++ @tagName(i_key))) {
                        @compileError("Enums " ++ @tagName(t_key) ++ " and " ++ @tagName(i_key) ++ "don't match");
                    }
                }
            }
            {
                // ensure token and instruction enums match
                const TokenIndexer = std.enums.EnumIndexer(Type);
                const Indexer = std.enums.EnumIndexer(Inst);
                assert(Indexer.indexOf(.invalid) == 0);
                const t0 = TokenIndexer.indexOf(.inst_invalid);
                for (0..Indexer.count) |i| {
                    const t = t0 + i;
                    const i_key = Indexer.keyForIndex(i);
                    const t_key = TokenIndexer.keyForIndex(t);
                    if (!str.eql(@tagName(t_key), "inst_" ++ @tagName(i_key))) {
                        @compileError("Enums " ++ @tagName(t_key) ++ " and " ++ @tagName(i_key) ++ "don't match");
                    }
                }
            }
        }

        fn parse(token: []const u8) Tag {
            if (str.eql(token, "#")) return .{ .type = .comment };
            if (str.eql(token, "fn")) return .{ .type = .@"fn" };
            if (str.eql(token, "var")) return .{ .type = .@"var" };
            if (str.eql(token, "def")) return .{ .type = .def };
            if (str.eql(token, "undef")) return .{ .type = .undef };
            if (Inst.parse(token)) |inst| return .{
                .type = @enumFromInt(@intFromEnum(inst) + @intFromEnum(Type.inst_invalid)),
            };
            if (Expr.parse(token)) |expr| return .{
                .type = @enumFromInt(@intFromEnum(expr.type) + @intFromEnum(Tag.Type.expr_i)),
                .data = expr.data,
            };
            return .{ .type = .label };
        }
    };
};

pub const Expr = struct {
    type: Type,
    data: u16 = 0,

    pub const Type = enum(u16) {
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
        def,
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
        if (token[0] == '$') return .{ .type = .def };
        if (token[0] == ':') return .{ .type = .label };
        return null;
    }
};

pub const Inst = enum(u16) {
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

pub fn deinit(self: *Self, gpa: Allocator) void {
    self.code.deinit(gpa);
    self.nodes.deinit(gpa);
}

pub fn reset(self: *Self) void {
    self.code.clearRetainingCapacity();
    self.nodes.clearRetainingCapacity();
}

pub fn sliceFor(self: *const Self, token: *const Token) []u8 {
    return self.code.items[token.offset..token.end];
}

pub fn read(self: *Self, gpa: Allocator, r: *std.Io.Reader) !void {
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

pub fn parse(self: *const Self, gpa: Allocator) !Prog {
    return Parser.parse(self, gpa);
}

fn tokenIterator(self: *Self) TokenIterator {
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
    state: State = .default,

    const State = enum {
        begin,
        in_comment,
        const default: State = .begin;
    };

    fn next(i: *@This()) !?Token {
        state: switch (i.state) {
            .begin => {
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
                if (tag.type == .comment) {
                    i.state = .in_comment;
                    i.idx = end;
                    log.debug("start comment", .{});
                    continue :state .in_comment;
                }
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
            },
            .in_comment => {
                const code = i.self.code.items;
                if (i.idx >= code.len) return null;
                while (code[i.idx] != '\n') {
                    i.idx += 1;
                    if (i.idx >= code.len) return error.StreamTooLong;
                }
                assert(code[i.idx] == '\n');
                i.state = .begin;
                i.idx += 1;
                i.ln += 1;
                i.ln_pos = 0;
                log.debug("end comment", .{});
                continue :state .begin;
            },
        }
    }

    fn rest(i: *@This()) !?Token {
        state: switch (i.state) {
            .begin => {
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
                if (tag.type == .comment) {
                    i.state = .in_comment;
                    i.idx = end;
                    continue :state .in_comment;
                }
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
            },
            .in_comment => {
                const code = i.self.code.items;
                if (i.idx >= code.len) return null;
                while (code[i.idx] != '\n') {
                    i.idx += 1;
                    if (i.idx >= code.len) return null;
                }
                assert(code[i.idx] == '\n');
                i.idx += 1;
                i.ln += 1;
                i.ln_pos = 0;

                continue :state .begin;
            },
        }
    }
};
