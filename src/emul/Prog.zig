const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;

const zengine = @import("zengine");
const str = zengine.str;
const ArrayMap = zengine.containers.ArrayMap;

const Inst = @import("Inst.zig");
const Mem = @import("Mem.zig");

const log = std.log.scoped(.prog);

fs: ArrayMap(Function),
vs: ArrayMap(Variable),

const Self = @This();
pub const Error = error{
    RefNotFound,
};

pub const Variable = struct {
    val: std.ArrayList(u8) = .empty,

    pub fn deinit(self: *@This(), gpa: Allocator) void {
        self.val.deinit(gpa);
    }

    pub fn write(self: *@This(), w: *std.Io.Writer) !u16 {
        return @intCast(try w.write(self.val.items));
    }
};

pub const Function = struct {
    prog: std.ArrayList(EncodedInst) = .empty,
    labels: std.ArrayList(Label) = .empty,

    pub fn deinit(self: *@This(), gpa: Allocator) void {
        self.prog.deinit(gpa);
        self.labels.deinit(gpa);
    }

    pub fn inst(self: *@This(), gpa: Allocator, ei: EncodedInst) !void {
        try self.prog.append(gpa, ei);
    }

    pub fn label(self: *@This(), gpa: Allocator, key: []const u8) !void {
        try self.labels.append(gpa, .{
            .key = key,
            .offset = self.prog.items.len,
        });
    }

    pub fn write(self: *@This(), w: *std.Io.Writer) !u16 {
        var offset: u16 = 0;
        for (self.prog.items) |ei| offset += try ei.write(w);
        return offset;
    }
};

pub const Label = struct {
    key: []const u8,
    offset: usize,
};

pub const EncodedInst = struct {
    inst: Inst,
    nnnn: Inst.Args = .{},
    resolve: Resolve = .{},

    pub const Resolve = struct {
        nnnn: ?[]const u8 = null,
        nnn: ?[]const u8 = null,
        offset: i16 = 0,
    };

    pub fn encode(
        comptime op: Inst.OpCode,
        args: Inst.OpCode.ArgsType(op).ArgsType,
        resolve: Resolve,
    ) @This() {
        return Inst.encode(op, args, resolve);
    }

    pub fn init(inst: Inst) @This() {
        return .{ .inst = inst };
    }

    pub fn write(ei: *const @This(), w: *std.Io.Writer) !u16 {
        var buf: [2]u8 = undefined;
        log.debug("ei: {t} ({X:04})", .{ ei.inst.op, ei.inst.args.data });
        buf[0] = @truncate(ei.inst.args.data >> 8);
        buf[1] = @truncate(ei.inst.args.data);
        var offset: usize = 0;
        offset += try w.write(&buf);
        if (ei.inst.op == .ld_nnnn) {
            buf[0] = @truncate(ei.nnnn.data >> 8);
            buf[1] = @truncate(ei.nnnn.data);
            offset += try w.write(&buf);
        }
        return @intCast(offset);
    }
};

pub fn init(gpa: Allocator, preheat: usize) !Self {
    return .{
        .fs = try .init(gpa, preheat),
        .vs = try .init(gpa, preheat),
    };
}

pub fn deinit(self: *Self, gpa: Allocator) void {
    for (self.fs.values()) |*f| f.deinit(gpa);
    self.fs.deinit(gpa);
    for (self.vs.values()) |*v| v.deinit(gpa);
    self.vs.deinit(gpa);
}

pub fn function(self: *Self, gpa: Allocator, key: []const u8) !*Function {
    return self.fs.tryInsert(gpa, key, .{});
}

pub fn variable(self: *Self, gpa: Allocator, key: []const u8) !*Variable {
    return self.vs.tryInsert(gpa, key, .{});
}

pub fn compile(self: *Self, gpa: Allocator) !Resolved {
    var result: Resolved = .{ .self = self };
    var wa: std.Io.Writer.Allocating = .init(gpa);
    errdefer wa.deinit();
    const w = &wa.writer;
    var offset: u16 = Mem.prog_begin;

    log.info("compiling", .{});
    {
        var iter = self.fs.map.iterator();
        while (iter.next()) |e| {
            log.debug("compiling function {s}", .{e.key_ptr.*});
            try result.refs.insert(gpa, e.key_ptr.*, .{
                .key = e.key_ptr.*,
                .offset = offset,
                .type = .function,
            });

            var last_label: usize = 0;
            for (e.value_ptr.*.prog.items, 0..) |ei, n| {
                log.debug("compiling instruction {t}", .{ei.inst.op});
                if (ei.resolve.nnnn) |res| {
                    log.debug("resolve nnnn {s}", .{res});
                    const args_type: Inst.Args.Type = switch (ei.inst.op) {
                        inline else => |op| op.ArgsType().args_type,
                    };
                    assert(args_type == .nnnn);
                    try result.resolves.append(gpa, .{
                        .key = res,
                        .offset = offset,
                        .var_offset = ei.resolve.offset,
                        .type = .nnnn,
                    });
                }
                if (ei.resolve.nnn) |res| {
                    log.debug("resolve nnn {s}", .{res});
                    const args_type: Inst.Args.Type = switch (ei.inst.op) {
                        inline else => |op| op.ArgsType().args_type,
                    };
                    assert(args_type == .nnn);
                    try result.resolves.append(gpa, .{
                        .key = res,
                        .offset = offset,
                        .var_offset = ei.resolve.offset,
                        .type = .nnn,
                    });
                }
                for (last_label..e.value_ptr.*.labels.items.len) |ln| {
                    const l = e.value_ptr.*.labels.items[ln];
                    if (l.offset == n) {
                        last_label = ln + 1;
                        log.debug("compiling label {s}", .{l.key});
                        try result.refs.insert(gpa, l.key, .{
                            .key = l.key,
                            .offset = offset,
                            .type = .label,
                        });
                    } else if (l.offset > n) break;
                }
                offset += try ei.write(w);
            }
        }
    }

    {
        var iter = self.vs.map.iterator();
        while (iter.next()) |e| {
            log.debug("compiling variable {s}", .{e.key_ptr.*});
            try result.refs.insert(gpa, e.key_ptr.*, .{
                .key = e.key_ptr.*,
                .offset = offset,
                .type = .variable,
            });
            offset += try e.value_ptr.write(w);
        }
    }

    try w.flush();
    result.code = wa.toArrayList();
    return result;
}

pub const Resolved = struct {
    self: *Self,
    code: std.ArrayList(u8) = .empty,
    refs: ArrayMap(@This().Ref) = .empty,
    resolves: std.ArrayList(Resolve) = .empty,

    pub const Variable = struct {
        key: []const u8,
        offset: u16,
    };

    pub const Function = struct {
        key: []const u8,
        offset: u16,
        len: u16 = 0,
    };

    pub const Ref = struct {
        key: []const u8,
        offset: u16,
        type: Type,

        pub const Type = enum {
            variable,
            function,
            label,
        };
    };

    pub const Resolve = struct {
        key: []const u8,
        offset: u16,
        var_offset: i16 = 0,
        type: Type,

        pub const Type = enum {
            nnnn,
            nnn,
        };
    };

    pub fn deinit(self: *Resolved, gpa: Allocator) void {
        self.code.deinit(gpa);
        self.refs.deinit(gpa);
        self.resolves.deinit(gpa);
    }

    pub fn offsetFor(self: *const Resolved, key: []const u8) ?Ref {
        const ref_opt = self.refs.getPtrOrNull(key);
        if (ref_opt) |ref| return ref.*;
        return null;
    }

    pub fn addressFor(self: *const Resolved, addr: u16) *u8 {
        assert(addr >= Mem.prog_begin);
        return &self.code.items[addr - Mem.prog_begin];
    }

    pub fn resolve(self: *Resolved) !void {
        log.info("resolving", .{});
        for (self.resolves.items) |res| {
            log.debug("resolve {t} {s}", .{ res.type, res.key });
            switch (res.type) {
                .nnnn => {
                    var target = self.offsetFor(res.key) orelse return Error.RefNotFound;
                    target.offset = @intCast(@as(i16, @intCast(target.offset)) + res.var_offset);
                    log.debug("resolve nnnn: @{X:04} #{X:04} /{X:04}", .{
                        res.offset,
                        target.offset,
                        Mem.prog_begin + self.code.items.len,
                    });
                    self.addressFor(res.offset + Inst.byte_size).* = @truncate(target.offset >> 8);
                    self.addressFor(res.offset + Inst.byte_size + 1).* = @truncate(target.offset);
                },
                .nnn => {
                    var target = self.offsetFor(res.key) orelse return Error.RefNotFound;
                    target.offset = @intCast(@as(i16, @intCast(target.offset)) + res.var_offset);
                    log.debug("resolve nnn: @{X:03} #{X:03} /{X:03}", .{
                        res.offset,
                        target.offset,
                        Mem.prog_begin + self.code.items.len,
                    });
                    self.addressFor(res.offset).* |= @as(u4, @truncate(target.offset >> 8));
                    self.addressFor(res.offset + 1).* = @truncate(target.offset);
                },
            }
        }
    }
};
