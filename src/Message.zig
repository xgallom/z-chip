const std = @import("std");
const zengine = @import("zengine");
const allocators = zengine.allocators;
const anim = zengine.anim;
const c = zengine.ext.c;
const global = zengine.global;
const gfx = zengine.gfx;
const math = zengine.math;
const time = zengine.time;

text: []const u8,
start: u64,
len: u64,
surf: gfx.Surface = .invalid,
render_surf: gfx.Surface = .invalid,

const Self = @This();

pub var messages: std.ArrayList(Self) = .empty;
var messages_len: anim.smv.ExpDecay(f32, .{}) = .init(1, .{ .smooth_time = 0.2 });

pub fn deinit() void {
    messages.deinit(allocators.gpa());
}

pub fn add(text: []const u8) !void {
    // if (messages.items.len >= 10) _ = messages.orderedRemove(0);
    const self = try messages.addOne(allocators.gpa());
    self.* = .{
        .text = text,
        .start = global.engineNowNano(),
        .len = std.time.ns_per_s * 2,
    };
    messages_len.setTarget(@max(1, @as(f32, @floatFromInt(messages.items.len))));
}

pub fn remove(idx: usize) void {
    var self = messages.orderedRemove(idx);
    self.surf.deinit();
    messages_len.setTarget(@max(1, @as(f32, @floatFromInt(messages.items.len))));
}

pub fn render(surf: gfx.Surface, font: gfx.ttf.Font) !void {
    const op = math.param.op;
    var to_remove: std.ArrayList(usize) = try .initCapacity(allocators.frame(), messages.items.len);
    const now = global.engineNowNano();
    var off: math.Point_i32 = .{ 64, 32 };
    @memset(surf.slice(u32), 0);
    messages_len.update(global.timeSinceLastFrame().toFloat().toValue32(.s));
    for (0..messages.items.len) |rn| {
        const n = messages.items.len - 1 - rn;
        const message = &messages.items[n];
        if (!message.surf.isValid()) message.surf = try font.renderText(
            message.text,
            .{ 255, 255, 255, 255 },
        );
        if (!message.render_surf.isValid()) message.render_surf = try .init(.{
            message.surf.width(),
            message.surf.height(),
        }, .default);

        var t = @as(f32, @floatFromInt(now - message.start)) / @as(f32, @floatFromInt(message.len));
        t *= op.fpow(0.5)(messages_len.current);
        var h: i32 = 0;
        var o: f32 = 1;

        if (t >= 1.5) {
            to_remove.appendAssumeCapacity(n);
            continue;
        }
        if (t >= 1) {
            const height = op.ease(.step, op.pow(3));
            h = @intFromFloat(math.param.lerp(
                0,
                @floatFromInt(message.surf.height()),
                height((t - 1) * 2),
            ));
            h = @divFloor(h, 2);
        }
        if (t >= 0.5) {
            const opacity = op.ease(.stop, op.chain(op.pow(4), op.reverse));
            o = opacity(t - 0.5);
        } else if (t <= 0.1) {
            const height = op.ease(.start, op.chain(op.pow(3), op.reverse));
            const opacity = op.ease(.start, op.pow(4));
            o = opacity(t / 0.1);
            h = @intFromFloat(math.param.lerp(
                0,
                @floatFromInt(message.surf.height()),
                height(t / 0.1),
            ));
        }

        for (0..message.surf.height()) |y| {
            for (0..message.surf.width()) |x| {
                const pos: math.Point_i32 = .{ @intCast(x), @intCast(y) };
                var px: math.RGBAf32 = undefined;
                try message.surf.readFloat(pos, &px);
                px[3] *= o;
                try message.render_surf.writeFloat(pos, &px);
            }
        }

        const src: math.Rect_i32 = .{ 0, 0, @intCast(message.surf.width()), @intCast(message.surf.height()) };
        const dst: math.Rect_i32 = .{ off[0], off[1] - h, off[0] + src[2], off[1] + src[3] };
        off[1] += src[3] - h;
        try surf.blit(&dst, message.render_surf, &src);
    }
    messages.orderedRemoveMany(to_remove.items);
}
