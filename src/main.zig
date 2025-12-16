const std = @import("std");
const assert = std.debug.assert;

const zengine = @import("zengine");
const Zengine = zengine.Zengine;
const allocators = zengine.allocators;
const ecs = zengine.ecs;
const Event = zengine.Event;
const gfx = zengine.gfx;
const Scene = zengine.gfx.Scene;
const global = zengine.global;
const math = zengine.math;
const perf = zengine.perf;
const c = zengine.ext.c;
const scheduler = zengine.scheduler;
const time = zengine.time;
const Engine = zengine.Engine;
const ui = zengine.ui;
const str = zengine.str;

const CPU = @import("CPU.zig");
const Inst = @import("Inst.zig");
const Mem = @import("Mem.zig");
const Message = @import("Message.zig");
const emul_render = @import("render.zig");

const log = std.log.scoped(.main);

pub const std_options: std.Options = .{
    .log_level = .info,
    .log_scope_levels = &.{
        // .{ .scope = .main, .level = .debug },
        // .{ .scope = .cpu, .level = .debug },
        // .{ .scope = .alloc, .level = .debug },
        // .{ .scope = .engine, .level = .debug },
        // .{ .scope = .gfx_gpu_device, .level = .debug },
        // .{ .scope = .gfx_loader, .level = .debug },
        // .{ .scope = .gfx_obj_loader, .level = .debug },
        // .{ .scope = .gfx_mesh, .level = .debug },
        // .{ .scope = .gfx_renderer, .level = .debug },
        // .{ .scope = .gfx_copy_pass, .level = .debug },
        // .{ .scope = .gfx_shader, .level = .debug },
        // .{ .scope = .gfx_shader_loader, .level = .debug },
        // .{ .scope = .gfx_scene_node, .level = .debug },
        // .{ .scope = .key_tree, .level = .debug },
        // .{ .scope = .radix_tree, .level = .debug },
        // .{ .scope = .scheduler, .level = .debug },
        // .{ .scope = .tree, .level = .debug },
        // .{ .scope = .scene, .level = .debug },
    },
    .logFn = logFn,
};

pub const zengine_options: zengine.Options = .{
    .has_debug_ui = false,
    .log_allocations = false,
};

const RenderPasses = struct {
    bloom: gfx.pass.Bloom = .{
        .intensity = 0.05,
    },
};

const font_key = "fonts/pixel_letters.ttf";

var app_args: [][:0]u8 = &.{};
var emul: Emulator = .{};

var gfx_loader: gfx.Loader = undefined;
var gfx_passes: RenderPasses = .{};
var gfx_fence: gfx.GPUFence = .invalid;

var allocs_window: zengine.ui.AllocsWindow = undefined;
var perf_window: zengine.ui.PerfWindow = undefined;
var log_window: zengine.ui.LogWindow = .invalid;

const TickCounter = time.StaticCounter(std.time.ns_per_s / 60);
const RunCounter = time.StaticCounter(std.time.ns_per_s / 500);

const Emulator = struct {
    cpu: *CPU = undefined,
    mem: *Mem = undefined,
    flags: Flags = .initEmpty(),
    tick_counter: TickCounter = .init,
    run_counter: RunCounter = .init,

    const State = enum(std.math.Log2Int(u32)) {
        exit,
        running,
        step,
        exec,
        wait_for_key,
        wait_for_draw,
        draw,
        play_sound,
        log_debug,
    };
    const Flags = std.EnumSet(State);
};

fn logFn(
    comptime message_level: std.log.Level,
    comptime scope: @Type(.enum_literal),
    comptime format: []const u8,
    args: anytype,
) void {
    const level_txt = comptime message_level.asText();
    const prefix2 = if (scope == .default) ": " else "(" ++ @tagName(scope) ++ "): ";
    log_window.print(level_txt ++ prefix2 ++ format ++ "\n", args) catch |err| {
        std.log.defaultLog(.err, .default, "failed printing to log window: {t}", .{err});
    };
    std.log.defaultLog(message_level, scope, format, args);
}

pub fn main() !void {
    allocators.init(1_000_000_000);
    defer allocators.deinit();

    log_window = try .init(allocators.gpa());
    defer log_window.deinit();

    app_args = try std.process.argsAlloc(allocators.global());
    if (app_args.len < 2) return error.NotEnoughArguments;
    if (app_args.len > 2) return error.TooManyArguments;

    // log_window = try .init(allocators.gpa());
    // defer log_window.deinit();

    var engine = try Zengine.create(.{
        .register = &register,
        .load = &load,
        .unload = &unload,
        .input = &input,
        .update = &update,
        .render = &render,
    });
    defer engine.deinit();
    return engine.run();
}

fn register() !void {
    try CPU.register();
}

fn load(self: *const Zengine) !bool {
    gfx_loader = try .init(self.renderer);
    errdefer gfx_loader.deinit();
    {
        errdefer gfx_loader.cancel();
        try gfx_loader.createGraphicsPipelines();
        try emul_render.createEmulPipeline(&gfx_loader);

        try gfx.pass.Bloom.init(&gfx_loader);

        const main_win = self.engine.windows.getPtr("main");
        _ = try gfx_loader.loadLut("lut/basic.cube");
        _ = try gfx_loader.loadFont(font_key, 60);
        _ = try gfx_loader.createSurfaceTexture("messages_buffer", main_win.logicalSize(), .default);

        const st = try gfx_loader.createSurfaceTexture(
            "emul_screen",
            .{ Mem.scr_hi_w, Mem.scr_hi_h },
            .default,
        );
        const surf = st.surf;
        const scr = surf.slice(u32);
        assert(surf.width() * surf.height() == Mem.scr_hi_size);
        for (0..Mem.scr_hi_h) |y| {
            for (0..Mem.scr_hi_w) |x| {
                const w = Mem.scr_hi_w - 1;
                const h = Mem.scr_hi_h - 1;
                const is_g = x == 0 and y == 0 or x == w and y == 0 or x == 0 and y == h or x == w and y == h;
                scr[y * Mem.scr_hi_w + x] = surf.rgba(.{
                    @intCast(255 * x / Mem.scr_hi_w),
                    if (is_g) 255 else 0,
                    @intCast(128 * y / Mem.scr_hi_h),
                    1,
                });
            }
        }
        gfx_fence = try gfx_loader.commit();
    }

    Zengine.sections.sub(.load).sub(.ui).begin();

    allocs_window = .init();
    perf_window = .init(allocators.global());

    Zengine.sections.sub(.load).sub(.ui).end();
    allocators.scratchRelease();

    emul.cpu = try CPU.init(allocators.gpa());
    emul.mem = try Mem.init(allocators.gpa());

    try reset();
    try Message.add("press space to start...");

    return true;
}

fn reset() !void {
    emul.mem.reset();
    emul.cpu.reset();

    {
        const src = try std.fs.cwd().openFile(app_args[1], .{});
        defer src.close();
        var buf: [256]u8 = undefined;
        var reader = src.reader(&buf);
        try emul.mem.data.load(&reader.interface);
        if (!reader.atEnd()) return error.ProgramTooLong;
    }

    emul.cpu.dump(.big);
    emul.mem.dump();
    emul.flags.insert(.draw);
}

fn unload(self: *const Zengine) void {
    Message.deinit();
    emul.cpu.deinit(allocators.gpa());
    emul.mem.deinit(allocators.gpa());
    allocs_window.deinit();
    perf_window.deinit();
    gfx_loader.deinit();
    if (gfx_fence.isValid()) {
        self.renderer.gpu_device.wait(.any, &.{gfx_fence}) catch unreachable;
        self.renderer.gpu_device.release(&gfx_fence);
    }
}

fn input(self: *const Zengine) !bool {
    while (Event.poll()) |event| {
        if (self.ui.show_ui and c.ImGui_ImplSDL3_ProcessEvent(&event.sdl)) {
            switch (event.type) {
                .quit => return false,
                .key_down => {
                    if (event.sdl.key.repeat) break;
                    switch (event.sdl.key.key) {
                        c.SDLK_F1 => self.ui.show_ui = !self.ui.show_ui,
                        c.SDLK_ESCAPE => self.ui.show_ui = !self.ui.show_ui,
                        else => {},
                    }
                },
                else => {},
            }
            continue;
        }

        switch (event.type) {
            .quit => return false,
            .key_down => {
                if (event.sdl.key.key == c.SDLK_RIGHTBRACKET) {
                    if (!emul.flags.contains(.running)) {
                        emul.flags.insert(.step);
                        try Message.add("step");
                    }
                }
                if (event.sdl.key.repeat) break;
                switch (event.sdl.key.key) {
                    c.SDLK_1 => keyDown(0x1),
                    c.SDLK_2 => keyDown(0x2),
                    c.SDLK_3 => keyDown(0x3),
                    c.SDLK_4 => keyDown(0xc),

                    c.SDLK_Q => keyDown(0x4),
                    c.SDLK_W => keyDown(0x5),
                    c.SDLK_E => keyDown(0x6),
                    c.SDLK_R => keyDown(0xd),

                    c.SDLK_A => keyDown(0x7),
                    c.SDLK_S => keyDown(0x8),
                    c.SDLK_D => keyDown(0x9),
                    c.SDLK_F => keyDown(0xe),

                    c.SDLK_Z => keyDown(0xa),
                    c.SDLK_X => keyDown(0x0),
                    c.SDLK_C => keyDown(0xb),
                    c.SDLK_V => keyDown(0xf),

                    c.SDLK_SPACE => {
                        emul.flags.toggle(.running);
                        try Message.add(if (emul.flags.contains(.running)) "play" else "pause");
                    },
                    c.SDLK_L => emul.flags.toggle(.log_debug),
                    c.SDLK_F1 => self.ui.show_ui = !self.ui.show_ui,
                    c.SDLK_F2 => {
                        emul.cpu.device.next();
                        emul.cpu.updateJumpTable();
                        emul.cpu.dump(.big);
                        try Message.add(@tagName(emul.cpu.device));
                    },
                    c.SDLK_F10 => try reset(),
                    c.SDLK_ESCAPE => return false,
                    else => {},
                }
            },
            .key_up => switch (event.sdl.key.key) {
                c.SDLK_1 => keyUp(0x1),
                c.SDLK_2 => keyUp(0x2),
                c.SDLK_3 => keyUp(0x3),
                c.SDLK_4 => keyUp(0xc),

                c.SDLK_Q => keyUp(0x4),
                c.SDLK_W => keyUp(0x5),
                c.SDLK_E => keyUp(0x6),
                c.SDLK_R => keyUp(0xd),

                c.SDLK_A => keyUp(0x7),
                c.SDLK_S => keyUp(0x8),
                c.SDLK_D => keyUp(0x9),
                c.SDLK_F => keyUp(0xe),

                c.SDLK_Z => keyUp(0xa),
                c.SDLK_X => keyUp(0x0),
                c.SDLK_C => keyUp(0xb),
                c.SDLK_V => keyUp(0xf),

                else => {},
            },
            else => {},
        }
    }
    return true;
}

fn keyDown(comptime key: u4) void {
    emul.mem.key.down(key);
    if (emul.flags.contains(.wait_for_key) and emul.cpu.key == null) {
        log.debug("received awaited key {X}", .{key});
        emul.cpu.key = key;
    }
}

fn keyUp(comptime key: u4) void {
    emul.mem.key.up(key);
    if (emul.flags.contains(.wait_for_key) and emul.cpu.key == key) {
        log.debug("released awaited key {X}", .{key});
        emul.flags.remove(.wait_for_key);
        emul.flags.insert(.exec);
    }
}

fn update(self: *const Zengine) !bool {
    if (try run()) emul.flags.insert(.draw);

    errdefer gfx_loader.cancel();
    if (emul.flags.contains(.draw)) {
        emul.flags.remove(.draw);
        const st = try gfx_loader.rendererSurfaceTexture("emul_screen");
        const surf = st.surf;
        const scr = surf.slice(u32);
        // const black = surf.rgba(.{ 174, 95, 0, 255 });
        // const white = surf.rgba(.{ 255, 198, 0, 255 });
        const black = surf.rgba(.{ 0, 0, 0, 255 });
        const white = surf.rgba(.{ 255, 255, 255, 255 });

        assert(surf.byteLen() == Mem.scr_hi_size * @sizeOf(u32));
        for (0..Mem.scr_hi_size) |n| scr[n] = if (emul.mem.scr.data.isSet(n)) white else black;
    }

    if (Message.messages.items.len) {
        const surf_tex = try gfx_loader.rendererSurfaceTexture("messages_buffer");
        try Message.render(surf_tex.surf, gfx_loader.fonts.get(font_key));
    }

    if (gfx_fence.isValid()) {
        try self.renderer.gpu_device.wait(.any, &.{gfx_fence});
        self.renderer.gpu_device.release(&gfx_fence);
    }
    gfx_fence = try gfx_loader.commit();
    return !emul.flags.contains(.exit);
}

fn run() !bool {
    CPU.beginSection();
    defer CPU.endSection();
    if (emul.flags.contains(.running)) {
        emul.run_counter.add(global.sinceLastFrameNano());
        while (emul.run_counter.run()) {
            const is_drw_sync = emul.cpu.run_flags.is_drw_sync;
            if (!try step()) return false;
            emul.tick_counter.add(RunCounter.interval);
            if (emul.tick_counter.run()) tick();
            if (is_drw_sync) return true;
        }
        return false;
    }
    if (emul.flags.contains(.step)) {
        emul.flags.remove(.step);
        const is_drw_sync = emul.cpu.run_flags.is_drw_sync;
        if (!try step()) return false;
        emul.tick_counter.add(RunCounter.interval);
        if (emul.tick_counter.run()) tick();
        return is_drw_sync;
    }
    return false;
}

fn step() !bool {
    emulStep() catch |err| switch (err) {
        Inst.Error.InvalidInstruction => {
            log.warn("invalid instruction", .{});
            emul.cpu.dump(.big);
            emul.mem.dump();
            return err;
        },
        Inst.Error.WaitForKey => {
            assert(!emul.flags.contains(.wait_for_key));
            log.debug("waiting for key", .{});
            emul.flags.insert(.wait_for_key);
            return true;
        },
        Inst.Error.WaitingForKey => {
            assert(emul.flags.contains(.wait_for_key));
            std.atomic.spinLoopHint();
        },
        Inst.Error.WaitForDraw => {
            assert(!emul.flags.contains(.wait_for_draw));
            log.debug("waiting for draw", .{});
            emul.flags.insert(.wait_for_draw);
            return false;
        },
        Inst.Error.WaitingForDraw => {
            assert(emul.flags.contains(.wait_for_draw));
            std.atomic.spinLoopHint();
        },
        Inst.Error.Exit => {
            emul.flags.insert(.exit);
            return false;
        },
        else => {
            emul.cpu.dump(.big);
            emul.mem.dump();
            return err;
        },
    };

    if (emul.flags.contains(.log_debug)) {
        emul.mem.regs.dump();
        switch (emul.cpu.inst.op) {
            .cls, .drw_rrc => emul.mem.scr.dump(),
            .skp_r, .sknp_r, .ld_rk => emul.mem.key.dump(),
            else => {},
        }
    }
    return true;
}

fn emulStep() !void {
    if (emul.flags.contains(.wait_for_key)) return Inst.Error.WaitingForKey;
    if (emul.flags.contains(.wait_for_draw)) {
        if (!emul.cpu.run_flags.is_drw_sync) return Inst.Error.WaitingForDraw;
        emul.flags.remove(.wait_for_draw);
        emul.flags.insert(.exec);
    }

    if (emul.flags.contains(.exec)) {
        log.debug("exec", .{});
        emul.flags.remove(.exec);
        try emul.cpu.exec(emul.mem);
    } else try emul.cpu.step(emul.mem);
    emul.cpu.run_flags.is_drw_sync = false;
}

fn tick() void {
    emul.mem.regs.dt -|= 1;
    emul.mem.regs.st -|= 1;
    emul.cpu.run_flags.is_drw_sync = true;
}

fn render(self: *const Zengine) !void {
    self.ui.beginDraw();
    self.ui.drawMainMenuBar(.{
        .allocs_open = &allocs_window.is_open,
        .perf_open = &perf_window.is_open,
        .log_open = &log_window.is_open,
    });
    self.ui.drawDock();

    self.ui.draw(allocs_window.element(), &allocs_window.is_open);
    self.ui.draw(perf_window.element(), &perf_window.is_open);
    self.ui.draw(log_window.element(), &log_window.is_open);

    self.ui.endDraw();

    _ = try emul_render.renderScreen(self.renderer, self.ui, &gfx_passes.bloom, &gfx_fence);
}

test {
    std.testing.refAllDecls(CPU);
    std.testing.refAllDecls(Inst);
    std.testing.refAllDecls(Mem);
}
