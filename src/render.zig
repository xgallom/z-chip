const std = @import("std");
const assert = std.debug.assert;

const zengine = @import("zengine");
const allocators = zengine.allocators;
const global = zengine.global;
const math = zengine.math;
const gfx_options = zengine.options.gfx;
const perf = zengine.perf;
const ui_mod = zengine.ui;
const gfx = zengine.gfx;
const Error = gfx.Error;
const Renderer = gfx.Renderer;
const sections = Renderer.sections;

const Mem = @import("Mem.zig");
const Message = @import("Message.zig");

const log = std.log.scoped(.gfx_render);

pub fn createEmulPipeline(loader: *gfx.Loader) !void {
    const vert = loader.renderer.shaders.get("system/screen.vert");
    const frag = try loader.loadShader(.fragment, "emul/screen.frag");
    _ = try loader.renderer.createGraphicsPipeline("emul_screen", &.{
        .vertex_shader = vert,
        .fragment_shader = frag,
        .target_info = .{
            .color_target_descriptions = &.{
                .{ .format = .hdr_f, .blend_state = .blend },
            },
        },
    });
}

pub fn renderScreen(
    self: *const Renderer,
    ui_ptr: ?*ui_mod.UI,
    bloom_pass: *gfx.pass.Bloom,
    fence: ?*gfx.GPUFence,
) !bool {
    const section = Renderer.sections.sub(.render);
    section.sub(.acquire).begin();
    section.begin();

    if (fence) |f| {
        if (f.isValid()) {
            try self.gpu_device.wait(.any, &.{f.*});
            self.gpu_device.release(f);
        }
    }

    const emul_pipeline = self.pipelines.graphics.get("emul_screen");
    const blend_pipeline = self.pipelines.graphics.get("blend");
    const render_pipeline = self.pipelines.graphics.get("render");
    const emul_screen = self.textures.get("emul_screen");
    const messages_buffer = self.textures.get("messages_buffer");
    const output_buffer = self.textures.get("output_buffer");
    const screen_buffer = self.textures.get("screen_buffer");
    const screen_sampler = self.samplers.get("nearest_clamp_to_edge");
    const lut_sampler = self.samplers.get("trilinear_clamp_to_edge");
    const lut_map = self.textures.get(self.settings.lut);

    const fa = allocators.frame();

    var command_buffer = try self.gpu_device.commandBuffer();
    errdefer command_buffer.cancel() catch {};

    const swapchain = try command_buffer.swapchainTexture(self.window);

    section.sub(.acquire).end();

    if (!swapchain.isValid()) {
        log.info("skip draw", .{});
        section.pop();
        return false;
    }

    section.sub(.init).begin();

    const win_size = math.point_u32.to(f32, &self.window.logicalSize());
    const wh_ratio = win_size[0] / win_size[1];
    const scr_size = math.point_u32.to(f32, &.{ Mem.scr_w, Mem.scr_h });
    const scr_ratio = scr_size[0] / scr_size[1];
    const scr_scl: math.Point_f32 = if (wh_ratio > scr_ratio) .{
        wh_ratio / scr_ratio,
        1,
    } else .{
        1,
        scr_ratio / wh_ratio,
    };

    const uniform_buf = try fa.alloc(f32, 32);
    @memcpy(uniform_buf[0..16], math.matrix4x4.sliceConst(&math.matrix4x4.identity));
    @memcpy(uniform_buf[16..32], math.matrix4x4.sliceConst(&math.matrix4x4.identity));

    section.sub(.init).end();

    {
        var render_pass = try command_buffer.renderPass(&.{
            .{ .texture = screen_buffer, .load_op = .clear, .store_op = .store },
        }, null);

        render_pass.bindPipeline(emul_pipeline);
        command_buffer.pushUniformData(.fragment, 0, &scr_scl);
        try render_pass.bindSamplers(.fragment, 0, &.{
            .{ .texture = emul_screen, .sampler = screen_sampler },
        });
        render_pass.drawScreen();
        render_pass.end();
    }

    {
        var render_pass = try command_buffer.renderPass(&.{
            .{ .texture = screen_buffer, .load_op = .load, .store_op = .store },
        }, null);

        render_pass.bindPipeline(blend_pipeline);
        try render_pass.bindSamplers(.fragment, 0, &.{
            .{ .texture = messages_buffer, .sampler = screen_sampler },
        });
        render_pass.drawScreen();
        render_pass.end();
    }

    try bloom_pass.render(self, command_buffer, screen_buffer, output_buffer);
    {
        var render_pass = try command_buffer.renderPass(&.{
            .{ .texture = swapchain, .load_op = .clear, .store_op = .store },
        }, null);

        render_pass.bindPipeline(render_pipeline);
        command_buffer.pushUniformData(.fragment, 0, &self.settings.uniformBuffer());
        try render_pass.bindSamplers(.fragment, 0, &.{
            .{ .texture = output_buffer, .sampler = screen_sampler },
            .{ .texture = lut_map, .sampler = lut_sampler },
        });
        render_pass.drawScreen();
        render_pass.end();
    }

    if (ui_ptr) |ui| {
        section.sub(.ui).begin();
        if (ui.render_ui) {
            try ui.submitPass(command_buffer, output_buffer);
            {
                var render_pass = try command_buffer.renderPass(&.{
                    .{ .texture = swapchain, .load_op = .load, .store_op = .store },
                }, null);

                const ui_settings: Renderer.Settings = .{
                    .config = .{
                        .has_srgb = true,
                    },
                };
                render_pass.bindPipeline(render_pipeline);
                command_buffer.pushUniformData(.fragment, 0, &ui_settings.uniformBuffer());
                try render_pass.bindSamplers(.fragment, 0, &.{
                    .{ .texture = output_buffer, .sampler = screen_sampler },
                    .{ .texture = lut_map, .sampler = lut_sampler },
                });
                render_pass.drawScreen();
                render_pass.end();
            }
        }
        section.sub(.ui).end();
    }

    section.sub(.submit).begin();
    log.debug("submit command buffer", .{});
    if (fence) |f| {
        assert(!f.isValid());
        f.* = try command_buffer.submitFence();
    } else try command_buffer.submit();
    section.sub(.submit).end();

    section.end();
    return true;
}
