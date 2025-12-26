const std = @import("std");

pub const CPU = @import("emul/CPU.zig");
pub const Inst = @import("emul/Inst.zig");
pub const Mem = @import("emul/Mem.zig");
pub const Prog = @import("emul/Prog.zig");
pub const Syntax = @import("emul/Syntax.zig");
pub const font = @import("emul/font.zig");
pub const storage = @import("emul/storage.zig");

test {
    std.testing.refAllDecls(@This());
}
