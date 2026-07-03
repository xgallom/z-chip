# z-chip

CHIP-8 / SUPER-CHIP / XO-CHIP-class emulator, written in Zig, GPU-rendered via [zengine](https://www.github.com/xgallom/zengine).

Includes `z-chip-c`, a compiler for `.zc8`, a small assembly-like source format that compiles to CHIP-8 bytecode.

## Requirements

- Zig 0.15.2+
- `zengine` checked out as a sibling directory (`../zengine`), per `build.zig.zon`
- Git LFS (repo assets are stored via LFS)

## Build

```sh
zig build
```

Produces:
- `zig-out/bin/z-chip` — the emulator
- `zig-out/bin/z-chip-c` — the `.zc8` compiler

## Run

```sh
zig build run -- <path-to-rom>
```

`<path-to-rom>` is a `.ch8` binary, or a `.bin` produced by `z-chip-c`.

## Compile a `.zc8` program

```sh
zig build compiler -- <output-path> <input.zc8> [more-input.zc8 ...]
```

Multiple input files are concatenated into one compilation unit. See `assets/prog/src/` for an example split across `stddef.zc8` and `main.zc8`.

The build also has a `build game` step that compiles `assets/prog/src/{stddef,main}.zc8` into `zig-out/prog/game.ch8`.

## Devices

Three quirk profiles, cycled at runtime with `F2`:

| device | vf_reset | i_increment | drw_sync | drw_clip_bottom | drw_16x16 | shift_vx | jp_v0a_n |
|---|---|---|---|---|---|---|---|
| `chip8` | yes | yes | yes | yes | no | no | no |
| `schip` | no | no | no | yes | yes | yes | yes |
| `zchip` (default) | no | yes | no | yes | yes | no | no |

Each profile sets the interpreter quirks (register-shift source, `I` increment on store/load, draw-clip, `Bnn`-style jump addressing, draw/vblank sync) matching that machine's known behavior. `zchip` is this emulator's own default profile.

Screen buffer is fixed at 128x64 (XO-CHIP hi-res), with lo-res 64x32 CHIP-8/SCHIP output upscaled into it. Two draw planes are supported (`F4` cycles plane selection).

## Controls

Standard CHIP-8 hex keypad, mapped to a QWERTY layout:

```
Keypad          Keyboard
1 2 3 C         1 2 3 4
4 5 6 D         Q W E R
7 8 9 E         A S D F
A 0 B F         Z X C V
```

Emulator controls:

| key | action |
|---|---|
| Space | play / pause |
| `]` | single-step (while paused) |
| `-` / `=` | decrease / increase run speed (420Hz .. 3.6MHz) |
| L | toggle debug logging |
| F1 | toggle debug UI |
| F2 | cycle device (`chip8` / `schip` / `zchip`) |
| F3 | toggle draw-sync |
| F4 | cycle draw plane |
| F5 | dump CPU/memory/screen state to stderr |
| F10 | reset |
| Esc | quit |

## Layout

```
src/
  main.zig        emulator entrypoint: engine setup, input, run loop, render
  compiler.zig     z-chip-c entrypoint: reads .zc8 files, emits CHIP-8 bytecode
  render.zig       GPU pipeline for the emulated screen
  Message.zig      on-screen toast messages
  emul/
    CPU.zig        instruction execution, device quirk tables
    Inst.zig       instruction decode
    Mem.zig        memory, registers, screen, keyboard, stack
    storage.zig     rom load/store, persisted flag registers
    font.zig       built-in hex-digit sprite font
    Syntax.zig      .zc8 tokenizer
    Parser.zig      .zc8 parser
    Prog.zig       parsed .zc8 -> resolved CHIP-8 bytecode
assets/
  prog/            .ch8 roms, .8o sources, .zc8 sources
  fonts/, lut/     UI font, color lookup table
shaders/emul/      screen render shaders
```

## Tests

```sh
zig build test
```
