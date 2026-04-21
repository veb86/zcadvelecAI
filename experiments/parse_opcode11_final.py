#!/usr/bin/env python3
"""Full decoding of OpCode 11 (pgcText2) body."""
import struct

# Actual 156-byte body from the 2000 file:
actual = bytes.fromhex("90194de2f795a4401868bcc075f89740000000000000000000000000000000000000000000000000000000000000f03f000000000000f03f0000000000000000000000000000000031350000ffffffff000000000000000000e07540000000000000f03f0000000000000000000000000000f03f000000000000000000000000000000000000000043535f476f7374323330342e7368780000000000")

# Second body (different last bytes due to overlap with next command? Let me parse it too)
actual2 = bytes.fromhex("90194de2f795a440f167bf7fb6e9e340000000000000000000000000000000000000000000000000000000000000f03f000000000000f03f000000000000000000000000000000003135001fffffffff000000000000000000e07540000000000000f03f0000000000000000000000000000f03f000000000000000000000000000000000000000043535f476f7374323330342e736878000056631f")

def parse_opcode11(body, is_unicode):
    off = 0

    def read_d():
        nonlocal off
        v = struct.unpack_from("<d", body, off)[0]; off += 8; return v
    def read_i():
        nonlocal off
        v = struct.unpack_from("<i", body, off)[0]; off += 4; return v

    def read_string_padded():
        nonlocal off
        if is_unicode:
            start = off
            end = off
            while end + 1 < len(body) and (body[end] != 0 or body[end+1] != 0):
                end += 2
            s = body[start:end].decode('utf-16-le', errors='replace')
            off = end + 2
        else:
            start = off
            end = body.find(b'\x00', off)
            if end < 0:
                end = len(body)
            s = body[start:end].decode('latin-1', errors='replace')
            off = end + 1
        # Pad to 4
        while off % 4 != 0 and off < len(body):
            off += 1
        return s

    print(f"\n=== Parsing body len={len(body)} unicode={is_unicode} ===")
    px, py, pz = read_d(), read_d(), read_d()
    print(f"Position: ({px:.3f}, {py:.3f}, {pz:.3f})")
    nx, ny, nz = read_d(), read_d(), read_d()
    print(f"Normal: ({nx:.3f}, {ny:.3f}, {nz:.3f})")
    dx, dy, dz = read_d(), read_d(), read_d()
    print(f"Direction: ({dx:.3f}, {dy:.3f}, {dz:.3f})")

    text = read_string_padded()
    print(f'Text: "{text}" (off={off})')

    length = read_i()
    raw = read_i()
    print(f"Length={length} Raw={raw}")

    height = read_d()
    wfac = read_d()
    oblique = read_d()
    tracking = read_d()
    print(f"Height={height} WidthFactor={wfac} Oblique={oblique} Tracking={tracking}")

    flags = [read_i() for _ in range(5)]
    print(f"Flags: {flags}")

    font_name = read_string_padded()
    print(f'FontName: "{font_name}" (off={off})')

    big_font = read_string_padded() if off < len(body) else ''
    print(f'BigFont: "{big_font}" (off={off})')

    print(f"Final off={off}/{len(body)}")


parse_opcode11(actual, is_unicode=False)
parse_opcode11(actual2, is_unicode=False)
