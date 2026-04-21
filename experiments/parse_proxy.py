#!/usr/bin/env python3
"""Parse proxy graphic binary - find OpCode 11 and OpCode 38 text commands."""
import struct
import sys


SYSTEM_OPCODES = {
    1: 'Extents',
    14: 'SetColor',
    16: 'SetLayer',
    18: 'SetLinetype',
    19: 'SetMarker',
    20: 'SetFill',
    22: 'SetTrueColor',
    23: 'SetLineweight',
    24: 'SetLtScale',
    25: 'SetThickness',
    29: 'PushMatrix',
    30: 'PushMatrix2',
    31: 'PopMatrix',
}

PRIMITIVE_OPCODES = {
    2: 'Circle',
    3: 'Circle3P',
    4: 'Arc',
    5: 'Arc3P',
    6: 'Polyline',
    7: 'Polygon',
    8: 'Mesh',
    9: 'Shell',
    10: 'Text',
    11: 'Text2',
    36: 'UnicodeText',
    38: 'UnicodeText2',
}


def parse(path, is_unicode):
    with open(path, 'rb') as f:
        data = f.read()

    print(f"\n=== Parsing {path} (unicode={is_unicode}) ===")
    print(f"Total size: {len(data)}")
    off = 0
    chunk_size, cmd_count = struct.unpack_from("<ii", data, off)
    off += 8
    print(f"ChunkSize={chunk_size} CommandCount={cmd_count}")

    for i in range(cmd_count):
        if off >= len(data):
            break
        size, opcode = struct.unpack_from("<ii", data, off)
        name = SYSTEM_OPCODES.get(opcode) or PRIMITIVE_OPCODES.get(opcode) or f'UnknownOp{opcode}'
        print(f"  Cmd[{i}] off={off} Size={size} OpCode={opcode} ({name})")
        if opcode == 11 or opcode == 36:
            # Dump bytes
            print(f"    DATA: {data[off+8:off+size].hex()}")
            decode_text2(data[off+8:off+size], is_unicode)
        elif opcode == 38:
            print(f"    DATA: {data[off+8:off+size].hex()}")
            decode_unicode_text2(data[off+8:off+size])
        elif opcode == 10:
            print(f"    DATA: {data[off+8:off+size].hex()}")
        off += size


def decode_text2(body, is_unicode):
    """Decode OpCode 11 (Text2) body.

    Based on OpenDesign docs for AcGiWorldDraw binary proxy graphics, OpCode 11
    is the "text 2" primitive (analogous to UnicodeText2 but without TypeFace).

    Guess format:
      position (3 doubles)
      normal (3 doubles)
      direction (3 doubles)
      msg (null-terminated wide or ansi string, padded to 4 bytes)
      length (int32)
      raw (int32)
      height (double)
      width factor (double)
      oblique angle (double)
      tracking (double)
      backward, upside_down (int32 x 2)
      font name (null-terminated, padded)
      big font name (null-terminated, padded)
    """
    off = 0
    px, py, pz = struct.unpack_from("<ddd", body, off); off += 24
    nx, ny, nz = struct.unpack_from("<ddd", body, off); off += 24
    dx, dy, dz = struct.unpack_from("<ddd", body, off); off += 24
    print(f"    Pos=({px:.3f},{py:.3f},{pz:.3f}) Normal=({nx:.3f},{ny:.3f},{nz:.3f}) Dir=({dx:.3f},{dy:.3f},{dz:.3f})")
    print(f"    After geometry (off={off}): {body[off:off+40].hex()}")

    def read_padded_string():
        nonlocal off
        if is_unicode:
            # UTF-16 null-term
            start = off
            end = off
            while end + 1 < len(body) and (body[end] != 0 or body[end+1] != 0):
                end += 2
            s = body[start:end].decode('utf-16-le', errors='replace')
            off = end + 2
        else:
            start = off
            end = body.find(b'\x00', off)
            s = body[start:end].decode('latin-1', errors='replace') if end >= 0 else ''
            off = end + 1
        # 4-byte padding
        while off % 4 != 0:
            off += 1
        return s

    text = read_padded_string()
    print(f'    Text = "{text}" (off={off})')
    print(f"    Remaining: {body[off:off+50].hex()}")

    if off + 4 <= len(body):
        length = struct.unpack_from("<i", body, off)[0]; off += 4
        raw = struct.unpack_from("<i", body, off)[0]; off += 4
        print(f"    Length={length} Raw={raw}")

    if off + 8 <= len(body):
        height = struct.unpack_from("<d", body, off)[0]; off += 8
        print(f"    Height={height}")
    if off + 8 <= len(body):
        wf = struct.unpack_from("<d", body, off)[0]; off += 8
        print(f"    WidthFactor={wf}")
    if off + 8 <= len(body):
        ob = struct.unpack_from("<d", body, off)[0]; off += 8
        print(f"    Oblique={ob}")

    print(f"    After fixed fields (off={off}), remaining: {body[off:].hex()}")


def decode_unicode_text2(body):
    off = 0
    px, py, pz = struct.unpack_from("<ddd", body, off); off += 24
    nx, ny, nz = struct.unpack_from("<ddd", body, off); off += 24
    dx, dy, dz = struct.unpack_from("<ddd", body, off); off += 24
    print(f"    Pos=({px:.3f},{py:.3f},{pz:.3f})")

    # UTF-16 null-term
    start = off
    end = off
    while end + 1 < len(body) and (body[end] != 0 or body[end+1] != 0):
        end += 2
    text = body[start:end].decode('utf-16-le', errors='replace')
    off = end + 2
    while off % 4 != 0:
        off += 1
    print(f'    Text = "{text}" (off={off})')


if __name__ == '__main__':
    parse('../cad_source/test/spdsconstructionline2000.proxy0.bin', is_unicode=False)
    parse('../cad_source/test/spdsconstructionline2007.proxy0.bin', is_unicode=True)
