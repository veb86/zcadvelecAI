#!/usr/bin/env python3
"""Decode SPDSTABLE2 proxy binary commands, printing opcode list."""
import os
import struct

DXF_PATH = os.path.join(
    os.path.dirname(os.path.abspath(__file__)),
    '..', 'cad_source', 'test', 'spdstable.dxf')


OPCODES = {
    1: 'EXTENTS', 2: 'CIRCLE', 3: 'CIRCLE_3P', 4: 'CIRCULAR_ARC',
    5: 'CIRCULAR_ARC_3P', 6: 'POLYLINE', 7: 'POLYGON', 8: 'MESH',
    9: 'SHELL', 10: 'TEXT', 11: 'TEXT2', 12: 'XLINE', 13: 'RAY',
    14: 'SET_COLOR', 16: 'SET_LAYER', 18: 'SET_LINETYPE',
    19: 'SET_MARKER', 20: 'SET_FILL', 22: 'SET_TRUE_COLOR',
    23: 'SET_LINEWEIGHT', 24: 'SET_LTSCALE', 25: 'SET_THICKNESS',
    27: 'PUSH_CLIP', 28: 'POP_CLIP', 29: 'PUSH_MATRIX',
    30: 'PUSH_MATRIX2', 31: 'POP_MATRIX', 32: 'POLYLINE_NORMALS',
    33: 'LWPOLYLINE', 36: 'UNICODE_TEXT', 38: 'UNICODE_TEXT2',
    44: 'ELLIPTIC_ARC',
}


def extract_first_310(path):
    with open(path, 'r', encoding='utf-8', errors='replace') as f:
        lines = [line.rstrip('\r\n') for line in f]
    i = 0
    while i < len(lines) - 1:
        if lines[i].strip() == '0' and lines[i + 1].strip() == 'SPDSTABLE2':
            j = i + 2
            hex_chunks = []
            while j < len(lines) - 1 and lines[j].strip() != '0':
                try:
                    code = int(lines[j].strip())
                except ValueError:
                    j += 1
                    continue
                if code == 310:
                    hex_chunks.append(lines[j + 1].strip())
                j += 2
            if hex_chunks:
                return ''.join(hex_chunks)
            i = j
        else:
            i += 1


def dump_text_cmd(data, offset):
    """Assume OpCode 10 (ANSI TEXT): position(3d) normal(3d) direction(3d) height(d) widthfactor(d) oblique(d) text(null)"""
    fmt_header = '<9d'
    insert_x, insert_y, insert_z, nx, ny, nz, dx, dy, dz = struct.unpack_from(fmt_header, data, offset)
    offset += struct.calcsize(fmt_header)
    height, width_factor, oblique = struct.unpack_from('<3d', data, offset)
    offset += 24
    # Then null-terminated string
    end = data.find(b'\x00', offset)
    text = data[offset:end].decode('cp1251', errors='replace')
    return insert_x, insert_y, insert_z, height, width_factor, oblique, text


def main():
    hexstr = extract_first_310(DXF_PATH)
    data = bytes.fromhex(hexstr)
    print(f'Total bytes: {len(data)}')
    # Skip 8-byte header
    idx = 8
    cmdn = 0
    while idx + 8 <= len(data):
        cmd_len = struct.unpack_from('<i', data, idx)[0]
        opcode = struct.unpack_from('<i', data, idx + 4)[0]
        name = OPCODES.get(opcode, f'?{opcode}')
        print(f'cmd{cmdn} idx={idx} len={cmd_len} op={opcode}({name})')
        if opcode == 10 and cmd_len >= 88:
            try:
                res = dump_text_cmd(data, idx + 8)
                print(f'  TEXT1: pos=({res[0]:.3f},{res[1]:.3f},{res[2]:.3f}) h={res[3]:.4f} wf={res[4]} obl={res[5]} text={res[6]!r}')
            except Exception as e:
                print(f'  TEXT1 decode error: {e}')
        if cmd_len <= 0 or cmd_len > len(data):
            break
        idx += cmd_len
        cmdn += 1
    print(f'Total commands: {cmdn}')


if __name__ == '__main__':
    main()
