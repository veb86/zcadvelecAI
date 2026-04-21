#!/usr/bin/env python3
"""Parse OpCode 11 (TEXT2) commands from SPDSTABLE2 binary proxy graphic."""
import os
import struct

DXF_PATH = os.path.join(
    os.path.dirname(os.path.abspath(__file__)),
    '..', 'cad_source', 'test', 'spdstable.dxf')


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


def read_padded_ansi(data, offset):
    """Read null-terminated ANSI string aligned to 4 bytes."""
    end = data.find(b'\x00', offset)
    if end == -1:
        end = len(data)
    s = data[offset:end].decode('cp1251', errors='replace')
    # Length includes null terminator, then align
    length = end - offset + 1
    aligned = (length + 3) & ~3
    return s, offset + aligned


def decode_text2(data, idx, cmd_len):
    """OpCode 11 TEXT2 format (from ezdxf):
      position: 3d (24 bytes)
      normal: 3d (24 bytes)
      text_direction: 3d (24 bytes)
      text: padded ANSI string
      ignore_length: int32, raw: int32
      height: d, width_factor: d, oblique: d, tracking_percentage: d
      is_backward, is_upside, is_vert, is_under, is_over: 5 * uint32
      font_filename: padded ANSI string
      big_font_filename: padded ANSI string
    """
    p = idx + 8  # skip cmd_len + opcode
    end = idx + cmd_len
    pos = struct.unpack_from('<3d', data, p); p += 24
    normal = struct.unpack_from('<3d', data, p); p += 24
    direction = struct.unpack_from('<3d', data, p); p += 24
    text, p = read_padded_ansi(data, p)
    ignore_len, raw = struct.unpack_from('<2l', data, p); p += 8
    height, width_factor, oblique, tracking = struct.unpack_from('<4d', data, p); p += 32
    flags = struct.unpack_from('<5L', data, p); p += 20
    font_filename, p = read_padded_ansi(data, p)
    if p < end:
        big_font, p = read_padded_ansi(data, p)
    else:
        big_font = ''
    return {
        'pos': pos,
        'text': text,
        'height': height,
        'width_factor': width_factor,
        'font': font_filename,
        'big_font': big_font,
    }


def main():
    hexstr = extract_first_310(DXF_PATH)
    data = bytes.fromhex(hexstr)
    idx = 8
    while idx + 8 <= len(data):
        cmd_len = struct.unpack_from('<i', data, idx)[0]
        opcode = struct.unpack_from('<i', data, idx + 4)[0]
        if opcode == 11:
            info = decode_text2(data, idx, cmd_len)
            print(f'idx={idx} TEXT2 text={info["text"]!r} h={info["height"]} wf={info["width_factor"]} font={info["font"]!r} bf={info["big_font"]!r}')
        if cmd_len <= 0 or cmd_len > len(data):
            break
        idx += cmd_len


if __name__ == '__main__':
    main()
