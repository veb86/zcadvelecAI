#!/usr/bin/env python3
"""Decode the PUSH_MATRIX at the start of SPDSTABLE2's proxy graphic."""
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


def main():
    data = bytes.fromhex(extract_first_310(DXF_PATH))
    # cmd0 is at idx 8, length 136, op 29 PUSH_MATRIX.
    # After len (4 bytes) + op (4 bytes) = 8, matrix data starts.
    p = 8 + 8  # cmd start idx 8, skip cmd_len and op
    mat = struct.unpack_from('<16d', data, p)
    print('PUSH_MATRIX 16 doubles:')
    for r in range(4):
        print('  ', mat[r*4:r*4+4])


if __name__ == '__main__':
    main()
