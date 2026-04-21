#!/usr/bin/env python3
"""Extract the proxy graphics binary data from the DXF file (310 lines after 92)."""
import sys


def extract_binary(path, size):
    """Find the proxy binary (after '92 <size>') in a DXF file and decode 310 lines."""
    with open(path, 'r', encoding='latin-1') as f:
        lines = f.readlines()

    results = []
    i = 0
    while i < len(lines):
        line = lines[i].strip()
        if line == '92':
            try:
                chunk_size = int(lines[i+1].strip())
            except:
                i += 1
                continue
            if chunk_size != size:
                i += 2
                continue
            # Found it. Now read 310 lines
            hex_parts = []
            j = i + 2
            while j < len(lines):
                if lines[j].strip() == '310':
                    hex_parts.append(lines[j+1].strip())
                    j += 2
                else:
                    break
            results.append(bytes.fromhex(''.join(hex_parts)))
            i = j
        else:
            i += 1
    return results


if __name__ == '__main__':
    path = sys.argv[1]
    size = int(sys.argv[2])
    bins = extract_binary(path, size)
    print(f"Found {len(bins)} binary chunks of size {size}")
    for idx, b in enumerate(bins):
        print(f"--- Chunk {idx} (len={len(b)}) ---")
        outpath = path.replace('.dxf', f'.proxy{idx}.bin')
        with open(outpath, 'wb') as f:
            f.write(b)
        print(f"Written to {outpath}")
