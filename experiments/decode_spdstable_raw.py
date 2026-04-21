#!/usr/bin/env python3
"""Decode SPDSTABLE2 proxy binary data from spdstable.dxf.

The DXF file contains a custom SPDSTABLE2 entity. zcad handles it as
an ACAD_PROXY_ENTITY (generic fallback). This script extracts the
raw 310-code binary data for the SPDSTABLE2 and parses it with the
ezdxf ProxyGraphic reader, so we can see what text heights ezdxf
reads for each individual text primitive."""
import os
import struct

import ezdxf
from ezdxf.proxygraphic import ProxyGraphic

DXF_PATH = os.path.join(
    os.path.dirname(os.path.abspath(__file__)),
    '..', 'cad_source', 'test', 'spdstable.dxf')


def extract_310_hex(path):
    """Return (entity_name, hex_string) for each entity with 310 codes."""
    with open(path, 'r', encoding='utf-8', errors='replace') as f:
        lines = [line.rstrip('\r\n') for line in f]

    entities = []
    i = 0
    while i < len(lines) - 1:
        if lines[i].strip() == '0':
            # Entity start
            entity_name = lines[i + 1].strip()
            j = i + 2
            hex_chunks = []
            ent_fields = {}
            while j < len(lines) - 1 and lines[j].strip() != '0':
                try:
                    code = int(lines[j].strip())
                except ValueError:
                    j += 1
                    continue
                value = lines[j + 1] if j + 1 < len(lines) else ''
                if code == 310:
                    hex_chunks.append(value.strip())
                elif code == 92:
                    ent_fields['92'] = value.strip()
                elif code == 160:
                    ent_fields['160'] = value.strip()
                j += 2
            if hex_chunks:
                entities.append((entity_name, ''.join(hex_chunks), ent_fields))
            i = j
        else:
            i += 1
    return entities


def main():
    entities = extract_310_hex(DXF_PATH)
    print(f'Entities with 310-binary: {len(entities)}')
    for idx, (name, hexstr, fields) in enumerate(entities):
        if name != 'SPDSTABLE2':
            continue
        try:
            data = bytes.fromhex(hexstr)
        except ValueError as e:
            print(f'  could not decode: {e}')
            continue
        print(f'\n#{idx} entity "{name}" fields={fields} size={len(data)} bytes')

        # ezdxf ProxyGraphic expects the first 8 bytes to be a header.
        # In ACAD_PROXY_ENTITY DXF representation, the 310 data IS the proxy
        # graphic section. But for SPDSTABLE2, the 310 data is the object-data
        # stream which may have a different prefix. Try a few offsets.
        for offset in (0, 4, 8, 12, 16, 20, 24, 28, 32):
            try:
                pg = ProxyGraphic(data[offset:])
                ents = list(pg.virtual_entities())
                if ents:
                    print(f'  offset {offset}: {len(ents)} virtual entities')
                    for e in ents[:20]:
                        dt = e.dxftype()
                        if dt in ('TEXT', 'MTEXT'):
                            h = float(e.dxf.get('height', 0) or 0)
                            try:
                                txt = e.dxf.get('text', '')
                            except Exception:
                                txt = ''
                            print(f'    {dt}: h={h:.4f} text={txt!r}')
                        else:
                            print(f'    {dt}')
                    break
            except Exception as e:
                pass
        else:
            print('  could not decode at any offset')


if __name__ == '__main__':
    main()
