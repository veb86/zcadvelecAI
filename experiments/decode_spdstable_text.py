#!/usr/bin/env python3
"""Decode proxy graphic from spdstable.dxf and list all text primitives
with their heights. Used to verify that the fix for issue #978 correctly
extracts text height from individual primitives (not from container).
"""
import os
import sys

import ezdxf
from ezdxf.proxygraphic import ProxyGraphic

DXF_PATH = os.path.join(
    os.path.dirname(os.path.abspath(__file__)),
    '..', 'cad_source', 'test', 'spdstable.dxf')


def main():
    doc = ezdxf.readfile(DXF_PATH)
    print(f'DXF version: {doc.dxfversion}')

    proxy_entities = []
    for e in doc.modelspace():
        if e.dxftype() == 'ACAD_PROXY_ENTITY':
            proxy_entities.append(e)

    print(f'Number of ACAD_PROXY_ENTITY: {len(proxy_entities)}')

    for idx, proxy in enumerate(proxy_entities):
        print(f'\n--- ProxyEntity #{idx} handle={proxy.dxf.handle} ---')
        if not proxy.proxy_graphic:
            print('No proxy graphic data')
            continue

        pg = ProxyGraphic(proxy.proxy_graphic, doc)
        text_heights = set()
        n_text = 0
        for e in pg.virtual_entities():
            if e.dxftype() in ('TEXT', 'MTEXT'):
                n_text += 1
                h = float(e.dxf.get('height', 0) or 0)
                text_heights.add(round(h, 4))

        print(f'Number of text entities in proxy: {n_text}')
        print(f'Distinct heights: {sorted(text_heights)}')


if __name__ == '__main__':
    main()
