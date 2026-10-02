#!/usr/bin/env python3
"""Печатает для каждой ACAD_TABLE: handle, xdict, вставку, 91/92, 141, а для
TABLECONTENT — число строк/столбцов и типы строк (TABLEROW_BEGIN -> 90).
Использование: python3 table_rows.py file.dxf"""
import sys

def pairs(path):
    with open(path, encoding='utf-8', errors='replace') as f:
        lines = f.read().splitlines()
    for i in range(0, len(lines) - 1, 2):
        yield int(lines[i].strip()), lines[i + 1].strip()

def objects(path):
    cur = None
    for c, v in pairs(path):
        if c == 0:
            if cur:
                yield cur
            cur = [(c, v)]
        elif cur is not None:
            cur.append((c, v))
    if cur:
        yield cur

for path in sys.argv[1:]:
    print('#####', path)
    for o in objects(path):
        if o[0][1] == 'ACAD_TABLE':
            d = dict()
            for c, v in o:
                d.setdefault(c, []).append(v)
            i = [k for k, (c, v) in enumerate(o) if c == 100 and v == 'AcDbTable']
            t = o[i[0]:] if i else []
            g = lambda code: [v for c, v in t if c == code]
            xd = [o[k + 1][1] for k, (c, v) in enumerate(o)
                  if c == 102 and v == '{ACAD_XDICTIONARY']
            print(' TABLE', d[5][0], 'xdict', xd, 'ins', d.get(10, ['?'])[0],
                  d.get(20, ['?'])[0], '90', g(90)[:1], 'rows', g(91), 'cols', g(92),
                  'h141', [round(float(x), 3) for x in g(141)])
        elif o[0][1] == 'TABLECONTENT':
            rt = []
            for k, (c, v) in enumerate(o):
                if c == 1 and v == 'TABLEROW_BEGIN':
                    for c2, v2 in o[k + 1:]:
                        if c2 == 90:
                            rt.append(int(v2))
                            break
            h = [v for c, v in o if c == 5][0]
            print(' TABLECONTENT', h, 'rows', len(rt), 'rowtypes', rt)
