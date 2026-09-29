#!/usr/bin/env python3
"""issue #1442: печатает стили таблиц DXF-файла: записи словаря ACAD_TABLESTYLE
(имя -> хэндл) и объекты TABLESTYLE (хэндл, описание группы 3)."""
import sys

def pairs(path):
    lines = open(path, encoding='utf-8', errors='replace').read().splitlines()
    for i in range(0, len(lines) - 1, 2):
        yield lines[i].strip(), lines[i + 1].strip()

def objects(path):
    obj = None
    for code, val in pairs(path):
        if code == '0':
            if obj:
                yield obj
            obj = [(code, val)]
        elif obj is not None:
            obj.append((code, val))
    if obj:
        yield obj

for path in sys.argv[1:]:
    objs = list(objects(path))
    nod_ts = None
    for o in objs:
        if o[0][1] == 'DICTIONARY':
            for (c, v), (c2, v2) in zip(o, o[1:]):
                if c == '3' and v == 'ACAD_TABLESTYLE' and c2 == '350':
                    nod_ts = v2
    print('==', path, 'ACAD_TABLESTYLE ->', nod_ts)
    for o in objs:
        h = dict(o).get('5')
        if o[0][1] == 'DICTIONARY' and h == nod_ts:
            for (c, v), (c2, v2) in zip(o, o[1:]):
                if c == '3':
                    print('  entry', v, '->', v2)
        if o[0][1] == 'TABLESTYLE':
            print('  TABLESTYLE', h, dict(o).get('3'))
