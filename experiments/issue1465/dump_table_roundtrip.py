#!/usr/bin/env python3
"""Эксперимент issue #1465: выводит XRECORD'ы ACAD_ROUNDTRIP_2008_TABLE_ENTITY
(и приватный маркер ZCAD), запись ACDB_RECOMPOSE_DATA корневого словаря и
владельцев (xdict -> сущность) для каждой DXF из аргументов."""
import sys

MARKERS = ('ACAD_ROUNDTRIP_2008_TABLE_ENTITY', 'ZCAD_SPLIT_TABLE_ENTITY')


def read_pairs(path):
    with open(path, encoding='utf-8', errors='replace') as f:
        lines = [l.rstrip('\r\n') for l in f]
    return [(int(lines[i].strip()), lines[i + 1].strip())
            for i in range(0, len(lines) - 1, 2)]


def split_objects(pairs):
    objs, cur, in_obj = [], None, False
    for code, val in pairs:
        if code == 0 and val == 'SECTION':
            in_obj = False
        if code == 2 and val == 'OBJECTS':
            in_obj = True
            continue
        if not in_obj:
            continue
        if code == 0:
            cur = [(code, val)]
            objs.append(cur)
        elif cur is not None:
            cur.append((code, val))
    return objs


def handle(o):
    return next((v for c, v in o if c == 5), '')


def owner(o):
    depth = 0
    for c, v in o:
        if c == 102:
            depth += 1 if v.startswith('{') else -1
        elif c == 330 and depth == 0:
            return v
    return ''


def main():
    for path in sys.argv[1:]:
        objs = split_objects(read_pairs(path))
        by_h = {handle(o): o for o in objs}
        print('#####', path)
        for o in objs:
            if o[0][1] == 'DICTIONARY' and owner(o) == '0':
                for i, (c, v) in enumerate(o):
                    if c == 3 and v == 'ACDB_RECOMPOSE_DATA':
                        rec = by_h.get(o[i + 1][1])
                        print('  NOD ACDB_RECOMPOSE_DATA ->', rec)
        for o in objs:
            if o[0][1] != 'XRECORD':
                continue
            m = [v for c, v in o if c == 102 and v in MARKERS]
            if not m:
                continue
            dic = by_h.get(owner(o))
            ent = owner(dic) if dic else '?'
            tail = o[[c for c, _ in o].index(102, 3) if False else 0:]
            start = next(i for i, (c, v) in enumerate(o) if v in MARKERS)
            print('  XRECORD', handle(o), 'owner', owner(o), 'entity', ent,
                  ' '.join('%d:%s' % p for p in o[start:]))


if __name__ == '__main__':
    main()
