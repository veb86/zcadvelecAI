#!/usr/bin/env python3
"""Проверка DXF после сохранения (этап 4 ТЗ NOD, issue #1450):
уникальность хэндлов, разрешимость ссылок, словарь ACAD_TABLESTYLE в NOD,
число TABLESTYLE, ссылки 342 ACAD_TABLE на стили.

usage: dxfcheck.py file.dxf ...
"""
import sys
sys.path.insert(0, __import__('os').path.dirname(__file__))
from dxfcanon import pairs, split, objects, handle_of, REF

rc = 0
for path in sys.argv[1:]:
    ps = pairs(path)
    defs, dup, refs = set(), [], []
    sec = None
    for i, (c, v) in enumerate(ps):
        if c == 0 and v == 'SECTION':
            sec = ps[i + 1][1]
        if sec == 'HEADER':
            continue
        if c in (5, 105):
            if v.upper() in defs:
                dup.append(v)
            defs.add(v.upper())
        elif c in REF and v not in ('0', ''):
            refs.append((c, v.upper()))
    bad = sorted(set('%d:%s' % r for r in refs if r[1] not in defs))
    objs = []
    for n, s in split(ps):
        if n == 'OBJECTS':
            objs = objects(s)
    nod = objs[0] if objs else []
    keys = [v for c, v in nod if c == 3]
    tsdict = None
    for i, (c, v) in enumerate(nod):
        if c == 3 and v == 'ACAD_TABLESTYLE':
            tsdict = nod[i + 1][1].upper()
    styles = [o for o in objs if o[0][1] == 'TABLESTYLE']
    tsd = [o for o in objs if handle_of(o) == tsdict]
    entries = [v for c, v in (tsd[0] if tsd else []) if c == 3]
    ok = not dup and not bad and (tsdict is None or tsd)
    rc |= 0 if ok else 1
    print('%s: %s' % (path, 'OK' if ok else 'FAIL'))
    print('  handles=%d dup=%s unresolved=%s' % (len(defs), dup[:5], bad[:10]))
    print('  NOD keys=%s' % keys)
    print('  ACAD_TABLESTYLE=%s entries=%s TABLESTYLE objects=%d' %
          (tsdict, entries, len(styles)))
    print('  NOD keys sorted: %s' % (keys == sorted(keys, key=str.upper)))
sys.exit(rc)
