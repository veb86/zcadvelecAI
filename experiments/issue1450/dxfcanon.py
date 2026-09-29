#!/usr/bin/env python3
"""Каноническая форма DXF для сравнения «с точностью до хэндлов и порядка»
(критерий приёмки этапа 4 ТЗ NOD, issue #1450).

Хэндлы переименовываются: сначала в порядке определения вне OBJECTS, затем
объекты OBJECTS — обходом от NOD (записи словарей по ключу, расширенные
словари, жёсткие ссылки 360), остальные — в порядке определения. Объекты
OBJECTS сортируются по каноническому хэндлу. Время и $HANDSEED
нормализуются.

usage: dxfcanon.py a.dxf b.dxf   -> печатает unified diff канонических форм
"""
import sys, difflib

REF = set([320, 330, 331, 332, 340, 341, 342, 343, 344, 345, 346, 347, 348,
           349, 350, 351, 352, 353, 354, 355, 356, 357, 358, 359, 360, 361,
           390, 391, 480, 481, 1005])


def pairs(path):
    lines = open(path, encoding='latin-1').read().replace('\r', '').split('\n')
    res = []
    for i in range(0, len(lines) - 1, 2):
        res.append((int(lines[i].strip()), lines[i + 1].strip()))
    return res


def split(ps):
    """[(section_name, [pairs])]"""
    secs, cur, name = [], None, None
    i = 0
    while i < len(ps):
        c, v = ps[i]
        if c == 0 and v == 'SECTION':
            name = ps[i + 1][1]
            cur = []
            i += 2
            continue
        if c == 0 and v == 'ENDSEC':
            secs.append((name, cur))
            cur = None
            i += 1
            continue
        if cur is not None:
            cur.append((c, v))
        i += 1
    return secs


def objects(sec):
    objs, cur = [], None
    for c, v in sec:
        if c == 0:
            cur = [(c, v)]
            objs.append(cur)
        elif cur is not None:
            cur.append((c, v))
    return objs


def handle_of(obj):
    for c, v in obj:
        if c in (5, 105):
            return v.upper()
    return None


def canon(path):
    secs = split(pairs(path))
    names = {}

    def name(h):
        h = h.upper()
        if h not in names:
            names[h] = 'H%04d' % len(names)

    for sname, sec in secs:
        if sname in ('OBJECTS', 'HEADER'):
            continue
        for c, v in sec:
            if c in (5, 105):
                name(v)
    objs = []
    for sname, sec in secs:
        if sname == 'OBJECTS':
            objs = objects(sec)
    byh = {handle_of(o): o for o in objs}
    queue = [handle_of(objs[0])] if objs else []
    while queue:
        h = queue.pop(0)
        if h is None or h in names:
            continue
        name(h)
        o = byh.get(h)
        if o is None:
            continue
        kids, key, inx = [], None, False
        for c, v in o:
            if c == 102:
                inx = v == '{ACAD_XDICTIONARY'
            elif c == 3:
                key = v
            elif c in (350, 360) and (key is not None or inx or c == 360):
                kids.append(((0 if inx else 1), key or '', v.upper()))
                key = None
        kids.sort()
        queue.extend(k[2] for k in kids)
    for o in objs:
        h = handle_of(o)
        if h:
            name(h)

    def sub(pairs_):
        out, prev = [], None
        for c, v in pairs_:
            if c in (5, 105) or c in REF:
                v = names.get(v.upper(), '?' + v)
            out.append('%d|%s' % (c, v))
        return out

    lines = []
    for sname, sec in secs:
        lines.append('SECTION ' + sname)
        if sname == 'HEADER':
            var = None
            for c, v in sec:
                if c == 9:
                    var = v
                elif var == '$HANDSEED' or var.startswith('$TD'):
                    v = '<norm>'
                elif c in REF:
                    v = names.get(v.upper(), '?' + v)
                lines.append('%d|%s' % (c, v))
        elif sname == 'OBJECTS':
            texts = []
            for o in objs:
                t = sub(o)
                texts.append((names.get(handle_of(o) or '', '~'), t))
            texts.sort()
            for _, t in texts:
                lines.extend(t)
        else:
            lines.extend(sub(sec))
    return lines


if __name__ == '__main__':
    a, b = canon(sys.argv[1]), canon(sys.argv[2])
    d = list(difflib.unified_diff(a, b, sys.argv[1], sys.argv[2], lineterm='', n=3))
    print('\n'.join(d))
    sys.exit(1 if d else 0)
