#!/usr/bin/env python3
"""Дамп дерева объектов, связанных с ACAD_TABLE (расширенный словарь,
round-trip XRECORD, TABLECONTENT, TABLESTYLE), для сравнения двух DXF.
Использование: dxf_objtree.py file.dxf > out.txt
Хэндлы заменяются на роли/порядковые номера, чтобы diff двух файлов
показывал только содержательные расхождения."""
import sys

def load(fn):
    L = open(fn, encoding='utf-8', errors='replace').read().split('\n')
    L = [l.rstrip('\r') for l in L]
    return [(L[i].strip(), L[i + 1]) for i in range(0, len(L) - 1, 2)]

def split_objs(p):
    res = []; cur = None
    for c, v in p:
        if c == '0':
            if cur: res.append(cur)
            cur = [(c, v)]
        elif cur is not None:
            cur.append((c, v))
    if cur: res.append(cur)
    return res

HCODES = set(['5', '105', '330', '340', '341', '342', '343', '344', '345', '346', '347', '348', '349', '360', '361', '331', '332', '390'])

def main(fn):
    objs = split_objs(load(fn))
    byh = {}
    for o in objs:
        for c, v in o[1:3]:
            if c in ('5', '105'):
                byh[v.strip().upper()] = o
    seen = set()
    def dump(h, depth, label):
        h = h.strip().upper()
        if h in seen or h not in byh or h == '0':
            print('  ' * depth + f'[{label}] -> {h} ' + ('(seen)' if h in seen else '(missing)'))
            return
        seen.add(h)
        o = byh[h]
        print('  ' * depth + f'[{label}] {o[0][1]}')
        children = []
        for c, v in o[1:]:
            if c in HCODES:
                print('  ' * (depth + 1) + f'{c:>4} <h>')
                if c in ('360',) or (o[0][1] == 'XRECORD' and False):
                    children.append((c, v))
            else:
                print('  ' * (depth + 1) + f'{c:>4} {v}')
        for c, v in children:
            dump(v, depth + 1, c)
    tables = [o for o in objs if o[0][1] == 'ACAD_TABLE']
    for t in tables:
        h = [v for c, v in t if c == '5'][0]
        print('##### ACAD_TABLE')
        for c, v in t:
            if c == '360':
                dump(v, 1, 'xdict')
        # TABLESTYLE
        for c, v in t:
            if c == '342':
                dump(v, 1, 'tablestyle')
    # TABLECONTENT objects anywhere
    for o in objs:
        if o[0][1] in ('TABLECONTENT', 'TABLESTYLE', 'CELLSTYLEMAP', 'TABLEGEOMETRY'):
            h = [v for c, v in o if c in ('5', '105')][0].upper()
            if h not in seen:
                dump(h, 0, 'loose')

main(sys.argv[1])
