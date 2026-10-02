#!/usr/bin/env python3
"""Проверка записи ACDB_RECOMPOSE_DATA и round-trip XRECORD таблиц в DXF
(issue #1465): ссылки 330 должны вести на TABLESTYLE и ACAD_TABLE."""
import sys

def pairs(path):
    with open(path, encoding='utf-8', errors='replace') as f:
        lines = [l.rstrip('\r\n') for l in f]
    return [(lines[i].strip(), lines[i + 1].strip())
            for i in range(0, len(lines) - 1, 2)]

def objects(prs):
    objs, cur = [], None
    for c, v in prs:
        if c == '0':
            cur = [v, []]
            objs.append(cur)
        elif cur is not None:
            cur[1].append((c, v))
    return objs

def main(path):
    objs = objects(pairs(path))
    by_handle = {}
    for t, body in objs:
        for c, v in body:
            if c == '5':
                by_handle[v.upper()] = (t, body)
                break
    rec = None
    for t, body in objs:
        if t == 'DICTIONARY':
            for i, (c, v) in enumerate(body):
                if c == '3' and v == 'ACDB_RECOMPOSE_DATA':
                    rec = body[i + 1][1].upper()
    if rec is None:
        print('NO RECOMPOSE')
        return 1
    t, body = by_handle[rec]
    refs = [v for c, v in body if c == '330'][2:]
    kinds = [by_handle.get(r.upper(), ('?',))[0] for r in refs]
    print('RECOMPOSE', rec, t, list(zip(refs, kinds)))
    bad = [k for k in kinds if k not in ('TABLESTYLE', 'ACAD_TABLE')]
    tables = sum(1 for t, _ in objs if t == 'ACAD_TABLE')
    print('ACAD_TABLE entities:', tables)
    return 1 if bad else 0

if __name__ == '__main__':
    sys.exit(main(sys.argv[1]))
