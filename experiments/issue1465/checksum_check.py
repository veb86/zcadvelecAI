"""Проверка формулы ACAD_ROUNDTRIP_2008_CELL_CHECKSUM по эталонному DXF.

Для каждой ячейки TABLECONTENT берётся текст (302 после CELLCONTENT/ACVALUE)
и ближайшая следующая контрольная сумма 140 из DATAMAP. Сравниваются две
формулы: простая сумма кодов и сумма код*позиция (1-based).
Использование: python3 checksum_check.py file.dxf
"""
import sys

lines = open(sys.argv[1], encoding='utf-8', errors='replace').read().splitlines()
pairs = [(lines[i].strip(), lines[i + 1].rstrip('\r')) for i in range(0, len(lines) - 1, 2)]
SKIP = {'', 'GRIDFORMAT', 'CONTENT', 'CONTENTFORMAT'}
cells = []
for i, (c, v) in enumerate(pairs):
    if c == '300' and v == 'ACAD_ROUNDTRIP_2008_CELL_CHECKSUM':
        cs = next(float(v2) for c2, v2 in pairs[i + 1:i + 12] if c2 == '140')
        cells.append([cs, ''])
    elif c == '302' and v.strip() not in SKIP and cells and cells[-1][1] == '':
        cells[-1][1] = v
ok_w = ok_p = 0
for cs, t in cells:
    plain = sum(ord(ch) for ch in t)
    weighted = sum(ord(ch) * (k + 1) for k, ch in enumerate(t))
    ok_p += plain == cs
    ok_w += weighted == cs
    print('%-12r checksum=%-8s plain=%-6d weighted=%d' % (t, cs, plain, weighted))
print('cells', len(cells), 'plain ok', ok_p, 'weighted ok', ok_w)
