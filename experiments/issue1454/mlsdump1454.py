#!/usr/bin/env python3
"""issue #1454: независимый (без zengine) дамп стилей MLEADERSTYLE файла DXF
в формате эталонов cad_source/zengine/tests/data/nod/stage6/*.txt.

Порядок стилей — как в словаре ACAD_MLEADERSTYLE; значения по умолчанию —
как в TGDBDXFMLeaderStyle.init. Сравнение с эталоном:
  python3 experiments/issue1454/mlsdump1454.py cad_source/test/+mleader2008.dxf \
    | diff - cad_source/zengine/tests/data/nod/stage6/+mleader2008.txt
"""
import re
import sys


def pairs(fn):
    lines = open(fn, encoding='utf-8', errors='replace').read().splitlines()
    return [(int(lines[i].strip()), lines[i + 1]) for i in range(0, len(lines) - 1, 2)]


def records(P):
    """0/<тип> → список пар до следующего 0"""
    out = []
    for i, (c, v) in enumerate(P):
        if c == 0:
            out.append((v.strip(), []))
        elif out:
            out[-1][1].append((c, v))
    return out


def num(v):
    f = float(v)
    return str(int(f)) if f == int(f) else repr(f)


DEFAULTS = {170: '2', 171: '1', 172: '0', 173: '1', 40: '0', 41: '0', 91: '-1056964608',
            92: '-2', 290: '1', 42: '0.09', 291: '1', 43: '0.36', 45: '0.18', 90: '2',
            3: '', 300: '', 44: '0.18', 93: '-1056964608', 174: '1', 178: '1', 175: '1',
            176: '0', 177: '0', 292: '0', 297: '0', 294: '1', 46: '0.18', 94: '-1056964608',
            47: '1', 49: '1', 142: '1', 143: '0.125', 295: '0', 296: '0', 140: '1',
            293: '1', 141: '0'}
FLOATS = {40, 41, 42, 43, 44, 45, 46, 47, 49, 140, 141, 142, 143}
REFS = {340: 'LTYPE', 341: 'BLOCK_RECORD', 342: 'STYLE', 343: 'BLOCK_RECORD'}


def fmt(template, g):
    return re.sub(r'\{(\d+)\}', lambda m: g[int(m.group(1))], template)


def main(fn):
    recs = records(pairs(fn))
    by_handle = {}
    symbols = {}
    for typ, body in recs:
        h = next((v.strip().upper() for c, v in body if c in (5, 105)), None)
        if h is None:
            continue
        by_handle.setdefault(h, (typ, body))
        if typ in ('LTYPE', 'STYLE', 'BLOCK_RECORD'):
            symbols[h] = (typ, next((v.strip() for c, v in body if c == 2), ''))
    nod = next(body for typ, body in recs if typ == 'DICTIONARY')
    entries = [(nod[i][1].strip(), nod[i + 1][1].strip().upper())
               for i in range(len(nod) - 1) if nod[i][0] == 3]
    branch = dict(entries).get('ACAD_MLEADERSTYLE')
    if branch is None:
        return
    body = by_handle[branch][1]
    items = [(body[i][1], body[i + 1][1].strip().upper())
             for i in range(len(body) - 1) if body[i][0] == 3 and body[i + 1][0] in (350, 360)]
    for name, h in items:
        typ, obj = by_handle.get(h, ('', []))
        if typ != 'MLEADERSTYLE' or not name:
            continue
        g = dict(DEFAULTS)
        refs = {c: ':' for c in REFS}
        xdict, ver, in102, xdata, header, app = '', 2, False, False, True, ''
        for c, v in obj:
            s = v.strip()
            if c == 1001:
                xdata, app = True, s
                continue
            if xdata:
                if c == 1070 and app == 'ACAD_MLEADERVER':
                    ver = int(s)
                continue
            if c == 102:
                in102 = s.startswith('{') and s != '{}'
                continue
            if in102:
                if c == 360:
                    xdict = s.upper()
                continue
            if header:
                header = c != 100
                continue
            if c in REFS:
                sym = symbols.get(s.upper())
                refs[c] = s.upper() + ':' + (sym[1] if sym and sym[0] == REFS[c] else '')
            elif c in (3, 300):
                g[c] = v
            elif c in g:
                g[c] = num(s) if c in FLOATS else s
        print(f'{name} xdict={xdict} ver={ver}')
        print(fmt('  leader 170={170} 171={171} 172={172} 173={173} 40={40} 41={41} 91={91} 92={92}', g)
              + f' 340={refs[340]}')
        print(fmt('  landing 290={290} 42={42} 291={291} 43={43}', g) + f' 341={refs[341]}'
              + fmt(' 45={45}', g))
        print(fmt('  text 90={90} 3="{3}" 300="{300}"', g) + f' 342={refs[342]}'
              + fmt(' 44={44} 93={93} 174={174} 178={178} 175={175} 176={176} 177={177} 292={292} 297={297} 294={294}', g))
        print(fmt('  block 46={46}', g) + f' 343={refs[343]}'
              + fmt(' 94={94} 47={47} 49={49} 142={142} 143={143} 295={295} 296={296}', g))
        print(fmt('  scale 140={140} 293={293} 141={141}', g))


if __name__ == '__main__':
    for f in sys.argv[1:]:
        main(f)
