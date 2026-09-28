#!/usr/bin/env python3
"""issue #1444: генерация синтетических DXF для теста этапа 1 NOD
(cad_source/zengine/tests/nodstage1.lpr).

Файлы содержат только секцию OBJECTS (0/SECTION 2/OBJECTS ... 0/ENDSEC
0/EOF) — разборщик этапа 1 работает с текстом секции. Перевод строк
задаётся явно (\n или \r\n), поэтому файлы генерируются, а не пишутся
руками.

Использование: experiments/issue1444/gen_nod_testdata.py
"""
import os

ROOT = os.path.normpath(os.path.join(os.path.dirname(__file__), '..', '..'))
OUT = os.path.join(ROOT, 'cad_source', 'zengine', 'tests', 'data', 'nod')


def obj(objtype, handle, owner=None, reactors=(), xdict=None, body=(),
        owner_code='330'):
    """Пары объекта: 0/тип, 5/хэндл, реакторы, xdictionary, 330/владелец, тело."""
    p = [('0', objtype), ('5', handle)]
    if reactors:
        p.append(('102', '{ACAD_REACTORS'))
        p += [('330', r) for r in reactors]
        p.append(('102', '}'))
    if xdict:
        p += [('102', '{ACAD_XDICTIONARY'), ('360', xdict), ('102', '}')]
    if owner is not None:
        p.append((owner_code, owner))
    p += list(body)
    return p


def dictionary(handle, owner, entries, hard=None, cloning='1', code='350',
               reactors=None, xdict=None):
    body = [('100', 'AcDbDictionary')]
    if hard is not None:
        body.append(('280', hard))
    body.append(('281', cloning))
    for key, target in entries:
        body += [('3', key), (code, target)]
    if reactors is None:
        reactors = (owner,) if owner != '0' else ()
    return obj('DICTIONARY', handle, owner, reactors, xdict, body)


def tablestyle(handle, owner, descr='Standard'):
    return obj('TABLESTYLE', handle, owner, (owner,), None, [
        ('100', 'AcDbTableStyle'), ('3', descr), ('70', '0'), ('71', '0'),
        ('40', '1.5'), ('41', '1.5'), ('280', '0'), ('281', '0'),
        ('7', 'Standard'), ('140', '2.5'), ('170', '5'),
    ])


def xrecord(handle, owner, body):
    return obj('XRECORD', handle, owner, (owner,), None,
               [('100', 'AcDbXrecord'), ('280', '1')] + list(body))


def write(name, objects, eol='\n', code_fmt='{:>3}'):
    pairs = [('0', 'SECTION'), ('2', 'OBJECTS')]
    for o in objects:
        pairs += o
    pairs += [('0', 'ENDSEC'), ('0', 'EOF')]
    text = ''.join(code_fmt.format(c) + eol + v + eol for c, v in pairs)
    path = os.path.join(OUT, name)
    with open(path, 'w', newline='') as f:
        f.write(text)
    print('written', os.path.relpath(path, ROOT), len(objects), 'objects')


os.makedirs(OUT, exist_ok=True)

# 1. NOD не первый объект секции; до NOD стоит сторонний словарь с ключом
#    ACAD_TABLESTYLE (ловушка для глобального поиска 3/ACAD_TABLESTYLE).
write('nod_not_first.dxf', [
    dictionary('20', 'C', [('ACAD_TABLESTYLE', '21'), ('Other', '22')]),
    xrecord('21', '20', [('1', 'decoy')]),
    xrecord('22', '20', [('1', 'other')]),
    dictionary('C', '0', [('ACAD_GROUP', 'D'), ('ACAD_TABLESTYLE', '40'),
                          ('THIRD_PARTY', '20')]),
    dictionary('D', 'C', []),
    dictionary('40', 'C', [('Standard', '41'), ('ZCAD1444', '42')]),
    tablestyle('41', '40'),
    tablestyle('42', '40', 'ZCAD1444'),
])

# 2. NOD без ACAD_TABLESTYLE (как savetemplate2000.dxf); пустой словарь.
write('nod_no_tablestyle.dxf', [
    dictionary('C', '0', [('ACAD_GROUP', 'D'), ('ACAD_MLINESTYLE', '17')]),
    dictionary('D', 'C', []),
    dictionary('17', 'C', [('Standard', '18')]),
    obj('MLINESTYLE', '18', '17', ('17',), None,
        [('100', 'AcDbMlineStyle'), ('2', 'Standard'), ('70', '0')]),
])

# 3. Словари с 280=1 и записями 360 вместо 350; перевод строк \r\n,
#    пробелы вокруг кодов групп, ведущие нули и нижний регистр в хэндлах,
#    расширенный словарь у TABLESTYLE.
write('nod_hardowner_360.dxf', [
    dictionary('00c', '0', [('ACAD_TABLESTYLE', '0040'), ('ACAD_GROUP', 'd')],
               hard='1', code='360'),
    dictionary('D', 'C', []),
    dictionary('40', 'c', [('Standard', '4a')], hard='1', code='360'),
    obj('TABLESTYLE', '4A', '40', ('40',), '4B', [
        ('100', 'AcDbTableStyle'), ('3', 'Standard'), ('70', '0')]),
    dictionary('4B', '4A', [('ACAD_XREC_ROUNDTRIP', '4C')], hard='1',
               code='360'),
    xrecord('4C', '4B', [('102', 'ACAD_ROUNDTRIP_2008_TABLESTYLE'),
                         ('1', 'data')]),
], eol='\r\n', code_fmt=' {} ')

# 4. Неизвестные ключи: ACDB_RECOMPOSE_DATA, ветка плагина
#    MY_PLUGIN → DICTIONARY → XRECORD (в данных XRECORD — 330, 360 и
#    102-блок, которые не должны приниматься за владельца/реакторы),
#    битая ссылка, ключ без хэндла, ACDBDICTIONARYWDFLT.
write('nod_unknown_keys.dxf', [
    dictionary('C', '0', [('ACAD_PLOTSTYLENAME', 'E'),
                          ('ACAD_TABLESTYLE', '40'),
                          ('ACDB_RECOMPOSE_DATA', '60'),
                          ('BROKEN_LINK', 'FFF'),
                          ('MY_PLUGIN', '70')]),
    obj('ACDBDICTIONARYWDFLT', 'E', 'C', ('C',), None, [
        ('100', 'AcDbDictionary'), ('281', '1'), ('3', 'Normal'),
        ('350', 'F'), ('100', 'AcDbDictionaryWithDefault'), ('340', 'F')]),
    obj('ACDBPLACEHOLDER', 'F', 'E', ('E',)),
    dictionary('40', 'C', []),
    xrecord('60', 'C', [('90', '1'), ('330', '40')]),
    obj('DICTIONARY', '70', 'C', ('C',), None, [
        ('100', 'AcDbDictionary'), ('281', '1'),
        ('3', 'Settings'), ('350', '71'),
        ('3', 'NoHandle'),
        ('3', 'Data'), ('360', '72')]),
    xrecord('71', '70', [('1', 'plugin settings'), ('40', '3.5')]),
    xrecord('72', '70', [('330', '71'), ('360', '71'),
                         ('102', '{MY_PLUGIN_BLOCK'), ('330', 'C'),
                         ('102', '}'), ('1', 'end')]),
])

# 5. Несколько DICTIONARY с 330=0: NOD — первый, второй игнорируется.
write('nod_multiple_roots.dxf', [
    dictionary('C', '0', [('ACAD_TABLESTYLE', '40')]),
    dictionary('40', 'C', [('Standard', '41')]),
    tablestyle('41', '40'),
    dictionary('90', '0', [('ACAD_TABLESTYLE', '91')]),
    dictionary('91', '90', [('Fake', '92')]),
    tablestyle('92', '91', 'Fake'),
])

# 6. Битая структура: код группы не число (ошибка разбора).
with open(os.path.join(OUT, 'nod_broken.dxf'), 'w', newline='') as f:
    f.write('  0\nSECTION\n  2\nOBJECTS\n  0\nDICTIONARY\n  5\nC\n330\n0\n'
            '100\nAcDbDictionary\n  3\nACAD_GROUP\n350\nD\n  0\nDICTIONARY\n'
            '  5\nD\n330\nC\nXYZ\nbroken\n  0\nENDSEC\n  0\nEOF\n')
print('written cad_source/zengine/tests/data/nod/nod_broken.dxf')
