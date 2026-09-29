#!/usr/bin/env python3
"""issue #1446: генерация синтетических DXF для теста этапа 2 NOD
(cad_source/zengine/tests/nodstage2.lpr).

В отличие от файлов этапа 1 (только секция OBJECTS), здесь полные файлы
с HEADER ($ACADVER), ENTITIES и OBJECTS — они загружаются через AddFromDXF:

* nod_load_r12.dxf — $ACADVER=AC1009 и (нестандартная для R12) корректная
  секция OBJECTS с NOD: pre-pass для R12 не вызывается, модель пустая;
* nod_load_broken_objects.dxf — $ACADVER=AC1015, секция OBJECTS с
  нарушенной структурой: загрузка не прерывается (отрезок загружается),
  модель пустая;
* nod_load_no_objects.dxf — $ACADVER=AC1015 без секции OBJECTS: модель
  пустая, ошибки нет.

Использование: experiments/issue1446/gen_nod_load_testdata.py
"""
import os

ROOT = os.path.normpath(os.path.join(os.path.dirname(__file__), '..', '..'))
OUT = os.path.join(ROOT, 'cad_source', 'zengine', 'tests', 'data', 'nod')

HEADER = [('0', 'SECTION'), ('2', 'HEADER'), ('9', '$ACADVER'), ('1', None),
          ('0', 'ENDSEC')]

ENTITIES = [('0', 'SECTION'), ('2', 'ENTITIES'),
            ('0', 'LINE'), ('5', '20'), ('8', '0'),
            ('10', '0.0'), ('20', '0.0'), ('30', '0.0'),
            ('11', '100.0'), ('21', '50.0'), ('31', '0.0'),
            ('0', 'ENDSEC')]

OBJECTS_OK = [('0', 'SECTION'), ('2', 'OBJECTS'),
              ('0', 'DICTIONARY'), ('5', 'C'), ('330', '0'),
              ('100', 'AcDbDictionary'), ('281', '1'),
              ('3', 'ACAD_GROUP'), ('350', 'D'),
              ('0', 'DICTIONARY'), ('5', 'D'),
              ('102', '{ACAD_REACTORS'), ('330', 'C'), ('102', '}'),
              ('330', 'C'), ('100', 'AcDbDictionary'), ('281', '1'),
              ('0', 'ENDSEC')]

# Код группы 99999999999999999999 не помещается даже в Int64: ParseDxfObjectsSection
# возвращает False ("invalid group code"), а основной читатель DXF
# (TZMemReader.ParseInteger2) читает его без исключения как неизвестный код
# и продолжает загрузку. Нечисловой код (например, 'XYZ') для такого теста
# не годится: на нём падает и основной читатель.
OBJECTS_BROKEN = [('0', 'SECTION'), ('2', 'OBJECTS'),
                  ('0', 'DICTIONARY'), ('5', 'C'), ('330', '0'),
                  ('100', 'AcDbDictionary'), ('3', 'ACAD_GROUP'), ('350', 'D'),
                  ('0', 'DICTIONARY'), ('5', 'D'), ('330', 'C'),
                  ('99999999999999999999', 'broken'),
                  ('0', 'ENDSEC')]


def write(name, acadver, sections):
    pairs = []
    for sec in sections:
        for code, value in sec:
            pairs.append((code, acadver if value is None else value))
    pairs.append(('0', 'EOF'))
    with open(os.path.join(OUT, name), 'w', newline='\n') as f:
        for code, value in pairs:
            f.write('{:>3}\n{}\n'.format(code, value))


def main():
    write('nod_load_r12.dxf', 'AC1009', [HEADER, ENTITIES, OBJECTS_OK])
    write('nod_load_broken_objects.dxf', 'AC1015',
          [HEADER, ENTITIES, OBJECTS_BROKEN])
    write('nod_load_no_objects.dxf', 'AC1015', [HEADER, ENTITIES])


if __name__ == '__main__':
    main()
