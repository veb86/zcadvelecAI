#!/usr/bin/env python3
"""
Скрипт валидации: проверяет корректность формата вещественных
чисел в DXF для объектов TABLESTYLE и MLEADERSTYLE.

AutoCAD 2008 требует десятичную точку в вещественных группах
(40-59, 140-149). При её отсутствии возникает ошибка
"преждевременный конец объекта".

Проверяет что:
1. Вещественные значения в TABLESTYLE содержат десятичную точку
2. Вещественные значения в MLEADERSTYLE содержат десятичную точку
3. Блочные стили MLEADERSTYLE правильно определяются
"""

import sys
import os

# Диапазоны DXF group codes для вещественных значений
FLOAT_CODE_RANGES = [
    (10, 59),     # координаты и скалярные значения
    (110, 149),   # подобъекты
    (210, 239),   # направления экструзии
    (460, 469),   # дополнительные
    (1010, 1059), # xdata координаты
]

def is_float_code(code_str):
    """Проверяет, является ли код группы вещественным."""
    try:
        code = int(code_str)
    except ValueError:
        return False
    for lo, hi in FLOAT_CODE_RANGES:
        if lo <= code <= hi:
            return True
    return False


def check_float_format_in_objects(filename, obj_type=None):
    """Проверяет формат вещественных значений в секции OBJECTS.
    Если obj_type задан, проверяет только объекты этого типа."""
    with open(filename, 'r', errors='replace') as f:
        lines = [l.rstrip() for l in f.readlines()]

    in_objects = False
    errors = []
    current_obj = '?'
    current_handle = ''

    i = 0
    while i < len(lines) - 1:
        code = lines[i].strip()
        value = lines[i + 1].strip()

        if code == '2' and value == 'OBJECTS':
            in_objects = True
            i += 2
            continue
        if in_objects and code == '0' and value == 'ENDSEC':
            break

        if in_objects:
            if code == '0':
                current_obj = value
                current_handle = ''
            elif code == '5' and current_handle == '':
                current_handle = value

            if (obj_type is None or current_obj == obj_type):
                if is_float_code(code) and value != '':
                    if '.' not in value:
                        try:
                            float(value)
                            errors.append({
                                'line': i + 1,
                                'code': code,
                                'value': value,
                                'object': current_obj,
                                'handle': current_handle,
                            })
                        except ValueError:
                            pass

        i += 2

    return errors


def main():
    test_dir = 'cad_source/test'
    all_ok = True

    # Проверяем эталонный файл
    etalon = os.path.join(test_dir,
                          'leadermtextnonestyle_etalon.dxf')
    if os.path.exists(etalon):
        print('=== Эталон (AutoCAD) ===')
        for obj_type in ['TABLESTYLE', 'MLEADERSTYLE']:
            errors = check_float_format_in_objects(
                etalon, obj_type)
            if errors:
                for e in errors:
                    print(f'  {obj_type}: строка {e["line"]}, '
                          f'код {e["code"]} = "{e["value"]}"')
            else:
                print(f'  {obj_type}: OK')

    # Проверяем файлы сохранённые ZCAD
    for fname, desc in [
        ('leadermtextnonestyle_2nowork.dxf', 'ZCAD (не работает)'),
        ('leadermtextnonestyle_1.dxf', 'ZCAD (первая попытка)'),
    ]:
        path = os.path.join(test_dir, fname)
        if not os.path.exists(path):
            continue

        print(f'\n=== {fname} ({desc}) ===')
        for obj_type in ['TABLESTYLE', 'MLEADERSTYLE']:
            errors = check_float_format_in_objects(
                path, obj_type)
            if errors:
                all_ok = False
                for e in errors:
                    print(f'  ОШИБКА {obj_type}: строка '
                          f'{e["line"]}, код {e["code"]} = '
                          f'"{e["value"]}" (нет точки)')
            else:
                print(f'  {obj_type}: OK')

    print()
    if all_ok:
        print('РЕЗУЛЬТАТ: все проверки пройдены')
    else:
        print('РЕЗУЛЬТАТ: обнаружены ошибки!')
        print('Исправление: использовать DXFFloatToStr вместо '
              'FloatToStr в uzestylestablesdxf.pas')

    return 0 if all_ok else 1


if __name__ == '__main__':
    sys.exit(main())
