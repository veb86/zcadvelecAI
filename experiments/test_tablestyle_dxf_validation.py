#!/usr/bin/env python3
"""
Тест для проверки корректности DXF-файлов со стилями таблиц.
Проверяет типичные проблемы, вызывающие отказ AutoCAD:
1. Дублирование хэндлов (eHandleInUse)
2. Наличие AcDbObject перед AcDbTableStyle (повреждённый подкласс)
3. Форматирование вещественных чисел (группа 140 без десятичной точки)
4. Целостность ссылок словаря ACAD_TABLESTYLE
5. Версия $ACADVER при наличии TABLESTYLE
"""

import sys
import os

def parse_dxf_lines(filename):
    """Читает DXF и возвращает список строк."""
    with open(filename, 'r', encoding='cp1251', errors='replace') as f:
        return [l.rstrip('\n') for l in f.readlines()]

def check_duplicate_handles(lines):
    """Проверяет дублирование хэндлов (группа 5 после группы 0)."""
    errors = []
    handles = {}
    i = 0
    while i < len(lines) - 3:
        code = lines[i].strip()
        if code == '0':
            obj_type = lines[i+1].strip()
            code5 = lines[i+2].strip()
            if code5 == '5':
                handle = lines[i+3].strip().upper()
                if handle in handles:
                    prev = handles[handle]
                    errors.append(
                        f'Дубликат хэндла {handle}: '
                        f'{prev[1]} (строка {prev[0]}) и '
                        f'{obj_type} (строка {i+1})')
                else:
                    handles[handle] = (i+1, obj_type)
        i += 2
    return errors

def check_acdbobject_before_tablestyle(lines):
    """Проверяет наличие лишнего AcDbObject перед AcDbTableStyle."""
    errors = []
    in_ts = False
    ts_line = 0
    i = 0
    while i < len(lines) - 1:
        code = lines[i].strip()
        val = lines[i+1].strip()
        if code == '0' and val == 'TABLESTYLE':
            in_ts = True
            ts_line = i + 1
        elif in_ts:
            if code == '100' and val == 'AcDbObject':
                errors.append(
                    f'Строка {i+1}: AcDbObject перед AcDbTableStyle '
                    f'(TABLESTYLE начинается на строке {ts_line})')
            if code == '100' and val == 'AcDbTableStyle':
                in_ts = False
            if code == '0':
                in_ts = False
        i += 2
    return errors

def check_float_formatting(lines):
    """Проверяет что группа 140 (double) внутри TABLESTYLE содержит
    десятичную точку. Итерация по парам (код, значение)."""
    errors = []
    in_ts = False
    i = 0
    while i < len(lines) - 1:
        code = lines[i].strip()
        val = lines[i+1].strip()
        if code == '0' and val == 'TABLESTYLE':
            in_ts = True
        elif in_ts and code == '0':
            in_ts = False
        elif in_ts and code == '140':
            if '.' not in val and 'E' not in val.upper():
                errors.append(
                    f'Строка {i+2}: группа 140 значение "{val}" '
                    f'без десятичной точки (должно быть "{val}.0")')
        i += 2
    return errors

def check_dictionary_integrity(lines):
    """Проверяет целостность ссылок словаря ACAD_TABLESTYLE."""
    errors = []
    # Находим хэндлы TABLESTYLE объектов
    ts_handles = set()
    for i in range(len(lines) - 3):
        code = lines[i].strip()
        if code == '0' and lines[i+1].strip() == 'TABLESTYLE':
            code5 = lines[i+2].strip()
            if code5 == '5':
                ts_handles.add(lines[i+3].strip().upper())

    # Находим хэндл словаря ACAD_TABLESTYLE из корневого словаря
    # Корневой словарь содержит запись: 3 ACAD_TABLESTYLE / 350 <dict_handle>
    ts_dict_handle = None
    for i in range(len(lines) - 3):
        code = lines[i].strip()
        val = lines[i+1].strip()
        if code == '3' and val == 'ACAD_TABLESTYLE':
            next_code = lines[i+2].strip()
            next_val = lines[i+3].strip()
            if next_code == '350':
                ts_dict_handle = next_val.upper()
                break

    if ts_dict_handle is None:
        if ts_handles:
            errors.append('TABLESTYLE объекты найдены, но словарь '
                         'ACAD_TABLESTYLE отсутствует в корневом словаре')
        return errors

    # Ищем DICTIONARY с хэндлом ts_dict_handle и читаем его записи
    dict_refs = {}
    in_dict = False
    for i in range(0, len(lines) - 1, 2):
        code = lines[i].strip()
        val = lines[i+1].strip()
        if not in_dict:
            if code == '5' and val.upper() == ts_dict_handle:
                in_dict = True
        else:
            if code == '3':
                style_name = val
            elif code == '350' and in_dict:
                dict_refs[val.upper()] = style_name
            elif code == '0':
                break

    # Проверяем что все ссылки из словаря ведут на TABLESTYLE объекты
    for ref, name in dict_refs.items():
        if ref not in ts_handles:
            errors.append(
                f'Словарь ACAD_TABLESTYLE: стиль "{name}" -> хэндл {ref}, '
                f'но TABLESTYLE с таким хэндлом не найден')
    # Проверяем обратно — что все TABLESTYLE объекты есть в словаре
    for h in ts_handles:
        if h not in dict_refs:
            errors.append(
                f'TABLESTYLE с хэндлом {h} не найден в словаре ACAD_TABLESTYLE')
    return errors

def check_acadver(lines):
    """Проверяет $ACADVER при наличии TABLESTYLE."""
    errors = []
    has_tablestyle = False
    acadver = None
    for i in range(len(lines) - 1):
        code = lines[i].strip()
        val = lines[i+1].strip()
        if code == '0' and val == 'TABLESTYLE':
            has_tablestyle = True
        if code == '9' and val == '$ACADVER':
            if i + 3 < len(lines):
                acadver = lines[i+3].strip()
    if has_tablestyle and acadver:
        ver_num = int(acadver.replace('AC', ''))
        if ver_num < 1018:
            errors.append(
                f'$ACADVER={acadver} (< AC1018), но файл содержит '
                f'TABLESTYLE (требуется AC1018+)')
    return errors


def validate_dxf(filename):
    """Запускает все проверки для DXF файла."""
    print(f'\n{"="*60}')
    print(f'Проверка: {os.path.basename(filename)}')
    print(f'{"="*60}')

    lines = parse_dxf_lines(filename)
    all_ok = True

    checks = [
        ('Дублирование хэндлов', check_duplicate_handles),
        ('AcDbObject перед AcDbTableStyle', check_acdbobject_before_tablestyle),
        ('Формат вещественных чисел (группа 140)', check_float_formatting),
        ('Целостность словаря ACAD_TABLESTYLE', check_dictionary_integrity),
        ('Версия $ACADVER', check_acadver),
    ]

    for name, check_fn in checks:
        errors = check_fn(lines)
        if errors:
            all_ok = False
            print(f'\n  ОШИБКА: {name}')
            for err in errors:
                print(f'    - {err}')
        else:
            print(f'  OK: {name}')

    return all_ok


if __name__ == '__main__':
    base = '/tmp/gh-issue-solver-1775452752965/cad_source'
    files = [
        os.path.join(base, 'testtablestyle2007.dxf'),
        os.path.join(base, 'testtablestyle2007_1.dxf'),
        os.path.join(base, 'testtablestyle2007_2.dxf'),
        os.path.join(base, 'testtablestyle2007_4.dxf'),
    ]

    results = {}
    for f in files:
        if os.path.exists(f):
            results[f] = validate_dxf(f)

    print(f'\n{"="*60}')
    print('Итог:')
    for f, ok in results.items():
        status = 'PASS' if ok else 'FAIL'
        print(f'  {status}: {os.path.basename(f)}')
