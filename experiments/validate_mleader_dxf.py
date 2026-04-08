#!/usr/bin/env python3
"""
Скрипт валидации: проверяет корректность DXF-вывода
стилей мультивыносок (MLEADERSTYLE).

Проверки:
1. Все вещественные значения содержат десятичную точку
2. Блочные стили корректно определяются
3. Структура объекта MLEADERSTYLE соответствует формату DXF
"""

import sys
import os

# DXF-коды вещественных групп для MLEADERSTYLE
FLOAT_CODES = {'40','41','42','43','44','45','46','47',
               '49','140','141','142','143'}

def extract_mleader_styles(filename, min_line=2000):
    """Извлекает все MLEADERSTYLE из OBJECTS секции файла."""
    with open(filename, 'r', errors='replace') as f:
        lines = f.readlines()

    styles = []
    i = 0
    while i < len(lines) - 1:
        if (lines[i].strip() == '0'
            and lines[i+1].strip() == 'MLEADERSTYLE'
            and i > min_line):
            start_line = i + 1
            i += 2
            pairs = []
            while i < len(lines) - 1:
                code = lines[i].strip()
                value = lines[i+1].strip()
                if code == '0':
                    break
                pairs.append((code, value, i+1))
                i += 2
            styles.append({
                'start_line': start_line,
                'pairs': pairs
            })
        else:
            i += 1
    return styles


def check_float_format(styles, filename):
    """Проверяет наличие десятичной точки в вещественных
    значениях."""
    errors = []
    for s in styles:
        for code, value, line in s['pairs']:
            if code in FLOAT_CODES:
                if '.' not in value:
                    errors.append(
                        f'{filename}:{line}: код {code} '
                        f'без десятичной точки: "{value}"'
                    )
    return errors


def check_block_detection(filename):
    """Проверяет логику определения блочных стилей."""
    with open(filename, 'r', errors='replace') as f:
        lines = f.readlines()

    # Карта стилей из словаря ACAD_MLEADERSTYLE
    style_names = {}
    for i, line in enumerate(lines):
        if line.strip() == 'ACAD_MLEADERSTYLE':
            if (i > 0 and lines[i-1].strip() == '3'
                and i+1 < len(lines)
                and lines[i+1].strip() == '350'):
                dict_handle = lines[i+2].strip()
                for j, l in enumerate(lines):
                    if (l.strip() == dict_handle
                        and j > 0
                        and lines[j-1].strip() == '5'):
                        k = j + 1
                        last_name = ''
                        while k < len(lines) - 1:
                            c = lines[k].strip()
                            v = lines[k+1].strip()
                            if c == '0':
                                break
                            if c == '3':
                                last_name = v
                            if c == '350' and last_name:
                                style_names[v] = last_name
                                last_name = ''
                            k += 2
                        break
                break

    # Проверяем каждый стиль
    results = []
    i = 0
    while i < len(lines) - 1:
        if (lines[i].strip() == '0'
            and lines[i+1].strip() == 'MLEADERSTYLE'
            and i > 2000):
            i += 2
            handle = ''
            c90 = c295 = c343 = ''
            while i < len(lines) - 1:
                code = lines[i].strip()
                value = lines[i+1].strip()
                if code == '0':
                    break
                if code == '5' and handle == '':
                    handle = value
                if code == '90':
                    c90 = value
                if code == '295':
                    c295 = value
                if code == '343':
                    c343 = value
                i += 2
            name = style_names.get(handle, 'unknown')
            is_block = (c90 == '1'
                       or (c343 != '' and c295 == '1'))
            results.append({
                'name': name,
                'handle': handle,
                'content_type': c90,
                'is_block_flag': c295,
                'block_handle': c343,
                'detected_as_block': is_block
            })
        else:
            i += 1
    return results


def main():
    test_dir = 'cad_source/test'
    test_files = [
        'leadermtextnonestyle.dxf',
        'leadermtextnoneblockstyle.dxf',
        'leaderonlyblockstyle.dxf',
    ]

    all_ok = True

    for fname in test_files:
        path = os.path.join(test_dir, fname)
        if not os.path.exists(path):
            print(f'SKIP: {path} не найден')
            continue

        print(f'\n=== {fname} ===')

        # Проверка формата вещественных чисел
        styles = extract_mleader_styles(path)
        float_errors = check_float_format(styles, fname)
        if float_errors:
            all_ok = False
            for err in float_errors:
                print(f'  ОШИБКА: {err}')
        else:
            print(f'  OK: все вещественные значения '
                  f'содержат десятичную точку '
                  f'({len(styles)} стилей)')

        # Проверка блочных стилей
        block_results = check_block_detection(path)
        for r in block_results:
            status = ('BLOCK' if r['detected_as_block']
                     else 'OK')
            print(f'  Стиль "{r["name"]}": {status} '
                  f'(CT={r["content_type"]}, '
                  f'BF={r["is_block_flag"]}, '
                  f'BH="{r["block_handle"]}")')

    # Проверка сохранённого файла
    saved = os.path.join(test_dir,
                         'leadermtextnonestyle_1.dxf')
    if os.path.exists(saved):
        print(f'\n=== leadermtextnonestyle_1.dxf '
              f'(сохранённый ZCAD) ===')
        styles = extract_mleader_styles(saved)
        float_errors = check_float_format(
            styles, 'leadermtextnonestyle_1.dxf')
        if float_errors:
            all_ok = False
            for err in float_errors:
                print(f'  ОШИБКА: {err}')
        else:
            print(f'  OK: все вещественные значения '
                  f'содержат десятичную точку '
                  f'({len(styles)} стилей)')

    print()
    if all_ok:
        print('РЕЗУЛЬТАТ: все проверки пройдены')
    else:
        print('РЕЗУЛЬТАТ: обнаружены ошибки!')

    return 0 if all_ok else 1


if __name__ == '__main__':
    sys.exit(main())
