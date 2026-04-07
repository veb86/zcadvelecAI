#!/usr/bin/env python3
"""
Тест валидации ссылок MLEADERSTYLE в DXF файлах.

Проверяет, что все хэндлы ссылок (коды 340-343) в объектах
MLEADERSTYLE указывают на реально существующие объекты
нужного типа в DXF файле.

Этот тест моделирует проверку, которую выполняет AutoCAD
при открытии файла. Невалидные ссылки приводят к ошибке
открытия файла.
"""
import sys
import os


def parse_dxf_pairs(filename):
    """Разбирает DXF файл в пары (код, значение)."""
    with open(filename, 'r', errors='replace') as f:
        lines = f.readlines()
    pairs = []
    i = 0
    while i < len(lines) - 1:
        code = lines[i].strip()
        val = lines[i + 1].strip()
        pairs.append((code, val))
        i += 2
    return pairs


def build_handle_map(pairs):
    """Строит карту хэндл → тип объекта."""
    handle_map = {}
    current_type = None
    for code, val in pairs:
        if code == '0':
            current_type = val
        elif code == '5' and current_type:
            handle_map[val.upper()] = current_type
    return handle_map


def extract_mleaderstyle_refs(pairs):
    """Извлекает ссылки из всех объектов MLEADERSTYLE."""
    styles = []
    in_mleaderstyle = False
    current = None

    for code, val in pairs:
        if code == '0':
            if in_mleaderstyle and current:
                styles.append(current)
            if val == 'MLEADERSTYLE':
                in_mleaderstyle = True
                current = {
                    'name': '',
                    'handle': '',
                    'refs': {}
                }
            else:
                in_mleaderstyle = False
            continue

        if in_mleaderstyle and current:
            if code == '5' and not current['handle']:
                current['handle'] = val.upper()
            elif code == '3':
                current['name'] = val
            elif code == '340':
                current['refs']['340_ltype'] = val.upper()
            elif code == '341':
                current['refs']['341_arrow'] = val.upper()
            elif code == '342':
                current['refs']['342_style'] = val.upper()
            elif code == '343':
                current['refs']['343_block'] = val.upper()

    if in_mleaderstyle and current:
        styles.append(current)

    return styles


# Ожидаемые типы объектов для каждого кода ссылки
EXPECTED_TYPES = {
    '340_ltype': 'LTYPE',
    '341_arrow': 'BLOCK_RECORD',
    '342_style': 'STYLE',
    '343_block': 'BLOCK_RECORD',
}


def validate_file(filename):
    """Валидирует ссылки MLEADERSTYLE в DXF файле."""
    print(f'\n=== Проверка: {filename} ===')

    if not os.path.exists(filename):
        print(f'  ОШИБКА: файл не найден')
        return False

    pairs = parse_dxf_pairs(filename)
    handle_map = build_handle_map(pairs)
    styles = extract_mleaderstyle_refs(pairs)

    if not styles:
        print('  Нет объектов MLEADERSTYLE')
        return True

    print(f'  Найдено {len(styles)} стилей MLEADERSTYLE')
    all_valid = True

    for style in styles:
        name = style['name'] or style['handle']
        print(f'\n  Стиль: {name} (хэндл: {style["handle"]})')

        for ref_key, ref_handle in style['refs'].items():
            expected_type = EXPECTED_TYPES.get(ref_key, '?')
            code = ref_key.split('_')[0]

            if ref_handle == '0':
                print(f'    код {code}: нулевой хэндл (допустимо)')
                continue

            actual_type = handle_map.get(ref_handle)
            if actual_type is None:
                print(f'    код {code}: ОШИБКА — хэндл {ref_handle}'
                      f' не существует'
                      f' (ожидался {expected_type})')
                all_valid = False
            elif actual_type != expected_type:
                print(f'    код {code}: ОШИБКА — хэндл {ref_handle}'
                      f' → {actual_type}'
                      f' (ожидался {expected_type})')
                all_valid = False
            else:
                print(f'    код {code}: OK — хэндл {ref_handle}'
                      f' → {actual_type}')

    return all_valid


def main():
    test_files = [
        'cad_source/test/+mleaderstyle2008.dxf',
        'cad_source/test/+mleaderstyle2008_1.dxf',
    ]

    results = {}
    for f in test_files:
        results[f] = validate_file(f)

    print('\n=== Итог ===')
    all_pass = True
    for f, ok in results.items():
        status = 'PASS' if ok else 'FAIL'
        print(f'  {status}: {f}')
        if not ok:
            all_pass = False

    sys.exit(0 if all_pass else 1)


if __name__ == '__main__':
    main()
