#!/usr/bin/env python3
"""
Тест валидации MLEADERSTYLE ссылок в DXF файлах.
Проверяет, что все handle-ссылки (коды 340-343) в MLEADERSTYLE объектах
указывают на существующие объекты в файле.

Используется для проверки корректности экспорта DXF из ZCAD.
"""

import sys
import os


def parse_dxf_handles(lines):
    """Строит карту хэндл -> (тип_объекта, имя) из DXF файла."""
    handle_map = {}
    obj_type = ''
    obj_name = ''
    obj_handle = ''
    i = 0
    while i < len(lines) - 1:
        code = lines[i].strip()
        val = lines[i + 1].strip()
        if code == '0':
            if obj_handle:
                handle_map[obj_handle] = (obj_type, obj_name)
            obj_type = val
            obj_name = ''
            obj_handle = ''
        elif code == '5' and not obj_handle:
            obj_handle = val.upper()
        elif code == '2' and not obj_name:
            obj_name = val
        i += 2
    if obj_handle:
        handle_map[obj_handle] = (obj_type, obj_name)
    return handle_map


def extract_mleaderstyle_refs(lines):
    """Извлекает ссылки (коды 340-343) из MLEADERSTYLE объектов."""
    styles = []
    in_mleader = False
    current = None
    i = 0
    while i < len(lines) - 1:
        code = lines[i].strip()
        val = lines[i + 1].strip()
        if code == '0':
            if current is not None:
                styles.append(current)
                current = None
            if val == 'MLEADERSTYLE':
                in_mleader = True
                current = {
                    'handle': '', 'name': '', 'refs': {}
                }
            else:
                in_mleader = False
        if in_mleader and current is not None:
            if code == '5' and not current['handle']:
                current['handle'] = val.upper()
            elif code == '3':
                current['name'] = val
            elif code in ('340', '341', '342', '343'):
                current['refs'][int(code)] = val.upper()
        i += 2
    if current is not None:
        styles.append(current)
    return styles


# Ожидаемые типы объектов для каждого кода (по спецификации DXF)
EXPECTED_TYPES = {
    340: ('LTYPE', 'LeaderLineType ID'),
    341: ('BLOCK_RECORD', 'Arrowhead ID'),
    342: ('STYLE', 'mTextStyleId'),
    343: ('BLOCK_RECORD', 'Block Content ID'),
}


def validate_dxf_file(filepath):
    """Проверяет валидность ссылок MLEADERSTYLE в DXF файле."""
    with open(filepath, 'r', errors='replace') as f:
        lines = [l.rstrip() for l in f.readlines()]

    handle_map = parse_dxf_handles(lines)
    styles = extract_mleaderstyle_refs(lines)
    errors = []

    print(f'Файл: {filepath}')
    print(f'Всего хэндлов в файле: {len(handle_map)}')
    print(f'Найдено MLEADERSTYLE: {len(styles)}')

    for style in styles:
        print(
            f'\n  MLEADERSTYLE "{style["name"]}" '
            f'(handle {style["handle"]})'
        )
        for code, ref_handle in sorted(style['refs'].items()):
            expected_type, desc = EXPECTED_TYPES.get(
                code, ('?', '?')
            )
            target = handle_map.get(ref_handle)
            if target is None:
                msg = (
                    f'code {code} ({desc}) = {ref_handle} '
                    f'-> НЕ СУЩЕСТВУЕТ!'
                )
                print(f'    ОШИБКА: {msg}')
                errors.append(msg)
            else:
                actual_type, actual_name = target
                status = 'OK'
                if actual_type != expected_type:
                    status = (
                        f'НЕВЕРНЫЙ ТИП! '
                        f'Ожидался {expected_type}'
                    )
                    errors.append(
                        f'code {code} ({desc}) = {ref_handle} '
                        f'-> {actual_type} "{actual_name}" '
                        f'(ожидался {expected_type})'
                    )
                print(
                    f'    code {code} ({desc}) = {ref_handle} '
                    f'-> {actual_type} "{actual_name}" [{status}]'
                )

    return errors


if __name__ == '__main__':
    test_dir = os.path.join(
        os.path.dirname(os.path.dirname(os.path.abspath(__file__))),
        'cad_source', 'test'
    )

    files = [
        os.path.join(test_dir, '+mleaderstyle2008.dxf'),
        os.path.join(test_dir, '+mleaderstyle2008_1.dxf'),
    ]

    all_errors = {}
    for filepath in files:
        if not os.path.exists(filepath):
            print(f'ПРОПУЩЕН: {filepath} (файл не найден)')
            continue
        print(f'\n{"=" * 60}')
        errors = validate_dxf_file(filepath)
        all_errors[filepath] = errors
        print()

    print('\n' + '=' * 60)
    print('ИТОГ:')
    total = 0
    for filepath, errors in all_errors.items():
        name = os.path.basename(filepath)
        if errors:
            print(f'  {name}: {len(errors)} ОШИБОК')
            for e in errors:
                print(f'    - {e}')
            total += len(errors)
        else:
            print(f'  {name}: OK')

    if total > 0:
        print(f'\nОбнаружено {total} ошибок!')
        sys.exit(1)
    else:
        print('\nВсе проверки пройдены!')
        sys.exit(0)
