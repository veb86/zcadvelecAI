#!/usr/bin/env python3
"""
Тест: проверка наличия хэндла владельца (код 330) в записях
BLOCK, ENDBLK и BLOCK_RECORD при экспорте DXF.

Этот тест верифицирует исправление бага, при котором AutoCAD
не мог открыть файл с MLEADERSTYLE, потому что блоки
(например _DetailCallout) не имели ссылки на свой BLOCK_RECORD
через код 330.

Запуск: python3 experiments/test_block_owner_handle.py
"""
import sys
import os


def parse_dxf(filepath):
    """Разбирает DXF-файл в список пар (код, значение)."""
    with open(filepath, errors='ignore') as f:
        lines = f.readlines()
    pairs = []
    i = 0
    while i < len(lines) - 1:
        try:
            code = int(lines[i].strip())
        except ValueError:
            i += 1
            continue
        value = lines[i + 1].strip()
        pairs.append((code, value))
        i += 2
    return pairs


def find_section_range(pairs, section_name):
    """Возвращает диапазон индексов секции."""
    start = None
    for i, (code, value) in enumerate(pairs):
        if code == 2 and value == section_name:
            if i > 0 and pairs[i - 1] == (0, 'SECTION'):
                start = i - 1
        if start is not None and code == 0 and value == 'ENDSEC':
            return start, i
    return None, None


def check_block_owner_handles(filepath):
    """Проверяет наличие кода 330 в BLOCK, ENDBLK, BLOCK_RECORD."""
    pairs = parse_dxf(filepath)
    errors = []

    # Проверяем BLOCK_RECORD в секции TABLES
    tables_s, tables_e = find_section_range(pairs, 'TABLES')
    if tables_s is None:
        errors.append('Секция TABLES не найдена')
        return errors

    # Собираем BLOCK_RECORD записи
    block_records = {}
    i = tables_s
    while i <= tables_e:
        code, value = pairs[i]
        if code == 0 and value == 'BLOCK_RECORD':
            handle = ''
            name = ''
            has_330 = False
            owner_handle = ''
            j = i + 1
            while j <= tables_e:
                c, v = pairs[j]
                if c == 0:
                    break
                if c == 5 and not handle:
                    handle = v
                if c == 2:
                    name = v
                if c == 330:
                    has_330 = True
                    owner_handle = v
                j += 1
            if handle and name:
                block_records[name] = {
                    'handle': handle,
                    'has_330': has_330,
                    'owner': owner_handle
                }
                if not has_330:
                    errors.append(
                        f'BLOCK_RECORD "{name}" '
                        f'(handle={handle}): '
                        f'отсутствует код 330')
            i = j
            continue
        i += 1

    # Проверяем BLOCK и ENDBLK в секции BLOCKS
    blocks_s, blocks_e = find_section_range(pairs, 'BLOCKS')
    if blocks_s is None:
        errors.append('Секция BLOCKS не найдена')
        return errors

    i = blocks_s
    while i <= blocks_e:
        code, value = pairs[i]
        if code == 0 and value in ('BLOCK', 'ENDBLK'):
            obj_type = value
            handle = ''
            name = ''
            has_330 = False
            owner_handle = ''
            j = i + 1
            while j <= blocks_e:
                c, v = pairs[j]
                if c == 0:
                    break
                if c == 5 and not handle:
                    handle = v
                if c == 2 and obj_type == 'BLOCK':
                    name = v
                if c == 330:
                    has_330 = True
                    owner_handle = v
                j += 1

            if not has_330:
                label = name if name else f'(handle={handle})'
                errors.append(
                    f'{obj_type} "{label}" '
                    f'(handle={handle}): '
                    f'отсутствует код 330')
            elif name and name in block_records:
                # Проверяем что 330 указывает на BLOCK_RECORD
                expected = block_records[name]['handle']
                if owner_handle.upper() != expected.upper():
                    errors.append(
                        f'{obj_type} "{name}": '
                        f'код 330={owner_handle} '
                        f'не совпадает с BLOCK_RECORD '
                        f'handle={expected}')
            i = j
            continue
        i += 1

    return errors


def main():
    test_files = [
        'cad_source/test/+mleaderstyle2008.dxf',
        'cad_source/test/+mleaderstyle2008_3.dxf',
    ]

    all_ok = True
    for filepath in test_files:
        if not os.path.exists(filepath):
            print(f'SKIP: {filepath} (файл не найден)')
            continue

        print(f'\nПроверка: {filepath}')
        errors = check_block_owner_handles(filepath)
        if errors:
            all_ok = False
            print(f'  ОШИБКИ ({len(errors)}):')
            for err in errors:
                print(f'    - {err}')
        else:
            print(f'  OK: все BLOCK/ENDBLK/BLOCK_RECORD '
                  f'имеют код 330')

    print()
    if all_ok:
        print('Все тесты пройдены.')
    else:
        print('Есть ошибки — см. выше.')
    return 0 if all_ok else 1


if __name__ == '__main__':
    sys.exit(main())
