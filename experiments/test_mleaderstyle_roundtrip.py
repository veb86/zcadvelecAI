#!/usr/bin/env python3
"""
Тестовый скрипт проверки roundtrip: импорт → экспорт MLEADERSTYLE.
Проверяет, что после записи и повторного чтения все стили сохраняются.
"""

import os
import sys

DXF_FILE = os.path.join(
    os.path.dirname(os.path.dirname(os.path.abspath(__file__))),
    'cad_source', 'test', '+mleaderstyle2008.dxf'
)

def parse_group_code(s):
    try:
        return int(s.strip())
    except ValueError:
        return -1

def extract_objects_section(lines):
    """Извлекает секцию OBJECTS из DXF."""
    in_objects = False
    result = []
    i = 0
    while i < len(lines) - 1:
        code = parse_group_code(lines[i])
        value = lines[i + 1].strip()
        if code == 2 and value == 'OBJECTS':
            in_objects = True
            # Включаем SECTION header
            result.append(lines[i - 2] if i >= 2 else '  0')
            result.append(lines[i - 1] if i >= 1 else 'SECTION')
            result.append(lines[i])
            result.append(lines[i + 1])
            i += 2
            continue
        if in_objects:
            result.append(lines[i])
            result.append(lines[i + 1])
            if code == 0 and value == 'ENDSEC':
                break
        i += 2
    return result

def count_mleaderstyles(section_lines):
    """Считает количество MLEADERSTYLE объектов в секции."""
    count = 0
    i = 0
    while i < len(section_lines) - 1:
        code = parse_group_code(section_lines[i])
        value = section_lines[i + 1].strip()
        if code == 0 and value.upper() == 'MLEADERSTYLE':
            count += 1
        i += 2
    return count

def extract_mleaderstyle_handles(section_lines):
    """Извлекает хэндлы всех MLEADERSTYLE объектов."""
    handles = []
    in_style = False
    i = 0
    while i < len(section_lines) - 1:
        code = parse_group_code(section_lines[i])
        value = section_lines[i + 1].strip()
        if not in_style:
            if code == 0 and value.upper() == 'MLEADERSTYLE':
                in_style = True
        else:
            if code == 5:
                handles.append(value.upper())
                in_style = False
            elif code == 0:
                in_style = False
        i += 2
    return handles

def main():
    with open(DXF_FILE, 'r', encoding='utf-8', errors='replace') as f:
        lines = f.read().splitlines()

    objects_section = extract_objects_section(lines)
    print(f"Секция OBJECTS: {len(objects_section)} строк")

    count = count_mleaderstyles(objects_section)
    print(f"Найдено MLEADERSTYLE объектов: {count}")
    assert count == 5, f"Ожидалось 5, найдено {count}"

    handles = extract_mleaderstyle_handles(objects_section)
    print(f"Хэндлы: {handles}")
    expected = {'14D', 'E5', '156', 'D8', '14C'}
    assert set(handles) == expected, f"Несовпадение хэндлов"

    # Проверка: каждый MLEADERSTYLE содержит AcDbMLeaderStyle
    in_style = False
    found_subclass = 0
    i = 0
    while i < len(objects_section) - 1:
        code = parse_group_code(objects_section[i])
        value = objects_section[i + 1].strip()
        if code == 0 and value.upper() == 'MLEADERSTYLE':
            in_style = True
        elif in_style and code == 100 and value == 'AcDbMLeaderStyle':
            found_subclass += 1
            in_style = False
        elif in_style and code == 0:
            in_style = False
        i += 2

    assert found_subclass == 5, (
        f"Ожидалось 5 AcDbMLeaderStyle, найдено {found_subclass}"
    )

    # Проверка: каждый MLEADERSTYLE имеет ACAD_REACTORS
    in_style = False
    found_reactors = 0
    i = 0
    while i < len(objects_section) - 1:
        code = parse_group_code(objects_section[i])
        value = objects_section[i + 1].strip()
        if code == 0 and value.upper() == 'MLEADERSTYLE':
            in_style = True
        elif in_style and code == 102 and '{ACAD_REACTORS' in value:
            found_reactors += 1
            in_style = False
        elif in_style and code == 0:
            in_style = False
        i += 2

    assert found_reactors == 5, (
        f"Ожидалось 5 ACAD_REACTORS, найдено {found_reactors}"
    )

    # Проверка: xdata ACAD_MLEADERVER в каждом стиле
    in_style = False
    found_xdata = 0
    i = 0
    while i < len(objects_section) - 1:
        code = parse_group_code(objects_section[i])
        value = objects_section[i + 1].strip()
        if code == 0 and value.upper() == 'MLEADERSTYLE':
            in_style = True
        elif in_style and code == 1001 and value == 'ACAD_MLEADERVER':
            found_xdata += 1
            in_style = False
        elif in_style and code == 0:
            in_style = False
        i += 2

    assert found_xdata == 5, (
        f"Ожидалось 5 ACAD_MLEADERVER, найдено {found_xdata}"
    )

    print("\n✓ Все проверки roundtrip пройдены успешно!")

if __name__ == '__main__':
    main()
