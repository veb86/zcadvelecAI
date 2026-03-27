#!/usr/bin/env python3
"""
Тест: проверка логики сохранения секций CLASSES и OBJECTS.

Имитирует работу нового кода:
1. Извлекает сырые секции CLASSES и OBJECTS из исходного файла AutoCAD
2. Заменяет эти секции в файле, сохранённом ZCAD
3. Проверяет корректность результата

Файлы:
- testspds3entity_original.txt — исходный файл от AutoCAD (рабочий)
- testspds3entity777.txt — файл после сохранения ZCAD (сломанный)
"""

import os
import sys

BASEDIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))


def extract_raw_section(lines, section_name):
    """Извлекает сырую секцию из DXF-файла (аналог ExtractDxfRawSection)."""
    start = -1
    end = -1
    i = 0
    while i < len(lines) - 3:
        if (lines[i].strip() == '0' and
            lines[i+1].strip() == 'SECTION' and
            lines[i+2].strip() == '2' and
            lines[i+3].strip() == section_name):
            start = i
            break
        i += 1
    if start < 0:
        return None, -1, -1

    i = start + 4
    while i < len(lines) - 1:
        if lines[i].strip() == '0' and lines[i+1].strip() == 'ENDSEC':
            end = i + 1
            break
        i += 1
    if end < 0:
        return None, -1, -1

    return lines[start:end+1], start, end


def replace_section(target_lines, section_name, replacement_lines):
    """Заменяет секцию в целевом файле (аналог WriteRawSectionContent + SkipTemplateSection)."""
    section_data, start, end = extract_raw_section(target_lines, section_name)
    if start < 0:
        print(f"  ОШИБКА: секция {section_name} не найдена в целевом файле")
        return target_lines

    # Заменяем секцию
    result = target_lines[:start] + replacement_lines + target_lines[end+1:]
    return result


def count_entities(lines, section_name):
    """Подсчитывает количество объектов в секции."""
    section_data, _, _ = extract_raw_section(lines, section_name)
    if section_data is None:
        return 0
    count = 0
    for i, line in enumerate(section_data):
        if line.strip() == '0' and i + 1 < len(section_data):
            val = section_data[i+1].strip()
            if val not in ('SECTION', 'ENDSEC'):
                count += 1
    return count


def main():
    orig_file = os.path.join(BASEDIR, 'testspds3entity_original.txt')
    zcad_file = os.path.join(BASEDIR, 'testspds3entity777.txt')

    if not os.path.exists(orig_file):
        print(f"ОШИБКА: файл {orig_file} не найден")
        sys.exit(1)
    if not os.path.exists(zcad_file):
        print(f"ОШИБКА: файл {zcad_file} не найден")
        sys.exit(1)

    with open(orig_file, 'r', encoding='utf-8', errors='replace') as f:
        orig_lines = f.readlines()
    with open(zcad_file, 'r', encoding='utf-8', errors='replace') as f:
        zcad_lines = f.readlines()

    print("=== Анализ исходного файла (AutoCAD) ===")
    for section in ['CLASSES', 'OBJECTS']:
        data, start, end = extract_raw_section(orig_lines, section)
        if data:
            count = count_entities(orig_lines, section)
            print(f"  {section}: строки {start+1}-{end+1} ({len(data)} строк, {count} объектов)")
        else:
            print(f"  {section}: НЕ НАЙДЕНА")

    print()
    print("=== Анализ файла ZCAD (до исправления) ===")
    for section in ['CLASSES', 'OBJECTS']:
        data, start, end = extract_raw_section(zcad_lines, section)
        if data:
            count = count_entities(zcad_lines, section)
            print(f"  {section}: строки {start+1}-{end+1} ({len(data)} строк, {count} объектов)")
        else:
            print(f"  {section}: НЕ НАЙДЕНА")

    # Извлекаем сырые секции из оригинала
    raw_classes, _, _ = extract_raw_section(orig_lines, 'CLASSES')
    raw_objects, _, _ = extract_raw_section(orig_lines, 'OBJECTS')

    if raw_classes is None or raw_objects is None:
        print("ОШИБКА: не удалось извлечь секции из оригинала")
        sys.exit(1)

    # Заменяем секции в ZCAD файле
    result_lines = zcad_lines[:]
    result_lines = replace_section(result_lines, 'CLASSES', raw_classes)
    result_lines = replace_section(result_lines, 'OBJECTS', raw_objects)

    print()
    print("=== Анализ файла после исправления ===")
    for section in ['CLASSES', 'OBJECTS']:
        data, start, end = extract_raw_section(result_lines, section)
        if data:
            count = count_entities(result_lines, section)
            print(f"  {section}: строки {start+1}-{end+1} ({len(data)} строк, {count} объектов)")
        else:
            print(f"  {section}: НЕ НАЙДЕНА")

    # Проверки
    print()
    print("=== Проверки ===")

    # Проверка 1: секция CLASSES содержит все классы из оригинала
    orig_classes_count = count_entities(orig_lines, 'CLASSES')
    result_classes_count = count_entities(result_lines, 'CLASSES')
    ok = orig_classes_count == result_classes_count
    print(f"  [{'OK' if ok else 'FAIL'}] CLASSES: {result_classes_count} классов "
          f"(ожидалось {orig_classes_count})")

    # Проверка 2: секция OBJECTS содержит все объекты из оригинала
    orig_objects_count = count_entities(orig_lines, 'OBJECTS')
    result_objects_count = count_entities(result_lines, 'OBJECTS')
    ok = orig_objects_count == result_objects_count
    print(f"  [{'OK' if ok else 'FAIL'}] OBJECTS: {result_objects_count} объектов "
          f"(ожидалось {orig_objects_count})")

    # Проверка 3: ACAD_GROUP присутствует в словаре
    has_acad_group = any('ACAD_GROUP' in line for line in result_lines)
    print(f"  [{'OK' if has_acad_group else 'FAIL'}] ACAD_GROUP присутствует в файле")

    # Проверка 4: файл заканчивается на EOF
    last_non_empty = ''
    for line in reversed(result_lines):
        if line.strip():
            last_non_empty = line.strip()
            break
    ok = last_non_empty == 'EOF'
    print(f"  [{'OK' if ok else 'FAIL'}] Файл заканчивается на EOF")

    # Проверка 5: секции HEADER, TABLES, BLOCKS, ENTITIES сохранены из ZCAD
    for section in ['HEADER', 'TABLES', 'BLOCKS', 'ENTITIES']:
        data, _, _ = extract_raw_section(result_lines, section)
        zcad_data, _, _ = extract_raw_section(zcad_lines, section)
        ok = data is not None and zcad_data is not None and len(data) == len(zcad_data)
        print(f"  [{'OK' if ok else 'FAIL'}] Секция {section} сохранена из ZCAD "
              f"({len(data) if data else 0} строк)")

    # Сохраняем результат
    out_file = os.path.join(BASEDIR, 'experiments', 'testspds3entity_fixed.txt')
    with open(out_file, 'w', encoding='utf-8') as f:
        f.writelines(result_lines)
    print(f"\n  Результат сохранён в: {out_file}")
    print(f"  Размер: {len(result_lines)} строк "
          f"(было {len(zcad_lines)}, оригинал {len(orig_lines)})")


if __name__ == '__main__':
    main()
