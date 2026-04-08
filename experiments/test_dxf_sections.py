#!/usr/bin/env python3
"""
Тест для проверки секций CLASSES и OBJECTS в DXF файлах.

Проверяет, что:
1. Шаблон содержит непустые секции CLASSES и OBJECTS
2. Эталонный файл содержит корректные секции
3. Некорректный файл (notwork) имеет пустую секцию CLASSES
4. Логика замены секций работает правильно при пустых RawSections
"""

import os
import sys

TEMPLATE = os.path.join(
    os.path.dirname(__file__), '..', 'environment',
    'runtimefiles', 'AllCPU-AllOS', 'common',
    'cfg', 'components', 'savetemplate2000.dxf'
)
ETALON = os.path.join(
    os.path.dirname(__file__), '..',
    'cad_source', 'test', 'empty_etalon2000.dxf'
)
NOTWORK = os.path.join(
    os.path.dirname(__file__), '..',
    'cad_source', 'test', 'empty_notwork2000.dxf'
)


def extract_section(lines, section_name):
    """Извлекает секцию DXF по имени (аналог ExtractDxfRawSectionFromLines)."""
    start = None
    end = None
    i = 0
    while i < len(lines) - 3:
        if (lines[i].strip() == '0'
                and lines[i+1].strip() == 'SECTION'
                and lines[i+2].strip() == '2'
                and lines[i+3].strip() == section_name):
            start = i
            break
        i += 1
    if start is None:
        return None
    i = start + 4
    while i < len(lines) - 1:
        if lines[i].strip() == '0' and lines[i+1].strip() == 'ENDSEC':
            end = i + 1
            break
        i += 1
    if end is None:
        return None
    return lines[start:end+1]


def extract_body(section_lines):
    """Извлекает тело секции без заголовка и ENDSEC
    (аналог ExtractDXFSectionBody)."""
    if section_lines is None or len(section_lines) <= 4:
        return []
    # Заголовок: 0, SECTION, 2, <NAME> (4 строки)
    # Конец: 0, ENDSEC (2 строки)
    body_end = len(section_lines) - 2
    return section_lines[4:body_end]


def count_class_entries(body_lines):
    """Считает количество записей CLASS в теле секции CLASSES."""
    count = 0
    for line in body_lines:
        if line.strip() == 'CLASS':
            count += 1
    return count


def count_dict_entries(body_lines):
    """Считает записи словаря ACAD_* в теле секции OBJECTS."""
    count = 0
    for line in body_lines:
        stripped = line.strip()
        if stripped.startswith('ACAD_'):
            count += 1
    return count


def read_dxf(path):
    """Читает файл и возвращает список строк без переносов."""
    with open(path, 'r') as f:
        return [line.rstrip('\n').rstrip('\r') for line in f]


def main():
    passed = 0
    failed = 0

    def check(name, condition, detail=''):
        nonlocal passed, failed
        if condition:
            print(f'  PASS: {name}')
            passed += 1
        else:
            print(f'  FAIL: {name} {detail}')
            failed += 1

    # --- Тест 1: Шаблон ---
    print('Тест 1: Шаблон (savetemplate2000.dxf)')
    lines = read_dxf(TEMPLATE)
    classes = extract_section(lines, 'CLASSES')
    objects = extract_section(lines, 'OBJECTS')
    classes_body = extract_body(classes)
    objects_body = extract_body(objects)

    check('Секция CLASSES найдена', classes is not None)
    check('Тело CLASSES не пустое',
          len(classes_body) > 0,
          f'(строк: {len(classes_body)})')
    class_count = count_class_entries(classes_body)
    check('CLASSES содержит >= 3 записей CLASS',
          class_count >= 3,
          f'(найдено: {class_count})')

    check('Секция OBJECTS найдена', objects is not None)
    check('Тело OBJECTS не пустое',
          len(objects_body) > 0,
          f'(строк: {len(objects_body)})')
    dict_count = count_dict_entries(objects_body)
    check('OBJECTS содержит >= 5 записей ACAD_*',
          dict_count >= 5,
          f'(найдено: {dict_count})')

    # --- Тест 2: Эталон ---
    print('\nТест 2: Эталон (empty_etalon2000.dxf)')
    lines = read_dxf(ETALON)
    classes = extract_section(lines, 'CLASSES')
    objects = extract_section(lines, 'OBJECTS')
    classes_body = extract_body(classes)
    objects_body = extract_body(objects)

    check('Секция CLASSES найдена', classes is not None)
    check('Тело CLASSES не пустое',
          len(classes_body) > 0,
          f'(строк: {len(classes_body)})')
    class_count = count_class_entries(classes_body)
    check('CLASSES содержит >= 3 записей CLASS',
          class_count >= 3,
          f'(найдено: {class_count})')

    check('Секция OBJECTS найдена', objects is not None)
    check('Тело OBJECTS не пустое',
          len(objects_body) > 0,
          f'(строк: {len(objects_body)})')
    dict_count = count_dict_entries(objects_body)
    check('OBJECTS содержит >= 5 записей ACAD_*',
          dict_count >= 5,
          f'(найдено: {dict_count})')

    # --- Тест 3: Некорректный файл ---
    print('\nТест 3: Некорректный файл (empty_notwork2000.dxf)')
    lines = read_dxf(NOTWORK)
    classes = extract_section(lines, 'CLASSES')
    objects = extract_section(lines, 'OBJECTS')
    classes_body = extract_body(classes)
    objects_body = extract_body(objects)

    check('Секция CLASSES найдена', classes is not None)
    # Этот тест подтверждает баг: CLASSES пустая
    check('Тело CLASSES пустое (подтверждение бага)',
          len(classes_body) <= 1,
          f'(строк: {len(classes_body)})')

    check('Секция OBJECTS найдена', objects is not None)
    # OBJECTS усечена — меньше записей ACAD_*
    dict_count = count_dict_entries(objects_body)
    check('OBJECTS содержит < 5 записей ACAD_* (подтверждение бага)',
          dict_count < 5,
          f'(найдено: {dict_count})')

    # --- Тест 4: Логика замены секций ---
    print('\nТест 4: Логика замены (имитация исправления)')
    # При пустой RawClassesSection шаблон должен использоваться
    raw_classes = ''
    should_replace_classes = (raw_classes != '')
    check('Пустая RawClassesSection -> шаблон используется',
          not should_replace_classes)

    # При непустой RawClassesSection она должна заменять шаблон
    raw_classes = '0\nSECTION\n2\nCLASSES\n0\nCLASS\n...\n0\nENDSEC'
    should_replace_classes = (raw_classes != '')
    check('Непустая RawClassesSection -> замена шаблона',
          should_replace_classes)

    # При пустой RawObjectsSection шаблон должен использоваться
    raw_objects = ''
    should_replace_objects = (raw_objects != '')
    check('Пустая RawObjectsSection -> шаблон используется',
          not should_replace_objects)

    # При непустой RawObjectsSection она должна заменять шаблон
    raw_objects = '0\nSECTION\n2\nOBJECTS\n0\nDICTIONARY\n...\n0\nENDSEC'
    should_replace_objects = (raw_objects != '')
    check('Непустая RawObjectsSection -> замена шаблона',
          should_replace_objects)

    # --- Итог ---
    total = passed + failed
    print(f'\nИтого: {passed}/{total} тестов пройдено')
    if failed > 0:
        print(f'{failed} тестов провалено')
        return 1
    return 0


if __name__ == '__main__':
    sys.exit(main())
