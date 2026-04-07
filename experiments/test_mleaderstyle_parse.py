#!/usr/bin/env python3
"""
Тестовый скрипт для проверки парсинга MLEADERSTYLE из DXF-файла.
Имитирует логику модуля uzestylesmleaderdxf.pas на Python
для верификации корректности алгоритма.
"""

import os
import sys

DXF_FILE = os.path.join(
    os.path.dirname(os.path.dirname(os.path.abspath(__file__))),
    'cad_source', 'test', '+mleaderstyle2008.dxf'
)

def read_dxf_lines(filepath):
    """Читает DXF-файл и возвращает список строк."""
    with open(filepath, 'r', encoding='utf-8', errors='replace') as f:
        return f.read().splitlines()

def parse_group_code(s):
    """Возвращает числовой код группы или -1."""
    try:
        return int(s.strip())
    except ValueError:
        return -1

def extract_mleaderstyle_dictionary(lines):
    """Ищет словарь ACAD_MLEADERSTYLE и возвращает карту handle->name."""
    dict_handle = ''
    style_map = {}

    # Шаг 1: найти хэндл словаря ACAD_MLEADERSTYLE
    i = 0
    while i < len(lines) - 1 and not dict_handle:
        code = parse_group_code(lines[i])
        value = lines[i + 1].strip()
        if code == 3 and value.upper() == 'ACAD_MLEADERSTYLE':
            if i + 3 < len(lines) and parse_group_code(lines[i + 2]) == 350:
                dict_handle = lines[i + 3].strip().upper()
        i += 2

    if not dict_handle:
        return {}, ''

    # Шаг 2: найти DICTIONARY с данным хэндлом
    in_target = False
    last_key = ''
    i = 0
    while i < len(lines) - 1:
        code = parse_group_code(lines[i])
        value = lines[i + 1].strip()

        if not in_target:
            if code == 5 and value.upper() == dict_handle:
                in_target = True
        else:
            if code == 0:
                in_target = False
            elif code == 3:
                last_key = value
            elif code == 350 and last_key:
                style_map[value.upper()] = last_key
                last_key = ''
        i += 2

    return style_map, dict_handle

def parse_mleaderstyle(lines, style_name):
    """Парсит group codes одного MLEADERSTYLE."""
    style = {}
    i = 0
    while i < len(lines) - 1:
        code = parse_group_code(lines[i])
        value = lines[i + 1].strip()
        style[code] = value
        i += 2
    return style

def main():
    if not os.path.exists(DXF_FILE):
        print(f"ERROR: файл {DXF_FILE} не найден")
        sys.exit(1)

    lines = read_dxf_lines(DXF_FILE)
    print(f"Прочитано {len(lines)} строк из {DXF_FILE}")

    # Извлекаем словарь
    style_map, dict_handle = extract_mleaderstyle_dictionary(lines)
    print(f"\nСловарь ACAD_MLEADERSTYLE: хэндл={dict_handle}")
    print(f"Найдено {len(style_map)} стилей:")
    for handle, name in style_map.items():
        print(f"  {name} -> {handle}")

    # Парсим объекты MLEADERSTYLE
    in_style = False
    obj_handle = ''
    obj_lines = []
    styles_found = 0

    i = 0
    while i < len(lines) - 1:
        code = parse_group_code(lines[i])
        value = lines[i + 1].strip()

        if not in_style:
            if code == 0 and value.upper() == 'MLEADERSTYLE':
                in_style = True
                obj_handle = ''
                obj_lines = []
                i += 2
                continue
        else:
            if code == 0:
                # Конец объекта
                if obj_handle:
                    name = style_map.get(obj_handle, '???')
                    styles_found += 1
                    print(f"\n=== MLEADERSTYLE '{name}' (хэндл={obj_handle}) ===")
                    style = parse_mleaderstyle(obj_lines, name)
                    # Выводим ключевые параметры
                    key_codes = {
                        170: 'LeaderLineType',
                        171: 'LeaderLineTypeId',
                        172: 'FirstSegAngleConstraint',
                        173: 'SecondSegAngleConstraint',
                        90: 'ContentType',
                        91: 'LeaderLineColor',
                        92: 'LeaderLineWeight',
                        93: 'TextColor',
                        94: 'BlockContentColor',
                        40: 'FirstSegAngle',
                        41: 'SecondSegAngle',
                        42: 'DoglegLength',
                        43: 'LandingGap',
                        44: 'TextHeight',
                        45: 'ArrowHeadSize',
                        46: 'BlockContentScale',
                        47: 'BlockContentScaleX',
                        49: 'BlockContentScaleY',
                        140: 'OverallScale',
                        141: 'BreakGapSize',
                        142: 'BlockContentScaleZ',
                        143: 'BlockContentRotation',
                        290: 'EnableDogleg',
                        291: 'EnableLanding',
                        292: 'TextAlignAlwaysLeft',
                        293: 'Annotative',
                        294: 'TextDirectionNegative',
                        295: 'IsBlockContent',
                        296: 'IsMTextContent',
                        297: 'AlignSpace',
                        3: 'Description',
                        300: 'DefaultTextContent',
                        340: 'TextStyleHandle',
                        341: 'BlockContentHandle',
                        342: 'ArrowHeadHandle',
                        343: 'ArrowHeadHandle2',
                        174: 'TextAttachmentLeft',
                        175: 'TextAngleType',
                        176: 'TextAlignmentType',
                        177: 'TextAttachmentDirection',
                        178: 'TextAttachmentRight',
                    }
                    for c, label in sorted(key_codes.items()):
                        if c in style:
                            print(f"  {label} (code {c}) = {style[c]}")
                in_style = False
                obj_handle = ''
                obj_lines = []
                continue
            if code == 5 and not obj_handle:
                obj_handle = value.upper()
            obj_lines.append(lines[i])
            obj_lines.append(lines[i + 1])
        i += 2

    print(f"\nВсего найдено {styles_found} стилей MLEADERSTYLE")

    # Проверки
    assert len(style_map) == 5, f"Ожидалось 5 стилей, найдено {len(style_map)}"
    assert styles_found == 5, f"Ожидалось 5 объектов, найдено {styles_found}"
    expected_names = {'Standard', 'ai', 'Annotative', 'spline', 'veb'}
    actual_names = set(style_map.values())
    assert actual_names == expected_names, f"Несовпадение имён: {actual_names} vs {expected_names}"

    print("\n✓ Все проверки пройдены успешно!")

if __name__ == '__main__':
    main()
