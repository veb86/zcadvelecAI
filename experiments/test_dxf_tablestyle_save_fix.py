#!/usr/bin/env python3
"""
Тест для проверки логики исправления сохранения стилей таблиц DXF.
Проверяет целостность ссылок: словарь ACAD_TABLESTYLE должен указывать
на реально существующие объекты TABLESTYLE в секции OBJECTS.

Основная проблема до исправления: TABLESTYLE объекты заменялись новыми хэндлами
(F000, F001, F002), но словарь оставался со старыми (299, 7F, 298) → битый файл.
"""

import sys

DXF_FILE = '/tmp/gh-issue-solver-1775023558502/cad_source/tablestyle.dxf'
SAVED_FILE = '/tmp/gh-issue-solver-1775023558502/cad_source/tablestyle_1.dxf'


def parse_dxf_pairs(filename):
    """Читает DXF и возвращает список пар (code, value)."""
    with open(filename, 'r', encoding='cp1251', errors='replace') as f:
        lines = f.read().splitlines()
    pairs = []
    i = 0
    while i < len(lines) - 1:
        try:
            code = int(lines[i].strip())
            value = lines[i + 1].strip()
            pairs.append((code, value))
        except ValueError:
            pass
        i += 2
    return pairs


def find_acad_tablestyle_dict(pairs):
    """
    Находит хэндл словаря ACAD_TABLESTYLE и карту handle->name.
    ACAD_TABLESTYLE встречается как: code=3 value='ACAD_TABLESTYLE', потом code=350 value=<handle>
    """
    dict_handle = None
    for i, (code, value) in enumerate(pairs):
        if code == 3 and value.upper() == 'ACAD_TABLESTYLE':
            if i + 1 < len(pairs) and pairs[i + 1][0] == 350:
                dict_handle = pairs[i + 1][1].upper()
                break

    if dict_handle is None:
        return None, {}

    # Читаем содержимое словаря
    handle_to_name = {}
    in_dict = False
    last_key = None
    for code, value in pairs:
        if not in_dict:
            if code == 5 and value.upper() == dict_handle:
                in_dict = True
        else:
            if code == 0:
                break  # Конец словаря
            elif code == 3:
                last_key = value
            elif code == 350 and last_key:
                handle_to_name[value.upper()] = last_key
                last_key = None

    return dict_handle, handle_to_name


def find_tablestyle_handles(pairs):
    """
    Ищет все объекты TABLESTYLE в секции OBJECTS (код 0, значение TABLESTYLE).
    Возвращает список хэндлов (из следующей группы 5).
    """
    handles = []
    in_objects = False
    in_tablestyle = False
    obj_handle = None

    for code, value in pairs:
        # Отслеживаем вход в секцию OBJECTS
        if code == 2 and value.upper() == 'OBJECTS':
            in_objects = True
        elif code == 0 and value.upper() == 'ENDSEC' and in_objects:
            in_objects = False

        if not in_objects:
            continue

        if not in_tablestyle:
            # Ищем начало объекта TABLESTYLE (code=0 value=TABLESTYLE)
            if code == 0 and value.upper() == 'TABLESTYLE':
                in_tablestyle = True
                obj_handle = None
        else:
            if code == 5 and obj_handle is None:
                obj_handle = value.upper()
            elif code == 0:
                # Конец объекта TABLESTYLE
                if obj_handle:
                    handles.append(obj_handle)
                in_tablestyle = False
                obj_handle = None
                # Начало следующего объекта
                if value.upper() == 'TABLESTYLE':
                    in_tablestyle = True

    if in_tablestyle and obj_handle:
        handles.append(obj_handle)

    return handles


def check_file(filename, label):
    """Проверяет целостность ссылок TABLESTYLE в DXF файле."""
    print(f"\n=== {label} ===")

    pairs = parse_dxf_pairs(filename)
    dict_handle, handle_to_name = find_acad_tablestyle_dict(pairs)

    if dict_handle is None:
        print("  ОШИБКА: словарь ACAD_TABLESTYLE не найден")
        return False

    print(f"  Словарь ACAD_TABLESTYLE хэндл={dict_handle}")
    print(f"  Содержимое словаря: {handle_to_name}")

    actual_handles = find_tablestyle_handles(pairs)
    print(f"  Объекты TABLESTYLE хэндлы: {actual_handles}")

    actual_set = set(actual_handles)
    ok = True

    for handle, name in handle_to_name.items():
        if handle in actual_set:
            print(f"  ✓ '{name}' хэндл={handle} → объект найден")
        else:
            print(f"  ✗ '{name}' хэндл={handle} → объект НЕ НАЙДЕН (битая ссылка!)")
            ok = False

    dict_handles = set(handle_to_name.keys())
    for handle in actual_handles:
        if handle not in dict_handles:
            print(f"  ✗ TABLESTYLE хэндл={handle} → отсутствует в словаре!")

    if ok:
        print("  => ФАЙЛ КОРРЕКТЕН")
    else:
        print("  => ФАЙЛ ПОВРЕЖДЁН")

    return ok


if __name__ == '__main__':
    print("Проверка целостности ссылок TABLESTYLE в DXF файлах")
    print("=" * 60)

    orig_ok = check_file(DXF_FILE, "Исходный файл tablestyle.dxf")
    saved_ok = check_file(SAVED_FILE, "Сохранённый файл tablestyle_1.dxf")

    print("\n" + "=" * 60)
    if orig_ok:
        print("✓ Исходный файл: OK")
    else:
        print("✗ Исходный файл: ОШИБКИ")

    if not saved_ok:
        print("✓ Сохранённый файл: ПОВРЕЖДЁН (баг подтверждён)")
    else:
        print("✗ Сохранённый файл: OK (неожиданно!)")

    sys.exit(0 if (orig_ok and not saved_ok) else 1)
