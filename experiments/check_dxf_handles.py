#!/usr/bin/env python3
"""
Проверяет DXF-файл на дублирование хэндлов (группа 5).
Хэндл (код группы 5) должен быть уникальным для каждого объекта в DXF.
Дублирование приводит к ошибке AutoCAD: "Неверная метка: уже используется".

Использование: python3 check_dxf_handles.py <файл.dxf>
"""
import sys
import re

def check_handles(filename):
    with open(filename, 'r', errors='replace') as f:
        lines = f.readlines()

    handles = {}  # handle -> list of (line_number, context)
    i = 0
    last_obj_type = ""
    last_obj_line = 0

    while i < len(lines) - 1:
        code = lines[i].strip()
        value = lines[i+1].strip()

        if code == '0':
            last_obj_type = value
            last_obj_line = i + 1

        # Код группы 5 = хэндл объекта
        if code == '5':
            handle_upper = value.upper()
            context = f"{last_obj_type} (строка {last_obj_line})"
            if handle_upper not in handles:
                handles[handle_upper] = []
            handles[handle_upper].append((i + 1, context))

        i += 2

    # Поиск дубликатов
    duplicates = {h: locs for h, locs in handles.items() if len(locs) > 1}

    if duplicates:
        print(f"ОШИБКА: Найдены дублированные хэндлы в {filename}:")
        for handle, locations in sorted(duplicates.items()):
            print(f"  Handle {handle}:")
            for line_num, ctx in locations:
                print(f"    строка {line_num}: {ctx}")
        return False
    else:
        print(f"OK: Все хэндлы уникальны в {filename} ({len(handles)} хэндлов)")
        return True

if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("Использование: python3 check_dxf_handles.py <файл.dxf>")
        sys.exit(1)

    all_ok = True
    for fname in sys.argv[1:]:
        if not check_handles(fname):
            all_ok = False

    sys.exit(0 if all_ok else 1)
