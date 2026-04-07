#!/usr/bin/env python3
"""
Тест фильтрации блочных стилей мультивыносок при загрузке DXF.

Эмулирует логику HasBlockContentInLines из uzestylesmleaderdxf.pas:
- ContentType (код 90) = 1 → блочный стиль
- Код 343 непустой → блочный стиль (AutoCAD 2008)

Проверяет все три тестовых файла из задачи:
1. leadermtextnonestyle.dxf — нет блочных стилей
2. leadermtextnoneblockstyle.dxf — один блочный стиль (blockstyle)
3. leaderonlyblockstyle.dxf — один блочный стиль (blockstyle)
"""
import sys
import os

CONTENT_TYPE_BLOCK = 1


def parse_dxf_lines(filepath):
    """Читает DXF-файл и возвращает строки"""
    with open(filepath, 'r') as f:
        return [line.rstrip('\n').rstrip('\r') for line in f.readlines()]


def find_style_dict(lines):
    """Находит словарь ACAD_MLEADERSTYLE и возвращает
    карту хэндл → имя"""
    # Шаг 1: найти хэндл словаря
    dict_handle = ''
    i = 0
    while i < len(lines) - 1:
        c = lines[i].strip()
        v = lines[i + 1].strip()
        if c == '3' and v == 'ACAD_MLEADERSTYLE':
            if i + 3 < len(lines) and lines[i + 2].strip() == '350':
                dict_handle = lines[i + 3].strip()
        i += 2

    if not dict_handle:
        return {}

    # Шаг 2: найти словарь и извлечь записи
    style_map = {}
    last_name = ''
    in_dict = False
    i = 0
    while i < len(lines) - 1:
        c = lines[i].strip()
        v = lines[i + 1].strip()
        if c == '5' and v == dict_handle:
            in_dict = True
        elif in_dict:
            if c == '0':
                break
            if c == '3':
                last_name = v
            elif c == '350' and last_name:
                style_map[v] = last_name
                last_name = ''
        i += 2

    return style_map


def has_block_content_in_lines(obj_codes):
    """Эмуляция HasBlockContentInLines из Pascal-кода.
    obj_codes — список кортежей (код, значение)"""
    for code, value in obj_codes:
        # Код 90 — ContentType: 1 означает блок
        if code == 90:
            try:
                if int(value) == CONTENT_TYPE_BLOCK:
                    return True
            except ValueError:
                pass
        # Код 343 — хэндл блока содержимого
        if code == 343 and value != '':
            return True
    return False


def extract_mleader_styles(lines, style_map):
    """Извлекает объекты MLEADERSTYLE и проверяет каждый"""
    results = []
    inv_map = {v: k for k, v in style_map.items()}

    i = 0
    while i < len(lines) - 1:
        if (lines[i].strip() == '0'
                and lines[i + 1].strip() == 'MLEADERSTYLE'):
            j = i + 2
            handle = ''
            obj_codes = []
            while j < len(lines) - 1:
                c = lines[j].strip()
                v = lines[j + 1].strip()
                if c == '0':
                    break
                try:
                    code_int = int(c)
                except ValueError:
                    j += 2
                    continue
                if code_int == 5 and not handle:
                    handle = v
                obj_codes.append((code_int, v))
                j += 2

            name = style_map.get(handle, '???')
            is_block = has_block_content_in_lines(obj_codes)
            results.append({
                'name': name,
                'handle': handle,
                'is_block': is_block,
            })
            i = j
        else:
            i += 1

    return results


def test_file(filepath, expected_block_names):
    """Тестирует один DXF-файл"""
    filename = os.path.basename(filepath)
    print(f'\n=== {filename} ===')

    lines = parse_dxf_lines(filepath)
    style_map = find_style_dict(lines)
    styles = extract_mleader_styles(lines, style_map)

    loaded = []
    skipped = []

    for s in styles:
        status = 'ПРОПУЩЕН (block)' if s['is_block'] else 'ЗАГРУЖЕН'
        print(f'  {s["name"]:20s} [{s["handle"]}] — {status}')
        if s['is_block']:
            skipped.append(s['name'])
        else:
            loaded.append(s['name'])

    # Проверяем ожидания
    actual_block_names = set(skipped)
    expected = set(expected_block_names)

    if actual_block_names == expected:
        print(f'  ✓ Корректно: пропущены {skipped or "ничего"}')
        return True
    else:
        print(f'  ✗ ОШИБКА: ожидали пропустить {expected},'
              f' пропустили {actual_block_names}')
        return False


def main():
    base = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    test_dir = os.path.join(base, 'cad_source', 'test')

    ok = True

    # Файл 1: только mtext и none стили — блочных нет
    ok &= test_file(
        os.path.join(test_dir, 'leadermtextnonestyle.dxf'),
        expected_block_names=[])

    # Файл 2: mtext, none и block стили — blockstyle блочный
    ok &= test_file(
        os.path.join(test_dir, 'leadermtextnoneblockstyle.dxf'),
        expected_block_names=['blockstyle'])

    # Файл 3: только block стиль — blockstyle блочный
    ok &= test_file(
        os.path.join(test_dir, 'leaderonlyblockstyle.dxf'),
        expected_block_names=['blockstyle'])

    print('\n' + '=' * 40)
    if ok:
        print('ВСЕ ТЕСТЫ ПРОЙДЕНЫ')
    else:
        print('ЕСТЬ ОШИБКИ')
        sys.exit(1)


if __name__ == '__main__':
    main()
