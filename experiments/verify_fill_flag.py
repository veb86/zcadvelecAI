#!/usr/bin/env python3
"""
Эксперимент: верификация флага заливки (SetFill, OpCode=20)
в proxy graphic данных из тестовых DXF файлов.

Парсит бинарные данные proxy graphic и проверяет:
1. Наличие команд SetFill(1) перед полигонами (OpCode=7)
2. Правильную последовательность: SetFill(1) -> Polygon
3. Координаты вершин полигонов совпадают с ожидаемыми
"""

import struct
import sys
import os

# OpCode константы
OPCODES = {
    1: 'Extents',
    2: 'Circle',
    3: 'Circle3P',
    4: 'CircularArc',
    5: 'CircularArc3P',
    6: 'Polyline',
    7: 'Polygon',
    8: 'Mesh',
    9: 'Shell',
    10: 'Text',
    11: 'Text2',
    14: 'SetColor',
    16: 'SetLayer',
    18: 'SetLinetype',
    19: 'SetMarker',
    20: 'SetFill',
    22: 'SetTrueColor',
    23: 'SetLineweight',
    24: 'SetLtScale',
    25: 'SetThickness',
    29: 'PushMatrix',
    30: 'PushMatrix2',
    31: 'PopMatrix',
    33: 'LwPolyline',
    36: 'UnicodeText',
    38: 'UnicodeText2',
    44: 'EllipticArc',
}


def extract_proxy_data(dxf_path):
    """Извлекает hex-данные proxy graphic (код 310) из DXF файла.
    Ищет любые entity с кодом 92 (размер графических данных)
    и последующими кодами 310 (hex-данные)."""
    with open(dxf_path, 'r', errors='replace') as f:
        lines = f.readlines()

    proxy_datas = []
    entity_names = []
    i = 0
    current_entity = ''
    hex_accum = ''
    in_entity = False

    while i < len(lines):
        code = lines[i].strip()
        if i + 1 < len(lines):
            value = lines[i + 1].strip()
        else:
            value = ''

        if code == '0' and in_entity:
            # Конец entity
            if hex_accum:
                proxy_datas.append(hex_accum)
                entity_names.append(current_entity)
            hex_accum = ''
            in_entity = False

        if code == '0':
            current_entity = value
            in_entity = True
            hex_accum = ''

        if code == '310' and in_entity:
            hex_accum += value

        i += 2 if code.lstrip().isdigit() else 1

    if in_entity and hex_accum:
        proxy_datas.append(hex_accum)
        entity_names.append(current_entity)

    return proxy_datas, entity_names


def parse_proxy_graphic(data: bytes):
    """Парсит бинарный блок proxy graphic и выводит команды."""
    if len(data) < 8:
        print(f"  Данных мало: {len(data)} байт")
        return

    chunk_size = struct.unpack_from('<i', data, 0)[0]
    command_count = struct.unpack_from('<i', data, 4)[0]
    print(f"  ChunkSize={chunk_size}, CommandCount={command_count}")

    offset = 8
    fill_active = False
    polygon_count = 0
    filled_polygon_count = 0

    for cmd_idx in range(command_count):
        if offset + 8 > len(data):
            print(f"  [!] Конец данных на команде {cmd_idx}")
            break

        cmd_size = struct.unpack_from('<i', data, offset)[0]
        opcode = struct.unpack_from('<i', data, offset + 4)[0]
        opname = OPCODES.get(opcode, f'Unknown({opcode})')

        cmd_data_offset = offset + 8
        cmd_data_size = cmd_size - 8

        detail = ''

        if opcode == 20:  # SetFill
            if cmd_data_size >= 4:
                fill_val = struct.unpack_from('<i', data, cmd_data_offset)[0]
                fill_active = (fill_val == 1)
                detail = f' value={fill_val} -> active={fill_active}'

        elif opcode == 7:  # Polygon
            polygon_count += 1
            if cmd_data_size >= 4:
                vertex_count = struct.unpack_from('<i', data, cmd_data_offset)[0]
                detail = f' vertices={vertex_count}'
                if fill_active:
                    filled_polygon_count += 1
                    detail += ' [FILLED]'
                    # Выводим координаты вершин
                    voff = cmd_data_offset + 4
                    for vi in range(min(vertex_count, 4)):
                        if voff + 24 <= len(data):
                            x, y, z = struct.unpack_from('<3d', data, voff)
                            detail += f'\n    v{vi}: ({x:.4f}, {y:.4f}, {z:.4f})'
                            voff += 24
                fill_active = False  # Авто-сброс

        elif opcode == 9:  # Shell
            if cmd_data_size >= 4:
                vertex_count = struct.unpack_from('<i', data, cmd_data_offset)[0]
                detail = f' vertices={vertex_count}'
                if fill_active:
                    detail += ' [FILLED]'
                fill_active = False

        elif opcode == 6:  # Polyline
            if cmd_data_size >= 4:
                vertex_count = struct.unpack_from('<i', data, cmd_data_offset)[0]
                detail = f' vertices={vertex_count}'
                fill_active = False

        elif opcode == 2:  # Circle
            if cmd_data_size >= 28:
                cx, cy, cz = struct.unpack_from('<3d', data, cmd_data_offset)
                r = struct.unpack_from('<f', data, cmd_data_offset + 24)[0]
                detail = f' center=({cx:.2f},{cy:.2f},{cz:.2f}) r={r:.2f}'
            fill_active = False

        elif opcode == 29 or opcode == 30:  # PushMatrix
            if cmd_data_size >= 128:
                m = struct.unpack_from('<16d', data, cmd_data_offset)
                tx, ty, tz = m[3], m[7], m[11]
                detail = f' translation=({tx:.3f}, {ty:.3f}, {tz:.3f})'

        elif opcode == 14:  # SetColor
            if cmd_data_size >= 4:
                color = struct.unpack_from('<I', data, cmd_data_offset)[0]
                detail = f' color={color}'

        elif opcode in (1, 16, 18, 19, 22, 23, 24, 25, 31):
            # Системные — пропускаем без деталей
            if opcode in (1,):  # Extents
                if cmd_data_size >= 48:
                    mn = struct.unpack_from('<3d', data, cmd_data_offset)
                    mx = struct.unpack_from('<3d', data, cmd_data_offset + 24)
                    detail = (f' min=({mn[0]:.2f},{mn[1]:.2f},{mn[2]:.2f})'
                              f' max=({mx[0]:.2f},{mx[1]:.2f},{mx[2]:.2f})')
        else:
            # Неизвестная графическая команда — сбрасываем fill
            fill_active = False

        print(f"  [{cmd_idx:3d}] OpCode={opcode:2d} ({opname:15s})"
              f" size={cmd_size:5d}{detail}")

        offset += cmd_size

    print(f"\n  Итого: полигонов={polygon_count},"
          f" из них с заливкой={filled_polygon_count}")


def main():
    dxf_dir = os.path.join(os.path.dirname(__file__), '..', 'dxf_samples')

    test_files = ['testarrow.txt', 'testSPDSSECTION.txt']

    for fname in test_files:
        fpath = os.path.join(dxf_dir, fname)
        if not os.path.exists(fpath):
            print(f"Файл не найден: {fpath}")
            continue

        print(f"\n{'='*70}")
        print(f"Файл: {fname}")
        print(f"{'='*70}")

        proxy_datas, entity_names = extract_proxy_data(fpath)
        print(f"Найдено proxy объектов: {len(proxy_datas)}")

        for idx, hex_data in enumerate(proxy_datas):
            try:
                data = bytes.fromhex(hex_data)
            except ValueError:
                print(f"\n--- Proxy #{idx+1}: неверные hex-данные ---")
                continue
            ename = entity_names[idx] if idx < len(entity_names) else '?'
            # Пропускаем XRECORD — это не proxy graphic
            if ename == 'XRECORD':
                continue
            print(f"\n--- Proxy объект #{idx+1} ({ename},"
                  f" {len(data)} байт) ---")
            try:
                parse_proxy_graphic(data)
            except Exception as e:
                print(f"  Ошибка парсинга: {e}")


if __name__ == '__main__':
    main()
