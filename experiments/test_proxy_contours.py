#!/usr/bin/env python3
"""
Скрипт для анализа бинарных данных Proxy Graphic из DXF-файла.

Читает testarrow.txt, извлекает hex-данные группового кода 310
из сущности SPDSNOTEPOSITION, парсит бинарный блок и выводит
информацию о найденных примитивах и их вершинах.

Формат бинарного блока Proxy Graphic:
  [ChunkSize: int32] [CommandCount: int32]
  Повтор CommandCount раз:
    [CommandSize: int32] [OpCode: int32] [Данные...]
"""

import struct
import sys
import os

# Имена OpCode команд Proxy Graphic
OPCODE_NAMES = {
    1: "Extents",
    2: "Circle",
    3: "Circle3P",
    4: "CircularArc",
    5: "CircularArc3P",
    6: "Polyline",
    7: "Polygon",
    8: "Mesh",
    9: "Shell",
    10: "Text",
    11: "Text2",
    12: "XLine",
    13: "Ray",
    14: "SetColor",
    15: "Unused15",
    16: "SetLayer",
    17: "Unused17",
    18: "SetLinetype",
    19: "SetMarker",
    20: "SetFill",
    21: "Unused21",
    22: "SetTrueColor",
    23: "SetLineWeight",
    24: "SetLtScale",
    25: "SetThickness",
    26: "SetPlotStyle",
    27: "PushClip",
    28: "PopClip",
    29: "PushMatrix",
    30: "PushMatrix2",
    31: "PopMatrix",
    32: "PolylineWithNormals",
    33: "LwPolyline",
    34: "SetMaterial",
    35: "SetMapper",
    36: "UnicodeText",
    37: "Unknown37",
    38: "UnicodeText2",
    44: "EllipticArc",
}

# Целевые координаты из описания задачи
EXPECTED_POINTS = {
    "Arrow tip":   (1495.8937, 1056.532),
    "Arrow top":   (1725.0568, 1257.2368),
    "Arrow bottom":(1777.9147, 1171.6941),
    "Leader end":  (2178.0568, 1478.0489),
    "Shelf end":   (5996.8068, 1478.0489),
}


def read_dxf_and_extract_310(filepath):
    """
    Читает DXF-файл и извлекает hex-данные группового кода 310,
    принадлежащие первой сущности SPDSNOTEPOSITION в секции ENTITIES.
    Возвращает объединённые байты.
    """
    with open(filepath, 'r', encoding='utf-8', errors='replace') as f:
        lines = [line.strip() for line in f.readlines()]

    # Находим секцию ENTITIES
    in_entities = False
    in_target_entity = False
    hex_chunks = []
    found_entity = False

    i = 0
    while i < len(lines):
        line = lines[i]

        # Ищем начало секции ENTITIES
        if not in_entities:
            if line == "SECTION" and i > 0 and lines[i - 1].strip() == "0":
                if i + 2 < len(lines) and lines[i + 1].strip() == "2" \
                        and lines[i + 2] == "ENTITIES":
                    in_entities = True
            i += 1
            continue

        # Внутри ENTITIES ищем SPDSNOTEPOSITION
        if not in_target_entity:
            if line == "SPDSNOTEPOSITION" and i > 0 \
                    and lines[i - 1].strip() == "0":
                in_target_entity = True
                found_entity = True
            i += 1
            continue

        # Внутри нужной сущности собираем код 310
        # Останавливаемся при встрече следующей сущности (код 0)
        group_code = line.strip()
        if group_code == "0" and i > 0:
            break

        if group_code == "310" and i + 1 < len(lines):
            hex_data = lines[i + 1].strip()
            hex_chunks.append(hex_data)
            i += 2
            continue

        i += 1

    if not found_entity:
        print("ОШИБКА: сущность SPDSNOTEPOSITION не найдена")
        sys.exit(1)

    combined_hex = "".join(hex_chunks)
    return bytes.fromhex(combined_hex)


def read_point3d(data, offset):
    """Читает 3D-точку (3 x float64) из буфера."""
    x, y, z = struct.unpack_from('<ddd', data, offset)
    return (x, y, z), offset + 24


def read_int32(data, offset):
    """Читает int32 (little-endian) из буфера."""
    val, = struct.unpack_from('<i', data, offset)
    return val, offset + 4


def parse_polyline(data, cmd_offset, cmd_size):
    """
    Парсит команду Polyline (OpCode 6).
    Формат: [VertexCount: int32] [Vertices: Point3d * N]
    За вершинами идёт массив рёбер (пропускаем).
    """
    offset = cmd_offset
    vertex_count, offset = read_int32(data, offset)
    vertices = []
    for _ in range(vertex_count):
        pt, offset = read_point3d(data, offset)
        vertices.append(pt)
    return vertex_count, vertices


def parse_polygon(data, cmd_offset, cmd_size):
    """
    Парсит команду Polygon (OpCode 7).
    Формат аналогичен Polyline: [VertexCount: int32] [Vertices: Point3d * N]
    """
    return parse_polyline(data, cmd_offset, cmd_size)


def parse_circle(data, cmd_offset):
    """
    Парсит команду Circle (OpCode 2).
    Формат: [Center: Point3d] [Radius: float64] [Normal: Point3d]
    """
    offset = cmd_offset
    center, offset = read_point3d(data, offset)
    radius, = struct.unpack_from('<d', data, offset)
    offset += 8
    normal, offset = read_point3d(data, offset)
    return center, radius, normal


def parse_arc(data, cmd_offset):
    """
    Парсит команду CircularArc (OpCode 4).
    Формат: [Center: Point3d] [Radius: float64]
             [Normal: Point3d] [StartVector: Point3d]
             [SweepAngle: float64] [ArcType: int32]
    """
    offset = cmd_offset
    center, offset = read_point3d(data, offset)
    radius, = struct.unpack_from('<d', data, offset)
    offset += 8
    normal, offset = read_point3d(data, offset)
    start_vec, offset = read_point3d(data, offset)
    sweep, = struct.unpack_from('<d', data, offset)
    offset += 8
    arc_type, offset = read_int32(data, offset)
    return center, radius, normal, start_vec, sweep, arc_type


def parse_lwpolyline(data, cmd_offset, cmd_size):
    """
    Парсит команду LwPolyline (OpCode 33).
    Формат: [VertexCount: int32] [Vertices: Point3d * N]
             [Bulges: float64 * N] [Closed: int32]
    """
    offset = cmd_offset
    vertex_count, offset = read_int32(data, offset)
    vertices = []
    for _ in range(vertex_count):
        pt, offset = read_point3d(data, offset)
        vertices.append(pt)
    return vertex_count, vertices


def parse_text(data, cmd_offset, cmd_size):
    """
    Парсит команду Text (OpCode 10).
    Формат: [Position: Point3d] [Normal: Point3d] [Direction: Point3d]
             [Height: float64] [WidthFactor: float64] [ObliqueAngle: float64]
             [TextLength: int32] [TextData: wchar * TextLength]
             ...
    """
    offset = cmd_offset
    position, offset = read_point3d(data, offset)
    normal, offset = read_point3d(data, offset)
    direction, offset = read_point3d(data, offset)
    height, = struct.unpack_from('<d', data, offset)
    offset += 8
    width_factor, = struct.unpack_from('<d', data, offset)
    offset += 8
    oblique, = struct.unpack_from('<d', data, offset)
    offset += 8
    text_len, offset = read_int32(data, offset)
    # Текст в формате UTF-16LE (2 байта на символ)
    if text_len > 0:
        text_bytes = data[offset:offset + text_len * 2]
        try:
            text = text_bytes.decode('utf-16-le')
        except Exception:
            text = repr(text_bytes)
    else:
        text = ""
    return position, height, text


def parse_shell(data, cmd_offset, cmd_size):
    """
    Парсит команду Shell (OpCode 9).
    Формат: [VertexCount: int32] [Vertices: Point3d * N]
             [FaceListSize: int32] [FaceList: int32 * FaceListSize]
    """
    offset = cmd_offset
    vertex_count, offset = read_int32(data, offset)
    vertices = []
    for _ in range(vertex_count):
        pt, offset = read_point3d(data, offset)
        vertices.append(pt)
    face_list_size, offset = read_int32(data, offset)
    faces = []
    for _ in range(face_list_size):
        val, offset = read_int32(data, offset)
        faces.append(val)
    return vertex_count, vertices, faces


def check_expected_point(x, y, tolerance=0.01):
    """Проверяет, совпадает ли точка с одной из целевых координат."""
    for name, (ex, ey) in EXPECTED_POINTS.items():
        if abs(x - ex) < tolerance and abs(y - ey) < tolerance:
            return name
    return None


def main():
    dxf_path = os.path.join(
        os.path.dirname(os.path.dirname(os.path.abspath(__file__))),
        "testarrow.txt"
    )

    print(f"Чтение DXF-файла: {dxf_path}")
    raw_data = read_dxf_and_extract_310(dxf_path)
    print(f"Общий размер бинарных данных (код 310): {len(raw_data)} байт")
    print()

    # Парсинг заголовка
    offset = 0
    chunk_size, offset = read_int32(raw_data, offset)
    command_count, offset = read_int32(raw_data, offset)
    print(f"=== Заголовок Proxy Graphic ===")
    print(f"  ChunkSize:    {chunk_size}")
    print(f"  CommandCount: {command_count}")
    print()

    # Счётчики
    primitives = []
    geometric_count = 0
    attribute_count = 0
    matched_points = {}

    # Парсинг команд
    print(f"=== Команды ({command_count}) ===")
    for cmd_idx in range(command_count):
        if offset + 8 > len(raw_data):
            print(f"  [!] Недостаточно данных для команды #{cmd_idx}")
            break

        cmd_size, _ = read_int32(raw_data, offset)
        opcode, _ = read_int32(raw_data, offset + 4)
        opcode_name = OPCODE_NAMES.get(opcode, f"Unknown({opcode})")
        data_offset = offset + 8  # после заголовка команды

        print(f"  Команда #{cmd_idx:2d}: OpCode={opcode:2d} "
              f"({opcode_name:25s}) Size={cmd_size}")

        # Геометрические примитивы
        if opcode == 6:  # Polyline
            geometric_count += 1
            vc, verts = parse_polyline(raw_data, data_offset, cmd_size)
            print(f"    Polyline: {vc} вершин")
            for vi, v in enumerate(verts):
                match = check_expected_point(v[0], v[1])
                tag = f"  <-- {match}" if match else ""
                if match:
                    matched_points[match] = v
                print(f"      [{vi}] X={v[0]:.4f}, "
                      f"Y={v[1]:.4f}, Z={v[2]:.4f}{tag}")
            primitives.append(("Polyline", vc, verts))

        elif opcode == 7:  # Polygon
            geometric_count += 1
            vc, verts = parse_polygon(raw_data, data_offset, cmd_size)
            print(f"    Polygon: {vc} вершин")
            for vi, v in enumerate(verts):
                match = check_expected_point(v[0], v[1])
                tag = f"  <-- {match}" if match else ""
                if match:
                    matched_points[match] = v
                print(f"      [{vi}] X={v[0]:.4f}, "
                      f"Y={v[1]:.4f}, Z={v[2]:.4f}{tag}")
            primitives.append(("Polygon", vc, verts))

        elif opcode == 2:  # Circle
            geometric_count += 1
            center, radius, normal = parse_circle(raw_data, data_offset)
            print(f"    Circle: Center=({center[0]:.4f}, "
                  f"{center[1]:.4f}, {center[2]:.4f}), "
                  f"Radius={radius:.4f}")
            primitives.append(("Circle", 0, [center]))

        elif opcode == 4:  # Arc
            geometric_count += 1
            center, radius, normal, sv, sweep, atype = parse_arc(
                raw_data, data_offset
            )
            print(f"    Arc: Center=({center[0]:.4f}, "
                  f"{center[1]:.4f}, {center[2]:.4f}), "
                  f"Radius={radius:.4f}, "
                  f"Sweep={sweep:.4f} rad")
            primitives.append(("Arc", 0, [center]))

        elif opcode == 33:  # LwPolyline
            geometric_count += 1
            vc, verts = parse_lwpolyline(
                raw_data, data_offset, cmd_size
            )
            print(f"    LwPolyline: {vc} вершин")
            for vi, v in enumerate(verts):
                match = check_expected_point(v[0], v[1])
                tag = f"  <-- {match}" if match else ""
                if match:
                    matched_points[match] = v
                print(f"      [{vi}] X={v[0]:.4f}, "
                      f"Y={v[1]:.4f}, Z={v[2]:.4f}{tag}")
            primitives.append(("LwPolyline", vc, verts))

        elif opcode == 9:  # Shell
            geometric_count += 1
            vc, verts, faces = parse_shell(
                raw_data, data_offset, cmd_size
            )
            print(f"    Shell: {vc} вершин, "
                  f"{len(faces)} элементов в face list")
            for vi, v in enumerate(verts):
                match = check_expected_point(v[0], v[1])
                tag = f"  <-- {match}" if match else ""
                if match:
                    matched_points[match] = v
                print(f"      [{vi}] X={v[0]:.4f}, "
                      f"Y={v[1]:.4f}, Z={v[2]:.4f}{tag}")
            primitives.append(("Shell", vc, verts))

        elif opcode == 10:  # Text
            geometric_count += 1
            pos, height, text = parse_text(
                raw_data, data_offset, cmd_size
            )
            print(f"    Text: Pos=({pos[0]:.4f}, "
                  f"{pos[1]:.4f}, {pos[2]:.4f}), "
                  f"H={height:.4f}, "
                  f"Content=\"{text}\"")
            primitives.append(("Text", 0, [pos]))

        else:
            attribute_count += 1

        # Переход к следующей команде
        # CommandSize включает собственное поле Size (4 байта),
        # OpCode (4 байта) и данные
        offset += cmd_size

    # Итоги
    print()
    print("=" * 60)
    print("=== ИТОГОВАЯ СВОДКА ===")
    print(f"  Всего команд:             {command_count}")
    print(f"  Геометрических примитивов: {geometric_count}")
    print(f"  Атрибутных команд:         {attribute_count}")
    print()

    print("=== Список примитивов ===")
    for i, (ptype, vc, verts) in enumerate(primitives):
        if ptype in ("Circle", "Arc", "Text"):
            print(f"  #{i}: {ptype}")
        else:
            print(f"  #{i}: {ptype} ({vc} вершин)")
    print()

    print("=== Проверка ключевых координат ===")
    for name, (ex, ey) in EXPECTED_POINTS.items():
        if name in matched_points:
            v = matched_points[name]
            print(f"  [OK] {name}: "
                  f"X={v[0]:.4f}, Y={v[1]:.4f} "
                  f"(ожидалось X={ex:.4f}, Y={ey:.4f})")
        else:
            print(f"  [--] {name}: НЕ НАЙДЕНО "
                  f"(ожидалось X={ex:.4f}, Y={ey:.4f})")

    print()
    found = len(matched_points)
    total = len(EXPECTED_POINTS)
    print(f"  Найдено {found} из {total} ключевых точек")

    if geometric_count > 1:
        print()
        print("ВЫВОД: бинарные данные содержат несколько "
              "отдельных примитивов,")
        print("  которые должны рисоваться независимо друг от друга.")
    else:
        print()
        print("ПРЕДУПРЕЖДЕНИЕ: обнаружен только один "
              "геометрический примитив!")


if __name__ == "__main__":
    main()
