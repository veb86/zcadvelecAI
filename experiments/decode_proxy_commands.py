#!/usr/bin/env python3
"""
Декодер бинарных данных proxy-графики из DXF-файлов.

Скрипт читает DXF-файлы, находит все сущности с proxy-графикой (код 310),
собирает бинарные данные и декодирует последовательность команд.

Автор: Claude
Дата: 2026-03-30
"""

import struct
import sys
import os

# Словарь имён опкодов
OPCODE_NAMES = {
    1: "Extents",
    2: "Circle",
    4: "Arc",
    6: "Polyline",
    7: "Polygon",
    9: "Shell",
    10: "Text",
    14: "SetColor",
    16: "SetLayer",
    18: "SetLinetype",
    19: "SetMarker",
    20: "SetFill",
    22: "SetTrueColor",
    23: "SetLineweight",
    24: "SetLtScale",
    25: "SetThickness",
    29: "PushMatrix",
    30: "PushMatrix2",
    31: "PopMatrix",
    33: "LwPolyline",
    36: "UnicodeText",
    38: "UnicodeText2",
    44: "EllipticArc",
}


def read_vertex(data, offset):
    """Читает вершину (3 x float64) из данных по смещению."""
    if offset + 24 > len(data):
        return None, offset
    x, y, z = struct.unpack_from('<ddd', data, offset)
    return (x, y, z), offset + 24


def format_vertex(vertex):
    """Форматирует вершину для вывода."""
    return f"({vertex[0]:.6f}, {vertex[1]:.6f}, {vertex[2]:.6f})"


def decode_command(data, offset, cmd_index, output_lines):
    """
    Декодирует одну команду proxy-графики.
    Возвращает новое смещение после обработки команды.
    """
    if offset + 8 > len(data):
        output_lines.append(f"  [!] Недостаточно данных для заголовка команды "
                            f"на смещении {offset}")
        return len(data)

    cmd_size = struct.unpack_from('<i', data, offset)[0]
    opcode = struct.unpack_from('<i', data, offset + 4)[0]
    opcode_name = OPCODE_NAMES.get(opcode, f"Unknown({opcode})")

    output_lines.append(
        f"  Cmd[{cmd_index:3d}]: opcode={opcode:2d} "
        f"({opcode_name:16s}) size={cmd_size}"
    )

    # Указатель на данные после заголовка (8 байт)
    data_offset = offset + 8
    # Конец текущей команды
    cmd_end = offset + cmd_size

    # Декодирование данных в зависимости от опкода
    if opcode == 1:
        # Extents: 2 вершины
        v1, data_offset = read_vertex(data, data_offset)
        v2, data_offset = read_vertex(data, data_offset)
        if v1 and v2:
            output_lines.append(f"           min = {format_vertex(v1)}")
            output_lines.append(f"           max = {format_vertex(v2)}")

    elif opcode == 2:
        # Circle: center, radius, normal
        center, data_offset = read_vertex(data, data_offset)
        if data_offset + 8 <= len(data):
            radius = struct.unpack_from('<d', data, data_offset)[0]
            data_offset += 8
        else:
            radius = None
        normal, data_offset = read_vertex(data, data_offset)
        if center:
            output_lines.append(
                f"           center = {format_vertex(center)}"
            )
        if radius is not None:
            output_lines.append(f"           radius = {radius:.6f}")
        if normal:
            output_lines.append(
                f"           normal = {format_vertex(normal)}"
            )

    elif opcode == 4:
        # Arc: center, radius, normal, startVector, sweepAngle, arcType
        center, data_offset = read_vertex(data, data_offset)
        if data_offset + 8 <= len(data):
            radius = struct.unpack_from('<d', data, data_offset)[0]
            data_offset += 8
        else:
            radius = None
        normal, data_offset = read_vertex(data, data_offset)
        start_vec, data_offset = read_vertex(data, data_offset)
        if data_offset + 8 <= len(data):
            sweep = struct.unpack_from('<d', data, data_offset)[0]
            data_offset += 8
        else:
            sweep = None
        if data_offset + 4 <= len(data):
            arc_type = struct.unpack_from('<i', data, data_offset)[0]
            data_offset += 4
        else:
            arc_type = None
        if center:
            output_lines.append(
                f"           center     = {format_vertex(center)}"
            )
        if radius is not None:
            output_lines.append(f"           radius     = {radius:.6f}")
        if normal:
            output_lines.append(
                f"           normal     = {format_vertex(normal)}"
            )
        if start_vec:
            output_lines.append(
                f"           startVec   = {format_vertex(start_vec)}"
            )
        if sweep is not None:
            output_lines.append(f"           sweepAngle = {sweep:.6f}")
        if arc_type is not None:
            output_lines.append(f"           arcType    = {arc_type}")

    elif opcode in (6, 7):
        # Polyline / Polygon: vertexCount, затем вершины
        if data_offset + 4 <= len(data):
            vertex_count = struct.unpack_from('<i', data, data_offset)[0]
            data_offset += 4
            output_lines.append(f"           vertexCount = {vertex_count}")
            for vi in range(vertex_count):
                v, data_offset = read_vertex(data, data_offset)
                if v:
                    output_lines.append(
                        f"           v[{vi}] = {format_vertex(v)}"
                    )

    elif opcode == 9:
        # Shell: vertexCount, vertices, faceEntryCount, faces
        if data_offset + 4 <= len(data):
            vertex_count = struct.unpack_from('<i', data, data_offset)[0]
            data_offset += 4
            output_lines.append(f"           vertexCount = {vertex_count}")
            for vi in range(vertex_count):
                v, data_offset = read_vertex(data, data_offset)
                if v:
                    output_lines.append(
                        f"           v[{vi}] = {format_vertex(v)}"
                    )
            if data_offset + 4 <= len(data):
                face_entry_count = struct.unpack_from(
                    '<i', data, data_offset
                )[0]
                data_offset += 4
                output_lines.append(
                    f"           faceEntryCount = {face_entry_count}"
                )
                face_entries = []
                for fi in range(face_entry_count):
                    if data_offset + 4 <= len(data):
                        fe = struct.unpack_from(
                            '<i', data, data_offset
                        )[0]
                        face_entries.append(fe)
                        data_offset += 4
                output_lines.append(
                    f"           faces = {face_entries}"
                )

    elif opcode == 10:
        # Text: insert, normal, direction, height,
        #        widthFactor, oblique, text
        insert, data_offset = read_vertex(data, data_offset)
        normal, data_offset = read_vertex(data, data_offset)
        direction, data_offset = read_vertex(data, data_offset)
        height = width_factor = oblique = None
        if data_offset + 8 <= len(data):
            height = struct.unpack_from('<d', data, data_offset)[0]
            data_offset += 8
        if data_offset + 8 <= len(data):
            width_factor = struct.unpack_from('<d', data, data_offset)[0]
            data_offset += 8
        if data_offset + 8 <= len(data):
            oblique = struct.unpack_from('<d', data, data_offset)[0]
            data_offset += 8
        # Оставшиеся байты - строка текста
        remaining = cmd_end - data_offset
        text_bytes = data[data_offset:data_offset + remaining] \
            if remaining > 0 else b''
        # Попытка декодировать текст
        text_str = ""
        if text_bytes:
            try:
                text_str = text_bytes.decode('utf-8', errors='replace')
            except Exception:
                text_str = repr(text_bytes)
        if insert:
            output_lines.append(
                f"           insert    = {format_vertex(insert)}"
            )
        if normal:
            output_lines.append(
                f"           normal    = {format_vertex(normal)}"
            )
        if direction:
            output_lines.append(
                f"           direction = {format_vertex(direction)}"
            )
        if height is not None:
            output_lines.append(f"           height      = {height:.6f}")
        if width_factor is not None:
            output_lines.append(
                f"           widthFactor = {width_factor:.6f}"
            )
        if oblique is not None:
            output_lines.append(f"           oblique     = {oblique:.6f}")
        if text_str:
            output_lines.append(f"           text = {repr(text_str)}")

    elif opcode in (36, 38):
        # UnicodeText / UnicodeText2: аналогично Text, но строка UTF-16
        insert, data_offset = read_vertex(data, data_offset)
        normal, data_offset = read_vertex(data, data_offset)
        direction, data_offset = read_vertex(data, data_offset)
        height = width_factor = oblique = None
        if data_offset + 8 <= len(data):
            height = struct.unpack_from('<d', data, data_offset)[0]
            data_offset += 8
        if data_offset + 8 <= len(data):
            width_factor = struct.unpack_from('<d', data, data_offset)[0]
            data_offset += 8
        if data_offset + 8 <= len(data):
            oblique = struct.unpack_from('<d', data, data_offset)[0]
            data_offset += 8
        remaining = cmd_end - data_offset
        text_bytes = data[data_offset:data_offset + remaining] \
            if remaining > 0 else b''
        text_str = ""
        if text_bytes:
            try:
                text_str = text_bytes.decode('utf-16-le', errors='replace')
            except Exception:
                text_str = repr(text_bytes)
        if insert:
            output_lines.append(
                f"           insert    = {format_vertex(insert)}"
            )
        if normal:
            output_lines.append(
                f"           normal    = {format_vertex(normal)}"
            )
        if direction:
            output_lines.append(
                f"           direction = {format_vertex(direction)}"
            )
        if height is not None:
            output_lines.append(f"           height      = {height:.6f}")
        if width_factor is not None:
            output_lines.append(
                f"           widthFactor = {width_factor:.6f}"
            )
        if oblique is not None:
            output_lines.append(f"           oblique     = {oblique:.6f}")
        if text_str:
            output_lines.append(f"           text = {repr(text_str)}")

    elif opcode in (14, 16, 18, 19, 22, 23):
        # Атрибут: одно значение int32
        if data_offset + 4 <= len(data):
            value = struct.unpack_from('<i', data, data_offset)[0]
            data_offset += 4
            label = opcode_name
            if opcode == 22:
                # TrueColor — показать в hex
                output_lines.append(
                    f"           value = {value} (0x{value & 0xFFFFFFFF:08X})"
                )
            else:
                output_lines.append(f"           value = {value}")

    elif opcode == 20:
        # SetFill: int32
        if data_offset + 4 <= len(data):
            value = struct.unpack_from('<i', data, data_offset)[0]
            data_offset += 4
            fill_desc = "FILLED" if value == 1 else \
                ("NOT FILLED" if value == 0 else f"unknown({value})")
            output_lines.append(
                f"           fill = {value} ({fill_desc}) "
                f"<<< SETFILL >>>"
            )

    elif opcode == 24:
        # SetLtScale: double
        if data_offset + 8 <= len(data):
            value = struct.unpack_from('<d', data, data_offset)[0]
            data_offset += 8
            output_lines.append(f"           ltScale = {value:.6f}")

    elif opcode == 25:
        # SetThickness: double
        if data_offset + 8 <= len(data):
            value = struct.unpack_from('<d', data, data_offset)[0]
            data_offset += 8
            output_lines.append(f"           thickness = {value:.6f}")

    elif opcode in (29, 30):
        # PushMatrix / PushMatrix2: 16 doubles (4x4 матрица)
        matrix = []
        for mi in range(16):
            if data_offset + 8 <= len(data):
                val = struct.unpack_from('<d', data, data_offset)[0]
                matrix.append(val)
                data_offset += 8
        if len(matrix) == 16:
            output_lines.append(
                f"           <<< PUSH MATRIX >>>"
            )
            for row in range(4):
                vals = matrix[row * 4:(row + 1) * 4]
                output_lines.append(
                    f"           | {vals[0]:12.6f} {vals[1]:12.6f} "
                    f"{vals[2]:12.6f} {vals[3]:12.6f} |"
                )

    elif opcode == 31:
        # PopMatrix: нет данных
        output_lines.append(f"           <<< POP MATRIX >>>")

    elif opcode == 33:
        # LwPolyline: сложный формат
        remaining = cmd_end - data_offset
        output_lines.append(
            f"           (LwPolyline raw data: {remaining} bytes)"
        )
        # Попытка базового декодирования
        if data_offset + 4 <= len(data):
            flags = struct.unpack_from('<i', data, data_offset)[0]
            output_lines.append(f"           flags = 0x{flags:08X}")

    elif opcode == 44:
        # EllipticArc
        remaining = cmd_end - data_offset
        output_lines.append(
            f"           (EllipticArc raw data: {remaining} bytes)"
        )

    return cmd_end


def decode_proxy_graphics(data, output_lines):
    """
    Декодирует полный блок proxy-графики.
    Первые 8 байт: ChunkSize (int32) + CommandCount (int32).
    Затем последовательность команд.
    """
    if len(data) < 8:
        output_lines.append("  [!] Данные слишком короткие (< 8 байт)")
        return

    chunk_size = struct.unpack_from('<i', data, 0)[0]
    command_count = struct.unpack_from('<i', data, 4)[0]

    output_lines.append(f"  ChunkSize    = {chunk_size}")
    output_lines.append(f"  CommandCount = {command_count}")
    output_lines.append(f"  Total bytes  = {len(data)}")
    output_lines.append("")

    offset = 8
    for cmd_idx in range(command_count):
        if offset >= len(data):
            output_lines.append(
                f"  [!] Достигнут конец данных на команде {cmd_idx}"
            )
            break
        offset = decode_command(data, offset, cmd_idx, output_lines)

    if offset < len(data):
        output_lines.append(
            f"  [Оставшиеся байты: {len(data) - offset}]"
        )


def parse_dxf_entities_with_proxy(filepath):
    """
    Парсит DXF-файл и находит все сущности с proxy-графикой (код 310).
    Возвращает список словарей с информацией о каждой сущности.
    """
    with open(filepath, 'r', encoding='utf-8', errors='replace') as f:
        lines = f.readlines()

    entities = []
    current_entity_type = None
    current_entity_start = 0
    current_310_data = []
    in_entity = False
    has_92 = False

    i = 0
    while i < len(lines) - 1:
        code = lines[i].strip()
        value = lines[i + 1].strip()

        if code == '0':
            # Завершаем предыдущую сущность, если у неё есть данные 310
            if in_entity and current_310_data:
                hex_str = ''.join(current_310_data)
                try:
                    binary_data = bytes.fromhex(hex_str)
                except ValueError:
                    binary_data = b''
                entities.append({
                    'type': current_entity_type,
                    'line': current_entity_start + 1,
                    'data': binary_data,
                    'hex_len': len(hex_str),
                })

            # Начинаем новую сущность
            current_entity_type = value
            current_entity_start = i
            current_310_data = []
            in_entity = True
            has_92 = False

        elif code == '310' and in_entity:
            current_310_data.append(value)

        i += 2

    # Обработка последней сущности
    if in_entity and current_310_data:
        hex_str = ''.join(current_310_data)
        try:
            binary_data = bytes.fromhex(hex_str)
        except ValueError:
            binary_data = b''
        entities.append({
            'type': current_entity_type,
            'line': current_entity_start + 1,
            'data': binary_data,
            'hex_len': len(hex_str),
        })

    return entities


def main():
    """Главная функция: обрабатывает оба DXF-файла."""
    base_dir = "/tmp/gh-issue-solver-1774855379383"
    files = [
        os.path.join(base_dir, "testarrow.txt"),
        os.path.join(base_dir, "testSPDSSECTION.txt"),
    ]

    output_lines = []

    for filepath in files:
        filename = os.path.basename(filepath)
        output_lines.append("=" * 80)
        output_lines.append(f"FILE: {filename}")
        output_lines.append("=" * 80)
        output_lines.append("")

        if not os.path.exists(filepath):
            output_lines.append(f"  [!] Файл не найден: {filepath}")
            output_lines.append("")
            continue

        entities = parse_dxf_entities_with_proxy(filepath)

        if not entities:
            output_lines.append("  Сущности с proxy-графикой не найдены.")
            output_lines.append("")
            continue

        for eidx, entity in enumerate(entities):
            output_lines.append("-" * 70)
            output_lines.append(
                f"Entity #{eidx + 1}: {entity['type']} "
                f"(DXF line {entity['line']}, "
                f"{len(entity['data'])} bytes)"
            )
            output_lines.append("-" * 70)

            if len(entity['data']) >= 8:
                # Проверяем, похожи ли данные на proxy-графику
                chunk_size = struct.unpack_from(
                    '<i', entity['data'], 0
                )[0]
                cmd_count = struct.unpack_from(
                    '<i', entity['data'], 4
                )[0]
                # Эвристика: chunk_size должен быть близок к длине
                # данных, cmd_count должен быть разумным
                if (0 < cmd_count < 10000
                        and 0 < chunk_size <= len(entity['data']) * 2):
                    decode_proxy_graphics(
                        entity['data'], output_lines
                    )
                else:
                    output_lines.append(
                        f"  [Пропуск] Не proxy-графика "
                        f"(chunk={chunk_size}, cmds={cmd_count})"
                    )
            else:
                output_lines.append(
                    f"  [!] Недостаточно данных: "
                    f"{len(entity['data'])} байт"
                )
            output_lines.append("")

    # Вывод результата
    result = '\n'.join(output_lines)
    print(result)

    # Сохранение в файл
    log_path = os.path.join(
        base_dir, "experiments", "proxy_decode_output.log"
    )
    with open(log_path, 'w', encoding='utf-8') as f:
        f.write(result)
        f.write('\n')
    print(f"\nOutput saved to {log_path}")


if __name__ == '__main__':
    main()
