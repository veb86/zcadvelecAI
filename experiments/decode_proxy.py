#!/usr/bin/env python3
"""Декодер бинарных данных proxy graphic из DXF файла testarrow.txt"""

import struct

# Склеиваем hex данные из group code 310
hex_chunks = [
    "5005000029000000880000001D000000000000000000F03F0000000000000000000000000000000000000000000000000000000000000000000000000000F03F0000000000000000000000000000000000000000000000000000000000000000000000000000F03F0000000000000000000000000000000000000000000000",
    "000000000000000000000000000000F03F0C00000016000000FFFFFFC20C00000013000000000000000C00000016000000000000C00C0000001700000019000000100000001800000000000000000059400C00000012000000FF7F00000C00000014000000010000000C00000016000000FFFFFFC2AC000000090000000500",
    "0000C4588D1A1DCCA140A4C5BA1232E09740000000000000000062AC468DCE08B740A4C5BA1232E09740000000000000000062AC468DCE08B740A4C5BA1232589D400000000000000000C4588D1A1DCCA140A4C5BA1232589D400000000000000000C4588D1A1DCCA140A4C5BA1232E0974000000000000000000600000005",
    "00000000000000010000000200000003000000040000000000000000000000000000000C00000016000000000000C00C0000001400000002000000EC00000026000000C4588D1A1D30A240A4C5BA1232A89840000000000000000000000000000000000000000000000000000000000000F03F000000000000F03F00000000",
    "0000000000000000000000002D043B0435043A044204400438044704350441043A043804350420004904380442044B0400000000FFFFFFFF000000000000000000406F40000000000000F03F0000000000000000000000000000F03F0000000000000000000000000000000000000000000000000000000001000000000000",
    "0000000000430053005F0047006F007300740032003300300034002E007300680078000000000000000C00000016000000000000C00C0000001700000019000000100000001800000000000000000059400C00000012000000FF7F00000C00000016000000000000C00C000000170000001900000010000000180000000000",
    "0000000059400C00000012000000FF7F00000C0000001400000001000000840000000900000003000000909ABA19935F9740E4638ABE20829040000000000000000006A5C0233AF49A404046987AF2A493400000000000000000FCEA0AA1A8C79B40281FE8B5C64E9240000000000000000004000000030000000000000001",
    "00000002000000010000000001000100019000030000000001F589FFFF6B00000000000C00000014000000020000000C00000016000000000000C00C0000001700000019000000100000001800000000000000000059400C00000012000000FF7F00000C00000013000000807F7F7F3C0000000600000002000000909ABA19",
    "935F9740E4638ABE208290400000000000000000C4588D1A1D04A140A4C5BA123218974000000000000000000C00000013000000000000000C00000016000000000000C00C0000001700000019000000100000001800000000000000000059400C00000012000000FF7F00000C00000013000000817F7F7F3C000000060000",
    "0002000000C4588D1A1D04A140A4C5BA1232189740000000000000000062AC468DCE6CB740A4C5BA123218974000000000000000000C0000001300000000000000880000001D000000000000000000F03F00000000000000000000000000000000000000000000F03F0000000000000000000000000000F03F000000000000",
    "0000000000000000000000000000000000000000000000000000000000000000F03F0000000000000000000000000000000000000000000000000000000000000000000000000000F03F080000001F000000080000001F000000",
]

data = bytes.fromhex("".join(hex_chunks))
print(f"Общий размер данных: {len(data)} байт")

# Опкоды из ezdxf/ZCAD
OPCODE_NAMES = {
    1: "EXTENTS",
    2: "CIRCLE",
    3: "CIRCLE_3P",
    4: "CIRCULAR_ARC",
    5: "CIRCULAR_ARC_3P",
    6: "POLYLINE",
    7: "POLYGON",
    8: "MESH",
    9: "SHELL",
    10: "TEXT",
    11: "TEXT2",
    12: "XLINE",
    13: "RAY",
    14: "SET_COLOR",
    15: "UNUSED_15",
    16: "SET_LAYER",
    17: "UNUSED_17",
    18: "SET_LINETYPE",
    19: "SET_MARKER",
    20: "SET_FILL",
    21: "UNUSED_21",
    22: "SET_TRUE_COLOR",
    23: "SET_LINEWEIGHT",
    24: "SET_LTSCALE",
    25: "SET_THICKNESS",
    26: "SET_PLOT_STYLE",
    27: "PUSH_CLIP",
    28: "POP_CLIP",
    29: "PUSH_MATRIX",
    30: "PUSH_MATRIX2",
    31: "POP_MATRIX",
    32: "POLYLINE_WITH_NORMALS",
    33: "LWPOLYLINE",
    34: "SET_MATERIAL",
    35: "SET_MAPPER",
    36: "UNICODE_TEXT",
    38: "UNICODE_TEXT2",
    44: "ELLIPTIC_ARC",
}

offset = 0

# Читаем заголовок proxy graphic
# Первые 8 байт: ChunkSize (4 bytes) + CommandCount (4 bytes)
# Но формат может отличаться, давайте посмотрим

# Пробуем разные варианты разбора заголовка
print(f"\nПервые 16 байт (hex): {data[:16].hex()}")
print(f"Первые 4 int32: {struct.unpack_from('<4i', data, 0)}")

# Из ZCAD кода: ChunkSize и CommandCount
chunk_size = struct.unpack_from('<i', data, 0)[0]
command_count = struct.unpack_from('<i', data, 4)[0]
print(f"\nChunkSize: {chunk_size}")
print(f"CommandCount: {command_count}")

# Теперь разбираем команды
offset = 8
cmd_num = 0

while offset < len(data) - 8:
    cmd_size = struct.unpack_from('<i', data, offset)[0]
    cmd_opcode = struct.unpack_from('<i', data, offset + 4)[0]
    cmd_name = OPCODE_NAMES.get(cmd_opcode, f"UNKNOWN_{cmd_opcode}")

    payload_size = cmd_size - 8  # вычитаем размер заголовка команды
    payload = data[offset + 8 : offset + cmd_size] if payload_size > 0 else b""

    cmd_num += 1
    print(f"\n--- Команда {cmd_num} (смещение {offset}) ---")
    print(f"  Размер: {cmd_size}, Опкод: {cmd_opcode} ({cmd_name})")
    print(f"  Payload ({payload_size} байт): {payload[:64].hex()}{'...' if len(payload) > 64 else ''}")

    # Разбираем payload для некоторых опкодов
    if cmd_opcode == 29:  # PUSH_MATRIX
        if payload_size >= 128:
            matrix = struct.unpack_from('<16d', payload, 0)
            print(f"  Матрица 4x4:")
            for row in range(4):
                vals = matrix[row*4:(row+1)*4]
                print(f"    [{vals[0]:12.6f} {vals[1]:12.6f} {vals[2]:12.6f} {vals[3]:12.6f}]")
    elif cmd_opcode == 14:  # SET_COLOR
        if payload_size >= 4:
            color = struct.unpack_from('<I', payload, 0)[0]
            print(f"  Цвет: {color} (0x{color:08X})")
    elif cmd_opcode == 16:  # SET_LAYER
        if payload_size >= 4:
            layer = struct.unpack_from('<I', payload, 0)[0]
            print(f"  Слой: {layer}")
    elif cmd_opcode == 22:  # SET_TRUE_COLOR
        if payload_size >= 4:
            tc = struct.unpack_from('<I', payload, 0)[0]
            print(f"  True Color: 0x{tc:08X}")
    elif cmd_opcode == 18:  # SET_LINETYPE
        if payload_size >= 4:
            lt = struct.unpack_from('<I', payload, 0)[0]
            print(f"  Linetype: {lt}")
    elif cmd_opcode == 19:  # SET_MARKER
        if payload_size >= 4:
            m = struct.unpack_from('<I', payload, 0)[0]
            print(f"  Marker: {m}")
    elif cmd_opcode == 20:  # SET_FILL
        if payload_size >= 4:
            f = struct.unpack_from('<I', payload, 0)[0]
            print(f"  Fill: {f} ({'ON' if f else 'OFF'})")
    elif cmd_opcode == 23:  # SET_LINEWEIGHT
        if payload_size >= 4:
            lw = struct.unpack_from('<I', payload, 0)[0]
            print(f"  Lineweight: {lw}")
    elif cmd_opcode == 24:  # SET_LTSCALE
        if payload_size >= 8:
            ls = struct.unpack_from('<d', payload, 0)[0]
            print(f"  LtScale: {ls}")
    elif cmd_opcode == 25:  # SET_THICKNESS
        if payload_size >= 8:
            th = struct.unpack_from('<d', payload, 0)[0]
            print(f"  Thickness: {th}")
    elif cmd_opcode == 6:  # POLYLINE
        if payload_size >= 4:
            vc = struct.unpack_from('<i', payload, 0)[0]
            print(f"  Кол-во вершин: {vc}")
            for i in range(min(vc, 10)):
                if 4 + i*24 + 24 <= payload_size:
                    x, y, z = struct.unpack_from('<3d', payload, 4 + i*24)
                    print(f"    Вершина {i}: ({x:.4f}, {y:.4f}, {z:.4f})")
    elif cmd_opcode == 7:  # POLYGON
        if payload_size >= 4:
            vc = struct.unpack_from('<i', payload, 0)[0]
            print(f"  Кол-во вершин: {vc}")
            for i in range(min(vc, 10)):
                if 4 + i*24 + 24 <= payload_size:
                    x, y, z = struct.unpack_from('<3d', payload, 4 + i*24)
                    print(f"    Вершина {i}: ({x:.4f}, {y:.4f}, {z:.4f})")
    elif cmd_opcode == 9:  # SHELL
        if payload_size >= 4:
            vc = struct.unpack_from('<i', payload, 0)[0]
            print(f"  Кол-во вершин: {vc}")
            for i in range(min(vc, 10)):
                if 4 + i*24 + 24 <= payload_size:
                    x, y, z = struct.unpack_from('<3d', payload, 4 + i*24)
                    print(f"    Вершина {i}: ({x:.4f}, {y:.4f}, {z:.4f})")
            # После вершин — грани
            faces_offset = 4 + vc * 24
            if faces_offset + 4 <= payload_size:
                fc = struct.unpack_from('<i', payload, faces_offset)[0]
                print(f"  Кол-во записей граней: {fc}")
                fo = faces_offset + 4
                face_num = 0
                while fo < payload_size and face_num < 10:
                    edge_count = struct.unpack_from('<i', payload, fo)[0]
                    fo += 4
                    if edge_count > 0:
                        indices = []
                        for j in range(edge_count):
                            if fo + 4 <= payload_size:
                                idx = struct.unpack_from('<I', payload, fo)[0]
                                indices.append(idx)
                                fo += 4
                        print(f"    Грань {face_num}: рёбер={edge_count}, индексы={indices}")
                    face_num += 1
    elif cmd_opcode == 38:  # UNICODE_TEXT2
        if payload_size >= 72:
            px, py, pz = struct.unpack_from('<3d', payload, 0)
            nx, ny, nz = struct.unpack_from('<3d', payload, 24)
            dx, dy, dz = struct.unpack_from('<3d', payload, 48)
            print(f"  Position: ({px:.4f}, {py:.4f}, {pz:.4f})")
            print(f"  Normal: ({nx:.4f}, {ny:.4f}, {nz:.4f})")
            print(f"  Direction: ({dx:.4f}, {dy:.4f}, {dz:.4f})")
            # Пробуем прочитать текст
            text_offset = 72
            # UTF-16 строка
            text_chars = []
            while text_offset + 2 <= payload_size:
                ch = struct.unpack_from('<H', payload, text_offset)[0]
                text_offset += 2
                if ch == 0:
                    break
                text_chars.append(chr(ch))
            text = "".join(text_chars)
            print(f"  Текст: '{text}'")
            # Выравнивание до 4 байт
            if text_offset % 4 != 0:
                text_offset += 4 - (text_offset % 4)
            # Оставшиеся данные
            if text_offset + 4 <= payload_size:
                ignore_len = struct.unpack_from('<i', payload, text_offset)[0]
                print(f"  IgnoreLen: {ignore_len}")
                text_offset += 4
            if text_offset + 4 <= payload_size:
                raw_val = struct.unpack_from('<i', payload, text_offset)[0]
                print(f"  RawValue: {raw_val}")
                text_offset += 4
            if text_offset + 8 <= payload_size:
                height = struct.unpack_from('<d', payload, text_offset)[0]
                print(f"  Height: {height}")
                text_offset += 8
    elif cmd_opcode == 31:  # POP_MATRIX
        print(f"  (без данных)")
    elif cmd_opcode == 1:  # EXTENTS
        if payload_size >= 48:
            x1, y1, z1 = struct.unpack_from('<3d', payload, 0)
            x2, y2, z2 = struct.unpack_from('<3d', payload, 24)
            print(f"  Min: ({x1:.4f}, {y1:.4f}, {z1:.4f})")
            print(f"  Max: ({x2:.4f}, {y2:.4f}, {z2:.4f})")

    offset += cmd_size
    if cmd_size <= 0:
        print("  ОШИБКА: нулевой или отрицательный размер команды!")
        break

print(f"\n=== Итого: {cmd_num} команд, обработано {offset} из {len(data)} байт ===")
