#!/usr/bin/env python3
"""
Верификация матричной трансформации для proxy объектов.
Проверяет, что транспонирование матрицы и применение VectorTransform3D
даёт правильные мировые координаты для тестовых файлов.
"""

import struct


def read_proxy_data(filename):
    """Читает hex-данные proxy из DXF файла."""
    hex_data = ''
    in_proxy = False
    with open(filename) as f:
        for line in f:
            line = line.strip()
            if line == 'ACAD_PROXY_ENTITY' or line in ('SPDSNOTEPOSITION', 'SPDSSECTION'):
                in_proxy = True
            if in_proxy and line == '310':
                hex_line = next(f).strip()
                hex_data += hex_line
            if in_proxy and line == '0':
                if hex_data:
                    break
    return bytes.fromhex(hex_data)


def transpose_matrix(raw_doubles):
    """
    Транспонирует матрицу из формата proxy (row-major, translation в col 3)
    в формат ZCAD (row-major, translation в row 3).
    """
    result = [[0.0]*4 for _ in range(4)]
    for row in range(4):
        for col in range(4):
            # ZCAD[row][col] = proxy[col][row] = raw[col*4 + row]
            result[row][col] = raw_doubles[col * 4 + row]
    return result


def transform_point(point, matrix):
    """
    VectorTransform3D: P' = P * M (row-vector convention).
    point = (x, y, z)
    matrix = 4x4 (translation in row 3)
    """
    x, y, z = point
    rx = x * matrix[0][0] + y * matrix[1][0] + z * matrix[2][0] + matrix[3][0]
    ry = x * matrix[0][1] + y * matrix[1][1] + z * matrix[2][1] + matrix[3][1]
    rz = x * matrix[0][2] + y * matrix[1][2] + z * matrix[2][2] + matrix[3][2]
    return (rx, ry, rz)


def print_matrix(m, label):
    print(f"\n{label}:")
    for row in range(4):
        vals = ' '.join(f'{m[row][col]:12.4f}' for col in range(4))
        print(f"  | {vals} |")


def test_spdssection():
    """Тест для testSPDSSECTION.txt — проверяет координаты прямоугольников."""
    data = read_proxy_data('testSPDSSECTION.txt')

    # Декодируем proxy data
    offset = 0
    chunk_size = struct.unpack_from('<i', data, offset)[0]; offset += 4
    cmd_count = struct.unpack_from('<i', data, offset)[0]; offset += 4
    print(f"ChunkSize={chunk_size}, CommandCount={cmd_count}")

    # Пройдём по командам и найдём PushMatrix + Shell пары
    for cmd_idx in range(cmd_count):
        if offset >= len(data):
            break
        cmd_size = struct.unpack_from('<i', data, offset)[0]
        opcode = struct.unpack_from('<i', data, offset + 4)[0]

        if opcode == 29:  # PushMatrix
            raw = struct.unpack_from('<16d', data, offset + 8)
            m = transpose_matrix(raw)
            tx, ty, tz = m[3][0], m[3][1], m[3][2]
            if abs(tx) > 1 or abs(ty) > 1:
                print_matrix(m, f"Cmd[{cmd_idx}] PushMatrix (ZCAD format)")

        elif opcode == 9:  # Shell
            n_verts = struct.unpack_from('<i', data, offset + 8)[0]
            verts = []
            for i in range(n_verts):
                v = struct.unpack_from('<3d', data, offset + 12 + i*24)
                verts.append(v)

            # Проверяем, нужна ли трансформация (вершины около 0)
            if all(abs(v[0]) < 1000 and abs(v[1]) < 1000 for v in verts):
                print(f"\nCmd[{cmd_idx}] Shell ({n_verts} verts, LOCAL coords):")
                for i, v in enumerate(verts):
                    print(f"  v[{i}] = ({v[0]:.6f}, {v[1]:.6f}, {v[2]:.6f})")

                # Ищем последнюю PushMatrix с ненулевой трансляцией
                # (пройдём заново до текущей позиции)
                off2 = 8  # после header
                last_matrix = None
                matrix_stack = []
                for j in range(cmd_idx):
                    if off2 >= len(data):
                        break
                    cs2 = struct.unpack_from('<i', data, off2)[0]
                    op2 = struct.unpack_from('<i', data, off2 + 4)[0]
                    if op2 == 29:  # PushMatrix
                        raw2 = struct.unpack_from('<16d', data, off2 + 8)
                        m2 = transpose_matrix(raw2)
                        matrix_stack.append(m2)
                    elif op2 == 31:  # PopMatrix
                        if matrix_stack:
                            matrix_stack.pop()
                    off2 += cs2

                if matrix_stack:
                    m = matrix_stack[-1]
                    print(f"\n  Applying top matrix (tx={m[3][0]:.3f}, ty={m[3][1]:.3f}):")
                    for i, v in enumerate(verts):
                        tv = transform_point(v, m)
                        print(f"  v[{i}] transformed = ({tv[0]:.4f}, {tv[1]:.4f}, {tv[2]:.4f})")
            else:
                print(f"\nCmd[{cmd_idx}] Shell ({n_verts} verts, WORLD coords):")
                for i, v in enumerate(verts):
                    print(f"  v[{i}] = ({v[0]:.6f}, {v[1]:.6f}, {v[2]:.6f})")

        offset += cmd_size


def test_expected_coordinates():
    """Проверяем, что трансформированные координаты совпадают с ожидаемыми."""
    print("\n" + "="*70)
    print("ПРОВЕРКА ОЖИДАЕМЫХ КООРДИНАТ (из issue comment #2)")
    print("="*70)

    # Матрица трансляции для cmd[13]: (17191.646332, 24705.692980, 0)
    tx1, ty1 = 17191.646332, 24705.692980
    # Локальные вершины cmd[16] Shell:
    local_verts_1 = [
        (308.708058, 394.872556, 0),
        (250.684875, 434.030061, 0),
        (-308.708058, -394.872556, 0),
        (-250.684875, -434.030061, 0),
    ]
    # Ожидаемые WCS:
    expected_1 = [
        (16882.9383, 24310.8204),
        (17442.3312, 25139.7230),
        (17500.3544, 25100.5655),
        (16940.9615, 24271.6629),
    ]

    # Строим ZCAD-формат матрицу
    m = [[0]*4 for _ in range(4)]
    m[0][0] = 1; m[1][1] = 1; m[2][2] = 1; m[3][3] = 1
    m[3][0] = tx1; m[3][1] = ty1

    print(f"\nТрансляция 1: ({tx1}, {ty1})")
    all_ok = True
    for i, lv in enumerate(local_verts_1):
        tv = transform_point(lv, m)
        ex = expected_1[i]
        dx = abs(tv[0] - ex[0])
        dy = abs(tv[1] - ex[1])
        ok = dx < 0.01 and dy < 0.01
        status = "OK" if ok else "FAIL"
        print(f"  v[{i}]: local=({lv[0]:.4f}, {lv[1]:.4f}) -> "
              f"WCS=({tv[0]:.4f}, {tv[1]:.4f}) "
              f"expected=({ex[0]:.4f}, {ex[1]:.4f}) [{status}]")
        if not ok:
            all_ok = False

    # Матрица трансляции для cmd[24]: (2148.788699, 2415.343340, 0)
    tx2, ty2 = 2148.788699, 2415.343340
    local_verts_2 = [
        (-308.708058, -394.872556, 0),
        (-250.684875, -434.030061, 0),
        (308.708058, 394.872556, 0),
        (250.684875, 434.030061, 0),
    ]
    expected_2 = [
        (1840.0806, 2020.4708),
        (2399.4736, 2849.3734),
        (2457.4968, 2810.2159),
        (1898.1038, 1981.3133),
    ]

    m2 = [[0]*4 for _ in range(4)]
    m2[0][0] = 1; m2[1][1] = 1; m2[2][2] = 1; m2[3][3] = 1
    m2[3][0] = tx2; m2[3][1] = ty2

    print(f"\nТрансляция 2: ({tx2}, {ty2})")
    for i, lv in enumerate(local_verts_2):
        tv = transform_point(lv, m2)
        ex = expected_2[i]
        dx = abs(tv[0] - ex[0])
        dy = abs(tv[1] - ex[1])
        ok = dx < 0.01 and dy < 0.01
        status = "OK" if ok else "FAIL"
        print(f"  v[{i}]: local=({lv[0]:.4f}, {lv[1]:.4f}) -> "
              f"WCS=({tv[0]:.4f}, {tv[1]:.4f}) "
              f"expected=({ex[0]:.4f}, {ex[1]:.4f}) [{status}]")
        if not ok:
            all_ok = False

    print(f"\n{'='*70}")
    print(f"Результат: {'ВСЕ ТЕСТЫ ПРОЙДЕНЫ' if all_ok else 'ЕСТЬ ОШИБКИ'}")
    print(f"{'='*70}")
    return all_ok


if __name__ == '__main__':
    import os
    os.chdir('/tmp/gh-issue-solver-1774855379383')

    print("="*70)
    print("АНАЛИЗ testSPDSSECTION.txt")
    print("="*70)
    test_spdssection()

    test_expected_coordinates()
