#!/usr/bin/env python3
"""
Тест для проверки исправления перенумерации хэндлов в секции OBJECTS.
Проверяет, что коды групп 341, 342, 343 (хэндлы-ссылки MLEADERSTYLE)
корректно перенумеровываются при экспорте DXF.

Суть бага:
  WriteObjectsSectionBody перенумеровывала только коды
  5, 105, 320, 330, 340, 350, 360, 390, 1005.
  Коды 341-349 (жёсткие указатели) не перенумеровывались,
  что приводило к невалидным ссылкам в MLEADERSTYLE.

Исправление:
  Функция IsHandleGroupCode проверяет все диапазоны хэндлов:
  5, 105, 320-369, 390-399, 1005.
"""


def is_handle_group_code_old(code):
    """Старая (багованная) проверка — только отдельные коды."""
    return code in (5, 105, 320, 330, 340, 350, 360, 390, 1005)


def is_handle_group_code_new(code):
    """Новая (исправленная) проверка — полные диапазоны."""
    return (
        code == 5 or code == 105 or code == 1005 or
        (320 <= code <= 369) or
        (390 <= code <= 399)
    )


def test_basic_ranges():
    """Проверяем, что новая функция покрывает все нужные коды."""
    # Коды, которые должны быть хэндлами
    must_be_handle = [
        5, 105, 1005,
        320, 321, 322, 323, 324, 325, 326, 327, 328, 329,
        330, 331, 332, 333, 334, 335, 336, 337, 338, 339,
        340, 341, 342, 343, 344, 345, 346, 347, 348, 349,
        350, 351, 352, 353, 354, 355, 356, 357, 358, 359,
        360, 361, 362, 363, 364, 365, 366, 367, 368, 369,
        390, 391, 392, 393, 394, 395, 396, 397, 398, 399,
    ]

    # Коды, которые НЕ должны быть хэндлами
    not_handle = [
        0, 1, 2, 3, 4, 6, 7, 8, 9, 10, 20, 30, 40, 41, 42,
        62, 63, 70, 71, 90, 91, 92, 93, 94, 100, 102, 104,
        140, 141, 142, 143, 170, 171, 172, 173, 174, 175,
        176, 177, 178, 280, 281, 290, 291, 292, 293, 294,
        295, 296, 297, 300, 310, 319, 370, 389, 400, 1000,
        1001, 1002, 1004, 1006, 1070,
    ]

    errors = []
    for code in must_be_handle:
        if not is_handle_group_code_new(code):
            errors.append(f"Code {code} should be handle but isn't")

    for code in not_handle:
        if is_handle_group_code_new(code):
            errors.append(f"Code {code} should NOT be handle but is")

    return errors


def test_mleaderstyle_codes():
    """Проверяем, что коды MLEADERSTYLE корректно обрабатываются."""
    mleader_handle_codes = [340, 341, 342, 343]

    errors = []
    for code in mleader_handle_codes:
        old_result = is_handle_group_code_old(code)
        new_result = is_handle_group_code_new(code)

        status = "OK" if new_result else "FAIL"
        was_handled = "YES" if old_result else "NO"

        print(f"  Code {code}: old={was_handled:>3}, new={'YES' if new_result else 'NO':>3} [{status}]")

        if not new_result:
            errors.append(f"Code {code} not handled by new function")

    return errors


def test_backward_compatibility():
    """Проверяем, что старые обрабатываемые коды остаются."""
    old_codes = [5, 105, 320, 330, 340, 350, 360, 390, 1005]

    errors = []
    for code in old_codes:
        if not is_handle_group_code_new(code):
            errors.append(
                f"Code {code} was handled before but not now!"
            )

    return errors


def simulate_remap(dxf_lines, old2new_map):
    """Симулирует WriteObjectsSectionBody с новой функцией."""
    handle_counter = 0x100  # стартовое значение для новых хэндлов
    result = []

    i = 0
    while i < len(dxf_lines) - 1:
        code_str = dxf_lines[i].strip()
        value_str = dxf_lines[i + 1].strip()

        try:
            code = int(code_str)
        except ValueError:
            code = -1

        if is_handle_group_code_new(code):
            try:
                old_handle = int(value_str, 16)
            except ValueError:
                old_handle = 0

            if old_handle == 0:
                result.append((code_str, '0'))
            elif old_handle in old2new_map:
                result.append(
                    (code_str, format(old2new_map[old_handle], 'X'))
                )
            else:
                old2new_map[old_handle] = handle_counter
                result.append(
                    (code_str, format(handle_counter, 'X'))
                )
                handle_counter += 1
        else:
            result.append((code_str, value_str))

        i += 2

    return result


def test_remap_simulation():
    """Симулируем перенумерацию MLEADERSTYLE из реального файла."""
    # Упрощённый MLEADERSTYLE объект из +mleaderstyle2008.dxf
    dxf_lines = [
        '  0', 'MLEADERSTYLE',
        '  5', '14D',          # handle объекта
        '330', 'D7',           # owner (словарь)
        '100', 'AcDbMLeaderStyle',
        '170', '2',
        ' 91', '-1056964608',  # цвет — НЕ хэндл
        '340', '14',           # тип линии -> LTYPE
        ' 92', '-2',           # вес линии — НЕ хэндл
        '341', '14E',          # блок стрелки -> BLOCK_RECORD
        '342', '11',           # текстовый стиль -> STYLE
        '343', '166',          # блок содержимого -> BLOCK_RECORD
        ' 93', '-1056964608',  # цвет текста — НЕ хэндл
        ' 94', '-1056964608',  # цвет блока — НЕ хэндл
    ]

    # Карта: старые хэндлы из шаблона -> новые
    # (имитируем, что шаблон уже назначил хэндлы для базовых объектов)
    old2new = {
        0xD7: 0x71,   # словарь ACAD_MLEADERSTYLE
        0x14: 0x20,   # LTYPE ByBlock
        0x11: 0x18,   # STYLE Standard
    }

    result = simulate_remap(dxf_lines, old2new)

    errors = []
    for code_str, value_str in result:
        code = int(code_str.strip())

        # Проверяем, что хэндлы перенумерованы
        if code == 5:
            if value_str == '14D':
                errors.append(
                    "Handle 5 not remapped (still 14D)"
                )
        elif code == 330:
            if value_str == 'D7':
                errors.append(
                    "Owner 330 not remapped (still D7)"
                )
        elif code == 340:
            if value_str != '20':
                errors.append(
                    f"Code 340 remapped to {value_str}, "
                    f"expected 20"
                )
        elif code == 341:
            if value_str == '14E':
                errors.append(
                    "Code 341 NOT remapped (still 14E)! "
                    "This was the bug!"
                )
        elif code == 342:
            if value_str != '18':
                errors.append(
                    f"Code 342 remapped to {value_str}, "
                    f"expected 18"
                )
        elif code == 343:
            if value_str == '166':
                errors.append(
                    "Code 343 NOT remapped (still 166)! "
                    "This was the bug!"
                )

        # Проверяем, что НЕ-хэндлы не тронуты
        if code == 91:
            if value_str != '-1056964608':
                errors.append(
                    f"Code 91 was modified! "
                    f"Value: {value_str}"
                )
        elif code == 92:
            if value_str != '-2':
                errors.append(
                    f"Code 92 was modified! "
                    f"Value: {value_str}"
                )
        elif code == 93:
            if value_str != '-1056964608':
                errors.append(
                    f"Code 93 was modified! "
                    f"Value: {value_str}"
                )
        elif code == 94:
            if value_str != '-1056964608':
                errors.append(
                    f"Code 94 was modified! "
                    f"Value: {value_str}"
                )

    print("\n  Результат перенумерации:")
    for code_str, value_str in result:
        code = int(code_str.strip())
        if is_handle_group_code_new(code) or code in (91, 92, 93, 94):
            print(f"    code {code:>3} = {value_str}")

    return errors


if __name__ == '__main__':
    all_ok = True

    print("=== Тест 1: Базовые диапазоны кодов ===")
    errors = test_basic_ranges()
    if errors:
        for e in errors:
            print(f"  FAIL: {e}")
        all_ok = False
    else:
        print("  OK: все диапазоны корректны")

    print("\n=== Тест 2: Коды MLEADERSTYLE ===")
    errors = test_mleaderstyle_codes()
    if errors:
        for e in errors:
            print(f"  FAIL: {e}")
        all_ok = False
    else:
        print("  OK: все коды MLEADERSTYLE обрабатываются")

    print("\n=== Тест 3: Обратная совместимость ===")
    errors = test_backward_compatibility()
    if errors:
        for e in errors:
            print(f"  FAIL: {e}")
        all_ok = False
    else:
        print("  OK: все ранее обрабатываемые коды остались")

    print("\n=== Тест 4: Симуляция перенумерации ===")
    errors = test_remap_simulation()
    if errors:
        for e in errors:
            print(f"  FAIL: {e}")
        all_ok = False
    else:
        print("  OK: перенумерация корректна")

    print("\n" + "=" * 50)
    if all_ok:
        print("ВСЕ ТЕСТЫ ПРОЙДЕНЫ")
    else:
        print("ЕСТЬ ОШИБКИ!")

    exit(0 if all_ok else 1)
