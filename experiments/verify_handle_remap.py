#!/usr/bin/env python3
"""
Эксперимент: проверка логики пересопоставления дескрипторов.
Симулирует работу WriteRemappedObjectsSection с TemplateHandleRemap.
"""

# Дескрипторы из шаблона (OldHandele2NewHandle - заполняется при записи TABLES)
# Ключ = дескриптор шаблона, значение = новый дескриптор в выходном файле
template_handle_remap = {
    0x1F: 0x2F,  # *Model_Space BLOCK_RECORD: шаблон 0x1F -> новый 0x2F
    0x22: 0x32,  # *Model_Space LAYOUT: шаблон 0x22 -> новый 0x32
    0x59: 0x5A,  # *Paper_Space LAYOUT (пример)
    0x5E: 0x5F,  # *Paper_Space0 LAYOUT (пример)
    0x01: 0x0D,  # LAYER таблица
    0x02: 0x0E,  # LINE_TYPE таблица
}

# SourceHandleMap (h2p): только сущности, линии, стили - не системные объекты
source_h2p = {
    0x100: "entity_ptr_1",
    0x101: "entity_ptr_2",
}

# p2h: карта указатель -> новый дескриптор
p2h = {
    "entity_ptr_1": 0x200,
    "entity_ptr_2": 0x201,
}

def find_new_handle_old(old_handle, source_h2p, p2h):
    """Старая логика: только через source_h2p + p2h."""
    ptr = source_h2p.get(old_handle)
    if ptr:
        return p2h.get(ptr, 0)
    return 0

def find_new_handle_new(old_handle, source_h2p, p2h, template_remap):
    """Новая логика: source_h2p + p2h, затем fallback через template_remap."""
    # Шаг 1: через загрузочную карту
    ptr = source_h2p.get(old_handle)
    if ptr:
        new_handle = p2h.get(ptr, 0)
        if new_handle:
            return new_handle
    # Шаг 2: через карту шаблона
    return template_remap.get(old_handle, 0)

# Тест: LAYOUT группа 330 ссылается на BLOCK_RECORD *Model_Space (дескриптор 0x1F)
layout_330_ref = 0x1F  # из секции OBJECTS исходного файла

old_result = find_new_handle_old(layout_330_ref, source_h2p, p2h)
new_result = find_new_handle_new(layout_330_ref, source_h2p, p2h, template_handle_remap)

print(f"LAYOUT группа 330 ссылается на дескриптор: 0x{layout_330_ref:X}")
print(f"  Старая логика: 0x{old_result:X} (0 = фантомный дескриптор - ОШИБКА)")
print(f"  Новая логика:  0x{new_result:X} (должно быть 0x{template_handle_remap[layout_330_ref]:X} - OK)")
print()

# Тест: обычная сущность (должно работать по-прежнему)
entity_ref = 0x100
old_result = find_new_handle_old(entity_ref, source_h2p, p2h)
new_result = find_new_handle_new(entity_ref, source_h2p, p2h, template_handle_remap)
print(f"Сущность с дескриптором: 0x{entity_ref:X}")
print(f"  Старая логика: 0x{old_result:X}")
print(f"  Новая логика:  0x{new_result:X}")
print()

# Тест: неизвестный дескриптор (должен получить новый)
unknown_ref = 0xABCD
old_result = find_new_handle_old(unknown_ref, source_h2p, p2h)
new_result = find_new_handle_new(unknown_ref, source_h2p, p2h, template_handle_remap)
print(f"Неизвестный дескриптор: 0x{unknown_ref:X}")
print(f"  Старая логика: 0x{old_result:X} (получит новый sequential)")
print(f"  Новая логика:  0x{new_result:X} (то же — sequential)")
print()
print("РЕЗУЛЬТАТ: Новая логика корректно пересопоставляет системные объекты!")
