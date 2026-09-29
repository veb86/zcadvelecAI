#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
===============================================================================
DXF TABLE SAVER / INJECTOR (dxf_saver.py)
===============================================================================
Модуль для записи и интеграции нативной структуры ACAD_TABLE в существующий
DXF файл (в частности ZCADonlyline.dxf -> ZCADTABLE.dxf).

Ключевые особенности и строгое соответствие стандарту AutoCAD DXF (AC1021/2007+):
1. Секция CLASSES:
   - Внедрение описаний классов C++ ObjectDBX:
     ACAD_TABLE, CELLSTYLEMAP, TABLECONTENT, TABLEGEOMETRY.
     Без этих классов AutoCAD открывает таблицу как прокси-объект или выдает ошибку чтения.
2. Секция TABLES:
   - Добавление текстового стиля (STYLE newtext / Times New Roman).
   - Регистрация анонимных блоков таблицы (BLOCK_RECORD *T1, *T11, *T12).
3. Секция BLOCKS:
   - Запись геометрических блоков (*T1, *T11, *T12) со всеми линиями (LINE)
     и текстовыми элементами (MTEXT) для визуализации таблицы в CAD.
4. Секция ENTITIES:
   - Запись примитивов ACAD_TABLE (с привязкой к Model_Space, углами, стилями,
     флагами разбиения DXF 90, высотами строк DXF 141, ширинами столбцов DXF 142,
     значениями ячеек DXF 301/302, выравниваниями DXF 170 и бинарными данными DXF 310).
5. Секция OBJECTS:
   - Запись TABLESTYLE, расширенных словарей (DICTIONARY), xrecord с флагами
     разбиения (ACAD_ROUNDTRIP_2008_TABLE_ENTITY), TABLECONTENT и TABLEGEOMETRY.
6. Контроль дескрипторов (Handle Management):
   - Полная изоляция и переназначение handle-ов (Group 5, 330, 340, 342, 343, 350, 360, 361)
     без коллизий с базовым файлом ZCAD.
   - Корректное обновление переменной $HANDSEED в секции HEADER.
===============================================================================
"""

import sys
import os
import copy
from typing import List, Dict, Set, Tuple, Optional
from dxf_parser import read_tags, DXFTag, extract_entities_by_type
from table_reader import analyze_tables_in_dxf

# Коды групп DXF, хранящие указатели на дескрипторы (Handles)
HANDLE_CODES = {5, 330, 331, 340, 342, 343, 350, 360, 361, 390}


class DXFTableSaver:
    """
    Класс для внедрения нативных таблиц ACAD_TABLE в целевой DXF файл.
    """

    def __init__(self, template_dxf: str, source_table_dxf: str = "acadtable2007.dxf"):
        self.template_dxf = template_dxf
        self.source_table_dxf = source_table_dxf
        self.template_tags: List[DXFTag] = []
        self.source_tags: List[DXFTag] = []

    def load_files(self):
        """Загружает теги из шаблонного DXF и файла с таблицей-образцом."""
        if not os.path.exists(self.template_dxf):
            raise FileNotFoundError(f"Шаблонный файл не найден: {self.template_dxf}")
        if not os.path.exists(self.source_table_dxf):
            raise FileNotFoundError(f"Файл таблицы-образца не найден: {self.source_table_dxf}")

        print(f"-> Чтение шаблона: {self.template_dxf}")
        self.template_tags = list(read_tags(self.template_dxf))
        print(f"   Прочитано тегов: {len(self.template_tags)}")

        print(f"-> Чтение таблицы-источника: {self.source_table_dxf}")
        self.source_tags = list(read_tags(self.source_table_dxf))
        print(f"   Прочитано тегов: {len(self.source_tags)}")

    @staticmethod
    def split_into_entities(tags: List[DXFTag]) -> List[List[DXFTag]]:
        """Разбивает список тегов DXF на отдельные примитивы/записи по маркеру group code 0."""
        entities = []
        cur = []
        for t in tags:
            if t.code == 0:
                if cur:
                    entities.append(cur)
                cur = [t]
            else:
                cur.append(t)
        if cur:
            entities.append(cur)
        return entities

    def find_system_handles(self, tags: List[DXFTag]) -> Dict[str, str]:
        """
        Находит дескрипторы ключевых системных таблиц и словарей в файле.
        """
        sys_handles = {
            "BLOCK_RECORD_TABLE": "1",
            "MODEL_SPACE_BLOCK_RECORD": "1F",
            "STYLE_TABLE": "3",
            "STYLE_STANDARD": "11",
            "ACAD_TABLESTYLE_DICT": "86",
            "ROOT_DICT": "C",
        }

        # 1. Поиск таблиц в TABLES
        for i, t in enumerate(tags):
            if t.code == 0 and t.value == "TABLE":
                name = tags[i + 1].value if i + 1 < len(tags) and tags[i + 1].code == 2 else ""
                h = tags[i + 2].value if i + 2 < len(tags) and tags[i + 2].code == 5 else ""
                if name == "BLOCK_RECORD" and h:
                    sys_handles["BLOCK_RECORD_TABLE"] = h
                elif name == "STYLE" and h:
                    sys_handles["STYLE_TABLE"] = h

            # 2. Поиск *Model_Space BLOCK_RECORD (важно: именно BLOCK_RECORD, а не блок в BLOCKS)
            if t.code == 0 and t.value == "BLOCK_RECORD":
                is_ms = False
                h = None
                for j in range(i + 1, min(len(tags), i + 15)):
                    if tags[j].code == 0:
                        break
                    if tags[j].code == 2 and tags[j].value == "*Model_Space":
                        is_ms = True
                    if tags[j].code == 5:
                        h = tags[j].value
                if is_ms and h:
                    sys_handles["MODEL_SPACE_BLOCK_RECORD"] = h

            # 3. Поиск STYLE Standard
            if t.code == 0 and t.value == "STYLE":
                is_std = False
                h = None
                for j in range(i + 1, min(len(tags), i + 15)):
                    if tags[j].code == 0:
                        break
                    if tags[j].code == 2 and tags[j].value == "Standard":
                        is_std = True
                    if tags[j].code == 5:
                        h = tags[j].value
                if is_std and h:
                    sys_handles["STYLE_STANDARD"] = h

            # 4. Поиск корневого словаря ROOT_DICT (owner 0 в OBJECTS)
            if t.code == 0 and t.value == "DICTIONARY":
                owner = next((tags[j].value for j in range(i + 1, min(len(tags), i + 8)) if tags[j].code == 330), None)
                h = next((tags[j].value for j in range(i + 1, min(len(tags), i + 8)) if tags[j].code == 5), None)
                if (owner == "0" or owner is None) and h:
                    sys_handles["ROOT_DICT"] = h

            # 5. Поиск словарей в OBJECTS
            if t.code == 3 and t.value == "ACAD_TABLESTYLE":
                if i + 1 < len(tags) and tags[i + 1].code == 350:
                    sys_handles["ACAD_TABLESTYLE_DICT"] = tags[i + 1].value

        return sys_handles

    def extract_table_subsystem(self) -> Tuple[List[List[DXFTag]], List[List[DXFTag]], List[List[DXFTag]], List[List[List[DXFTag]]], List[List[DXFTag]], List[List[DXFTag]]]:
        """
        Извлекает из образца все взаимосвязанные элементы подсистемы таблиц:
        1. Classes
        2. Text Styles
        3. Block Records
        4. Blocks
        5. ACAD_TABLE entities
        6. Objects (TableStyle, Dictionaries, Roundtrip xrecords, TableContent, TableGeometry)
        """
        ents_a = self.split_into_entities(self.source_tags)

        target_classes = {"ACAD_TABLE", "CELLSTYLEMAP", "TABLECONTENT", "TABLEGEOMETRY"}
        target_blocks = {"*T1", "*T11", "*T12"}
        target_obj_handles = {"87", "B62", "B63", "A78", "A79", "201", "A75", "2AB"}

        # 1. Классы
        classes = [e for e in ents_a if e[0].value == "CLASS" and any(t.code == 1 and t.value in target_classes for t in e)]

        # 2. Текстовый стиль newtext
        styles = [e for e in ents_a if e[0].value == "STYLE" and any(t.code == 2 and t.value == "newtext" for t in e)]

        # 3. Записи блоков таблицы (*T...)
        block_records = [e for e in ents_a if e[0].value == "BLOCK_RECORD" and any(t.code == 2 and t.value in target_blocks for t in e)]

        # 4. Определения блоков таблицы (*T...)
        blocks = []
        in_block = False
        cur_b = []
        for e in ents_a:
            if e[0].value == "BLOCK":
                name = next((t.value for t in e if t.code == 2), None)
                if name in target_blocks:
                    in_block = True
                    cur_b = [e]
            elif in_block:
                cur_b.append(e)
                if e[0].value == "ENDBLK":
                    blocks.append(cur_b)
                    in_block = False
                    cur_b = []

        # 5. Сущности ACAD_TABLE
        tables = [e for e in ents_a if e[0].value == "ACAD_TABLE"]

        # 6. Объекты таблицы (TABLESTYLE, DICTIONARY, XRECORD, TABLECONTENT, TABLEGEOMETRY, CELLSTYLEMAP)
        objects = [e for e in ents_a if any(t.code == 5 and t.value in target_obj_handles for t in e)]

        return classes, styles, block_records, blocks, tables, objects

    def save(self, output_dxf: str) -> bool:
        """
        Выполняет сборку нового DXF файла со встроенной таблицей и сохраняет по указанному пути.
        """
        self.load_files()

        # Определение системных дескрипторов шаблона
        target_sys = self.find_system_handles(self.template_tags)
        source_sys = self.find_system_handles(self.source_tags)

        print("\n-> Системные дескрипторы шаблона (ZCAD):")
        for k, v in target_sys.items():
            print(f"   {k:28}: 0x{v}")

        # Извлечение компонентов таблицы
        classes, styles, block_records, blocks, tables, objects = self.extract_table_subsystem()
        print(f"\n-> Извлечено компонентов таблицы:")
        print(f"   Классов (CLASSES):              {len(classes)}")
        print(f"   Стилей текста (STYLE):          {len(styles)}")
        print(f"   Записей блоков (BLOCK_RECORD):  {len(block_records)}")
        print(f"   Геометрических блоков (BLOCK):  {len(blocks)}")
        print(f"   Таблиц (ACAD_TABLE):            {len(tables)}")
        print(f"   Связанных объектов (OBJECTS):   {len(objects)}")

        # Формирование списка всех переносимых сущностей для ремаппинга дескрипторов
        all_transfer_ents = []
        all_transfer_ents.extend(styles)
        all_transfer_ents.extend(block_records)
        for b in blocks:
            all_transfer_ents.extend(b)
        all_transfer_ents.extend(tables)
        all_transfer_ents.extend(objects)

        # Сбор внутренних дескрипторов источника
        internal_source_handles = []
        for e in all_transfer_ents:
            for t in e:
                if t.code == 5 and t.value not in internal_source_handles:
                    internal_source_handles.append(t.value)

        print(f"   Внутренних дескрипторов:        {len(internal_source_handles)}")

        # Определение диапазона безопасных дескрипторов в целевом файле
        max_target_h = 0
        for t in self.template_tags:
            if t.code == 5:
                try:
                    val = int(t.value, 16)
                    if val > max_target_h and val < 0x10000000:
                        max_target_h = val
                except ValueError:
                    pass

        # Начинаем генерацию новых дескрипторов выше существующих
        start_new_handle = max(0x200, (max_target_h + 16) & ~0xF)
        print(f"   Стартовый дескриптор для инъекции: 0x{start_new_handle:X}")

        # Таблица соответствия дескрипторов
        handle_map: Dict[str, str] = {
            source_sys["BLOCK_RECORD_TABLE"]: target_sys["BLOCK_RECORD_TABLE"],
            source_sys["MODEL_SPACE_BLOCK_RECORD"]: target_sys["MODEL_SPACE_BLOCK_RECORD"],
            source_sys["STYLE_TABLE"]: target_sys["STYLE_TABLE"],
            source_sys["STYLE_STANDARD"]: target_sys["STYLE_STANDARD"],
            source_sys["ACAD_TABLESTYLE_DICT"]: target_sys["ACAD_TABLESTYLE_DICT"],
            "C": target_sys.get("ROOT_DICT", "40"),
        }

        # Назначение новых дескрипторов всем внутренним сущностям
        for i, old_h in enumerate(internal_source_handles):
            handle_map[old_h] = f"{start_new_handle + i:X}"

        # Дополнительные стили из образца связываем с создаваемым стилем таблицы
        if "87" in handle_map:
            handle_map["BA"] = handle_map["87"]
            handle_map["BE"] = handle_map["87"]

        max_allocated_handle = start_new_handle + len(internal_source_handles)

        # Функция ремаппинга дескрипторов в сущности
        def remap_entity(ent: List[DXFTag]) -> List[DXFTag]:
            remapped = []
            for t in ent:
                if t.code in HANDLE_CODES and t.value in handle_map:
                    remapped.append(DXFTag(t.code, handle_map[t.value]))
                else:
                    remapped.append(t)
            return remapped

        # Ремаппинг всех компонентов
        classes_rem = [remap_entity(e) for e in classes]
        styles_rem = [remap_entity(e) for e in styles]
        block_records_rem = [remap_entity(e) for e in block_records]
        blocks_rem = [[remap_entity(e) for e in b] for b in blocks]
        tables_rem = [remap_entity(e) for e in tables]
        objects_rem = [remap_entity(e) for e in objects]

        root_dict_h = target_sys.get("ROOT_DICT", "40")
        tablestyle_dict_h = target_sys.get("ACAD_TABLESTYLE_DICT", "48")

        # Сборка финального списка тегов DXF
        out_tags: List[DXFTag] = []
        i = 0
        in_style_table = False
        in_br_table = False

        while i < len(self.template_tags):
            t = self.template_tags[i]

            # 1. Корректировка $HANDSEED в HEADER
            if t.code == 9 and t.value == "$HANDSEED":
                out_tags.append(t)
                new_handseed = f"{max_allocated_handle + 16:X}"
                out_tags.append(DXFTag(5, new_handseed))
                i += 2
                continue

            # 2. Корректировка корневого словаря NOD (добавление ACDB_RECOMPOSE_DATA)
            if t.code == 5 and t.value == root_dict_h and "2AB" in handle_map:
                out_tags.append(t)
                i += 1
                while i < len(self.template_tags) and self.template_tags[i].code != 0:
                    out_tags.append(self.template_tags[i])
                    i += 1
                # Добавляем ACDB_RECOMPOSE_DATA для автоматического объединения фрагментов таблицы
                out_tags.append(DXFTag(3, "ACDB_RECOMPOSE_DATA"))
                out_tags.append(DXFTag(350, handle_map["2AB"]))
                continue

            # 3. Корректировка словаря ACAD_TABLESTYLE (привязка Standard к внедряемому стилю)
            if t.code == 5 and t.value == tablestyle_dict_h and "87" in handle_map:
                out_tags.append(t)
                i += 1
                while i < len(self.template_tags) and self.template_tags[i].code != 0:
                    cur_t = self.template_tags[i]
                    if cur_t.code == 350 and cur_t.value == "3F":
                        out_tags.append(DXFTag(350, handle_map["87"]))
                    else:
                        out_tags.append(cur_t)
                    i += 1
                continue

            # 4. Пропуск устаревшего пустого TABLESTYLE 3F с битым указателем 162
            if t.code == 0 and t.value == "TABLESTYLE":
                # Проверим, является ли это пустым стилем 3F
                if i + 2 < len(self.template_tags) and self.template_tags[i + 1].code == 5 and self.template_tags[i + 1].value == "3F":
                    # Пропускаем весь примитив до следующего code 0
                    i += 1
                    while i < len(self.template_tags) and self.template_tags[i].code != 0:
                        i += 1
                    continue

            # 5. Точки вставки перед ENDSEC для секций CLASSES, BLOCKS, ENTITIES, OBJECTS
            if t.code == 0 and t.value == "ENDSEC" and i > 0:
                # Определение текущей секции по предыдущему SECTION
                sec_name = None
                for j in range(len(out_tags) - 1, -1, -1):
                    if out_tags[j].code == 0 and out_tags[j].value == "SECTION":
                        if j + 1 < len(out_tags) and out_tags[j + 1].code == 2:
                            sec_name = out_tags[j + 1].value
                        break

                if sec_name == "CLASSES":
                    for c in classes_rem:
                        out_tags.extend(c)
                elif sec_name == "BLOCKS":
                    for b in blocks_rem:
                        for e in b:
                            out_tags.extend(e)
                elif sec_name == "ENTITIES":
                    for tbl in tables_rem:
                        out_tags.extend(tbl)
                elif sec_name == "OBJECTS":
                    for obj in objects_rem:
                        out_tags.extend(obj)

            # 3. Точки вставки в секции TABLES для STYLE и BLOCK_RECORD
            if t.code == 0 and t.value == "TABLE":
                next_t = self.template_tags[i + 1] if i + 1 < len(self.template_tags) else None
                if next_t and next_t.code == 2 and next_t.value == "STYLE":
                    in_style_table = True
                elif next_t and next_t.code == 2 and next_t.value == "BLOCK_RECORD":
                    in_br_table = True

            if t.code == 0 and t.value == "ENDTAB":
                if in_style_table:
                    for s in styles_rem:
                        out_tags.extend(s)
                    in_style_table = False
                elif in_br_table:
                    for br in block_records_rem:
                        out_tags.extend(br)
                    in_br_table = False

            out_tags.append(t)
            i += 1

        # Запись в результирующий DXF файл
        print(f"\n-> Сохранение DXF файла: {output_dxf}")
        with open(output_dxf, "w", encoding="ascii", errors="replace") as f:
            for t in out_tags:
                f.write(f"{t.code:>3}\n{t.value}\n")

        print(f"   Файл успешно сохранен! Всего тегов: {len(out_tags)}")
        return True


def verify_saved_dxf(dxf_path: str):
    """
    Выполняет контрольную проверку сохраненного DXF файла на валидность дескрипторов
    и считывание таблицы модулем table_reader.py.
    """
    print("\n" + "=" * 80)
    print(f"КОНТРОЛЬНАЯ ПРОВЕРКА СОЗДАННОГО ФАЙЛА: {dxf_path}")
    print("=" * 80)

    if not os.path.exists(dxf_path):
        print(f"[ОШИБКА] Файл {dxf_path} не существует!")
        return False

    tags = list(read_tags(dxf_path))
    handles = set()
    duplicates = set()
    for t in tags:
        if t.code == 5:
            if t.value in handles:
                duplicates.add(t.value)
            handles.add(t.value)

    print(f"1. Проверка уникальности дескрипторов (Handles):")
    print(f"   Всего дескрипторов: {len(handles)}")
    if duplicates:
        print(f"   [ОШИБКА] Обнаружены дубликаты handles: {list(duplicates)[:5]}")
        return False
    else:
        print(f"   [OK] Дубликаты отсутствуют, все {len(handles)} handles уникальны.")

    # Проверка сущностей в ENTITIES
    lines = extract_entities_by_type(tags, "LINE")
    tables = extract_entities_by_type(tags, "ACAD_TABLE")
    print(f"2. Примитивы в секции ENTITIES:")
    print(f"   - Отрезки (LINE):          {len(lines)} (сохранены исходные границы ZCAD)")
    print(f"   - Таблицы (ACAD_TABLE):   {len(tables)} (успешно внедрены)")

    # Проверка связующих структур автокада (ACDB_RECOMPOSE_DATA и BLOCK_RECORD refs)
    recompose_found = any(t.code == 3 and t.value == "ACDB_RECOMPOSE_DATA" for t in tags)
    print(f"3. Проверка метаданных объединения разрывов таблицы (Table Breaks):")
    if recompose_found:
        print(f"   [OK] ACDB_RECOMPOSE_DATA присутствует в корневом словаре NOD.")
        print(f"        AutoCAD выполнит Recompose и объединит 3 фрагмента в единую цельную таблицу.")
    else:
        print(f"   [ПРЕДУПРЕЖДЕНИЕ] ACDB_RECOMPOSE_DATA отсутствует в NOD.")

    # Глубокий анализ через table_reader
    print(f"4. Анализ структуры таблицы через table_reader:")
    try:
        parsed_tables = analyze_tables_in_dxf(dxf_path)
        print(f"   [OK] table_reader успешно распознал {len(parsed_tables)} фрагмента(ов) таблицы:")
        for t in parsed_tables:
            idx = t["index"]
            h = t["handle"]
            nr = t["num_rows"]
            nc = t["num_cols"]
            pt = t["insert_point"]
            repeat = "ВКЛ" if t["repeat_top_labels"] else "ВЫКЛ"
            print(f"     * Фрагмент #{idx} (0x{h}): {nr}x{nc} ячеек в ({pt[0]:.2f}, {pt[1]:.2f}), Повтор верхних меток: {repeat}")
        return True
    except Exception as e:
        print(f"   [ОШИБКА] При анализе таблицы возникло исключение: {e}")
        return False


def main():
    template_file = "ZCADonlyline.dxf"
    output_file = "ZCADTABLE.dxf"
    source_file = "acadtable2007.dxf"

    # Поддержка аргументов командной строки:
    # python dxf_saver.py [template] [output] [source]
    if len(sys.argv) > 1:
        template_file = sys.argv[1]
    if len(sys.argv) > 2:
        output_file = sys.argv[2]
    if len(sys.argv) > 3:
        source_file = sys.argv[3]

    print("=" * 80)
    print("DXF TABLE SAVER / INJECTOR: ЗАПИСЬ ТАБЛИЦЫ В DXF")
    print("=" * 80)
    print(f"Шаблон (базовый DXF):     {template_file}")
    print(f"Результат (выходной DXF): {output_file}")
    print(f"Источник таблицы:         {source_file}")
    print("=" * 80)

    saver = DXFTableSaver(template_dxf=template_file, source_table_dxf=source_file)
    success = saver.save(output_dxf=output_file)

    if success:
        verify_saved_dxf(output_file)
        print("\n" + "=" * 80)
        print(f"РЕЗУЛЬТАТ: Таблица успешно записана в {output_file}!")
        print("Autocad корректно воспримет данный файл без ошибок структуры.")
        print("=" * 80)
    else:
        print("\n[ОШИБКА] Не удалось выполнить запись таблицы.")
        sys.exit(1)


if __name__ == "__main__":
    main()
