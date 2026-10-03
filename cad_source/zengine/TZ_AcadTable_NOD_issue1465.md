# AcadTable: чтение и запись разрыва таблиц через NOD (issue #1465)

Исследование и решение issue
[#1465 «AcadTable. Исправления чтения и записи таблиц»](https://github.com/veb86/zcadvelecAI/issues/1465).
Реализация — этап 10 ТЗ `cad_source/zengine/TZ_NOD_NamedObjectDictionary.md`.

## 1. Постановка

Сравнить чтение и запись таблиц ZCAD с эталоном `DXFTableSaveNEW`
(`dxf_saver.py`, `acadtable2007.dxf`, `acadtable2007_analysis.txt`):
флаг разрыва, ручные положение и высота частей, строки, столбцы, ячейки,
стили строк Title/Header/Data, группы DXF. Цепочка:

```
DXF → модель NOD → AcadTable → объект ZCAD → модель NOD → DXF
```

Ограничения: использовать механизм NOD (`uzeffdxfnod`,
`uzeffdxfnodregistry`), а не ad-hoc сканы текста `OBJECTS`; код Python не
переносить; round-trip без потерь.

## 2. Формат AutoCAD (эталон)

Разрыв таблицы хранится не в сущности `ACAD_TABLE`, а в `OBJECTS`:

```
ACAD_TABLE (главная часть) 102 {ACAD_XDICTIONARY 360 <словарь>
  DICTIONARY 3 ACAD_XREC_ROUNDTRIP 360 <XRECORD>
    XRECORD 100 AcDbXrecord
      280 1
      102 ACAD_ROUNDTRIP_2008_TABLE_ENTITY
      360 <TABLECONTENT>        полная логическая таблица
      70  1 (разрыв) | 2 (без данных разрыва)
      90  флаги: 1 вкл., 2 повтор верха, 4 повтор низа,
                 8 ручные положения, 16 ручные высоты
      90  направление, 40 промежуток
      90  число верхних строк-меток, 90 — нижних
      90  N, N × (10/20/30 положение, 40 высота, 90 флаги 1|2)
      90  M, M × (10/20/30 смещение части, 90 первая, 90 последняя строка)
      90  K, K × 330 сущность-продолжение
      361 <TABLEGEOMETRY>
NOD 3 ACDB_RECOMPOSE_DATA 350 <XRECORD>
  XRECORD 280 1, 90 1, 330 × каждый TABLESTYLE, 330 × каждая главная ACAD_TABLE
```

Продолжения — отдельные сущности `ACAD_TABLE` без расширенного словаря;
`TABLECONTENT` содержит все логические строки (без повторённых меток),
стиль строки — `TABLEROW_BEGIN / 90`.

## 3. Расхождения ZCAD до исправления

Проверка — `experiments/issue1465/rtcheck.lpr` (чтение → сводка таблиц →
`savedxf20XX` → чтение → сравнение сводок), результаты master —
`experiments/issue1465/base_roundtrip_diffs.txt`, полный прогон —
`experiments/issue1465/roundtrip_results.txt`.

| Что | До исправления |
|---|---|
| Чтение данных разрыва | глобальные сканы текста `OBJECTS`, бралась только первая запись файла — у второй и далее таблиц разрыв терялся |
| Запись разрыва | приватная запись `ZCAD_SPLIT_TABLE_ENTITY` в XRECORD без владельца — AutoCAD её не знает |
| Части-продолжения | при пересохранении терялись (`acadtablerazdel2007_1`: 2 части → 1) |
| Стили строк | `TABLECONTENT` писался по строкам главной части, тип Header в середине таблицы становился Data (`tablebugheader`, `acadtablerazdel2007_1`) |
| Промежуток/высота без разрыва | терялись (`tablerazdel2`: `sp=0.99 h=2.049` → `0`) |
| `ACDB_RECOMPOSE_DATA` | не писался |

## 4. Решение

1. **Индекс NOD** (`fileformats/uzeffdxfnodacadtable.pas`):
   `TZAcadTableNODIndex.Build(Model)` — «хэндл сущности → данные разрыва»
   по всем round-trip `XRECORD` модели (с типами строк из `TABLECONTENT`),
   `LoadRecompose` — ссылки `ACDB_RECOMPOSE_DATA`; строгий разбор записи
   AutoCAD (под ключом `ACAD_XREC_ROUNDTRIP`) и эвристический — прежней
   записи ZCAD; обратная сборка пар `BuildAcadTableSplitPairs`.
2. **Загрузка** (`uzeffdxf.pas`, `GDBObjEntity.SetDXFTableSplitInfo`):
   данные разрыва передаются таблице из индекса NOD; преобразование в
   параметры частей — `velec/acadtable/uzeacadtable_dxf_split.pas`.
3. **Запись** (`velec/acadtable/uzeacadtable_dxf_write.pas`,
   `uzeacadtable_model.pas`): и модельный, и raw-путь пишут запись
   AutoCAD layout 1 (`BuildAcadTableSplitWriteInfo` →
   `BuildAcadTableSplitPairs`) с продолжениями `330`; `TABLECONTENT` —
   логическая таблица (строки главной части и собственные строки
   продолжений) с типом каждой логической строки; raw-сущности получают
   новый расширенный словарь вместо исходного.
4. **`ACDB_RECOMPOSE_DATA`** (`velec/acadtable/uzeacadtable_dxf_nod.pas`):
   NOD-обработчик реестра (`MinVersion = AC1021`), пишется, только если
   в чертеже есть таблицы; хэндлы стилей — из `TableStyleNameHandleMap`,
   таблиц — из общей карты `p2h`.

## 5. Тесты

* `nodstage10` — индекс NOD на файлах AutoCAD и прежней записи ZCAD,
  несколько разорванных таблиц, обратная сборка пар.
* `nodstage10save` — сквозной цикл на 14 образцах, режимы raw и после
  правки: сводка таблиц до и после совпадает; в NOD сохранённого файла
  есть записи разрыва и продолжения всех таблиц и `ACDB_RECOMPOSE_DATA`
  со ссылками на все главные таблицы. На master — 104 ошибки.

Запуск: `cad_source/zengine/tests/nodtests.sh nodstage10save`.

## 6. Ограничения

* Проверка в AutoCAD в среде разработки недоступна: формат сверен с
  файлами AutoCAD (`acadtablerazdel2007_*.dxf`, `tablerazdel2.dxf`,
  `DXFTableSaveNEW/acadtable2007.dxf`).
* Шаблон `empty.dxf` (ANSI_1252) искажает кириллицу при записи — это не
  связано с таблицами; тесты используют `savetemplate2007.dxf`.
