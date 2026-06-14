# План рефакторинга `uzeacadtable_model.pas`

> Документ описывает, **как** разделить разросшийся модуль
> `uzeacadtable_model.pas` (~2635 строк, ~107 КБ) на несколько модулей по
> смыслу и логике, **сохранив** при этом публичный интерфейс сущности
> `GDBObjAcadTable` и существующие тесты. План носит проектный характер:
> сам код в рамках этого документа не переносится — здесь зафиксирована
> целевая структура, ограничения и пошаговая стратегия.

Связано с issue [#1322](https://github.com/veb86/zcadvelecAI/issues/1322).

---

## 1. Цель

`GDBObjAcadTable` — это «многорежимный» примитив: он одновременно отвечает
за хранение данных таблицы, импорт/экспорт DXF, построение геометрии
(линии + MText), трансформацию (масштаб/поворот), а также за сложную
логику разбиения таблицы по ширине/высоте на части-продолжения и повтор
строк-меток. Из-за этого один модуль соединяет 5–6 разных областей
ответственности и тяжело развивается.

Задача — снизить размер и связность `uzeacadtable_model.pas`, выделив
независимые области в отдельные модули, **в том же стиле**, что уже принят
в подсистеме `acadtable/` (см. §3), и не сломав внешних потребителей (§7).

---

## 2. Текущее состояние подсистемы `acadtable/`

Подсистема уже частично декомпозирована. Сводка модулей (без
`uzccommand_adddxftablestyle.pas`, который не входит в задачу):

| Модуль | Строк | Роль |
|---|---:|---|
| `uzeacadtable_types.pas` | 172 | Базовые типы, записи, перечисления, константы. Без зависимостей. |
| `uzeacadtable_styles.pas` | 105 | Инициализация `TCellStyle`/`TTableStyle`, конвертация выравнивания DXF. |
| `uzeacadtable_cell.pas` | 224 | Логика ячеек: текст, разрешение стиля по иерархии. |
| `uzeacadtable_merge.pas` | 256 | Объединение ячеек, корень объединения, видимость рамок. |
| `uzeacadtable_layout.pas` | 240 | Геометрия: высоты/ширины, сегменты рендеринга, габариты. |
| `uzeacadtable_edit.pas` | 119 | Редактирование содержимого ячеек. |
| `uzeacadtable_stylemanager.pas` | 209 | Применение DXF-стилей таблицы (TABLESTYLE). |
| `uzeacadtable_dxf_read.pas` | 448 | Импорт `ACAD_TABLE` из DXF в запись `TAcadTableDXFData`. |
| `uzeacadtable_dxf_write.pas` | 656 | Экспорт `ACAD_TABLE` в DXF из записей `TAcadTableDXFWritePart`. |
| `uzeacadtable_manager.pas` | 72 | Хаб: реэкспорт типов, связывание всех модулей. |
| **`uzeacadtable_model.pas`** | **2635** | **Сущность `GDBObjAcadTable` — цель рефакторинга.** |

### Принятые в подсистеме конвенции

1. **«Данные + свободные функции».** Все уже выделенные модули (`cell`,
   `merge`, `layout`, `styles`, `dxf_read`, `dxf_write`,
   `stylemanager`) — это **наборы свободных процедур/функций**, которые
   получают данные (записи и массивы) **по ссылке/значению**, а не методы
   класса. Объект `GDBObjAcadTable` в модели держит **тонкие
   методы-обёртки**, делегирующие в эти модули (см. `GetTotalWidth`,
   `GetRowHeightLocal` и т. п.).
2. **Иерархия зависимостей без циклов** (снизу вверх):
   `types` → `styles` → {`cell`, `merge`, `layout`, `edit`} →
   {`dxf_read`, `dxf_write`, `stylemanager`} → `model` → `manager`.
   Утилитарные модули **не ссылаются** на `model`.
3. **Записи-агрегаты** (`TAcadTableDXFData`, `TAcadTableDXFWritePart`)
   передаются как «пакеты данных» между сущностью и подмодулями.
4. **Псевдонимы массивов** (`TTableCellArray`, `TMergeRangeArray` и т. д.)
   введены, чтобы можно было присваивать данные целиком (issue #1300).

> **Вывод:** правильный рефакторинг `model` — это **продолжение** уже
> начатой декомпозиции по тому же шаблону, а не введение нового стиля.

---

## 3. Ключевое ограничение Free Pascal

Методы типа `object`/`class` **обязаны** реализовываться в том же модуле,
где объявлен сам тип. Поэтому **нельзя** просто «перенести часть методов
`GDBObjAcadTable` в другой unit». Объявление класса и все его методы
остаются в `uzeacadtable_model.pas`.

Отсюда — единственная согласованная с подсистемой стратегия:

> Выносить в новые модули **не методы, а тело логики** в виде свободных
> функций, оперирующих данными. В `model` остаются **тонкие обёртки**
> (как уже сделано для `layout`/`merge`/`cell`).

Это автоматически уменьшает `model`: тяжёлые алгоритмы (рендеринг,
сегментация по высоте, повтор строк-меток, сборка DXF-частей) переезжают в
отдельные модули, а в сущности остаются вызовы в 2–5 строк.

---

## 4. Карта ответственностей `uzeacadtable_model.pas`

Методы сгруппированы по смыслу (диапазоны строк приблизительны и служат
ориентиром объёма):

| # | Группа | Методы | ≈ строк |
|---|---|---|---:|
| A | **Жизненный цикл / ядро** | `initnul`, `done`, `Clone`, `GetObjType`, `GetObjTypeName`, `AllocAcadTable`, `AllocAndInitAcadTable`, `initialization` | ~250 |
| B | **DXF-импорт** | `LoadFromDXF` (483–697) | ~214 |
| C | **DXF-экспорт** | `FillDXFWritePartFromSelf`, `FillDXFWritePartFromContinuation`, `BuildDXFContinuationWriteParts`, `SaveToDXF`, `SaveToDXFFollow`, `DXFOut`, `InvalidateRawDXFEntity`, `CanSaveRawDXFEntity`, `SetDXFRawEntityText`, `DXFDelayedBuildGeometry` | ~200 |
| D | **Рендеринг / геометрия** | `RenderCurrentTable` (697–1041, **~344**), `BuildVisualRepresentation`, `BuildGeometry`, `FormatEntity`, `IsStagedFormatEntity`, `getoutbound`, обёртки `GetRowHeightLocal`/`GetColWidthLocal`/`GetTotalHeight`/`GetTotalWidth`/`GetCellTextLocal` | ~520 |
| E | **Части-продолжения: данные** | `SwapTableData`, `CaptureTableDataToPart`, `CopyTablePart`, `ClearPart`, инспекторы `GetContinuationPartCount`/`ContinuationPartRowCount`/`ContinuationPartCellText` | ~260 |
| F | **Разбиение по высоте/ширине** | `MergeAllContinuationPartsIntoMain`, `SlicePartFromPart`, `SplitMainTableByBreakHeight`, `RepositionContinuationParts`, `GetBreakSpacing`/`Set`, `GetBreakHeight`/`Set`, `GetBreakEnabled`/`Set`, `SetBreakDirection`, `TryMergeContinuation`, `SetTableBreakData` | ~450 |
| G | **Повтор строк-меток (Title/Header)** | `ComputeTopLabelRowCount`, `EffectiveRepeatTopRowCount`, `PartRepeatsTopLabels`, `UpdateContinuationRowBaseIndexes`, `DetectBreakRepeatTopLabels`, `GetBreakRepeatTopLabels`/`Set`, `RemoveTopLabelsFromParts`, `AddTopLabelsToParts`, `PrependTopLabelsToPart`, `RecomputeBreakRepeatTopLabels` | ~330 |
| H | **Трансформация (матрица)** | `decomposite`, `setrot`, `ReCalcFromObjMatrix`, `CalcObjMatrix`, `rtsave`, `TransformAt` | ~130 |

Самые тяжёлые и самостоятельные блоки — **D (рендеринг)**, **F+G
(разбиение и строки-метки, вместе ~780 строк)** и **B/C (DXF I/O)**.

---

## 5. Ключевая архитектурная идея: `TAcadTablePart` как общий контейнер данных

Сейчас в `model` объявлена запись `TAcadTablePart`, агрегирующая
**ровно те же** поля, что хранит и сама сущность (точка вставки, размеры,
строки/столбцы/ячейки/объединения, флаги и параметры разбиения, raw-DXF).
Дублирование полей — главный источник «толщины» методов групп E/F/G:
почти каждый из них вручную копирует одни и те же 15+ полей между `Self`
и частью.

**Рекомендация:** ввести единый «снимок данных таблицы» и сделать
свободные функции, работающие над ним.

- Перенести запись `TAcadTablePart` (и сопутствующие операции
  `CopyTablePart`/`ClearPart`/`SlicePartFromPart`) в отдельный модуль
  **`uzeacadtable_part.pas`** (или в `uzeacadtable_types.pas`).
- Представлять «главную часть» сущности как такой же `TAcadTablePart`.
  Тогда `SwapTableData`/`CaptureTableDataToPart`/`MergeAll…`/`Split…` и
  логика строк-меток становятся свободными функциями над
  `TAcadTablePart` и `array of TAcadTablePart`, без знания о
  `GDBObjAcadTable`.
- В сущности останутся тонкие обёртки, как уже сделано для `layout`.

Это не обязательное предусловие, но именно оно даёт наибольшее
сокращение объёма и устраняет дублирование. Если миграцию полей сущности
на `TAcadTablePart` делать рискованно за один шаг — функции групп E/F/G
всё равно можно вынести, передавая поля по ссылке (как это уже устроено в
`uzeacadtable_layout`/`merge`).

---

## 6. Целевая структура модулей

Предлагается добавить новые модули того же слоя, что и существующие
утилиты, и оставить в `model` только сущность с тонкими обёртками.

```
types ─► styles ─► {cell, merge, layout, edit}
                          │
                          ▼
                 ┌─────────────────────────────────────────┐
   НОВЫЕ:        │  part      — TAcadTablePart + copy/clear  │
                 │  render    — построение линий/MText        │
                 │  parts     — слияние/нарезка/перепозиция   │
                 │  toplabels — детекция/повтор строк-меток    │
                 │  transform — матрица/масштаб/поворот        │
                 └─────────────────────────────────────────┘
                          │
   I/O:          {dxf_read, dxf_write, stylemanager}
                          │
                          ▼
              model (тонкая сущность GDBObjAcadTable)
                          │
                          ▼
                      manager (хаб)
```

| Новый модуль | Что переносится (из групп §4) | Зависит от |
|---|---|---|
| `uzeacadtable_part.pas` | Запись `TAcadTablePart`; `CopyTablePart`, `ClearPart`, инициализация части (E) | `types` |
| `uzeacadtable_render.pas` | Тело `RenderCurrentTable`; помощники построения линий/MText (D) | `types`, `styles`, `cell`, `merge`, `layout`, `stylemanager` + entity-модули линий/текста |
| `uzeacadtable_parts.pas` | `MergeAllContinuationPartsIntoMain`, `SlicePartFromPart`, `SplitMainTableByBreakHeight`, `RepositionContinuationParts`, `SwapTableData`/`CaptureTableDataToPart` (F, E) | `types`, `part`, `layout` |
| `uzeacadtable_toplabels.pas` | `ComputeTopLabelRowCount`, `EffectiveRepeatTopRowCount`, `PartRepeatsTopLabels`, `Detect/Get/Set BreakRepeatTopLabels`, `Remove/Add/Prepend TopLabels…`, `UpdateContinuationRowBaseIndexes` (G) | `types`, `part` |
| `uzeacadtable_transform.pas` | `decomposite`, `setrot` и геометрия матрицы (H) | `types`, `uzegeometry(types)` |
| (расширить) `uzeacadtable_dxf_write.pas` | Сборку `TAcadTableDXFWritePart` из части (часть C) — рядом с уже существующим writer | как есть |

В `uzeacadtable_model.pas` остаются:

- объявление и поля `GDBObjAcadTable`;
- виртуальные переопределения (`LoadFromDXF`, `SaveToDXF*`, `DXFOut`,
  `BuildGeometry`, `FormatEntity`, `CalcObjMatrix`, `ReCalcFromObjMatrix`,
  `TransformAt`, `Clone`, `GetObjType*`, `rtsave`, `TryMergeContinuation`
  и т. д.) — но **в виде тонких обёрток**, делегирующих в новые модули;
- жизненный цикл (`initnul`/`done`/`Alloc*`) и регистрация DXF-сущности
  (`initialization`).

Ожидаемый итог: `model` сокращается примерно с 2635 до ~700–900 строк;
каждая выделенная область — отдельный тестируемый модуль 150–400 строк.

После добавления новых модулей их нужно подключить в список `uses`
модуля-хаба `uzeacadtable_manager.pas` (см. §2, конвенция 2).

---

## 7. Совместимость публичного API

Внешние потребители сущности:

- `cad_source/zcad/register/uzcregacadtable.pas` — инспектор объектов;
  использует свойства `Width`, `Height`, `TableStyleName`, `RowCount`,
  `ColCount`, `BreakEnabled`, `BreakRepeatTopLabels`,
  `BreakRepeatBottomLabels`, `BreakManualPosition`, `BreakManualHeight`,
  `BreakSpacing`, `BreakHeight` (чтение/запись через сущность).
- `cad_source/zengine/tests/uzctacadtable.pas` — тесты; используют
  `PGDBObjAcadTable`, `GetObjType`, `ContinuationPartCount`,
  `ContinuationPartRowCount`, `ContinuationPartCellText`, `BreakEnabled`,
  построение геометрии и сохранение в DXF.

**Требование рефакторинга:** интерфейс `GDBObjAcadTable` (поля-свойства,
сигнатуры публичных и виртуальных методов) **не меняется**. Меняется
только то, **где** живёт реализация. Это позволяет вести рефакторинг
строго внутренними шагами без правок потребителей.

---

## 8. Пошаговый план (каждый шаг компилируется и проходит тесты)

Порядок — от наименее связанного к наиболее связанному, чтобы каждый
шаг был маленьким, обратимым и проверяемым существующими тестами.

1. **Подготовка.** Зафиксировать «зелёный» прогон `uzctacadtable.pas`
   как базовую линию (см. §9). Включить трассировку в `model` по
   необходимости (по умолчанию выключенную).
2. **`uzeacadtable_part.pas`.** Вынести `TAcadTablePart`, `CopyTablePart`,
   `ClearPart`. В `model` оставить обёртки. Реэкспорт типа в `manager`.
3. **`uzeacadtable_transform.pas`.** Вынести матричную геометрию
   (`decomposite`, `setrot`, помощники для `CalcObjMatrix`/`ReCalcFromObjMatrix`).
   Группа H самодостаточна и почти не связана с остальными.
4. **`uzeacadtable_render.pas`.** Вынести тело `RenderCurrentTable` и
   построение линий/MText. Самый крупный выигрыш по объёму (группа D).
5. **`uzeacadtable_toplabels.pas`.** Вынести логику повтора строк-меток
   (группа G) — она оперирует частями и хорошо изолируется.
6. **`uzeacadtable_parts.pas`.** Вынести слияние/нарезку/перепозицию
   частей (группа F + перенос данных из E).
7. **DXF I/O.** Перенести сборку `TAcadTableDXFWritePart` ближе к
   `uzeacadtable_dxf_write.pas`; при необходимости вынести оркестрацию
   `LoadFromDXF` помощниками в `uzeacadtable_dxf_read.pas`.
8. **Чистка `model`.** Убедиться, что остались только сущность, тонкие
   обёртки, жизненный цикл и регистрация. Обновить шапку-комментарий и
   список `uses`; подключить новые модули в `uzeacadtable_manager.pas`.

> На **каждом** шаге: компиляция проекта + прогон тестов AcadTable.
> Один шаг = один атомарный коммит в ветку PR, чтобы прерванную работу
> можно было восстановить.

---

## 9. Проверка и тесты

- Регрессионная база — существующий набор `uzctacadtable.pas`
  (загрузка разделённой таблицы как одного объекта, рендеринг разрывов,
  чтение параметров разбиения, стили текста ячеек, сохранение в
  структурированный DXF и round-trip). Эти тесты должны оставаться
  зелёными после **каждого** шага §8.
- Для вынесенных свободных функций (рендеринг-сегменты, нарезка частей,
  детекция повтора строк-меток) желательно добавить **юнит-тесты на
  чистые функции** (вход — записи/массивы, выход — данные), что станет
  возможным именно благодаря выносу логики из методов сущности.
- DXF round-trip: сохранить → загрузить → сравнить — как защита от
  регрессий при переносе логики экспорта.

---

## 10. Риски

| Риск | Снижение |
|---|---|
| Ограничение FPC: методы нельзя «переселить» | Выносим тело логики в свободные функции, оставляем обёртки (§3). |
| Циклические зависимости | Новые модули зависят только «вниз» (types/part/styles); `model` — единственный, кто их объединяет (§6). |
| Скрытое состояние через много полей сущности | Унификация через `TAcadTablePart` (§5) либо передача полей по ссылке, как в `layout`/`merge`. |
| Регрессия рендеринга/round-trip | Маленькие шаги + прогон `uzctacadtable.pas` после каждого (§8–9). |
| Изменение публичного API | Запрещено: интерфейс сущности фиксирован (§7). |

---

## 11. Чеклист готовности

- [ ] Создан `uzeacadtable_part.pas`; `TAcadTablePart` вынесена.
- [ ] Создан `uzeacadtable_transform.pas` (группа H).
- [ ] Создан `uzeacadtable_render.pas` (группа D).
- [ ] Создан `uzeacadtable_toplabels.pas` (группа G).
- [ ] Создан `uzeacadtable_parts.pas` (группы E/F).
- [ ] Логика DXF-экспорта частей перенесена ближе к `dxf_write`.
- [ ] `uzeacadtable_model.pas` сокращён до сущности + тонких обёрток.
- [ ] Новые модули подключены в `uzeacadtable_manager.pas`.
- [ ] Публичный интерфейс `GDBObjAcadTable` не изменён.
- [ ] Все тесты `uzctacadtable.pas` зелёные; добавлены юнит-тесты на
      вынесенные чистые функции.
