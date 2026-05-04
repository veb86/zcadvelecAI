# Техническое задание: загрузка DWG в ZCAD через LibreDWG без новой domain model

> Issue: [veb86/zcadvelecAI#1079](https://github.com/veb86/zcadvelecAI/issues/1079)
> Рабочая зона: `cad_source/components/fpdwg/`, `cad_source/zengine/fileformats/`
> Статус документа: техническое задание и план реализации, без реализации кода в рамках issue #1079.
> Главная корректировка: для загрузки DWG в ZCAD не создается третья domain model. Источник данных - LibreDWG `Dwg_Data`; целевая модель - существующий ZCAD drawing/entity graph.

## 1. Цель

Нужно пересмотреть подход к подключению LibreDWG к ZCAD так, чтобы `.dwg` загружался в существующую модель ZCAD без промежуточной самостоятельной DWG domain model.

Итоговая схема:

```text
DWG file
  -> LibreDWG Dwg_Data
  -> fpdwg/dwgproc binding helpers
  -> DWG import context and deferred queues
  -> existing ZCAD drawing, tables, block definitions, entities
```

Слой `fpdwg/inspector` остается полезным диагностическим инструментом для просмотра LibreDWG-данных и отладки handle-ссылок. Он не должен становиться обязательной зависимостью загрузчика ZCAD и не должен диктовать структуру ZCAD-импорта.

Ключевая задача этого ТЗ - описать поэтапную реализацию загрузчика, который:

- читает DWG через LibreDWG;
- использует ZCAD как единственную целевую прикладную модель;
- не создает `TDWGDocument` как обязательный мост для ZCAD;
- корректно переживает порядок объектов, где сущность может встретиться раньше родителя;
- строит таблицы, блоки, сущности, стили и геометрию в правильных фазах;
- сохраняет диагностику по неизвестным и частично загруженным объектам.

## 2. Связь с предыдущими документами

В проекте уже есть три важных документа:

| Документ | Что использовать | Что скорректировать |
|---|---|---|
| `cad_source/zcad/velec/loadDWG/TZ_loadDWG.md` | Анализ DXF/DWG-паритета, список сущностей, текущие дефекты `uzefflibredwg*` | Там допустима отдельная модульная архитектура DWG-загрузки, но не зафиксирован запрет на новую domain model |
| `cad_source/components/fpdwg/TZ_DWG_CONSOLE_INSPECTOR.md` | Двухфазность, handle registry, диагностика unknown, safe text decode | Это ТЗ про инспектор, не про ZCAD-импорт |
| `cad_source/components/fpdwg/TZ_DWG_DOMAIN_MODEL_CORRECTION.md` | Аудит текущего `fpdwg`, границы inspector/raw binding | Для issue #1079 не применяем идею "ZCAD через TDWGDocument"; ZCAD импортируется напрямую из LibreDWG raw в ZCAD |

Текущее ТЗ не удаляет предыдущую работу. Оно задает отдельное направление: "DWG -> ZCAD" без обязательного промежуточного `TDWGDocument`.

## 3. Аудит текущего состояния

### 3.1 LibreDWG binding

В `cad_source/components/fpdwg/` уже есть:

| Файл | Назначение | Оценка |
|---|---|---|
| `dwg.pp` | h2pas binding к `libredwg/dwg.h`, типы `Dwg_Data`, `Dwg_Object`, `Dwg_Object_Ref`, `DWG_OBJECT_TYPE` | Использовать как raw API, вручную не править без отдельной задачи |
| `dwgproc.pp` | Динамическая загрузка `libredwg.so` / `libredwg-0.dll`, `dwg_read_file`, `dxf_read_file`, `dwg_free`, generic `GDWGParser` | Оставить low-level слоем, добавить безопасные helper-функции |
| `inspector/fpdwg_reader.pp` | Более аккуратный reader с `try/finally`, inject API для тестов | Можно переиспользовать идеи, но не тянуть весь inspector в ZCAD-загрузчик |
| `inspector/fpdwg_libredwg_utils.pp` | Decode/handle утилиты | Выделить общие утилиты из inspector-зависимостей, если они нужны загрузчику |

### 3.2 Текущая загрузка DWG в ZCAD

Текущий вход:

- `cad_source/zengine/fileformats/uzefflibredwg.pas`
  - `addfromdwg` вызывает `LoadLibreDWG`, `dwg_read_file`, `ZCDWGParser.parseDwg_Data`, затем `dwg_free`.
  - Сейчас `try/finally` не защищает `dwg_free` от исключений внутри parse-фазы.
  - `DebugDWG` только логирует header и не переносит его в ZCAD drawing vars.
- `cad_source/zengine/fileformats/uzefflibredwg2ents.pas`
  - `AddLayer` реально создает/обновляет layer через `LayerTable.MergeItem`.
  - `AddLineEntity` реально создает `GDBObjLine`, но всегда добавляет его в `pObjRoot`.
  - `AddLineType`, `AddBlockHeader`, `AddBlock` являются заглушками.
  - У LINE есть явный дефект: `lBegin.z` и `lEnd.z` берутся из `.x`, а не `.z`.
- `cad_source/components/fpdwg/dwgproc.pp`
  - `parseDwg_Data` проходит `dwg.object[]` одним циклом и сразу вызывает handler.
  - Такой подход не выдерживает DWG, где child entity идет раньше owner/block/table record.

### 3.3 Как сейчас "рисуются" объекты в ZCAD

ZCAD уже имеет свою domain model:

- `TSimpleDrawing`;
- `pObjRoot`;
- `BlockDefArray`;
- таблицы `LayerTable`, `LTypeStyleTable`, `TextStyleTable`, `DimStyleTable`;
- сущности `GDBObjLine`, `GDBObjCircle`, `GDBObjArc`, `GDBObjBlockInsert`, `GDBObjText`, `GDBObjMText` и т. д.;
- общий механизм добавления в owner через `AddMi` / `DXFLoadAddMi`;
- построение геометрии через `BuildGeometry`;
- приведение отображения через `FormatAfterDXFLoad`;
- late hooks `FromDXFPostProcessBeforeAdd` и `FromDXFPostProcessAfterAdd`.

DXF-загрузчик уже показывает правильный жизненный цикл:

```text
allocate entity
  -> load scalar fields
  -> register handle
  -> find owner
  -> AddMi/DXFLoadAddMi
  -> BuildGeometry
  -> FormatAfterDXFLoad
  -> FromDXFPostProcessAfterAdd
```

DWG-загрузчик должен прийти к тому же жизненному циклу, но с отдельной фазой deferred resolve, потому что порядок `dwg.object[]` не гарантирует, что owner уже создан.

## 4. Архитектурное решение

### 4.1 Что НЕ создаем

Для ZCAD-загрузки не создаем новый слой вида:

```text
LibreDWG -> TDWGDocument -> ZCAD drawing
```

Причины:

- LibreDWG уже является raw-моделью файла.
- ZCAD уже является прикладной domain model.
- Третья модель увеличит объем маппинга: raw -> DWG domain -> ZCAD domain.
- Для загрузки и отрисовки в ZCAD достаточно import context, handle indexes и deferred queues.
- Inspector может иметь свою диагностическую модель, но она не должна быть частью production import pipeline.

### 4.2 Что создаем вместо domain model

Создается технический контекст загрузки, живущий только в процессе импорта:

```text
TDWGZCADLoadContext
  - references to TZDrawingContext / TSimpleDrawing / owner / DC
  - RawHandle -> raw object index
  - RawHandle -> ZCAD object pointer
  - RawHandle -> ZCAD object kind
  - pending owner links
  - pending style/layer/linetype/block refs
  - pending geometry finalization
  - warnings/errors/statistics
```

Это не domain model, потому что:

- не является долговременным представлением чертежа;
- не копирует все поля DWG;
- не используется после завершения загрузки;
- не заменяет `TSimpleDrawing`;
- не должен попадать в UI или бизнес-логику ZCAD.

### 4.3 Целевой pipeline

```text
Phase 0. Read raw
  dwg_read_file -> Dwg_Data

Phase 1. Scan raw graph
  collect handles, raw indexes, object classes, owner refs

Phase 2. Create ZCAD shells
  create tables, block definitions, entity objects
  register RawHandle -> ZCAD pointer immediately after allocation
  do not require owner to exist at this moment

Phase 3. Resolve ownership and references
  attach entities to real owners
  resolve layer, linetype, style, block, dimension refs

Phase 4. Finalize visual state
  BuildGeometry
  FormatAfterDXFLoad
  FromDXFPostProcessAfterAdd
  pack/correct object arrays

Phase 5. Diagnostics
  report skipped/unknown/partial objects and broken refs

Phase 6. Free raw
  dwg_free in guaranteed finally
```

Главное правило: ZCAD objects must not keep pointers into `Dwg_Data` after `dwg_free`.

## 5. Решение проблемы "сначала линия, родитель позже"

### 5.1 Почему текущий one-pass подход ломается

Сейчас handler вызывается сразу при встрече объекта:

```text
for object in dwg.object[]
  if handler exists
    handler(ZCDCtx, DWGContext, object, typedPointer)
```

Если `DWG_TYPE_LINE` встретился раньше `DWG_TYPE_BLOCK_HEADER` или другого owner:

- LINE создается сразу;
- owner не найден;
- объект добавляется в `pObjRoot`;
- позднее появляется настоящий родитель, но LINE уже в неправильном месте;
- transform, block membership и последующая отрисовка могут быть неверными.

### 5.2 Обязательный алгоритм

Алгоритм загрузки должен быть таким:

1. На scan-фазе записать каждый raw object по handle:

   ```text
   RawHandleIndex[Raw.handle.value] = raw index
   RawOwnerIndex[Raw.handle.value] = owner handle
   ```

2. На shell-фазе создать ZCAD-объект, но не требовать готового owner:

   ```text
   line := AllocAndInitLine(nil)
   FillLineGeometry(line, rawLine)
   RegisterZCADHandle(rawHandle, line, OT_Entity)
   AddPendingOwner(line, rawHandle, ownerHandle, fallbackOwner)
   AddPendingFinalize(line)
   ```

3. На resolve-фазе пройти `PendingOwner[]` после создания всех shell-объектов:

   ```text
   for pending in PendingOwner
     owner := ResolveZCADOwner(pending.OwnerHandle)
     if owner = nil
       owner := pending.FallbackOwner
     owner.AddMi(pending.Entity)
   ```

4. Только после прикрепления к owner запускать geometry/finalize:

   ```text
   entity.BuildGeometry(drawing)
   entity.FormatAfterDXFLoad(drawing, dc)
   entity.FromDXFPostProcessAfterAdd
   ```

5. Если owner относится к block definition, применять тот же порядок, что в DXF:

   - не рисовать block definition как модельное пространство;
   - defer formatting для block content;
   - при необходимости применить transform относительно owner после resolve.

### 5.3 Варианты fallback для unresolved owner

| Ситуация | Поведение |
|---|---|
| `ownerhandle = nil / 0` у model-space entity | Добавить в `ZCDCtx.POwner` |
| owner handle есть, но не найден | Добавить в `ZCDCtx.POwner`, записать warning |
| owner найден, но это не container | Добавить в `ZCDCtx.POwner`, записать warning с типом найденного объекта |
| owner найден как block definition | Добавить в block definition через `AddMi` / `DXFLoadAddMi`, геометрию отложить |
| duplicate raw handle | strict mode: ошибка; tolerant mode: первый объект остается, второй пропускается с warning |

Fallback не должен молча терять объект.

## 6. Контракты модулей

### 6.1 `dwg.pp`

Контракт:

- только generated/raw binding;
- не добавлять ZCAD-зависимости;
- не править вручную для конкретного загрузчика;
- обновлять только вместе с обновлением `libredwg/dwg.h`.

### 6.2 `dwgproc.pp`

Текущий `GDWGParser` можно оставить для простых диагностических проходов, но production ZCAD loader должен получить API, пригодный для фазовой загрузки.

Нужные изменения:

- добавить helper для raw handle:

  ```pascal
  function DWGObjectHandleValue(const Obj: Dwg_Object): QWord;
  function DWGObjectOwnerHandleValue(const Obj: Dwg_Object; out Value: QWord): Boolean;
  function DWGRefHandleValue(Ref: BITCODE_H; out Value: QWord): Boolean;
  ```

- добавить безопасный decode text helper без зависимости от inspector;
- проверить duplicate registration в `RegisterDWGEntityLoadProc` / `RegisterDWGObjectLoadProc`;
- не делать `dwgproc.pp` владельцем ZCAD import state.

### 6.3 `uzefflibredwg.pas`

Назначение после переработки:

- точка входа формата `.dwg`;
- загрузка/освобождение `Dwg_Data`;
- запуск нового `TDWGZCADLoader`;
- long-process reporting по фазам;
- проброс `LogProc`;
- отсутствие entity-specific логики.

Обязательное изменение lifetime:

```pascal
Success := dwg_read_file(..., @dwg);
try
  if IsCriticalDWGReadError(Success) then
    Exit;
  Loader.LoadRawIntoZCAD(dwg, ZCDCtx, LogProc);
finally
  dwg_free(@dwg);
end;
```

`dwg_free` должен срабатывать при исключении в scan/materialize/resolve/finalize.

### 6.4 `uzefflibredwg2ents.pas`

Текущий файл нельзя расширять как большой монолит. Его нужно оставить compatibility-unit или постепенно разнести.

Минимальные изменения:

- исправить LINE Z coordinate;
- убрать прямое добавление всех сущностей в `pObjRoot`;
- перевести handlers на использование `TDWGZCADLoadContext`;
- для новых сущностей создавать отдельные units.

Целевое состояние:

```text
cad_source/zengine/fileformats/
  uzefflibredwg.pas
  uzefflibredwg2ents.pas          compatibility/registration only
  dwg/
    uzedwgloadcontext.pas
    uzedwghandle.pas
    uzedwgrawscan.pas
    uzedwgloader.pas
    uzedwgtables.pas
    uzedwgblocks.pas
    uzedwgresolver.pas
    uzedwgfinalize.pas
    uzedwgdiagnostics.pas
    entities/
      uzedwgentline.pas
      uzedwgentcircle.pas
      uzedwgentarc.pas
      uzedwgentlwpolyline.pas
      uzedwgenttext.pas
      uzedwgentmtext.pas
      uzedwgentinsert.pas
      uzedwgentunknown.pas
```

Каждый модуль должен иметь одну ответственность и целевой размер до 300-500 строк, кроме generated binding.

### 6.5 `fpdwg/inspector/*`

Контракт:

- inspector можно использовать для анализа проблемных DWG;
- ZCAD loader не зависит от `fpdwg_document`, `fpdwg_factory`, `fpdwg_resolver`, `fpdwg_validator`;
- общие raw helpers можно вынести ниже, например в `dwgproc.pp` или отдельный `fpdwg_libredwg_common.pp`;
- inspector tests не должны ломаться из-за внедрения ZCAD loader.

## 7. Структура `TDWGZCADLoadContext`

Рекомендуемая форма:

```pascal
type
  TDWGZCADObjectKind = (
    dokUnknown,
    dokLayer,
    dokLineType,
    dokTextStyle,
    dokDimStyle,
    dokBlockDef,
    dokEntity
  );

  TDWGZCADHandleEntry = record
    Handle: QWord;
    Kind: TDWGZCADObjectKind;
    Ptr: Pointer;
    RawIndex: Integer;
  end;

  TDWGZCADPendingOwner = record
    Entity: PGDBObjEntity;
    EntityHandle: QWord;
    OwnerHandle: QWord;
    FallbackOwner: PGDBObjGenericSubEntry;
    RawIndex: Integer;
  end;

  TDWGZCADPendingFinalize = record
    Entity: PGDBObjEntity;
    Owner: PGDBObjGenericSubEntry;
    InBlockDefinition: Boolean;
  end;

  TDWGZCADLoadContext = record
    Z: ^TZDrawingContext;
    Raw: ^Dwg_Data;
    H2Z: TDWGHandle2ZCADObjectMap;
    RawIndex: TDWGRawHandleIndex;
    PendingOwners: TDWGPendingOwnerList;
    PendingRefs: TDWGPendingReferenceList;
    PendingFinalize: TDWGPendingFinalizeList;
    Warnings: TDWGImportWarningList;
    Stats: TDWGImportStats;
    procedure Init(var AZ: TZDrawingContext; var ARaw: Dwg_Data);
    procedure Done;
  end;
```

Имена типов можно уточнить при реализации под существующие generic containers. Важен контракт: контекст хранит индексы и очереди, а не полную копию DWG.

## 8. Обработка таблиц и стилей

### 8.1 Header

Из `Dwg_Data.header` нужно перенести в ZCAD context:

- version/from_version;
- codepage;
- units, если поле надежно доступно в binding;
- current layer, text style, dimension style, line type, если их можно получить из header vars или table refs;
- диагностику read code LibreDWG.

Если LibreDWG не дает стабильный доступ к конкретной header variable, это фиксируется warning и отдельной TODO, а загрузка продолжается.

### 8.2 Layers

`AddLayer` уже делает правильное базовое действие:

```pascal
LayerTable.MergeItem(name, LoadMode)
```

Нужно расширить:

- register raw layer handle -> `PGDBLayerProp`;
- переносить color, lineweight, on/off, lock, plot flag;
- сохранять linetype handle как pending ref, если LibreDWG дает ссылку;
- учитывать codepage/Unicode без ручных `if R_2007 then Tria_Utf8ToAnsi` в каждом handler.

### 8.3 Linetypes, text styles, dimstyles

Сейчас это заглушки. Нужны отдельные phases до entity finalization:

- создать/merge запись в ZCAD style table;
- зарегистрировать handle;
- сохранить pending refs на shape/text style/line type;
- после создания всех таблиц выполнить resolve style refs.

### 8.4 Blocks

Для block headers:

- на shell-фазе создать или найти `BlockDefArray` entry;
- зарегистрировать raw block handle -> `PGDBObjBlockDef`;
- сохранить base point, flags, name;
- не строить геометрию block content до resolve-фазы;
- model space/paper space обрабатывать как контейнеры, а не как обычные пользовательские блоки.

Для block content:

- entity может встретиться раньше block header;
- entity добавляется в pending owner;
- после resolve owner указывает на нужный `PGDBObjBlockDef`.

## 9. Обработка сущностей

### 9.1 Общий контракт entity mapper

Каждый DWG entity mapper должен:

1. Проверить `Raw.supertype = DWG_SUPERTYPE_ENTITY`.
2. Проверить typed pointer на `nil`.
3. Создать ZCAD entity через существующий allocator.
4. Скопировать геометрию и scalar-свойства.
5. Зарегистрировать raw handle -> entity.
6. Сохранить owner handle в pending owner.
7. Сохранить layer/ltype/material/plotstyle refs в pending refs.
8. Не вызывать `BuildGeometry` до resolve owner.
9. Не хранить raw pointer внутри ZCAD entity.

### 9.2 LINE как первый MVP

Исправленная логика LINE:

```pascal
Line := AllocAndInitLine(nil);
Line^.CoordInOCS.lBegin.x := RawLine^.start.x;
Line^.CoordInOCS.lBegin.y := RawLine^.start.y;
Line^.CoordInOCS.lBegin.z := RawLine^.start.z;
Line^.CoordInOCS.lEnd.x := RawLine^.end_.x;
Line^.CoordInOCS.lEnd.y := RawLine^.end_.y;
Line^.CoordInOCS.lEnd.z := RawLine^.end_.z;
RegisterEntityHandle(Ctx, RawObject, Line);
QueueOwnerResolve(Ctx, Line, RawObject);
QueueEntityRefs(Ctx, Line, RawObject);
```

### 9.3 Приоритет сущностей

После стабилизации LINE добавлять сущности в таком порядке:

1. `CIRCLE`, `ARC`, `POINT` - простая геометрия, мало зависимостей.
2. `LWPOLYLINE`, `POLYLINE` - vertices, bulge, width.
3. `TEXT`, `MTEXT` - codepage, style, alignment.
4. `INSERT`, `ATTRIB`, `ATTDEF` - block refs и атрибуты.
5. `DIMENSION` - зависит от dimstyles и generated blocks.
6. `HATCH`, `SPLINE`, `ELLIPSE`, `SOLID`, `3DFACE`.
7. Proxy/unknown entities with diagnostic fallback.

## 10. Unknown и proxy

Требование: загрузчик не должен молча терять DWG-объекты.

Режимы:

| Тип | Поведение |
|---|---|
| Unsupported non-entity object | Warning + stats, без создания ZCAD entity |
| Unsupported entity with proxy graphics | Создать ZCAD proxy entity, если доступен существующий proxy-механизм |
| Unsupported entity without proxy graphics | Warning + optional placeholder only в debug mode |
| Corrupted object | Warning/error по severity, загрузка остальных объектов продолжается |

Если используется proxy entity, он должен быть ZCAD-сущностью, а не частью отдельной DWG domain model.

## 11. Ошибки и диагностика

Нужны уровни:

| Уровень | Пример | Действие |
|---|---|---|
| Fatal | файл не открыть, `dwg_read_file` вернул critical error, нет `dwg_free` export | Прервать загрузку |
| Error | невозможно создать обязательную ZCAD-структуру | Пропустить объект или прервать phase по режиму |
| Warning | owner/layer/ltype не найден, unsupported entity | Fallback и продолжить |
| Info | статистика типов, количество skipped | Записать в verbose/debug log |

`LogProc` из `addfromdwg` должен использоваться вместе с `zDebugLn`, чтобы вызывающий код мог получить машинно-обрабатываемую диагностику.

## 12. Поэтапный план реализации

### Этап 1. Стабилизация текущего контура

Цель: не менять архитектуру полностью, но убрать очевидные ошибки и подготовить безопасную базу.

Изменения:

- `uzefflibredwg.pas`: гарантировать `dwg_free` через `try/finally`;
- `uzefflibredwg2ents.pas`: исправить Z coordinate у LINE;
- `dwgproc.pp`: добавить handle/text helpers или вынести их в общий unit;
- добавить минимальную диагностику read code LibreDWG;
- зафиксировать, что inspector units не подключаются к ZCAD loader.

Проверка:

- unit test/helper test для извлечения handle из fake `Dwg_Object_Ref`;
- regression test на LINE Z coordinate через direct mapper function;
- manual load одного fixture DWG, если LibreDWG доступен.

### Этап 2. Import context и delayed owner resolve

Цель: решить проблему parent-after-child.

Изменения:

- добавить `uzedwgloadcontext.pas`;
- добавить raw scan phase `RawHandle -> raw index`;
- добавить `RawHandle -> ZCAD pointer` registry;
- добавить `PendingOwner` queue;
- перевести LINE на shell + pending owner + finalize;
- не добавлять entity в `pObjRoot` до resolve-фазы.

Проверка:

- fake test: raw LINE имеет owner handle блока, raw BLOCK_HEADER идет позже;
- после resolve LINE находится в block definition, а не в root;
- broken owner падает в fallback root с warning.

### Этап 3. Таблицы и ссылки визуальных свойств

Цель: entity получает корректные layer/linetype/style refs до `BuildGeometry`.

Изменения:

- расширить LAYER mapper;
- реализовать LTYPE mapper;
- начать STYLE mapper;
- добавить `PendingRef` для layer/ltype/style;
- fallback на system layer и `ByLayer` linetype при broken refs;
- перенести header codepage в общий decode path.

Проверка:

- LINE на слое, объявленном позже, после resolve получает правильный `vp.Layer`;
- отсутствующий layer дает warning и system layer;
- `TLOLoad` / `TLOMerge` соблюдаются для tables.

### Этап 4. Block definitions и model/paper space

Цель: корректно загружать block content и подготовить INSERT.

Изменения:

- создать block definition shells по `DWG_TYPE_BLOCK_HEADER`;
- распознать model space и paper space;
- связать first/last entity handles, если они доступны и полезны;
- добавлять block content через pending owner;
- defer geometry/format для block definition content.

Проверка:

- entity внутри block не попадает в model root;
- повторный block name обрабатывается как merge/duplicate по `LoadMode`;
- block content форматируется при использовании INSERT или финальной formatting-фазе drawing.

### Этап 5. MVP сущностей для видимой загрузки

Цель: получить полезную отрисовку типовых DWG.

Приоритет:

- `LINE`;
- `CIRCLE`;
- `ARC`;
- `POINT`;
- `LWPOLYLINE`;
- `TEXT`;
- `MTEXT`.

Для каждой сущности:

- отдельный mapper unit;
- отдельные tests на scalar fields;
- pending owner/ref/finalize contract;
- fallback на warning вместо падения.

### Этап 6. INSERT, attributes и размеры

Цель: загрузить блоковые ссылки и базовые dimension-объекты.

Изменения:

- `INSERT` -> `GDBObjBlockInsert`;
- refs на block definition по handle/name;
- `ATTRIB` / `ATTDEF` как дочерние или связанные entities по существующим ZCAD правилам;
- базовые `DIMENSION` по существующим dimension allocators;
- resolve generated dimension blocks.

### Этап 7. Proxy и unknown fallback

Цель: не терять unsupported graphics.

Изменения:

- исследовать доступные proxy fields LibreDWG;
- использовать существующий ZCAD proxy mechanism, если формат данных совместим;
- добавить debug stats по unknown/proxy;
- не хранить raw pointer после `dwg_free`.

### Этап 8. Паритет с DXF и hardening

Цель: приблизить DWG loader к DXF-паритету.

Добавить:

- `HATCH`;
- `SPLINE`;
- `ELLIPSE`;
- `SOLID`;
- `3DFACE`;
- `POLYLINE` variants;
- расширенные style tables;
- fixtures разных версий DWG;
- performance metrics для больших файлов.

## 13. Тестовая стратегия

Так как issue #1079 просит ТЗ, а не код, в рамках этого PR автоматизированный reproducing test не создается. Для реализации по этому ТЗ тесты обязательны.

Минимальный набор будущих тестов:

| Тест | Тип | Что проверяет |
|---|---|---|
| `handle_ref_absolute_vs_handleref` | unit | fallback `absolute_ref` -> `handleref.value` |
| `line_z_coordinate` | unit | LINE переносит `.z`, а не `.x` |
| `line_owner_declared_later` | unit/fake raw | parent-after-child resolve |
| `missing_owner_fallback` | unit/fake raw | fallback root + warning |
| `layer_declared_later` | unit/fake raw | deferred layer resolve |
| `dwg_fixture_smoke` | integration optional | чтение реального DWG при наличии LibreDWG |
| `unknown_entity_no_crash` | unit/integration | unsupported entity не роняет загрузку |

Тесты, которым не нужен LibreDWG, должны использовать fake raw records или thin mapper functions. Integration tests с реальной библиотекой должны skip-аться с понятной причиной, если `libredwg` недоступен в CI.

## 14. Definition of Done для реализации

Реализация считается готовой для первого production MVP, когда:

- `.dwg` открывается через существующую регистрацию формата;
- `dwg_free` гарантирован при любых ошибках после успешного read;
- LINE/LAYER загружаются без старого Z-coordinate бага;
- entity с owner, объявленным позже, попадает в правильный owner после resolve;
- entity без owner не теряется и получает warning;
- `BuildGeometry` и `FormatAfterDXFLoad` запускаются после attachment к owner;
- ZCAD loader не зависит от `fpdwg/inspector/fpdwg_document.pp`;
- есть unit tests на delayed owner resolve и LINE geometry;
- есть PR-описание с ручной проверкой на fixture DWG или объяснением, почему LibreDWG integration test был skipped.

## 15. Практический порядок следующих PR

1. PR "DWG loader stabilization": `try/finally`, LINE Z fix, common handle helpers, tests.
2. PR "DWG import context": `TDWGZCADLoadContext`, raw scan, handle registry, pending owner.
3. PR "DWG delayed owner resolve": LINE через pending owner, tests для parent-after-child.
4. PR "DWG tables phase": LAYER/LTYPE/STYLE + pending visual refs.
5. PR "DWG blocks phase": BLOCK_HEADER, block definition shells, model/paper space.
6. PR "DWG visible MVP": CIRCLE/ARC/POINT/LWPOLYLINE/TEXT/MTEXT.
7. PR "DWG INSERT MVP": block inserts and attributes.
8. PR "DWG proxy and unknown": fallback without data loss where possible.

Каждый PR должен сохранять правило: LibreDWG raw model и ZCAD drawing model существуют, import context только связывает их на время загрузки.
