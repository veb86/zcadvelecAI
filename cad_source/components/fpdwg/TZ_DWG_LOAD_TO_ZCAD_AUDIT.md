# Отчет по аудиту реализации DWG-загрузчика и план рефакторинга

> Issue: [veb86/zcadvelecAI#1101](https://github.com/veb86/zcadvelecAI/issues/1101)
> Базовое ТЗ: [`TZ_DWG_LOAD_TO_ZCAD.md`](./TZ_DWG_LOAD_TO_ZCAD.md)
> Реализованные этапы: 1-5 (commits `1bd9bc2fe`, `f7ef632f1`, `967ec7ee6`, `689c03170`, `6fc7f9d02`)
> Не реализованы: этапы 6, 7, 8.
> Статус документа: ревизия результата + ТЗ на рефакторинг + план достройки этапов 6-8.

## 1. Что было сделано в рамках этапов 1-5

| Этап | Commit | Ключевые изменения |
|---|---|---|
| 1. Стабилизация | `1bd9bc2fe` | `try/finally` вокруг `dwg_read_file`, helper'ы handle/text в `dwgproc.pp`, fix LINE Z-coordinate |
| 2. Import context + delayed owner | `f7ef632f1` | `uzedwgloadcontext` (handle map, pending owners, resolver с обнаружением циклов), `BeginDWGImport`/`EndDWGImport` |
| 3. Tables + pending refs | `967ec7ee6` | `AddLayer` + linetype ref, `AddLineType`, `AddTextStyle`; `PendingRefs` для layer/linetype/textstyle/dimstyle |
| 4. Block definitions | `689c03170` | `AddBlockHeader` (распознавание model/paper/user), регистрация `dokBlockDef`/`dokModelSpace`/`dokPaperSpace`, defer BuildGeometry для содержимого блоков |
| 5. MVP сущностей | `6fc7f9d02` | `CIRCLE`, `ARC`, `POINT`, `LWPOLYLINE`, `TEXT`, `MTEXT` mappers; helper `DWGRegisterEntityShell`, copy-helpers `DWGCopy*Props` в `dwgproc.pp` |

Все добавленные изменения сосредоточены в четырех файлах:

| Файл | Размер | Роль по факту |
|---|---|---|
| `cad_source/zengine/fileformats/uzefflibredwg.pas` | 194 строки | Точка входа `addfromdwg`/`addfromdxf`, `BeginDWGImport`/`EndDWGImport` |
| `cad_source/zengine/fileformats/uzefflibredwg2ents.pas` | **717 строк** | **ВСЕ** mapper'ы (LAYER, LTYPE, STYLE, BLOCK_HEADER, BLOCK, LINE, CIRCLE, ARC, POINT, LWPOLYLINE, TEXT, MTEXT) + attach-callback'и + lifecycle import context |
| `cad_source/zengine/fileformats/uzedwgloadcontext.pas` | 999 строк | Handle map, pending owner queue, pending ref queue, resolver, диагностика (одним юнитом) |
| `cad_source/components/fpdwg/dwgproc.pp` | 606 строк | LibreDWG dynamic load + handle/text/scalar copy helpers + `GDWGParser` |

Дополнительно реализованы тесты, но они лежат в `cad_source/components/fpdwg/inspector/tests/`:

- `fpdwg_test_loadcontext.pp` (1127 строк) — поведение `TDWGZCADLoadContext`;
- `fpdwg_test_dwgproc.pp` (534 строки) — handle/scalar helpers.

В `cad_source/components/fpdwg/` лежит **точная копия** `uzedwgloadcontext.pp` (999 строк, идентичен файлу из `zengine/fileformats/`).

## 2. Сопоставление с ТЗ `TZ_DWG_LOAD_TO_ZCAD.md`

### 2.1 Что соблюдено

- Главный архитектурный принцип «не создавать третью domain model» (§4.1-4.2) выдержан: `TDWGZCADLoadContext` живет только во время загрузки.
- Жизненный цикл `dwg_read_file` -> `dwg_free` через `try/finally` (§6.3) реализован.
- Контракт mapper'а LINE (§9.2): allocate с `nil`, копирование scalar полей (Z fix), `RegisterShell`, `QueueOwnerResolve`, `QueueRefResolve` — реализован.
- Состояния `msUnseen/msCreating/msCreated/msSkipped/msFailed`, `asPending/asResolving/asAttached/asFallback/asSkipped`, типы kind `dokLayer/dokLineType/.../dokModelSpace/dokPaperSpace/dokBlockDef/dokEntity` (§5.2, §7) — реализованы.
- Cycle-safe resolver (§5.4): self-owner, A->B->A, owner not container, owner not found — все ветки покрыты.
- Idempotent attachment (§5.3): `Pending^.AttachState in [asAttached, asFallback, asSkipped] -> Exit` присутствует.
- Коды диагностики `DWG_WARN_OWNER_*` и `DWG_WARN_REF_*` (§11) выделены.
- Регистрация `pObjRoot` под handle 0 в `BeginDWGImport` (§5.5: "ownerhandle = 0/nil -> добавить в `ZCDCtx.POwner`").
- LoadMode по `LayerTable.MergeItem` / `LTypeStyleTable.MergeItem` соблюдается (§8.2-8.3).
- Распознавание model-space / paper-space через `mspace_block`/`pspace_block` указатели + name fallback (§8.4) — добавлено для устойчивости к fixture-тестам.
- Defer BuildGeometry для содержимого блочных определений (§8.4 + §12.4: "block content форматируется при использовании INSERT или финальной formatting-фазе") — реализовано через `DWGOwnerIsBlockDef`.

### 2.2 Главное отступление: §6.5 «целевая файловая структура»

ТЗ §6.5 явно требует одного entity-типа на файл, лимит 1000 строк на любой hand-written unit, целевой размер 300-500 строк, и каталоги `cad_source/zengine/fileformats/dwg/` + `cad_source/zengine/fileformats/dwg/entities/`.

Фактическое состояние:

| Требование §6.5 | Реализация |
|---|---|
| Каталог `cad_source/zengine/fileformats/dwg/` | **отсутствует** |
| Каталог `cad_source/zengine/fileformats/dwg/entities/` | **отсутствует** |
| `uzedwgtypes.pas` (общие enums/records) | enums живут внутри `uzedwgloadcontext.pas` |
| `uzedwghandle.pas` (handle helpers) | helpers лежат в `dwgproc.pp` (общий с raw binding) |
| `uzedwgtext.pas` (safe decode) | `DWGSafeDecodeText` в `dwgproc.pp` |
| `uzedwgdiagnostics.pas` | warnings + counters внутри `uzedwgloadcontext.pas` |
| `uzedwgloadcontext.pas` (<= 600 строк, без per-type mapper'ов) | **999 строк**, превышен лимит на 60 % |
| `uzedwgrawscan.pas` (Phase 1 raw scan) | **отсутствует**: scan не выделен, mapper'ы вызываются напрямую из `parseDwg_Data` |
| `uzedwgloader.pas` (фазовая оркестровка) | оркестровка размазана между `uzefflibredwg.pas` и `uzefflibredwg2ents.pas` |
| `uzedwgtables.pas` | `AddLayer`/`AddLineType`/`AddTextStyle` сидят в `uzefflibredwg2ents.pas` |
| `uzedwgblocks.pas` | `AddBlockHeader`/`AddBlock` сидят в `uzefflibredwg2ents.pas` |
| `uzedwgresolver.pas` | resolver сидит внутри `uzedwgloadcontext.pas` |
| `uzedwgfinalize.pas` (`BuildGeometry` + `FormatAfterDXFLoad` + post-process) | **отсутствует**: только `BuildGeometry` вызывается прямо из `DWGAttachEntity`, `FormatAfterDXFLoad` и `FromDXFPostProcessAfterAdd` **пропущены** (см. §2.4) |
| `uzedwgentityregistry.pas` | регистрация в `initialization` блоке `uzefflibredwg2ents.pas` |
| `entities/uzedwgentbase.pas` | `DWGRegisterEntityShell` — общий хелпер сидит в `uzefflibredwg2ents.pas` |
| `entities/uzedwgentline.pas` | `AddLineEntity` в `uzefflibredwg2ents.pas` |
| `entities/uzedwgentcircle.pas` | `AddCircleEntity` в `uzefflibredwg2ents.pas` |
| `entities/uzedwgentarc.pas` | `AddArcEntity` в `uzefflibredwg2ents.pas` |
| `entities/uzedwgentpoint.pas` | `AddPointEntity` в `uzefflibredwg2ents.pas` |
| `entities/uzedwgentlwpolyline.pas` | `AddLWPolylineEntity` в `uzefflibredwg2ents.pas` |
| `entities/uzedwgenttext.pas` | `AddTextEntity` в `uzefflibredwg2ents.pas` |
| `entities/uzedwgentmtext.pas` | `AddMTextEntity` в `uzefflibredwg2ents.pas` |
| `dwg/tests/` | тесты лежат в `cad_source/components/fpdwg/inspector/tests/` |

В §6.4 явно сказано: «Целевое состояние - файл `uzefflibredwg2ents.pas` перестает быть местом, куда добавляется вся новая логика. Он может временно оставаться фасадом/registration unit для обратной совместимости, но все новые handlers должны жить в отдельных units ниже». Эта норма прямо нарушена: вся логика ровно в этом файле.

В §6.3 сказано «`uzefflibredwg.pas` <= 300 lines» — фактически 194, лимит соблюден.
В §6.4 «`uzefflibredwg2ents.pas` <= 250 lines, compatibility/registration only» — фактически 717 строк, превышение в 2.87 раза, и это **не** registration unit.
В §6.5 «`uzedwgloadcontext.pas` <= 600 lines» — фактически 999 строк, превышение на 66 %.

### 2.3 Дублирование `uzedwgloadcontext`

Файл `uzedwgloadcontext.pp` (999 строк) лежит в `cad_source/components/fpdwg/`, и **байт-в-байт** тот же файл `uzedwgloadcontext.pas` лежит в `cad_source/zengine/fileformats/`. Один из них — мертвый код (если расширения `.pp` и `.pas` указывают на разный uses). Дубликат должен быть устранен на этапе рефакторинга.

### 2.4 Пропущенные шаги Phase 4 (§4.3 / §9.1)

ТЗ требует на Phase 4 запускать:

```text
BuildGeometry
FormatAfterDXFLoad
FromDXFPostProcessAfterAdd
pack/correct object arrays
```

Реализация (`uzefflibredwg2ents.pas:119-125`):

- `BuildGeometry` вызывается, но **не для содержимого block definition** (это правильно для §12.4);
- `FormatAfterDXFLoad` **не вызывается** с явным комментарием «belong to the DXF code path and require a TDrawContext that the DWG pipeline does not yet thread through Stage 2»;
- `FromDXFPostProcessAfterAdd` **не вызывается**;
- pack/correct object arrays **не выполняется**.

Это означает, что DWG entity, попавший в model space через DWG-импорт, отрисуется (BuildGeometry), но не получит того post-processing, который применяется к DXF-импорту. На стиль Layer/LType `vp` это не повлияет (refs резолвятся через `DWGAttachRef`), но на vp фоновых полей, late hooks отрисовки и пакование контейнеров — повлияет. **Это незакрытый долг этапов 4-5**, требующий отдельного шага в плане.

### 2.5 Отсутствие raw scan фазы

ТЗ §4.3 / §5.3 описывает явную Phase 1 «scan raw graph: collect handles, raw indexes, object classes, owner refs», после которой Phase 2 одним проходом создает все shell'ы.

Реализация по факту использует `GDWGParser.parseDwg_Data` — линейный обход `dwg.&object[]`, в каждом обращении вызывается зарегистрированный handler, который сразу создает shell + помещает pending owner. Из-за того, что shell creation и pending queue работают через handle map с состояниями, **функционально** parent-after-child корректно решается на ResolveOwners. Но §5.3 требует, чтобы `RegisterShell` происходил после raw scan, а **до** того сохранялась `RawHandleIndex[handle] = raw index`.

Текущий механизм работает на тестах, но не дает:

- обнаружения duplicate handles до allocation (§4.3 Phase 1);
- корректной ассоциации handle -> raw index, нужной §6.5 для `RawIndex` поля `TDWGZCADHandleEntry` (поле есть, но всегда `-1`);
- разделения «ZCAD shell registry» от «raw scan registry».

Этот долг блокирует адекватную работу будущих этапов 6 (INSERT с переиспользуемыми block handle) и 8 (HATCH/SPLINE/DIMENSION с массивами refs внутри объекта).

### 2.6 Прочие замечания

- Регистрация по handle 0 для model space (`BeginDWGImport`) корректна, но при `AddBlockHeader` появилась вторая регистрация под реальным handle модельного пространства. Если real handle != 0 и при этом для entity указан owner == real model handle, она пойдет в `ZContext.PDrawing^.pObjRoot`, как и для handle == 0 — это согласовано (обе регистрации указывают на один pointer). Но при `LoadMode = TLOMerge` вторая регистрация одного pointer под двумя ключами усложняет диагностику — нужен явный stat «aliased model space».
- В `DWGOwnerIsBlockDef` блок-def распознается линейным обходом всех handle (`Handles.Count-1`) на каждый прикрепляемый entity. Для DWG с тысячами entity это `O(N*M)`. Стоит инлайнить bool-флаг `OwnerIsBlockDef` прямо в pending owner или хранить kind резолвленного владельца.
- `DWGAttachRef` для `rsDimStyle` пуст (`Same as above — reserved for Stage 4 dimension mappers.`). Этап 4 в текущей реализации не покрыл DIMENSION, поэтому ветка остается заглушкой и должна быть закрыта либо в этапе 6 (DIMENSION), либо помечена явно как **не реализовано** в публичном API.
- В `AddTextStyle`: при отсутствии стиля по имени **подставляется** `Standard` под зарегистрированным handle. Это означает, что если DWG содержит два разных стиля и оба попадают в fallback `Standard`, их handle указывают на один pointer. При TEXT/MTEXT с этими handle получится одинаковая отрисовка вне зависимости от исходного стиля — потеря данных. Нужно либо создавать новую запись стиля через `TextStyleTable.MergeItem`/аналог, либо явно отмечать ситуацию warning + skip.
- `uzefflibredwg2ents.pas` использует `uzeffLibreDWG` через `implementation uses`, и наоборот — `uzefflibredwg.pas` использует `uzeffLibreDWG2Ents`. Циклическая зависимость закрыта **обоюдным** `if ZCDWGParser=nil then Create` в обеих секциях `initialization`. Это работает, но рефакторинг должен ее разорвать — оркестровка (`addfromdwg`) и mapper'ы должны жить в разных слоях через registry-точку (`uzedwgentityregistry.pas` по §6.5).

## 3. План рефакторинга (этап 5.x: «приведение к §6.5»)

Цель: до того как стартовать этапы 6-8, физически разделить файлы в соответствии с §6.5 и закрыть Phase 4 + duplicate-файл.

### 3.1 Шаг R1. Подготовка файловой структуры

Создать каталоги:

```text
cad_source/zengine/fileformats/dwg/
cad_source/zengine/fileformats/dwg/entities/
cad_source/zengine/fileformats/dwg/tests/
```

Удалить дубликат `cad_source/components/fpdwg/uzedwgloadcontext.pp`. Если ему есть пользователи в inspector (поиск по uses показал, что нет, но проверить тесты `fpdwg_test_loadcontext.pp` — они смотрят на тот же символ через include path), оставить только одну канонiческую версию.

### 3.2 Шаг R2. Вынос инфраструктуры из `uzedwgloadcontext`

Из текущего `uzedwgloadcontext.pas` (999 строк) выделить:

| Новый юнит | Что переносится | Цель размера |
|---|---|---|
| `dwg/uzedwgtypes.pas` | `TDWGZCADHandle`, `TDWGZCADObjectKind`, `TDWGShellState`, `TDWGAttachState`, `TDWGAttachReason`, `TDWGZCADRefSlot`, `TDWGImportSeverity`, `TDWGZCADHandleEntry`, `TDWGZCADPendingOwner`, `TDWGZCADPendingRef`, `TDWGImportWarning`, callback типы `TDWGAttachProc`/`TDWGRefAttachProc`, константы `DWG_WARN_*`, `DWGAttachReasonToText` | <= 300 строк |
| `dwg/uzedwgdiagnostics.pas` | `TDWGImportWarning` запись, `TDWGImportWarningList`, счетчики `AttachCount`/`FallbackCount`/`CycleCount`/`RefAttachCount`/`RefFallbackCount`, форматировщики LogProc | <= 250 строк |
| `dwg/uzedwgloadcontext.pas` (оставшийся) | только `TDWGZCADLoadContext`: handle map, pending queues, конфиг fallback'ов, диспетчер `Resolve*`, `BeginDWGImport`/`EndDWGImport` API | <= 600 строк |
| `dwg/uzedwgresolver.pas` | `ResolvePending` + `ResolveRef` + cycle stack + warnings из контекста | <= 700 строк (с тестируемым выделением логики) |

Переходный приём: `uzedwgloadcontext.pas` объявляет `re-export` через `uses` секцию interface для юнитов, которые сейчас полагаются на единый импорт; это снижает риск массовых правок uses-списков на одном PR.

### 3.3 Шаг R3. Вынос `dwg/uzedwghandle.pas` и `dwg/uzedwgtext.pas`

Из `dwgproc.pp` извлечь:

- `DWGObjectHandleValue`, `DWGObjectOwnerHandleValue`, `DWGRefHandleValue`, `DWGEntityLayerHandleValue`, `DWGEntityLineTypeHandleValue`, `DWGLayerLineTypeHandleValue`, `DWGTextStyleHandleValue`, `DWGMTextStyleHandleValue` -> `dwg/uzedwghandle.pas` (<= 350 строк);
- `BITCODE_T2Text`, `DWGSafeDecodeText` -> `dwg/uzedwgtext.pas` (<= 250 строк).

`dwgproc.pp` остается чисто binding/loader unit для `libredwg.so`/`libredwg-0.dll` и `GDWGParser` (см. R5).

### 3.4 Шаг R4. Phase 1 / Phase 2 разделение

Создать `dwg/uzedwgrawscan.pas`:

```pascal
// Стадия R4: предсканирование dwg.&object[] до начала allocation.
procedure ScanRawObjects(var Raw: Dwg_Data; var Ctx: TDWGZCADLoadContext);
```

Контракт:

- проход по `Raw.&object[i]`;
- регистрация handle -> raw index в `Ctx.RawIndex`;
- обнаружение duplicate handle до allocation (warning DWG_WARN_DUPLICATE_HANDLE на ранней фазе);
- определение supertype/fixedtype для будущей маршрутизации mapper'а.

После этой фазы `parseDwg_Data` остается, но `RegisterShell` начинает использовать `RawIndex`, и duplicate-warning возникает один раз на pre-scan, а не на каждом allocation.

### 3.5 Шаг R5. Регистрация mapper'ов через `uzedwgentityregistry`

Создать `dwg/uzedwgentityregistry.pas` со следующим API:

```pascal
type
  TDWGEntityHandler = procedure(var Ctx: TDWGZCADLoadContext;
    var DWGContext: TDWGCtx; var DWGObject: Dwg_Object; P: Pointer);

procedure RegisterDWGEntityHandler(DOT: DWG_OBJECT_TYPE; H: TDWGEntityHandler);
procedure RegisterDWGObjectHandler(DOT: DWG_OBJECT_TYPE; H: TDWGEntityHandler);
function FindDWGHandler(DOT: DWG_OBJECT_TYPE; out H: TDWGEntityHandler;
  out IsEntity: Boolean): Boolean;
```

Внутренняя реализация переиспользует `GDWGParser`'s dictionary, но скрывает его за публичным API. Это разрывает циклический uses между `uzefflibredwg.pas` и `uzefflibredwg2ents.pas`: оркестровка получает мапу через `uzedwgentityregistry`, не подключая каждый mapper-юнит напрямую.

### 3.6 Шаг R6. Раскатка mapper'ов по entity-юнитам

Каждый существующий `Add*Entity`/`Add*` вынести в свой юнит и переписать в современный сигнатурный стиль:

| Юнит | Источник | Целевой размер |
|---|---|---|
| `dwg/entities/uzedwgentbase.pas` | `DWGRegisterEntityShell` + общий wrapper | <= 500 |
| `dwg/entities/uzedwgentline.pas` | `AddLineEntity` | <= 250 |
| `dwg/entities/uzedwgentcircle.pas` | `AddCircleEntity` | <= 250 |
| `dwg/entities/uzedwgentarc.pas` | `AddArcEntity` | <= 300 |
| `dwg/entities/uzedwgentpoint.pas` | `AddPointEntity` | <= 250 |
| `dwg/entities/uzedwgentlwpolyline.pas` | `AddLWPolylineEntity` | <= 500 |
| `dwg/entities/uzedwgenttext.pas` | `AddTextEntity` | <= 400 |
| `dwg/entities/uzedwgentmtext.pas` | `AddMTextEntity` | <= 500 |
| `dwg/uzedwgtables.pas` | `AddLayer`, `AddLineType`, `AddTextStyle` | <= 700 |
| `dwg/uzedwgblocks.pas` | `AddBlockHeader`, `AddBlock`, `DWGBlockHeaderIsModelSpace`, `DWGBlockHeaderIsPaperSpace`, `DWGOwnerIsBlockDef` | <= 700 |

Каждый mapper-юнит регистрируется через `uzedwgentityregistry` в своей секции `initialization`. Старый `uzefflibredwg2ents.pas` сжимается до compatibility-фасада (re-export старых символов плюс агрегированный `uses` всех новых юнитов для сохранения единой точки подключения mapper'ов в `uzefflibredwg.pas`). Целевой размер <= 250 строк.

### 3.7 Шаг R7. Phase 4: `dwg/uzedwgfinalize.pas`

Создать юнит, ответственный за:

- `BuildGeometry` после `ResolveOwners` для всех `dokEntity`-shell'ов с привязанным owner;
- `FormatAfterDXFLoad` (нужен `TDrawContext` — пробросить из вызывающего DWG loader через параметр `addfromdwg`);
- `FromDXFPostProcessAfterAdd`;
- pack/correct object arrays через `BlockDefArray.PackArray` / `pObjRoot.PackArray` (по аналогии с DXF-loader).

Контракт:

```pascal
procedure FinalizeImport(var Ctx: TDWGZCADLoadContext;
  Drawing: PTSimpleDrawing; const DC: TDrawContext);
```

Перенести вызовы `PGDBObjEntity(pobj)^.BuildGeometry(LoadDrawing^)` из `DWGAttachEntity` в `FinalizeImport`, чтобы attach остался идемпотентным и не зависел от состояния drawing на момент resolve.

### 3.8 Шаг R8. Перенос тестов

Перенести `cad_source/components/fpdwg/inspector/tests/fpdwg_test_loadcontext.pp` и `fpdwg_test_dwgproc.pp` в `cad_source/zengine/fileformats/dwg/tests/` под именами `uzedwgtestloadcontext.pas`, `uzedwgtestdwgproc.pas`. Оставить под inspector только тесты, действительно зависящие от inspector domain model.

Добавить новые тесты, которые сейчас отсутствуют:

- `uzedwgtestrawscan.pas` — duplicate handle + raw index registration;
- `uzedwgtestfinalize.pas` — `BuildGeometry` для entity в model space, defer для block-def, post-process invocation;
- `uzedwgtestentityregistry.pas` — двойная регистрация одного DOT.

### 3.9 Definition of Done для рефакторинга

- ни один hand-written `.pas`/`.pp` под `dwg/` и `dwg/entities/` не превышает 1000 строк, целевой 300-500;
- `uzefflibredwg2ents.pas` <= 250 строк, фактически compatibility-фасад;
- `uzedwgloadcontext.pas` <= 600 строк;
- ровно одна копия каждого юнита: дубликат в `cad_source/components/fpdwg/` устранен;
- `uzefflibredwg.pas` -> `uzedwgentityregistry` -> mapper'ы (нет циклической `uses`);
- все существующие зеленые тесты (Stage 1-5) остаются зелеными;
- `BuildGeometry`, `FormatAfterDXFLoad`, `FromDXFPostProcessAfterAdd` вызываются из `uzedwgfinalize`.

## 4. План достройки этапов 6, 7, 8

После R1-R8 архитектура готова к расширению. Ниже — расшифровка ТЗ §12.6-12.8 в виде задач для PR.

### 4.1 Этап 6. INSERT, ATTRIB/ATTDEF, базовые DIMENSION (TZ §12.6)

#### 6.1 INSERT -> `GDBObjBlockInsert`

Файлы:

- `dwg/entities/uzedwgentinsert.pas` (<= 500 строк): mapper для `DWG_TYPE_INSERT`.

Поля LibreDWG (из `Dwg_Entity_INSERT`):

- `ins_pt` (insert point);
- `scale_flag` + `scale.x`/`scale.y`/`scale.z`;
- `rotation`;
- `extrusion`;
- `block_header` (BITCODE_H -> handle на BLOCK_HEADER);
- `has_attribs`, `num_owned`;
- `column_count`/`column_spacing`/`row_count`/`row_spacing` (для MINSERT);
- `first_attrib`/`last_attrib`/`seqend` (BITCODE_H — handle цепочки атрибутов);
- `attrib_handles[]` — массив handle на ATTRIB.

Контракт mapper'а:

1. `AllocAndInitBlockInsert(nil)`.
2. Скопировать `ins_pt`, `scale`, `rotation`, `extrusion`.
3. `RegisterShell(EntityHandle, dokEntity, pobj)`.
4. `QueueOwnerResolve(...)` (как для линий).
5. `QueueRefResolve(EntityHandle, BlockHandle, dokBlockDef, rsBlockDef)` — **новый slot**, добавить в `TDWGZCADRefSlot`.
6. `QueueRefResolve` для `layer`/`linetype` (стандартный набор).
7. Если `has_attribs<>0`, для каждого handle из `attrib_handles[]` поставить pending child link «этот ATTRIB должен попасть как child этого INSERT» (см. 6.3).

Изменения вне entity-юнита:

- расширить `TDWGZCADRefSlot` на `rsBlockDef`;
- в `DWGAttachRef` обработать `rsBlockDef` -> запись в insert vp/`pBlockDef`;
- в `uzedwgloadcontext` ввести `dokBlockRef` или переиспользовать существующий `dokBlockDef` через kind-check.

Тесты (`dwg/tests/uzedwgtestinsert.pas`):

- INSERT, у которого block declared earlier -> resolve;
- INSERT, у которого block declared later -> deferred resolve;
- INSERT с broken block handle -> fallback (`*ANONYMOUS` block? предлагать warning `DWG_WARN_REF_NOT_FOUND` с slot=rsBlockDef, и не падать);
- INSERT в block definition контексте (вложенные блоки).

#### 6.2 MINSERT (массив INSERT) — расширение 6.1

Если `column_count>1` или `row_count>1`, mapper создает один `GDBObjBlockInsert` и сохраняет array-параметры. Полный mapping массива на N независимых entity делается в Phase 4 (finalize) через существующий ZCAD-механизм.

#### 6.3 ATTRIB / ATTDEF

Файлы:

- `dwg/entities/uzedwgentattrib.pas` (<= 450 строк).

Поля LibreDWG `Dwg_Entity_ATTRIB`/`Dwg_Entity_ATTDEF`: общие с TEXT (`ins_pt`, `height`, `rotation`, `text_value`, `tag`, `prompt`, `flags`, `style`).

ATTRIB/ATTDEF в DWG лежат как самостоятельные entity, привязанные к INSERT через `prev_handle/next_handle/seqend` или `attrib_handles[]`. ZCAD-аналог — `GDBObjAttDef`/`GDBObjAttrib`, размещаемые внутри block-insert через `AddMi`.

Контракт:

1. Создание ZCAD attrib-entity (по аналогии с TEXT, плюс `tag`).
2. `RegisterShell(handle, dokEntity, pobj)`.
3. `QueueOwnerResolve` с `OwnerHandle = INSERT handle` (берется из `Raw.tio.entity^.ownerhandle`).
4. `QueueRefResolve` для layer/linetype/textstyle.

Тесты:

- ATTRIB, owner которого — INSERT, declared earlier;
- ATTDEF в block definition;
- broken owner -> fallback root.

#### 6.4 Базовые DIMENSION

Файлы:

- `dwg/entities/uzedwgentdimension.pas` (<= 700 строк) — диспетчер по `DWG_TYPE_DIMENSION_LINEAR`/`DIMENSION_ALIGNED`/`DIMENSION_RADIUS`/`DIMENSION_DIAMETER`/`DIMENSION_ANGULAR_LN2`/`DIMENSION_ANGULAR_PT3`/`DIMENSION_ORDINATE`.

Подходы:

- DIMENSION в DWG ссылается на generated block с геометрией размера (`block` BITCODE_H). ZCAD имеет аналогичный механизм. Mapper:
  1. Создать `GDBObjDim*` (тип по DOT).
  2. Скопировать геометрию: `def_pt`, `text_midpt`, `ext_line_pt1`/`pt2`, `ins_pt`, `rotation`, `text_rotation`, измерение `act_measurement`.
  3. `QueueRefResolve(BlockHandle, dokBlockDef, rsBlockDef)` для generated dimension block.
  4. `QueueRefResolve(StyleHandle, dokDimStyle, rsDimStyle)` — здесь активируется `rsDimStyle` ветка `DWGAttachRef`, до этого она была заглушкой.
  5. `QueueOwnerResolve` как у обычной entity.

Сопровождающие изменения:

- `dwg/uzedwgtables.pas`: добавить `AddDimStyle` mapper для `DWG_TYPE_DIMSTYLE`, регистрация в `dokDimStyle` shell registry.
- `DWGAttachRef`/`rsDimStyle`: реальная запись `vp.DimStyle` (или соответствующего поля у dim-entity).

Тесты:

- LINEAR DIM с обоими endpoints + dimstyle declared later;
- RADIUS/DIAMETER на CIRCLE;
- ANGULAR с тремя точками;
- broken dimstyle -> fallback на default style + warning.

#### 6.5 Связи с резолвером

Резолвер должен обработать новый `rsBlockDef` slot: ожидаемый kind = `dokBlockDef`. Если попадает `dokModelSpace`/`dokPaperSpace`, это **не** ошибка для INSERT (некоторые xref'ы), но warning стоит выдавать в info-уровне.

#### 6.6 Definition of Done этапа 6

- `INSERT`, `ATTRIB`, `ATTDEF`, базовые `DIMENSION` загружаются и отрисовываются;
- `BLOCK_DEF` declared later корректно резолвится;
- `DIMSTYLE` declared later корректно резолвится в новом `rsDimStyle` slot;
- broken block reference -> fallback с warning, без падения;
- размеры в model space строятся через `BuildGeometry`; в block definition остаются deferred до finalize;
- тесты на parent-after-child для INSERT, ATTRIB->INSERT, DIM->BLOCK->DIMSTYLE цепочек.

### 4.2 Этап 7. Proxy и unknown fallback (TZ §12.7 / §10)

#### 7.1 Аудит доступных в LibreDWG proxy-полей

В LibreDWG есть `Dwg_Entity_PROXY_ENTITY` и `Dwg_Object_PROXY_OBJECT` с полями `proxy_class` (handle на PROXY_CLASS), `data_size`, `data` (BITCODE_TF), `graphics_present`, `graphics_size`, `graphics`, `version`. Задача — выяснить, какой минимум полей нужен ZCAD'у для сохранения «proxy graphics» в виде ZCAD-сущности.

Файлы:

- `dwg/entities/uzedwgentproxy.pas` (<= 350 строк): mapper для `DWG_TYPE_PROXY_ENTITY` и `DWG_TYPE__3DSOLID`/`REGION`/`BODY` через единый «opaque» путь, если у ZCAD нет реализаций.

#### 7.2 Использование существующего ZCAD proxy mechanism

ZCAD использует `GDBObjProxyEntity` (если такой класс есть; иначе — ближайший `GDBObjGenericSubEntry`). Mapper:

1. Прочитать proxy graphics как массив byte (NB: пока `dwg_free` еще не вызван, нужно скопировать данные **до** конца импорта).
2. Создать ZCAD entity с прикрепленным графическим payload'ом.
3. `RegisterShell` + `QueueOwnerResolve` + `QueueRefResolve(layer)`.

#### 7.3 Debug stats по unknown/proxy

Расширить `TDWGImportStats` (см. §7) полями:

```pascal
UnknownEntities: Integer;
UnknownObjects: Integer;
ProxiesLoaded: Integer;
ProxiesFailed: Integer;
DroppedDueToFreedRaw: Integer;
```

Логировать после `EndDWGImport` через `zDebugLn`.

#### 7.4 Запрет на хранение raw pointer после `dwg_free`

В `TDWGZCADLoadContext.Done` (или эквивалентный teardown) пройти все `Handles.EntryAt(*).Ptr` и проверить, что ни один не указывает в `Raw`-память. Это контракт §12.7 «не хранить raw pointer после `dwg_free`».

Реализация: для unsupported entity, у которой не скопированы все нужные поля, на этапе scan/shell либо копируется payload в heap-копию ZCAD entity, либо entity помечается `msSkipped` + warning `DWG_WARN_UNKNOWN_NO_COPY`.

#### 7.5 Definition of Done этапа 7

- proxy entities из реальных DWG не теряются (ZCAD-proxy entity создается);
- unsupported entity без proxy graphics дает warning + статистику, но не падает;
- corrupted entity (например, `tio.entity = nil`) дает error-warning и пропускает объект;
- `dwg_free` гарантированно вызывается; ZCAD-объекты после него self-contained;
- тест `unknown_entity_no_crash` (см. §13) реализован.

### 4.3 Этап 8. Паритет с DXF и hardening (TZ §12.8)

#### 8.1 Дополнительные entity-юниты

| Тип | Юнит | Размер | Особенности |
|---|---|---|---|
| `HATCH` | `dwg/entities/uzedwgenthatch.pas` | <= 700 | сложные boundary loops, gradient, pattern |
| `SPLINE` | `dwg/entities/uzedwgentspline.pas` | <= 650 | control points, knots, fit points |
| `ELLIPSE` | `dwg/entities/uzedwgentellipse.pas` | <= 450 | center, major axis, ratio, start/end angle |
| `SOLID` | `dwg/entities/uzedwgentsolid.pas` | <= 350 | 4 corners |
| `3DFACE` | `dwg/entities/uzedwgent3dface.pas` | <= 350 | 4 corners + invisible flags |
| `POLYLINE_2D`/`POLYLINE_3D`/`POLYLINE_PFACE`/`POLYLINE_MESH` | `dwg/entities/uzedwgentpolyline.pas` | <= 600 | sub-entity VERTEX и SEQEND, обработка через pending child links |
| `VERTEX_*` | внутри `uzedwgentpolyline.pas` или отдельно | — | child of POLYLINE по seq link |
| `RAY`/`XLINE` | `dwg/entities/uzedwgentray.pas` (если требуется) | <= 250 | две точки |

Каждый — отдельный юнит, отдельный тест.

#### 8.2 Расширенные style tables

- `MLINESTYLE` -> `dwg/uzedwgtables.pas` add `AddMLineStyle`;
- `UCS` -> при необходимости поддержки именованных UCS;
- `VPORT` для paper space layouts (при выходе на полноценный paper space в этапе после 8).

#### 8.3 Fixtures разных версий DWG

Подготовить тестовые DWG (R2000, R2004, R2007, R2010, R2013, R2018, R2032 если выпущен) и интеграционные тесты, которые в CI **skip-аются** при отсутствии `libredwg`. Минимум:

- `tests/fixtures/dwg/r2007_basic.dwg` (LINE+CIRCLE+TEXT+LAYER на одном слое);
- `tests/fixtures/dwg/r2018_with_block.dwg` (BLOCK_HEADER + INSERT + ATTRIB);
- `tests/fixtures/dwg/r2007_dim_radial.dwg`;
- `tests/fixtures/dwg/r2010_proxy.dwg` (от стороннего CAD).

Утилита для добавления fixture: реверс-инспектор пишет минимальный raw `Dwg_Data` через LibreDWG API и сохраняет результат, чтобы fixture был воспроизводимый.

#### 8.4 Performance metrics

В `TDWGImportStats` добавить:

- `LoadDurationMs: Int64`;
- `EntityCount: Integer`;
- `OwnerLookupHits` / `OwnerLookupMisses`;
- `RefLookupHits` / `RefLookupMisses`.

Использовать `EpikTimer`/`Now`/`GetTickCount64` для измерения. Для крупных файлов (>=10 MB) лог должен показывать «scan: X ms, shell: Y ms, resolve: Z ms, finalize: W ms».

Дополнительно: оптимизация `DWGOwnerIsBlockDef` (см. §2.6) и `FindByEntityHandle` (`O(n)` -> `O(log n)` при сортировке pending owners).

#### 8.5 Definition of Done этапа 8

- хотя бы по одному fixture для каждой целевой версии DWG;
- HATCH/SPLINE/ELLIPSE/SOLID/3DFACE/POLYLINE_2D/3D/PFACE/MESH рендерятся;
- интеграционные тесты skip-аются при отсутствии libredwg, но запускаются в локальной разработке;
- performance log по фазам присутствует;
- регрессии по этапам 1-7 отсутствуют (зеленые `fpdwg_tests` + новые `uzedwgtest*`).

## 5. Порядок и оценка PR

| PR | Шаги | Зависит от | Заметка |
|---|---|---|---|
| PR-RA | R1, R2, R3 | — | подготовка `dwg/` каталога, разделение `uzedwgloadcontext.pas`, перенос handle/text helpers; обнуление дублирующего `uzedwgloadcontext.pp` |
| PR-RB | R5, R6 | RA | разнос mapper'ов по entity-юнитам, registry, `uzefflibredwg2ents.pas` -> compatibility |
| PR-RC | R4, R7, R8 | RB | raw scan фаза, `uzedwgfinalize.pas`, перенос тестов в `dwg/tests/`, добавление новых тестов |
| PR-6A | этап 6.1 | RC | INSERT mapper + `rsBlockDef` slot |
| PR-6B | этап 6.3 | 6A | ATTRIB/ATTDEF |
| PR-6C | этап 6.4 | 6B | DIMSTYLE shell + базовые DIMENSION |
| PR-7 | этап 7 | 6C | proxy + unknown fallback + stats |
| PR-8A | этап 8.1 (часть): ELLIPSE, SOLID, 3DFACE | 7 | низкозависимые типы |
| PR-8B | этап 8.1: POLYLINE family | 8A | sub-entity link через pending child |
| PR-8C | этап 8.1: HATCH, SPLINE | 8B | сложная геометрия |
| PR-8D | этап 8.3, 8.4 | 8C | fixtures, performance metrics |

Каждый PR сохраняет правило ТЗ §15: «LibreDWG raw model и ZCAD drawing model существуют, import context только связывает их на время загрузки».

## 6. Риски и открытые вопросы

1. **Цикл `uzefflibredwg.pas` <-> `uzefflibredwg2ents.pas`**. Сейчас закрыт обоюдным `if ZCDWGParser=nil then Create`. Шаг R5 разрывает цикл через registry. Нужно проверить, что компилятор FPC не зависит от текущего порядка.
2. **Отсутствие `TDrawContext` на DWG-пути**. `FormatAfterDXFLoad` требует `TDrawContext`. Нужно либо пробросить его как параметр в `addfromdwg`, либо инициализировать минимальный stub. Решение: проброс из caller, как у DXF.
3. **Дубликат `uzedwgloadcontext.pp` в `cad_source/components/fpdwg/`**. Перед удалением проверить, не подключается ли он какими-либо тестами через include path.
4. **`AddTextStyle` теряет данные, подменяя на `Standard`**. Нужно решить в Stage 6 или Stage 8: создавать новую запись, либо сохранять оригинальное имя как алиас.
5. **`O(N*M)` в `DWGOwnerIsBlockDef`**. Пока это работает, но после Stage 6 (INSERT с большим числом entity) начнет быть заметным. Оптимизация — в Stage 8.4.
6. **Дубликат `uzedwgloadcontext` юнита по обеим расширениям**. Вместе с дубликатом файла существует `*.pp` и `*.pas` имени. Один из них надо признать каноном. Предлагается `cad_source/zengine/fileformats/dwg/uzedwgloadcontext.pas` как канон, `.pp` копию из `fpdwg/` удалить.

## 7. Сводный чек-лист для следующего PR (приоритетные)

- [ ] Создать `cad_source/zengine/fileformats/dwg/` и `dwg/entities/`, `dwg/tests/`.
- [ ] Удалить `cad_source/components/fpdwg/uzedwgloadcontext.pp` (дубликат).
- [ ] Разделить `uzedwgloadcontext.pas` на `uzedwgtypes.pas` + `uzedwgdiagnostics.pas` + ужатый `uzedwgloadcontext.pas` + `uzedwgresolver.pas`.
- [ ] Извлечь handle/text helpers из `dwgproc.pp` в `uzedwghandle.pas` + `uzedwgtext.pas`.
- [ ] Создать `uzedwgentityregistry.pas` и перевести регистрацию mapper'ов на него.
- [ ] Раскатать каждый `Add*` mapper в свой `entities/uzedwgent*.pas` (8 файлов).
- [ ] Сжать `uzefflibredwg2ents.pas` до compatibility-фасада <= 250 строк.
- [ ] Добавить `uzedwgrawscan.pas` и `uzedwgfinalize.pas` (Phase 1 / Phase 4).
- [ ] Перенести `fpdwg_test_loadcontext.pp` и `fpdwg_test_dwgproc.pp` в `dwg/tests/` под новыми именами.
- [ ] Прогнать `fpdwg_tests` + новый набор тестов; обеспечить зеленый CI.

После выполнения этого чек-листа открывается дорога к этапам 6-8 без накапливания нового долга в монолитном `uzefflibredwg2ents.pas`.
