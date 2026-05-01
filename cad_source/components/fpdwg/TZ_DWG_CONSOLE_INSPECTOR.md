# Техническое задание: консольный инспектор DWG на базе fpdwg и LibreDWG

> Issue: [veb86/zcadvelecAI#1041](https://github.com/veb86/zcadvelecAI/issues/1041)
> Доработка: [veb86/zcadvelecAI#1043](https://github.com/veb86/zcadvelecAI/issues/1043)
> Редакция: 2.0 (учтены замечания специалистов из `TZ_for_analysis_1.md` и `TZ_for_analysis_2.md`)
> Рабочая папка: `cad_source/components/fpdwg/`
> Цель: описать изменение проекта `fpdwg.lpk` и создание консольного приложения, которое читает DWG через LibreDWG, строит граф объектов по handle-ссылкам и выводит полную информацию об объектах чертежа.

---

## 0. История редакций

- **1.0** — первая версия ТЗ (issue #1041): архитектура слоёв, двухфазная загрузка, базовая модель.
- **2.0** — настоящая редакция (issue #1043). Учтены замечания внешних специалистов:
  - явная модель состояний объекта (`RAW / RESOLVED / PARTIAL / BROKEN / FAILED`);
  - честная политика по lazy mode — отказ в первой версии в пользу фильтрации (см. §11);
  - `TDWGHandleRef.Source` для диагностики handle resolution на разных версиях DWG;
  - `RawBytes` в `TDWGUnknownObject` опционально по флагу CLI;
  - synthetic объекты для таблиц (Layer table, Linetype table) — чтобы `OwnerHandle` записей таблиц резолвился;
  - `length_xy` и `length_3d` для LINE (см. §7.5);
  - стратегия конфликтов handle (strict/tolerant);
  - безопасный декодер текста (`SafeDecodeText`) с fallback на hex-дамп;
  - расширенный набор интерактивных консольных команд (`ls`, `cd`, `show`, `dump`, `tree`, `refs`, `owners`, `find`, `validate`, `stats`) — поверх batch CLI, который остаётся MVP;
  - таблица рисков и митигаций (§19);
  - расширенный набор fixture-DWG для разных версий (R14..R2018).

---

## 1. Назначение

Нужно развить пакет `fpdwg` из набора низкоуровневых Pascal-биндингов LibreDWG в отдельный слой анализа DWG-файла и консольный инструмент поверх него.

Целевой результат:

```text
DWG = граф объектов + отложенное разрешение ссылок + явные состояния узлов
```

Консольное приложение должно:

- открыть `.dwg` через LibreDWG;
- прочитать header, tables, dictionaries, blocks, entities;
- сохранить каждый объект в доменной модели с собственным handle;
- построить глобальный индекс `Handle -> Object`;
- выполнить вторую фазу разрешения ссылок;
- вывести сведения об объектах: тип, handle, owner, layer, linetype, цвет, геометрию, длины линий, имена слоев, типы линий, блоки, неизвестные объекты, ошибки загрузки;
- не падать на неизвестных, частично поддержанных или повреждённых DWG-объектах;
- сохранять работоспособность даже на повреждённом или нестандартном DWG.

Это ТЗ не требует сразу реализовывать полный CAD-импорт в ZCAD. Задача инструмента — инспекция DWG и формирование устойчивой архитектурной основы для будущей загрузки.

### 1.1 Сценарии использования

ТЗ ориентируется на три явных сценария:

| Сценарий | Пользователь | Что он делает |
|---|---|---|
| Debug парсера | разработчик ZCAD/fpdwg | смотрит, как LibreDWG распарсил конкретный объект, ищет broken refs, неизвестные типы, расхождения версий |
| Анализ DWG | инженер | хочет получить статистику: сколько слоёв, линий, блоков; какие объекты есть в файле |
| Reverse engineering | разработчик нового маппера | смотрит сырые поля неизвестного объекта, чтобы понять его структуру и написать mapper |

### 1.2 Что НЕ делаем (границы)

- **Не** реализуем полноценный CAD-импорт в ZCAD;
- **Не** поддерживаем запись или модификацию DWG;
- **Не** делаем GUI;
- **Не** реализуем все 70+ типов объектов DWG в первой версии;
- **Не** поддерживаем частичную загрузку в смысле `mmap`/lazy-чтения файла — только фильтрацию объектов в доменной модели.

---

## 2. Текущее состояние проекта fpdwg

### 2.1 Состав каталога

В `cad_source/components/fpdwg/` сейчас находятся:

| Файл | Назначение |
|---|---|
| `fpdwg.lpk` | Lazarus package `fpdwg`, подключает `dwg.pp` и `dwgproc.pp` |
| `fpdwg.pas` | автосгенерированный Lazarus package unit |
| `dwg.pp` | h2pas-биндинг к `dwg.h` LibreDWG, содержит типы `Dwg_Data`, `Dwg_Object`, `Dwg_Object_Ref`, `DWG_OBJECT_TYPE`, entity/object records |
| `dwgproc.pp` | динамическая загрузка `libredwg.so` / `libredwg-0.dll`, функции `dwg_read_file`, `dxf_read_file`, `dwg_free`, базовый generic parser `GDWGParser<GUserCtx>` |
| `dwg_test.lpr` / `dwg_test.lpi` | минимальное тестовое приложение, сейчас печатает часть header и текстовые entity |
| `libredwg/dwg.h` | исходный C-header для генерации Pascal-биндинга |

### 2.2 Сильные стороны текущей базы

- Уже есть актуальный Pascal-биндинг LibreDWG (`dwg.pp`).
- `dwgproc.pp` умеет динамически подгружать LibreDWG без жесткой линковки.
- `DWG_OBJECT_TYPE` покрывает широкий набор стандартных и расширенных типов.
- В `Dwg_Object` есть `handle: Dwg_Handle`, `fixedtype`, `supertype`, `name`, `dxfname`, `unknown_bits`, `unknown_rest`.
- В `Dwg_Object_Entity` уже есть common entity handles: `ownerhandle`, `layer`, `ltype`, `material`, `plotstyle`, `prev_entity`, `next_entity`.
- В `Dwg_Object_Ref` есть `handleref`, `absolute_ref` и resolved pointer `obj`.
- `GDWGParser` уже реализует регистрацию обработчиков по `DWG_OBJECT_TYPE`.

### 2.3 Ограничения текущей базы

- `dwg_test.lpr` является диагностическим примером, а не архитектурой инспектора: он не строит registry, не разрешает ссылки, не агрегирует ошибки.
- `GDWGParser.parseDwg_Data` сейчас проходит `dwg.object[]` одним циклом и вызывает обработчики сразу. Для DWG-графа этого недостаточно: forward references, циклы, owner/child-связи и битые handles требуют двух фаз.
- Обработчики в `cad_source/zengine/fileformats/uzefflibredwg2ents.pas` относятся к загрузке в ZCAD, а не к автономному `fpdwg`-инспектору. Их нельзя считать целевой моделью для консольного приложения.
- Неизвестные типы сейчас легко пропустить молча, если для `fixedtype` нет зарегистрированного обработчика.
- В `dwgproc.pp` экспортированы только `dwg_read_file`, `dxf_read_file`, `dwg_free`. Для полноценного inspector API желательно добавить безопасные обертки для resolve/find-функций LibreDWG только там, где они реально нужны.
- Порядок объектов в `dwg.object[]` не гарантирует, что родитель идёт перед потомком, поэтому forward-references неизбежны.
- Между `dwg_read_file` и `dwg_free` возможны утечки при исключениях — нужна явная обвязка `try/finally`.

---

## 3. Корректировки к исходному наброску

Исходный набросок верно задает главное: DWG надо читать как граф объектов с отложенным разрешением ссылок. Есть несколько уточнений.

### 3.1 `TDWGHandle = UInt64`, плюс источник handle

В LibreDWG handle представлен как `BITCODE_HV = UInt64`, а ссылка на объект — как `BITCODE_H = ^Dwg_Object_Ref`. Поэтому в доменной модели нужно различать:

```pascal
type
  TDWGHandle = UInt64;       // абсолютное значение handle

  TDWGHandleSource = (
    hsNull,         // BITCODE_H = nil или оба поля == 0
    hsAbsoluteRef,  // взято из Ref^.absolute_ref
    hsHandleref     // взято из Ref^.handleref.value (fallback)
  );

  TDWGHandleRef = record
    Value: TDWGHandle;
    Source: TDWGHandleSource;
    function IsNull: Boolean;
    function ToString: string;   // hex без префикса '0x', например '2A'
    class function Null: TDWGHandleRef; static;
  end;
```

`TDWGHandle` должен хранить именно `Dwg_Handle.value` или `Dwg_Object_Ref.absolute_ref`, а не указатель `BITCODE_H`. Поле `Source` — диагностическое: оно нужно, чтобы при анализе разных версий DWG (R14..R2018) видеть, откуда реально взялся handle. На некоторых версиях `absolute_ref` может быть нулевым даже для валидных ссылок, и это не должно превращаться в ложный broken-ref.

### 3.2 Двухфазность обязательна, но фаз больше двух

Внешне архитектура должна оставаться двухфазной:

```text
Phase 1: Allocation (без связей)
Phase 2: Resolve (связи)
```

Практически конвейер лучше разложить так:

```text
Read LibreDWG -> Build raw object wrappers -> Build domain objects -> Resolve links -> Validate -> Report
```

Это не противоречит требованию. `Read` — технический этап LibreDWG, `Validate` ловит broken/orphan/cycles, `Report` — потребитель результата. Смысловая загрузка домена остаётся двухфазной.

### 3.3 Порядок Header -> Tables -> Objects -> Entities не отменяет registry

LibreDWG возвращает массив `dwg.object[]`, где порядок не обязан совпадать с удобным прикладным порядком. Поэтому требование про порядок надо трактовать как порядок доменной обработки (после Phase 1, перед resolve):

1. Header.
2. Tables: Layer, Linetype, Style, DimStyle, VPort.
3. Dictionaries and objects: Block, Dictionary, Layout, XRecord.
4. Entities.
5. Resolve all.

При этом registry заполняется для всех объектов до разрешения связей.

### 3.4 Unknown object должен хранить диагностическую информацию

`RawData: Pointer` недостаточно безопасен: память принадлежит `Dwg_Data` и освобождается через `dwg_free`. Для инспектора нужно хранить:

- исходный `DWG_OBJECT_TYPE`;
- `name` / `dxfname`;
- `handle`;
- `supertype`;
- размеры `size`, `bitsize`, `unknown_bits`, `unknown_rest`;
- текстовую причину, почему объект не поддержан;
- **опционально** `RawBytes: TBytes` — копия сырых байт объекта, **только** если включён флаг `--dump-unknown` (см. §12.2). Это нужно для будущего reverse engineering без LibreDWG.

Ключевой принцип: `TDWGUnknownObject` никогда не держит указатель на память LibreDWG после `dwg_free`. Все нужные данные копируются в Phase 1.

### 3.5 Состояния объекта

Каждый доменный объект имеет явное состояние, видимое в отчёте:

```pascal
type
  TDWGObjectStatus = (
    osRaw,        // создан, но Phase 2 ещё не прошла
    osResolved,   // все обязательные ссылки разрешены
    osPartial,    // часть ссылок разрешена, часть — broken
    osBroken,     // обязательная ссылка не разрешена (например, нет layer)
    osFailed      // ошибка маппера, объект — заглушка
  );
```

Состояние помогает в reporter различать "успешно загруженный" объект и "загруженный с предупреждениями", а в `validate` — отлавливать orphan/broken nodes.

---

## 4. Целевая архитектура

```text
             +----------------------+
             |   fpdwginspect CLI   |
             +----------------------+
                       |
             +----------------------+
             |   Reporter / Shell   |
             +----------------------+
                       |
             +----------------------+
             |    Domain Model      |
             +----------------------+
                       |
             +----------------------+
             | Resolver (Phase 2)   |
             +----------------------+
                       |
             +----------------------+
             | Builder (Phase 1)    |
             +----------------------+
                       |
             +----------------------+
             | Reader / LibreDWG    |
             +----------------------+
```

**Запрещённые направления зависимостей:**

- Reporter → Builder
- Resolver → Reader
- Mapper → Registry (в Phase 1)
- Domain Model → CLI

### 4.1 Reader

Ответственность:

- загрузить `libredwg.so` / `libredwg-0.dll`;
- вызвать `dwg_read_file`;
- вернуть `Dwg_Data` и код результата (`TDWGReadResult`);
- освободить память через `dwg_free`;
- не создавать доменные объекты и не выводить отчёт.

Reader не должен знать про слои, линии, блоки и CLI-формат вывода.

### 4.2 Builder

Ответственность:

- пройти по `Dwg_Data.object[]`;
- для каждого `Dwg_Object` создать пустой доменный объект нужного класса через `TDWGObjectFactory`;
- записать `Handle`, `ObjectType`, `Version`, `RawIndex`, `Name`, `DxfName`;
- скопировать scalar-поля (геометрия, имя слоя как handle и т.п.);
- положить объект в `TObjectRegistry`;
- сохранить handle-ссылки как числа, не пытаясь сразу найти целевой объект;
- получить на вход `TFilterStrategy` (см. §11), которая решает: создать полноценный объект, stub-объект или unknown.

### 4.3 Resolver

Ответственность:

- пройти по всем доменным объектам;
- разрешить `LayerHandle -> TDWGLayer`;
- разрешить `LinetypeHandle -> TDWGLinetype`;
- разрешить `OwnerHandle -> TDWGObject`;
- разрешить `BlockRecordHandle`, `PrevEntity`, `NextEntity`, `Dictionary` references;
- зафиксировать warning для битых ссылок;
- проставить `Status = osResolved | osPartial | osBroken`;
- не удалять объект при ошибке разрешения.

### 4.4 Domain Model

Ответственность:

- хранить чистую объектную модель DWG;
- предоставлять API для отчёта и будущего использования в других подсистемах;
- не зависеть от консольного вывода;
- не держать dangling pointers на память LibreDWG после `dwg_free`. Все scalar-поля копируются в Phase 1, никаких lazy-ссылок на сырой буфер LibreDWG.

### 4.5 Reporter / Shell

Ответственность:

- строить текстовый, JSON или Markdown-отчёт по `TDWGDocument` (batch CLI);
- предоставлять интерактивный shell с командами `ls`, `cd`, `show`, `dump`, `tree`, `refs`, `owners`, `find`, `stats`, `validate` (см. §12);
- считать производные метрики: длина LINE (3D и XY), bounding box, количество объектов по типам, список слоёв, список linetypes, broken refs, orphan nodes, циклы;
- не читать LibreDWG напрямую.

### 4.6 CLI

Ответственность:

- разобрать аргументы;
- создать logger;
- вызвать Reader -> Builder -> Resolver -> Reporter;
- при флаге `--shell` запустить интерактивный shell;
- вернуть корректный process exit code.

---

## 5. Предлагаемая структура файлов

Все новые файлы для этой задачи должны находиться в `cad_source/components/fpdwg/`.

```text
cad_source/components/fpdwg/
├── TZ_DWG_CONSOLE_INSPECTOR.md       — это ТЗ
├── fpdwg.lpk                         — пакет, после реализации добавить новые units
├── fpdwg.pas
├── dwg.pp                            — generated LibreDWG binding, не редактировать вручную без отдельной задачи
├── dwgproc.pp                        — low-level LibreDWG loader / parser helpers
│
├── inspector/
│   ├── README.md                     — инструкция: как добавить mapper нового DWG-типа
│   ├── fpdwg_types.pp                — базовые типы: handles, versions, load modes, errors, status
│   ├── fpdwg_logger.pp               — IDWGLogger, console logger, memory logger для тестов
│   ├── fpdwg_reader.pp               — Reader поверх dwgproc.pp
│   ├── fpdwg_registry.pp             — TObjectRegistry (binary search или map)
│   ├── fpdwg_document.pp             — TDWGDocument, ownership model
│   ├── fpdwg_filter.pp               — TFilterStrategy (см. §11)
│   ├── fpdwg_builder.pp              — Phase 1 allocation
│   ├── fpdwg_resolver.pp             — Phase 2 resolve + статусы
│   ├── fpdwg_validator.pp            — broken / orphan / cycles
│   ├── fpdwg_factory.pp              — registry DWG_OBJECT_TYPE -> mapper
│   ├── fpdwg_reporter_text.pp        — text output
│   ├── fpdwg_reporter_json.pp        — json output
│   ├── fpdwg_reporter_md.pp          — markdown (после MVP)
│   ├── fpdwg_shell.pp                — интерактивный shell
│   ├── fpdwg_stats.pp                — counts, geometry metrics
│   ├── fpdwg_libredwg_utils.pp       — safe helpers for BITCODE_H, BITCODE_T, colors, SafeDecodeText
│   │
│   ├── model/
│   │   ├── fpdwg_model_base.pp       — TDWGObject, TDWGEntity, TDWGTableRecord, TDWGSyntheticTable
│   │   ├── fpdwg_model_tables.pp     — Layer, Linetype, Style
│   │   ├── fpdwg_model_blocks.pp     — BlockHeader, Block, Insert
│   │   ├── fpdwg_model_entities.pp   — Line, Circle, Text, UnknownEntity
│   │   └── fpdwg_model_unknown.pp    — TDWGUnknownObject, TDWGDuplicateHandleObject
│   │
│   ├── mappers/
│   │   ├── fpdwg_map_layer.pp
│   │   ├── fpdwg_map_linetype.pp
│   │   ├── fpdwg_map_block.pp
│   │   ├── fpdwg_map_line.pp
│   │   ├── fpdwg_map_circle.pp
│   │   ├── fpdwg_map_text.pp
│   │   └── fpdwg_map_unknown.pp
│   │
│   └── tests/
│       ├── fpdwg_test_handles.pp
│       ├── fpdwg_test_registry.pp
│       ├── fpdwg_test_resolver.pp
│       ├── fpdwg_test_reporter.pp
│       └── fpdwg_test_validator.pp
│
└── fpdwginspect/
    ├── fpdwginspect.lpi
    └── fpdwginspect.lpr
```

Примечание: структура `inspector/` может быть введена поэтапно. Для MVP допустимо начать с меньшего набора файлов, но границы ответственности должны остаться такими же.

---

## 6. Базовые типы и контракты

### 6.1 Handles

```pascal
type
  TDWGHandle = UInt64;

  TDWGHandleSource = (hsNull, hsAbsoluteRef, hsHandleref);

  TDWGHandleRef = record
    Value: TDWGHandle;
    Source: TDWGHandleSource;
    function IsNull: Boolean;
    function ToString: string;
    class function Null: TDWGHandleRef; static;
  end;

function HandleRefFromBitCode(const Ref: BITCODE_H): TDWGHandleRef;
```

Правила:

- `Handle = 0` не считается нормальным handle доменного объекта (исключение — synthetic-объекты, см. §7.7).
- `BITCODE_H = nil` превращается в `TDWGHandleRef.Null` (`Source = hsNull`).
- если `Ref^.absolute_ref <> 0`, берём его (`Source = hsAbsoluteRef`).
- иначе если `Ref^.handleref.value <> 0`, берём его (`Source = hsHandleref`).
- иначе возвращаем `Null`.
- все unresolved references остаются в объекте и попадают в отчёт.

### 6.2 Version

```pascal
type
  TDWGVersion = (
    dvInvalid,
    dvR13,
    dvR14,
    dvR2000,
    dvR2004,
    dvR2007,
    dvR2010,
    dvR2013,
    dvR2018,
    dvAfter
  );
```

Требование: конвертер версии должен принимать `DWG_VERSION_TYPE` из `dwg.pp` и не ломаться при `R_INVALID`. Если `header.version = R_INVALID`, использовать `header.from_version`.

### 6.3 Object type

```pascal
type
  TDWGDomainObjectType = (
    dotHeader,
    dotLayer,
    dotLinetype,
    dotStyle,
    dotBlockHeader,
    dotBlock,
    dotLine,
    dotCircle,
    dotText,
    dotSyntheticTable,
    dotUnknown
  );
```

`TDWGDomainObjectType` не заменяет `DWG_OBJECT_TYPE`. Он нужен для стабильного API приложения. В каждом объекте надо хранить оба значения:

```pascal
RawObjectType: DWG_OBJECT_TYPE;
DomainType: TDWGDomainObjectType;
```

### 6.4 Load modes

```pascal
type
  TDWGLoadMode = (
    lmStrict,    // любая неподдержанная важная структура -> ошибка
    lmTolerant   // warning + unknown object / partial
  );
```

Lazy mode из v1.0 убран сознательно (см. §11). Для частичной материализации используется `TFilterStrategy`.

| Ситуация | lmStrict | lmTolerant |
|---|---|---|
| Unknown entity | error | `TDWGUnknownObject` + warning |
| Unknown non-entity object | error, если critical | `TDWGUnknownObject` |
| Broken handle (required, например owner) | error | warning, ссылка `null`, `Status = osPartial` |
| Broken handle (optional) | warning | warning, `Status = osPartial` |
| Нет layer у entity | error | warning + default layer "0", `Status = osPartial` |
| Невозможно декодировать текст | error | warning + escaped/hex bytes |
| Дубликат handle | exception | `TDWGDuplicateHandleObject` + warning |
| Критическая ошибка LibreDWG | fatal | fatal |

### 6.5 Errors and logger

```pascal
type
  TDWGErrorSeverity = (desInfo, desWarning, desError, desFatal);

  TDWGError = record
    Code: Integer;
    Severity: TDWGErrorSeverity;
    Handle: TDWGHandle;
    ObjectType: DWG_OBJECT_TYPE;
    Message: string;
  end;

  IDWGLogger = interface
    procedure Log(const Error: TDWGError);
    procedure Info(const Msg: string);
    procedure Warning(Code: Integer; const Msg: string; Handle: TDWGHandle = 0);
    procedure Error(Code: Integer; const Msg: string; Handle: TDWGHandle = 0);
  end;
```

Минимальные коды ошибок:

| Code | Severity | Meaning |
|---|---|---|
| 1000 | Fatal | LibreDWG library not loaded |
| 1001 | Fatal | `dwg_read_file` returned critical error |
| 1100 | Warning | Unsupported object type |
| 1101 | Warning | Unknown object mapped to fallback |
| 1200 | Warning | Broken handle reference |
| 1201 | Warning | Duplicate handle in registry |
| 1202 | Warning | Handle resolved via handleref fallback |
| 1300 | Error | Required table missing |
| 1400 | Warning | Text decoding fallback used |
| 1500 | Warning | Orphan object (нет owner и не root) |
| 1501 | Warning | Cycle detected in ownership graph |

---

## 7. Domain Model

### 7.1 Базовый объект

```pascal
type
  TDWGObject = class
  public
    Handle: TDWGHandle;
    OwnerHandle: TDWGHandleRef;
    Owner: TDWGObject;
    RawObjectType: DWG_OBJECT_TYPE;
    DomainType: TDWGDomainObjectType;
    Version: TDWGVersion;
    RawIndex: Integer;
    Name: string;
    DxfName: string;
    Status: TDWGObjectStatus;

    procedure ResolveLinks(Registry: TObjectRegistry; Logger: IDWGLogger); virtual;
  end;
```

Требования:

- каждый объект, кроме synthetic header object и synthetic-таблиц, имеет handle;
- handle уникален внутри `TDWGDocument`;
- duplicate handle обрабатывается по политике из таблицы §6.4;
- объект не должен обращаться к `Dwg_Data` после завершения загрузки.

### 7.2 Entity

```pascal
type
  TDWGEntity = class(TDWGObject)
  public
    LayerHandle: TDWGHandleRef;
    LinetypeHandle: TDWGHandleRef;
    MaterialHandle: TDWGHandleRef;
    PlotStyleHandle: TDWGHandleRef;
    Layer: TDWGLayer;
    Linetype: TDWGLinetype;
    ColorIndex: Integer;
    LineWeight: Integer;
    Visible: Boolean;

    procedure ResolveLinks(Registry: TObjectRegistry; Logger: IDWGLogger); override;
  end;
```

Требование: common entity properties читаются один раз в общем mapper helper, а не копируются в каждом mapper.

### 7.3 Layer

```pascal
type
  TDWGLayer = class(TDWGObject)
  public
    LayerName: string;
    ColorIndex: Integer;
    LineWeight: Integer;
    Off: Boolean;
    Locked: Boolean;
    Plot: Boolean;
    LinetypeHandle: TDWGHandleRef;
    Linetype: TDWGLinetype;
  end;
```

CLI должен уметь вывести список слоёв:

```text
Layer 12 "Walls" color=7 lineweight=25 linetype=Continuous off=false locked=false
```

### 7.4 Linetype

```pascal
type
  TDWGLinetype = class(TDWGObject)
  public
    LinetypeName: string;
    Description: string;
    PatternLength: Double;
  end;
```

Для MVP достаточно имени, описания и pattern length. Dash pattern можно добавить после стабилизации модели.

### 7.5 Line

```pascal
type
  TDWGPoint3D = record
    X, Y, Z: Double;
  end;

  TDWGLine = class(TDWGEntity)
  public
    StartPoint: TDWGPoint3D;
    EndPoint: TDWGPoint3D;
    function Length3D: Double;
    function LengthXY: Double;
  end;
```

Формулы:

```text
Length3D = sqrt((x2-x1)^2 + (y2-y1)^2 + (z2-z1)^2)
LengthXY = sqrt((x2-x1)^2 + (y2-y1)^2)
```

В отчёт выводятся обе длины: `length_3d` и `length_xy`. Это снимает неоднозначность для почти плоских линий с шумом по Z. Координата `Z` должна читаться из `PLine^.start.z` и `PLine^.end_.z`, не из `x`.

### 7.6 Unknown object

```pascal
type
  TDWGUnknownObject = class(TDWGObject)
  public
    Supertype: Dwg_Object_Supertype;
    Size: UInt32;
    BitSize: UInt32;
    UnknownBitsSize: UInt32;
    UnknownRestSize: UInt32;
    Reason: string;
    RawBytes: TBytes;        // заполняется только при --dump-unknown
  end;

  TDWGDuplicateHandleObject = class(TDWGObject)
  public
    OriginalHandle: TDWGHandle;
    ConflictWith: TDWGObject;     // ссылка на ранее зарегистрированный объект
  end;
```

Unknown object является полноценным объектом registry. Он должен участвовать в подсчёте статистики и выводиться в отчёте. `RawBytes` хранится только если включён CLI-флаг `--dump-unknown` — иначе остаётся пустым, чтобы не раздувать память.

### 7.7 Synthetic-объекты для таблиц

В DWG записи таблиц (например, layer records) имеют `OwnerHandle`, указывающий на саму таблицу (`Dwg_Data.Table`). Таблица не является обычным `Dwg_Object` и без специальной обработки resolver на ней споткнётся.

Решение: в Phase 1 builder создаёт synthetic-объекты для таблиц с фиксированными handle:

| Таблица | Synthetic handle |
|---|---|
| Layer table | `0xFFFFFFF1` |
| Linetype table | `0xFFFFFFF2` |
| Style table | `0xFFFFFFF3` |
| DimStyle table | `0xFFFFFFF4` |
| VPort table | `0xFFFFFFF5` |
| BlockRecord table | `0xFFFFFFF6` |

Реализация:

```pascal
type
  TDWGSyntheticTable = class(TDWGObject)
  public
    TableKind: string;   // 'LAYER', 'LTYPE', ...
  end;
```

`OwnerHandle` записи таблицы при resolve мапится на эти synthetic-объекты. Это позволяет не делать исключений в resolver и не считать такие связи broken refs.

---

## 8. Registry

```pascal
type
  TObjectRegistry = class
  private
    FMap: TDictionary<TDWGHandle, TDWGObject>;  // или sorted array + binary search
  public
    procedure Add(Obj: TDWGObject);
    function TryGet(Handle: TDWGHandle; out Obj: TDWGObject): Boolean;
    function Get(Handle: TDWGHandle): TDWGObject;
    function Count: Integer;
    procedure Iterate(Callback: TProc<TDWGObject>);
  end;
```

Для FPC реализация может использовать доступный в проекте контейнер вместо `TDictionary`; интерфейс должен остаться эквивалентным. Поиск по handle — O(1) или O(log N), но не O(N).

Правила:

- `Add` проверяет `Handle <> 0` (исключение — synthetic объекты);
- `Add` при дубликате:
  - в `lmStrict` — выбрасывает исключение;
  - в `lmTolerant` — оставляет первый объект, второй заменяется на `TDWGDuplicateHandleObject` и логируется warning 1201;
- `TryGet` используется в resolver, чтобы broken refs не превращались в exception в tolerant mode;
- registry должен поддерживать перечисление всех объектов для Phase 2.

---

## 9. Factory and Mappers

### 9.1 Factory

```pascal
type
  IDWGObjectMapper = interface
    function CreateObject(const Raw: Dwg_Object; const Ctx: TDWGBuilderContext): TDWGObject;
    procedure FillObject(Obj: TDWGObject; const Raw: Dwg_Object; const Ctx: TDWGBuilderContext);
  end;

  TDWGObjectFactory = class
  public
    procedure RegisterMapper(DWGType: DWG_OBJECT_TYPE; Mapper: IDWGObjectMapper);
    function CreateObject(const Raw: Dwg_Object; const Ctx: TDWGBuilderContext): TDWGObject;
  end;
```

MVP registrations:

```pascal
Factory.RegisterMapper(DWG_TYPE_LAYER, TLayerMapper.Create);
Factory.RegisterMapper(DWG_TYPE_LTYPE, TLinetypeMapper.Create);
Factory.RegisterMapper(DWG_TYPE_BLOCK_HEADER, TBlockHeaderMapper.Create);
Factory.RegisterMapper(DWG_TYPE_LINE, TLineMapper.Create);
```

Если mapper не найден, factory создаёт `TDWGUnknownObject`.

### 9.2 Mapper rules

Каждый mapper:

- читает только поля своего LibreDWG-типа;
- копирует handle-ссылки как `TDWGHandleRef`;
- не вызывает `Registry.Get` в Phase 1;
- не пишет в stdout;
- не освобождает память LibreDWG;
- логирует unsupported fields через `IDWGLogger`;
- при работе со строками использует `SafeDecodeText` (см. §9.3).

### 9.3 SafeDecodeText

LibreDWG возвращает строки в кодировке, указанной в DWG (часто ANSI/CP-1251 или другой codepage). Прямой `string := PAnsiChar` без проверки может выдать мусор или исключение при конверсии в UTF-8.

Контракт хелпера:

```pascal
function SafeDecodeText(P: PAnsiChar; Codepage: Integer; Logger: IDWGLogger): string;
```

Поведение:

- если `P = nil` — вернуть пустую строку;
- если конверсия по `Codepage` успешна — вернуть UTF-8;
- если конверсия падает или содержит invalid sequences — вернуть `'<hex:NN NN NN ...>'`, залогировать warning 1400.

Все mapper'ы используют только `SafeDecodeText`, прямой каст `string(PAnsiChar)` запрещён.

---

## 10. Pipeline загрузки

### 10.1 Полный pipeline

```text
1.  Parse CLI options
2.  Load LibreDWG
3.  Read file into Dwg_Data
4.  Create TDWGDocument (+ synthetic tables)
5.  Read Header
6.  Phase 1: Allocate all objects and fill scalar fields (через Factory + Filter)
7.  Phase 1b: Add all objects to Registry
8.  Phase 2: Resolve handle references, проставить Status
9.  Validate: orphan / cycles / broken refs
10. Generate report (text/json/md) или запустить shell
11. Free Dwg_Data
12. Free Document
13. Return exit code
```

### 10.2 Псевдокод

```pascal
function LoadDWGDocument(const FileName: string;
                         Mode: TDWGLoadMode;
                         Filter: TFilterStrategy;
                         Logger: IDWGLogger): TDWGDocument;
var
  Raw: Dwg_Data;
  Builder: TDWGBuilder;
  Resolver: TDWGResolver;
  Validator: TDWGValidator;
begin
  Raw := Reader.Read(FileName, Logger);
  try
    Result := TDWGDocument.Create;
    Result.RegisterSyntheticTables;  // §7.7
    Result.Header := HeaderMapper.Map(Raw.header, Raw.header_vars);

    Builder := TDWGBuilder.Create(Result.Registry, Factory, Filter, Logger);
    try
      Builder.AllocateObjects(Raw);
    finally
      Builder.Free;
    end;

    Resolver := TDWGResolver.Create(Result.Registry, Logger);
    try
      Resolver.ResolveAll;
    finally
      Resolver.Free;
    end;

    Validator := TDWGValidator.Create(Result, Logger);
    try
      Validator.Validate;
    finally
      Validator.Free;
    end;
  finally
    dwg_free(@Raw);
  end;
end;
```

`dwg_free` вызывается в `finally`, чтобы исключение в Builder/Resolver не приводило к утечке.

---

## 11. Частичная загрузка через фильтрацию (вместо lazy mode)

LibreDWG читает DWG-файл целиком в `Dwg_Data`. Любая попытка lazy-чтения полей из `Dwg_Data` после того, как `Reporter` отработал, упирается в проблему времени жизни: к моменту вызова reporter `Dwg_Data` обычно уже нужно освободить, иначе мы плодим dangling pointers.

**Решение для v1.0:** lazy mode из ТЗ исключён. Вместо него вводится `TFilterStrategy`, которая решает на этапе Phase 1, какой объект создавать:

```pascal
type
  TDWGFilterDecision = (
    fdMaterialize,   // полноценный объект через mapper
    fdStub,          // stub-объект: только handle, owner, тип
    fdSkip           // пропустить (нельзя — нарушит ссылки)
  );

  TFilterStrategy = class
  public
    function Decide(const Raw: Dwg_Object): TDWGFilterDecision; virtual; abstract;
  end;
```

Реализации:

- `TFilterAll` — всё материализуется (по умолчанию);
- `TFilterByDomainType` — список разрешённых `TDWGDomainObjectType`, остальное → stub;
- `TFilterTablesOnly` — для `--summary --layers`;
- `TFilterEntitiesByType` — для `--entities=line,circle`.

`fdSkip` по факту нельзя вернуть для объектов, на которые есть handle-ссылки (иначе сломаем resolver). Поэтому реальные стратегии возвращают только `fdMaterialize` и `fdStub`. `fdSkip` зарезервирован для будущих оптимизаций.

```pascal
type
  TDWGStubObject = class(TDWGObject)
  public
    // не содержит геометрии, только handle, OwnerHandle, RawObjectType, имя
  end;
```

CLI-фильтры:

| Опция | Поведение |
|---|---|
| `--summary` | строит только статистику, header, counts по типам, всё остальное stub |
| `--layers` | материализует layers + linetypes |
| `--entities=line,circle,text` | материализует только выбранные entity types |
| `--object=<handle>` | материализует один объект и его прямые ссылки |
| `--all` | всё материализуется (по умолчанию) |

---

## 12. CLI: команды и формат вывода

### 12.1 Имя приложения

Рабочее имя:

```text
fpdwginspect
```

### 12.2 Batch-режим (Usage)

```text
fpdwginspect <file.dwg> [options]

Output filters:
  --summary                 Print header and object counters
  --objects                 Print object list
  --layers                  Print layer table
  --linetypes               Print linetype table
  --entities=<list>         Print selected entity types: line,circle,text,all
  --object=<hex-handle>     Print one object by handle
  --warnings                Include warnings section
  --stats                   Include counts and geometry metrics

Format and mode:
  --format=text|json|md     Output format, default text
  --mode=strict|tolerant    Default tolerant
  --dump-unknown            Save raw bytes of unknown objects (RawBytes)

Resolution:
  --resolve-depth=<n>       Graph reference depth for object view
  --lib=<path>              Explicit LibreDWG library path

Interactive:
  --shell                   Start interactive REPL after load
  --verbose                 Verbose log
  --help                    Show help and exit
```

### 12.3 Интерактивный shell

Если запущен с `--shell`, после успешной загрузки документа открывается REPL:

```text
Команды навигации:
  ls                        список детей текущего узла
  cd <handle>               перейти к узлу
  cd ..                     к родителю
  pwd                       текущий handle и тип

Просмотр:
  show <handle>             краткая информация (тип, owner, кол-во ссылок, status)
  dump <handle>             полный raw-дамп (поля, hex для unknown)
  tree [<handle>] [-d N]    дерево вниз по ownership, глубина N
  refs <handle>             все ссылки (hard/soft/reactors)
  owners <handle>           цепочка владельцев вверх

Поиск:
  find type=<type>          объекты заданного типа
  find broken               объекты со статусом osBroken / osPartial
  find orphan               объекты без owner и не root
  find dup                  duplicate-handle stubs

Диагностика:
  stats                     счётчики по типам, длины, broken refs
  validate                  отдельный проход validator
  warnings                  список warnings
  modes                     текущие mode/filter
  q | quit                  выход
```

Shell обязателен только для P1-расширения после MVP; в MVP достаточно batch-режима (см. §13).

### 12.4 Exit codes

| Code | Meaning |
|---|---|
| 0 | success |
| 1 | CLI usage error |
| 2 | file not found / cannot read |
| 3 | LibreDWG library load error |
| 4 | LibreDWG parse fatal error |
| 5 | strict mode validation failed |
| 10 | internal inspector error |

### 12.5 Text output example

```text
File: samples/simple.dwg
DWG version: R_2007
Objects: 42
Entities: 12
Layers: 3
Warnings: 1

Layers:
  [10] "0" color=7 linetype=Continuous off=false locked=false
  [12] "Walls" color=1 linetype=Continuous off=false locked=false

Entities:
  LINE handle=2A owner=1F layer=Walls linetype=Continuous status=resolved
    start=(0.000, 0.000, 0.000)
    end=(100.000, 0.000, 0.000)
    length_3d=100.000
    length_xy=100.000

Unknown:
  handle=8F rawType=DWG_TYPE_PROXY_ENTITY dxfname="ACAD_PROXY_ENTITY" reason="mapper not implemented"
```

### 12.6 JSON output requirements

JSON must be stable enough for tests and automation:

```json
{
  "file": "samples/simple.dwg",
  "version": "R_2007",
  "objects": [
    {
      "handle": "2A",
      "rawType": "DWG_TYPE_LINE",
      "domainType": "line",
      "status": "resolved",
      "layer": "Walls",
      "geometry": {
        "start": [0.0, 0.0, 0.0],
        "end": [100.0, 0.0, 0.0],
        "length_3d": 100.0,
        "length_xy": 100.0
      }
    }
  ],
  "warnings": []
}
```

Требование: numbers остаются numbers, handles выводятся hex strings без потери ведущих значимых символов. Отсутствующие ссылки — `null`. Индентация — 2 пробела. JSON должен проходить `jq .`.

---

## 13. Минимальный MVP

MVP считается готовым, если реализованы:

1. `fpdwginspect` открывает DWG через `dwg_read_file`.
2. Header выводит версию, codepage, количество объектов, количество classes.
3. `TObjectRegistry` индексирует все объекты по handle с поиском за O(log N) или быстрее.
4. Phase 1 создаёт доменные объекты для:
   - `DWG_TYPE_LAYER`;
   - `DWG_TYPE_LTYPE`;
   - `DWG_TYPE_BLOCK_HEADER`;
   - `DWG_TYPE_LINE`;
   - synthetic-таблицы;
   - all unsupported -> `TDWGUnknownObject`.
5. Phase 2 разрешает:
   - `Entity.LayerHandle -> TDWGLayer`;
   - `Layer.LinetypeHandle -> TDWGLinetype`;
   - `OwnerHandle -> TDWGObject`, включая synthetic-таблицы;
   - проставляет `Status`.
6. Validator находит broken refs и orphan-объекты (cycles — по возможности).
7. Reporter (batch) выводит:
   - summary;
   - layer list;
   - linetype list;
   - line entities with `length_3d` и `length_xy`;
   - unknown object count and list;
   - warnings.
8. Поддерживаются `--format=text` и `--format=json`.
9. Broken handles не приводят к access violation в `lmTolerant`.
10. Duplicate handle обрабатывается без падения.
11. Текст из DWG проходит через `SafeDecodeText`.
12. Есть unit tests для registry, handle helpers, line length (3D и XY), resolver, validator.
13. Есть интеграционный smoke test на минимальном DWG-файле.

Интерактивный shell, `--dump-unknown`, markdown-отчёт, `find broken/orphan/dup`, lazy-объекты — **вне MVP**.

---

## 14. Расширение после MVP

Приоритеты после MVP:

1. **P1** — интерактивный shell (`--shell`) с `ls`, `cd`, `show`, `dump`, `tree`, `refs`, `owners`, `find`.
2. **P1** — `CIRCLE`, `ARC`, `POINT`.
3. **P1** — `--dump-unknown` + reverse engineering tooling.
4. **P2** — `TEXT`, `MTEXT`.
5. **P2** — `INSERT`, `ATTRIB`, `ATTDEF`, block graph.
6. **P2** — `LWPOLYLINE`, `POLYLINE_2D`, `POLYLINE_3D`.
7. **P3** — `STYLE`, `DIMSTYLE`, `VPORT`, dictionaries.
8. **P3** — `HATCH`, `SPLINE`, `ELLIPSE`.
9. **P4** — `PROXY_ENTITY`, `PROXY_OBJECT` with raw diagnostic payload.
10. **P4** — Markdown report, Graphviz-экспорт графа ссылок.

---

## 15. Тестирование

### 15.1 Unit tests

Обязательные unit tests:

- `TDWGHandleRef` conversion from `BITCODE_H` для всех трёх случаев `Source` (`hsAbsoluteRef`, `hsHandleref`, `hsNull`);
- duplicate handle in `TObjectRegistry` (strict / tolerant);
- broken handle warning in resolver;
- layer resolve for line entity;
- line length in 2D (`LengthXY`) и 3D (`Length3D`);
- unknown object creation for unsupported `DWG_OBJECT_TYPE`;
- synthetic table resolution (record's owner -> synthetic table);
- `SafeDecodeText` на корректных ANSI/CP-1251 строках и на битых байтах (fallback на hex);
- validator: orphan / cycle / broken-list;
- text/json reporter snapshots for fixed in-memory document.

### 15.2 Integration tests

Минимальные DWG fixtures:

| Fixture | Версия | Содержимое | Проверка |
|---|---|---|---|
| `line_one_layer.dwg` | R2000 | one layer, one line | layer resolved, length_3d/xy printed |
| `two_layers_linetypes.dwg` | R2000 | layers + linetypes | table output |
| `layers_r14.dwg` | R14 | 3 слоя | handle resolution, version detect |
| `layers_r2004.dwg` | R2004 | 3 слоя | handle resolution |
| `layers_r2007.dwg` | R2007 | 3 слоя | handle resolution |
| `layers_r2010.dwg` | R2010 | 3 слоя | handle resolution |
| `layers_r2018.dwg` | R2018 | 3 слоя | handle resolution |
| `broken_ref.dwg` | R2000 | entity with broken layer handle | warning, no crash, status=osBroken |
| `unknown_proxy.dwg` | R2007 | proxy object/entity | unknown object report |
| `large_10k.dwg` | R2007 | 10 000 объектов | время загрузки <2 сек, память <2× файла |

`broken_ref.dwg` сложно создать штатными средствами (AutoCAD не сохраняет битые ссылки). Допустимые способы:

- маленькая C-утилита поверх libredwg, которая после чтения подменяет handle на заведомо отсутствующий и сохраняет;
- ручная правка hex-редактором по известному смещению handle;
- DXF с битой ссылкой через `dxf_read_file` как промежуточный smoke-чек (не заменяет настоящий DWG-тест).

Если DWG fixtures сложно поддерживать в Git, допускается использовать DXF как smoke check для Reader/Reporter, но приёмка DWG-инспектора должна включать настоящий `.dwg` хотя бы для R2000 и R2007.

### 15.3 Manual verification

Команды для ручной проверки:

```text
fpdwginspect cad_source/test/spdsconstructionline.dwg --summary --layers --stats
fpdwginspect cad_source/test/spdsconstructionline.dwg --entities=line --format=text
fpdwginspect cad_source/test/spdsconstructionline.dwg --objects --warnings --format=json
fpdwginspect cad_source/test/spdsconstructionline.dwg --shell        # после P1
```

### 15.4 Метрики качества

- покрытие тестами: >70% для Registry, Resolver, HandleRef, Validator;
- время загрузки файла на 10 000 объектов: <2 секунд;
- память: не более 2× размер файла;
- ноль access violation на полном fixture-наборе.

---

## 16. Критерии приёмки

ТЗ считается выполненным для реализации, если:

1. Новый код находится в `cad_source/components/fpdwg/`.
2. `fpdwg.lpk` собирается с новыми units.
3. `fpdwginspect` собирается отдельным console project.
4. Приложение не требует запуска GUI ZCAD.
5. LibreDWG загружается динамически, как сейчас в `dwgproc.pp`.
6. Все объекты получают handle и регистрируются до resolve-фазы.
7. Ссылки не разрешаются во время allocation-фазы.
8. Unknown/unsupported objects не теряются молча.
9. Duplicate handle обрабатывается по выбранной политике.
10. В tolerant mode битые ссылки дают warning, но отчёт строится.
11. Для LINE выводятся start/end, `length_3d` и `length_xy`, layer, linetype, status.
12. Для LAYER выводятся name/color/lineweight/on-off/locked/linetype.
13. Для LTYPE выводятся name/description/pattern summary.
14. Synthetic tables создаются и используются как owner для записей таблиц.
15. JSON output валиден, проходит `jq .` и покрыт snapshot-тестом.
16. `SafeDecodeText` используется для всех строк из LibreDWG.
17. Validator выдаёт списки broken / orphan / cycles.
18. В `inspector/README.md` указано, как добавить mapper нового DWG-типа.

---

## 17. Этапы разработки

### Этап 1. Инфраструктура

- Создать `inspector/fpdwg_types.pp` с `TDWGHandleRef` (Source включён), `TDWGObjectStatus`, `TDWGLoadMode`.
- Создать `inspector/fpdwg_logger.pp`.
- Создать `inspector/fpdwg_libredwg_utils.pp`, включая `SafeDecodeText`.
- Добавить безопасные helpers:
  - `DWGHandleValue(const Handle: Dwg_Handle): TDWGHandle`;
  - `HandleRefFromBitCode(const Ref: BITCODE_H): TDWGHandleRef`;
  - `DWGObjectTypeName(DWG_OBJECT_TYPE): string`;
  - `DWGVersionFromLibre(DWG_VERSION_TYPE): TDWGVersion`.
- Unit-тест: handle resolution для всех трёх `Source`.

### Этап 2. Registry, model, synthetic tables

- Создать `TObjectRegistry` (binary search или dictionary).
- Создать `TDWGDocument` с регистрацией synthetic-таблиц.
- Создать базовые классы model, включая `TDWGStubObject`, `TDWGUnknownObject`, `TDWGDuplicateHandleObject`, `TDWGSyntheticTable`.
- Unit tests для registry (включая дубликаты).

### Этап 3. Reader

- Обернуть `LoadLibreDWG`, `dwg_read_file`, `dwg_free`.
- Добавить `--lib=<path>` для CLI.
- Проверить Windows/Linux names: `libredwg-0.dll`, `libredwg.so`.
- `try/finally` вокруг `dwg_free`.

### Этап 4. Factory, Filter, Mappers

- Создать `TDWGObjectFactory`.
- Создать `TFilterStrategy` и базовые реализации (`TFilterAll`, `TFilterByDomainType`).
- Добавить mappers: layer, linetype, block header, line, unknown.
- Phase 1 не должна резолвить ссылки.

### Этап 5. Resolver и Validator

- Реализовать `ResolveAll` с проставлением `Status`.
- Разрешить layer/ltype/owner (включая synthetic tables).
- Добавить warning по broken refs.
- Реализовать `TDWGValidator` (orphan / cycles / broken list).

### Этап 6. Reporter (batch)

- Text reporter.
- JSON reporter.
- Summary/stats.
- Object detail by handle.
- Snapshot-тесты JSON.

### Этап 7. CLI

- Создать `fpdwginspect/fpdwginspect.lpr`.
- Добавить parse args.
- Подключить load mode, filter, output format.
- Вернуть exit codes.

### Этап 8. Tests and docs

- Unit tests для core.
- Smoke test на DWG (минимум R2000 и R2007).
- Документ `inspector/README.md` с инструкцией добавления mapper.

### Этап 9 (после MVP). Shell, dump-unknown, markdown

- Интерактивный REPL (`--shell`).
- `find broken/orphan/dup`.
- `--dump-unknown` сохраняет `RawBytes` в `TDWGUnknownObject`.
- Markdown reporter, Graphviz export.

---

## 18. Главные архитектурные запреты

- Нельзя считать DWG плоским списком объектов.
- Нельзя резолвить `LayerHandle` во время создания entity.
- Нельзя падать на unknown object в tolerant mode.
- Нельзя молча пропускать unsupported object.
- Нельзя смешивать stdout-reporting с model/builder/resolver.
- Нельзя держать неописанные dangling pointers на LibreDWG memory после `dwg_free`.
- Нельзя добавлять новый DWG-тип правкой большого `case` без регистрации mapper.
- Нельзя кастовать `PAnsiChar` напрямую в `string` — только через `SafeDecodeText`.
- Нельзя возвращать `fdSkip` из фильтра для объектов, на которые есть handle-ссылки.
- Нельзя считать `Dwg_Object_Ref^.absolute_ref = 0` автоматически broken-refs — нужно проверить `handleref.value`.

---

## 19. Риски и митигации

| Риск | Вероятность | Митигация |
|---|---|---|
| LibreDWG падает на специфичном DWG | Средняя | Обёртка `try/except` вокруг `dwg_read_file`, fallback к `dxf_read_file`, явный exit code 4 |
| `BITCODE_H.absolute_ref` всегда 0 на некоторых версиях | Высокая | Поле `Source` в `TDWGHandleRef`, fallback на `handleref.value`, тесты для R14..R2018 |
| Утечка памяти через исключения в Builder/Resolver | Средняя | `try/finally` вокруг `dwg_free`, RAII через `TDWGDocument` ownership |
| Несовместимость типов в h2pas-биндинге | Низкая | Автоматизированная регенерация из свежего `dwg.h`, не редактируем `dwg.pp` вручную |
| Дубликат handle в реальном файле | Очень низкая | `TDWGDuplicateHandleObject`, лог 1201, в strict mode — exception |
| Owner записи таблицы не резолвится в TDWGObject | Средняя | Synthetic-таблицы с зарезервированными handle (§7.7) |
| Codepage текста ломает конверсию в UTF-8 | Высокая | `SafeDecodeText` с fallback на hex, warning 1400 |
| Lazy mode даёт dangling pointers на `Dwg_Data` | Высокая | Отказ от lazy mode в v1, переход к фильтрации (`TFilterStrategy`) |
| `broken_ref.dwg` нельзя сделать AutoCAD'ом | Средняя | Утилита-генератор поверх libredwg или ручная hex-правка |
| Большой DWG (100k+) перегружает память | Средняя | Stub-объекты по фильтру, метрика "память не более 2× файла" |

---

## 20. Документация для разработчика

В `cad_source/components/fpdwg/inspector/README.md` должно быть:

```markdown
## Как добавить новый тип объекта

1. Создать класс-наследник TDWGEntity или TDWGObject в `model/`.
2. Реализовать mapper, наследующий IDWGObjectMapper, в `mappers/`.
3. Зарегистрировать в фабрике: `Factory.RegisterMapper(DWG_TYPE_CIRCLE, TCircleMapper.Create);`
4. Добавить в Reporter метод вывода (text + json).
5. Добавить в CLI фильтр (опционально, через `TFilterByDomainType`).
6. Написать unit-тест на mapper и snapshot json reporter.

Пример: см. `mappers/fpdwg_map_line.pp`.
```

---

## 21. Итог

Целевая система должна быть не "примером чтения LibreDWG", а малым DWG analysis framework внутри `fpdwg`:

```text
LibreDWG raw data
  -> typed domain objects with explicit status
  -> global handle registry (incl. synthetic tables)
  -> deferred reference resolution + validation
  -> robust reports for humans and automation
  -> interactive shell for debug and reverse engineering (post-MVP)
```

Минимальный полезный результат — консольный `fpdwginspect`, который уверенно показывает header, layers, linetypes, lines (3D и XY длина), unknown objects, broken refs и warnings. Эта база затем расширяется новыми mappers, интерактивным shell и `--dump-unknown` без изменения общей архитектуры.

**Ключевые архитектурные компромиссы редакции 2.0:**

1. Отказ от lazy mode в v1 — в пользу фильтрации и предсказуемого времени жизни данных.
2. Synthetic-таблицы вместо специальных исключений в resolver — единый интерфейс registry.
3. `RawBytes` опционально по флагу — диагностика без раздувания памяти.
4. Явный `Status` объекта — отчёт честно показывает, насколько успешно прошёл resolve.
5. `SafeDecodeText` обязателен — codepage не должен валить инспектор.
