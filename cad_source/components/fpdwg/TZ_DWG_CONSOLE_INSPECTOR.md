# Техническое задание: консольный инспектор DWG на базе fpdwg и LibreDWG

> Issue: [veb86/zcadvelecAI#1041](https://github.com/veb86/zcadvelecAI/issues/1041)
> Рабочая папка: `cad_source/components/fpdwg/`
> Цель: описать изменение проекта `fpdwg.lpk` и создание консольного приложения, которое читает DWG через LibreDWG, строит граф объектов по handle-ссылкам и выводит полную информацию об объектах чертежа.

---

## 1. Назначение

Нужно развить пакет `fpdwg` из набора низкоуровневых Pascal-биндингов LibreDWG в отдельный слой анализа DWG-файла и консольный инструмент поверх него.

Целевой результат:

```text
DWG = граф объектов + отложенное разрешение ссылок + частичная загрузка
```

Консольное приложение должно:

- открыть `.dwg` через LibreDWG;
- прочитать header, tables, dictionaries, blocks, entities;
- сохранить каждый объект в доменной модели с собственным handle;
- построить глобальный индекс `Handle -> Object`;
- выполнить вторую фазу разрешения ссылок;
- вывести сведения об объектах: тип, handle, owner, layer, linetype, цвет, геометрию, длины линий, имена слоев, типы линий, блоки, неизвестные объекты, ошибки загрузки;
- не падать на неизвестных или частично неподдержанных DWG-типах.

Это ТЗ не требует сразу реализовывать полный CAD-импорт в ZCAD. Задача инструмента - инспекция DWG и формирование устойчивой архитектурной основы для будущей загрузки.

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

---

## 3. Корректировки к исходному наброску

Исходный набросок верно задает главное: DWG надо читать как граф объектов с отложенным разрешением ссылок. Есть несколько уточнений.

### 3.1 `TDWGHandle = UInt64`

В LibreDWG handle представлен как `BITCODE_HV = UInt64`, а ссылка на объект - как `BITCODE_H = ^Dwg_Object_Ref`. Поэтому в доменной модели нужно различать:

```pascal
type
  TDWGHandle = UInt64;       // абсолютное значение handle
  TDWGHandleRef = record     // ссылка, которая может быть пустой или битой
    Value: TDWGHandle;
    IsNull: Boolean;
  end;
```

`TDWGHandle` должен хранить именно `Dwg_Handle.value` или `Dwg_Object_Ref.absolute_ref`, а не указатель `BITCODE_H`.

### 3.2 Двухфазность обязательна, но фаз больше двух

Внешне архитектура должна оставаться двухфазной:

```text
Phase 1: Allocation
Phase 2: Resolve
```

Практически конвейер лучше разложить так:

```text
Read LibreDWG -> Build raw object wrappers -> Build domain objects -> Resolve links -> Report
```

Это не противоречит требованию. `Read` - технический этап LibreDWG, `Report` - потребитель результата. Смысловая загрузка домена остается двухфазной.

### 3.3 Порядок Header -> Tables -> Objects -> Entities не отменяет registry

LibreDWG уже возвращает массив `dwg.object[]`, где порядок не обязан совпадать с удобным прикладным порядком. Поэтому требование про порядок нужно трактовать как порядок доменной обработки:

1. Header.
2. Tables: Layer, Linetype, Style, DimStyle, VPort.
3. Dictionaries and objects: Block, Dictionary, Layout, XRecord.
4. Entities.
5. Resolve all.

При этом registry заполняется для всех объектов до разрешения связей.

### 3.4 Unknown object должен хранить диагностическую информацию

`RawData: Pointer` недостаточно безопасен: память принадлежит `Dwg_Data` и освобождается через `dwg_free`. Для инспектора лучше хранить:

- исходный `DWG_OBJECT_TYPE`;
- `name` / `dxfname`;
- `handle`;
- `supertype`;
- размеры `size`, `bitsize`, `unknown_bits`, `unknown_rest`;
- текстовую причину, почему объект не поддержан;
- опционально snapshot raw bytes, если он доступен и нужен.

---

## 4. Целевая архитектура

```text
             +----------------------+
             |   fpdwginspect CLI   |
             +----------------------+
                       |
             +----------------------+
             |      Reporter        |
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

### 4.1 Reader

Ответственность:

- загрузить `libredwg.so` / `libredwg-0.dll`;
- вызвать `dwg_read_file`;
- вернуть `Dwg_Data` и код результата;
- освободить память через `dwg_free`;
- не создавать доменные объекты и не выводить отчет.

Reader не должен знать про слои, линии, блоки и CLI-формат вывода.

### 4.2 Builder

Ответственность:

- пройти по `Dwg_Data.object[]`;
- для каждого `Dwg_Object` создать пустой доменный объект нужного класса;
- записать `Handle`, `ObjectType`, `Version`, `RawIndex`, `Name`, `DxfName`;
- положить объект в `TObjectRegistry`;
- сохранить handle-ссылки как числа, не пытаясь сразу найти целевой объект.

### 4.3 Resolver

Ответственность:

- пройти по всем доменным объектам;
- разрешить `LayerHandle -> TDWGLayer`;
- разрешить `LinetypeHandle -> TDWGLinetype`;
- разрешить `OwnerHandle -> TDWGObject`;
- разрешить `BlockRecordHandle`, `PrevEntity`, `NextEntity`, `Dictionary` references;
- зафиксировать warning для битых ссылок;
- не удалять объект при ошибке разрешения.

### 4.4 Domain Model

Ответственность:

- хранить чистую объектную модель DWG;
- предоставлять API для отчета и будущего использования в других подсистемах;
- не зависеть от консольного вывода;
- не держать dangling pointers на память LibreDWG после `dwg_free`, кроме явно описанного read-only режима внутри lifetime `TDWGDocument`.

### 4.5 Reporter

Ответственность:

- строить текстовый, JSON или Markdown-отчет по `TDWGDocument`;
- считать производные метрики: длина LINE, bounding box, количество объектов по типам, список слоев, список linetypes, broken refs;
- не читать LibreDWG напрямую.

### 4.6 CLI

Ответственность:

- разобрать аргументы;
- создать logger;
- вызвать Reader -> Builder -> Resolver -> Reporter;
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
│   ├── fpdwg_types.pp                — базовые типы: handles, versions, load modes, errors
│   ├── fpdwg_logger.pp               — IDWGLogger, console logger, memory logger для тестов
│   ├── fpdwg_reader.pp               — Reader поверх dwgproc.pp
│   ├── fpdwg_registry.pp             — TObjectRegistry
│   ├── fpdwg_document.pp             — TDWGDocument, ownership model
│   ├── fpdwg_builder.pp              — Phase 1 allocation
│   ├── fpdwg_resolver.pp             — Phase 2 resolve
│   ├── fpdwg_factory.pp              — registry DWG_OBJECT_TYPE -> mapper
│   ├── fpdwg_reporter.pp             — text/json/markdown output
│   ├── fpdwg_stats.pp                — counts, geometry metrics
│   ├── fpdwg_libredwg_utils.pp       — safe helpers for BITCODE_H, BITCODE_T, colors
│   │
│   ├── model/
│   │   ├── fpdwg_model_base.pp       — TDWGObject, TDWGEntity, TDWGTableRecord
│   │   ├── fpdwg_model_tables.pp     — Layer, Linetype, Style
│   │   ├── fpdwg_model_blocks.pp     — BlockHeader, Block, Insert
│   │   ├── fpdwg_model_entities.pp   — Line, Circle, Text, UnknownEntity
│   │   └── fpdwg_model_unknown.pp    — TDWGUnknownObject
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
│       ├── fpdwg_test_registry.pp
│       ├── fpdwg_test_resolver.pp
│       └── fpdwg_test_reporter.pp
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

  TDWGHandleRef = record
    Value: TDWGHandle;
    IsNull: Boolean;
    class function Null: TDWGHandleRef; static;
  end;
```

Правила:

- `Handle = 0` не считается нормальным handle доменного объекта.
- `BITCODE_H = nil` превращается в `TDWGHandleRef.Null`.
- `BITCODE_H^.absolute_ref` является предпочтительным значением ссылки.
- если `absolute_ref = 0`, fallback - `BITCODE_H^.handleref.value`.
- все unresolved references остаются в объекте и попадают в отчет.

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
    lmTolerant,  // warning + unknown object
    lmLazy       // читать только запрошенные группы, остальное как stub/unknown
  );
```

Поведение:

| Ситуация | lmStrict | lmTolerant | lmLazy |
|---|---|---|---|
| Unknown entity | error | `TDWGUnknownObject` + warning | lazy stub + warning |
| Unknown non-entity object | error, если critical | `TDWGUnknownObject` | lazy stub |
| Broken handle | error для required refs | warning | warning |
| Нет layer у entity | error | warning + default layer | warning + default layer |
| Невозможно декодировать текст | error | warning + escaped bytes | warning + escaped bytes |
| Критическая ошибка LibreDWG | fatal | fatal | fatal |

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
| 1300 | Error | Required table missing |
| 1400 | Warning | Text decoding fallback used |

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

    procedure ResolveLinks(Registry: TObjectRegistry; Logger: IDWGLogger); virtual;
  end;
```

Требования:

- каждый объект, кроме synthetic header object, имеет handle;
- handle уникален внутри `TDWGDocument`;
- duplicate handle не должен затирать предыдущий объект молча;
- объект не должен обращаться к `Dwg_Data` после завершения загрузки, если данные не скопированы.

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

CLI должен уметь вывести список слоев:

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
    function Length: Double;
  end;
```

Формула длины:

```text
Length = sqrt((x2-x1)^2 + (y2-y1)^2 + (z2-z1)^2)
```

Важно: координата `Z` должна читаться из `PLine^.start.z` и `PLine^.end_.z`, не из `x`.

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
  end;
```

Unknown object является полноценным объектом registry. Он должен участвовать в подсчете статистики и выводиться в отчете.

---

## 8. Registry

```pascal
type
  TObjectRegistry = class
  private
    FMap: TDictionary<TDWGHandle, TDWGObject>;
  public
    procedure Add(Obj: TDWGObject);
    function TryGet(Handle: TDWGHandle; out Obj: TDWGObject): Boolean;
    function Get(Handle: TDWGHandle): TDWGObject;
    function Count: Integer;
  end;
```

Для FPC реализация может использовать доступный в проекте контейнер вместо `TDictionary`; интерфейс должен остаться эквивалентным.

Правила:

- `Add` проверяет `Handle <> 0`;
- `Add` логирует duplicate handle;
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

Если mapper не найден, factory создает `TDWGUnknownObject`.

### 9.2 Mapper rules

Каждый mapper:

- читает только поля своего LibreDWG-типа;
- копирует handle-ссылки как `TDWGHandleRef`;
- не вызывает `Registry.Get` в Phase 1;
- не пишет в stdout;
- не освобождает память LibreDWG;
- логирует unsupported fields через `IDWGLogger`.

---

## 10. Pipeline загрузки

### 10.1 Полный pipeline

```text
1. Parse CLI options
2. Load LibreDWG
3. Read file into Dwg_Data
4. Create TDWGDocument
5. Read Header
6. Phase 1: Allocate all objects and fill scalar fields
7. Phase 1b: Add all objects to Registry
8. Phase 2: Resolve handle references
9. Validate document invariants
10. Generate report
11. Free Dwg_Data
12. Return exit code
```

### 10.2 Псевдокод

```pascal
function LoadDWGDocument(const FileName: string;
                         Mode: TDWGLoadMode;
                         Logger: IDWGLogger): TDWGDocument;
var
  Raw: Dwg_Data;
  Builder: TDWGBuilder;
  Resolver: TDWGResolver;
begin
  Raw := Reader.Read(FileName, Logger);
  try
    Result := TDWGDocument.Create;
    Result.Header := HeaderMapper.Map(Raw.header, Raw.header_vars);

    Builder := TDWGBuilder.Create(Result.Registry, Factory, Logger);
    Builder.AllocateObjects(Raw);

    Resolver := TDWGResolver.Create(Result.Registry, Logger);
    Resolver.ResolveAll;

    Result.Validate(Logger);
  finally
    dwg_free(@Raw);
  end;
end;
```

---

## 11. Частичная загрузка и lazy mode

LibreDWG читает DWG-файл целиком в `Dwg_Data`, поэтому "частичная загрузка" на первом этапе означает прикладную частичную материализацию доменной модели.

CLI-фильтры:

| Опция | Поведение |
|---|---|
| `--summary` | построить только статистику, header, counts по типам |
| `--layers` | материализовать layers + linetypes |
| `--entities=line,circle,text` | материализовать только выбранные entity types |
| `--object=<handle>` | вывести один объект и его ближайшие ссылки |
| `--resolve-depth=<N>` | ограничить глубину вывода графа |

В lazy mode для невыбранных объектов создается легкий `TDWGLazyObject`:

```pascal
type
  TDWGLazyObject = class(TDWGObject)
  public
    Loaded: Boolean;
  end;
```

При обращении к объекту через reporter можно догрузить scalar fields из `Dwg_Data`, пока документ находится в активном lifetime. Если `Dwg_Data` уже освобожден, lazy object должен честно сообщить, что details unavailable.

---

## 12. CLI: команды и формат вывода

### 12.1 Имя приложения

Рабочее имя:

```text
fpdwginspect
```

### 12.2 Usage

```text
fpdwginspect <file.dwg> [options]

Options:
  --summary                 Print header and object counters
  --objects                 Print object list
  --layers                  Print layer table
  --linetypes               Print linetype table
  --entities=<list>         Print selected entity types: line,circle,text,all
  --object=<hex-handle>     Print one object by handle
  --format=text|json|md     Output format, default text
  --mode=strict|tolerant|lazy
  --resolve-depth=<n>       Graph reference depth for object view
  --warnings                Include warnings section
  --stats                   Include counts and geometry metrics
  --lib=<path>              Explicit LibreDWG library path
  --help
```

### 12.3 Exit codes

| Code | Meaning |
|---|---|
| 0 | success |
| 1 | CLI usage error |
| 2 | file not found / cannot read |
| 3 | LibreDWG library load error |
| 4 | LibreDWG parse fatal error |
| 5 | strict mode validation failed |
| 10 | internal inspector error |

### 12.4 Text output example

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
  LINE handle=2A owner=1F layer=Walls linetype=Continuous
    start=(0.000, 0.000, 0.000)
    end=(100.000, 0.000, 0.000)
    length=100.000

Unknown:
  handle=8F rawType=DWG_TYPE_PROXY_ENTITY dxfname="ACAD_PROXY_ENTITY" reason="mapper not implemented"
```

### 12.5 JSON output requirements

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
      "layer": "Walls",
      "geometry": {
        "start": [0.0, 0.0, 0.0],
        "end": [100.0, 0.0, 0.0],
        "length": 100.0
      }
    }
  ],
  "warnings": []
}
```

Требование: numbers остаются numbers, handles выводятся hex strings без потери ведущих значимых символов.

---

## 13. Минимальный MVP

MVP считается готовым, если реализованы:

1. `fpdwginspect` открывает DWG через `dwg_read_file`.
2. Header выводит версию, codepage, количество объектов, количество classes.
3. `TObjectRegistry` индексирует все объекты по handle.
4. Phase 1 создает доменные объекты для:
   - `DWG_TYPE_LAYER`;
   - `DWG_TYPE_LTYPE`;
   - `DWG_TYPE_BLOCK_HEADER`;
   - `DWG_TYPE_LINE`;
   - all unsupported -> `TDWGUnknownObject`.
5. Phase 2 разрешает:
   - `Entity.LayerHandle -> TDWGLayer`;
   - `Layer.LinetypeHandle -> TDWGLinetype`;
   - `OwnerHandle -> TDWGObject`, если owner доступен.
6. Reporter выводит:
   - summary;
   - layer list;
   - linetype list;
   - line entities with length;
   - unknown object count and list;
   - warnings.
7. Поддерживаются `--format=text` и `--format=json`.
8. Broken handles не приводят к access violation в `lmTolerant`.
9. Есть unit tests для registry, handle helpers, line length, resolver.
10. Есть интеграционный smoke test на минимальном DWG-файле.

---

## 14. Расширение после MVP

Приоритеты после MVP:

1. `CIRCLE`, `ARC`, `POINT`.
2. `TEXT`, `MTEXT`.
3. `INSERT`, `ATTRIB`, `ATTDEF`, block graph.
4. `LWPOLYLINE`, `POLYLINE_2D`, `POLYLINE_3D`.
5. `STYLE`, `DIMSTYLE`, `VPORT`, dictionaries.
6. `HATCH`, `SPLINE`, `ELLIPSE`.
7. `PROXY_ENTITY`, `PROXY_OBJECT` with raw diagnostic payload.
8. Markdown report and graph export.

---

## 15. Тестирование

### 15.1 Unit tests

Обязательные unit tests:

- `TDWGHandleRef` conversion from `BITCODE_H`;
- duplicate handle in `TObjectRegistry`;
- broken handle warning in resolver;
- layer resolve for line entity;
- line length in 2D and 3D;
- unknown object creation for unsupported `DWG_OBJECT_TYPE`;
- text/json reporter snapshots for fixed in-memory document.

### 15.2 Integration tests

Минимальные DWG fixtures:

| Fixture | Содержимое | Проверка |
|---|---|---|
| `line_one_layer.dwg` | one layer, one line | layer resolved, length printed |
| `two_layers_linetypes.dwg` | layers + linetypes | table output |
| `broken_ref.dwg` | entity with broken layer handle, если возможно подготовить | warning, no crash |
| `unknown_proxy.dwg` | proxy object/entity | unknown object report |

Если DWG fixtures сложно поддерживать в Git, допускается временно использовать DXF через `dxf_read_file` как smoke check только для Reader/Reporter, но приемка DWG-инспектора должна включать настоящий `.dwg`.

### 15.3 Manual verification

Команды для ручной проверки:

```text
fpdwginspect cad_source/test/spdsconstructionline.dwg --summary --layers --stats
fpdwginspect cad_source/test/spdsconstructionline.dwg --entities=line --format=text
fpdwginspect cad_source/test/spdsconstructionline.dwg --objects --warnings --format=json
```

---

## 16. Критерии приемки

ТЗ считается выполненным для реализации, если:

1. Новый код находится в `cad_source/components/fpdwg/`.
2. `fpdwg.lpk` собирается с новыми units.
3. `fpdwginspect` собирается отдельным console project.
4. Приложение не требует запуска GUI ZCAD.
5. LibreDWG загружается динамически, как сейчас в `dwgproc.pp`.
6. Все объекты получают handle и регистрируются до resolve-фазы.
7. Ссылки не разрешаются во время allocation-фазы.
8. Unknown/unsupported objects не теряются молча.
9. В tolerant mode битые ссылки дают warning, но отчет строится.
10. Для LINE выводятся start/end/length/layer/linetype.
11. Для LAYER выводятся name/color/lineweight/on-off/locked/linetype.
12. Для LTYPE выводятся name/description/pattern summary.
13. JSON output валиден и покрыт snapshot-тестом.
14. В README или docs указано, как добавить mapper нового DWG-типа.

---

## 17. Этапы разработки

### Этап 1. Инфраструктура

- Создать `inspector/fpdwg_types.pp`.
- Создать `inspector/fpdwg_logger.pp`.
- Создать `inspector/fpdwg_libredwg_utils.pp`.
- Добавить безопасные helpers:
  - `DWGHandleValue(const Handle: Dwg_Handle): TDWGHandle`;
  - `DWGRefValue(const Ref: BITCODE_H): TDWGHandleRef`;
  - `DWGObjectTypeName(DWG_OBJECT_TYPE): string`;
  - `DWGVersionFromLibre(DWG_VERSION_TYPE): TDWGVersion`.

### Этап 2. Registry and model

- Создать `TObjectRegistry`.
- Создать `TDWGDocument`.
- Создать базовые классы model.
- Написать unit tests для registry.

### Этап 3. Reader

- Обернуть `LoadLibreDWG`, `dwg_read_file`, `dwg_free`.
- Добавить явный `--lib=<path>` для CLI.
- Проверить Windows/Linux names: `libredwg-0.dll`, `libredwg.so`.

### Этап 4. Factory and mappers

- Создать `TDWGObjectFactory`.
- Добавить mappers: layer, linetype, block header, line, unknown.
- Phase 1 не должна резолвить ссылки.

### Этап 5. Resolver

- Реализовать `ResolveAll`.
- Разрешить layer/ltype/owner.
- Добавить warning по broken refs.

### Этап 6. Reporter

- Text reporter.
- JSON reporter.
- Summary/stats.
- Object detail by handle.

### Этап 7. CLI

- Создать `fpdwginspect/fpdwginspect.lpr`.
- Добавить parse args.
- Подключить load mode and output format.
- Вернуть exit codes.

### Этап 8. Tests and docs

- Unit tests для core.
- Smoke test на DWG.
- Документ `inspector/README.md` с инструкцией добавления mapper.

---

## 18. Главные архитектурные запреты

- Нельзя считать DWG плоским списком объектов.
- Нельзя резолвить `LayerHandle` во время создания entity.
- Нельзя падать на unknown object в tolerant mode.
- Нельзя молча пропускать unsupported object.
- Нельзя смешивать stdout-reporting с model/builder/resolver.
- Нельзя держать неописанные dangling pointers на LibreDWG memory после `dwg_free`.
- Нельзя добавлять новый DWG-тип правкой большого `case` без регистрации mapper.

---

## 19. Итог

Целевая система должна быть не "примером чтения LibreDWG", а малым DWG analysis framework внутри `fpdwg`:

```text
LibreDWG raw data
  -> typed domain objects
  -> global handle registry
  -> deferred reference resolution
  -> robust reports for humans and automation
```

Минимальный полезный результат - консольный `fpdwginspect`, который уверенно показывает header, layers, linetypes, lines with length, unknown objects and warnings. Эта база затем расширяется новыми mappers без изменения общей архитектуры.
