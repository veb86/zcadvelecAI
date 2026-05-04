# Техническое задание: корректировка fpdwg вокруг domain model DWG

> Issue: [veb86/zcadvelecAI#1077](https://github.com/veb86/zcadvelecAI/issues/1077)
> Основание: прежнее ТЗ `TZ_DWG_CONSOLE_INSPECTOR.md` было реализовано как "Inspector = инструмент", а требуется трактовка "Inspector = view над domain model".
> Рабочая папка: `cad_source/components/fpdwg/`
> Статус документа: план изменения проекта, без реализации кода в рамках issue #1077.

## 1. Цель корректировки

Нужно перестроить развитие `fpdwg` вокруг самостоятельной DWG domain model, которая не зависит от консольного инспектора, ZCAD UI и времени жизни структур LibreDWG.

Целевая архитектура:

```text
RAW (LibreDWG C structs)
  ↓
Binding / Adapter (fpdwg)
  ↓
Mapper / Builder
  ↓
Domain Model DWG
  ↓
Inspector / UI / ZCAD
```

Два основных потока данных:

```text
Чтение:
libredwg -> fpdwg binding -> Mapper -> Domain Model -> Inspector / ZCAD

Создание и сохранение:
ZCAD / API -> Domain Model -> Builder -> fpdwg binding -> libredwg -> DWG
```

Главный результат корректировки - получить domain model DWG как центральный слой. Консольный вывод нужен только как ранний способ отладки этой модели и не должен определять структуру данных.

## 2. Аудит текущего состояния

### 2.1 Что уже полезно и должно быть сохранено

В `cad_source/components/fpdwg/` уже есть рабочая основа:

| Зона | Текущие файлы | Что сохранить |
|---|---|---|
| RAW / binding | `dwg.pp`, `dwgproc.pp`, `libredwg/dwg.h` | Pascal-биндинг LibreDWG, динамическая загрузка библиотеки, чтение и освобождение `Dwg_Data` |
| Reader | `inspector/fpdwg_reader.pp` | Обвязка lifetime `dwg_read_file` / `dwg_free`, инъекция API для тестов |
| Object mapping | `inspector/fpdwg_factory.pp`, `inspector/mappers/*` | Копирование scalar-полей из LibreDWG, fallback unknown-объектов |
| Object graph | `inspector/fpdwg_registry.pp`, `inspector/fpdwg_resolver.pp`, `inspector/fpdwg_validator.pp` | Registry по handle, deferred resolve, диагностика broken/orphan/cycle |
| Current model | `inspector/model/*` | Начальные классы layers, linetypes, blocks, line/arc/circle/lwpolyline/text |
| Debug view | `inspector/fpdwg_reporter.pp`, `inspector/fpdwg_cli.pp`, `fpdwginspect/` | Консольный отчет, JSON/text, фильтры, `--show` |
| Tests | `inspector/tests/*` | Тестовые паттерны для factory, registry, resolver, reporter, CLI |

Текущее решение уже содержит важные технические идеи из старого ТЗ: двухфазное построение, handle registry, object status, unknown fallback, safe text decode, tolerant/strict режимы.

### 2.2 Основная архитектурная проблема

Сейчас модель и сервисы построения живут внутри каталога `inspector/`:

```text
inspector/model/*
inspector/fpdwg_document.pp
inspector/fpdwg_registry.pp
inspector/fpdwg_resolver.pp
inspector/fpdwg_validator.pp
inspector/fpdwg_factory.pp
inspector/mappers/*
```

Из-за этого создается неверный смысловой центр: кажется, что domain model существует для консольного инспектора. На самом деле инспектор должен быть потребителем domain model.

Практические последствия, если оставить как есть:

- ZCAD-импорт начнет зависеть от `inspector`-модулей.
- API создания DWG будет некуда положить без смешивания с CLI.
- Сохранение через LibreDWG потребует обратного builder-слоя, но текущий pipeline спрятан в `fpdwg_cli.pp`.
- Domain classes напрямую используют `dwg`-типы (`DWG_OBJECT_TYPE`, `DWG_OBJECT_SUPERTYPE`), что протаскивает RAW-слой выше допустимого уровня.
- "Показать в консоли" может начать диктовать поля domain model, хотя должен быть только view-проекцией.

## 3. Архитектурные правила

1. `dwg.pp` и `dwgproc.pp` - это RAW/binding слой. Он знает о LibreDWG и C-структурах.
2. Domain model не зависит от `dwg`, `dwgproc`, `fpdwg_reader`, CLI, reporter и ZCAD.
3. Mapper зависит от binding и domain: `LibreDWG raw -> Domain`.
4. Builder зависит от domain и binding: `Domain -> LibreDWG raw`.
5. Inspector зависит от domain и read pipeline, но не от raw pointers и не от mapper internals.
6. ZCAD integration зависит от domain, но не от inspector.
7. Все данные, которые нужны после `dwg_free`, копируются в domain model или diagnostic snapshot.
8. Handle, owner, references, statuses и validation diagnostics являются частью domain layer, а не inspector layer.
9. Консольный инспектор - это view над domain model: tree/list/detail/raw-diagnostic projections.
10. Запись DWG должна идти через domain model, а не через ручное заполнение LibreDWG структур из UI.

## 4. Целевая структура каталогов

Рекомендуемая структура после миграции:

```text
cad_source/components/fpdwg/
  dwg.pp
  dwgproc.pp
  libredwg/

  binding/
    fpdwg_libredwg_api.pp
    fpdwg_raw_reader.pp
    fpdwg_raw_writer.pp
    fpdwg_raw_utils.pp

  domain/
    fpdwg_domain_types.pp
    fpdwg_domain_document.pp
    fpdwg_domain_registry.pp
    fpdwg_domain_diagnostics.pp
    fpdwg_domain_geometry.pp
    fpdwg_domain_objects.pp
    fpdwg_domain_tables.pp
    fpdwg_domain_blocks.pp
    fpdwg_domain_entities.pp
    fpdwg_domain_unknown.pp
    fpdwg_domain_resolver.pp
    fpdwg_domain_validator.pp
    fpdwg_domain_edit.pp

  mapping/
    fpdwg_read_pipeline.pp
    fpdwg_domain_mapper.pp
    mappers/
      fpdwg_map_layer.pp
      fpdwg_map_linetype.pp
      fpdwg_map_block.pp
      fpdwg_map_line.pp
      fpdwg_map_arc.pp
      fpdwg_map_circle.pp
      fpdwg_map_lwpolyline.pp
      fpdwg_map_text.pp
      fpdwg_map_unknown.pp

  building/
    fpdwg_write_pipeline.pp
    fpdwg_domain_builder.pp
    builders/
      fpdwg_build_header.pp
      fpdwg_build_tables.pp
      fpdwg_build_blocks.pp
      fpdwg_build_entities.pp
      fpdwg_build_handles.pp

  inspector/
    fpdwg_inspector_model.pp
    fpdwg_reporter.pp
    fpdwg_cli.pp
    fpdwg_filter.pp
    README.md

  zcad/
    fpdwg_zcad_importer.pp
    fpdwg_zcad_exporter.pp
```

На первом этапе не обязательно физически переносить все файлы сразу. Допустимы compatibility-units со старыми именами, но новые зависимости должны двигаться к этой структуре.

## 5. Контракты слоев

### 5.1 RAW / LibreDWG

Содержит только то, что приходит из LibreDWG:

- `Dwg_Data`
- `Dwg_Object`
- `Dwg_Object_Ref`
- enum и record-типы из `dwg.pp`
- функции чтения, записи, освобождения, добавления objects/handles, если они доступны в установленной LibreDWG.

RAW-слой не должен знать о domain classes и ZCAD.

### 5.2 Binding / Adapter fpdwg

Задача binding-слоя - дать безопасный Pascal API поверх LibreDWG:

- загрузить библиотеку;
- прочитать файл;
- освободить `Dwg_Data`;
- создать пустой `Dwg_Data` для записи;
- добавить raw object;
- добавить handle reference;
- записать DWG-файл;
- преобразовать raw handle/text/version в простые Pascal-значения;
- скрыть различия платформ `.so` / `.dll`.

Нужно расширить текущий `dwgproc.pp`: сейчас он экспортирует только `dwg_read_file`, `dxf_read_file`, `dwg_free`. Для записи требуется отдельное исследование доступных exports конкретной LibreDWG. В `libredwg/dwg.h` видны закомментированные declarations для `dwg_write_file`, `dwg_add_object`, `dwg_add_handleref`; перед реализацией записи нужно проверить фактическую библиотеку и при необходимости обновить binding.

### 5.3 Domain Model DWG

Domain model хранит логическую структуру чертежа:

- document metadata;
- handle registry;
- tables: layer, linetype, style, dimstyle, vport;
- blocks and block records;
- entities;
- dictionaries and extension data по мере необходимости;
- references as values, not raw pointers;
- validation diagnostics;
- unknown/proxy objects as stable snapshots.

Domain model не использует `DWG_OBJECT_TYPE` как публичный тип. Если нужно сохранить происхождение объекта, вводится нейтральная metadata:

```pascal
type
  TDWGSourceObjectInfo = record
    SourceFormat: string;      // например 'LibreDWG'
    SourceTypeCode: Integer;   // numeric raw type
    SourceTypeName: string;    // text name for diagnostics
    SourceSuperType: string;
    RawIndex: Integer;
  end;
```

Это позволяет inspector показывать raw diagnostics, но не заставляет domain layer зависеть от `dwg.pp`.

### 5.4 Mapper

Mapper строит domain model из raw LibreDWG:

```text
Dwg_Data -> TDWGDocument
Dwg_Object -> TDWGObject descendant
BITCODE_H -> TDWGHandleRef
LibreDWG strings -> UTF-8 strings
```

Mapper обязан:

- копировать scalar fields;
- не хранить raw pointers;
- создавать unknown/proxy domain objects для неподдержанных типов;
- строить registry до resolve;
- выполнять resolve отдельной фазой;
- возвращать domain diagnostics вместо печати в stdout.

### 5.5 Builder / Writer

Builder выполняет обратное преобразование:

```text
TDWGDocument -> Dwg_Data -> dwg_write_file
```

Builder обязан:

- валидировать domain model перед записью;
- назначать handles, если объект создан без handle;
- строить owner/reference links;
- создавать минимальные обязательные tables/block records;
- записывать только поддержанный набор объектов;
- возвращать diagnostics для неподдержанных или неполных объектов;
- освобождать raw `Dwg_Data` через binding layer.

### 5.6 Inspector / UI

Inspector является view над domain model:

- summary;
- object list;
- tree by ownership;
- object detail by handle;
- refs/owners;
- validation warnings;
- optional raw diagnostic view from `TDWGSourceObjectInfo` and unknown snapshots.

Inspector не должен:

- создавать domain objects из `Dwg_Object`;
- вызывать `dwg_free`;
- заполнять LibreDWG structs;
- зависеть от mapper classes напрямую, кроме высокоуровневого read pipeline.

### 5.7 ZCAD integration

ZCAD должен работать с domain model:

```text
TDWGDocument -> ZCAD drawing
ZCAD drawing -> TDWGDocument
```

Это дает один общий источник правды для чтения, инспекции, импорта, создания и сохранения DWG.

## 6. Требования к domain model

### 6.1 Базовые типы

Минимальные value-типы:

- `TDWGHandle`
- `TDWGHandleRef`
- `TDWGObjectId`
- `TDWGPoint2D`
- `TDWGPoint3D`
- `TDWGColor`
- `TDWGLineWeight`
- `TDWGObjectStatus`
- `TDWGDiagnostic`
- `TDWGSourceObjectInfo`

`TDWGHandleRef` должен сохранять источник значения при чтении:

```text
null
absolute_ref
handleref fallback
generated by builder
```

### 6.2 Document

`TDWGDocument` должен быть доменным контейнером, а не inspector document:

- `FileName` optional;
- `Version`;
- `Codepage`;
- `Units`;
- `Header`;
- `Registry`;
- `Tables`;
- `Blocks`;
- `ModelSpace`;
- `PaperSpaces`;
- `Diagnostics`;
- API добавления объектов.

Пример целевого API:

```pascal
Document := TDWGDocument.CreateEmpty;
Layer := Document.Layers.Add('E-WIRE');
Document.ModelSpace.AddLine(Point3D(0, 0, 0), Point3D(100, 0, 0), Layer);
Document.ValidateForWrite;
```

### 6.3 Objects and references

Каждый domain object должен иметь:

- стабильный domain id;
- optional DWG handle;
- owner reference;
- list of outgoing references;
- source metadata;
- status;
- diagnostics.

References должны быть значениями, а не указателями:

```pascal
TDWGObjectRef = record
  TargetId: TDWGObjectId;
  TargetHandle: TDWGHandleRef;
  RefKind: TDWGReferenceKind; // owner, layer, linetype, style, block, reactor
  Required: Boolean;
end;
```

Resolved pointers допустимы как cache после resolve, но canonical state остается в object refs.

### 6.4 Минимальный набор domain objects

MVP для чтения, отладки и первой записи:

- document header;
- layer table;
- linetype table;
- style table stub;
- block record table;
- model space block;
- paper space block stub;
- `LINE`;
- `ARC`;
- `CIRCLE`;
- `LWPOLYLINE`;
- `TEXT`;
- unknown/proxy object.

Для ZCAD integration этот набор достаточен, чтобы проверить базовый контур "создать чертеж -> сохранить DWG -> прочитать обратно -> построить в ZCAD".

## 7. Поток чтения

Целевой read pipeline:

```text
1. fpdwg_raw_reader loads LibreDWG.
2. libredwg reads DWG into Dwg_Data.
3. fpdwg_domain_mapper allocates TDWGDocument.
4. Phase 1: all raw objects become domain objects or stubs.
5. Registry is filled by handle/domain id.
6. Phase 2: domain references are resolved.
7. Validator records broken refs, orphan objects, owner cycles.
8. Raw Dwg_Data is freed.
9. Domain model is passed to Inspector or ZCAD.
```

Important: reporter and ZCAD must be able to use `TDWGDocument` after `Dwg_Data` is freed.

## 8. Поток создания и сохранения

Целевой create/save pipeline:

```text
1. ZCAD or API creates/edits TDWGDocument.
2. Domain validator checks write constraints.
3. Builder allocates empty Dwg_Data.
4. Builder creates mandatory header/tables/blocks.
5. Builder assigns handles and owner links.
6. Builder materializes supported domain objects into raw Dwg_Object records.
7. Builder creates LibreDWG handle refs.
8. fpdwg_raw_writer calls dwg_write_file.
9. Binding frees temporary Dwg_Data.
```

MVP записи должен поддерживать:

- создание пустого чертежа с model space;
- добавление layer;
- добавление linetype by name или fallback `Continuous`;
- добавление `LINE`;
- добавление `CIRCLE`;
- добавление `ARC`;
- сохранение в DWG;
- чтение сохраненного DWG тем же read pipeline;
- сравнение domain snapshot до/после для поддержанных полей.

Если LibreDWG write API на текущей версии недоступен или нестабилен, это фиксируется диагностикой и отдельным technical spike. Но архитектура builder-слоя все равно должна быть заложена сразу.

## 9. План миграции

### Этап 0. Зафиксировать текущее состояние

Цель: не потерять работающий inspector.

Действия:

- оставить текущие tests как safety net;
- зафиксировать список поддержанных raw types и CLI features;
- не добавлять новые object types до разделения слоев.

Acceptance criteria:

- существующие inspector tests проходят;
- описан текущий список возможностей и ограничений.

### Этап 1. Выделить domain units

Цель: вынести domain model из `inspector/model`.

Действия:

- перенести `fpdwg_model_*` в `domain/fpdwg_domain_*`;
- перенести `fpdwg_document`, `fpdwg_registry`, `fpdwg_resolver`, `fpdwg_validator` в domain layer;
- заменить публичные `dwg`-типы в domain API на нейтральные source metadata;
- оставить старые unit names как thin compatibility wrappers, если это нужно для постепенной миграции package.

Acceptance criteria:

- domain units компилируются без `uses dwg, dwgproc`;
- inspector depends on domain, not наоборот;
- tests domain layer не требуют LibreDWG.

### Этап 2. Выделить read mapping

Цель: убрать построение document из `fpdwg_cli.pp`.

Действия:

- создать `mapping/fpdwg_read_pipeline.pp`;
- перенести `TDWGObjectFactory`, mapper interfaces и mappers в mapping layer;
- CLI вызывает только high-level `ReadDWGDocument(FileName, Options)`;
- filter остается input option read pipeline, а не часть domain model.

Acceptance criteria:

- `fpdwg_cli.pp` не содержит цикла по `Raw.object[]`;
- mapper tests остаются отдельными от reporter tests;
- один и тот же read pipeline может использовать inspector и ZCAD.

### Этап 3. Сделать inspector настоящим view

Цель: inspector только отображает domain model.

Действия:

- оставить в `inspector/` reporter, CLI и view helpers;
- добавить `fpdwg_inspector_model.pp`, который строит tree/list/detail projections из `TDWGDocument`;
- убрать из inspector знание о raw mapper classes;
- команды `summary`, `objects`, `show`, `refs`, `owners`, `tree`, `validate` строить из domain services.

Acceptance criteria:

- inspector можно протестировать на вручную созданном `TDWGDocument` без DWG-файла;
- вывод всех объектов доступен через domain registry.

### Этап 4. Добавить domain edit API

Цель: domain model должна позволять создавать чертеж программно.

Действия:

- добавить `TDWGDocument.CreateEmpty`;
- добавить builders для default tables/model space;
- добавить методы `AddLayer`, `AddLinetype`, `AddLine`, `AddCircle`, `AddArc`, `AddText`;
- добавить validation profile `ValidateForWrite`.

Acceptance criteria:

- unit test создает документ полностью без LibreDWG;
- document registry, owner refs и layer refs корректны;
- inspector умеет вывести созданный in-memory document.

### Этап 5. Добавить writer через LibreDWG

Цель: сохранить domain document в DWG.

Действия:

- исследовать фактические LibreDWG exports для записи;
- расширить binding writer API;
- создать `building/fpdwg_write_pipeline.pp`;
- реализовать builder для MVP objects;
- добавить read-after-write тест или manual fixture test, если CI не имеет LibreDWG.

Acceptance criteria:

- можно создать domain document с `LINE`, сохранить DWG и прочитать его обратно;
- read-after-write snapshot сохраняет geometry, layer, linetype и handles для поддержанных объектов;
- ошибки записи возвращаются как diagnostics.

### Этап 6. Подключить ZCAD через domain

Цель: ZCAD не зависит от inspector.

Действия:

- создать `zcad/fpdwg_zcad_importer.pp`: `TDWGDocument -> ZCAD drawing`;
- создать `zcad/fpdwg_zcad_exporter.pp`: `ZCAD drawing -> TDWGDocument`;
- начать с layer + line/circle/arc/text;
- расширять маппинг по мере стабилизации domain model.

Acceptance criteria:

- ZCAD может построить чертеж из `TDWGDocument`;
- ZCAD может создать `TDWGDocument` и передать его writer pipeline;
- inspector остается независимым debug-view для того же document.

## 10. Карта переноса текущих файлов

| Текущий файл | Целевое место | Комментарий |
|---|---|---|
| `inspector/model/fpdwg_model_base.pp` | `domain/fpdwg_domain_objects.pp` / `domain/fpdwg_domain_types.pp` | Разделить базовые типы и object classes |
| `inspector/model/fpdwg_model_entities.pp` | `domain/fpdwg_domain_entities.pp` | Убрать зависимости от RAW-типов |
| `inspector/model/fpdwg_model_tables.pp` | `domain/fpdwg_domain_tables.pp` | Сделать table API пригодным для create/save |
| `inspector/model/fpdwg_model_blocks.pp` | `domain/fpdwg_domain_blocks.pp` | Выделить model space/paper space |
| `inspector/model/fpdwg_model_unknown.pp` | `domain/fpdwg_domain_unknown.pp` | Unknown как stable diagnostic snapshot |
| `inspector/fpdwg_document.pp` | `domain/fpdwg_domain_document.pp` | Центральный domain root |
| `inspector/fpdwg_registry.pp` | `domain/fpdwg_domain_registry.pp` | Registry не принадлежит inspector |
| `inspector/fpdwg_resolver.pp` | `domain/fpdwg_domain_resolver.pp` | Resolve - domain service |
| `inspector/fpdwg_validator.pp` | `domain/fpdwg_domain_validator.pp` | Validation - domain service |
| `inspector/fpdwg_reader.pp` | `binding/fpdwg_raw_reader.pp` | Binding/service вокруг LibreDWG |
| `inspector/fpdwg_factory.pp` | `mapping/fpdwg_domain_mapper.pp` | Factory принадлежит read mapping |
| `inspector/mappers/*` | `mapping/mappers/*` | Raw-to-domain mappers |
| `inspector/fpdwg_reporter.pp` | `inspector/fpdwg_reporter.pp` | Оставить как view |
| `inspector/fpdwg_cli.pp` | `inspector/fpdwg_cli.pp` | Упростить до orchestration/view |
| `inspector/fpdwg_filter.pp` | `mapping/fpdwg_read_filter.pp` или оставить view option | Фильтр влияет на materialization, не на domain |

## 11. Тестирование

Минимальный набор проверок по этапам:

1. Domain unit tests:
   - create empty document;
   - add layer/linetype/entities;
   - registry uniqueness;
   - owner refs;
   - validation for read/write.

2. Mapper tests:
   - synthetic `Dwg_Object` -> domain object;
   - handle source `absolute_ref` / `handleref`;
   - unknown object snapshot;
   - no raw pointer retained.

3. Inspector tests:
   - summary/list/show/tree на in-memory `TDWGDocument`;
   - JSON/text snapshots;
   - warnings output.

4. Writer tests:
   - build minimal domain document;
   - save DWG through LibreDWG when library is available;
   - read saved file back;
   - compare supported domain fields.

5. ZCAD adapter tests:
   - domain `LINE/CIRCLE/ARC/TEXT` -> ZCAD primitives;
   - ZCAD primitives -> domain objects;
   - no dependency on inspector units.

Если CI не содержит LibreDWG, writer/read integration tests должны быть split:

- pure unit tests always run;
- LibreDWG integration tests skip with clear reason;
- manual fixture command documented for local verification.

## 12. Риски и решения

| Риск | Что делать |
|---|---|
| LibreDWG write API может отличаться от header | Сначала technical spike: проверить exports установленной библиотеки и обновить binding минимально |
| Domain model снова начнет зависеть от raw structs | Ввести правило: `domain/*` не использует `dwg` и `dwgproc`; закрепить тестом или review checklist |
| Builder сложнее reader из-за обязательных таблиц DWG | MVP записи ограничить default document + layer + basic entities |
| Потеря unknown/proxy data при read/write | Unknown хранить как diagnostic snapshot; write для unknown сначала запрещать с понятной diagnostic |
| Handle conflicts при создании | Ввести centralized handle allocator в domain/building layer |
| ZCAD и inspector начнут расходиться | Оба потребляют один `TDWGDocument`; не дублировать модели |
| Большие DWG расходуют память | Сначала сохранить stub/materialization strategy, но она должна быть частью read pipeline, а не inspector |
| Текст и codepage ломают round-trip | Все строки проходят через safe decode/encode в binding/mapping/building |

## 13. Definition of Done для корректировки

Проект считается исправленным архитектурно, когда выполнены условия:

- есть самостоятельный `domain` layer;
- `inspector` не владеет domain model и не содержит raw mapping pipeline;
- read flow возвращает `TDWGDocument`;
- inspector может вывести все объекты из `TDWGDocument`;
- ZCAD integration может работать с `TDWGDocument` без inspector;
- есть create/edit API для domain document;
- есть первый writer pipeline через LibreDWG или документированный technical blocker по фактической write API;
- тесты покрывают domain, mapper, inspector view и read/write MVP.

## 14. Ближайшие задачи после этого ТЗ

1. Создать issue/PR на этап 1: физически выделить `domain/` и убрать `dwg` из публичного domain API.
2. Создать issue/PR на этап 2: вынести read pipeline из `fpdwg_cli.pp`.
3. Создать issue/PR на этап 4: добавить in-memory создание `TDWGDocument`.
4. Создать technical spike на LibreDWG write API и список нужных exports.
5. После spike создать issue/PR на write MVP: empty drawing + layer + line + read-back verification.
6. Создать issue/PR на ZCAD adapter MVP через domain model.

Это ТЗ не отменяет старый консольный inspector. Оно меняет его роль: inspector остается отладочным view, а основным продуктом становится reusable DWG domain model.
