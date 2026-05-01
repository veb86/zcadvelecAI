## 🔴 Критические риски реализации

### 1. **Двойная модель handle: `BITCODE_H` vs `Dwg_Object_Ref` — источник трудноуловимых багов**
В ТЗ сказано:  
> `BITCODE_H = nil` превращается в `TDWGHandleRef.Null`.  
> `BITCODE_H^.absolute_ref` является предпочтительным значением ссылки.  
> если `absolute_ref = 0`, fallback — `BITCODE_H^.handleref.value`.

**Проблема:**  
В реальных DWG-файлах `absolute_ref` может быть **0 даже для валидной ссылки** — это зависит от версии DWG и того, как LibreDWG её обработал. Опора на `autodesk`-специфичные поля может породить ложные `broken ref`.

**Что сделать заранее:**
- Добавить **диагностический режим**, который для каждого `BITCODE_H` выводит и `absolute_ref`, и `handleref.value`, и `nil`-статус.
- В `TDWGHandleRef` добавить поле `Source: (hsAbsolute, hsHandleref, hsNull)` — чтобы в отчёте видно было, откуда взялся handle.
- На тестовых DWG разных версий (R14, 2000, 2004, 2007, 2010) сравнить, когда какое поле заполнено.

### 2. **Время жизни `Dwg_Data` и lazy-объекты — риск дампинга памяти**
ТЗ требует:  
> В lazy mode … при обращении к объекту через reporter можно догрузить scalar fields из `Dwg_Data`, пока документ находится в активном lifetime. Если `Dwg_Data` уже освобожден, lazy object должен честно сообщить, что details unavailable.

**Проблема:**  
Это создаёт **сильную связь между доменной моделью и сырым буфером LibreDWG**. Reporter запускается **после** вызова `dwg_free` — значит, lazy-поля будут всегда недоступны. Фактически lazy mode бесполезен.

**Что сделать заранее:**
- Упростить: запретить lazy mode для первого релиза. Частичная загрузка → только фильтрация по типам объектов при **создании доменных объектов**.
- Если lazy очень нужен — хранить внутри `TDWGLazyObject` **снэпшот полей** (копию scalar-полей), а не указатель на `Dwg_Data`.

### 3. **Неизвестные типы: хранение `RawData: Pointer` запрещено, но что тогда хранить?**
ТЗ правильно запрещает `RawData: Pointer`, но предлагает хранить размеры `size`, `bitsize`, `unknown_bits`, `unknown_rest`.  
**Проблема:**  
Эти поля есть в `Dwg_Object`, но для выявления **структуры неизвестного объекта** их недостаточно. Для будущего reverse engineering или fallback-рендеринга нужны **сами байты**.

**Что сделать заранее:**
- Добавить в `TDWGUnknownObject` поле `RawBytes: TBytes` (опционально, по запросу через `--dump-unknown`).
- Это позволит потом анализировать неизвестные объекты вне LibreDWG.

### 4. **LoD-фильтры CLI (`--entities=line,circle`) конфликтуют с фабрикой объектов**
В ТЗ:  
> В lazy mode для невыбранных объектов создается легкий `TDWGLazyObject`.

**Проблема:**  
Фабрика создаёт объекты на основе `DWG_OBJECT_TYPE`. Если фильтр CLI исключает тип — фабрика должна создать `TDWGLazyObject`, но при этом **нужно сохранить handle и все ссылки** (чтобы resolver не упал). Это ломает принцип «один тип → один маппер».

**Что сделать заранее:**
- Ввести `TFilterStrategy`, который решает: полный объект, lazy-объект или игнорировать (но игнорировать нельзя из-за ссылок).
- Фабрика должна получать этот фильтр на вход.

## 🟠 Организационные слабые места

### 5. **Тестовые DWG-файлы с битыми ссылками почти невозможно создать штатными средствами**
ТЗ требует `broken_ref.dwg`: entity with broken layer handle.  
**Реальность:**  
AutoCAD и большинство редакторов не позволяют сохранить чертёж с невалидной ссылкой. Такой файл придётся **ручно править hex-редактором** или генерировать через libredwg с подменой handle.

**Что сделать заранее:**
- Написать маленькую утилиту на основе `libredwg` (на C), которая меняет handle в существующем DWG на заведомо отсутствующий.
- Или использовать DXF как промежуточный формат для генерации битых ссылок (но это не полноценный DWG-тест).

### 6. **Дубликаты handle — редкая, но убийственная ситуация**
ТЗ требует логировать duplicate handle.  
**Проблема:**  
В корректном DWG handle уникальны. Если дубликат появился — это либо ошибка LibreDWG, либо повреждённый файл. Но если затереть объект молча — сломаются ссылки. Если не затереть — нарушится контракт `Registry`.

**Что сделать заранее:**
- В `TObjectRegistry.Add` при конфликте выбрасывать исключение в strict mode.
- В tolerant mode — создавать объект-заглушку `TDWGDuplicateHandleObject`, который не участвует в resolve, но помечает второй объект как ошибочный.
- В отчёт выводить оба handle с указанием конфликта.

## 🟡 Технические детали, которые легко упустить

### 7. **Обработка `ownerhandle` для объектов вне контейнера (например, у таблиц)**
ТЗ говорит: разрешить `OwnerHandle -> TDWGObject`.  
**Проблема:**  
У некоторых объектов (например, у записей таблиц слоёв) `ownerhandle` может указывать на **саму таблицу**, которая не является `TDWGObject`, а является частью `Dwg_Data.Table`. Надо решить: сделать таблицу отдельным объектом registry или игнорировать такие owner-ссылки.

**Что сделать заранее:**
- Создать synthetic-объекты для `LayerTable`, `LinetypeTable` и т.д. с фиктивными handle (например, `0xFFFFFFFF`), чтобы resolve не падал.

### 8. **Геометрия LINE: длина в 3D, но вывод — в плоскости?**
ТЗ требует:  
> Length = sqrt((x2-x1)^2 + (y2-y1)^2 + (z2-z1)^2)

**Проблема:**  
Пользователь, увидев длину, ожидает **проекцию на плоскость XY**, если Z разные, но незначительные. Вычисление 3D-длины для почти плоской линии введёт в заблуждение.

**Что сделать заранее:**
- Выводить обе длины: `length_3d` и `length_xy`.
- В JSON добавлять поле `length_xy`.

### 9. **Codepage и текст — самый частый источник крашей**
LibreDWG возвращает текст в кодировке, указанной в DWG (чаще всего ANSI). Перекодировка в UTF-8 без проверки может выбросить исключение.

**Что сделать заранее:**
- В `fpdwg_libredwg_utils` добавить функцию `SafeDecodeText(PAnsiChar, Codepage): string`, которая ловит ошибки преобразования и возвращает hex-дамп при падении.

## ✅ Резюме: что обязательно проверить до начала разработки

| Зона риска | Действие до старта |
|------------|---------------------|
| Handle resolution | Создать таблицу тестов: DWG R14,2000,2004,2007,2010,2018 — и проверить поля `absolute_ref` и `handleref` |
| Lazy mode | Упростить до фильтрации, убрать зависимость от времени жизни `Dwg_Data` |
| Неизвестные объекты | Добавить опциональное сохранение сырых байт |
| Битая ссылка в DWG | Написать генератор повреждённого DWG (C-утилита) |
| Дубликат handle | Реализовать стратегию конфликта, а не просто лог |
| Owner таблиц | Создать synthetic объекты для таблиц |
| LINE длина | Выводить 3D и XY-длину |
| Текст | Написать защищённый декодер с fallback на hex |

**Самое главное:**  
ТЗ написано очень хорошо — но оно **оптимистично относительно границ ответственности модулей**. На практике именно эти границы размываются (особенно между Builder, Resolver и временем жизни `Dwg_Data`). **Начните с макета загрузки + registry + двухфазного resolve на 2-3 типах, и только потом расширяйте.** Иначе рискуете получить монолит, который нельзя отладить.

# Пердложение по изменению технического задания

# Техническое задание (ред. 2.0, исполнимая)
## Консольный инспектор DWG на базе fpdwg и LibreDWG

> **Статус:** Актуальная редакция для реализации  
> **Рабочая папка:** `cad_source/components/fpdwg/`  
> **Issue:** [veb86/zcadvelecAI#1041](https://github.com/veb86/zcadvelecAI/issues/1041)

---

## 1. Цель и ограничения

### 1.1 Что делаем
Развиваем пакет `fpdwg.lpk` из набора Pascal-биндингов LibreDWG в промышленный анализатор DWG-файлов с консольным интерфейсом `fpdwginspect`.

### 1.2 Что НЕ делаем (границы)
- **Не** реализуем полноценный CAD-импорт в ZCAD
- **Не** поддерживаем запись или модификацию DWG
- **Не** делаем GUI
- **Не** реализуем все 70+ типов объектов DWG в первой версии
- **Не** поддерживаем частичную загрузку в смысле `mmap`/lazy reading — только фильтрацию объектов

### 1.3 Ключевое архитектурное решение
**Отказ от lazy mode в первой версии.**  
Причина: в реальных условиях `Dwg_Data` освобождается до вызова Reporter, что делает lazy-объекты бесполезными. Вместо этого:

- **Фильтрация по типам** — создаём только запрошенные доменные объекты
- **Stub-объекты** для всех остальных (хранят handle и ссылки, но не геометрию)
- **Полная загрузка** всей сырой структуры LibreDWG, но материализация доменной модели — частичная

---

## 2. Текущее состояние (честный аудит)

### 2.1 Что уже есть и работает
| Файл | Состояние | Пригодность |
|------|-----------|--------------|
| `dwg.pp` | Полный h2pas-биндинг | ✅ Можно использовать |
| `dwgproc.pp` | Динамическая загрузка `.so`/`.dll` | ✅ Работает |
| `Dwg_Object` | Содержит handle, fixedtype, unknown_* поля | ✅ Данные есть |
| `GDWGParser` | Базовый парсер с регистрацией обработчиков | ⚠️ Требует обёртки |

### 2.2 Чего НЕТ и нужно создать
- [ ] Реестр объектов `Handle -> Object`
- [ ] Двухфазный резолвер ссылок
- [ ] Доменная модель (не надо тянуть ZCAD-сущности)
- [ ] Консольный парсер аргументов
- [ ] Reporter (text/JSON)
- [ ] Обработка ошибок и unknown-объектов

### 2.3 Критические риски текущей архитектуры
1. **Порядок объектов** в `dwg.object[]` не гарантирует, что родитель будет перед потомком → forward references неизбежны
2. **Утечки памяти** при исключениях между `dwg_read_file` и `dwg_free`
3. **Нет изоляции** между сырыми данными LibreDWG и доменной моделью

---

## 3. Архитектура (железобетонная)

### 3.1 Схема слоёв (строгая зависимость только сверху вниз)

```
CLI (fpdwginspect.lpr)
  ↓ зависит от
Reporter (text/json)
  ↓ зависит от
Resolver (Phase 2)
  ↓ зависит от
Document + Registry
  ↓ зависит от
Builder (Phase 1) ← Factory + Mappers
  ↓ зависит от
Reader (обёртка над dwgproc)
  ↓ зависит от
LibreDWG (.so/.dll)
```

**Запрещено:**
- Reporter → Builder
- Resolver → Reader
- Mapper → Registry (в Phase 1)

### 3.2 Время жизни данных (критично)

```pascal
type
  TDWGLifetime = record
    Raw: Dwg_Data;           // выделено LibreDWG
    Document: TDWGDocument;  // доменная модель
  end;

// Правильный порядок:
// 1. Reader.Read → Raw
// 2. Builder.Build(Raw) → Document
// 3. Resolver.Resolve(Document)
// 4. Reporter.Report(Document)
// 5. dwg_free(@Raw)        // ← после отчёта
// 6. Free Document
```

**Исключение:** Если Document хранит указатели на `Raw` — это баг. Все нужные данные копируются в Phase 1.

---

## 4. Модели данных (жесткая спецификация)

### 4.1 Handle (никакой магии)

```pascal
type
  TDWGHandle = UInt64;  // абсолютное значение handle
  
  TDWGHandleSource = (hsNull, hsHandleref, hsAbsoluteRef);
  
  TDWGHandleRef = record
    Value: TDWGHandle;
    Source: TDWGHandleSource;
    function IsNull: Boolean;
    function ToString: string;  // hex без '0x', например "2A"
  end;

// Конвертер из BITCODE_H (libredwg)
function HandleRefFromBitCode(const Ref: BITCODE_H): TDWGHandleRef;
begin
  if Ref = nil then
    Exit(TDWGHandleRef.CreateNull);
    
  if Ref^.absolute_ref <> 0 then
    Result := TDWGHandleRef.Create(Ref^.absolute_ref, hsAbsoluteRef)
  else if Ref^.handleref.value <> 0 then
    Result := TDWGHandleRef.Create(Ref^.handleref.value, hsHandleref)
  else
    Result := TDWGHandleRef.CreateNull;
end;
```

**Правило:** Если оба поля нулевые — ссылка битая, но мы всё равно создаём `TDWGHandleRef` с `IsNull=True` и логгируем warning.

### 4.2 Иерархия объектов (минимализм)

```pascal
TDWGObject = class abstract
  Handle: TDWGHandle;
  OwnerHandle: TDWGHandleRef;
  Owner: TDWGObject;          // resolved
  RawType: Integer;           // DWG_OBJECT_TYPE из dwg.pp
  DomainType: TDWGDomainType;
  RawIndex: Integer;          // индекс в dwg.object[]
public
  procedure ResolveReferences(Registry: TObjectRegistry; Logger: IDWGLogger); virtual;
end;

TDWGEntity = class abstract(TDWGObject)
  LayerHandle: TDWGHandleRef;
  LinetypeHandle: TDWGHandleRef;
  Layer: TDWGLayer;           // resolved
  Linetype: TDWGLinetype;     // resolved
  ColorIndex: SmallInt;
  Visible: Boolean;
end;

TDWGLayer = class(TDWGObject)
  Name: string;
  ColorIndex: SmallInt;
  IsOff: Boolean;
  IsLocked: Boolean;
end;

TDWGLinetype = class(TDWGObject)
  Name: string;
  Description: string;
end;

TDWGLine = class(TDWGEntity)
  StartPoint: TPoint3D;
  EndPoint: TPoint3D;
  function Length3D: Double;
  function LengthXY: Double;
end;

TDWGUnknownObject = class(TDWGObject)
  Reason: string;
  RawBytes: TBytes;           // опционально, только по запросу
end;
```

### 4.3 Реестр (простой и быстрый)

```pascal
TObjectRegistry = class
private
  FMap: array of record
    Handle: TDWGHandle;
    Obj: TDWGObject;
  end;  // sorted array, binary search
  FLogger: IDWGLogger;
public
  procedure Add(Obj: TDWGObject);
  function TryGet(Handle: TDWGHandle; out Obj: TDWGObject): Boolean;
  function Get(Handle: TDWGHandle): TDWGObject;  // exception если нет
  procedure Iterate(Callback: TProc<TDWGObject>);
end;

// При дубликате handle в TolerantMode — лог + не добавляем второй объект
// В StrictMode — исключение
```

---

## 5. Reader (простой и тупой — это хорошо)

```pascal
TDWGReader = class
public
  class function LoadLibrary(const Path: string): Boolean;  // явный путь или поиск
  class function ReadFile(const Filename: string; out Data: Dwg_Data; Logger: IDWGLogger): TDWGReadResult;
end;

TDWGReadResult = (
  rrSuccess,
  rrLibNotFound,
  rrFileNotFound,
  rrParseError,
  rrUnsupportedVersion
);
```

**Никакой логики, только вызовы:** `LoadLib + dwg_read_file + проверка кода возврата`.

---

## 6. Builder + Factory + Mappers (Phase 1)

### 6.1 Контракт маппера

```pascal
IDWGObjectMapper = interface
  // Создаёт объект нужного класса, заполняет handle, rawtype, ownerhandle
  function CreateObject(const Raw: Dwg_Object; RawIndex: Integer): TDWGObject;
  
  // Заполняет специфичные поля (геометрия, имя слоя как handle и т.п.)
  // ВАЖНО: не вызывает Registry.Get — только handle-ссылки как числа
  procedure FillObject(Obj: TDWGObject; const Raw: Dwg_Object);
end;
```

### 6.2 Фабрика с fallback

```pascal
TDWGObjectFactory = class
private
  FRegistrations: array[DWG_OBJECT_TYPE] of IDWGObjectMapper;  // sparse
public
  procedure Register(RawType: Integer; Mapper: IDWGObjectMapper);
  function CreateAndFill(const Raw: Dwg_Object; RawIndex: Integer): TDWGObject;
end;

// Если маппера нет → TDWGUnknownObject с Reason = "mapper not registered"
```

### 6.3 Builder (один проход по массиву)

```pascal
TDWGBuilder = class
public
  procedure Build(const Raw: Dwg_Data; Document: TDWGDocument; Factory: TDWGObjectFactory);
end;

// Реализация — один цикл for i := 0 to Raw.num_objects - 1:
//   Obj := Factory.CreateAndFill(Raw.object[i], i)
//   Document.Registry.Add(Obj)
//   if Obj is TDWGEntity then Document.Entities.Add(Obj)
//   и т.п.
```

**Никакой магии, только создание и добавление в реестр.**

---

## 7. Resolver (Phase 2)

### 7.1 Алгоритм (простой и надёжный)

```pascal
TDWGResolver = class
public
  procedure Resolve(Document: TDWGDocument; Logger: IDWGLogger);
end;

// Реализация:
// 1. Document.Registry.Iterate(ResolveOwner)
// 2. Document.Layers.Iterate(ResolveLinetype)
// 3. Document.Entities.Iterate(ResolveLayerAndLinetype)
// 4. Document.Entities.Iterate(ResolveSpecificLinks) — для LINE, CIRCLE и т.п.
```

### 7.2 Хелпер для безопасного разрешения

```pascal
function SafeResolve<T>(Registry: TObjectRegistry; HandleRef: TDWGHandleRef; 
                        out Obj: T; Logger: IDWGLogger): Boolean;
begin
  Result := False;
  if HandleRef.IsNull then Exit;
  if not Registry.TryGet(HandleRef.Value, {%H-}Pointer(Obj)) then
  begin
    Logger.LogWarning('Broken handle', HandleRef.Value, 'reference not found');
    Exit;
  end;
  Result := True;
end;
```

---

## 8. Reporter (text + json)

### 8.1 Интерфейс

```pascal
IDWGReporter = interface
  procedure ReportHeader(const Doc: TDWGDocument);
  procedure ReportLayers(const Doc: TDWGDocument);
  procedure ReportLinetypes(const Doc: TDWGDocument);
  procedure ReportEntities(const Doc: TDWGDocument; Filter: TEntityFilter);
  procedure ReportUnknown(const Doc: TDWGDocument);
  procedure ReportWarnings(const Doc: TDWGDocument);
  procedure WriteTo(const Stream: TStream);
end;

TEntityFilter = set of TDWGDomainType;
```

### 8.2 Требования к JSON

- Поля `handle` — как hex-строки (без префикса `0x`)
- Числа — как числа (не строки)
- Отсутствующие ссылки — как `null`
- Индиентация — 2 пробела

---

## 9. CLI (fpdwginspect)

### 9.1 Синтаксис (железный)

```bash
fpdwginspect <dwg-file> [options]

Options:
  --summary                 # Только header + статистика
  --layers                  # Таблица слоёв
  --linetypes               # Таблица типов линий
  --entities=line,circle    # Фильтр по типам (line,circle,text,lwpolyline,all)
  --object=<handle>         # Один объект (hex)
  --format=text|json        # По умолчанию text
  --mode=strict|tolerant    # По умолчанию tolerant
  --warnings                # Показать предупреждения
  --stats                   # Длины, bounding box и т.п.
  --lib=<path>              # Путь к libredwg.so/dll
  --verbose                 # Подробный лог процесса
```

### 9.2 Exit codes

| Code | Meaning |
|------|---------|
| 0 | Success |
| 1 | Ошибка CLI (неверный аргумент) |
| 2 | Файл не найден |
| 3 | libredwg не загружена |
| 4 | Ошибка парсинга DWG (fatal) |
| 5 | Strict mode: неразрешённая ошибка |
| 10 | Внутренняя ошибка (access violation и т.п.) |

---

## 10. Обработка ошибок (стратегия)

### 10.1 Режимы

| Ситуация | Strict mode | Tolerant mode |
|----------|-------------|----------------|
| Неизвестный тип объекта | **Ошибка**, загрузка останавливается | `TDWGUnknownObject` + Warning |
| Битый handle | **Ошибка** (для critical ссылок, например owner) | Warning, ссылка остаётся `null` |
| Нет слоя у entity | **Ошибка** | Warning + `layer = "0"` |
| Ошибка декодирования текста | **Ошибка** | Warning + hex-дамп |

### 10.2 Логгер

```pascal
IDWGLogger = interface
  procedure Error(Code: Integer; const Msg: string; Handle: TDWGHandle = 0);
  procedure Warning(Code: Integer; const Msg: string; Handle: TDWGHandle = 0);
  procedure Info(const Msg: string);
end;

// Коды ошибок (фиксированные)
const
  ERR_LIB_NOT_LOADED = 100;
  ERR_PARSE_FAILED = 101;
  WARN_UNKNOWN_TYPE = 200;
  WARN_BROKEN_HANDLE = 201;
  WARN_TEXT_DECODE = 202;
```

---

## 11. Этапы разработки (понедельно)

### Неделя 1: Фундамент
- [ ] `fpdwg_types.pas` + `TDWGHandleRef`
- [ ] `fpdwg_logger.pas`
- [ ] `fpdwg_registry.pas` + unit tests (дубликаты, поиск)
- [ ] `fpdwg_reader.pas` + smoke test на корректном DWG

### Неделя 2: Модель + Builder
- [ ] `model/fpdwg_model_base.pas` (TDWGObject, TDWGEntity)
- [ ] `model/fpdwg_model_tables.pas` (Layer, Linetype)
- [ ] `model/fpdwg_model_entities.pas` (Line)
- [ ] `fpdwg_factory.pas` + `fpdwg_mapper_*.pas`
- [ ] `fpdwg_builder.pas` + интеграционный тест

### Неделя 3: Resolver + Reporter
- [ ] `fpdwg_resolver.pas`
- [ ] `fpdwg_reporter_text.pas`
- [ ] `fpdwg_reporter_json.pas`
- [ ] Интеграция с Document

### Неделя 4: CLI + Полировка
- [ ] `fpdwginspect.lpr` (парсер аргументов)
- [ ] Поддержка `--lib`, `--mode`, `--format`
- [ ] Тесты на битых ссылках (сгенерированных)
- [ ] Документация (README + примеры)

---

## 12. Критерии приёмки (без вариантов)

### 12.1 Блокеры (must have)
1. Приложение собирается без ошибок на FPC 3.2+ / Lazarus 2.2+
2. `fpdwginspect samples/simple.dwg --summary` выдаёт непустой вывод
3. Ни одного access violation на предоставленном наборе DWG (5+ файлов)
4. Для LINE корректно выводятся: start/end (3D), length_3d, length_xy, layer, linetype
5. Unknown object не пропадает молча (есть в отчёте)
6. Битая ссылка не вызывает падения в tolerant mode
7. JSON валиден и проходит `jq .`

### 12.2 Тестовый набор (минимальный)
| Файл | Что проверяет |
|------|---------------|
| `line.dwg` | 1 линия на слое "0" |
| `layers.dwg` | 3 слоя, разные цвета |
| `broken_ref.dwg` | ссылка на несуществующий слой |
| `r2000.dwg` | старая версия DWG |
| `r2018.dwg` | новая версия DWG |

### 12.3 Метрики качества
- Покрытие кода тестами: >70% для Registry, Resolver, HandleRef
- Время загрузки файла на 10 000 объектов: <2 секунд
- Память: не более 2× размер файла

---

## 13. Расширения после релиза (не в MVP)

Приоритетный бэклог:

1. **P1** — CIRCLE, ARC, POINT
2. **P1** — LWPOLYLINE (вершины)
3. **P2** — TEXT, MTEXT
4. **P2** — INSERT (блоки)
5. **P3** — HATCH, SPLINE
6. **P3** — Markdown-отчёты
7. **P4** — Graphviz-экспорт (визуализация ссылок)

---

## 14. Риски и митигации (форс-мажор)

| Риск | Вероятность | Митигация |
|------|-------------|------------|
| LibreDWG падает на специфичном DWG | Средняя | Обёртка `try/except` вокруг `dwg_read_file`, fallback к `dxf_read_file` |
| `BITCODE_H.absolute_ref` всегда 0 на некоторых версиях | Высокая | Диагностический вывод, поддержка `handleref.value` как fallback |
| Утечка памяти через исключения в Builder | Средняя | `TInterfacedObject` для документа + `finally dwg_free` |
| Несовместимость типов в h2pas-биндинге | Низкая | Автоматизированная регенерация из свежего `dwg.h` |
| Дубликат handle в реальном файле | Очень низкая | Лог + игнорирование второго объекта, но не падение |

---

## 15. Документация для разработчика (должна быть)

В `cad_source/components/fpdwg/inspector/README.md`:

```markdown
## Как добавить новый тип объекта

1. Создать класс-наследник TDWGEntity или TDWGObject
2. Реализовать mapper, наследующий IDWGObjectMapper
3. Зарегистрировать в фабрике: `Factory.Register(DWG_TYPE_CIRCLE, TCircleMapper.Create)`
4. Добавить в Reporter метод вывода
5. Добавить в CLI фильтр (опционально)

Пример: см. `mappers/fpdwg_map_line.pp`
```

---

## 16. Подпись

Данное ТЗ считается **исполнимым и достаточным** для реализации синьор-разработчиком.  
Все неопределённости сняты, границы ответственности модулей зафиксированы, риски документированы.

**Архитектурный компромисс:**  
Отказ от lazy loading в первой верзии — осознанный, в пользу предсказуемости времени жизни данных.

**Главный принцип реализации:**  
Проще воспроизвести всю сырую структуру LibreDWG, но материализовать только нужные объекты, чем пытаться частично читать файл.

---