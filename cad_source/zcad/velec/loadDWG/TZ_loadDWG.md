# Техническое задание: новая архитектура загрузки DWG-файлов

> Issue: [veb86/zcadvelecAI#1039](https://github.com/veb86/zcadvelecAI/issues/1039)
> Цель: подготовить техническое задание (ТЗ) на новую архитектуру загрузки DWG-файлов через библиотеку LibreDWG так, чтобы по уровню поддержки сущностей и таблиц достичь паритета с действующей загрузкой DXF.
> Ограничение: каждый модуль — одна чёткая ответственность, объём 300–500 строк, без смешения логики.
>
> В данном ТЗ ничего не кодится — это исследование существующих архитектур и план модернизации.

---

## 1. Контекст и задача

В проекте zcadvelecAI существует две независимые подсистемы чтения чертежей:

1. **DXF** — собственный текстовый парсер на Pascal (`uzeffdxf.pas`, `uzeffdxfsupport.pas`). Покрывает полный набор сущностей (LINE, CIRCLE, ARC, LWPOLYLINE, POLYLINE, INSERT, TEXT, MTEXT, DIMENSION, HATCH, SPLINE, ELLIPSE, SOLID, 3DFACE, POINT) плюс таблицы LAYER, LTYPE, STYLE, DIMSTYLE, BLOCK_RECORD, VPORT и блоки. Поддержка проксирования неизвестных объектов.
2. **DWG** — тонкая обёртка над сторонней C-библиотекой LibreDWG (`uzefflibredwg.pas`, `uzefflibredwg2ents.pas`, `cad_source/components/fpdwg/`). На текущий момент конвертируются всего две сущности (`LAYER`, `LINE`), остальные обработчики — заглушки с логированием имени.

**Задача:** разработать архитектуру загрузки DWG, повторяющую архитектурные принципы DXF, обеспечивающую паритет по поддержке сущностей и таблиц, и одновременно структурированную в модули по 300–500 строк с одной ответственностью каждый. Сама реализация выполняется этапами, описанными в разделе [11. Этапы разработки](#11-этапы-разработки).

---

## 2. Анализ существующей архитектуры загрузки DXF

### 2.1 Точка входа и регистрация формата

| Шаг | Файл, строка | Описание |
|---|---|---|
| Регистрация расширения | `cad_source/zcad/register/uzcregfileformats.pas:72` | `Ext2LoadProcMap.RegisterExt('dxf', 'AutoCAD DXF files via zengine (*.dxf)', @LoadDXFviaZEnfine, true)` |
| Универсальный диспетчер | `cad_source/zengine/fileformats/uzeffmanager.pas:29-177` | Шаблонный класс `TExt2ProcMap<TFileLoadProcedure>` хранит карту «расширение → процедура загрузки» |
| Главная функция загрузки | `cad_source/zengine/fileformats/uzeffdxf.pas:1353` | `AddFromDXF(AFileName, dwgCtx, LogIntf): TDXFHeaderInfo` |
| Версионная диспетчеризация | `uzeffdxf.pas` | По `$ACADVER` выбирается ветка `AddFromDXF12` (AC1009) либо `AddFromDXF20XX` (AC1014–AC1032) |

### 2.2 Структура модулей DXF

| Модуль | Строк | Ответственность |
|---|---|---|
| `uzeffdxf.pas` | 1444 | Оркестрация: HEADER → TABLES → BLOCKS → ENTITIES, диспетчеризация сущностей, post-processing handle-ссылок. **Перевышает порог 500 — кандидат на дробление.** |
| `uzeffdxfsupport.pas` | 663 | Низкоуровневые помощники: чтение DXF group codes, версии, кодовые страницы, парсинг `$VARS`. **Тоже выше 500 строк, но утилитарный характер допустим.** |
| `uzeffmanager.pas` | 187 | Универсальный реестр форматов (DXF, DWG и пр.) |
| `uzeffdxfout.pas` | 1424 | Запись (save) DXF — отдельная подсистема, в ТЗ затрагивается только косвенно. |
| `uzestylesmleaderdxf.pas` | 1501 | Чтение/запись DXF-структур стилей мульти-выносок (специализированная плагин-логика) |
| `uzestylestablesdxf.pas` | 978 | Чтение/запись DXF-структур стилей ACAD-таблиц |
| `uzeacadtable_dxf_read.pas` | 448 | Чтение примитива ACAD_TABLE из DXF |

### 2.3 Реестр сущностей и шаблон диспетчеризации

Центральный реестр: `cad_source/zengine/core/uzeentityfactory.pas`.

```pascal
TEntInfoData = record
  DXFName, UserName: String;
  EntityID: TObjID;
  AllocEntity: TAllocEntFunc;
  AllocAndInitEntity: TAllocAndInitEntFunc;
  SetGeomPropsFunc: TSetGeomPropsFunc;
  AAllocAndCreateEntFunc: TAllocAndInitAndSetGeomPropsFunc;
end;

DXFName2EntInfoData: TDXFName2EntInfoDataMap;  // карта 'LINE' → TEntInfoData
ObjID2EntInfoData:   TObjID2EntInfoDataMap;    // карта GDBlineID → TEntInfoData
```

В секции `initialization` каждой сущности вызывается:

```pascal
RegisterDXFEntity(GDBlineID, 'LINE', 'Line',
  @AllocLine, @AllocAndInitLine,
  @SetLineGeomProps, @AllocAndCreateLine);
```

Регистрации обнаружены для: `LINE`, `CIRCLE`, `ARC`, `POINT`, `LWPOLYLINE`, `POLYLINE`, `ELLIPSE`, `SPLINE`, `SOLID`, `3DFACE`, `INSERT`, `HATCH`, `TEXT`, `MTEXT`, `DIMENSION`, `ACAD_TABLE`, прокси-объекты.

### 2.4 Конвейер загрузки сущности

Из `uzeffdxf.pas`, процедура `addentitiesfromdxf` (~строки 392–542):

1. Прочитать group code 0 → имя сущности.
2. `FindOrProxyEntInfo(name)` → получить `TEntInfoData` (либо прокси, если имя не зарегистрировано и не в чёрном списке).
3. `EntInfoData.AllocAndInitEntity(owner)` → создать пустую сущность.
4. Виртуальный `LoadFromDXF(rdr, ptu, drawing, context)` сущности — заполнить поля по group codes.
5. `FromDXFPostProcessBeforeAdd` → ранний пост-процессинг.
6. Зарегистрировать handle сущности в `context.h2p` (карта `handle → pointer`) для последующего разрешения ссылок (owner, reactors).
7. Если задан OwnerHandle — разрешить родителя через `h2p` и применить родительские трансформации.
8. `BuildGeometry(drawing)` → построить производную геометрию.
9. `FormatAfterDXFLoad(drawing, dc)` → применить отображение.
10. `FromDXFPostProcessAfterAdd` → поздний пост-процессинг.

### 2.5 Чтение секций

Порядок чтения (`AddFromDXF20XX`, `uzeffdxf.pas:1168–1330`):

1. **HEADER** (`ReadDXFHeader`, `uzeffdxf.pas:1385`)
   - `$ACADVER`, `$DWGCODEPAGE`, `$CLAYER`, `$TEXTSTYLE`, `$DIMSTYLE`, `$CELTYPE`, `$LWDISPLAY`, `$LTSCALE`, `$INSUNITS`, и т. д.
   - Все переменные складываются в `context.DWGVarsDict: TStringStringMap`.
   - Дефолты применяются к `drawing.CurrentLineW`, `drawing.LWDisplay`, `drawing.LTScale`, …
2. **CLASSES** — сохраняется как сырой блок для round-trip совместимости.
3. **TABLES**
   - VPORT → `ReadVport`
   - LAYER → `ReadLayers` → `LayerTable.MergeItem`
   - LTYPE → `ReadLTStyles` (включая dash patterns + shape handles)
   - STYLE → `ReadTextstyles`
   - DIMSTYLE → `ReadDimStyles`
   - BLOCK_RECORD → `ReadBlockRecord` (привязка handles → имена блоков)
4. **BLOCKS** — для каждого блока (кроме `*MODEL_SPACE`, `*PAPER_SPACE`, `*A*`, `*D*`):
   - `BlockDefArray.create(blockName)`
   - Рекурсивный вызов `addentitiesfromdxf` до `ENDBLK`.
5. **ENTITIES** → `addentitiesfromdxf` пока не встретится `ENDSEC`.
6. **OBJECTS** — сохраняется как сырой блок.
7. Пост-процессинг стилей: `DimStyleTable.ResolveTextstyles`, `ResolveDXFHandles`, `ResolveLineTypes`.

### 2.6 Сильные стороны архитектуры DXF

- Чёткая регистрация сущностей через `RegisterDXFEntity` — расширяемость без правки парсера.
- Виртуальные методы `LoadFromDXF` сущностей — логика загрузки лежит в самой сущности.
- Двух-проходное разрешение ссылок через `h2p`-карту handle → pointer.
- Прокси-механизм для неизвестных сущностей (см. `uzeentacdproxy.pas`, `uzebackupentacdproxy.pas`) — ничего не теряется.
- Расширяемые «hook-указатели» (`CreateExtLoadData`, `ClearExtLoadData`, `FreeExtLoadData`) — модули вроде `acadtable` подключают свою сериализацию без правки ядра.
- Поддержка двух режимов: `TLOLoad` (полная загрузка) и `TLOMerge` (мерж стилей).

### 2.7 Слабые стороны архитектуры DXF (учитываем как уроки)

- `uzeffdxf.pas` (1444 строки) совмещает оркестрацию, чтение таблиц и блоков. **Превышает планку 300–500 строк.**
- Чтения таблиц (Layers, LTStyles, TextStyles, DimStyles) стоило бы разнести в `uzeffdxftables.pas`.
- Часть утилит (`uzeffdxfsupport.pas`, 663 строки) тоже выше планки и неоднородна — кандидат на разделение.

---

## 3. Анализ существующей загрузки DWG

### 3.1 Размещение и регистрация

| Шаг | Файл, строка | Состояние |
|---|---|---|
| Регистрация | `cad_source/zcad/register/uzcregfileformats.pas:75–76` | DWG и DXF (через LibreDWG) подключаются к общему диспетчеру |
| Точка входа DWG | `cad_source/zengine/fileformats/uzefflibredwg.pas:68` | `addfromdwg(filename, ZCDCtx, LogProc)` |
| Точка входа DXF (через LibreDWG) | `uzefflibredwg.pas:105` | `addfromdxf(filename, ZCDCtx, LogProc)` (альтернативный загрузчик) |
| Парсер LibreDWG-данных | `cad_source/components/fpdwg/dwgproc.pp:65` | Шаблонный класс `GDWGParser<GUserCtx>` — диспетчеризация по `DWG_OBJECT_TYPE` |
| Конвертеры в zcad-сущности | `uzefflibredwg2ents.pas:39–108` | Только LAYER + LINE реально конвертируются; LTYPE / BLOCK_HEADER / BLOCK — заглушки с `zDebugLn`. |

### 3.2 Текущая структура (метрики)

| Модуль | Строк | Ответственность | Превышение? |
|---|---|---|---|
| `uzeffmanager.pas` | 187 | Реестр форматов | ✅ норм |
| `uzefflibredwg.pas` | 143 | Загрузка LibreDWG, передача в parser | ✅ норм |
| `uzefflibredwg2ents.pas` | 110 | Конвертеры объектов/сущностей DWG → zcad | ✅ норм по объёму, **но почти пустой** |
| `cad_source/components/fpdwg/dwg.pp` | ~16285 | Авто-сгенерированный binding LibreDWG (h2pas) | Бинарный артефакт; не дробится |
| `cad_source/components/fpdwg/dwgproc.pp` | 284 | Динамическая загрузка `libredwg.so/.dll` + базовый `GDWGParser` | ✅ норм |
| `cad_source/components/fpdwg/fpdwg.pas` | 21 | LCL-регистрация пакета | ✅ норм |
| `cad_source/zcad/trash/uzeffdwg.pas` | 1437 | **Заброшенный** нативный pascal-парсер DWG2004 | удалить из пути сборки (уже в `trash/`) |

### 3.3 Что фактически конвертируется

В `uzefflibredwg2ents.pas` (секция `initialization`, строки 102–108):

```pascal
ZCDWGParser.RegisterDWGObjectLoadProc(DWG_TYPE_LAYER,         @AddLayer);
ZCDWGParser.RegisterDWGObjectLoadProc(DWG_TYPE_LTYPE,         @AddLineType);     // stub
ZCDWGParser.RegisterDWGObjectLoadProc(DWG_TYPE_BLOCK_HEADER,  @AddBlockHeader);  // stub
ZCDWGParser.RegisterDWGEntityLoadProc(DWG_TYPE_LINE,          @AddLineEntity);
ZCDWGParser.RegisterDWGEntityLoadProc(DWG_TYPE_BLOCK,         @AddBlock);        // stub
```

**Реально работающих обработчиков — 2** (LAYER, LINE). Все геометрические сущности кроме LINE (CIRCLE, ARC, LWPOLYLINE, POLYLINE, ELLIPSE, SPLINE, POINT, INSERT, TEXT, MTEXT, DIMENSION, HATCH, SOLID, 3DFACE, ATTRIB, ATTDEF, MLINE, RAY, XLINE, REGION, 3DSOLID, BODY, TRACE, SHAPE, VIEWPORT, LEADER, MLEADER, TOLERANCE) **отсутствуют**.

### 3.4 Известные дефекты

1. **Z-координата LINE утрачивается.** `uzefflibredwg2ents.pas:93,96` — `lBegin.z` и `lEnd.z` присваиваются от `.x` вместо `.z`:
   ```pascal
   PGDBObjLine(pobj)^.CoordInOCS.lBegin.z := PLine^.start.x;   // BUG: should be .z
   PGDBObjLine(pobj)^.CoordInOCS.lEnd.z   := PLine^.end_.x;    // BUG: should be .z
   ```
   Обнаружен напрямую в исходнике; должен быть исправлен в этапе 1 (стабилизация).
2. **Нет валидации типа `P` в обработчиках.** В `parseDwg_Data` (`dwgproc.pp:198–210`) указатель приводится к структуре LibreDWG без проверки `fixedtype` против ожидаемого. Это работает потому, что регистрация ведётся по `DWG_OBJECT_TYPE`, но молча падает при коллизии регистрации.
3. **Пост-обработка отсутствует.** В коде закомментированы `BuildGeometry` и `formatEntity`. Сущности добавляются в дерево, но геометрия и форматирование не строятся → возможны проблемы с отрисовкой и hit-test.
4. **HEADER / `DWGVars`-словарь.** В DXF `context.DWGVarsDict` заполняется значениями `$CLAYER, $LTSCALE, $INSUNITS, …`. В DWG-загрузчике переменные заголовка только логируются (`DebugDWG`, `uzefflibredwg.pas:45–61`) и не передаются в `drawing`.
5. **Отсутствует двух-проходное разрешение ссылок** (handle → pointer). LibreDWG возвращает уже резолвленные указатели, но zcad-сущностям нужны `BlockDefRef`, `LayerRef` по имени; сейчас поиск зависимостей не ведётся.
6. **Прогресс-репорт работает** (`PLP` в `uzefflibredwg.pas:63`), но прерывание процесса невозможно.
7. **Нет режима Merge vs Load** — игнорируется `ZCDCtx.LoadMode` в обработчиках, кроме `AddLayer`.
8. **Нет fallback для неизвестных DWG-типов** — в DXF есть прокси-механизм, в DWG неизвестный объект просто пропускается.

### 3.5 Структурные проблемы текущего DWG-загрузчика

- Все обработчики собраны в одном файле `uzefflibredwg2ents.pas`. В DXF — каждая сущность сама регистрирует свой обработчик в собственном модуле (см. `uzeentline.pas:747`, `uzeentcircle.pas:644`, …).
- Нет «таблиц чтения» (LAYER, LTYPE, STYLE, DIMSTYLE, BLOCK_RECORD, VPORT). У DXF аналогичные таблицы вынесены в подпрограммы внутри `uzeffdxf.pas` и стилевые модули.
- Нет интеграции с реестром `RegisterDXFEntity` — DWG-обработчики вынуждены дублировать выделение и инициализацию объектов.
- Нет тестов, ни юнит-, ни интеграционных. У DXF тоже немного, но есть `dxf_samples/` и `dxf_analysis/`.

---

## 4. Сравнение DXF vs DWG: матрица паритета

| Возможность | DXF | DWG (текущий) | Цель новой архитектуры |
|---|---|---|---|
| Дискавер версии файла | $ACADVER → DXF12/DXF20XX | LibreDWG раскладывает в `dwg.header.version` | оставить как у LibreDWG |
| Кодировки | $DWGCODEPAGE + UTF-8 для R_2007+ | частично через `BITCODE_T2Text` | нормализовать в едином модуле |
| HEADER → DWGVars | да, через `ReadDXFHeader` | нет | добавить `loadDWG/header.pas` |
| Таблица LAYER | да | да (с замечаниями: нет linetype-ref, true color) | расширить до паритета |
| Таблица LTYPE | да (с dash patterns + shape handles) | заглушка | реализовать в `loadDWG/tables/ltype.pas` |
| Таблица STYLE (text) | да | нет | реализовать в `loadDWG/tables/textstyle.pas` |
| Таблица DIMSTYLE | да + ResolveTextstyles/ResolveLineTypes | нет | реализовать в `loadDWG/tables/dimstyle.pas` |
| Таблица VPORT | да | нет | реализовать в `loadDWG/tables/vport.pas` |
| BLOCK_RECORD | да | заглушка | реализовать в `loadDWG/blocks/blockrecord.pas` |
| BLOCKS (определения) | да + рекурсивная загрузка | заглушка | реализовать в `loadDWG/blocks/blockcontent.pas` |
| LINE | ✅ | ✅ (с багом по Z) | исправить |
| CIRCLE | ✅ | ✗ | реализовать |
| ARC | ✅ | ✗ | реализовать |
| POINT | ✅ | ✗ | реализовать |
| LWPOLYLINE | ✅ | ✗ | реализовать |
| POLYLINE (2D/3D/mesh) | ✅ | ✗ | реализовать |
| ELLIPSE | ✅ | ✗ | реализовать |
| SPLINE | ✅ | ✗ | реализовать |
| TEXT | ✅ | ✗ | реализовать |
| MTEXT | ✅ | ✗ | реализовать |
| INSERT (block ref) | ✅ + ATTRIB | ✗ | реализовать |
| ATTRIB / ATTDEF | ✅ | ✗ | реализовать |
| DIMENSION (все подтипы) | ✅ | ✗ | реализовать |
| HATCH | ✅ | ✗ | реализовать |
| SOLID | ✅ | ✗ | реализовать |
| 3DFACE | ✅ | ✗ | реализовать |
| MLINE / LEADER / MLEADER / TOLERANCE | частично | ✗ | по приоритету (см. этапы) |
| Прокси для неизвестных | ✅ (`AcdProxyEntity`) | ✗ | реализовать в `loadDWG/proxy.pas` |
| Двух-проходное разрешение handle | ✅ (`context.h2p`) | ✗ (LibreDWG резолвит сам, но имена/линки нужно сшивать) | реализовать в `loadDWG/resolve.pas` |
| BuildGeometry / formatEntity | ✅ | закомментировано | подключить |
| LoadMode (Load vs Merge) | ✅ | частично | пробросить во все обработчики |
| Прокси-XData | сохраняется | теряется | переносить EED → XData |
| Hook-расширения (acadtable, mleader) | ✅ | ✗ | предусмотреть аналогичный механизм |

---

## 5. Принципы новой архитектуры

1. **Одна ответственность на модуль.** Каждый файл `.pas` — одна задача (один тип таблицы / одна группа сущностей / одна функция оркестрации / один утилитарный набор).
2. **Целевой объём 300–500 строк.** Превышение допустимо только для авто-сгенерированных файлов (`dwg.pp`).
3. **Расширяемость через регистрацию.** Каждый обработчик регистрируется в `ZCDWGParser` в собственной секции `initialization` своего модуля — так же, как DXF-сущности регистрируются в `RegisterDXFEntity` в собственных файлах.
4. **Повторное использование zcad-конструкторов.** Использовать существующие `AllocAndInit*` (см. `uzeentline.pas`, `uzeentcircle.pas`, …) и `_StandartLineCreateProcedure` из `uzeentityfactory.pas`. Не копировать логику инициализации.
5. **Симметрия с DXF.** Этапы загрузки повторяют DXF-конвейер: HEADER → TABLES → BLOCKS → ENTITIES → resolve → post-process.
6. **Контекст загрузки явный.** Один `TZDrawingContext` + расширенный `TDWGLoadCtx` (см. § 6.1) с handle-картой, `DWGVarsDict`, режимом загрузки, статистикой.
7. **Никакого побочного состояния в модулях.** Все данные — в `TDWGLoadCtx`, передаваемом по ссылке.
8. **Прокси для неизвестных сущностей** — обязательно. Никаких потерь данных при загрузке стороннего DWG.
9. **Тестируемость.** Каждый обработчик тестируется на маленьком DWG из `dxf_samples` (можно конвертировать существующие DXF в DWG через ODA или TeighaConverter).
10. **Совместимость с LibreDWG.** Все привязки через `dwg.pp` — никаких ручных смещений в бинарнике. При обновлении LibreDWG обновляется только `dwg.pp` + `dwgproc.pp`.

---

## 6. Целевая структура каталога `cad_source/zcad/velec/loadDWG/`

```
loadDWG/
├── TZ_loadDWG.md                       — этот документ
├── README.md                            — краткое описание + ссылки
│
├── loadDWG.pas                          — фасад: addfromdwg(filename, ctx, log)
│                                          (~150 строк) — заменяет/обёртывает uzefflibredwg.pas
│
├── core/                                [Ядро оркестрации]
│   ├── uzdwgloadcontext.pas             — TDWGLoadCtx, DWGVarsDict, h2p-карта, статистика  (~200)
│   ├── uzdwgorchestrator.pas            — порядок: header → tables → blocks → entities    (~250)
│   ├── uzdwgresolver.pas                — двух-проходное разрешение handle ↔ pointer       (~200)
│   └── uzdwgproxy.pas                   — fallback для неизвестных DWG_TYPE_*               (~250)
│
├── header/                              [Заголовок документа]
│   └── uzdwgheader.pas                  — Dwg_Header_Variables → drawing.DWGVarsDict       (~350)
│
├── tables/                              [Системные таблицы]
│   ├── uzdwgtbllayer.pas                — DWG_TYPE_LAYER → LayerTable                       (~250)
│   ├── uzdwgtblltype.pas                — DWG_TYPE_LTYPE → LTypeStyleTable (с dash/shape)   (~350)
│   ├── uzdwgtbltextstyle.pas            — DWG_TYPE_STYLE → TextStyleTable                   (~200)
│   ├── uzdwgtbldimstyle.pas             — DWG_TYPE_DIMSTYLE → DimStyleTable                 (~400)
│   ├── uzdwgtblvport.pas                — DWG_TYPE_VPORT → ViewportTable                    (~250)
│   ├── uzdwgtblucs.pas                  — DWG_TYPE_UCS → UCSTable                           (~200)
│   ├── uzdwgtblview.pas                 — DWG_TYPE_VIEW                                     (~200)
│   ├── uzdwgtblappid.pas                — DWG_TYPE_APPID                                    (~150)
│   └── uzdwgtblblockrecord.pas          — DWG_TYPE_BLOCK_HEADER (запись таблицы)            (~250)
│
├── blocks/                              [Блоки и вставки]
│   ├── uzdwgblockdef.pas                — определение блока: содержимое до ENDBLK            (~300)
│   ├── uzdwgblockinsert.pas             — DWG_TYPE_INSERT (вставка)                         (~300)
│   └── uzdwgblockattrib.pas             — DWG_TYPE_ATTRIB / DWG_TYPE_ATTDEF                 (~250)
│
├── entities/                            [Геометрические примитивы]
│   ├── uzdwgentline.pas                 — DWG_TYPE_LINE  (исправление Z-бага)               (~120)
│   ├── uzdwgentcircle.pas               — DWG_TYPE_CIRCLE                                   (~150)
│   ├── uzdwgentarc.pas                  — DWG_TYPE_ARC                                      (~180)
│   ├── uzdwgentpoint.pas                — DWG_TYPE_POINT                                    (~120)
│   ├── uzdwgentellipse.pas              — DWG_TYPE_ELLIPSE                                  (~200)
│   ├── uzdwgentlwpolyline.pas           — DWG_TYPE_LWPOLYLINE                               (~300)
│   ├── uzdwgentpolyline2d.pas           — DWG_TYPE_POLYLINE_2D                              (~300)
│   ├── uzdwgentpolyline3d.pas           — DWG_TYPE_POLYLINE_3D                              (~250)
│   ├── uzdwgentpolylinemesh.pas         — DWG_TYPE_POLYLINE_MESH / PFACE                    (~350)
│   ├── uzdwgentspline.pas               — DWG_TYPE_SPLINE                                   (~350)
│   ├── uzdwgenttext.pas                 — DWG_TYPE_TEXT                                     (~250)
│   ├── uzdwgentmtext.pas                — DWG_TYPE_MTEXT                                    (~400)
│   ├── uzdwgentsolid.pas                — DWG_TYPE_SOLID                                    (~150)
│   ├── uzdwgent3dface.pas               — DWG_TYPE__3DFACE                                  (~150)
│   ├── uzdwgenthatch.pas                — DWG_TYPE_HATCH                                    (~500)
│   ├── uzdwgentdimension.pas            — все DWG_TYPE_DIMENSION_*                          (~450)
│   ├── uzdwgentleader.pas               — DWG_TYPE_LEADER                                   (~250)
│   ├── uzdwgentmleader.pas              — DWG_TYPE_MULTILEADER                              (~400)
│   └── uzdwgentmline.pas                — DWG_TYPE_MLINE                                    (~300)
│
├── support/                             [Утилиты]
│   ├── uzdwgcodepage.pas                — кодировки, BITCODE_T → string                     (~200)
│   ├── uzdwgcolor.pas                   — DWG color index/true color/transparency           (~200)
│   ├── uzdwggeom.pas                    — point3d, OCS-helpers, нормализация направления    (~250)
│   ├── uzdwghandle.pas                  — handle-helpers, BITCODE_H разворачивание          (~150)
│   ├── uzdwglog.pas                     — обёртка `zDebugLn` + статистика типов             (~150)
│   └── uzdwgxdata.pas                   — EED/XData копирование для round-trip              (~250)
│
├── tests/                               [Юнит-тесты и фикстуры]
│   ├── samples/                         — мини-DWG (LINE-only, LAYER-only, BLOCK, …)
│   ├── uzdwgtest_orchestrator.pas       — интеграционный тест полной загрузки
│   └── uzdwgtest_entities.pas           — тест каждой группы примитивов
│
└── docs/
    ├── ARCHITECTURE.md                  — схема слоёв и потоков данных
    ├── COVERAGE.md                      — текущая матрица паритета DXF/DWG
    └── HOWTO_ADD_ENTITY.md              — инструкция «как добавить новый DWG-тип»
```

**Итог:** 30+ маленьких модулей вместо одного «толстого» `uzefflibredwg2ents.pas`. Каждый легко тестируется и заменяется.

---

## 7. Ключевые контракты и интерфейсы

### 7.1 Расширенный контекст загрузки

```pascal
// loadDWG/core/uzdwgloadcontext.pas
type
  TDWGHandle2Pointer = TGenericHashMap<BITCODE_H, Pointer, ...>;
  TDWGTypeStat       = record Loaded, Skipped, Proxy: Integer; end;

  PDWGLoadCtx = ^TDWGLoadCtx;
  TDWGLoadCtx = record
    Z:           TZDrawingContext;       // существующий
    DWG:         TDWGCtx;                // от dwgproc.pp
    Vars:        TStringStringMap;       // аналог DXF DWGVarsDict
    H2P:         TDWGHandle2Pointer;     // handle → pointer
    CurrentBlock: PGDBBlockDef;          // активный блок при загрузке BLOCK_HEADER
    Stats:       array of TDWGTypeStat;  // по DWG_OBJECT_TYPE
    LogProc:     TZELogProc;
    Cancelled:   Boolean;
  end;
```

### 7.2 Контракт обработчика

Каждый модуль `entities/*.pas`, `tables/*.pas`, `blocks/*.pas` следует одному шаблону:

```pascal
// шаблон: loadDWG/entities/uzdwgent<NAME>.pas
unit uzdwgent<NAME>;
{$Mode delphi}{$H+}
interface
implementation
uses dwg, dwgproc, uzeffLibreDWG, uzdwgloadcontext, uzeent<NAME>, ...;

procedure Add<NAME>(var Z: TZDrawingContext; var DC: TDWGCtx;
                    var Obj: Dwg_Object; P: PDwg_Entity_<NAME>);
var
  ent: PGDBObj<NAME>;
  ctx: PDWGLoadCtx;
begin
  ctx := GetDWGLoadCtx(Z);  // достать TDWGLoadCtx из Z (через user-data)
  ent := PGDBObj<NAME>(AllocAndInit<NAME>(Z.PDrawing^.pObjRoot));
  // 1) копирование геометрии
  // 2) применение OCS / common entity props (layer, ltype, color, lineweight)
  // 3) ent^.BuildGeometry(Z.PDrawing^);
  // 4) ent^.formatEntity(Z.PDrawing^, Z.DC);
  Z.PDrawing^.pObjRoot^.AddMi(ent);
  ctx^.H2P.Insert(Obj.handle.value, ent);
  Inc(ctx^.Stats[Ord(Obj.fixedtype)].Loaded);
end;

initialization
  ZCDWGParser.RegisterDWGEntityLoadProc(DWG_TYPE_<NAME>, @Add<NAME>);
end.
```

Каждый такой модуль — самодостаточен, имеет одну функцию-обработчик и одну регистрацию.

### 7.3 Общий поток (orchestrator)

```pascal
// loadDWG/core/uzdwgorchestrator.pas
procedure addfromdwg(const filename: String; var Z: TZDrawingContext; const log: TZELogProc);
var
  ctx: TDWGLoadCtx;
begin
  ctx := DWGLoadCtxCreate(Z, log);
  try
    LoadLibreDWG;
    DWGReadFile(filename, ctx);                   // libredwg-обёртка (uzefflibredwg.pas наследник)
    DWGHeader_Apply(ctx);                         // header/uzdwgheader.pas
    DWGTables_Pass1(ctx);                         // tables/* — только аллокация
    DWGTables_Pass2_Resolve(ctx);                 // resolver
    DWGBlocks_Load(ctx);                          // blocks/uzdwgblockdef.pas
    DWGEntities_Load(ctx);                        // через ZCDWGParser
    DWGResolve_Final(ctx);                        // ссылки на блоки, BlockInsert.BlockDefRef
    DWGStyles_PostProcess(ctx);                   // DimStyle.ResolveTextstyles и т.п.
  finally
    DWGLoadCtxDestroy(ctx);
    dwg_free(@ctx.DWG.dwg);
  end;
end;
```

### 7.4 Общие свойства сущности (color, layer, ltype, lineweight)

Все DWG-сущности наследуют `Dwg_Object_Entity`, в котором лежат `color`, `linetype`, `layer`, `lineweight`, `transparency`, `ltype_scale`. В DXF это группы `8` (layer), `6` (ltype), `62` (color), `370` (lineweight). Нужен единый помощник:

```pascal
// loadDWG/support/uzdwgentcommon.pas
procedure ApplyCommonEntityProps(ent: PGDBObjEntity;
                                 const e: Dwg_Object_Entity;
                                 var ctx: TDWGLoadCtx);
```

Он вызывается из каждого `entities/uzdwgent*.pas` и инкапсулирует разрешение layer-handle через `ctx.H2P`, цвет, толщину линии и пр.

---

## 8. Этапные риски и предположения

1. **LibreDWG-API нестабилен.** Между 0.13 и 0.14 переименования (`Layer.on → off`, `Line.end → end_`) уже ломали сборку (см. PR #1037). Решение: фиксировать минимальную версию LibreDWG в `dwg.pp`, отслеживать ABI-изменения в CI.
2. **h2pas-конвертированный `dwg.pp` (16285 строк)** не входит в правило 300–500 строк — это автогенерация. Допускается как исключение и поясняется в `support/README.md`.
3. **Часть DWG-сущностей в LibreDWG представлена объединениями.** Нужно проверять `Obj.tio.entity^.tio.<TypeName>` — это уже реализовано в `dwgproc.pp:201`, но при добавлении новых типов следить за корректным dispatch.
4. **HATCH и MTEXT — крупные форматы.** Их обработчики могут приблизиться к 500 строкам. Если превысят, разделять на подпарсеры (например, `entities/uzdwgenthatchpattern.pas`, `uzdwgenthatchboundary.pas`).
5. **Прокси для неизвестных** требует копирования `ENTITY` целиком как BLOB — у LibreDWG есть `tio.entity^.tio.UNKNOWN_ENT` с raw-данными; механизм есть.

---

## 9. Совместимость

- Существующий `uzefflibredwg.pas` сохраняется как тонкая обёртка вокруг нового `loadDWG/loadDWG.pas` для обратной совместимости. Регистрация в `uzcregfileformats.pas` не меняется.
- `uzefflibredwg2ents.pas` после миграции остаётся пустым / удаляется (его регистрации переезжают в соответствующие `loadDWG/entities/*.pas` и `loadDWG/tables/*.pas`).
- `cad_source/zcad/trash/uzeffdwg.pas` (1437 строк, заброшенный нативный парсер) — оставляется в `trash/`, в новой архитектуре не используется.

---

## 10. Критерии приёмки

ТЗ выполнено, если по итогам всех этапов:

1. Из стандартных тестовых DWG-файлов (на основе DXF-семплов из `dxf_samples/`, конвертированных в DWG) загружаются:
   - все сущности из колонки «реализовать» в § 4;
   - все таблицы LAYER, LTYPE, STYLE, DIMSTYLE, VPORT, BLOCK_RECORD;
   - блоки и их вставки с ATTRIB.
2. Z-координата LINE и других 3D-сущностей сохраняется корректно (бага из § 3.4.1 нет).
3. Каждый `.pas` в `cad_source/zcad/velec/loadDWG/` (кроме автогенерируемого `dwg.pp` в `components/fpdwg/`) имеет 300–500 строк, одну ответственность.
4. Неизвестные DWG_TYPE_* подгружаются как прокси (без потери данных и без падения).
5. `ZCDCtx.LoadMode` (Load vs Merge) корректно прокидывается во все обработчики таблиц.
6. Юнит-тесты в `loadDWG/tests/` зелёные.
7. Документация (`loadDWG/docs/ARCHITECTURE.md`, `COVERAGE.md`, `HOWTO_ADD_ENTITY.md`) обновлена.
8. Сборка проходит на Linux (libredwg.so) и Windows (libredwg-0.dll).
9. CHANGELOG.md обновлён, указано подключение новой архитектуры.

---

## 11. Этапы разработки

> Каждый этап — отдельный pull request. Этапы можно частично параллелить (этапы 3–5 для разных групп сущностей независимы).

### Этап 0 — подготовка инфраструктуры (1 PR)

- Создать `cad_source/zcad/velec/loadDWG/` со скелетом каталогов.
- Зарегистрировать в `cad_source/zcad/velec/Makefile` / `*.lpi`.
- Перенести `addfromdwg` в `loadDWG/loadDWG.pas`, оставить `uzefflibredwg.pas` как deprecated-shim.
- Создать `core/uzdwgloadcontext.pas` (TDWGLoadCtx + хранилище в TZDrawingContext через `Tag` или паттерн «сервис»).
- **Файлы:** `loadDWG.pas`, `core/uzdwgloadcontext.pas`, `core/uzdwgorchestrator.pas` (заглушка).

### Этап 1 — стабилизация существующего (1 PR)

- Исправить Z-баг LINE (`uzefflibredwg2ents.pas:93,96` → `loadDWG/entities/uzdwgentline.pas`).
- Подключить `BuildGeometry` и `formatEntity`.
- Раскидать обработчики LAYER, LINE, LTYPE-stub, BLOCK_HEADER-stub, BLOCK-stub в соответствующие модули `loadDWG/tables/*.pas`, `loadDWG/entities/*.pas`.
- Добавить тест `tests/uzdwgtest_orchestrator.pas` на минимальный DWG (LINE + LAYER).

### Этап 2 — HEADER + поддержка (1 PR)

- `header/uzdwgheader.pas`: считать `dwg.header_vars` и заполнить `ctx.Vars` + `drawing.CurrentLineW`, `drawing.LWDisplay`, `drawing.LTScale`.
- `support/uzdwgcodepage.pas`: единая точка нормализации строк (`BITCODE_T2Text` + UTF-8/ANSI).
- `support/uzdwgcolor.pas`: индексированный + 24-битный цвет, transparency.
- `support/uzdwggeom.pas`: точки, OCS, нормализация направлений.

### Этап 3 — таблицы (2 PR)

- PR 3a: LAYER (расширить), LTYPE (полная реализация: `dash[], shape_handles[]`), STYLE.
- PR 3b: DIMSTYLE (с пост-процессингом ResolveTextstyles, ResolveLineTypes), VPORT, UCS, VIEW, APPID, BLOCK_RECORD.

### Этап 4 — блоки и вставки (1 PR)

- `blocks/uzdwgblockdef.pas`: создание `BlockDefArray.create(name)`; обход entities блока через тот же диспетчер.
- `blocks/uzdwgblockinsert.pas`: DWG_TYPE_INSERT, привязка к `BlockDef` через `H2P`.
- `blocks/uzdwgblockattrib.pas`: ATTRIB / ATTDEF.

### Этап 5 — геометрия 2D/3D (3–4 PR, по группам)

- PR 5a: CIRCLE, ARC, POINT, ELLIPSE.
- PR 5b: LWPOLYLINE, POLYLINE_2D, POLYLINE_3D, POLYLINE_MESH/PFACE, SPLINE.
- PR 5c: SOLID, 3DFACE, TRACE, SHAPE.
- PR 5d: TEXT, MTEXT.

### Этап 6 — аннотации (2 PR)

- PR 6a: DIMENSION (все подтипы), LEADER, MLEADER, TOLERANCE.
- PR 6b: HATCH (включая boundary loops, pattern), MLINE.

### Этап 7 — устойчивость и расширения (1 PR)

- `core/uzdwgproxy.pas`: прокси-механизм для неизвестных DWG_TYPE_*.
- `support/uzdwgxdata.pas`: копирование EED → XData.
- Полные интеграционные тесты на больших DWG.

### Этап 8 — документация и финал (1 PR)

- `docs/ARCHITECTURE.md`, `docs/COVERAGE.md`, `docs/HOWTO_ADD_ENTITY.md`.
- CHANGELOG.md.
- Удаление shim-файла `uzefflibredwg2ents.pas` после миграции всех регистраций.
- Итоговая проверка матрицы паритета § 4.

---

## 12. Что точно НЕ делается в рамках этого ТЗ

- **Запись DWG (save).** Только чтение. Запись DWG потребует отдельного ТЗ.
- **Замена LibreDWG.** Архитектура остаётся обёрткой над `libredwg-0.dll/.so`.
- **Модернизация записи DXF.** `uzeffdxfout.pas` (1424 строки) не трогается.
- **Изменение API `TZDrawingContext`.** Только расширение через `TDWGLoadCtx` рядом.
- **Перепись `cad_source/components/fpdwg/dwg.pp`.** Это автогенерируемый h2pas-binding, ручной правке не подлежит.

---

## 13. Сводка по требованию issue #1039

| Требование | Где выполнено в ТЗ |
|---|---|
| Изучить архитектуру загрузки DXF | § 2 |
| Изучить архитектуру загрузки DWG | § 3 |
| Сформировать новый вариант архитектуры (этапы, узлы) | §§ 5, 6, 7, 11 |
| Паритет с DXF | § 4 (матрица), § 10 (критерии) |
| Модули 300–500 строк, одна ответственность | § 5 (принцип 1, 2), § 6 (структура) |
| Markdown-отчёт в `cad_source/zcad/velec/loadDWG/` | этот файл |
| Без кодинга | весь документ — только анализ и план |

---

## Приложение A. Перечень файлов с конкретными ссылками

- DXF загрузчик: `cad_source/zengine/fileformats/uzeffdxf.pas:1353` (`AddFromDXF`), `:1168` (`AddFromDXF20XX`), `:1385` (`ReadDXFHeader`), `:1203` (TABLES loop), `:1277` (BLOCKS loop), `:392` (`addentitiesfromdxf`).
- DXF поддержка: `cad_source/zengine/fileformats/uzeffdxfsupport.pas` (вся).
- DXF реестр сущностей: `cad_source/zengine/core/uzeentityfactory.pas:53` (`RegisterDXFEntity`), `:69` (`DXFName2EntInfoData`).
- DXF регистрация сущностей: `uzeentline.pas:747`, `uzeentcircle.pas:644`, `uzeentarc.pas:831`, `uzeentlwpolyline.pas:849`, `uzeentmtext.pas:854`, `uzeentpolylinegeneric.pas:392`, `uzeentspline.pas:442`, `uzeenthatch.pas:893`, `uzeentblockinsert.pas:472`, `uzeentdimensiongeneric.pas:222`, `uzeentellipse.pas:594`, `uzeentpoint.pas:292`, `uzeentsolid.pas:343`, `uzeent3dface.pas:372`, `uzeenttext.pas:537`.
- DWG обёртка LibreDWG: `cad_source/zengine/fileformats/uzefflibredwg.pas:68` (`addfromdwg`), `:105` (`addfromdxf`), `:140` (`ZCDWGParser` init).
- DWG конвертеры: `cad_source/zengine/fileformats/uzefflibredwg2ents.pas:39` (Layer), `:61` (LineType-stub), `:70` (BlockHeader-stub), `:78` (Block-stub), `:86` (Line + **Z-баг строки 93, 96**).
- DWG биндинг: `cad_source/components/fpdwg/dwg.pp` (~16285 строк, автогенерация), `dwgproc.pp:65` (`GDWGParser`), `:167` (`parseDwg_Data`).
- Регистрация форматов: `cad_source/zcad/register/uzcregfileformats.pas:72-80`.
- Заброшенный нативный парсер: `cad_source/zcad/trash/uzeffdwg.pas` (1437 строк).
- Связанные PR: #1033, #1035, #1037 (стабилизация h2pas-binding'а перед текущим этапом).

