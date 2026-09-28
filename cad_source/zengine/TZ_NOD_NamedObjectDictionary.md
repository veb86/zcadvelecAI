# ТЗ: поддержка Named Object Dictionary (NOD) в DXF-чтении/записи ZCAD

Issue: [#1440](https://github.com/veb86/zcadvelecAI/issues/1440) — «NOD. Добавить поддержку Named Object Dictionary».

Документ — только техническое задание. Код в рамках issue не пишется.
Реализация выполняется поэтапно, каждый этап — отдельный PR, после каждого
этапа чертёж должен открываться и сохраняться не хуже, чем до него.

Содержание:

1. [Проверка достоверности информации из issue](#1-проверка-достоверности-информации-из-issue)
2. [Текущее состояние кода](#2-текущее-состояние-кода)
3. [Границы задачи и жёсткие ограничения](#3-границы-задачи-и-жёсткие-ограничения)
4. [Целевая архитектура](#4-целевая-архитектура)
5. [Этапы реализации](#5-этапы-реализации)
6. [Тестирование](#6-тестирование)
7. [Риски и как их снять](#7-риски-и-как-их-снять)
8. [Глоссарий DXF-групп, используемых в ТЗ](#8-глоссарий-dxf-групп-используемых-в-тз)

---

## 1. Проверка достоверности информации из issue

Текст в issue в целом верен. Ниже — пункты, которые нужно уточнить, чтобы
не заложить ошибки в реализацию.

| Утверждение в issue | Оценка | Уточнение |
|---|---|---|
| NOD — корневой `DICTIONARY` в секции `OBJECTS`, первый объект секции | Верно | На практике AutoCAD всегда пишет NOD первым объектом `OBJECTS`. Но читатель **не должен** на это полагаться: NOD надо искать как `DICTIONARY` с `330=0` (владелец отсутствует). В шаблонах ZCAD NOD тоже первый (хэндл `C`). |
| У NOD `330=0` | Верно | Это единственный признак «корня». У всех прочих объектов `330` указывает на владельца. |
| Пары `3` (имя) / `350` (хэндл) | Верно, с оговоркой | По DXF Reference `350` — soft-owner ссылка на объект записи, `360` — hard-owner ссылка. Словари с `280=1` (например, расширенные словари, в т. ч. `ACAD_XREC_ROUNDTRIP` → `360`, который пишет `uzeacadtable_dxf_write`) используют `360`. Парсер должен принимать и `350`, и `360` после `3` и сохранять исходный код при записи. |
| Хэндл NOD «40» (или любой пример из текста) | Не так | В файлах AutoCAD и в шаблонах ZCAD (`savetemplate2000.dxf`, `savetemplate2007.dxf`, `empty.dxf`) NOD имеет хэндл `C`. Хэндл в любом случае произвольный и в ZCAD **перенумеровывается** при сохранении (`OldHandele2NewHandle` в `uzeffdxfout.pas`). Жёстко привязываться к значению нельзя. |
| Группа `281` у словаря | Неточно | `280` — флаг hard owner (1 = словарь жёстко владеет записями), `281` — режим клонирования дублирующихся записей (`0` not applicable, `1` keep existing, `2` use clone, `3`–`5` xref-префиксы, `6` unmangle). Для NOD обычно `281=1`. |
| Стандартные ключи `ACAD_TABLESTYLE`, `ACAD_MLEADERSTYLE`, `ACAD_GROUP`, `ACAD_LAYOUT`, `ACAD_MATERIAL`, `ACAD_VISUALSTYLE`, `ACAD_SCALELIST`, `AcDbVariableDictionary` | Верно | Также часто встречаются `ACAD_MLINESTYLE`, `ACAD_PLOTSETTINGS`, `ACAD_PLOTSTYLENAME` (это `ACDBDICTIONARYWDFLT` — словарь со значением по умолчанию, группа `340`), `ACAD_COLOR`, `ACAD_DETAILVIEWSTYLE`, `ACAD_SECTIONVIEWSTYLE`, `ACAD_FIELDLIST`, `ACAD_WIPEOUT_VARS` и т. д. Полный список зависит от версии AutoCAD. |
| `ACDB_RECOMPOSE_DATA` — стандартный ключ NOD | Не так | Это не стандартный ключ. Встречается в отдельных файлах некоторых версий/продуктов. В ТЗ он рассматривается как «неизвестная ветка», которая должна сохраняться без изменений (этап 7). |
| NOD появился в R13 | Верно | NOD и секция `OBJECTS` появились в R13 (1994). Но **конкретные ключи** появились гораздо позже: `ACAD_TABLESTYLE` — AutoCAD 2005 (формат DXF 2004, `AC1018`), `ACAD_MLEADERSTYLE` — AutoCAD 2008 (`AC1021`/`AC1024`), `ACAD_SCALELIST` — 2008, `ACAD_VISUALSTYLE`/`ACAD_MATERIAL` — 2007. Для DXF R12 (`savedxf12`) NOD не существует вообще. |
| `AcDbVariableDictionary` | Верно | Содержит объекты `DICTIONARYVAR` — «переменные чертежа», которые не попали в HEADER: `CTABLESTYLE`, `CMLEADERSTYLE`, `CANNOSCALE` и др. Текущий стиль таблиц / мультивыносок хранится **именно тут**, это надо учитывать на этапах 5–6. |
| Плагины пишут свои ветки в NOD / XRECORD | Верно | AutoCAD сохраняет неизвестные `DICTIONARY`/`XRECORD` без изменений. Неизвестные **классы** объектов (не `DICTIONARY`/`XRECORD`) требуют записи в секции `CLASSES`, иначе AutoCAD их отбрасывает или превращает в proxy. |
| Владельцы и обратные ссылки | Дополнение | Объект, принадлежащий словарю, ссылается на него через `330` **и** через `102 {ACAD_REACTORS 330 … 102 }`. Если при записи не выдержать это соответствие, AutoCAD выдаёт ошибки AUDIT/RECOVER. |
| PURGE удаляет неиспользуемые записи NOD | Неточно | PURGE работает с отдельными объектами (стилями таблиц, мультивыносок и т. п.), а не с NOD как таковым. Стандартные записи (например, `Standard` у `ACAD_TABLESTYLE`) не удаляются. |

Вывод: модель данных должна быть **общей для любого словаря** (ключ → ссылка
на объект, `280`/`281`, владелец, реакторы) и **не зависеть** от конкретных
хэндлов и от наличия того или иного ключа в шаблоне.

---

## 2. Текущее состояние кода

### 2.1 Чтение (`cad_source/zengine/fileformats/uzeffdxf.pas`)

* `AddFromDXF` до разбора секций извлекает «сырые» секции текстовым проходом
  (`ExtractDxfRawSection`, строка ~103):
  `CLASSES` → `PDrawing^.RawClassesSection`, `OBJECTS` → `PDrawing^.RawObjectsSection`,
  `ENTITIES` → локальная строка (строки ~1880–1884).
* Дальше идут ещё несколько отдельных построчных пре-сканов для ACAD_TABLE
  (`ScanAcadTableRawEntities`, `ScanTableContinuationHandles`, `ScanTableBreakData`,
  `ScanTableRowStyleTypes`).
* `AddFromDXF20XX` (строка ~1650) разбирает `TABLES` (BLOCK_RECORD, DIMSTYLE,
  LAYER, LTYPE, STYLE, VPORT; APPID/UCS/VIEW пропускаются), `BLOCKS`, `ENTITIES`.
  **Секция `OBJECTS` не разбирается вообще** — она существует только как текст.
* Загрузка стилей таблиц **закомментирована** (строки ~1939–1944):

  ```pascal
  //ReadTableStylesFromDXFObjects(
  //  dwgCtx.PDrawing^.RawObjectsSection,
  //  dwgCtx.PDrawing^.DXFTableStyleTable);
  ```

  Отключено коммитом `f3f0721e1` («Отключил чтение табличных стилей - там баг,
  какой то. Нужен сильный ИИ»). Кроме того, вызов стоит **после** `fileCtx.Done`,
  то есть после загрузки `ENTITIES`, а `ACAD_TABLE` применяет стиль в
  `BuildGeometry` сразу при загрузке сущности (`ApplyDXFTableStyle` в
  `uzeacadtable_stylemanager.pas`). Даже если раскомментировать — стиль придёт
  слишком поздно.

### 2.2 Запись (`cad_source/zengine/fileformats/uzeffdxfout.pas`)

* `savedxf20XX` (строка ~538) построчно копирует шаблон
  (`savetemplate2000.dxf` / `savetemplate2007.dxf` / `empty.dxf`), перемапливая
  все хэндл-группы (`5`, `320`, `330`, `340`, `350`, `360`, `390`, `105`, `1005`)
  через `OldHandele2NewHandle` на новые значения `IODXFContext.handle`.
* 9 символьных таблиц перегенерируются из данных чертежа на `ENDTAB`, записи
  шаблона игнорируются (`IgnoredSource`) — **это работает и не трогается**.
* Для стилей таблиц в секции `OBJECTS` реализован отдельный «конечный автомат»:
  * `PreallocateTableStyleHandles` (строка ~612) выделяет на каждый стиль 3 хэндла
    (TABLESTYLE, его расширенный DICTIONARY, CELLSTYLEMAP) и заполняет
    `IODXFContext.TableStyleNameHandleMap`;
  * пара `3/ACAD_TABLESTYLE` в NOD запоминает `tablestyledicthandle` (строки ~1557–1583);
  * внутри словаря стилей пары шаблона `3/350` вырезаются и заменяются стилями
    чертежа, объекты `TABLESTYLE` шаблона пропускаются и заменяются
    `WriteTableStyleObjectToStream` (строка ~372) +
    `WriteCellStyleMapObjectsToStream` (строка ~292);
  * на `ENDSEC` секции `OBJECTS` стили дописываются, **только если
    `tablestyledicthandle>0`** (строки ~1705–1721).
* В `savetemplate2000.dxf` и `empty.dxf` **нет** ключа `ACAD_TABLESTYLE`
  (NOD `C`: `ACAD_GROUP D`, `ACAD_LAYOUT 1A`, `ACAD_MLINESTYLE 17`,
  `ACAD_PLOTSETTINGS 19`, `ACAD_PLOTSTYLENAME E`). Следствие: при сохранении в
  DXF 2000 и при копировании в буфер обмена стили таблиц **теряются молча**.
* В `savetemplate2007.dxf` есть `ACAD_TABLESTYLE 86` и один `TABLESTYLE`,
  но **нет `ACAD_MLEADERSTYLE`**.
* `RawObjectsSection` при сохранении **не используется**: всё содержимое
  `OBJECTS` исходного файла, кроме того, что знает ZCAD, теряется.
* Расширения записи через колбэки: `RegisterBeforeSaveDxfProc`,
  `RegisterClassesSaveDxfProc`, `RegisterObjectsSaveDxfProc` (строки ~54–56).
  Ими пользуются `uzeacadtable_dxf_write` (round-trip объекты ACAD_TABLE,
  классы), `uzeacadtable_model` (разбиение таблиц), `uzeentacdproxy`.
* `$HANDSEED` патчится в конце по позиции `handlepos`.

### 2.3 Модели стилей

* `cad_source/zengine/styles/uzestylestablesdxf.pas`:
  `TGDBDXFTableStyle` / `GDBDXFTableStyleArray` (`AddStyle`, `GetStyleByHandle`).
  Разбор — текстовыми функциями `ReadTableStylesFromDXFObjects`,
  `ExtractTableStyleDictionary` (ищет `3/ACAD_TABLESTYLE` глобально по всему
  тексту, а не внутри NOD), `ParseTableStyleObject`.
  `WriteTableStylesToDXFObjects` генерирует хэндлы с `$F000` — возможна
  коллизия с реальными хэндлами чертежа; в текущем writer не используется.
* `cad_source/zengine/styles/uzestylesmleaderdxf.pas`:
  `TGDBDXFMLeaderStyle` / `GDBDXFMLeaderStyleArray`,
  `ReadMLeaderStylesFromDXFObjects` / `WriteMLeaderStylesToDXFObjects`.
  **Нигде не подключён**. Использует `uzcinterface` (модуль слоя zcad) —
  нарушение слоёв: zengine не должен зависеть от zcad. Сущности
  `MULTILEADER` в ZCAD нет.
* `cad_source/zengine/core/drawings/uzedrawingsimple.pas`:
  поле `DXFTableStyleTable: GDBDXFTableStyleArray` есть, но
  **в конструкторе `TSimpleDrawing.init` не вызывается `DXFTableStyleTable.init`,
  а в деструкторе `done` — `DXFTableStyleTable.done`** (другие таблицы —
  `DimStyleTable.init(100)`, `TableStyleTable.init(10)` и т. д. — инициализируются).
  Работа с неинициализированным `GZVector` — вероятная причина «бага, какого-то»
  из коммита `f3f0721e1`.

### 2.4 Потребители стилей таблиц

`uzeacadtable_stylemanager` (`ApplyDXFTableStyle` по хэндлу `342`,
`ApplyDXFTableStyleByName`), `uzeacadtable_model` (`BuildGeometry`),
`uzeacadtable_dxf_write` (хэндл `342` через `TableStyleNameHandleMap`),
`uzcregacadtable` (список стилей в инспекторе), `uzccommand_adddxftablestyle`,
GUI `uzcui_tablestylemanager`, `uzcftablestyles`, `uzcftablestylecreate`,
`uzvspreadsheet_cmdcreateacadtable`. Их интерфейс (`GetDXFTableStyleTable`,
`TGDBDXFTableStyle`) на этапах 1–5 **не меняется**.

### 2.5 История неудачных попыток (учитывать!)

* `235736137` — откат изменений `uzedrawingsimple`/`uzeffdxf`/`uzeffdxfout`:
  запись стилей таблиц «сломала систему сохранения»; требуется логика, «котрая
  не будет ломать систему сохранения». Эталон: `cad_source/test/tablestyleetalon.dxf`.
* `1f840bd6a` — откат «фабрики стилей» (`uzestylesfactory.pas`,
  `RegisterDXFStyle`, `uzestyleslayerdxf.pas`), которая переводила на общий
  механизм в том числе слои. **Символьные таблицы на общий механизм не переводятся.**

---

## 3. Границы задачи и жёсткие ограничения

1. **9 символьных таблиц** (`VPORT`, `LTYPE`, `LAYER`, `STYLE`, `VIEW`, `UCS`,
   `APPID`, `DIMSTYLE`, `BLOCK_RECORD`) — существующее чтение/запись
   **остаются как есть**. Ни один этап не меняет их код в `uzeffdxf.pas` /
   `uzeffdxfout.pas`, кроме переноса строк без изменения поведения.
2. На NOD переводятся **только** `ACAD_TABLESTYLE` и `ACAD_MLEADERSTYLE`.
   Остальные ключи NOD (`ACAD_GROUP`, `ACAD_LAYOUT`, `ACAD_MLINESTYLE`,
   `ACAD_PLOTSETTINGS`, `ACAD_PLOTSTYLENAME`, `ACAD_MATERIAL`,
   `ACAD_VISUALSTYLE`, `ACAD_COLOR`, `AcDbVariableDictionary`) продолжают
   писаться из шаблона, как сейчас.
3. Никакой «универсальной фабрики стилей» для слоёв/типов линий и т. п.
4. Новая логика живёт в `zengine` и не зависит от модулей `zcad`
   (`uzcinterface`, `uzcdrawings` и т. п.).
5. Формат шаблонов DXF не меняется обязательно — writer должен уметь
   **добавить** отсутствующий ключ в NOD, а не требовать правки шаблона.
6. Любое изменение формата записи проверяется открытием результата в AutoCAD
   (или хотя бы ODA File Converter / `AUDIT`), см. раздел 6.
7. Логирование — через `programlog` с уровнем `LM_Info`/`LM_Debug`,
   детальные трассы — выключены по умолчанию (отдельный флаг/уровень).
8. DWG-загрузчик (`fileformats/dwg/`) в рамках этапов 0–8 не трогается.

---

## 4. Целевая архитектура

```
                 ┌────────────────────────── zengine ─────────────────────────────┐
  DXF файл ──►  │ uzeffdxf.AddFromDXF                                            │
                 │   ├─ ExtractDxfRawSection('OBJECTS')  (как сейчас)             │
                 │   ├─ [НОВОЕ] NOD pre-pass: ParseDxfObjectsSection ──► TZNODModel │
                 │   │        └─ NOD-обработчики (реестр):                         │
                 │   │             ACAD_TABLESTYLE   → DXFTableStyleTable          │
                 │   │             ACAD_MLEADERSTYLE → DXFMLeaderStyleTable        │
                 │   ├─ AddFromDXF20XX (TABLES/BLOCKS/ENTITIES — как сейчас)      │
                 │   └─ стили уже загружены → ACAD_TABLE.BuildGeometry их видит   │
                 │                                                                │
  DXF файл ◄──  │ uzeffdxfout.savedxf20XX                                        │
                 │   ├─ символьные таблицы — как сейчас                           │
                 │   ├─ [НОВОЕ] перед ENTITIES: NOD-обработчики резервируют хэндлы│
                 │   └─ OBJECTS: [НОВОЕ] NOD writer                               │
                 │        ├─ ключи шаблона → копируются/перемапливаются           │
                 │        ├─ ключи «своих» обработчиков → генерируются из модели  │
                 │        └─ сохранённые неизвестные ветки → выводятся из модели  │
                 └────────────────────────────────────────────────────────────────┘
```

### 4.1 Новые модули (все в `cad_source/zengine/fileformats/`)

| Модуль | Назначение |
|---|---|
| `uzeffdxfobjects.pas` | Лёгкий разборщик секции `OBJECTS` в список «сырых объектов» `TZDXFRawObject` (тип `0`, хэндл `5`, владелец `330`, реакторы, xdictionary, список пар group/value). Никакой предметной логики. |
| `uzeffdxfnod.pas` | Модель NOD: `TZDXFDictionary` (хэндл, владелец, `280`, `281`, упорядоченный список записей `ключ → хэндл + код 350/360`), поиск NOD (`DICTIONARY` c `330=0`), навигация по пути (`NOD/ACAD_TABLESTYLE/Standard`). |
| `uzeffdxfnodregistry.pas` | Реестр NOD-обработчиков: `RegisterNODHandler(Key, ObjectType, LoadProc, ReserveHandlesProc, SaveProc, …)`. |
| `uzestylestablesdxfnod.pas` (в `styles/`) | NOD-обработчик `ACAD_TABLESTYLE` поверх существующего `TGDBDXFTableStyle`. |
| `uzestylesmleaderdxfnod.pas` (в `styles/`) | NOD-обработчик `ACAD_MLEADERSTYLE` поверх `TGDBDXFMLeaderStyle`. |

Названия — рекомендация; допускается объединение модулей, если это не
нарушает слои.

### 4.2 Модель данных (описание, не код)

**`TZDXFRawObject`** — один объект секции `OBJECTS`:

* `ObjType: string` — значение группы `0` (`DICTIONARY`, `TABLESTYLE`, `XRECORD`, …);
* `Handle: TDWGHandle` — группа `5`;
* `OwnerHandle: TDWGHandle` — группа `330` вне блока реакторов;
* `Reactors: array of TDWGHandle` — `102 {ACAD_REACTORS … }`;
* `XDictHandle: TDWGHandle` — `102 {ACAD_XDICTIONARY 360 … }`;
* `Pairs` — полный упорядоченный список пар `(code, value)` объекта без
  изменения (нужен для round-trip неизвестных объектов).

**`TZDXFDictionary`** — представление объекта `DICTIONARY` поверх `TZDXFRawObject`:

* `HardOwner` (`280`), `CloningFlag` (`281`);
* `Entries` — упорядоченный список `TZDXFDictEntry = (Key, TargetHandle, OwnershipCode: 350|360)`;
* функция поиска записи по ключу (регистр — как в AutoCAD: ключи
  сравниваются без учёта регистра, но сохраняются как есть).

**`TZNODModel`** — результат разбора:

* `Objects` — все сырые объекты, индекс `хэндл → объект`;
* `NOD` — ссылка на корневой словарь (или `nil`, если его нет: R12 / битый файл);
* `ClaimedHandles` — множество хэндлов объектов, которые «забрал» какой-либо
  обработчик (они не будут выводиться как неизвестные на этапе 7).

Модель хранится в загрузочном контексте (`TIODXFLoadContext`) на время
загрузки; на этапе 7 её часть (неизвестные ветки) переезжает в чертёж.

### 4.3 Контракт NOD-обработчика

Для каждого ключа NOD (`ACAD_TABLESTYLE`, `ACAD_MLEADERSTYLE`) регистрируется:

| Процедура | Когда вызывается | Что делает |
|---|---|---|
| `LoadProc(const Model: TZNODModel; const Dict: TZDXFDictionary; var Drawing)` | Чтение, до `ENTITIES` | Для каждой записи словаря находит объект по хэндлу, проверяет `ObjType`, переносит данные в таблицу стилей чертежа, запоминает исходный хэндл (`DXFHandle`) для резолва `342`/`340` из сущностей. Помечает хэндлы как «забранные». |
| `ReserveHandlesProc(var Drawing; var IODXFContext)` | Запись, **до** `ENTITIES` (там, где сейчас `PreallocateTableStyleHandles`) | Выделяет хэндлы для словаря-ветки и всех объектов через `IODXFContext.handle`, заполняет карты имя → хэндл (`TableStyleNameHandleMap`, аналогичная для MLeader). Идемпотентна. |
| `SaveProc(var outstream; var Drawing; var IODXFContext; DictHandle, NODHandle)` | Запись, секция `OBJECTS` | Пишет `DICTIONARY` ветки (`330`=NOD, реактор NOD, `281=1`, пары `3/350`) и все объекты ветки (с `330`=словарь ветки, `102 {ACAD_REACTORS}`, xdictionary и т. п.). |
| `ClassesProc` (опционально) | Запись, секция `CLASSES` | Объявляет классы, если их нет в шаблоне (`TABLESTYLE`, `MLEADERSTYLE`, `CELLSTYLEMAP`). |
| `MinVersion` | Запись | Минимальная версия DXF, в которой ключ пишется. Для ниже — ключ **не пишется**, выдаётся предупреждение в лог (см. 5.4). |
| `DefaultNames` | Чтение/запись | Имя обязательной записи (`Standard`), которая создаётся, если стилей нет. |

### 4.4 Где хранится хэндл «текущего» стиля

`CTABLESTYLE`/`CMLEADERSTYLE` лежат в `AcDbVariableDictionary` как
`DICTIONARYVAR` (группа `1` — имя стиля). На этапах 5–6 обработчик может
читать их, но **запись** `AcDbVariableDictionary` остаётся шаблонной, пока
не будет отдельного решения (см. раздел 7, риск R6).

---

## 5. Этапы реализации

Каждый этап: отдельный PR, свои тесты, критерии приёмки. Порядок важен.

### Этап 0. Подготовка и исправление явных дефектов

Цель: убрать причины падений до любой новой архитектуры.

1. `TSimpleDrawing.init`: добавить инициализацию `DXFTableStyleTable`
   (по аналогии с `TableStyleTable.init(10)`); `TSimpleDrawing.done` —
   финализацию. Проверить, что `GDBDXFTableStyleArray.done` освобождает
   вложенные `CellFormats` каждого стиля.
2. Проверить все места, где `DXFTableStyleTable` используется до загрузки
   (GUI, команды), на корректную работу с пустой таблицей.
3. Вынести `uzestylesmleaderdxf.pas` из зависимости от `uzcinterface`
   (заменить на `uzclog`/`uzbLogIntf`, как в `uzestylestablesdxf.pas`).
4. Зафиксировать в тестах **текущее** поведение сохранения (golden-файлы
   `savetemplate2000`/`2007` без таблиц и с таблицей из
   `cad_source/test/tablestyleetalon.dxf`), чтобы последующие этапы
   сравнивались с ним.

Приёмка: существующие тесты `cad_source/zengine/tests` проходят;
новые тесты фиксируют текущий вывод; утечек по `heaptrc` при
открытии/закрытии чертежа нет.

**Статус: выполнен (issue #1442).**

1. `TSimpleDrawing.init` вызывает `DXFTableStyleTable.init(10)`,
   `TSimpleDrawing.done` — `DXFTableStyleTable.Done`. Раньше поле не
   инициализировалось вовсе, а `TZCADDrawingsManager.CreateDWG` выделяет
   чертёж через `Getmem` без обнуления, поэтому таблица содержала мусор
   (воспроизведение: `AddStyle` на чертеже в «грязной» памяти →
   `EAccessViolation`). `GDBDXFTableStyleArray.done`
   (`GZVectorPData.done`) вызывает `done` каждого стиля, а
   `TGDBDXFTableStyle.Done` освобождает `CellFormats` и строки — проверено
   тестом по `GetFPCHeapStatus` (без `DXFTableStyleTable.Done` теряется
   5952 байта на 20 стилей) и `heaptrc`.
2. Потребители (`uzcregacadtable`, `uzccommand_adddxftablestyle`,
   `uzeacadtable_stylemanager`, `uzeacadtable_dxf_write`,
   `uzvspreadsheet_cmdcreateacadtable`, `uzcui_tablestylemanager`,
   `uzcftablestyles`, `uzcftablestylecreate`, `uzeffdxfout`) обходят
   таблицу через `beginiterate`/`count`/`AddStyle` и после `init` корректно
   работают с пустой таблицей. Найдено (не исправлено, вне этапа 0):
   удаление стиля в GUI (`uzcui_tablestylemanager.pas`,
   `uzcftablestyles.pas`) вызывает `RemoveDataFromArray`, который только
   убирает указатель из массива, без `Done`/`Freemem`, — удалённый стиль
   остаётся в памяти до выхода. Исправлять вместе с переводом стилей на NOD
   (этап 5); при исправлении учесть, что `ListView` читает `Item.Data`.
3. `uzestylesmleaderdxf.pas` больше не использует `uzcinterface`: сообщение
   о пропущенном стиле с блочным содержимым выводится через
   `programlog.LogOutFormatStr(..., LM_Warning, 1, MO_SH)` — флаг `MO_SH`
   отправляет его в историю команд (`uzcreglog.TLogerMBoxBackend`), как
   раньше `zcUI.TextMessage(..., TMWOHistoryOut)`.
4. Тест `cad_source/zengine/tests/nodstage0.lpr` (сборка без IDE:
   `experiments/issue1442/build_and_run_nodstage0.sh`, перезапись
   эталонов — ключ `--update`) сравнивает вывод `savedxf20XX` с
   эталонами `cad_source/zengine/tests/data/nod/golden/` (значения
   `$TDCREATE`/`$TDUCREATE`/`$TDUPDATE`/`$TDUUPDATE` заменяются на
   `<time>`). Зафиксированное текущее поведение:
   - `empty_2000/2007` — пустой чертёж; в 2007 из шаблона пишется один
     стиль `Standard`;
   - `tablestyles_2000/2007` — чертёж с двумя стилями в
     `DXFTableStyleTable` (`Standard`, `ZCAD1442`): в 2007 оба пишутся в
     `ACAD_TABLESTYLE`, в 2000 стили **молча теряются** (в шаблоне нет
     словаря `ACAD_TABLESTYLE`, добавляется только класс `CELLSTYLEMAP`);
   - `tablestyleetalon_2000/2007` — загрузка `tablestyleetalon.dxf`
     (стили `aits`, `Standard`, `vebts`) и сохранение: пользовательские
     стили **теряются**, так как чтение `TABLESTYLE` отключено
     (`uzeffdxf.pas`, закомментированный вызов
     `ReadTableStylesFromDXFObjects`); в 2007 остаётся `Standard` из
     шаблона.
   Изменение эталонов на следующих этапах — только осознанное, с
   объяснением в PR.

`heaptrc` (`HEAPTRC=1 experiments/issue1442/build_and_run_nodstage0.sh`):
после всех проверок, включая загрузку/сохранение `tablestyleetalon.dxf`,
не освобождены только 2 блока глобальных реестров форматов
(`uzeffmanager`, секция `initialization`) — к чертежу не относятся.

### Этап 1. Модель OBJECTS/NOD (только чтение, без побочных эффектов)

1. Реализовать `uzeffdxfobjects.pas`: разбор секции `OBJECTS` из
   `RawObjectsSection` (или из `TZMemReader` по диапазону секции, если это
   проще и быстрее) в список `TZDXFRawObject`.
   * Корректно обрабатывать блоки `102 { … 102 }` (реакторы, xdictionary,
     произвольные прикладные блоки).
   * Хэндлы нормализовать тем же способом, что `NormalizeHandle` в `uzeffdxf.pas`.
   * Поддерживать как `\r\n`, так и `\n`; пробелы вокруг кодов групп.
2. Реализовать `uzeffdxfnod.pas`: построить `TZDXFDictionary` для всех
   `DICTIONARY`, найти NOD (`330=0`; если таких несколько — первый, с
   предупреждением в лог).
3. Заменить `ExtractTableStyleDictionary` (глобальный поиск
   `3/ACAD_TABLESTYLE`) на поиск **внутри NOD** — пока только в тестах, без
   подключения к загрузке.
4. Детальный лог разбора (количество объектов, ключи NOD, время) —
   за отключённым по умолчанию флагом.

Приёмка: unit-тесты разбора на `tablestyleetalon.dxf`, шаблонах 2000/2007,
синтетических файлах (NOD не первый; NOD без `ACAD_TABLESTYLE`; `360` вместо
`350`; неизвестный ключ; `ACDB_RECOMPOSE_DATA`). Поведение загрузки
чертежа **не изменилось** (модель ещё не подключена).

**Статус: выполнен (issue #1444).**

1. `cad_source/zengine/fileformats/uzeffdxfobjects.pas` —
   `ParseDxfObjectsSection(Text, Objects, out Error)`: разбор текста
   секции `OBJECTS` (формат `RawObjectsSection`: с `0/SECTION`, `2/OBJECTS`
   или без них) одним проходом, без `TStringList`, в `TZDXFRawObject`
   (`ObjType`, `Handle`, `OwnerHandle`, `HasOwnerGroup`, `Reactors`,
   `XDictHandle`, все пары `Pairs` в исходном порядке, `LineNumber`).
   * Переводы строк `\r\n`, `\n`, одиночный `\r`; пробелы вокруг кодов
     групп; UTF-8 BOM; разбор заканчивается на `0/ENDSEC` или `0/EOF`.
   * Блоки `102 {… 102 }` отслеживаются, реакторы берутся из
     `{ACAD_REACTORS`, xdictionary — из `{ACAD_XDICTIONARY`, прочие блоки
     только сохраняются в `Pairs`. Владелец (`330` вне блоков 102),
     реакторы и xdictionary берутся **только до первой группы `100`**: после
     маркера подкласса начинаются данные объекта (например, `330`/`360`/`102`
     в `XRECORD`), и они не принимаются за заголовок.
   * `NormalizeDXFHandleStr` — тот же алгоритм, что `NormalizeHandle`
     (`uzeffdxf.pas`, в интерфейс не вынесен, модуль не менялся);
     `TryDXFStrToHandle`/`DXFHandleToStr` — хэндл как `TDWGHandle`.
   * Нарушенная структура (код группы — не число, пара без значения) —
     `False` и описание с номером строки; объекты до ошибки сохраняются.
2. `cad_source/zengine/fileformats/uzeffdxfnod.pas` — `TZNODModel`:
   объекты, индекс по хэндлу (повторяющийся хэндл — предупреждение, в
   индекс попадает первый объект), `TZDXFDictionary` для каждого
   `DICTIONARY` и `ACDBDICTIONARYWDFLT` (записи `3` + `350`/`360` с
   сохранением кода владения, `280`, `281`, `340`; ключ без хэндла
   пропускается), NOD — первый `DICTIONARY` с `330=0` (остальные корневые
   словари — предупреждение в общий лог, `RootDictionaryCount`),
   `ResolvePath('ACAD_TABLESTYLE/Standard')`/`ResolveDictionary` (ключи без
   учёта регистра), `ClaimHandle`/`IsHandleClaimed` для будущих
   обработчиков (этап 3).
3. `cad_source/zengine/styles/uzestylestablesdxfnod.pas` —
   `ExtractTableStyleDictionaryFromNOD(Model, StyleNameByHandle, out
   DictHandle)`: замена `ExtractTableStyleDictionary`, формат карты тот
   же (`хэндл=имя`), но словарь берётся только из `NOD/ACAD_TABLESTYLE`.
   Используется только в тестах; `uzeffdxf.pas`, `uzeffdxfout.pas` и
   `uzestylestablesdxf.pas` не менялись — поведение загрузки и сохранения
   прежнее (эталоны этапа 0 совпадают).
4. `cad_source/zengine/fileformats/uzeffdxfnodlog.pas` — модуль лога `NOD`,
   зарегистрирован выключенным. Трасса (количество объектов и словарей,
   корневые словари, время разбора, ключи NOD, битые ссылки) включается
   ключом `lem NOD`; предупреждения (несколько корневых словарей,
   повторяющиеся хэндлы, ошибка разбора) пишутся в общий лог всегда.
   Новые модули не зависят от модулей слоя zcad (кроме `uzclog`, как
   `uzedwglog`/`uzeentproxylog`) и от `uzeffdxf`.
5. Тест `cad_source/zengine/tests/nodstage1.lpr` (сборка без IDE:
   `experiments/issue1444/build_and_run_nodstage1.sh`):
   - `tablestyleetalon.dxf` (41 объект), `savetemplate2000.dxf` (11),
     `savetemplate2007.dxf` (39), `empty.dxf` (11): NOD = `C`, ключи NOD,
     целостность ссылок (владелец каждого объекта существует, объект записи
     словаря принадлежит словарю), `ACAD_TABLESTYLE` = `86`
     (`BE=aits, 87=Standard, BA=vebts` в эталоне; `87=Standard` в шаблоне
     2007; нет в 2000). На эталоне результат совпадает со старым
     `ReadTableStylesFromDXFObjects`;
   - синтетические файлы `cad_source/zengine/tests/data/nod/nod_*.dxf`
     (генератор `experiments/issue1444/gen_nod_testdata.py`):
     `nod_not_first` — NOD четвёртый, перед ним сторонний словарь с ключом
     `ACAD_TABLESTYLE` (старый глобальный поиск находит его и теряет оба
     стиля, NOD-поиск — нет); `nod_no_tablestyle`; `nod_hardowner_360` —
     `280=1`, записи `360`, `\r\n`, пробелы у кодов, хэндлы `00c`/`0040`/`d`,
     xdictionary у `TABLESTYLE`; `nod_unknown_keys` — `ACDB_RECOMPOSE_DATA`,
     ветка плагина с `XRECORD` (в данных `330`, `360`, блок 102), битая
     ссылка, ключ без хэндла, `ACDBDICTIONARYWDFLT`; `nod_multiple_roots`;
     `nod_broken` — ошибка структуры;
   - одинаковая модель при `\n` и `\r\n`;
   - загрузка `tablestyleetalon.dxf` через `AddFromDXF`: `RawObjectsSection`
     даёт ту же модель, что секция файла, `DXFTableStyleTable` пуста, как и
     до этапа 1;
   - модуль лога `NOD` выключен по умолчанию, при включении все сообщения
     форматируются без ошибок.

   `heaptrc`: как и в `nodstage0`, не освобождены только 2 блока реестров
   форматов `uzeffmanager`.

### Этап 2. Подключение NOD pre-pass к чтению DXF

1. В `AddFromDXF` сразу после `ExtractDxfRawSection('OBJECTS')` и **до**
   `AddFromDXF20XX` строить `TZNODModel` и класть в `TIODXFLoadContext`.
2. Для R12 (`AddFromDXF12`) — модель пустая, pre-pass не вызывается.
3. Ошибка разбора `OBJECTS` не должна прерывать загрузку чертежа: лог +
   пустая модель.
4. Удалить закомментированный вызов `ReadTableStylesFromDXFObjects` после
   `fileCtx.Done` (его заменит обработчик на этапе 5).

Приёмка: время загрузки больших DXF (`dxfloadbench.cmd`) выросло не более
чем на оговорённый процент (предложение — 5 %); на всех тестовых файлах
модель строится без ошибок.

**Статус: выполнен (issue #1446).**

1. `TIODXFLoadContext.NODModel` (`uzeffdxfsupport.pas`) — модель
   `OBJECTS`/NOD загружаемого файла; владеет контекст (освобождается в
   `Done`). `nil` — контекст создан не `AddFromDXF` (например, собственный
   контекст `AddFromDXF12`).
2. `AddFromDXF` (`uzeffdxf.pas`) после `ExtractDxfRawSection('OBJECTS')` и
   сканеров `ACAD_TABLE` создаёт пустую модель; для DXF 2000+
   (`AC1014`…`AC1032`) до `AddFromDXF20XX` выполняется pre-pass
   `DXFNODPrePass` → `BuildDXFNODModel(RawObjectsSection, Model, out
   Error)`. Для R12 (`AC1009`) и неизвестных версий pre-pass не вызывается,
   модель остаётся пустой (даже если в файле R12 есть секция `OBJECTS`).
3. Ошибка разбора `OBJECTS` (как и исключение внутри разбора) загрузку не
   прерывает: `BuildDXFNODModel` возвращает `False` и описание ошибки,
   модель очищается (частично разобранные объекты не используются),
   предупреждение с именем файла пишется в общий лог. Трасса pre-pass
   (объекты, словари, найден ли NOD, время) — в модуле лога `NOD`
   (`lem NOD`). Контекст загрузки вместе с моделью теперь освобождается и
   при исключении разбора чертежа (`try … finally fileCtx.Done`): раньше на
   файлах, которые не загружаются (`cad_source/test/empty_notwork*.dxf`),
   он терялся.
4. Закомментированный вызов `ReadTableStylesFromDXFObjects` после
   `fileCtx.Done` удалён, как и ставшая ненужной ссылка `uzeffdxf` на
   `uzestylestablesdxf`.
5. `DXFNODModelBuiltProc` — временная точка наблюдения (вызывается сразу
   после pre-pass, в том числе для R12 с пустой моделью), нужна тестам
   этапа 2; на этапе 3 её заменит реестр `uzeffdxfnodregistry`.
   Сохранение не менялось: эталоны этапа 0 совпадают.
6. Время. Секции `OBJECTS` обычных чертежей малы: для файла
   `dxfloadbench.cmd` (`zcadelectrotech/data/examples/test_dxf/ops.dxf`,
   7,5 МБ, `OBJECTS` 6 КБ) pre-pass занимает < 1 мс при загрузке ≈ 2,4 с
   (0 %). У файлов с большими таблицами почти вся секция `OBJECTS` —
   содержимое `ACAD_TABLE` (сотни тысяч пар в нескольких объектах), и
   модель этапа 1 хранит все пары: `cad_source/test/bugbreaktable.dxf`
   (20 МБ, `OBJECTS` 14,4 МБ) — pre-pass 337 мс при загрузке 6,1 с
   (5,5 %), `tableheighttextbug.dxf` (10,7 МБ, `OBJECTS` 9 МБ) — 192 мс при
   3,6 с (5,3 %). Чтобы приблизиться к 5 %, разбор этапа 1 ускорен
   примерно на 30 % без изменения результата (код группы разбирается без
   выделения строки, `Trim` значений — только для кодов заголовка
   объекта); до этого было 450 мс (7,2 %) и 270 мс (7,4 %). Оставшееся
   время — выделение и хранение строк значений пар (≈ 1 млн пар); сократить
   его можно только отложенным хранением пар (ссылки на текст секции вместо
   копий) — при необходимости это отдельная задача.
7. Тест `cad_source/zengine/tests/nodstage2.lpr` (сборка без IDE:
   `experiments/issue1446/build_and_run_nodstage2.sh`):
   - `BuildDXFNODModel`: эталон — NOD `C`, `ACAD_TABLESTYLE` = `86`;
     `nod_broken.dxf` — `False`, ошибка с номером строки, модель пустая;
     пустой текст — пустая модель без ошибки; быстрый разбор кодов групп
     совпадает с `TryStrToInt` (пробелы, табуляция, `-3`, `+5`, 10 цифр,
     выход за `Int64`, `-`);
   - загрузка `tablestyleetalon.dxf`: точка наблюдения вызвана один раз, в
     момент pre-pass сущностей 0, модель контекста совпадает с моделью
     секции `OBJECTS` файла, `DXFTableStyleTable` пуста (этап 5);
   - синтетические полные DXF `cad_source/zengine/tests/data/nod/nod_load_*.dxf`
     (генератор `experiments/issue1446/gen_nod_load_testdata.py`, в каждом
     `LINE`): `nod_load_r12` — `AC1009` с корректной секцией `OBJECTS`,
     модель пустая; `nod_load_broken_objects` — `AC1015`, код группы
     `99999999999999999999` (основной читатель его пропускает, разбор
     `OBJECTS` — ошибка), модель пустая, отрезок загружен;
     `nod_load_no_objects` — без `OBJECTS`, модель пустая, отрезок загружен;
   - все тестовые DXF (`cad_source/test`, эталоны этапа 0, шаблоны
     2000/2007 и `empty.dxf`, 63 файла): для 61 файла DXF 2000+ модель
     строится без ошибок, NOD найден, модель совпадает с моделью секции
     файла, pre-pass идёт до сущностей (в 22 файлах сущности загружены
     после него); 2 файла `empty_notwork*.dxf` не загружаются и без
     pre-pass (исключение `TZMemReader`) и пропускаются;
   - модуль лога `NOD` выключен по умолчанию, при включении трасса pre-pass
     форматируется без ошибок;
   - `--bench` — замер из п. 6 (провал, если на `ops.dxf` доля pre-pass
     больше 5 %).

   `heaptrc`: объектов модели NOD среди неосвобождённых блоков нет;
   остаются блоки, не связанные с NOD (реестр форматов `uzeffmanager`,
   модель `ACAD_TABLE`, локальные данные `AddFromDXF20XX` при исключении на
   `empty_notwork*.dxf`).

### Этап 3. Реестр NOD-обработчиков

1. `uzeffdxfnodregistry.pas`: регистрация по ключу NOD, контракт — раздел 4.3.
2. Чтение: после построения модели для каждого зарегистрированного ключа,
   найденного в NOD, вызывается `LoadProc`. Незарегистрированные ключи —
   пропускаются (до этапа 7).
3. Запись: `savedxf20XX` на месте `PreallocateTableStyleHandles` вызывает
   `ReserveHandlesProc` всех обработчиков; на `CLASSES` — `ClassesProc`;
   в `OBJECTS` — `SaveProc` (см. этап 4).
4. Регистрация обработчиков — в `initialization` соответствующих модулей
   (как `RegisterObjectsSaveDxfProc` в `uzeacadtable_dxf_write`).
5. Порядок вызова обработчиков фиксирован (порядок регистрации) —
   чтобы хэндлы в файле были детерминированы.

Приёмка: реестр с одним тестовым обработчиком-заглушкой проходит round-trip
(загрузка → сохранение), выходной файл совпадает с эталоном этапа 0.

### Этап 4. NOD writer вместо ad-hoc автомата `ACAD_TABLESTYLE`

Это самый рискованный этап; он меняет только логику секции `OBJECTS`.

1. При проходе шаблона в `OBJECTS`:
   * объект NOD шаблона (первый `DICTIONARY` с `330=0`) выводится с
     перемапленными хэндлами, **но** его пары `3/350` для ключей,
     у которых есть зарегистрированный обработчик, заменяются на хэндлы
     из `ReserveHandlesProc`;
   * если ключа обработчика нет в NOD шаблона (DXF2000, `empty.dxf`,
     `ACAD_MLEADERSTYLE` в 2007) — пара `3/<ключ>` + `350/<хэндл>`
     **добавляется** в NOD (с сохранением алфавитного порядка ключей, как
     делает AutoCAD — это не обязательно, но облегчает сравнение файлов),
     при условии `Ver >= MinVersion`;
   * ветка шаблона для ключа обработчика (словарь и его объекты)
     пропускается целиком — по множеству хэндлов, полученному заранее
     разбором шаблона той же моделью этапа 1 (**не** по состоянию
     «конечного автомата» с `intablestyledict`/`lasthandle`);
   * хэндлы пропущенных объектов шаблона всё равно регистрируются в
     `OldHandele2NewHandle`, если на них есть ссылки из других объектов
     шаблона (например, `CTABLESTYLE` → `DICTIONARYVAR`), иначе ссылки
     должны быть перенаправлены на стиль `Standard` чертежа.
2. Перед `ENDSEC` секции `OBJECTS` вызываются `SaveProc` обработчиков,
   затем `RunObjectsSaveDxfProcs` (как сейчас).
3. Удалить из `savedxf20XX` переменные/ветки автомата
   (`tablestyledicthandle`, `intablestyledict`, `writtenstylecount` и т. п.),
   перенеся запись `TABLESTYLE`/`CELLSTYLEMAP`
   (`WriteTableStyleObjectToStream`, `WriteCellStyleMapObjectsToStream`)
   в обработчик `ACAD_TABLESTYLE` (этап 5). До этапа 5 они вызываются из
   временного обработчика-адаптера, чтобы вывод не изменился.
4. `$HANDSEED` — как сейчас, через `handlepos`; все новые хэндлы — только из
   `IODXFContext.handle`. Хэндлы с `$F000` (`WriteTableStylesToDXFObjects`)
   запрещены, функцию пометить устаревшей и удалить на этапе 5.

Приёмка:

* на шаблоне 2007 вывод совпадает с эталоном этапа 0 (с точностью до
  хэндлов, если их порядок изменился — тогда сравнивать через нормализацию
  хэндлов);
* на шаблоне 2000 в NOD появляется `ACAD_TABLESTYLE`, стили таблиц
  сохраняются (сейчас теряются);
* результат открывается в AutoCAD без сообщений об ошибках, `AUDIT` — 0 ошибок.

### Этап 5. Перевод `ACAD_TABLESTYLE` на NOD

1. Обработчик `ACAD_TABLESTYLE`:
   * `LoadProc`: для каждой пары `3/350` словаря — объект `TABLESTYLE`,
     парсинг полей в `TGDBDXFTableStyle` (перенести логику
     `ParseTableStyleObject` на пары `TZDXFRawObject`, без повторного
     текстового сканирования); `DXFHandle` = исходный хэндл;
     `XDictHandle` = хэндл расширенного словаря; при наличии
     `CELLSTYLEMAP` — сохранить сырые данные для round-trip (issue #1409);
   * `ReserveHandlesProc` = нынешний `PreallocateTableStyleHandles`
     (TABLESTYLE + DICTIONARY + CELLSTYLEMAP на каждый стиль);
   * `SaveProc` = нынешние `WriteTableStyleObjectToStream` +
     `WriteCellStyleMapObjectsToStream` + запись словаря ветки
     (`330`=NOD, `102 {ACAD_REACTORS 330=NOD}`, `281=1`, пары `3/350`);
   * `ClassesProc`: класс `TABLESTYLE` (и `CELLSTYLEMAP`), если его нет в шаблоне;
   * `MinVersion` = DXF 2004 (`AC1018`). Для DXF 2000 (`AC1015`) —
     решение принимает владелец проекта (см. R4): либо писать
     (AutoCAD 2000 проигнорирует неизвестный класс), либо не писать с
     предупреждением.
2. Если после загрузки стилей нет — создать `Standard` с параметрами
   по умолчанию (как в `savetemplate2007.dxf`).
3. Резолв `342` у `ACAD_TABLE`: при загрузке — `GetStyleByHandle` по
   исходному хэндлу (работает, т. к. стили загружены до `ENTITIES`);
   при записи — `TableStyleNameHandleMap` (как сейчас).
4. Удалить `ReadTableStylesFromDXFObjects`, `ExtractTableStyleDictionary`,
   `WriteTableStylesToDXFObjects` из `uzestylestablesdxf.pas` после того,
   как тесты подтвердят эквивалентность.
5. Интерфейс для потребителей (`GetDXFTableStyleTable`, `TGDBDXFTableStyle`)
   не меняется.

Приёмка:

* `tablestyleetalon.dxf`: загрузка → стили в инспекторе объектов, таблица
  отрисовывается со стилем (высоты, выравнивание, цвета);
* сохранение 2007 → повторная загрузка → те же стили (round-trip тест);
* AutoCAD открывает результат, `TABLESTYLE` в диалоге стилей таблиц
  совпадает с исходным;
* существующие тесты ACAD_TABLE (`uzctacadtable`, `roundtrip1339`,
  `roundtrip1381`, `roundtrip1436`) проходят.

### Этап 6. Перевод `ACAD_MLEADERSTYLE` на NOD

1. Обработчик `ACAD_MLEADERSTYLE` на базе `uzestylesmleaderdxf.pas`
   (`TGDBDXFMLeaderStyle`), аналогично этапу 5.
2. В `TSimpleDrawing` добавить таблицу `DXFMLeaderStyleTable` (init/done!)
   и метод доступа по аналогии с `GetDXFTableStyleTable`.
3. Пока сущности `MULTILEADER` в ZCAD нет, цель этапа — **сохранение
   стилей мультивыносок без потерь** при открытии/сохранении DXF из AutoCAD.
   Ссылки на `LTYPE`/`STYLE`/`BLOCK_RECORD` внутри `MLEADERSTYLE`
   (группы `340`/`342`/`343`) при записи перемапливаются через карты
   хэндлов символьных таблиц (`TextStyleNameHandleMap`,
   `BlockNameHandleMap`, аналогичная для типов линий — добавить, если нет).
4. `MinVersion` = DXF 2007 (`AC1021`); класс `MLEADERSTYLE` — через `ClassesProc`.
5. Если шаблон 2007 не содержит `ACAD_MLEADERSTYLE` — ключ добавляется
   в NOD (механизм этапа 4). При отсутствии стилей — `Standard`.

Приёмка: DXF с мультивыносками из AutoCAD → открытие в ZCAD → сохранение →
AutoCAD видит все стили мультивыносок с прежними параметрами; тесты
round-trip.

### Этап 7. Сохранение неизвестных и собственных веток NOD

Цель: не терять данные сторонних приложений и позволить ZCAD хранить свои.

1. После `LoadProc` всех обработчиков все объекты, достижимые из NOD по
   **незарегистрированным** и **не шаблонным** ключам (например,
   `ACDB_RECOMPOSE_DATA`, ключи плагинов), вместе с поддеревом
   (`350`/`360`, xdictionary), копируются в `TSimpleDrawing` как список
   `TZDXFRawObject` (например, поле `PreservedNODBranches`).
2. При записи эти ветки выводятся после ветвей обработчиков, с
   перемапингом **всех** хэндл-кодов (`5`, `330`, `340`, `350`, `360`,
   `1005`, …) через тот же механизм, что и для шаблона. Ссылки на объекты,
   которых нет в новом файле, заменяются на `0` с предупреждением.
3. Неизвестные классы объектов требуют записи их определения из
   `RawClassesSection` в `CLASSES` — выводить только те классы, объекты
   которых реально сохраняются.
4. Ключ ZCAD (например, `ZCAD_DATA`) — зарезервировать имя и оформить
   как обычный обработчик; наполнение — вне рамок этого ТЗ.
5. Ключи, которые есть и в шаблоне, и в исходном файле (`ACAD_GROUP`,
   `ACAD_LAYOUT`, …) — по-прежнему из шаблона (ограничение 2 раздела 3).

Приёмка: DXF с искусственно добавленной веткой `3/MY_PLUGIN → DICTIONARY →
XRECORD` проходит round-trip без потерь; AutoCAD `AUDIT` — 0 ошибок.

### Этап 8. Тесты, документация, лог

1. Все тесты из раздела 6 включены в `cad_source/zengine/tests/Makefile`
   и CI.
2. Документация: этот файл обновляется фактическим статусом этапов;
   комментарии в новых модулях на русском, в стиле существующих
   (`uzeffdxfout.pas`, `uzeacadtable_*`).
3. Трассировочный лог NOD (разбор, обработчики, выделение хэндлов)
   выключен по умолчанию.

### Этап 9 (будущее, вне рамок). DWG

`fileformats/dwg/uzedwgcontrolobjects.pas` уже распознаёт
`DICTIONARY`/`XRECORD`. После этапа 5 можно построить `TZNODModel` из DWG
(корневой словарь — из заголовка DWG, `NAMED OBJECTS DICTIONARY` handle)
и переиспользовать те же `LoadProc`. Отдельное ТЗ.

---

## 6. Тестирование

### 6.1 Входные файлы

| Файл | Что проверяет |
|---|---|
| `cad_source/test/tablestyleetalon.dxf` | Эталон стилей таблиц (issue #1339 и далее). |
| `savetemplate2000.dxf`, `savetemplate2007.dxf`, `empty.dxf` | Шаблоны: NOD без `ACAD_TABLESTYLE`; с `ACAD_TABLESTYLE`, без `ACAD_MLEADERSTYLE`. |
| Синтетические DXF в `cad_source/zengine/tests/data/nod/` | NOD не первый; `360` вместо `350`; неизвестные ключи; пустой словарь; битая ссылка; несколько `DICTIONARY` с `330=0`; `\n` и `\r\n`. |
| DXF с мультивыносками (AutoCAD 2010+) | Этап 6. |

### 6.2 Виды тестов

1. **Unit-тесты модели** (этап 1): количество объектов, ключи NOD, хэндлы,
   владельцы, реакторы.
2. **Round-trip** (этапы 4–7): загрузка → сохранение → загрузка; сравнение
   модели NOD и таблиц стилей до и после (не побайтовое сравнение файлов —
   хэндлы перенумеровываются).
3. **Инварианты ссылочной целостности** выходного файла (автоматическая
   проверка по модели этапа 1):
   * каждый объект, кроме NOD, имеет `330` на существующий объект;
   * каждая пара `3/350` словаря указывает на объект, у которого `330` —
     этот словарь, и реактор на него;
   * все хэндлы уникальны и меньше `$HANDSEED`;
   * каждый объект неизвестного класса имеет определение в `CLASSES`.
4. **Ручная проверка** в AutoCAD / ODA File Converter + `AUDIT` на
   каждом этапе 4–7 (результат фиксируется в описании PR).
5. **Производительность**: `dxfloadbench.cmd` до/после этапа 2.

---

## 7. Риски и как их снять

| # | Риск | Мера |
|---|---|---|
| R1 | Изменение `savedxf20XX` снова «сломает систему сохранения» (как в `235736137`). | Этап 0 фиксирует эталоны; этап 4 сначала воспроизводит текущий вывод через адаптер и только потом меняет поведение; инварианты 6.2.3 в CI. |
| R2 | Коллизия хэндлов (`$F000`, повторное использование хэндлов шаблона). | Все хэндлы — только из `IODXFContext.handle`; `WriteTableStylesToDXFObjects` удаляется. |
| R3 | Стили загружаются после `ENTITIES`, таблицы строятся без стиля. | NOD pre-pass до `AddFromDXF20XX` (этап 2). |
| R4 | DXF 2000 не знает `TABLESTYLE`. | `MinVersion` обработчика; решение по DXF 2000 — за владельцем проекта; по умолчанию писать (AutoCAD 2000 игнорирует неизвестные классы, а стили не теряются при повторном открытии в ZCAD). |
| R5 | Порча `DXFTableStyleTable` из-за отсутствия `init`/`done`. | Этап 0. |
| R6 | `CTABLESTYLE`/`CMLEADERSTYLE` (`DICTIONARYVAR`) из шаблона ссылаются на стиль, которого нет в чертеже. | Перенаправлять на `Standard`; отдельное решение о переносе `AcDbVariableDictionary` в обработчики. |
| R7 | Нарушение слоёв (`uzcinterface` в zengine). | Этап 0, п. 3; ревью зависимостей новых модулей. |
| R8 | Рост времени загрузки из-за ещё одного прохода по тексту. | Разбор по уже извлечённой `RawObjectsSection` (один проход), замер на этапе 2; при необходимости — объединить с существующими пре-сканами. |
| R9 | Попытка перевести на NOD символьные таблицы или другие ключи. | Ограничения раздела 3; ревью PR. |

---

## 8. Глоссарий DXF-групп, используемых в ТЗ

| Код | Значение |
|---|---|
| `0` | Тип объекта (`DICTIONARY`, `TABLESTYLE`, `MLEADERSTYLE`, `XRECORD`, `DICTIONARYVAR`, …). |
| `5` | Хэндл объекта. |
| `102` | Начало/конец блока приложения: `{ACAD_REACTORS`, `{ACAD_XDICTIONARY`, `}`. |
| `330` | Soft-pointer на владельца (вне `102`) или на реактор (внутри `{ACAD_REACTORS`). `0` у NOD. |
| `360` | Hard-owner ссылка: на расширенный словарь (внутри `{ACAD_XDICTIONARY`) или на запись словаря с `280=1`. |
| `3` | Имя записи словаря. |
| `350` | Soft-owner ссылка на объект записи словаря. |
| `280` | Флаг hard owner у `DICTIONARY`. |
| `281` | Режим клонирования дублирующихся записей у `DICTIONARY`. |
| `340` | Soft-pointer (значение по умолчанию у `ACDBDICTIONARYWDFLT`, ссылки внутри стилей). |
| `342` | Ссылка `ACAD_TABLE` → `TABLESTYLE`. |
| `$HANDSEED` | Переменная HEADER: следующий свободный хэндл. |
