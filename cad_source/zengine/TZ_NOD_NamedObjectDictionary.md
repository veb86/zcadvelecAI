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
| `FindObjectHandleProc(const Name; var IODXFContext)` (опционально, этап 4) | Запись, после `ReserveHandlesProc` | Новый хэндл объекта ветки по имени (0 — нет). Нужна, чтобы перенаправить ссылки других объектов шаблона на пропущенные объекты его ветки; при 0 ссылка ведёт на объект с именем `DefaultName`. |
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
   **Исправлено на этапе 5 (issue #1452)**: после удаления из списка и
   `RemoveDataFromArray` стиль освобождается (`Done` + `Freemem`).
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
5. `DXFNODModelBuiltProc` — точка наблюдения (вызывается сразу
   после pre-pass, в том числе для R12 с пустой моделью), нужна тестам
   этапа 2; загрузку данных NOD на этапе 3 взял на себя реестр
   `uzeffdxfnodregistry`, точка наблюдения оставлена для тестов.
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

**Статус: выполнен (issue #1448).**

1. `uzeffdxfnodregistry.pas` (`zengine/fileformats`, зависимостей от `zcad`
   нет): `TZNODHandler` — ключ NOD, `ObjectType`, `MinVersion` (по
   умолчанию `AC1015`), `DefaultName`, `LoadProc`, `ReserveHandlesProc`,
   `SaveProc`, `ClassesProc` (сигнатуры раздела 4.3; любая процедура
   может быть `nil`). `RegisterNODHandler` (запись или перегрузка со
   списком процедур) — `False` и предупреждение в лог для пустого ключа и
   для повторной регистрации ключа (ключи сравниваются без учёта
   регистра). `UnregisterNODHandler`, `NODHandlerCount`, `GetNODHandler`,
   `FindNODHandler` — для тестов и выгрузки модулей. Реестр сам хэндлы не
   выделяет и в файл ничего не пишет: без обработчиков (и с обработчиком,
   который ничего не пишет) вывод не меняется.
2. Чтение: `AddFromDXF` для DXF 2000+ после pre-pass (и точки наблюдения
   `DXFNODModelBuiltProc`, оставленной для тестов этапов 2–3) вызывает
   `RunNODLoadHandlers(fileCtx.NODModel, Drawing)` — до `TABLES`/`ENTITIES`.
   Для каждого обработчика (в порядке регистрации; не в порядке ключей
   NOD) с ключом, найденным в NOD: если запись ссылается не на словарь —
   предупреждение, обработчик не вызывается; иначе хэндл словаря-ветки
   помечается «забранным» (`ClaimHandle`) и вызывается `LoadProc`; хэндлы
   объектов ветки помечает сам обработчик. Исключение в `LoadProc`
   пишется в лог и не прерывает ни загрузку, ни остальные обработчики.
   Незарегистрированные ключи пропускаются (трасса `NOD`). Для R12, файлов
   без `OBJECTS` и с ошибкой разбора `OBJECTS` модель пустая — обработчики
   не вызываются.
3. Запись: `savedxf20XX` создаёт `TZNODSaveSession` (снимок обработчиков,
   у которых `MinVersion` ≤ версии файла; для остальных — предупреждение
   «ключ не пишется в DXF …») и находит хэндл NOD шаблона разбором его
   секции `OBJECTS` моделью этапа 1 (только если обработчики есть).
   - `ReserveHandlesProc` — рядом с `PreallocateTableStyleHandles` (перед
     `ENTITIES` и, если `ENTITIES` в шаблоне нет, в начале `OBJECTS`), не
     более одного раза за сохранение; результат — хэндл словаря-ветки;
   - `ClassesProc` — перед `ENDSEC` секции `CLASSES`, до
     `RunClassesSaveDxfProcs`. Секция `CLASSES` идёт раньше `ENTITIES`,
     поэтому `ClassesProc` вызывается до `ReserveHandlesProc` и не должна
     зависеть от выделенных хэндлов;
   - `SaveProc(…, DictHandle, NODHandle)` — перед `ENDSEC` секции
     `OBJECTS`, до `RunObjectsSaveDxfProcs`; `NODHandle` — новый хэндл NOD
     шаблона (`OldHandele2NewHandle`), 0 — NOD в шаблоне нет.
   Регистрация в `initialization` и фиксированный порядок вызова — по
   порядку регистрации (обработчиков пока нет: `ACAD_TABLESTYLE` — этап 5,
   `ACAD_MLEADERSTYLE` — этап 6).
4. Пары `3/350` в NOD шаблона и пропуск шаблонной ветки ключа обработчика
   **не** делаются — это этап 4. До этапа 4 `SaveProc` обработчика,
   который пишет словарь-ветку, создаёт в файле словарь, не
   зарегистрированный в NOD.
5. Тест `cad_source/zengine/tests/nodstage3.lpr` (сборка без IDE:
   `experiments/issue1448/build_and_run_nodstage3.sh`):
   - регистрация: порядок, отказ для пустого и повторного ключа (другой
     регистр), удаление;
   - `RunNODLoadHandlers` на синтетической секции `OBJECTS`: вызовы в
     порядке регистрации, только для ключей NOD, ссылающихся на словарь;
     XRECORD, битая ссылка и отсутствующий ключ — без вызова;
     словарь-ветка помечена «забранной», исключение в `LoadProc` не мешает
     следующему обработчику; `nil` и модель без NOD — без вызовов;
   - `AddFromDXF`: на эталоне `LoadProc` `ACAD_TABLESTYLE` получает словарь
     `86` (3 записи), `ACAD_GROUP` — `D`; на `polylinearc.dxf` `LoadProc`
     вызывается при 0 сущностях, после загрузки их 2; R12 и файл с ошибкой
     `OBJECTS` — без вызовов; исключение в `LoadProc` не прерывает загрузку;
   - `TZNODSaveSession`: фильтр `MinVersion`, хэндл NOD шаблона `C`,
     `ReserveHandles`/`WriteObjects` идемпотентны;
   - **приёмка**: с обработчиком-заглушкой `ACAD_TABLESTYLE`
     round-trip `tablestyleetalon.dxf` и пустого чертежа на шаблонах
     2000/2007 совпадает с эталонами этапа 0; заглушка вызывается по одному
     разу (`Load`, `Classes`, `Reserve`, `Save`), в `SaveProc` приходит
     хэндл NOD сохранённого файла; с `MinVersion = AC1018` в DXF 2000
     вызывается только `LoadProc`, вывод совпадает с эталоном;
   - обработчик, который пишет данные: класс `ClassesProc` — внутри
     `CLASSES`, словарь `SaveProc` — последний объект `OBJECTS`, владелец —
     NOD, повторяющихся хэндлов нет; хэндл `ReserveHandlesProc` меньше
     `$HANDSEED` и хэндлов сущностей (`polylinearc.dxf`);
   - модуль лога `NOD` выключен по умолчанию, трасса реестра при включении
     форматируется без ошибок.

   Тесты этапов 0–2 проходят без изменений. `heaptrc`: объектов реестра
   среди неосвобождённых блоков нет (остаётся реестр форматов
   `uzeffmanager`).

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

**Статус: выполнен (issue #1450).**

1. Реестр (`uzeffdxfnodregistry.pas`), `TZNODSaveSession`:
   - `LoadTemplate` (в `savedxf20XX`, только если обработчики есть)
     разбирает `OBJECTS` шаблона моделью этапа 1 и строит для каждого
     ключа обработчика, найденного в NOD шаблона, множество хэндлов ветки:
     словарь-ветка, объекты по её записям и, рекурсивно, их расширенные
     словари (`102 {ACAD_XDICTIONARY` / `360`) с записями, а также ссылки
     на объекты ветки из остальных объектов шаблона (`TemplateRefs`: хэндл,
     обработчик, ключ объекта в словаре ветки). Состояния «конечного
     автомата» больше нет;
   - `ReserveHandlesProc` возвращает хэндл словаря-ветки; **0 — ветка
     обработчиком не пишется, ветка шаблона копируется как есть** (так
     сохраняется чертёж без стилей таблиц — вывод совпадает с эталонами
     этапа 0 побайтно);
   - `MapTemplateHandles` (после `ReserveHandles`, идемпотентна) заносит в
     `OldHandele2NewHandle`: словарь-ветку шаблона → словарь обработчика
     (так пара `350` записи NOD шаблона указывает на новую ветку); объекты
     ветки, на которые ссылаются другие объекты шаблона, → объект чертежа
     с тем же именем (`FindObjectHandleProc`, новое поле контракта 4.3),
     иначе — объект `DefaultName`; без обработчика/имени ссылка остаётся
     на хэндл, который выдаст обычный перемаппинг;
   - `IsTemplateHandleSkipped`/`HasTemplateSkips` — пропуск объектов ветки
     шаблона, если обработчик выделил свой словарь;
   - `TakeNODInsertions(ANextKey)` — записи `3/350` ключей, которых нет в
     NOD шаблона (`DictHandle > 0`), меньших `ANextKey` без учёта регистра;
     каждая выдаётся один раз, по алфавиту; `''` — все оставшиеся; без NOD
     в шаблоне — ничего (словарь пишется без записи в NOD, предупреждение
     в лог адаптера).
2. `savedxf20XX` (`uzeffdxfout.pas`): автомат стилей таблиц удалён
   (`tablestyledicthandle`, `intablestyledict`, `writtenstylecount`,
   `tsHandles`/`tsDictHandles`/`tsMapHandles`, `PreallocateTableStyleHandles`;
   от последней остался `PreallocateAcadTableOwnerHandle` — хэндл
   `*Model_Space` для сырых `ACAD_TABLE`, issue #1339).
   - перед `ENTITIES` (и, если их нет, в начале `OBJECTS`) —
     `ReserveHandles` + `MapTemplateHandles`;
   - в объекте NOD шаблона перед каждой записью `3/<ключ>` пишутся
     недостающие записи обработчиков (`TakeNODInsertions(ключ)`), в конце
     NOD — оставшиеся; пары `350` перемаплены через `OldHandele2NewHandle`;
   - объект шаблона пропускается целиком, если его хэндл (группа `5`)
     входит во множество пропускаемых (заглядывание на одну пару вперёд);
   - перед `ENDSEC` секции `OBJECTS` — `SaveProc` обработчиков, затем
     `RunObjectsSaveDxfProcs`;
   - имена классов шаблона собираются в `IODXFContext.TemplateClassNames`
     (`uzeffdxfsupport.pas`) — для `ClassesProc`;
   - новые хэндлы — только из `IODXFContext.handle`, `$HANDSEED` — как
     раньше. `WriteTableStylesToDXFObjects` (хэндлы с `$F000`) помечена
     `deprecated`, вызовов нет; удалить — на этапе 5.
3. Временный обработчик-адаптер `ACAD_TABLESTYLE` (в `uzeffdxfout.pas`,
   регистрируется в `initialization`; `ObjectType = TABLESTYLE`,
   `MinVersion = AC1015`, `DefaultName = Standard`, `LoadProc = nil` —
   чтение переводится на этапе 5):
   - `ReserveHandlesProc` — бывший `PreallocateTableStyleHandles`: словарь,
     затем на каждый стиль `TABLESTYLE`, его расширенный словарь и
     `CELLSTYLEMAP` (issue #1409), карта `TableStyleNameHandleMap`;
     повторяющееся имя стиля пропускается с предупреждением (раньше
     писались два объекта с одним ключом); нет стилей — 0;
   - `SaveProc` — `DICTIONARY` ветки (`330` и реактор — NOD, `281=1`,
     записи по стилям) и `WriteTableStyleObjectToStream` по стилям;
   - `ClassesProc` — класс `TABLESTYLE`, если стили есть, а в шаблоне
     класса нет (шаблон 2000); группа `91` — только с DXF 2004 (`AC1018`);
   - `FindObjectHandleProc` — хэндл стиля по имени из
     `TableStyleNameHandleMap`.
   В шаблонах 2000/2007 ссылок на объекты ветки `ACAD_TABLESTYLE` извне
   нет (`CTABLESTYLE` в `AcDbVariableDictionary` хранит имя в группе `1`),
   перенаправление ссылок проверено на синтетическом шаблоне.
4. Эталоны: `tablestyles_2000.dxf`/`tablestyles_2007.dxf` в
   `tests/data/nod/golden` обновлены под этап 4 (в `nodstage0` у чертежа
   теперь два стиля таблиц, раньше ветка шаблона просто копировалась,
   т.к. загрузка DXF не заполняет `DXFTableStyleTable`); эталоны этапа 0
   сохранены в `tests/data/nod/stage0`. Эталоны `empty_*` и
   `tablestyleetalon_*` не изменились.
5. Тест `cad_source/zengine/tests/nodstage4.lpr` (сборка без IDE:
   `experiments/issue1450/build_and_run_nodstage.sh nodstage4`):
   - адаптер зарегистрирован, поля и процедуры — как в п. 3;
   - без стилей вывод `empty`/`tablestyleetalon` на шаблонах 2000/2007
     совпадает с эталонами этапа 0;
   - **приёмка 2007**: со стилями `Standard`, `ZCAD1442` вывод совпадает с
     эталоном этапа 0 (`stage0/tablestyles_2007.dxf`) с точностью до
     хэндлов и порядка объектов `OBJECTS` (каноническая форма: хэндлы
     переименованы обходом от NOD, объекты отсортированы; то же делает
     `experiments/issue1450/dxfcanon.py`);
   - **приёмка 2000**: в NOD появляется `ACAD_TABLESTYLE` (ключи NOD по
     алфавиту), словарь со стилями `Standard`, `ZCAD1442`, класс
     `TABLESTYLE` без группы `91`;
   - в обеих версиях: одна запись `ACAD_TABLESTYLE`, владелец словаря —
     NOD, записи — `TABLESTYLE` с владельцем-словарём и расширенным
     словарём `ACAD_ROUNDTRIP_2008_TABLESTYLE_CELLSTYLEMAP` → `CELLSTYLEMAP`,
     `TABLESTYLE` шаблона в файле нет, хэндлы уникальны и меньше
     `$HANDSEED`, неразрешённые ссылки — те же, что в эталоне этапа 0
     (в 2007 — `331:94`: `LAYOUT` ссылается на `VPORT` шаблона, который
     при записи заменяется; было и до этапа 4);
   - `+testtable.dxf` + стиль `Standard`: `342` всех `ACAD_TABLE` ведёт на
     `Standard` ветки, неразрешённых ссылок нет; файл загружается и
     сохраняется повторно с тем же результатом (каноническая форма);
   - `TZNODSaveSession` на синтетическом шаблоне: множество пропуска
     (словарь, стили, расширенный словарь, `CELLSTYLEMAP`), `TemplateRefs`,
     перемаппинг (словарь → новый, `Standard`/`Other` → по имени, запись
     расширенного словаря → `DefaultName`), идемпотентность, без стилей —
     ничего не пропускается и не перемапливается; вставки в NOD по
     алфавиту и однократно (обработчик с `DictHandle = 0` не вставляется),
     шаблон без NOD; повторяющиеся имена стилей;
   - трасса этапа 4 при включённом модуле `NOD` форматируется без ошибок.

   Тесты этапов 0–3 проходят (в `nodstage3` адаптер снимается перед
   тестами реестра на заглушках). `heaptrc`: объектов реестра и адаптера
   среди неосвобождённых блоков нет (остаются реестр форматов
   `uzeffmanager` и строки загрузчика `ACAD_TABLE`).

   **Не проверено**: открытие результата в AutoCAD и `AUDIT` (нет AutoCAD
   в среде сборки) — нужна ручная проверка файлов `tablestyles_2000.dxf`,
   `tablestyles_2007.dxf` из `tests/data/nod/golden`.

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

**Статус: выполнен (issue #1452).**

1. Обработчик `ACAD_TABLESTYLE` перенесён из `uzeffdxfout.pas` в
   `cad_source/zengine/styles/uzestylestablesdxfnod.pas` (регистрируется в
   `initialization` модуля; `ObjectType = TABLESTYLE`,
   `DefaultName = Standard`):
   - `LoadProc` (`LoadTableStylesFromDictionary`): для каждой записи
     словаря-ветки — объект `TABLESTYLE` из модели этапа 1, поля
     разбираются по парам `TZDXFRawObject` (`ParseTableStyleRawObject`,
     логика прежнего `ParseTableStyleObject`: группа `7` начинает блок
     ячейки, `102 {…}` пропускаются), `DXFHandle` — исходный хэндл,
     `XDictHandle` — хэндл расширенного словаря. Запись не на `TABLESTYLE`,
     на несуществующий объект или без имени пропускается (предупреждение в
     лог). Если стиль с таким именем в чертеже уже есть (вставка/слияние,
     повтор имени в словаре) — существующий не меняется. Забираются
     хэндлы `TABLESTYLE`, его расширенного словаря и `CELLSTYLEMAP` в нём;
     прочие записи расширенного словаря не переносятся (предупреждение);
   - **`CELLSTYLEMAP` не хранится**: при записи карта стилей ячеек строится
     заново по параметрам стиля (`WriteCellStyleMapObjectsToStream`, issue
     #1409), поэтому сырые данные исходного файла не нужны;
   - `ReserveHandlesProc`, `SaveProc`, `FindObjectHandleProc` — как у
     адаптера этапа 4; `ClassesProc` пишет классы `TABLESTYLE` и
     `CELLSTYLEMAP` (класс `CELLSTYLEMAP` раньше объявлял
     `uzeacadtable_dxf_write`), если стили есть, а в шаблоне класса нет;
     группа `91` — только с DXF 2004 (`AC1018`), поэтому в эталоне
     `tests/data/nod/golden/tablestyles_2000.dxf` из класса `CELLSTYLEMAP` ушла пара
     `91/2`;
   - **R4: `MinVersion = AC1015`** — стили таблиц пишутся и в DXF 2000,
     как на этапе 4 (AutoCAD 2000 неизвестный класс игнорирует);
   - числа читаются и пишутся с `DXFFormat` (десятичная точка) независимо
     от локали.
2. Стиль `Standard` по умолчанию: новое поле контракта 4.3
   `EnsureDefaultsProc` и `RunNODEnsureDefaults`
   (`uzeffdxfnodregistry.pas`) — вызывается в `AddFromDXF` после
   `RunNODLoadHandlers` (для R12 — вместо него), до `ENTITIES`, для любой
   версии DXF и независимо от наличия ключа в NOD. Обработчик стилей
   таблиц создаёт `Standard`, только если таблица пуста
   (`EnsureDefaultTableStyle`), со значениями `TABLESTYLE Standard` шаблона
   `savetemplate2007.dxf` (`FillDefaultTableStyle`: отступы `0.06`, высоты
   `0.18/0.25/0.18`, выравнивание `2/5/5`, текстовый стиль `Standard`).
3. `342` у `ACAD_TABLE`: стили загружаются до `ENTITIES`, поэтому
   `GetStyleByHandle` находит стиль исходного файла (`+testtable.dxf`:
   `342 = BA` → `vebtable`; раньше стили не загружались, и при записи
   ссылка вела на `Standard`). Ограничение: при вставке чертежа в
   непустой (`TLOMerge`) стиль с уже существующим именем не загружается, а
   `342` вставляемых таблиц ищется по хэндлам исходного чертежа — стиль
   может не найтись (берётся первый стиль таблицы, как и раньше).
4. `ReadTableStylesFromDXFObjects`, `ExtractTableStyleDictionary`,
   `WriteTableStylesToDXFObjects` и разбор сырого текста удалены из
   `uzestylestablesdxf.pas` (в модуле остались типы и
   `GDBDXFTableStyleArray`). Эквивалентность проверена до удаления
   (`experiments/issue1452/tsequiv1452.lpr`) на 77 DXF репозитория: 75
   совпадают; расходятся `nod_not_first.dxf` и `nod_hardowner_360.dxf`, где
   прежний разбор ошибался (NOD не первый объект `OBJECTS`; ссылка `360`
   вместо `350`), новый загрузчик читает их верно. Интерфейс для
   потребителей (`GetDXFTableStyleTable`, `TGDBDXFTableStyle`) не изменился.
5. GUI: удалённый стиль таблицы освобождается (`uzcftablestyles.pas`,
   `uzcui_tablestylemanager.pas`, см. этап 0, п. 2); компиляция модулей
   проверена `experiments/issue1452/compile_gui_units.sh`.
6. Эталоны: `tests/data/nod/golden/tablestyleetalon_2000.dxf` и `_2007.dxf` теперь
   содержат стили эталона (`aits`, `Standard`, `vebts`) и классы
   `CELLSTYLEMAP` (и `TABLESTYLE` для 2000) — раньше стили эталона при
   сохранении терялись. Прежние эталоны сохранены в `tests/data/nod/stage3`
   (с ними сравнивается вывод заглушки `nodstage3`, которая стили не
   загружает и не пишет). Ожидания `nodstage0`–`nodstage4` обновлены
   (`nodstage4`: `342` ведёт на `vebtable`, стили берутся из файла).
7. Тест `cad_source/zengine/tests/nodstage5.lpr` (сборка без IDE:
   `experiments/issue1452/build_and_run.sh nodstage5`):
   - обработчик зарегистрирован с `LoadProc`, `EnsureDefaultsProc`,
     `MinVersion = AC1015`;
   - стили 8 файлов (`tablestyleetalon`, `+testtable`, `testtable`,
     `tableheighttextbug`, `bugbreaktable`, `savetemplate2007`,
     `nod_not_first`, `nod_hardowner_360`) совпадают с эталонами
     `tests/data/nod/stage5/*.txt` (получены прежним разбором, для двух
     последних — проверены вручную);
   - забраны хэндлы стилей, их расширенных словарей и `CELLSTYLEMAP`, и
     только они;
   - синтетическая ветка: пропуск записей не на `TABLESTYLE`, на
     несуществующий объект и без имени; существующий стиль и повтор имени
     не меняют таблицу;
   - `Standard` по умолчанию для R12 и для файла без `ACAD_TABLESTYLE`;
     непустая таблица не дополняется;
   - `GetStyleByHandle('ba')` → `vebtable` (`+testtable.dxf`);
   - `tablestyleetalon`: загрузка → сохранение 2000/2007 → загрузка даёт
     те же стили (с точностью до хэндлов); класс `CELLSTYLEMAP` записан
     один раз, группа `91` — только в 2007; по одному `TABLESTYLE` и
     `CELLSTYLEMAP` на стиль.

   Тесты `nodstage0`–`nodstage5` проходят; `roundtrip1339`, `roundtrip1381`,
   `roundtrip1436` — OK (`experiments/issue1452/run_roundtrips.sh`,
   проверка `experiments/issue1450/dxfcheck.py`: у `1339_testtable`
   исчезла неразрешённая ссылка `342:BA`; `331:94` — из шаблона, было и
   раньше). `heaptrc` (`nodstage5`): неосвобождённые блоки — только строки
   загрузчика `ACAD_TABLE`. Тест `uzctacadtable`
   (`testacadtable_standalone`) не собирается и на `master` (нет
   `NulPoint`, `CreateVertex`, `VertexAdd`) — вне рамок этапа.

   **Не проверено**: открытие результата в AutoCAD, `AUDIT` и диалог
   стилей таблиц (нет AutoCAD в среде сборки) — нужна ручная проверка
   `tests/data/nod/golden/tablestyleetalon_2007.dxf` и `tests/data/nod/golden/tablestyleetalon_2000.dxf`;
   отображение стилей в инспекторе объектов ZCAD (GUI не запускался).

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

**Статус: выполнен (issue #1454).**

1. Обработчик `ACAD_MLEADERSTYLE` — новый модуль
   `cad_source/zengine/styles/uzestylesmleaderdxfnod.pas` (регистрируется в
   `initialization`; `ObjectType = MLEADERSTYLE`, `DefaultName = Standard`):
   - `LoadProc` (`LoadMLeaderStylesFromDictionary`): для каждой записи
     словаря-ветки — объект `MLEADERSTYLE` модели этапа 1, поля
     разбираются по парам `TZDXFRawObject` (`ParseMLeaderStyleRawObject`);
     `DXFHandle` — исходный хэндл, `XDictHandle` — хэндл расширенного
     словаря, `MLeaderVersion` — `1070` после `1001 ACAD_MLEADERVER`.
     Группы тела, которые модель стиля не знает (например, `271`–`273`
     AutoCAD 2010+), сохраняются в новом поле `ExtraPairs` и пишутся
     обратно в исходном порядке. Запись не на `MLEADERSTYLE`, на
     несуществующий объект или без имени пропускается (предупреждение в
     лог); стиль с уже существующим именем не меняется. Забираются хэндлы
     стиля и его расширенного словаря; записи расширенного словаря не
     переносятся (предупреждение), как у стилей таблиц этапа 5;
   - прежний разбор и запись по сырому тексту `OBJECTS` в
     `uzestylesmleaderdxf.pas` нигде не вызывались и удалены (в модуле
     остались типы `TGDBDXFMLeaderStyle` и `GDBDXFMLeaderStyleArray`).
2. `TSimpleDrawing.DXFMLeaderStyleTable` (`init` в `init`, освобождение
   стилей в `done`) и метод доступа `GetDXFMLeaderStyleTable`
   (`uzedrawingsimple.pas`).
3. Ссылки `340`/`341`/`342`/`343`:
   - **загрузка**: `MLEADERSTYLE` ссылается на записи символьных таблиц,
     а `OBJECTS` читается до `TABLES` ZCAD. Поэтому модель NOD получила
     индекс символьных таблиц (`TZNODModel.LoadSymbolTablesFromText`,
     `FindSymbolName`, `SymbolRecordCount`): секция `TABLES` читается
     ещё раз (`DXFNODLoadSymbolTables` в `uzeffdxf.pas`), только если
     обработчик с новым полем контракта 4.3 `NeedsSymbolTables` будет
     вызван (`NODLoadNeedsSymbolTables`: ключ есть в NOD и словарь не
     пуст). Имена DXF до 2007 перекодируются из `$DWGCODEPAGE`. Хэндл
     переводится в имя, только если он ведёт на запись нужной таблицы
     (`340` → `LTYPE`, `341`/`343` → `BLOCK_RECORD`, `342` → `STYLE`),
     иначе — предупреждение, имя пустое;
   - **запись**: по именам через `LineTypeNameHandleMap` (новая карта
     контекста записи, заполняется при записи `LTYPE` в
     `uzeffdxfout.pas`), `TextStyleNameHandleMap`, `BlockNameHandleMap`.
     Запасные значения: тип линии не сохраняется — `ByBlock`
     (`Continuous`), текстовый стиль — `Standard`, иначе `0`
     (предупреждение в лог); блок стрелки/содержимого не сохраняется —
     группа `341`/`343` не пишется (как у AutoCAD для стиля без блока).
4. `MinVersion = AC1021` (DXF 2007+; в DXF 2000 ветка, стили и класс не
   пишутся). `ClassesProc` — класс `MLEADERSTYLE` (`AcDbMLeaderStyle`,
   `ACDB_MLEADERSTYLE_CLASS`, `90 = 4095`, `91` = число стилей), если
   стили есть, а в шаблоне класса нет. Новое поле контракта 4.3
   `XDataAppName`: `uzeffdxfout.pas` пишет в `APPID` приложения
   обработчиков с подходящей `MinVersion` — `ACAD_MLEADERVER` теперь есть
   в каждом DXF 2007 (как у AutoCAD), в DXF 2000 — нет.
5. `Standard` по умолчанию: `EnsureDefaultsProc`
   (`EnsureDefaultMLeaderStyle`) — если после загрузки DXF любой версии
   стилей нет, создаётся `Standard` с параметрами стиля `Standard`
   AutoCAD (`+mleader2008.dxf`), текстовый стиль `Standard`, тип линии
   `ByBlock`. Шаблон `savetemplate2007.dxf` ветки `ACAD_MLEADERSTYLE` не
   содержит — ключ добавляется в NOD механизмом этапа 4. Чертёж, не
   загруженный из DXF (новый), стилей не получает, ветка не пишется
   (`ReserveHandlesProc`: стилей нет — ветки нет, как на этапе 5).
6. Эталоны: `tests/data/nod/golden/empty_2007.dxf`,
   `tablestyles_2007.dxf` — появилась запись `APPID ACAD_MLEADERVER`
   (сдвиг хэндлов на 1); `tablestyleetalon_2007.dxf` — ещё ветка
   `ACAD_MLEADERSTYLE` со `Standard` и класс `MLEADERSTYLE`. Эталоны DXF
   2000 не изменились. `nodstage3` (заглушки без обработчиков zengine)
   сравнивает `empty_2007` с прежним эталоном
   `tests/data/nod/stage3/empty_2007.dxf`; `nodstage4` сравнивает с
   эталоном этапа 0 вывод без обработчика `ACAD_MLEADERSTYLE`.
7. Тест `cad_source/zengine/tests/nodstage6.lpr` (сборка без IDE:
   `experiments/issue1452/build_and_run.sh nodstage6`):
   - обработчик зарегистрирован со всеми процедурами, `MinVersion =
     AC1021`, `XDataAppName = ACAD_MLEADERVER`, `NeedsSymbolTables`;
   - стили 4 файлов AutoCAD (`cad_source/test/+mleader2008.dxf`,
     `mleaderblock.dxf`, `mleader2007notwork.dxf`,
     `mleader2000notwork.dxf`) совпадают с эталонами
     `tests/data/nod/stage6/*.txt`; эталоны сверены независимым
     разбором на Python (`experiments/issue1454/mlsdump1454.py`);
   - имена ссылок: блоки стрелок `_Dot`, `_BoxBlank`, блок содержимого
     `_TagSlot`/`_DetailCallout`, тип линии `ByBlock`, текстовый стиль
     `Standard`;
   - забраны хэндлы стилей и их расширенных словарей, и только они;
     индекс символьных таблиц строится, только если он нужен;
   - синтетическая ветка: пропуск записей не на `MLEADERSTYLE`, на
     несуществующий объект и без имени; существующий стиль и повтор имени
     не меняют таблицу; `ExtraPairs` сохраняются; `343` на запись `STYLE`
     не разрешается;
   - `Standard` по умолчанию для R12, файла без `ACAD_MLEADERSTYLE`, без
     `OBJECTS` и `tablestyleetalon.dxf` совпадает со `Standard` AutoCAD;
   - round-trip 2007 для всех 4 файлов и `tablestyleetalon.dxf`: число
     `MLEADERSTYLE`, класс один раз (`91` = число стилей), `APPID` один
     раз, владельцы ветки и стилей, `1001 ACAD_MLEADERVER`, ссылки
     `340`–`343` ведут на записи нужных таблиц сохранённого файла;
     повторная загрузка даёт те же стили (с точностью до хэндлов);
   - DXF 2000: ни ветки, ни класса, ни `APPID`; пустой чертёж в DXF 2007:
     только `APPID`.

   Тесты `nodstage0`–`nodstage6` проходят. Проверка чувствительности: без
   записи `343` в `WriteMLeaderStyleObjectToStream` round-trip тесты
   падают.

   **Не проверено**: открытие результата в AutoCAD, `AUDIT` и диалог
   стилей мультивыносок (нет AutoCAD в среде сборки) — нужна ручная
   проверка файлов, сохранённых из `cad_source/test/+mleader2008.dxf` и
   `mleaderblock.dxf` в DXF 2007, и `tests/data/nod/golden/tablestyleetalon_2007.dxf`.
   Мультивыноски (`MULTILEADER`) ZCAD по-прежнему читает как прокси —
   сохраняются ли сами мультивыноски, вне рамок этапа.

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
