# ТЗ: поддержка растровых изображений IMAGE (DXF/DWG) в ZCAD

Issue: [#1462](https://github.com/veb86/zcadvelecAI/issues/1462) — «IMAGE. Добавить поддержку IMAGE ТЗ».

Документ — только техническое задание. Код в рамках issue не пишется.
Реализация выполняется поэтапно (4 этапа), каждый этап — отдельный PR.
После каждого этапа чертёж должен открываться и сохраняться не хуже, чем
до него. Исполнитель — ИИ-программист, поэтому в каждом этапе перечислены
конкретные файлы, точки встраивания и критерии приёмки.

Основа — система NOD (ТЗ
[`TZ_NOD_NamedObjectDictionary.md`](TZ_NOD_NamedObjectDictionary.md),
этапы 0–9 выполнены). Справочные материалы (только для ознакомления):
`DXFTableSaveNEW/ZCAD_IMAGE_INTEGRATION_SPEC_AND_TZ.md`,
`DXFTableSaveNEW/DXF_IMAGE_READING_SPECIFICATION.md`, эталонный проект
`DXFTableSaveNEW/src/services/dxfParser.ts` (TypeScript, только чтение),
эталонный файл `DXFTableSaveNEW/acadtableandhrefImage2007.dxf` +
`DXFTableSaveNEW/testimage.png`.

Содержание:

1. [Проверка достоверности справочного ТЗ](#1-проверка-достоверности-справочного-тз)
2. [Текущее состояние кода](#2-текущее-состояние-кода)
3. [Границы задачи и жёсткие ограничения](#3-границы-задачи-и-жёсткие-ограничения)
4. [Целевая архитектура](#4-целевая-архитектура)
5. [Этапы реализации](#5-этапы-реализации)
6. [Тестирование](#6-тестирование)
7. [Риски и как их снять](#7-риски-и-как-их-снять)
8. [Глоссарий DXF-групп, используемых в ТЗ](#8-глоссарий-dxf-групп-используемых-в-тз)

---

## 1. Проверка достоверности справочного ТЗ

`ZCAD_IMAGE_INTEGRATION_SPEC_AND_TZ.md` в целом верно описывает связку
`IMAGE` → `IMAGEDEF` → `ACAD_IMAGE_DICT`. Ниже — пункты, которые нужно
уточнить, чтобы не заложить ошибки в реализацию. Сверка выполнена по
эталонному файлу `acadtableandhrefImage2007.dxf` (AutoCAD, `AC1021`).

| Утверждение в справочном ТЗ | Оценка | Уточнение |
|---|---|---|
| В `CLASSES` нужны классы `IMAGEDEF` и `IMAGE` | Неполно | В эталоне AutoCAD пишет **четыре** класса приложения `ISM`: `RASTERVARIABLES` / `AcDbRasterVariables` (`90=0, 280=0, 281=0`), `IMAGEDEF` / `AcDbRasterImageDef` (`90=0, 280=0, 281=0`), `IMAGEDEF_REACTOR` / `AcDbRasterImageDefReactor` (`90=1, 280=0, 281=0`), `IMAGE` / `AcDbRasterImage` (`90=2175, 280=0, 281=1`). Без классов `RASTERVARIABLES` и `IMAGEDEF_REACTOR` AutoCAD не распознает эти объекты. Группа `91` (счётчик экземпляров) пишется только начиная с `AC1018` — так уже делает `WriteClassRecord` (`zengine/styles/uzestylestablesdxfnod.pas:850`). |
| Приложение класса — `ISM`, `281=1` у `IMAGE` | Верно | Но общий `WriteClassRecord` пишет жёстко `3=ObjectDBX Classes` и `281=0` — для растровых классов нужен свой вывод записи класса (см. этап 2). |
| `IMAGEDEF`: `1` путь, `10/20` размер, `11/21` размер пикселя, `280=1`, `281: 1 = мм, 2 = см` | Неточно | `281` у `IMAGEDEF` — единицы разрешения: `0` нет, `2` сантиметры, `5` дюймы. Значения `1 = мм` нет. Кроме того, `IMAGEDEF` содержит `90` (версия класса, `0`) и блок `102 {ACAD_REACTORS 330 <словарь> 330 <реактор> … 102 }`: **каждый** реактор каждой сущности `IMAGE` должен быть перечислен. В эталоне: `330 B84` (словарь) и `330 B87` (реактор). |
| `IMAGEDEF_REACTOR`: `330 <IMAGE>`, `90 2`, `330 <IMAGE>` | Верно | Владелец реактора — **сущность** `IMAGE`, а не словарь. Поэтому реактор недостижим из NOD и не попадает ни в одну ветку при обходе дерева NOD (важно для этапа 7 ТЗ NOD, см. 2.3). |
| `RASTERVARIABLES`: `70` IMAGEFRAME, `71` качество, `72` единицы | Верно | Дополнительно: `90` — версия класса (`0`), владелец — NOD (`102 {ACAD_REACTORS 330 <NOD>}`, `330 <NOD>`). `70`: `0` рамка скрыта, `1` видна и печатается, `2` видна, но не печатается. |
| Ключи NOD `ACAD_IMAGE_DICT` и `ACAD_IMAGE_VARS`, «NOD — дескриптор `C`» | Верно с оговоркой | Хэндл NOD в ZCAD перенумеровывается при записи (`OldHandele2NewHandle` в `uzeffdxfout.pas`), привязываться к `C` нельзя. В шаблонах ZCAD (`savetemplate2000.dxf`, `savetemplate2007.dxf`) этих ключей **нет** — их должен добавить NOD writer (`TakeNODInsertions`), как это уже делается для `ACAD_TABLESTYLE`. |
| Словарь `ACAD_IMAGE_DICT`: `3 <имя>` → `350 <IMAGEDEF>` | Верно | У словаря `281=1`. Имя записи — имя определения (в эталоне `testimage`), не обязательно совпадает с именем файла. Имена уникальны без учёта регистра. |
| `IMAGE`: `90 0`, `10/11/12/13`, `340`, `70`, `280`, `281–283`, `360`, `71`, `91`, `14/24` | Верно | Дополнительно: AutoCAD пишет proxy-графику `92/310` (при чтении пропускать, при записи не писать); с `AC1024` есть `290` (режим подрезки, «инвертированная» подрезка). `11/21/31` и `12/22/32` — **векторы шага одного пикселя в WCS**, не единичные. У `IMAGE` нет группы `210` (всё в WCS). |
| Контур подрезки `14/24` в пиксельных координатах, `(-0.5,-0.5)` — «центр левого нижнего пикселя» (`DXF_IMAGE_READING_SPECIFICATION.md`, раздел 6) | Неточно | `(-0.5,-0.5)` и `(W-0.5,H-0.5)` — **углы** изображения, центры пикселей — целые. По реализации ezdxf (`Image.boundary_path_wcs`: «Boundary/Clipping path origin 0/0 is in the Left/Top corner of the image») начало пиксельных координат — **левый верхний** угол, ось Y вниз: `WCS = P0 + (x+0.5)·U + (H−y−0.5)·V`. Для симметричного контура по умолчанию разница не видна; проверить на файле AutoCAD с несимметричной подрезкой (этап 1, тест 3). |
| «Отложенное связывание (Deferred Linking)»: `IMAGE → IMAGEDEF` после чтения всего файла | Не нужно для DXF | В ZCAD секция `OBJECTS` разбирается NOD pre-pass (`DXFNODPrePass`) и NOD-обработчиками **до** `ENTITIES` (`uzeffdxf.pas`, ~2040–2068). Определения уже загружены, когда читается `IMAGE`, и ссылку `340` можно разрешить сразу в `LoadFromDXF`. Для DWG действует тот же порядок (`DWGNODLoad` до сущностей, `uzedwgimport.pas:985`). |
| Эталонный проект «читает IMAGE» | Верно с оговоркой | `dxfParser.ts` при единственном `IMAGEDEF` связывает его с любой сущностью, а путь `testimage.png` частично зашит. `dxf_saver.py` и `dxfInjector.ts` растры не пишут — эталона **записи** нет, эталон записи — файл AutoCAD. |
| Класс `TZCImage = class(TZCEntity)` и `TZCADImageDef = class` (раздел 3.1) | Не подходит | В ZCAD сущности — `object` на иерархии `GDBObj*` (`GDBObjEntity`, `uzeentity.pas:55`), регистрируются через `RegisterDXFEntity` (`uzeentityfactory.pas:53`). Именованные объекты чертежа — `object(GDBNamedObject)` в `GDBNamedObjectsArray` (образец — `TGDBDXFMLeaderStyle`, `zengine/styles/uzestylesmleaderdxf.pas`). |
| Поддержка DWG «чтение и запись» (issue) | Запись невозможна | ZCAD не пишет DWG вообще: форматы сохранения — только DXF 2000 и DXF 2007 через zengine (`zcad/register/uzcregfileformats.pas:77–79`). DWG — только чтение (LibreDWG). Этот же путь используется для «DXF via LibreDWG». |

Вывод: растр в DXF — это **пять связанных узлов** (4 класса в `CLASSES`,
сущность `IMAGE`, словарь `ACAD_IMAGE_DICT` + `IMAGEDEF`, реактор
`IMAGEDEF_REACTOR` на каждую сущность, `RASTERVARIABLES`), и при записи
ссылки в обе стороны (`IMAGE.340/360` ↔ `IMAGEDEF` реакторы ↔
`IMAGEDEF_REACTOR.330`) должны быть согласованы.

---

## 2. Текущее состояние кода

### 2.1 Сущности и фабрика

* Сущности `IMAGE` в ZCAD нет (поиск `IMAGE`, `IMAGEDEF`, `ACAD_IMAGE_DICT`,
  `RASTERVARIABLES` по `*.pas` ничего не находит).
* Фабрика: `RegisterDXFEntity(ID, DXFName, UserName, Alloc, AllocAndInit,
  SetGeomProps, AllocAndCreate)` — `zengine/core/uzeentityfactory.pas:53`,
  заполняет `DXFName2EntInfoData`, `ENTName2EntInfoData`, `ObjID2EntInfoData`.
  Регистрация — в секции инициализации модуля сущности (образец —
  `uzeentsolid.pas:338–340`).
* Идентификаторы — `zengine/core/uzeconsts.pas:35–71`. Стандартные DXF
  сущности занимают `1..19` (`GDBLeaderID=19`), `100..104` — сущности ZCAD
  (`GDBZCadEntsMinID/MaxID`, не использовать), далее `105..111`
  (`GDBAcdProxyID=110`, `GDBAcadTableID=111`). Имена объектов —
  `ObjN_GDBObj*` (там же, ~142–170).
* Ближайшие образцы плоской сущности из 4 точек:
  * `GDBObj3DFace` (`uzeent3dface.pas`, наследник `GDBObj3d`) — точки в
    **WCS**, `transform`/`TransformAt` преобразуют точки напрямую
    (`uzeent3dface.pas:75–89`). Это ближе к `IMAGE` (у него нет OCS/`210`).
  * `GDBObjSolid` (`uzeentsolid.pas:35`) — полный набор методов
    (`FormatEntity`, `onmouse`, `CalcTrueInFrustum` через
    `CalcOutBound4VInFrustum`, 4 ручки, `Clone`, `getoutbound`), но точки
    в OCS и `SaveToDXFObjPostfix` пишет `210`
    (`uzeentwithlocalcs.pas:207–212`) — для `IMAGE` не подходит как база.

### 2.2 Чтение DXF сейчас

* Диспетчер сущностей по имени — `FindOrProxyEntInfo`
  (`zengine/fileformats/uzeffdxf.pas:101–122`): неизвестное имя
  превращается в `GDBObjAcdProxy`.
* `IMAGE` сейчас читается как proxy: `GDBObjAcdProxy.LoadFromDXF`
  (`uzeentacdproxy.pas:357–415`) сохраняет только `90/91/93/94/95/70/310`,
  геометрия `10/11/12/13`, ссылки `340/360` и контур `14/24` теряются.
  При записи proxy превращается во вставку блока `PE<N>`
  (`uzeentacdproxy.pas:453–471`, `EnsureConvertedBlockDef` `:898`), в
  котором в лучшем случае остаются векторы proxy-графики `92/310`
  (обычно рамка). **Изображение и связь с `IMAGEDEF` теряются.**
* Порядок загрузки DXF 2000+ (`uzeffdxf.pas`, ~2040–2068):
  `DXFNODPrePass` → `DXFNODLoadSymbolTables` → `RunNODLoadHandlers` →
  `DXFNODPreserveUnknownBranches` → `RunNODEnsureDefaults` →
  `AddFromDXF20XX` (`TABLES`, `BLOCKS`, `ENTITIES`). То есть модель
  `OBJECTS` (`TIODXFLoadContext.NODModel`, `TZNODModel.FindObject`,
  `uzeffdxfnod.pas:147`) и всё, что загрузили обработчики, **доступны при
  чтении сущностей**.
* Образец разрешения ссылки сущности на объект `OBJECTS` — `ACAD_TABLE`:
  `342` сохраняется строкой (`zcad/velec/acadtable/uzeacadtable_dxf_read.pas:411`)
  и разрешается через таблицу стилей (`GetStyleByHandle`,
  `uzeacadtable_stylemanager.pas:192`).

### 2.3 NOD и сохранение неизвестных веток

* Реестр — `zengine/fileformats/uzeffdxfnodregistry.pas`, запись
  `TZNODHandler` (`:130–155`): `Key`, `ObjectType`, `MinVersion`,
  `DefaultName`, `LoadProc`, `ReserveHandlesProc`, `SaveProc`,
  `ClassesProc`, `FindObjectHandleProc`, `EnsureDefaultsProc`,
  `XDataAppName`, `NeedsSymbolTables`. **Один ключ NOD — один
  обработчик**, для двух ключей нужны два обработчика.
* `RunNODLoadHandlers` «забирает» только словарь ветки; объекты ветки
  обработчик забирает сам (`TZNODModel.ClaimHandle`).
* `ReserveHandlesProc` возвращает хэндл словаря ветки; `0` — ветку пишет
  шаблон (в шаблонах ZCAD ключей `ACAD_IMAGE_*` нет, значит ничего не
  пишется). Отсутствующий в шаблоне ключ добавляется в NOD
  (`TakeNODInsertions`), если хэндл `> 0`.
* Этап 7 ТЗ NOD (`RunNODPreserveUnknownBranches`, `uzeffdxfnodregistry.pas:604`):
  ключи без обработчика сохраняются «как есть». Сейчас так сохраняются
  `ACAD_IMAGE_DICT` и `ACAD_IMAGE_VARS`, и результат **некорректен**:
  * `IMAGEDEF_REACTOR` в ветку не попадает (владелец — сущность);
  * ссылка `IMAGEDEF` на реактор (`330` в `{ACAD_REACTORS`) указывает вне
    ветки и не разрешается при записи (лог `refers to … outside the branch`);
  * сама сущность `IMAGE` при записи становится `INSERT`, так что в файле
    остаётся «осиротевший» `IMAGEDEF`.
  После этапа 1 ключи забирает новый обработчик, и эти ветки больше не
  сохраняются как неизвестные.
* Образцы обработчиков: `zengine/styles/uzestylestablesdxfnod.pas`
  (`ACAD_TABLESTYLE`, 926 строк), `zengine/styles/uzestylesmleaderdxfnod.pas`
  (`ACAD_MLEADERSTYLE`, `MinVersion=AC1021`),
  `zengine/fileformats/uzeffdxfnodzcad.pas` (`ZCAD_DATA`). Модули
  обработчиков подключаются в implementation-`uses` `uzeffdxfout.pas`
  (~63–68) и регистрируются в `initialization`.
* Хранилища данных NOD в чертеже — поля `TSimpleDrawing`
  (`zengine/core/drawings/uzedrawingsimple.pas:66–78`):
  `DXFTableStyleTable`, `DXFMLeaderStyleTable`, `PreservedNODBranches`.

### 2.4 Запись DXF (`zengine/fileformats/uzeffdxfout.pas`)

* Порядок секций шаблона: `HEADER`, `CLASSES`, `TABLES`, **`BLOCKS`**,
  `ENTITIES`, `OBJECTS`.
* `ReserveNODHandles` (`:330–334`: `NODSave.ReserveHandles` +
  `MapTemplateHandles`) вызывается в начале `ENTITIES` (`:492–494`) и
  повторно (идемпотентно) в `OBJECTS` (`:1272–1273`). Сущности блоков
  (`saveentitiesdxf2000` в `BLOCKS`, `:529`) пишутся **раньше**
  резервирования. Следствие: `IMAGE` внутри блока не может взять хэндлы
  `IMAGEDEF`/реактора из `ReserveHandlesProc` — нужен ленивый
  get-or-create (см. 4.4).
* Контекст записи `TIODXFSaveContext` (`zengine/fileformats/uzeffdxfsupport.pas:100–138`)
  живёт всё сохранение (`IODXFContext.InitRec` — `uzeffdxfout.pas:353`,
  `IODXFContext.done` — `uzeffdxfout.pas:1365`). Поле
  `p2h: TMapPointerToHandle` (указатель → хэндл) уже используется для
  предварительного резервирования хэндлов.
* Хэндл сущности (`5`) берётся в `GDBObjEntity.SaveToDXFObjPrefix`
  (`uzeentity.pas:1078–1097`) через
  `IODXFContext.p2h.MyGetOrCreateValue(@self, …)`. Владельца (`330`)
  сущности ZCAD не пишет. Значит, к записи `OBJECTS` хэндлы всех `IMAGE`
  известны, и реактор может корректно сослаться на свою сущность.
* Формата DXF R12 для записи нет (см. 1), `IMAGE` появился в R14 —
  `MinVersion` обработчиков `AC1015` (по умолчанию) достаточно.

### 2.5 Отображение

* Абстрактный рисовальщик `TZGLAbstractDrawer`
  (`zengine/zgl/common/uzgldrawerabstract.pas:31`) умеет только векторы:
  `DrawLine`, `DrawTriangle*`, `DrawQuad*`, `DrawQuad3DInModelSpace` (`:81–82`),
  `DrawContour3DInModelSpace` (`:84`), стенсил. **Растрового примитива нет.**
* Реализации: `TZGLGeneralDrawer` (заглушки, `common/uzgldrawergeneral.pas`),
  `TZGLOpenGLDrawer` (`opengl/uzgldrawerogl.pas`) → `TZGLOpenGLDrawerModern`,
  `TZGLGeneral2DDrawer` (`common/uzgldrawergeneral2d.pas`) →
  `TZGLGDIDrawer` (`gdi/uzgldrawergdi.pas`), `TZGLCanvasDrawer`
  (`canvas/uzgldrawercanvas.pas`), `TZGLDXDrawer` (`dx/uzgldrawerdx.pas`).
* Обёртки OpenGL для текстур уже есть (`myglGenTextures`, `myglTexImage2D`,
  `myglTexCoord2d` в `uzgloglstatemanager.pas`), используются только для
  сохранения/восстановления экранного буфера.
* Декодеров растра в zengine нет (`FPImage`/`FPReadPNG` нигде не
  используются).

### 2.6 DWG (только чтение, LibreDWG)

* Конвейер: `BeginDWGImport` → `ScanDWGImport` (вызывает `DWGNODLoad` до
  сущностей, `uzedwgimport.pas:985`) → `parseDwg_Data` → `EndDWGImport`.
* `uzedwgnod.pas` `ObjectToRaw` (~408–450) переводит в `TZDXFRawObject`
  с телом только `DICTIONARY`, `DICTIONARYWDFLT`, `TABLESTYLE`,
  `MLEADERSTYLE`; прочие объекты — «заглушки» из заголовка. После этого
  работают те же NOD-обработчики, что и для DXF.
* Обработчики сущностей: образец `uzedwgentsolid.pas` —
  `AddSolidEntity(...)` + `RegisterDWGEntityHandler(DWG_TYPE_SOLID, …)` в
  `initialization`; модуль перечисляется в `uses`
  `fileformats/uzefflibredwg2ents.pas` (~52–76). Сейчас `IMAGE` попадает в
  `AddUnknownEntity`, `IMAGEDEF` молча пропускается.
* Структуры LibreDWG (`components/fpdwg/dwg.pp`): `_dwg_entity_IMAGE`
  (~5903–5922: `pt0`, `uvec`, `vvec`, `image_size`, `display_props`,
  `clipping`, `brightness`, `contrast`, `fade`, `clip_mode`,
  `clip_boundary_type`, `num_clip_verts`, `clip_verts`, `imagedef`,
  `imagedefreactor`), `_dwg_object_IMAGEDEF` (~5929–5938: `image_size`,
  `file_path`, `is_loaded`, `resunits`, `pixel_size`),
  `_dwg_object_RASTERVARIABLES` (~6017–6024), константы `DWG_TYPE_IMAGE`,
  `DWG_TYPE_IMAGEDEF`, `DWG_TYPE_IMAGEDEF_REACTOR` (~625–626),
  `DWG_TYPE_RASTERVARIABLES` (~649).

### 2.7 Копирование и буфер обмена

* `TZCADDrawingsManager.CopyEnt` (`zcad/core/drawings/uzcdrawings.pas:817`)
  клонирует сущность и вызывает `RemapAll` (`:797`), где по типу сущности
  переносятся стили в чертёж-приёмник (`createtstyleifneed` и т. п.).
* Буфер обмена: `CopyClip_com` сохраняет выделение как DXF
  (`uzccommand_copyclip.pas:65`), вставка — `addfromdxf`
  (`uzccommand_pasteclip.pas`). Для `IMAGE` это работает, только если
  полностью работает DXF round-trip.

---

## 3. Границы задачи и жёсткие ограничения

1. **Старую систему загрузки/записи трогать минимально.** Допустимые
   изменения существующего кода перечислены явно в этапах (регистрация
   модулей в `uses`, новые поля в `TSimpleDrawing` и `TIODXFSaveContext`,
   ветки в `ObjectToRaw`, `RemapAll`, новый виртуальный метод рисовальщика).
   Всё остальное — в новых модулях.
2. Объекты `OBJECTS` (`ACAD_IMAGE_DICT`, `IMAGEDEF`, `IMAGEDEF_REACTOR`,
   `RASTERVARIABLES`) читаются и пишутся **только через NOD-обработчики**
   (`RegisterNODHandler`). Никаких новых ad-hoc проходов по
   `RawObjectsSection` и `RegisterObjectsSaveDxfProc`.
3. Символьные таблицы и шаблоны `savetemplate*.dxf` не меняются. Классы и
   ключи NOD для растров добавляет обработчик.
4. `zengine` не зависит от `zcad` (слои). Декодирование растра — на
   `fcl-image` (`FPImage`, `FPReadPNG`, `FPReadJPEG`, `FPReadBMP`), не на
   LCL `TBitmap`, чтобы модель и тесты собирались с `-dLCLnogui`.
5. DWG — **только чтение**. Запись DWG в ZCAD отсутствует и в задачу не
   входит; чертёж, открытый из DWG, сохраняется в DXF.
6. Сам растровый файл не встраивается в DXF и не копируется: хранится и
   пишется путь (`IMAGEDEF.1`) как в исходном файле.
7. Вне рамок (отдельные задачи): команда вставки изображения
   (аналог `IMAGEATTACH`), команды `IMAGECLIP`/`IMAGEADJUST`,
   диспетчер внешних ссылок, `WIPEOUT` (та же структура, что у `IMAGE`,
   но другой смысл), OLE (`OLE2FRAME`), экспорт в SVG/PDF.
8. Лог — через `programlog.LogOutFormatStr(…, LM_Info)`, трасса NOD —
   `NODLogTraceFormatStr` (выключена по умолчанию). Комментарии и сообщения
   — на русском, модули ≤ 700–1000 строк, функции ≤ 30 строк, без
   «магических чисел» (коды групп, флаги и значения по умолчанию —
   именованные константы).

---

## 4. Целевая архитектура

```
DXF/DWG ──► NOD pre-pass / DWGNODLoad ──► обработчики ACAD_IMAGE_DICT, ACAD_IMAGE_VARS
                                               │
                                               ▼
                      TSimpleDrawing.ImageDefTable (TGDBImageDef[]) + RasterVariables
                                               ▲  указатель
ENTITIES/BLOCKS ──► GDBObjImage.LoadFromDXF ───┘  (разрешение 340 по хэндлу)

Запись: CLASSES (ClassesProc) → BLOCKS/ENTITIES (IMAGE, ленивые хэндлы 340/360)
        → OBJECTS (SaveProc: словарь, IMAGEDEF c реакторами, IMAGEDEF_REACTOR, RASTERVARIABLES)

Отображение: GDBObjImage.DrawGeometry → рамка (векторы)
             + TZGLAbstractDrawer.DrawImage3DInModelSpace(углы, растр) → кэш растров (fcl-image)
```

### 4.1 Новые модули

| Модуль | Назначение | Этап |
|---|---|---|
| `zengine/styles/uzestylesimagedef.pas` | `TGDBImageDef = object(GDBNamedObject)` и `GDBImageDefArray` (по образцу `uzestylesmleaderdxf.pas`), запись `TZRasterVariables`. Только данные, без DXF. | 1 |
| `zengine/styles/uzestylesimagedefdxfnod.pas` | Два NOD-обработчика: `ACAD_IMAGE_DICT` (`IMAGEDEF`) и `ACAD_IMAGE_VARS` (`RASTERVARIABLES`); чтение — этап 1, запись и классы — этап 2. | 1–2 |
| `zengine/core/entities/uzeentimage.pas` | Сущность `GDBObjImage` (DXF-имя `IMAGE`), чтение/запись DXF, рамка, ручки, преобразования. | 1–2 |
| `zengine/core/utils/uzerasterimages.pas` | Разрешение пути, загрузка и декодирование файла (`fcl-image`), кэш растров (ключ — полный путь + время изменения), яркость/контраст/затухание. Без LCL. | 3 |
| `zengine/fileformats/dwg/entities/uzedwgentimage.pas` | Обработчик DWG-сущности `IMAGE`. | 4 |

Если `uzeentimage.pas` превысит ~1000 строк — вынести DXF-часть в
`uzeentimagedxf.pas` (как `uzeacadtable_dxf_read/write`).

### 4.2 Модель данных (описание, не код)

`TGDBImageDef` (имя = ключ записи в `ACAD_IMAGE_DICT`):

* `FilePath: string` — `1`, как в файле (относительный `.\x.png` или
  абсолютный), без нормализации при хранении;
* `PixelCountX/Y` — `10/20` (double в DXF, целые по смыслу);
* `PixelSizeX/Y` — `11/21`, размер пикселя по умолчанию в единицах чертежа;
* `IsLoaded` — `280`; `ResolutionUnits` — `281` (`0`/`2`/`5`);
* `ClassVersion` — `90`;
* `DXFHandle: string` — хэндл из исходного файла (только для разрешения
  ссылок при загрузке, как `DXFHandle` у стилей таблиц);
* кэшированный растр (указатель на запись кэша этапа 3), не сохраняется.

`TZRasterVariables`: `Present: Boolean` (была ли в файле), `ClassVersion`
(`90`), `ImageFrame` (`70`), `ImageQuality` (`71`), `Units` (`72`).
Значения по умолчанию для чертежа без `ACAD_IMAGE_VARS`: `70=1`, `71=1`,
`72=0`.

`GDBObjImage` (все координаты — WCS):

* `InsertPoint` (`10/20/30`), `UVector` (`11/21/31`), `VVector` (`12/22/32`),
  `ImageSizeX/Y` (`13/23`);
* `PImageDef: PGDBImageDef` — указатель на определение в чертеже
  (`nil` — ссылка не разрешена); `ImageDefHandle: string` — исходный `340`
  до разрешения;
* `DisplayFlags` (`70`), `ClippingOn` (`280`), `Brightness`/`Contrast`/`Fade`
  (`281/282/283`, по умолчанию `50/50/0`), `ClassVersion` (`90`);
* `ClipBoundaryType` (`71`: `1` прямоугольник, `2` многоугольник),
  `ClipVertices` (`14/24`, как в файле, в пиксельных координатах),
  `ClipMode` (`290`, `AC1024+`);
* вычисляемые: 4 угла в WCS, контур рамки (с учётом подрезки), габарит.

Геометрия (одна функция, покрыта тестом):

* углы: `P0`, `P0+W·U`, `P0+W·U+H·V`, `P0+H·V`;
* пиксель → WCS: `P0 + (x+0.5)·U + (H−y−0.5)·V` (начало пиксельных
  координат — левый верхний угол, см. раздел 1);
* `transform`/`TransformAt`: `P0' = T·P0`, `U' = T·(P0+U) − P0'`,
  `V' = T·(P0+V) − P0'` (так переносятся поворот, масштаб и зеркало).

### 4.3 Контракт NOD-обработчиков

| Процедура | `ACAD_IMAGE_DICT` | `ACAD_IMAGE_VARS` |
|---|---|---|
| `LoadProc` | Для каждой записи словаря: `IMAGEDEF` → `TGDBImageDef` в `ImageDefTable`, `ClaimHandle` словаря-записи и объекта; реакторы (`IMAGEDEF_REACTOR`) найти в модели по ссылкам `{ACAD_REACTORS` и тоже забрать (`ClaimHandle`) — они будут пересозданы при записи. Не `IMAGEDEF` в записи — предупреждение, объект не забирается (уйдёт в сохранённые ветки этапа 7). | `RASTERVARIABLES` → `TSimpleDrawing.RasterVariables` (`Present=True`), `ClaimHandle`. |
| `ReserveHandlesProc` | `ImageDefTable` пуст → `0` (ключ не пишется); иначе хэндл словаря из `IODXFContext.handle`. | `RasterVariables.Present` или `ImageDefTable` не пуст → хэндл объекта; иначе `0`. |
| `SaveProc` | Словарь (`102 {ACAD_REACTORS 330 <NOD>}`, `330 <NOD>`, `281 1`, пары `3/350`), затем все `IMAGEDEF` с реакторами, затем все `IMAGEDEF_REACTOR` (см. 4.4). | `RASTERVARIABLES`. |
| `ClassesProc` | Классы `IMAGEDEF`, `IMAGEDEF_REACTOR`, `IMAGE` (если определения есть и класса нет в `TemplateClassNames`). | Класс `RASTERVARIABLES`. |
| `FindObjectHandleProc` | Имя определения → его хэндл (для сохранённых веток, ссылающихся на `IMAGEDEF`). | — |
| `EnsureDefaultsProc` | — | — |
| `MinVersion` | `AC1015` | `AC1015` |

Особенность `ACAD_IMAGE_VARS`: значение ключа — сам объект, а не словарь;
`ReserveHandlesProc` возвращает хэндл `RASTERVARIABLES`, и NOD writer
пишет его в пару `3/350` так же, как хэндл словаря.

### 4.4 Хэндлы при записи (ленивое резервирование)

Так как `BLOCKS` пишется до `ReserveNODHandles` (2.4), хэндлы, на которые
ссылается `IMAGE`, берутся лениво:

* хэндл `IMAGEDEF`: `IODXFContext.p2h.MyGetOrCreateValue(PImageDef, …)` —
  один и тот же при вызове из сущности (`340`) и из `SaveProc`;
* хэндл реактора: при записи каждой `IMAGE` выделяется новый хэндл из
  `IODXFContext.handle`, и в новое поле контекста
  `ImageReactors` (список записей `{хэндл сущности, хэндл реактора,
  указатель определения}`) добавляется запись; `IMAGE` пишет его в `360`;
* `SaveProc` в `OBJECTS` берёт из `ImageReactors` реакторы каждого
  определения (для `{ACAD_REACTORS` в `IMAGEDEF`) и пишет сами
  `IMAGEDEF_REACTOR` (`330 <сущность>`, `100 AcDbRasterImageDefReactor`,
  `90 2`, `330 <сущность>`).

Новое поле `TIODXFSaveContext` — единственное изменение контекста; тип
списка объявить в `uzeffdxfsupport.pas` через `Pointer`, чтобы не тянуть
`uzestylesimagedef` в модуль поддержки. Инициализация/освобождение — в
`InitRec`/`done`.

---

## 5. Этапы реализации

### Этап 1. Модель, сущность `IMAGE` и чтение DXF (рамка)

Цель: `IMAGE` из DXF загружается как полноценная сущность с верной
геометрией и ссылкой на определение, видна на экране рамкой,
выделяется, двигается, поворачивается, копируется.

1. `uzeconsts.pas`: `GDBImageID` (следующее свободное значение
   стандартного блока, `20`) и `ObjN_GDBObjImage`.
2. `uzestylesimagedef.pas` (4.1, 4.2); в `TSimpleDrawing` — поля
   `ImageDefTable` и `RasterVariables` с `init`/`done` (не повторить
   дефект `DXFTableStyleTable` из этапа 0 ТЗ NOD) и метод доступа
   `GetImageDefTable`.
3. `uzestylesimagedefdxfnod.pas`: регистрация двух обработчиков, только
   `LoadProc` (4.3). Добавить модуль в implementation-`uses`
   `uzeffdxfout.pas` рядом с остальными обработчиками.
4. `uzeentimage.pas`: `GDBObjImage = object(GDBObj3d)` по образцу
   `GDBObj3DFace` + методы выделения/ручек по образцу `GDBObjSolid`:
   * `LoadFromDXF`: цикл по группам как в `GDBObjSolid.LoadFromDXF`
     (`LoadFromDXFObjShared` → свои группы → `rdr.ParseString`);
     `92/310` пропускаются; `14/24` — в список вершин;
   * разрешение `340`: поиск в `ImageDefTable` по `DXFHandle`; если не
     найдено, но `context.NODModel.FindObject(340)` — `IMAGEDEF` (файл без
     ключа `ACAD_IMAGE_DICT` в NOD), создать определение из этого объекта
     (имя — имя файла без расширения, уникализировать) и забрать хэндл;
     иначе `PImageDef=nil` и предупреждение в лог;
   * `FormatEntity`: углы, контур рамки (прямоугольник или многоугольник
     подрезки при `280=1` и бите `4` в `70`), габарит;
   * `DrawGeometry`: контур рамки через `DrawContour3DInModelSpace`
     (рамка рисуется всегда, пока нет растра — этап 3);
   * `onmouse`/`CalcTrueInFrustum` по 4 углам, 4 ручки (`CPA_Strech`)
     по углам, `rtmodifyonepoint` пересчитывает `P0/U/V` так, чтобы
     прямоугольник сохранялся (перемещение угла = масштаб относительно
     противоположного угла);
   * `transform`/`TransformAt` (4.2), `Clone`, `rtsave`,
     `GetObjTypeName`, `CreateInstance`, `GetObjType`;
   * `SaveToDXF` на этом этапе **ничего не пишет** и выводит
     предупреждение в лог (запись — этап 2). Это не хуже текущего
     поведения: сейчас изображение при записи тоже теряется, остаётся
     в лучшем случае рамка из proxy-графики (2.2);
   * `RegisterDXFEntity(GDBImageID,'IMAGE','Image',…)`.
5. Подключить модули в `zcad.pas`/`zcad.lpi` (рядом с `uzeentleader`).
6. `RemapAll` (`uzcdrawings.pas:797`): для `GDBImageID` — найти или
   создать в чертеже-приёмнике определение с тем же именем и путём и
   перенаправить `PImageDef` (иначе копия ссылается на определение чужого
   чертежа).
7. Лог: `IMAGE: def "%s" (%s) resolved`, `IMAGE %s: IMAGEDEF %s not found`,
   `ACAD_IMAGE_DICT: %d definitions loaded` (трасса NOD).

Приёмка:

* `acadtableandhrefImage2007.dxf` открывается, `IMAGE` — сущность
  `GDBObjImage`, а не proxy; определение `testimage` с путём
  `.\testimage.png`, размером `248×72`, `281=2`;
* углы рамки совпадают с расчётом (`P0=(124.9464…,13.5135…)`,
  `U=V=0.10418…`, ширина `248·0.10418…`, высота `72·0.10418…`) с точностью
  `1e-9`;
* ключи `ACAD_IMAGE_DICT`/`ACAD_IMAGE_VARS` больше не попадают в
  `PreservedNODBranches`, в логе нет `outside the branch` для `IMAGEDEF`;
* перемещение, поворот, зеркало, копирование, отмена — рамка корректна;
* тест `imagestage1` (раздел 6) проходит.

### Этап 2. Запись DXF (round-trip)

Цель: чертёж с изображениями сохраняется в DXF 2000/2007 так, что
AutoCAD открывает его без ошибок `AUDIT`, а ZCAD читает обратно без потерь.

1. `TIODXFSaveContext.ImageReactors` (4.4) — инициализация в `InitRec`,
   освобождение в `done`.
2. `GDBObjImage.SaveToDXF`: `SaveToDXFObjPrefix(…,'IMAGE','AcDbRasterImage',…)`,
   затем `90`, `10/20/30`, `11/21/31`, `12/22/32`, `13/23`, `340`
   (хэндл определения, 4.4), `70`, `280`, `281`, `282`, `283`, `360`
   (новый реактор), `71`, `91`, пары `14/24`, `290` (только
   `AC1024+` и только если было прочитано). Порядок групп — как в эталоне
   AutoCAD (раздел 8). Если `PImageDef=nil` — сущность не пишется,
   предупреждение в лог (без определения AutoCAD считает `IMAGE` ошибкой).
3. `uzestylesimagedefdxfnod.pas`: `ReserveHandlesProc`, `SaveProc`,
   `ClassesProc`, `FindObjectHandleProc` (4.3):
   * `IMAGEDEF`: `0 IMAGEDEF`, `5`, `102 {ACAD_REACTORS`, `330 <словарь>`,
     `330 <реактор>` для каждой сущности этого определения, `102 }`,
     `330 <словарь>`, `100 AcDbRasterImageDef`, `90`, `1`, `10/20`, `11/21`,
     `280`, `281`;
   * `RASTERVARIABLES`: `5`, `102 {ACAD_REACTORS 330 <NOD> 102 }`,
     `330 <NOD>`, `100 AcDbRasterVariables`, `90`, `70`, `71`, `72`;
   * запись класса — локальная процедура модуля (приложение `ISM`,
     параметры `90`/`280`/`281` из раздела 1); общий `WriteClassRecord`
     не менять;
   * определения без сущностей пишутся (AutoCAD допускает
     «неиспользуемые» определения).
4. Кодировка пути `1`: через `dxfStringout` (как прочие строки), при
   чтении — декодирование, как у прочих строк NOD.

Приёмка:

* round-trip `acadtableandhrefImage2007.dxf` в DXF 2000 и 2007:
  загрузка → запись → загрузка даёт те же определения, геометрию,
  флаги, контур подрезки;
* в записанном файле выполняются инварианты 6.2.3 (в том числе
  «растровые»);
* `IMAGE` внутри блока (синтетический DXF) получает верные `340/360`;
* две сущности на одно определение — у `IMAGEDEF` два реактора;
* чертёж без изображений пишется байт-в-байт как до этапа (сравнение с
  эталоном `roundtrip*`);
* ручная проверка: AutoCAD / ODA File Converter открывает файл,
  `AUDIT` — 0 ошибок, изображение на месте (результат — в описании PR).

### Этап 3. Отображение растра

Цель: изображение видно на экране (OpenGL и 2D-рисовальщики), с учётом
яркости, контраста, затухания, подрезки и `IMAGEFRAME`.

1. `uzerasterimages.pas`:
   * разрешение пути (по порядку): как есть, если абсолютный и
     существует → относительно папки чертежа (`GetFileName`) →
     имя файла в папке чертежа → подпапка `images\`; разделители `\`
     и `/` нормализуются; результат не записывается обратно в `FilePath`;
   * декодирование PNG/JPEG/BMP через `fcl-image` в RGBA-буфер; прочие
     форматы (TIFF и т. п.) — «не загружено», предупреждение один раз на
     файл;
   * кэш по полному пути (+ время изменения файла), общий для чертежей;
     ограничение размера текстуры (при превышении максимального размера
     — уменьшение);
   * коррекция: контраст `c = Contrast/50`, яркость
     `b = (Brightness−50)/50`, затухание `f = Fade/100`:
     `v' = clamp((v−0.5)·c + 0.5 + b)`, затем смешение с цветом фона
     `v'' = v'·(1−f) + bg·f` (при `50/50/0` — без изменений);
   * прозрачность: альфа PNG учитывается при бите `8` в `70`.
2. Рисовальщик: новый виртуальный метод
   `DrawImage3DInModelSpace(const p1,p2,p3,p4:TzePoint3d; AImage:Pointer;
   var matrixs:tmatrixs)` в `TZGLAbstractDrawer`; в `TZGLGeneralDrawer` —
   запасной вариант (ничего не делает, рамку рисует сущность).
   Реализации:
   * `TZGLOpenGLDrawer` (и `Modern`, если не наследует): текстура на
     квад, идентификатор текстуры кэшируется в записи кэша растров для
     контекста;
   * `TZGLGeneral2DDrawer` (GDI и LCL Canvas): проекция 4 углов в экран;
     для прямоугольника без поворота/сдвига — растяжение растра, иначе —
     аффинное отображение; если backend не умеет аффинно — рамка
     (с записью в лог один раз);
   * `TZGLDXDrawer` — запасной вариант (рамка), вне приёмки этапа.
3. `GDBObjImage.DrawGeometry`: при `70` бит `1` и загруженном растре —
   `DrawImage3DInModelSpace`; рамка — по `RasterVariables.ImageFrame`
   (`0` — нет, `1`/`2` — есть); если растр не загружен — рамка всегда.
4. Подрезка: прямоугольная — через текстурные координаты; многоугольная —
   через стенсил (OpenGL) / область отсечения (2D); если не поддержано —
   подрезка по габариту многоугольника и предупреждение.
5. Инспектор объектов: минимальный набор свойств по образцу
   `zcad/register/uzcregleader.pas` (`RegisterPhysMultiproperty`):
   имя определения и путь (только чтение), точка вставки, ширина, высота,
   угол поворота, яркость, контраст, затухание, видимость.

Приёмка:

* `testimage.png` отображается на месте рамки в OpenGL и LCL Canvas,
  без зеркального отражения по Y (сравнение скриншотов в описании PR);
* повёрнутое/отмасштабированное изображение отображается верно;
* отсутствующий файл — только рамка, чертёж открывается;
* `brightness/contrast/fade` меняют изображение (скриншоты);
* юнит-тест разрешения пути и коррекции пикселей (`imagestage3`)
  собирается с `-dLCLnogui` без GUI.

### Этап 4. DWG (чтение)

Цель: изображения из DWG (и «DXF via LibreDWG») загружаются так же, как из
DXF; сохранение — в DXF по этапу 2.

1. `uzedwgnod.pas` `ObjectToRaw`: ветки для `IMAGEDEF` (группы `90`, `1`,
   `10/20`, `11/21`, `280`, `281`, реакторы) и `RASTERVARIABLES`
   (`90`, `70`, `71`, `72`) — после этого работают обработчики этапа 1
   без изменений. `IMAGEDEF_REACTOR` — заглушка из заголовка (достаточно
   хэндла и владельца).
2. `uzedwgentimage.pas`: `AddImageEntity` по образцу `uzedwgentsolid.pas`,
   поля из `_dwg_entity_IMAGE` (2.6); ссылка `imagedef` — через её
   абсолютный хэндл и поиск в `ImageDefTable` по `DXFHandle`;
   `RegisterDWGEntityHandler(DWG_TYPE_IMAGE, …)`; модуль — в `uses`
   `uzefflibredwg2ents.pas`.
3. Если классы растров в DWG имеют переменный номер типа (объекты
   «класса», `>= 500`), определять тип по `dxfname`, как уже делает
   `ObjectToRaw`.

Приёмка:

* тест `imagestage4` на `Dwg_Data`, построенном в памяти (как
  `TDWGFixture` в `nodstage9.lpr`): `IMAGE` + `IMAGEDEF` + словарь →
  определение и сущность с той же геометрией, что в DXF-тесте этапа 1;
* ручная проверка: DWG из AutoCAD с изображением открывается, рамка и
  растр на месте, сохранение в DXF проходит проверки этапа 2.

---

## 6. Тестирование

### 6.1 Входные файлы

| Файл | Что проверяет |
|---|---|
| `DXFTableSaveNEW/acadtableandhrefImage2007.dxf` + `testimage.png` | Эталон AutoCAD (`AC1021`): `IMAGE B88`, `IMAGEDEF B86`, реактор `B87`, словарь `B84`, `RASTERVARIABLES B85`, 4 класса `ISM`. Скопировать в `cad_source/zengine/tests/data/image/`. |
| Синтетические DXF в `cad_source/zengine/tests/data/image/` | `IMAGE` в блоке; два `IMAGE` на одно определение; два определения; многоугольная подрезка; несимметричная прямоугольная подрезка; `IMAGE` с несуществующим `340`; `IMAGEDEF` без ключа в NOD; нет `ACAD_IMAGE_VARS`; повёрнутое изображение (`U`/`V` не по осям). |
| Чертёж без изображений (`savetemplate*.dxf`, эталоны `roundtrip*`) | Запись не изменилась. |

### 6.2 Виды тестов

Тесты — консольные программы `cad_source/zengine/tests/imagestageN.lpr`
(+ `.lpi`) по образцу `nodstageN.lpr`, сборка `fpc -Mdelphi -dLCLnogui`.
В `nodtests.sh` и в `Makefile` (переменная `NODTESTS`) шаблон поиска
расширить на `imagestage*.lpr`, тогда они выполняются в CI
(`.github/workflows/nodtests.yml`) без нового workflow.

1. **Модель и чтение** (этап 1): число определений, имена, пути, размеры;
   геометрия углов; разрешение `340`; поведение при битой ссылке;
   `transform` (перенос, поворот 90°, зеркало) — углы до/после.
2. **Round-trip** (этап 2): загрузка → запись → загрузка, сравнение
   модели (не байтов — хэндлы перенумеровываются).
3. **Инварианты ссылочной целостности** записанного файла (по модели
   `TZNODModel`), дополнительно к инвариантам ТЗ NOD:
   * каждый `IMAGE.340` указывает на `IMAGEDEF`, а `IMAGE.360` — на
     `IMAGEDEF_REACTOR`, у которого оба `330` — этот `IMAGE`;
   * у каждого `IMAGEDEF` в `{ACAD_REACTORS` — словарь `ACAD_IMAGE_DICT` и
     ровно реакторы сущностей, ссылающихся на него; `330` — словарь;
   * словарь `ACAD_IMAGE_DICT` и `RASTERVARIABLES` — записи NOD;
   * все 4 класса растров есть в `CLASSES`, если в файле есть `IMAGEDEF`.
4. **Пиксельная математика** (этап 3): разрешение пути (временная папка),
   коррекция яркости/контраста/затухания на известных значениях.
5. **DWG** (этап 4): `Dwg_Data` в памяти.
6. **Ручная проверка** в AutoCAD / ODA File Converter + `AUDIT` на этапах
   2 и 4; скриншоты отображения на этапе 3 — в описании PR.

---

## 7. Риски и как их снять

| # | Риск | Мера |
|---|---|---|
| R1 | Порча записи чертежей без изображений. | Обработчики возвращают `0` из `ReserveHandlesProc`, если определений нет; классы не пишутся; сравнение с эталонами `roundtrip*` (этап 2). |
| R2 | `IMAGE` в блоке пишется до `ReserveNODHandles` — неверные `340/360`. | Ленивые хэндлы через `p2h` и `ImageReactors` (4.4); тест «`IMAGE` в блоке». |
| R3 | Расхождение реакторов `IMAGEDEF` ↔ `IMAGEDEF_REACTOR` → ошибки `AUDIT`. | Реакторы никогда не копируются из исходного файла, а строятся при записи из фактически записанных сущностей; инвариант 6.2.3. |
| R4 | Этап 7 NOD продолжает сохранять ключи `ACAD_IMAGE_*` как неизвестные → дубли в NOD. | Обработчики зарегистрированы — ветки забираются; тест: `PreservedNODBranches` не содержит ключей `ACAD_IMAGE_*`. |
| R5 | Неверная ориентация пиксельных координат подрезки (раздел 1). | Одна функция «пиксель → WCS», тест на несимметричной подрезке из AutoCAD. |
| R6 | Зависимость zengine от LCL из-за декодера. | `fcl-image` (ограничение 4); тест `imagestage3` с `-dLCLnogui`. |
| R7 | Большие растры: память, размер текстуры. | Кэш с общим владением, уменьшение до максимального размера текстуры, загрузка по требованию (при первом рисовании). |
| R8 | Относительный путь ломается при «Сохранить как» в другую папку. | Как в AutoCAD: путь не переписывается; разрешение пути (этап 3) дополнительно ищет файл рядом с чертежом. Переписывание путей — отдельная задача. |
| R9 | Копирование между чертежами / буфер обмена теряет определение. | `RemapAll` (этап 1, п. 6); буфер обмена работает через DXF round-trip этапа 2. |
| R10 | Разрастание этапа 3 из-за 5 рисовальщиков. | В приёмке — только OpenGL и LCL Canvas; остальные — запасной вариант (рамка). |

---

## 8. Глоссарий DXF-групп, используемых в ТЗ

Порядок групп — как в эталоне AutoCAD `acadtableandhrefImage2007.dxf`.

**`IMAGE`** (`ENTITIES`/`BLOCKS`, `100 AcDbEntity`, затем `100 AcDbRasterImage`):

| Код | Значение |
|---|---|
| `92/310` | Proxy-графика (пропускается). |
| `90` | Версия класса (`0`). |
| `10/20/30` | Точка вставки `P0` (левый нижний угол изображения), WCS. |
| `11/21/31` | Вектор `U` — шаг одного пикселя вдоль ширины, WCS. |
| `12/22/32` | Вектор `V` — шаг одного пикселя вдоль высоты, WCS. |
| `13/23` | Размер изображения в пикселях `W/H`. |
| `340` | Hard-pointer на `IMAGEDEF`. |
| `70` | Флаги: `1` показывать, `2` показывать невыровненным, `4` использовать подрезку, `8` прозрачность. |
| `280` | Подрезка включена (`0`/`1`). |
| `281/282/283` | Яркость / контраст / затухание (`0..100`, по умолчанию `50/50/0`). |
| `360` | Hard-owner на `IMAGEDEF_REACTOR`. |
| `71` | Тип контура подрезки: `1` прямоугольник, `2` многоугольник. |
| `91` | Число вершин контура. |
| `14/24` | Вершина контура в пиксельных координатах. |
| `290` | Режим подрезки (`AC1024+`): `0` снаружи, `1` внутри. |

**`IMAGEDEF`** (`OBJECTS`, `100 AcDbRasterImageDef`): `90` версия класса,
`1` путь к файлу, `10/20` размер в пикселях, `11/21` размер пикселя в
единицах, `280` загружено, `281` единицы разрешения (`0` нет, `2` см,
`5` дюймы); владелец `330` — словарь `ACAD_IMAGE_DICT`; в
`{ACAD_REACTORS` — словарь и все реакторы.

**`IMAGEDEF_REACTOR`** (`OBJECTS`, `100 AcDbRasterImageDefReactor`):
`330` владелец — сущность `IMAGE`; `90` версия класса (`2`); `330` —
сущность `IMAGE`.

**`RASTERVARIABLES`** (`OBJECTS`, ключ NOD `ACAD_IMAGE_VARS`,
`100 AcDbRasterVariables`): `90` версия класса, `70` IMAGEFRAME (`0`/`1`/`2`),
`71` качество (`0` черновое, `1` высокое), `72` единицы вставки.

**`CLASS`** (`CLASSES`): `1` DXF-имя, `2` C++-имя, `3` приложение (`ISM`),
`90` флаги proxy, `91` число экземпляров (`AC1018+`), `280` был proxy,
`281` класс сущности (`1` только у `IMAGE`).
