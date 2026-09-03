# loaderDWG: загрузка handle и гипотеза по переполнению после FFFF

## Краткий вывод

В текущей Pascal-привязке LibreDWG handle не ограничен четырьмя hex-символами. Базовый тип `BITCODE_HV` объявлен как `BITCODE_RLLx`, а `Dwg_Handle.value` хранит именно это значение (`cad_source/components/fpdwg/dwg.pp:287`, `cad_source/components/fpdwg/dwg.pp:1292-1298`). В загрузчике ZCAD ключ handle тоже хранится как `QWord` (`cad_source/zengine/fileformats/dwg/uzedwgtypes.pas:30`).

Симптом "после FFFF адресация начинается сначала" лучше объясняется не ограничением типа, а тем, что один из путей чтения ссылок (`absolute_ref` у `BITCODE_H`) может содержать только младшие 16 бит. Если такое значение безусловно записать поверх полного `Dwg_Object.handle.value`, последовательность после `FFFF` превращается в `0`, `1`, `2` и далее. Это ломает регистрацию объектов в handle-map: поздние объекты попадают в ветку duplicate и не получают собственную оболочку/ссылки.

Сейчас в коде уже есть защита от этого сценария:

- `DWGNormalizeObjectHandles` расширяет handle только если `absolute_ref` больше текущего `obj.handle.value`, но не сужает полный handle до младших бит (`cad_source/components/fpdwg/uzedwghandle.pas:305-345`).
- `DWGRefHandleCandidatesValue` ставит `Ref^.obj^.handle.value` первым кандидатом, затем `absolute_ref`, затем `handleref.value`, что повторяет поведение `fpdwginspect` (`cad_source/components/fpdwg/uzedwghandle.pas:372-399`).
- Регрессии закрыты тестами `ResolvedObjectHandleWinsOverAbsoluteRef`, `RefHandleCandidatesPreferResolvedObjectHandle`, `ObjectHandleNormalizeUsesObjectRefAbsoluteRefs` и `ObjectHandleNormalizePreservesFullHandleAgainstTruncatedRef` (`cad_source/zengine/fileformats/dwg/tests/uzedwgtestdwgproc.pas:309-451`).

## Дополнение по issue #1221

Новые данные из `zcad.log`, `.dwg.owners.csv`, `.dwg.refs.csv` и `.dwg.handles.csv` показывают, что после уже добавленной защиты от `absolute_ref -> obj.handle.value` проблема проявляется раньше: в raw scan сам `Dwg_Object.handle.value` приходит как `FFFF, 0, 1, 2...`, а `handles.csv` заканчивается на `FFFF`. Это означает, что handle ломается до регистрации shell-ов и resolver-а; side files только фиксируют уже поврежденный raw-поток.

Самая сильная проверяемая гипотеза для #1221 - ZCAD и `fpdwginspect` фактически используют разные бинарники LibreDWG:

- `fpdwginspect` лежит рядом с `cad_source/components/fpdwg/fpdwginspect/libredwg-0.dll` (`sha256=20cf92d51abc8c27f1af623f68aa4f4d90cf4993737ceac4bc482b237f1f781b`, PE timestamp `2026-03-18`).
- Runtime ZCAD для `x86_64-win64` брал `environment/runtimefiles/x86_64-win64/common/bin/libredwg-0.dll` (`sha256=0e3302aec058b0c4d28554ed8ad8317b473c3f6bd67e1d0262f22668b57262d0`, PE timestamp `2024-02-26`).
- `Makefile` при сборке/подготовке окружения копирует `environment/runtimefiles/$(ZPLATFORM)/common/*` в рабочий `cad/bin`, а `fpdwginspect --lib=libredwg-0.dll` обычно берет DLL из своего каталога. Поэтому ручное сравнение могло идти через новый LibreDWG в инспекторе и старый LibreDWG в основном ZCAD.

План устранения для #1221:

1. Синхронизировать `environment/runtimefiles/x86_64-win64/common/bin/libredwg-0.dll` с тем DLL, который уже используется `fpdwginspect` и читает проблемный DWG корректно.
2. Пересобрать/обновить runtime ZCAD, чтобы `LoadLibrary('libredwg-0.dll')` получил новый DLL из `cad/bin`.
3. Повторить загрузку с `ZCAD_DWG_DIAG=trace` и `ZCAD_DWG_TARGET_HANDLES=A325E`.
4. Проверить, что raw trace больше не содержит переход `FFFF -> 0 -> 1`, а показывает полные handle выше `FFFF` (`10000`, `10001`, ... или фактический `A325E`).
5. Проверить side files: `.dwg.handles.csv` должен содержать handle выше `FFFF`, а предупреждения `Raw scan: duplicate handle 1/2/...` должны исчезнуть для последовательности после `FFFF`.
6. Если после синхронизации DLL проблема сохранится, следующий шаг - не "дорисовывать" старшие биты в Pascal по монотонности массива, а сначала добавить в лог точную версию/путь загруженного LibreDWG и сравнить raw output с внешним `dwgread` на том же DLL. Искусственное восстановление `10000 + low16` можно делать только как последний fallback, потому что DWG handle не обязан быть строго непрерывным во всех файлах.

## Где загружаются и используются handle

### Низкоуровневые структуры LibreDWG

- `cad_source/components/fpdwg/dwg.pp:287`: `BITCODE_HV = BITCODE_RLLx`. Это значение handle, а не строка фиксированной длины.
- `cad_source/components/fpdwg/dwg.pp:1292-1298`: `Dwg_Handle.value` имеет тип `BITCODE_HV`.
- `cad_source/components/fpdwg/dwg.pp:1318-1327`: `Dwg_Object_Ref` содержит три источника ссылки: `obj`, `handleref` и `absolute_ref`.
- `cad_source/components/fpdwg/dwg.pp:14820-14857`: `Dwg_Data` хранит массив `&object`, таблицу `object_ref` и `object_ordered_ref`.

### Вход в импорт DWG в ZCAD

- `cad_source/zengine/fileformats/uzefflibredwg.pas:73-126`: `addfromdwg` вызывает `dwg_read_file`, затем открывает контекст импорта `BeginDWGImport`, выполняет `ScanDWGImport`, запускает `GetDWGParser.parseDwg_Data` и завершает `EndDWGImport`.
- `cad_source/zengine/fileformats/dwg/uzedwgimport.pas:760-813`: `BeginDWGImport` создает `TDWGZCADLoadContext`, настраивает fallback-объекты и регистрирует корневой model-space под handle `0`.
- `cad_source/zengine/fileformats/dwg/uzedwgimport.pas:823-880`: `ScanDWGImport` читает header-handle, запускает raw scan и пишет сводку по количеству объектов, refs и зарегистрированных handles.

### Функции чтения handle

- `cad_source/components/fpdwg/uzedwghandle.pas:290-293`: `DWGObjectHandleValue` возвращает `Obj.handle.value`. Это основной ключ объекта.
- `cad_source/components/fpdwg/uzedwghandle.pas:295-303`: `DWGRefAbsoluteHandleValue` читает `Ref^.absolute_ref`.
- `cad_source/components/fpdwg/uzedwghandle.pas:305-345`: `DWGNormalizeObjectHandles` проходит по `object_ref` и `object_ordered_ref`. Нормализация сейчас работает только "на расширение": если `absolute_ref` больше текущего `obj.handle.value`, объект получает большее значение; если `absolute_ref` меньше, полный handle не затирается.
- `cad_source/components/fpdwg/uzedwghandle.pas:372-399`: `DWGRefHandleCandidatesValue` собирает кандидатов в порядке `obj.handle.value`, `absolute_ref`, `handleref.value`; `DWGRefHandleValue` возвращает первого кандидата.
- `cad_source/components/fpdwg/uzedwghandle.pas:412-491`: `DWGObjectOwnerHandleCandidatesValue` строит кандидатов owner-handle, включая implicit owner для `entmode=1/2`.
- `cad_source/components/fpdwg/uzedwghandle.pas:504-510`: layer-handle сущности берется через тот же механизм кандидатов `BITCODE_H`.

### Первичный raw scan

- `cad_source/zengine/fileformats/dwg/uzedwgrawscan.pas:148-164`: `ScanRawObjects` вызывается до mapper-ов и сначала запускает `DWGNormalizeObjectHandles`.
- `cad_source/zengine/fileformats/dwg/uzedwgrawscan.pas:168-180`: scan идет по `Raw.&object[i]`, берет handle через `DWGObjectHandleValue` и пишет строку `DWG [read]`.
- `cad_source/zengine/fileformats/dwg/uzedwgrawscan.pas:186-190`: при `DWG_DIAG_MODE = dmTrace` можно получить полную trace-строку по каждому raw-объекту через модуль programlog `DWG`.
- `cad_source/zengine/fileformats/dwg/uzedwgrawscan.pas:195-219`: handle `0` пропускается, остальные handle регистрируются как placeholder `dokUnknown`. Если такой handle уже есть, scan фиксирует duplicate вместо создания второй записи.

### Регистрация объектов и разрешение ссылок

- `cad_source/zengine/fileformats/dwg/uzedwgimport.pas:1045-1143`: `DWGRegisterEntityShellWithTextStyleCandidates` регистрирует shell сущности по `DWGObjectHandleValue`, ставит owner/layer/linetype/textstyle candidates в очереди разрешения.
- `cad_source/zengine/fileformats/dwg/uzedwgloadcontext.pas:313-349`: `TDWGZCADHandleMap.RegisterShell` хранит отсортированный handle-map. Placeholder из raw scan можно обновить реальным kind/ptr, но настоящий duplicate не заменяет первую запись.
- `cad_source/zengine/fileformats/dwg/uzedwgloadcontext.pas:766-784`: `TDWGZCADLoadContext.RegisterShell` логирует создание или duplicate-warning.
- `cad_source/zengine/fileformats/dwg/uzedwgloadcontext.pas:824-850`: owner candidates попадают в очередь resolver-а.
- `cad_source/zengine/fileformats/dwg/uzedwgloadcontext.pas:953-983`: ref candidates попадают в очередь resolver-а.
- `cad_source/zengine/fileformats/dwg/uzedwgresolver.pas:201-219`: owner resolver перебирает кандидатов по порядку.
- `cad_source/zengine/fileformats/dwg/uzedwgresolver.pas:389-458`: ref resolver перебирает кандидатов по порядку, ищет handle в registry и проверяет ожидаемый kind.

## Поток информации по шагам

1. `dwg_read_file` заполняет `Dwg_Data`: массив `Raw.&object`, объектные refs, header vars и связи `BITCODE_H`.
2. `BeginDWGImport` создает контекст загрузки, fallback-объекты и начальную запись handle `0`.
3. `ScanDWGImport` считывает важные header refs и запускает `ScanRawObjects`.
4. `ScanRawObjects` перед обходом вызывает `DWGNormalizeObjectHandles`. Этот шаг нужен, чтобы восстановить полный handle, если он есть в `object_ref.absolute_ref`, но отсутствует в `Dwg_Object.handle.value`.
5. Raw scan проходит по каждому `Raw.&object[i]`, берет `Obj.handle.value` как ключ, логирует его и регистрирует placeholder в handle-map.
6. `parseDwg_Data` вызывает mapper-ы сущностей и таблиц. Например, mapper LINE получает тот же `Dwg_Object`.
7. Mapper создает ZCAD-объект и вызывает `DWGRegisterEntityShell...`.
8. Регистрация shell использует handle объекта как ключ и добавляет кандидатов owner/layer/linetype/textstyle. Для `BITCODE_H` первым кандидатом идет handle resolved object pointer, затем scalar refs.
9. `EndDWGImport` вызывает resolver, который последовательно пробует кандидатов и привязывает объекты к owner-ам и визуальным refs.
10. Если включены диагностические side files, конец импорта записывает сводки и предупреждения рядом с DWG-файлом.

## Почему fpdwginspect читает handle корректно

`fpdwginspect.lpr` является тонкой CLI-оберткой. Реальная логика находится в `cad_source/components/fpdwg/inspector`.

- `cad_source/components/fpdwg/inspector/fpdwg_cli.pp:556-612`: `BuildDocumentFromRaw` напрямую обходит `Raw.&object[i]`, создает объект через factory и добавляет его в registry.
- `cad_source/components/fpdwg/inspector/fpdwg_factory.pp:118-132`: `FillCommonObjectFields` записывает `Obj.Handle := DWGHandleValue(Raw.handle)`. То есть identity объекта берется из `Dwg_Object.handle.value`, без перезаписи из `absolute_ref`.
- `cad_source/components/fpdwg/inspector/fpdwg_factory.pp:134-160`: общие ссылки сущности (`layer`, `ltype`, `material`, `plotstyle`, `prev_entity`, `next_entity`) читаются через `HandleRefFromBitCode`.
- `cad_source/components/fpdwg/inspector/fpdwg_libredwg_utils.pp:38-65`: `DWGHandleValue` возвращает `Handle.value`, а `HandleRefFromBitCode` выбирает источники в порядке `Ref^.obj^.handle.value`, `Ref^.absolute_ref`, `Ref^.handleref.value`.
- `cad_source/components/fpdwg/inspector/tests/fpdwg_test_handles.pp:87-105`: тест `ResolvedObjectHandleWinsOverScalarRefs` проверяет, что ссылка с resolved object pointer выбирает `obj.handle.value`, даже если scalar refs отличаются.

За счет этого inspector не затирает полный object handle младшими битами из `absolute_ref`. Поэтому его поведение стало эталоном для текущего ZCAD loader path: `DWGRefHandleCandidatesValue` теперь повторяет тот же приоритет источников.

## Гипотеза по исходному сбою

Вероятный дефект был таким:

1. LibreDWG возвращал `Dwg_Object.handle.value` с полным значением, например `A325E`.
2. В соответствующем `Dwg_Object_Ref.absolute_ref` для R2007+ `OFFSETOBJHANDLE` оказывались только младшие 16 бит, например `325E`.
3. Старый код нормализации безусловно копировал `absolute_ref` в `Ref^.obj^.handle.value`.
4. Все handle выше `FFFF` превращались в младшие 16 бит. После `FFFF` видимый ряд становился `0`, `1`, `2`, ...
5. `ScanRawObjects` видел повторяющиеся или нулевые handle. Handle `0` пропускался, а duplicate попал в ветку "first shell remains indexed".
6. Mapper-ы для поздних объектов либо не могли обновить правильный placeholder, либо их owner/ref queues пропускались как duplicate-shell.
7. Resolver искал refs по неправильным ключам и уходил в fallback/not found, поэтому часть геометрии не попадала в итоговый чертеж.

Это объясняет диагностический лог с переполнением после `FFFF` и одновременно объясняет, почему `fpdwginspect` мог показывать правильные handle: inspector никогда не делал безусловную обратную запись `absolute_ref -> obj.handle.value`.

## Что проверять вручную при повторении

1. Включить полный trace raw-объектов: временно поставить `DWG_DIAG_MODE = dmTrace` в `dwg/uzedwgsidefiles.pas`, пересобрать и запускать ZCAD с включенным модулем `DWG` (`logfile <path> lem DWG`).
2. Для конкретных handles включить точечный лог: временно поставить `DWG_TARGET_HANDLE_LIST = 'A325E'` в `dwg/uzedwgtargetedlog.pas` или задать список через поддержанные разделители, затем пересобрать.
3. Сравнить строки `DWG [read] raw_index=... handle=...` и `DWG raw object trace: ... handle_hex=...` с выводом `fpdwginspect` на том же DWG.
4. Если после текущей защиты снова виден ряд `FFFF, 0, 1, 2`, искать новый путь, где scalar `absolute_ref` выбирается раньше `obj.handle.value` или где объект отсутствует в `object_ref/object_ordered_ref` и не проходит через `DWGNormalizeObjectHandles`.
5. Если raw scan показывает правильный handle, но объект не появляется в чертеже, следующая точка проверки - наличие mapper-а (`has_handler`) и очереди resolver-а (`DWG [decode-owner]`, `DWG [decode-ref]`).

## Связанные регрессии

- `cad_source/zengine/fileformats/dwg/tests/uzedwgtestdwgproc.pas:309-329`: полный `obj.handle.value` должен выигрывать у усеченного `absolute_ref`.
- `cad_source/zengine/fileformats/dwg/tests/uzedwgtestdwgproc.pas:331-356`: список кандидатов должен начинаться с resolved object handle.
- `cad_source/zengine/fileformats/dwg/tests/uzedwgtestdwgproc.pas:368-404`: нормализация умеет расширять низкий object handle до полного `absolute_ref`.
- `cad_source/zengine/fileformats/dwg/tests/uzedwgtestdwgproc.pas:406-451`: нормализация не должна сужать полный object handle до усеченного `absolute_ref`; это конкретно защищает сценарий wraparound после `FFFF`.
