# loaderDWG: план оптимизации `dwg-import.resolve-refs`

## Краткий вывод

В замере из issue #1232 самая дорогая фаза:

```text
DWG timing: phase=dwg-import.resolve-refs elapsed_ms=65061 pending_refs=139523 refs_attached=139523 refs_fallback=0
```

Сама логика резолвера ссылок уже должна работать почти линейно: один проход по
`PendingRefs`, до трех handle-кандидатов на запись, поиск каждого кандидата в
отсортированной `TDWGZCADHandleMap` через бинарный поиск, затем запись указателя
в entity/layer/table slot. Для 139523 ссылок это не объясняет 65 секунд.

Главная причина задержки находится не в поиске handle, а в диагностическом
контексте attach-callback'а. `DWGAttachRef` перед каждой записью указателя
восстанавливает `EntityHandle` и `RefHandle` через два линейных прохода по
`LoadCtx.PendingRefs`. На 139523 refs это дает примерно:

- средний случай: `139523 * (139523 + 1) = 19 466 807 052` проверок записей;
- худший случай: `139523 * 139523 * 2 = 38 933 335 058` проверок записей.

Эта квадратичная работа выполняется для диагностической строки
`DWG [attach-ref]` до фактического `case Slot of ...`, поэтому возникает даже
когда все refs успешно resolved (`refs_fallback=0`).

Та же схема есть в `DWGAttachEntity`: перед `AddMi` два линейных прохода по
`LoadCtx.PendingOwners`. Поэтому следующая фаза из замера тоже медленная:

```text
DWG timing: phase=dwg-import.resolve-owners elapsed_ms=39425 pending_owners=68081 attached=68081 fallback=0 cycles=0
```

Для 68081 owner-записи это примерно `4 635 090 642` проверок в среднем.

## Статус после issue #1234

Ветка `issue-1234-9fb03804d45b` продолжает этот план после уже внесенного
контекстного attach-callback пути:

- P3 реализован для ссылок: `TDWGZCADResolver` хранит per-import cache по
  `(Slot, ExpectedKind, InlineRef, Fallback, RefCandidates)` и повторно
  использует найденный pointer/resolved handle для следующих entity refs.
- P2 частично закрыт для горячего attach-пути: массовые успешные
  `DWG [attach]`, `DWG [attach-ref]` и layer-linetype info строки теперь
  выключены быстрым флагом `DWG_VERBOSE_ATTACH_LOG`; targeted и fallback
  diagnostics остаются включенными.
- P6 расширен счетчиками `ref_cache_hits`, `ref_cache_misses`,
  `unique_ref_keys` в timing detail, resolve-summary и diagnostic side-files.

## Что происходит внутри `dwg-import.resolve-refs`

1. `EndDWGImport` запускает таймер `dwg-import.resolve-refs` и вызывает
   `LoadCtx.ResolveRefs` (`dwg/uzedwgimport.pas`).
2. `TDWGZCADLoadContext.ResolveRefs` делает один проход:
   `for I := 0 to FPendingRefs.Count - 1 do FResolver.ResolveRef(...)`
   (`dwg/uzedwgloadcontext.pas`).
3. `TDWGZCADResolver.ResolveRef` пропускает уже обработанные записи, выбирает
   fallback по slot, обрабатывает inline-ref и затем перебирает
   `Pending^.RefCandidates` (`dwg/uzedwgresolver.pas`).
4. Каждый кандидат ищется через `FHost.TryGetEntry`, то есть через
   `TDWGZCADHandleMap.TryGet` и бинарный поиск по handle.
5. При найденной записи проверяется ожидаемый kind (`dokLayer`, `dokLineType`,
   `dokTextStyle`, `dokDimStyle`, `dokBlockDef`) и ненулевой `Ptr`.
6. При успехе вызывается `FinishRef`: состояние pending-записи становится
   `asAttached`, сохраняется `AttachedRef`, увеличивается `RefAttachCount`.
7. `FinishRef` вызывает `FHost.InvokeRefAttach`, а контекст вызывает production
   callback `DWGAttachRef`.
8. `DWGAttachRef` сначала вызывает `DWGRefEntityHandleForLog(Entity, Slot)` и
   `DWGRefHandleForLog(Entity, Slot)`.
9. Оба helper'а заново обходят весь `LoadCtx.PendingRefs` и ищут запись по
   `Entity` pointer + `Slot`.
10. Только после этого пишется `DWG [attach-ref]` и выполняется полезная работа:
    `pobj^.vp.Layer := ...`, `pobj^.vp.LineType := ...`,
    `PGDBObjText(pobj)^.TXTStyle := ...`, `PDimStyle := ...`,
    `pInsert^.PDef := ...`.

Итог: полезная работа на одну ref-запись мала, но перед ней стоят два
линейных поиска по массиву из 139523 записей. Индексы `PendingRefs` по
`(EntityHandle, Slot)`, добавленные ранее, здесь не помогают, потому что
callback получает только pointer `Entity`, pointer `Ref`, `Slot` и `Reason`,
а не исходную pending-запись или handle'ы.

## Главная причина

Горячий путь attach-callback'ов восстанавливает handle'ы из глобальных pending
очередей по pointer'ам. Это диагностическая операция, но она выполняется
синхронно на каждую resolved ref/owner запись и имеет сложность `O(N)` на одну
запись. Весь `resolve-refs` становится `O(N^2)`, хотя собственно resolver уже
близок к `O(N log H)`, где `H` - число зарегистрированных DWG handle'ов.

Дополнительный фактор: high-volume info logging (`DWG [attach-ref]`,
`DWG [attach]`) формирует аргументы до вызова `DWGLogInfoFormatStr`. Поэтому
дорогая подготовка диагностического контекста не исчезает сама по себе от того,
что модуль `DWG` в programlog обычно выключен.

## План изменений

### P1. Передавать handle-контекст напрямую в attach-callback

Цель: полностью убрать линейные поиски из `DWGAttachRef` и `DWGAttachEntity`.

Предлагаемый вариант:

1. Ввести небольшой record с уже известным контекстом:

   ```pascal
   TDWGAttachContext = record
     EntityHandle: TDWGZCADHandle;
     TargetHandle: TDWGZCADHandle; // owner/ref handle
     Slot: TDWGZCADRefSlot;        // значим только для refs
     Reason: TDWGAttachReason;
   end;
   ```

2. Расширить callback-и или добавить новые `...Ex` callback-и:
   `TDWGAttachProcEx` и `TDWGRefAttachProcEx`.
3. В `TDWGZCADResolver.FinishRef` передавать в callback значения из
   `Pending^.EntityHandle`, `Pending^.RefHandle`, `Pending^.Slot`,
   `Pending^.AttachReason`.
4. В `TDWGZCADResolver.FinishOwner` аналогично передавать
   `Pending^.EntityHandle` и `Pending^.OwnerHandle`.
5. Переписать `DWGAttachRef` и `DWGAttachEntity` так, чтобы они брали handle'ы
   из переданного контекста, а не из `DWGRefEntityHandleForLog`,
   `DWGRefHandleForLog`, `DWGOwnerEntityHandleForLog`,
   `DWGOwnerHandleForLog`.
6. Удалить или оставить только для legacy/debug helper'ы, которые линейно
   сканируют pending-очереди. В production hot path они использоваться не
   должны.
7. `DWGRefHandlesForLog` заменить на форматирование из текущего контекста,
   чтобы fallback-логи тоже не запускали дополнительный полный проход.

Ожидаемый эффект на замере из issue: убрать десятки миллиардов проверок
pending-записей. После этого `resolve-refs` должен масштабироваться от числа
refs линейно, а не квадратично.

### P2. Разделить полезный attach и подробный per-ref/per-owner trace

Цель: не платить за подробный info trace в обычной загрузке.

1. Оставить обязательными только счетчики и итоговые timing/summary строки.
2. `DWG [attach-ref]` и `DWG [attach]` перевести в trace/targeted режим или
   защитить быстрым флагом `DWGLogInfoEnabled`.
3. Если в `programlog` нет дешевого API "модуль включен?", добавить helper в
   `dwg/uzedwglog.pas`, чтобы дорогие аргументы не вычислялись до проверки.
4. Предупреждения fallback'ов оставить, но формировать их из переданного
   attach-контекста, без поиска по очереди.

Это не заменяет P1: даже при включенном подробном DWG trace код все равно
должен использовать уже готовые handle'ы, а не искать их по pointer'ам.

### P3. Кэшировать повторяющиеся результаты ref-resolution

После устранения квадратичного лога следующий источник работы - повторные
одинаковые refs. В больших DWG тысячи сущностей обычно указывают на одни и те
же layer/linetype/textstyle handles.

План:

1. Ввести per-import cache в `TDWGZCADLoadContext` или resolver:
   key = `(Slot, ExpectedKind, InlineRef, RefCandidates[0..Count-1], Fallback)`.
2. Cache value = `(AttachState, AttachReason, AttachedRef, ResolvedHandle)`.
3. В `ResolveRef` сначала проверять cache. При hit сразу вызывать `FinishRef`
   с готовым указателем.
4. При miss выполнить текущий поиск кандидатов через `TryGetEntry`, затем
   сохранить результат.
5. Добавить счетчики `ref_cache_hits`, `ref_cache_misses`, `unique_ref_keys`
   в timing/detail или diagnostic summary.

Это не отменяет необходимость пройти все pending refs, потому что каждой
entity нужно записать указатель в свой `vp`/style/block slot. Но поиск по
handle-map можно сделать по числу уникальных refs, а не по числу entity refs.

### P4. Уменьшить размер `PendingRefs`

Часть refs не требует отложенного поиска вообще:

- inline linetype (`ByLayer`, `ByBlock`, `Continuous`) уже имеет готовый
  fallback pointer и сейчас проходит через `PendingRefs` как `InlineRef`;
- default/null refs с заранее известным fallback можно применять fast-path'ом,
  если это не ухудшает диагностику.

План:

1. Для `InlineRef=True` рассмотреть немедленную запись pointer в mapper-е или
   отдельный fast-path в `QueueRefResolveCandidates`.
2. Сохранить семантику counters: inline refs должны оставаться `attached`, а не
   `fallback`, и не должны порождать warning.
3. Добавить счетчики по слотам:
   `pending_refs_by_slot`, `inline_refs`, `resolved_handle_refs`.
4. После P1/P3 сравнить, дает ли уменьшение очереди заметный выигрыш на
   реальном `maintest.dwg`. Если вклад мал, оставить этот пункт вторым этапом.

### P5. Аналогично исправить owner path

`resolve-owners` в том же замере занял 39425 ms. Там корневая схема такая же:
`DWGAttachEntity` вызывает два линейных helper'а по `PendingOwners`.

План:

1. Использовать тот же `TDWGAttachContext` для owner attach.
2. Передавать `EntityHandle` и `OwnerHandle` из `Pending` напрямую.
3. Убрать вызовы `DWGOwnerEntityHandleForLog` и `DWGOwnerHandleForLog` из hot
   path.
4. Оставить существующий индекс `FindPendingOwner` для resolver-а: он нужен для
   owner-chain resolution и уже не является главным bottleneck'ом.

### P6. Добавить измерения, которые подтвердят исправление

Перед кодовым исправлением и после него стоит добавить временные или
постоянные счетчики в `DWGTIMER` detail:

- `refs_total`;
- `refs_by_slot`;
- `ref_cache_hits/ref_cache_misses` после P3;
- `attach_ref_calls`;
- `attach_ref_context_lookup_ms` до удаления старых helper'ов, если нужен
  промежуточный proof;
- `owners_total`;
- `attach_owner_calls`.

Финальная цель по структуре лога:

```text
DWG timing: phase=dwg-import.resolve-refs elapsed_ms=... pending_refs=139523 refs_attached=139523 refs_fallback=0 unique_ref_keys=...
DWG timing: phase=dwg-import.resolve-owners elapsed_ms=... pending_owners=68081 attached=68081 fallback=0 cycles=0
```

## План тестирования

1. Юнит-тесты `dwg/tests/uzedwgtestloadcontext.pas`:
   - ref attach callback получает `EntityHandle`, `RefHandle`, `Slot`,
     `Reason` без lookup по pending list;
   - owner attach callback получает `EntityHandle`, `OwnerHandle`, `Reason`;
   - inline linetype сохраняет текущее поведение: `asAttached`,
     `arResolved`, warning count = 0.
2. Регрессионные тесты на существующие сценарии:
   - `ResolveRefsIsIdempotent`;
   - alternate candidate после kind mismatch;
   - fallback для missing/null/kind mismatch refs;
   - owner cycles и fallback owner.
3. Нагрузочный эксперимент вне обязательного CI:
   - создать synthetic `TDWGZCADLoadContext` с 100000 entity refs на несколько
     layer/linetype handles;
   - сравнить время `ResolveRefs` до/после P1;
   - после P3 проверить cache hit ratio.
4. Реальная проверка на файле из issue:
   - включить `lem DWGTIMER`;
   - загрузить `maintest.dwg`;
   - сравнить `dwg-import.resolve-refs`, `dwg-import.resolve-owners`,
     `addfromdwg.parse-data` до/после;
   - убедиться, что `built`, `refs_attached`, `refs_fallback`, `attached`,
     `fallback`, `cycles` не ухудшились.

## Риски и ограничения

- Изменение callback-сигнатур затронет unit tests и production wiring. Чтобы
  снизить риск, можно добавить `...Ex` callback-и и временно оставить старые
  wrapper'ы.
- Нельзя заменять pending lookup на pointer-based index как основной вариант:
  pointer aliasing уже был источником duplicate-shell проблем, а resolver
  владеет точной pending-записью и может передать handle'ы без эвристик.
- Нельзя просто выключить `DWG [attach-ref]`: это уменьшит симптомы, но не
  исправит архитектурную ошибку "callback восстанавливает контекст сканированием
  глобальной очереди".
- Performance-тест с жестким временем в CI может быть нестабильным. Лучше
  покрыть контракт callback-контекста unit-тестами, а нагрузочный замер держать
  как manual/experiment.

## Приоритет внедрения

1. **P1 + P5**: убрать `O(N^2)` scans из ref/owner attach callbacks. Это
   основной ускоритель и минимальный по риску путь.
2. **P2**: сделать high-volume attach trace действительно диагностическим, а не
   постоянной работой на каждую запись.
3. **P3**: добавить cache для повторяющихся ref targets.
4. **P4**: уменьшить количество queued refs для inline/default случаев, если
   реальные замеры после P1-P3 покажут заметный остаточный вклад.
5. **P6**: оставить счетчики в timing summary, чтобы следующие оптимизации
   проверялись цифрами, а не только субъективным временем загрузки.
