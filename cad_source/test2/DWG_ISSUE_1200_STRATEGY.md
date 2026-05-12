# Стратегия расследования проблемы загрузки DWG из issue #1200

Дата анализа: 2026-05-12.

Этот документ не предлагает программных изменений. Цель - зафиксировать, что уже видно из
`cad_source/test2/zcad.log`, и описать порядок расследования, который даст проверяемый ответ:
почему в большом DWG отсутствует часть объектов, почему часть объектов загружается с ошибками
цвета/поворота/содержимого блоков, и какие данные нужно получить следующими.

## Короткий вывод

Текущий лог после правок из PR #1199 и #1201 больше не похож на простое переполнение массива
объектов в ZCAD: `alloced_objects=131072`, а `objects=70191` (`zcad.log:827`).

Главный оставшийся сигнал - массовые дубликаты DWG handle и несколько больших неразобранных
`ACAD_TABLE` proxy-entity:

- LibreDWG возвращает код `68 (DWG_ERR_UNHANDLEDCLASS,DWG_ERR_VALUEOUTOFBOUNDS)` (`zcad.log:812`).
- В файле `70191` raw objects, `68275` entities, но только `57726` handle в карте загрузчика
  (`zcad.log:827`). Разница `12465` - не прямой счетчик потерянной геометрии, но хороший
  индикатор масштаба расхождения между raw-объектами и индексируемыми объектами.
- Сводка предупреждений показывает `23928` случаев `duplicate handle` по `11243` handle
  (`zcad.log:1614`). Это доминирующая ошибка текущего лога.
- Финальная сборка сообщает `built=57304` при `entities=68275` (`zcad.log:2855`). Разница
  `10971` - отдельная метрика для проверки жалобы "много объектов нет".
- Загружено 6 proxy-preview таблиц, но 3 `ACAD_TABLE` пропущены как неизвестные entity
  (`unknown_entities=3`, `proxy_loaded=6`, `zcad.log:1613`). Пропущенные большие таблицы:
  `7859` с `preview_size=1079043` (`zcad.log:1451`), `DD83` с `preview_size=2633473`
  (`zcad.log:1533`), `E201` с `preview_size=1847809` (`zcad.log:1535`).

Гипотеза для проверки: основная потеря объектов сейчас связана не с поздним разрешением ссылок,
а с тем, что часть raw-объектов имеет повторяющиеся или неверно прочитанные handle. Загрузчик
оставляет первый shell в индексе, а последующие объекты с тем же handle уже не становятся
нормально адресуемыми. Отдельный класс потерь - крупные `ACAD_TABLE` proxy, которые сейчас
не переводятся в полноценную геометрию.

## Что уже улучшилось и не должно быть первой целью

Старые логи до PR #1201 показывали каскад ошибок владельцев и ссылок: тысячи `ref kind mismatch`,
тысячи owner fallback и почти десять тысяч визуальных предупреждений на финализации. В текущем
логе это почти исчезло:

- `refs_fallback=87` вместо тысяч проблемных ссылок (`zcad.log:1613`).
- `fallback=3` по owner (`zcad.log:1613`).
- `visual_warnings=0` на финализации (`zcad.log:2855`).

Значит, сначала не стоит разбирать каждый `layer fallback` или цвет по отдельности. Эти симптомы
важны для финальной валидации, но они уже не объясняют массовую потерю объектов.

## Рабочие гипотезы

1. **LibreDWG или привязки возвращают повторяющиеся handle после `VALUEOUTOFBOUNDS`.**
   Нужно понять, видит ли внешний декодер те же дубликаты. Если внешний декодер показывает
   уникальные handle, проблема ближе к Pascal binding/структурам/чтению памяти.

2. **Часть дубликатов относится к proxy/table-объектам.**
   Если дубликаты группируются вокруг `fixedtype=65534`, `ACAD_TABLE` или class `532`, сначала
   нужно изолировать таблицы и proxy-preview.

3. **Часть raw-entity реально не строится, потому что shell с таким handle уже занят.**
   После PR #1201 duplicate-shell больше не должен перенацеливать pending refs/owners, но это
   означает, что геометрия второго объекта с тем же handle может быть проигнорирована.

4. **Цвета, повороты и содержимое блоков могут быть вторичными симптомами.**
   Их нужно проверять на малом наборе handle после того, как станет понятно, какие raw-объекты
   потеряны и почему.

## План расследования

### 1. Зафиксировать воспроизводимый baseline

Для каждого запуска сохранять одинаковый набор метрик:

| Метрика | Текущее значение |
| --- | ---: |
| LibreDWG read code | `68` |
| raw objects | `70191` |
| raw entities | `68275` |
| handles_total | `57726` |
| duplicate handle warnings | `23928` |
| duplicate handle keys | `11243` |
| owner fallback | `3` |
| ref fallback | `87` |
| unknown entities | `3` |
| unknown objects | `24` |
| proxy loaded | `6` |
| finalized built entities | `57304` |
| visual warnings | `0` |

Следующий запуск лучше делать с включенными side-files:

```bat
set ZCAD_DWG_DIAG=full
```

На Linux/Unix аналог:

```sh
ZCAD_DWG_DIAG=full ./zcad ...
```

После запуска нужно сохранить `zcad.log` и файлы рядом с DWG:

- `<dwg>.summary.txt`
- `<dwg>.summary.json`
- `<dwg>.handles.csv`
- `<dwg>.refs.csv`
- `<dwg>.owners.csv`

Если режим `trace` уже дает больше деталей в конкретной сборке, полезно повторить запуск с
`ZCAD_DWG_DIAG=trace`, но первым эталоном должен быть `full`.

### 2. Прочитать существующие side-files в правильном порядке

1. `summary.txt/json`: проверить общие счетчики, распределение handle по kind/fixedtype,
   предупреждения по кодам.
2. `refs.csv`: отфильтровать только `AttachState=asFallback`, сгруппировать по `RefHandle`,
   `Slot`, `ExpectedKind`. Сейчас таких случаев мало, поэтому это не главный поток анализа.
3. `owners.csv`: отфильтровать `AttachState=asFallback`. Сейчас всего 3 случая, их можно
   разобрать вручную.
4. `handles.csv`: посмотреть распределение `ResolvedKind`, `FixedType`, `ShellState`. Важно:
   текущий `handles.csv` показывает уникальные записи карты handle, но сам по себе не раскрывает
   все duplicate raw-index. Для дубликатов нужен отдельный отчет.

### 3. Нужный формат диагностического вывода

Чтобы расследование не превращалось в ручное чтение 70 тысяч объектов, следующий полезный вывод -
табличные отчеты. Их можно добавить позже, если текущих side-files недостаточно.

`*.duplicates.csv`:

```text
HandleHex;FirstRawIndex;DupRawIndex;FirstFixedType;DupFixedType;FirstDxfName;DupDxfName;FirstClass;DupClass;FirstSupertype;DupSupertype;FirstOwner;DupOwner;Action
```

Назначение: показать, являются ли дубликаты одним и тем же типом объекта, разными типами, proxy
таблицами, block content или результатом неверного чтения handle.

`*.raw_objects.csv`:

```text
RawIndex;HandleHex;FixedType;Supertype;DxfName;ClassDxf;OwnerCandidates;LayerCandidates;PreviewSize;ProxyCommands;Mapper;RegistryAction
```

Назначение: получить полный список raw-объектов до регистрации в ZCAD map.

`*.build.csv`:

```text
RawIndex;HandleHex;FixedType;DxfName;Mapper;CreatedPtr;BuiltCountDelta;SkipReason;OwnerHandle;LayerHandle;BBox
```

Назначение: связать raw-объект с фактом создания ZCAD entity и причиной пропуска.

`*.proxy_tables.csv`:

```text
HandleHex;PreviewSize;ProxyCommands;Loaded;SkipReason;BBox;PrimitiveCount;TextCount;LineCount;FirstOpcodes
```

Назначение: отдельно проверить `ACAD_TABLE`, потому что именно они видны как крупные пропущенные
unknown entity.

Сводный отчет должен также включать:

- `raw_objects`
- `raw_entities`
- `unique_handles`
- `duplicate_raw_objects`
- `zero_handle_objects`
- `built_entities`
- `skipped_by_reason`
- top-20 duplicate handle
- top-20 fallback refs
- top skipped proxy/table objects

### 4. Сравнить с внешним декодером DWG

Нужно получить внешний контрольный дамп того же DWG через один из инструментов:

- `dwgread`/утилиты LibreDWG, если доступны;
- ODA File Converter / ODA Drawings Explorer;
- AutoCAD save-as в более простой DWG/DXF;
- другой просмотрщик, который умеет показать handle/object id и тип объекта.

Сравнивать нужно не весь файл, а контрольные выборки:

- первые 20 handle из `duplicate handle`;
- пропущенные `ACAD_TABLE`: `7859`, `DD83`, `E201`;
- несколько `INSERT` с повторяющимися block refs, например из участков лога с `block_ref=8999`,
  `1E1B`, `9E8`;
- несколько объектов с `layer fallback` из `zcad.log:1571-1603`.

Критерии:

- Если внешний декодер тоже показывает повторяющиеся handle, исходный DWG или LibreDWG decode
  действительно дает конфликтные объекты.
- Если внешний декодер показывает уникальные handle, а ZCAD/LibreDWG binding видит дубликаты,
  нужно проверять чтение структуры `dwg_object`, `handleref`, `absolute_ref`, `fixedtype`.
- Если дубликаты почти все вокруг `ACAD_TABLE`/proxy, сначала надо изолировать parser proxy-preview.
- Если дубликаты есть на простых `LINE`/`INSERT`, проблема шире, чем таблицы.

### 5. Сократить DWG до минимального проблемного примера

Большой файл нужно делить не по количеству мегабайт, а по сохранению симптомов. После каждого
изменения запускать загрузку и записывать те же baseline-метрики.

Практичные варианты редукции:

- удалить/скрыть все таблицы `ACAD_TABLE` и проверить, падает ли число duplicate handle;
- оставить только modelspace без layout/paperspace;
- оставить только блоки с проблемными `INSERT`;
- оставить половину modelspace, затем повторять деление;
- экспортировать в DXF и сравнить количество entities/handle;
- выполнить save-as в более старую версию DWG и сравнить лог.

Если после удаления таблиц исчезают `unknown_entities=3` и резко падают duplicate handle, фокус -
`ACAD_TABLE`/proxy. Если таблицы удалены, но duplicate handle остается на тысячах объектов, фокус -
сырой decode handle или регистрация raw objects.

### 6. Проверить отдельные визуальные симптомы только на образцах

Цвет:

- выбрать 5-10 handle, где цвет точно неверный на экране;
- сравнить raw color/layer color/truecolor с итоговым цветом ZCAD entity;
- отдельно проверить `color.index=256` (ByLayer) и signed/unsigned truecolor вроде значений
  `0xC3000007`.

Поворот и блоки:

- выбрать несколько `INSERT`, которые визуально повернуты неверно;
- сравнить insertion point, scale, rotation, extrusion и block reference с внешним дампом;
- проверить, не относится ли block content к duplicate handle или skipped owner.

Содержимое таблиц:

- начать с `ACAD_TABLE` `7859`, `DD83`, `E201`, потому что они прямо помечены как skipped;
- для загруженных proxy-preview сравнить количество proxy commands и построенных primitives.

## Что не делать первым

- Не читать весь `zcad.log` вручную сверху вниз. Нужны сводки и CSV.
- Не исправлять цвета и повороты до понимания, какие raw-объекты вообще не строятся.
- Не считать доказанным переполнение массива только из-за зависимости от размера файла:
  текущий лог показывает запас `alloced_objects`.
- Не увеличивать лимиты вслепую. Сначала нужно понять, где именно теряются raw-index, handle
  и mapper/build result.
- Не смешивать три разные проблемы в один фикс: duplicate handle, skipped proxy table,
  визуальные атрибуты.

## Ожидаемый результат следующего шага

После следующего диагностического запуска должно быть возможно отнести проблему к одному из
трех классов:

1. **Ошибка decode/binding handle.** Внешний декодер показывает уникальные объекты, а ZCAD видит
   дубликаты.
2. **Проблема proxy/table объектов.** Дубликаты и пропуски концентрируются вокруг `ACAD_TABLE`
   и больших proxy-preview.
3. **Ошибка регистрации/построения ZCAD entity.** Raw decode выглядит корректно, но конкретный
   mapper или registry action пропускает объекты.

Только после такого разделения имеет смысл планировать программное исправление. Сейчас самая
полезная форма работы - собрать `full` diagnostics, добавить при необходимости отчет по
duplicate raw-index, и сравнить несколько контрольных handle с внешним DWG/DXF дампом.
