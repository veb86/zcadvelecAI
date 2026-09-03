# Техническое задание: обновление Pascal-обёртки fpdwg для libredwg 0.13

## 1. Назначение документа

Документ описывает поэтапную работу по созданию новой Pascal-обёртки модуля
`fpdwg` для C-библиотеки **LibreDWG** на основе нового заголовочного файла
`libredwg/dwg_new.h` (версия 0.13). Текущая обёртка `dwg.pp` была получена
автоматически (`H2Pas 1.0.0`) из старого `libredwg/dwg.h` (версия 0.10) и
вручную доработана. Прямой повторный запуск `H2Pas` на `dwg_new.h` приведёт к
потере ручных правок и к неработоспособной обёртке, поэтому работа разбита на
последовательные этапы, каждый из которых имеет чёткие границы, входные и
выходные артефакты, а также критерии проверки. Каждый этап рассчитан на
выполнение ИИ-программистом с уровнем «поздний джуниор» в рамках одного
коммита/PR.

## 2. Контекст и текущее состояние модуля

### 2.1 Состав каталога `cad_source/components/fpdwg/`

| Файл | Размер | Назначение |
| --- | --- | --- |
| `fpdwg.pas` | 22 строки | Заглушка пакета Lazarus (`unit fpdwg`), регистрирует пакет |
| `fpdwg.lpk` | 38 строк | Манифест пакета Lazarus (включает `dwg.pp`, `dwgproc.pp`) |
| `dwg.pp` | 10 579 строк | **Основная обёртка** — типы, перечисления, записи (auto-generated H2Pas + ручные правки) |
| `dwgproc.pp` | 285 строк | Динамическая загрузка библиотеки + вспомогательные классы (`TDWGCtx`, `GDWGParser`) |
| `dwg_test.lpr` | 57 строк | Тестовая программа: чтение DWG и печать TEXT-объектов |
| `dwg_test.lpi` | — | Проект Lazarus для тестовой программы |
| `LICENSE` | — | Лицензия LibreDWG (GPLv3) |
| `libredwg/dwg.h` | 9 793 строк | Старый C-заголовок (версия libredwg 0.10) |
| `libredwg/dwg_new.h` | 12 866 строк | **Новый** C-заголовок (версия libredwg 0.13) — целевой |

### 2.2 Архитектура текущей обёртки `dwg.pp`

| Раздел | Строки | Содержимое |
| --- | --- | --- |
| Заголовок и `unit dwg;` | 1–26 | GPL-шапка, комментарий H2Pas |
| Forward-указатели `P_dwg_*` | 28–1031 | ~1000 объявлений вида `P_dwg_X = ^_dwg_X` |
| Директивы `{$IFDEF FPC}{$PACKRECORDS C}{$ENDIF}` | 1032–1034 | Упаковка записей в формате C |
| Алиасы BITCODE_* | 1041–1150 | `BITCODE_RC = byte`, `BITCODE_BL = uint32`, и т. д. |
| `DWG_VERSION_TYPE` | 1220–1230 | 45 значений `R_INVALID..R_AFTER` |
| `dwg_versions` (запись) | 1235–1243 | Описатель версии |
| `DWG_CLASS_STABILITY` | 1246–1249 | 4 значения |
| `DWG_OBJECT_SUPERTYPE` | 1252–1254 | ENTITY/OBJECT |
| `DWG_OBJECT_TYPE` | 1256–1432 | 200+ значений, hex-литералы |
| Записи `_dwg_*` (entities/objects/structures) | 1459–9800+ | Сотни struct-эквивалентов |
| `_dwg_object_entity.tio` (union) | 9428–9576 | 90 case-вариантов |
| `_dwg_object_object.tio` (union) | 9580–9830 | 175 case-вариантов |
| `_dwg_object` | 9840–9869 | Обёртка с союзом entity/object |
| Заголовочные структуры файла | 9900–10470 | `Dwg_Header`, `Dwg_R2004_Header`, `Dwg_Second_Header` и т. д. |
| `_dwg_struct` (Dwg_Data) | 10474–10512 | Корневая запись документа |
| `RESBUF_VALUE_TYPE` | 10515–10522 | 12 значений |
| Битовые маски/смещения | 10534–10548 | Эквиваленты C bitfields |
| `implementation .. end.` | 10556–10579 | Закомментированные get/set для bitfields |

### 2.3 Что делает `dwgproc.pp`

* Декларирует переменные-указатели на функции:
  `dwg_read_file`, `dxf_read_file`, `dwg_free` (соглашение `extdecl`,
  `stdcall` под Windows и `cdecl` под прочими ОС).
* Платформенно-зависимые имена SO/DLL:
  `libredwg-0.dll` (Windows) и `libredwg.so` (Linux). На прочих ОС имя
  библиотеки закомментировано и должно быть добавлено по необходимости.
* Класс `TDWGCtx` хранит ссылку на `Dwg_Data` и определяет рабочую версию.
* Generic-класс `GDWGParser<GUserCtx>` использует `THashmap<DWG_OBJECT_TYPE, ...>`
  для регистрации обработчиков сущностей/объектов и обхода `dwg.&object[i]`.
* Утилиты `BITCODE_T2Text` (учёт `version<=R_2004` для выбора `pchar` или
  `punicodechar`), `DWG_V2Str`, `LoadLibreDWG`, `FreeLibreDWG`.

### 2.4 Тестовая программа `dwg_test.lpr`

Простая консольная утилита: загружает DLL, читает DWG-файл, печатает поля
`Dwg_Header`, перечисляет объекты типа `DWG_TYPE_TEXT` и освобождает
структуру. Используется как минимальный smoke-тест после изменений в
обёртке.

## 3. Сравнение `dwg.h` (0.10) и `dwg_new.h` (0.13)

### 3.1 Версионные константы

```c
// dwg_new.h:28–30
#define LIBREDWG_VERSION_MAJOR 0
#define LIBREDWG_VERSION_MINOR 13
#define LIBREDWG_VERSION ((LIBREDWG_VERSION_MAJOR * 100) + LIBREDWG_VERSION_MINOR)
```

В новой версии **отсутствует** `LIBREDWG_SO_VERSION`. Это влияет на правила
имени shared library и должно быть учтено в `dwgproc.pp`.

### 3.2 Новые/изменённые типы BITCODE

| Тип | Старая версия | Новая версия |
| --- | --- | --- |
| `BITCODE_MC` | `long` (платформо-зависимый) | `int32_t` (фикс. 32 бита) |
| `BITCODE_UMC` | `unsigned long` | `uint64_t` |
| `BITCODE_RLLx` | отсутствует | `uint64_t` (новый) |
| `BITCODE_RLLd` | отсутствует | `int64_t` (новый) |
| `BITCODE_HV` | отсутствует | алиас `BITCODE_RLLx` (handle value, 64-bit) |
| `BITCODE_TU16`, `BITCODE_TU32` | отсутствуют | макро-алиасы для строк |
| `BITCODE_RD` | `double` | `double` (без изменений) |

Это означает, что в новой обёртке требуется:

* добавить `BITCODE_RLLx = uint64`, `BITCODE_RLLd = int64`,
  `BITCODE_HV = BITCODE_RLLx`;
* перевести `BITCODE_MC` с `longint` на `int32` (явно), `BITCODE_UMC` —
  с `dword` на `uint64`.

### 3.3 Изменения в перечислениях

* `DWG_VERSION_TYPE` — добавлены `R_2022b` и пересмотрены комментарии
  релизов; общий состав расширен до 59 значений.
* `DWG_OBJECT_TYPE_R11` — добавлено `DWG_TYPE_JUMP_r11 = 18`.
* `DWG_OBJECT_TYPE` — добавлены: `DWG_TYPE_POINTCLOUD`, `DWG_TYPE_POINTCLOUDEX`,
  `DWG_TYPE_POINTCLOUDDEF`, `DWG_TYPE_POINTCLOUDDEFEX`,
  `DWG_TYPE_POINTCLOUDDEF_REACTOR`, `DWG_TYPE_POINTCLOUDDEF_REACTOR_EX`,
  `DWG_TYPE_POINTCLOUDCOLORMAP`, `DWG_TYPE_PARTIAL_VIEWING_INDEX` и др.
* Перечисления `DWG_CLASS_STABILITY`, `DWG_OBJECT_SUPERTYPE`,
  `DWG_ENTITY_SECTIONS` остались, но в новом заголовке вынесены отдельно;
  значения совместимы.
* Перечисления `DWG_ERROR`, `DWG_HDL_CODE`, `DWG_COLOR_METHOD` — без
  изменений.

### 3.4 Изменения в записях

| Запись | Старая версия | Новая версия |
| --- | --- | --- |
| `Dwg_Data` (root) | 51 поле, `r2004_header` без `union`, `next_hdl: unsigned long`, `second_header` | 59 полей, `union { r2004_header; r2007_file_header } fhdr;`, `next_hdl: BITCODE_RLL` (uint64), `secondheader` (переименование), новые поля `object_ordered_ref`, `num_object_ordered_refs`, `prev_entity_index` |
| `Dwg_Second_Header` | существует | переименовано в `Dwg_SecondHeader` + появились вложенные `_dwg_secondheader_handles`, `_dwg_secondheader_sections` |
| Точечно (entities) | базовый набор | добавлены `_dwg_entity_POINTCLOUD`, `_dwg_entity_POINTCLOUDEX`, `_dwg_entity_JUMP`, серия R11-сущностей |
| Точечно (objects) | базовый набор | добавлены `_dwg_object_POINTCLOUDDEF/_EX/_REACTOR/_REACTOR_EX/_COLORMAP`, `_dwg_object_PARTIAL_VIEWING_INDEX` |
| `_dwg_AcDbMTextObjectEmbedded` | отсутствует | добавлено |
| `_dwg_object_LAYOUTPRINTCONFIG` | существует | перемещено в `_dwg_entity_LAYOUTPRINTCONFIG` |

Всего в новом заголовке ~471 структура (было ~465); чистый прирост — 6
структур плюс несколько переименований/перемещений.

### 3.5 Новые экспорты библиотеки

В `dwg_new.h` объявлено **338** функций со спецификатором `EXPORT`. В
текущем `dwgproc.pp` динамически загружаются только три:
`dwg_read_file`, `dxf_read_file`, `dwg_free`. Большинство остальных —
аксессоры и утилиты для работы со ссылками, обходом сущностей и т. д.
Расширение списка загружаемых функций — тема отдельного этапа (§5.10).

### 3.6 Новые макросы и атрибуты

* `__counted_by(x)` — атрибут компилятора (Clang ≥ 18, GCC ≥ 15) для
  массивов с длиной, заданной соседним полем. Для C++ макрос пустой.
  В Pascal-обёртке транслируется в обычный указатель + комментарий, без
  семантической поддержки атрибута.
* `EXPORT` — без изменений (visibility/dllexport).

## 4. Цели и принципы новой обёртки

### 4.1 Цели

1. Полностью покрыть типы и перечисления `dwg_new.h` в Pascal-эквивалентах.
2. Сохранить бинарную совместимость записей (`{$PACKRECORDS C}` обязателен).
3. Сохранить совместимость публичного API существующих потребителей в
   репозитории (поиск использований обязателен на этапе 5.1).
4. Дать возможность вызывать новые функции LibreDWG (point cloud, partial
   viewing, расширенные аксессоры).
5. Все комментарии в новом коде — на русском (см. требования `CLAUDE.md`).

### 4.2 Принципы оформления (по `CLAUDE.md`)

* Каждый Pascal-файл начинается со служебного блока (название модуля,
  назначение, автор, дата, зависимости).
* Длина строки ≤ 100 символов.
* Длина функции ≤ 30 строк, вложенность ≤ 3 уровней, ранний выход через
  `Exit`.
* Никаких «магических чисел» — только именованные константы.
* Комментарии — на русском, перед каждой публичной функцией.
* Логирование — через `uses uzclog;` и `programlog.LogOutFormatStr(...)`
  с типом `LM_Info`.

### 4.3 Соглашения преобразования C → Pascal

Сводная таблица для повторного использования при ручной правке/проверке
вывода H2Pas:

| Конструкция C | Эквивалент Pascal | Примечание |
| --- | --- | --- |
| `typedef unsigned char BITCODE_RC;` | `BITCODE_RC = byte;` | целочисленный алиас |
| `uint16_t BITCODE_BS;` | `BITCODE_BS = uint16;` | использовать FPC `uintN`/`intN` |
| `uint64_t BITCODE_RLL;` | `BITCODE_RLL = uint64;` | под все платформы |
| `char *` | `Pchar` | NUL-терминированная строка |
| `unsigned char *` | `Pbyte` (как `BITCODE_TF`) | бинарный буфер |
| `dwg_wchar_t *` | `Pdwg_wchar_t` (`^BITCODE_RS`) | UTF-16 на R2007+ |
| `enum X { ... };` | `X = (V0, V1 := 5, ...)` | использовать `:=` для явных значений |
| `struct X { ... };` | `_dwg_X = record ... end; Dwg_X = _dwg_X;` | поле `&object` для зарезервированного слова |
| `union { A a; B b; }` | `record case longint of 0:(a:A); 1:(b:B); end;` | в `record case` нет имени селектора |
| `T arr[N];` | `arr: array[0..N-1] of T` | фиксированный массив |
| `T *arr; size_t n;` | `arr: PT; n: ...;` | динамический массив с длиной |
| `int x: 4;` (bitfield) | `x: byte;` + константы `bm_*`, `bp_*` | get/set через сдвиги |
| `void *cb(...)` (callback) | `function ...(...): pointer; cdecl;` | использовать модификатор `extdecl` |

## 5. Поэтапное техническое задание

> **Правила выполнения этапов**
>
> * Один этап = один коммит (или короткая серия атомарных коммитов) и одна
>   задача в трекере. Завершённый этап не должен ломать сборку пакета
>   `fpdwg.lpk`, тестовую программу `dwg_test.lpr` и зависимые модули
>   `cad_source/components/...`.
> * Перед началом этапа: прочитать пункт TZ полностью, изучить указанные
>   диапазоны строк в `dwg.pp`/`dwg_new.h`, при необходимости задать
>   уточняющие вопросы в комментариях к PR.
> * После каждого этапа: запустить `make installpkgstolaz` (если
>   установлен Lazarus) или собрать `dwg_test.lpr` (`fpc dwg_test.lpr`).
>   Зафиксировать результат в описании коммита.
> * Каждый этап содержит: цель, входные артефакты, действия, выходные
>   артефакты, критерии готовности (Definition of Done, DoD).

### 5.1 Этап 1. Подготовка и аудит зависимостей

**Цель.** Зафиксировать публичный API текущей обёртки и составить список
символов, которые потребляются внешними модулями. Это нужно, чтобы новая
обёртка не сломала компиляцию zcad/zcadelectrotech.

**Входные артефакты.**

* `cad_source/components/fpdwg/dwg.pp` (строки 1041–10579 — весь набор
  типов).
* `cad_source/components/fpdwg/dwgproc.pp` (полный файл).
* Ветка `master` репозитория для `git grep`/`grep -r`.

**Действия.**

1. В корне репозитория выполнить `grep -RIn "uses .*\bdwg\b" cad_source/`
   и собрать список потребителей `unit dwg`/`unit dwgproc`.
2. Для каждого потребителя выписать перечень используемых публичных
   идентификаторов (`Dwg_Data`, `DWG_TYPE_TEXT`, конкретные record-поля и
   т. д.) в файл `cad_source/components/fpdwg/audit_external_usage.md`
   (промежуточный артефакт, удаляется после завершения работ).
3. Сформировать «контрольный список» (list of symbols), которые
   обязательно должны остаться доступными в новой обёртке без изменения
   имён.
4. Зафиксировать соглашение об именовании: алиасы `Dwg_X = _dwg_X` для
   совместимости со старым кодом сохраняются. При этом исходные
   `_dwg_X` из C-заголовка используются как «внутренние».

**Выходные артефакты.** Промежуточный отчёт `audit_external_usage.md`
(не попадает в финальный мерж), список затрагиваемых модулей в описании
этапа PR.

**DoD.** Отчёт приложен к PR; ни один публичный идентификатор из
контрольного списка не помечен как «допустимо к удалению» без явного
обсуждения.

### 5.2 Этап 2. Скелет нового модуля и compile-time переключатель

**Цель.** Создать новый исходник `dwg_new.pp` рядом со старым `dwg.pp`,
оформить шапку по требованиям `CLAUDE.md`, добавить директивы FPC
(`{$Mode objfpc}`, `{$H+}`, `{$PACKRECORDS C}`). На этом этапе обёртка
ещё не используется в `fpdwg.lpk`.

**Входные артефакты.** `dwg.pp:1–34` (как образец заголовка); `CLAUDE.md`
(требования к шапке); `dwg_new.h:1–230` (типы BITCODE/координат).

**Действия.**

1. Создать файл `cad_source/components/fpdwg/dwg_new.pp`. Шапка в
   формате `CLAUDE.md` (русский язык), указать автора (тот, кто
   выполняет этап) и дату.
2. Объявить `unit dwg_new;` и базовые директивы:
   ```pascal
   {$IFDEF FPC}
     {$PACKRECORDS C}
     {$Mode objfpc}{$H+}
   {$ENDIF}
   ```
3. В `interface` оставить пустые секции `type`, `const`, `var`. Добавить
   `implementation`/`end.`. Файл должен компилироваться FPC отдельно
   (вне `fpdwg.lpk`).
4. Добавить заглушки доктестов в виде комментариев — список этапов
   §5.3–§5.10 со ссылками на разделы TZ.

**Выходные артефакты.** Файл `dwg_new.pp` (≤ 100 строк), компилируется
командой `fpc -Mobjfpc -Sh dwg_new.pp`.

**DoD.** Сборка `fpc dwg_new.pp` без ошибок; шапка соответствует
требованиям; в `fpdwg.lpk` файл пока **не** включён.

### 5.3 Этап 3. Forward-указатели и алиасы простых типов

**Цель.** Перенести в `dwg_new.pp` все объявления указателей `P_dwg_*` и
`PDwg_*`, а также алиасы простых типов BITCODE/координат.

**Входные артефакты.** `dwg.pp:28–1031` (forward-объявления); `dwg_new.h:89–280`
(новые типы BITCODE и координат); `dwg_new.h:281–360` (типы перечислений
ниже по тексту).

**Действия.**

1. Сгенерировать `dwg_new_pre.pp` через `H2Pas dwg_new.h -D -p -o dwg_new_pre.pp`.
   Сохранить в `experiments/h2pas_pre.pp` для дальнейшего использования
   как «черновика».
2. Из черновика скопировать в `dwg_new.pp`:
   * блок `P_dwg_*` (указатели на структуры);
   * алиасы `BITCODE_RC..BITCODE_BLL`;
   * **новые** типы: `BITCODE_RLLx`, `BITCODE_RLLd`, `BITCODE_HV`,
     `BITCODE_TU16`, `BITCODE_TU32`, `BITCODE_D2T`;
   * структуры `Dwg_Bitcode_TimeBLL`, `_2RD`, `_2BD`, `_3RD`, `_3BD` и
     алиасы `BITCODE_2RD..BITCODE_BE`.
3. Заменить `BITCODE_MC = longint` на `BITCODE_MC = int32` (см. §3.2),
   `BITCODE_UMC = dword` на `BITCODE_UMC = uint64`.
4. К каждому новому идентификатору добавить русский комментарий с
   назначением, ссылкой на строку в `dwg_new.h`.

**Выходные артефакты.** `dwg_new.pp` объёмом ~400 строк, сборка
`fpc dwg_new.pp` без ошибок.

**DoD.** Все простые типы из `dwg_new.h:89–280` представлены; новые
`BITCODE_*` помечены как новые; список `P_dwg_*` совпадает по составу со
списком структур, которые планируются к переносу далее.

### 5.4 Этап 4. Перечисления (enums)

**Цель.** Перенести все перечисления из `dwg_new.h`, привести значения
точно к C-эквивалентам, в т. ч. `R_AFTER`, `DWG_TYPE_FREED`,
`DWG_TYPE_UNKNOWN_ENT`, `DWG_TYPE_UNKNOWN_OBJ`.

**Входные артефакты.** `dwg_new.h:281–1654` (все основные перечисления);
`dwg.pp:1220–1432` (старые версии перечислений как референс).

**Действия.**

1. Перевести по порядку:
   * `DWG_VERSION_TYPE` (включая `R_2022b`, `R_AFTER`).
   * `DWG_CLASS_STABILITY`.
   * `DWG_ENTITY_SECTIONS`.
   * `DWG_OBJECT_SUPERTYPE`.
   * `DWG_OBJECT_TYPE` (с явными hex для значений ≤ 0x52, явным
     `DWG_TYPE_PROXY_ENTITY = $1f2`, `DWG_TYPE_FREED = $fffd`,
     `DWG_TYPE_UNKNOWN_ENT = $fffe`, `DWG_TYPE_UNKNOWN_OBJ = $ffff`).
   * `DWG_OBJECT_TYPE_R11` (включая `DWG_TYPE_JUMP_r11 = 18`).
   * `DWG_ERROR` (флаги, `DWG_NOERR = 0`, далее `1 shl N`).
   * `DWG_HDL_CODE`.
   * `DWG_COLOR_METHOD`.
   * `DWG_SECTION_TYPE`, `DWG_SECTION_TYPE_R13`, `DWG_SECTION_TYPE_R11`.
   * `RESBUF_VALUE_TYPE`.
2. Для перечислений с явными значениями использовать синтаксис
   `(NAME := value, ...)`. Между «логическими блоками» — одна пустая
   строка.
3. Каждый идентификатор перечисления, добавленный относительно старой
   версии, пометить русским комментарием `// новое в 0.13`.
4. Для `DWG_ERROR` определить константу `DWG_ERR_CRITICAL` и выписать
   битовые маски как `const`.

**Выходные артефакты.** `dwg_new.pp` ~1500 строк; компилируется.

**DoD.** Каждый enum из C-заголовка имеет соответствие в Pascal; набор
значений совпадает по количеству и порядку с `dwg_new.h`.

### 5.5 Этап 5. Базовые «несоставные» структуры

**Цель.** Перенести «инфраструктурные» записи, не зависящие от entity/
object union: `Dwg_Handle`, `Dwg_Object_Ref`, `Dwg_Color`, `Dwg_ResBuf`,
`Dwg_Chain`, `Dwg_Class`, `Dwg_Bitcode_*`, `Dwg_Versions`, `Dwg_AuxHeader`,
`Dwg_SummaryInfo`, `Dwg_AppInfo`, `Dwg_AppInfoHistory`,
`Dwg_FileDepList[+_Files]`, `Dwg_Security`, `Dwg_VBAProject`,
`Dwg_RevHistory`, `Dwg_ObjFreeSpace`, `Dwg_Template`, `Dwg_AcDs[+_Schema/...]`.

**Входные артефакты.** `dwg_new.h:1599–1724` (handle/ref/color/resbuf);
`dwg_new.h:9000–9420` (вспомогательные структуры); `dwg.pp:1459–9400`
(старые соответствия).

**Действия.**

1. Сравнить каждую структуру со старой версией в `dwg.pp`, выписать
   изменения (порядок полей, добавленные поля, новые типы — например
   `BITCODE_HV`).
2. Для bitfields (`unsigned codepage:15; is_tu:1;` в `Dwg_Color`,
   `_dwg_entity_eed_data`) сгенерировать константы `bm_*`/`bp_*` и
   inline-функции в секции `implementation` (по образцу строк
   10550–10576 старого файла, но без комментариев — функции должны
   работать).
3. Документировать каждую структуру русским комментарием перед
   объявлением.
4. После переноса — сборка `fpc dwg_new.pp`.

**Выходные артефакты.** `dwg_new.pp` ~3500 строк.

**DoD.** Все упомянутые структуры присутствуют, поля совпадают по
порядку и типу с `dwg_new.h`; bitfield-аксессоры реализованы и
прошли smoke-проверку (см. §5.11).

### 5.6 Этап 6. Структуры заголовка файла (Header/SecondHeader/R20xx)

**Цель.** Перенести `Dwg_Header`, `Dwg_Header_Variables`,
`Dwg_R2004_Header`, `Dwg_R2007_Header`, новый
`Dwg_SecondHeader` (с вложенными `_handles`, `_sections`).

**Входные артефакты.** `dwg_new.h:1725–2128` (`Dwg_Header_Variables`),
`dwg_new.h:10753–11453` (header structures, R2004/R2007, second header);
`dwg.pp:9870–10470` (старые версии).

**Действия.**

1. Перенести `Dwg_Header_Variables` целиком, обращая внимание на:
   * фиксированные массивы (`zero_5: array[0..4] of BITCODE_RC`);
   * строки `BITCODE_T DWGCODEPAGE` и т. п. (тип `BITCODE_T = BITCODE_TV`).
2. Перенести `Dwg_R2004_Header` и **новый** `Dwg_R2007_Header`. Учесть,
   что в `Dwg_Data` они объединены через `union`.
3. Для нового `Dwg_SecondHeader` создать структуры
   `_dwg_secondheader_handles`, `_dwg_secondheader_sections`, затем
   корневую `_dwg_secondheader`.
4. Каждое старое имя сохранить как алиас:
   `Dwg_Second_Header = Dwg_SecondHeader;`.
5. Указать в комментарии, что старое имя оставлено для обратной
   совместимости.

**Выходные артефакты.** `dwg_new.pp` ~5000 строк.

**DoD.** Сборка проходит; алиасы старых имён присутствуют; набор полей
заголовка совпадает с C.

### 5.7 Этап 7. Структуры сущностей (entities)

**Цель.** Перенести все `_dwg_entity_*` (~120 структур) с поддержкой
union'ов и вложенных структур.

**Входные артефакты.** `dwg_new.h:2129–8974` (entity definitions);
`dwg.pp:1459–9400` (старые версии); рекомендуется обработка по группам
(буквы алфавита `_3D`, `A..Z`).

**Действия.**

1. Завести таблицу прогресса в комментарии в начале файла:
   `// Этап 7: 0/120 entity-структур переведено`. Обновлять после каждой
   группы.
2. Для каждой entity-структуры:
   * перенести поля по порядку;
   * union → `record case longint of`;
   * массивы переменной длины (`field[] __counted_by(count)`) —
     указатель + соседнее поле длины с комментарием;
   * вложенные подструктуры (`_dwg_3DSOLID_silhouette`,
     `_dwg_HATCH_Path` и т. п.) — отдельные `_dwg_X` записи рядом.
3. Особое внимание:
   * `_dwg_entity_TEXT` — поле `text_value: BITCODE_T` (см. использование в
     `dwg_test.lpr:45`).
   * `_dwg_entity_LWPOLYLINE` — массив точек.
   * `_dwg_entity_HATCH` — самый сложный (вложенные пути, циклы);
     рекомендуется выделить в отдельный коммит.
   * `_dwg_entity_POINTCLOUD` / `_POINTCLOUDEX` — новое в 0.13.
   * R11-сущности (`_dwg_entity_REPEAT`, `_ENDREP`, `_LOAD`, `_3DLINE`,
     `_JUMP`).
4. После каждой группы — проверка `fpc dwg_new.pp`.

**Выходные артефакты.** `dwg_new.pp` ~8000 строк.

**DoD.** Все entity-структуры из `dwg_new.h:2129–8974` представлены;
сборка успешна; в комментарии этапа в TZ-файле обновлён счётчик.

### 5.8 Этап 8. Структуры объектов (objects), control-таблиц и dictionary

**Цель.** Перенести все `_dwg_object_*`, `_dwg_object_<TABLE>_CONTROL`,
включая новые `_POINTCLOUDDEF*`, `_PARTIAL_VIEWING_INDEX`,
`_LAYOUTPRINTCONFIG`.

**Входные артефакты.** `dwg_new.h:2129–8974` (общий блок),
`dwg_new.h:8974–9045` (eed/related); `dwg.pp` 9400–9830 (старые версии).

**Действия.**

1. Аналогично §5.7, но для object-структур (~180 типов).
2. Для control-таблиц (`Dwg_Object_BLOCK_CONTROL`, `_LAYER_CONTROL`,
   `_LTYPE_CONTROL` и т. д.) проверить порядок и тип `entries`.
3. Для `_dwg_object_DICTIONARY*` — поля `texts`, `itemhandles`, `dict`.
4. Для новых `_dwg_object_POINTCLOUDDEF*` — выделить отдельный коммит,
   приложить ссылку на `dwg_new.h:8804–8900`.
5. Перенос `_dwg_AcDs_*` и связанных подструктур.

**Выходные артефакты.** `dwg_new.pp` ~10500 строк.

**DoD.** Все object-структуры присутствуют; сборка успешна; счётчик
прогресса в файле обновлён.

### 5.9 Этап 9. `_dwg_object_entity`, `_dwg_object_object`, `_dwg_object`, `_dwg_struct`

**Цель.** Перенести наиболее «тяжёлые» union'ы и корневую запись
`Dwg_Data`. Обновить структуру корня под новые поля 0.13.

**Входные артефакты.** `dwg_new.h:11384–11443` (`Dwg_Data`);
`dwg.pp:9428–9869` (старые `_dwg_object_entity`, `_dwg_object_object`,
`_dwg_object`); `dwg.pp:10474–10512` (старый `_dwg_struct`).

**Действия.**

1. `_dwg_object_entity.tio` — `record case longint of` со всеми типами
   entity, включая новые (`POINTCLOUD`, `JUMP`, R11-сущности).
2. `_dwg_object_object.tio` — аналогично для objects, включая
   `POINTCLOUDDEF*`, `PARTIAL_VIEWING_INDEX`.
3. `_dwg_object.tio` — `case` с двумя вариантами `entity`/`&object`
   (имя `object` — зарезервированное, использовать FPC-эскейп).
4. `_dwg_struct` (Dwg_Data):
   * добавить новые поля `object_ordered_ref`, `num_object_ordered_refs`,
     `prev_entity_index`;
   * заменить отдельный `r2004_header` на `record case longint of 0:(
     r2004_header: Dwg_R2004_Header); 1:(r2007_file_header: Dwg_R2007_Header);
     end;` (поле `fhdr`);
   * сменить тип `next_hdl` с `dword` на `BITCODE_RLL`;
   * заменить `second_header: Dwg_Second_Header` на
     `secondheader: Dwg_SecondHeader` с алиасом `second_header` (см. §5.6).

**Выходные артефакты.** `dwg_new.pp` финальной длины ~11500 строк;
сборка успешна.

**DoD.** `Dwg_Data` совпадает по бинарному размеру с C-структурой
(проверка `sizeof(dwg) = sizeof(Dwg_Data)` через тестовый запуск
LibreDWG, см. §5.11).

### 5.10 Этап 10. Расширение `dwgproc.pp` под libredwg 0.13

**Цель.** Адаптировать загрузчик под новые имена/функции и вынести
часть API в публичные процедурные переменные.

**Входные артефакты.** `dwgproc.pp` (полный файл); `dwg_new.h:11474–11679`
(блок `EXPORT`-функций); матрица «нужных» функций из §5.1.

**Действия.**

1. Заменить `uses ... dwg ...` на `uses ... dwg_new ...` (после
   завершения этапа 11 — окончательное переименование).
2. Поверх существующих `dwg_read_file`, `dxf_read_file`, `dwg_free`
   добавить минимально необходимый набор:
   * `dwg_write_file`;
   * `dwg_get_layer_count`, `dwg_get_layers`;
   * `dwg_next_object`, `dwg_next_entity`;
   * `dwg_resolve_handle`, `dwg_resolve_handleref`;
   * `dwg_get_first_owned_entity`, `dwg_get_next_owned_entity`;
   * `dwg_version_type`, `dwg_version_as`;
   * `dwg_class_is_entity`, `dwg_obj_is_*`.
3. Описать каждую функцию русским комментарием с указанием параметров и
   возвращаемого типа.
4. Платформенные имена библиотеки: уточнить под 0.13 (по changelog
   LibreDWG `libredwg-0.so`/`libredwg.so.0` на Linux, новые имена под
   macOS — `libredwg.dylib`).

**Выходные артефакты.** `dwgproc.pp` ~400 строк; компиляция вместе с
`dwg_new.pp` успешна.

**DoD.** Все добавленные функции загружаются (`Assigned(...)` = `True`)
после `LoadLibreDWG`; smoke-тест из §5.11 проходит.

### 5.11 Этап 11. Smoke-тесты и валидация бинарной совместимости

**Цель.** Подтвердить, что новая обёртка читает реальные DWG-файлы
библиотекой 0.13 и значения совпадают с ожидаемыми.

**Входные артефакты.** `dwg_test.lpr`; небольшие тестовые DWG-файлы
(добавить в `cad_source/components/fpdwg/test_data/`, разметить как
GPL/тестовые).

**Действия.**

1. Скопировать `dwg_test.lpr` в `dwg_new_test.lpr`, заменить
   `uses dwg` на `uses dwg_new`. Дополнительно вывести: новый
   `dwg.fhdr.r2007_file_header.*`, новые поля `Dwg_Data.next_hdl`,
   `prev_entity_index`. Проверить, что `dwg.&object[i].fixedtype` для
   `POINTCLOUD` корректно определяется.
2. Подготовить минимальные DWG-файлы (от R14 до R2018) с известными
   текстами/слоями. Сохранить шаги воспроизведения в
   `experiments/test_plan.md` (формат: команда, ожидание, фактический
   результат).
3. Сверить `sizeof(Dwg_Data)`, `sizeof(Dwg_Object)`, `sizeof(Dwg_Header)`
   с эталоном (узнать через малую утилиту на C либо по логам LibreDWG).
4. Дополнить `dwg_test`/`dwg_new_test` логированием через
   `programlog.LogOutFormatStr` (см. `CLAUDE.md:42–43`).

**Выходные артефакты.** `dwg_new_test.lpr`, отчёт `experiments/test_plan.md`.

**DoD.** `dwg_new_test` корректно читает все тестовые файлы; вывод
заголовка совпадает со старой версией там, где поля общие; для новых
полей — выводится осмысленное значение.

### 5.12 Этап 12. Переключение пакета и удаление старого `dwg.pp`

**Цель.** Перевести пакет `fpdwg.lpk` на использование `dwg_new.pp`,
переименовать `dwg_new.pp` → `dwg.pp` (после удаления исходного),
обновить зависимые модули.

**Входные артефакты.** `fpdwg.lpk`; результат `audit_external_usage.md`
(§5.1); все потребители `unit dwg`.

**Действия.**

1. Удалить старый `cad_source/components/fpdwg/dwg.pp`.
2. Переименовать `dwg_new.pp` → `dwg.pp`, заменить `unit dwg_new;` на
   `unit dwg;`. Обновить `uses` в `dwgproc.pp`, `dwg_test.lpr`,
   `dwg_new_test.lpr` (или удалить второй после слияния тестов).
3. В `fpdwg.lpk` оставить только `dwg.pp` и `dwgproc.pp`. Файл
   `dwg_new_test` удалить либо переместить в `experiments/`.
4. Прогнать `make installpkgstolaz` и сборку `zcad`/`zcadelectrotech`.
5. Удалить промежуточные артефакты: `audit_external_usage.md`,
   `experiments/h2pas_pre.pp` (но не план тестов).

**Выходные артефакты.** Финальное состояние модуля без временных
файлов; собранный `zcad` и `zcadelectrotech`.

**DoD.** Все потребители из §5.1 продолжают компилироваться; CI «зелёный».

### 5.13 Этап 13. Документация и финал

**Цель.** Зафиксировать результат и подготовить инструкции для будущих
обновлений LibreDWG.

**Действия.**

1. В `cad_source/components/fpdwg/` обновить (или добавить) README.md с
   описанием:
   * назначения модуля;
   * версии libredwg, под которую сделана обёртка;
   * порядка обновления (ссылка на этот TZ);
   * имени библиотеки на каждой платформе.
2. В `BUILD_FROM_SOURCES.md` добавить раздел о требованиях LibreDWG 0.13
   (имя пакета в дистрибутивах, минимальная версия).
3. В корне репозитория (если применимо) обновить changelog/release
   notes.
4. Закрыть issue, к которому относится этот TZ; в комментарии PR
   привести ссылки на каждый этапный коммит.

**DoD.** Документация актуальна; PR готов к слиянию (ready for review).

## 6. Сводная таблица этапов

| № | Этап | Артефакт | Объём | Зависит от |
| --- | --- | --- | --- | --- |
| 5.1 | Аудит внешних потребителей | `audit_external_usage.md` | малый | — |
| 5.2 | Скелет `dwg_new.pp` | `dwg_new.pp` (~100 стр.) | малый | 5.1 |
| 5.3 | Forward-указатели + BITCODE | `dwg_new.pp` (~400 стр.) | средний | 5.2 |
| 5.4 | Перечисления | `dwg_new.pp` (~1500 стр.) | средний | 5.3 |
| 5.5 | Базовые структуры | `dwg_new.pp` (~3500 стр.) | большой | 5.4 |
| 5.6 | Заголовки файла | `dwg_new.pp` (~5000 стр.) | средний | 5.5 |
| 5.7 | Entity-структуры | `dwg_new.pp` (~8000 стр.) | большой | 5.5 |
| 5.8 | Object-структуры | `dwg_new.pp` (~10500 стр.) | большой | 5.7 |
| 5.9 | Союзы + Dwg_Data | `dwg_new.pp` (~11500 стр.) | средний | 5.7, 5.8 |
| 5.10 | Расширенный `dwgproc.pp` | `dwgproc.pp` | средний | 5.9 |
| 5.11 | Тесты | `dwg_new_test.lpr` | средний | 5.10 |
| 5.12 | Переключение пакета | `fpdwg.lpk`, `dwg.pp` | малый | 5.11 |
| 5.13 | Документация | `README.md`, `BUILD_FROM_SOURCES.md` | малый | 5.12 |

## 7. Список потенциальных проблем и стратегий решения

* **Имена-зарезервированные слова Pascal** (`object`, `type`, `unit`,
  `interface`, `set`). H2Pas частично эскейпит через `&`. Решение: в
  каждом этапе при переносе structures проверять на список
  reserved-words FPC и применять `&имя`.
* **Bitfields**. C-bitfields неустойчивы по битовому порядку между
  компиляторами. Решение: всегда использовать целое поле + ручные
  макроподобные функции (см. §5.5).
* **Платформенно-зависимые типы**. `unsigned long` (старый
  `BITCODE_UMC`/`next_hdl`) различается на 32/64-bit ОС. В новой версии
  они уже фиксированной ширины — упрощается. Перепроверить, что в
  Pascal используется `uint64`/`uint32`, а не `dword`/`longword`.
* **Различие H2Pas-версий**. Если установлен H2Pas новее 1.0.0,
  результат может отличаться. Решение: фиксировать в TZ номер версии
  H2Pas (1.0.0), на которой выполнялась исходная конвертация, и
  сравнивать ручную правку с её выводом.
* **Производительность парсинга**. После добавления `object_ordered_ref`
  в `Dwg_Data` появляется новый способ обхода. В `dwgproc.pp` следует
  оставить старый цикл обхода `dwg.&object[i]` (он по-прежнему
  валиден), а использование `object_ordered_ref` — отнести к
  отдельному будущему улучшению.
* **Ломкие тесты на больших файлах**. Для сложных DWG-файлов
  возможны падения на ещё непереведённых структурах. Решение:
  smoke-тесты из §5.11 запускать на минимальных DWG, а полную
  валидацию — после §5.12.

## 8. Приложение: ключевые ссылки на исходный код

* C-заголовок 0.13: `cad_source/components/fpdwg/libredwg/dwg_new.h`
* C-заголовок 0.10 (для сравнения): `cad_source/components/fpdwg/libredwg/dwg.h`
* Текущая обёртка: `cad_source/components/fpdwg/dwg.pp`
* Загрузчик/утилиты: `cad_source/components/fpdwg/dwgproc.pp`
* Манифест пакета: `cad_source/components/fpdwg/fpdwg.lpk`
* Тестовая программа: `cad_source/components/fpdwg/dwg_test.lpr`
* Стандарт оформления кода: `CLAUDE.md` (корень репозитория)

---

*Документ подготовлен в рамках issue #1027 как техническое задание
без выполнения работ по самой конвертации. Этапы §5.1–§5.13 предназначены
для последующих pull-request'ов.*
