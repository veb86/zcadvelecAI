# Сквозная проверка #1478 на собранном ZCAD (Linux, Xvfb)

Ручной сценарий: настоящий ZCAD (`make zcadelectrotech`), настоящий HTTP-сервер
и штатный DXF-писатель. Дополняет модульные и HTTP-регрессии из
`experiments/issue1478/README.md`, которые выполняются в CI. В CI не включён:
нужна полная сборка ZCAD с LCL/GTK2 и X-сервер.

## Файлы

| Файл | Назначение |
|------|------------|
| `run_e2e.sh` | Поднимает Xvfb + openbox, трижды запускает ZCAD со сценарием `runscript` и вызывает проверки |
| `e2e_check.mjs` | HTTP-проверки одной фазы (`nodoc`, `nosel`, `sel`), пишет `summary.json` |
| `verify_dxf.py` | Читает выгруженные файлы через `ezdxf`, как MCP-клиент |
| `inspect_owner.py` | Диагностика: как устройства ZCAD раскладываются на объекты DXF |
| `linux-build/` | Правки, без которых `master` не собирается в Linux (только для копии сборки) |

## Подготовка

1. Копия репозитория для сборки; в ней применить правки для Linux
   (в `master` эти места рассчитаны только на Windows):

   ```sh
   git apply experiments/issue1478/e2e/linux-build/velectrnav-linux.patch
   cp experiments/issue1478/e2e/linux-build/comobj.pas cad_source/zcad/velec/managerem/
   make zcadelectrotech LP=/usr/bin PCP=/tmp/pcp
   ```

2. Пакеты времени выполнения (Ubuntu 24.04): `xvfb`, `openbox`, `xdotool`,
   `imagemagick`, `libglu1-mesa`, `libgl1-mesa-dri`; Node.js >= 18;
   для `verify_dxf.py` — Python с `ezdxf`.

   * без `libglu1-mesa` ZCAD завершается с кодом 217 при инициализации GLU;
   * без оконного менеджера GTK2 под Xvfb бесконечно обрабатывает события
     configure во время заставки — поэтому запускается `openbox`.

## Запуск

```sh
experiments/issue1478/e2e/run_e2e.sh <cad> \
  environment/runtimefiles/AllCPU-AllOS/zcadelectrotech/data/examples/test_dxf/em.dxf \
  /tmp/e2e-work node
python -I experiments/issue1478/e2e/verify_dxf.py <em.dxf> /tmp/e2e-work
```

`<cad>` — каталог `cad` собранной копии (в нём `bin/zcad`). Скрипт задаёт
`ZCAD_EXPORT_DIR=<work>/exports` и `ZCAD_EXPORT_TIMEOUT=60`, журнал ZCAD каждой
фазы копируется в `<work>/zcad-<фаза>.internal.log`.

## Фазы

| Фаза | Сценарий ZCAD | Проверки |
|------|---------------|----------|
| `nodoc` | `DWGNew`, `HTTPStart`, `DWGClose` | `NO_DOCUMENT`, файл не создан |
| `nosel` | `Load(em.dxf)`, `HTTPStart` | `EMPTY_SELECTION`; `scope=all` → успех |
| `sel` | `Load(em.dxf)`, `SelectAll`, `HTTPStart` | `{}` → `selection.dxf`; невыделенная LINE через `/ipc` не попадает в выгрузку; `scope=all` включает её и расширяет `bbox`; кириллический `out_path` с `include_xdata=false`; `FILE_EXISTS`; `INVALID_PATH` для `/etc/zcad.dxf` |

`HTTPStart` зарегистрирована с флагом `CADWG` и без открытого чертежа не
выполняется, поэтому `NO_DOCUMENT` получается закрытием чертежа после запуска
сервера. `DWGClose` даже для нового чертежа показывает модальный вопрос
«Drawing "Unnamed001" not saved. Save?». Модальный цикл обслуживает очередь
HTTP IPC, поэтому пока вопрос открыт, выгрузка видит пустой чертёж
(`EMPTY_SELECTION`, без падений). `run_e2e.sh` нажимает «No» через
`xdotool` и сохраняет снимок вопроса в `<work>/nodoc-save-prompt.png`.

## Результат (ZCAD из ветки, em.dxf)

```
PASS e2e phase nodoc
PASS e2e phase nosel
PASS e2e phase sel
sample   : 312 {'LINE': 210, 'MTEXT': 33, 'INSERT': 34, 'TEXT': 26, 'POLYLINE': 7, 'CIRCLE': 2} xdata 312
selection: 312 {'LINE': 210, 'MTEXT': 33, 'INSERT': 34, 'TEXT': 26, 'POLYLINE': 7, 'CIRCLE': 2} xdata 312
layers   : ['0', 'EL_DEVICE_NAME', 'SYS_DEVICE_BORDER', 'SYS_METRIC', 'TEXT']
blocks   : ['CONNECTOR_SQUARE', 'EL_CONTACTOR', 'EL_MOTOR', 'EL_POSITION', 'EL_SR', 'KIP_KNOPKA', 'KIP_UKP']
PASS ezdxf verification
```

Журнал ZCAD:

```
[HTTP-IPC] export_selection: path=".../exports/selection.dxf" entity_count=36 dxf_entity_count=312 duration_ms=290 code=OK
[HTTP-IPC] export_selection: path=".../exports/all.dxf" entity_count=37 dxf_entity_count=313 duration_ms=209 code=OK
[HTTP-IPC] export_selection: path=".../exports/selection.dxf" entity_count=0 dxf_entity_count=0 duration_ms=0 code=NO_DOCUMENT
```

`entity_count` — число объектов ZCAD (устройство — один объект),
`dxf_entity_count` — число объектов пространства модели в файле: устройство
записывается как `INSERT` и его примитивы (с XDATA `_OWNERHANDLE`).

Найдено этим сценарием и исправлено: после успешной записи файла ZCAD
сообщал об access violation — `ExportErrorCodeOf('')` разыменовывал `nil`
(`GetJSON('')` в FPC 3.2.2 возвращает `nil`). Регрессия — в
`test_export_selection.pas`.
