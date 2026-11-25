# HTTP Server: выгрузка выделенного фрагмента в DXF (#1478)

Документация команды: `cad_source/zcad/velec/IPCserver/EXPORT_SELECTION.md`.

## Воспроизведение в ZCAD

1. Запустить `HTTPStart` (по умолчанию `127.0.0.1:5000`).
2. Выделить несколько объектов в активном чертеже.
3. Выполнить:

   ```sh
   curl -s -X POST http://127.0.0.1:5000/api/export_selection -d '{}'
   ```

4. До изменения маршрута не было (404). Теперь ответ содержит `"ok":true`,
   `path`, `entity_count`, `dxf_entity_count`, `bbox` и `units`; файл по `path` уже закрыт и
   открывается `ezdxf.readfile(path)`.
5. Снять выделение и повторить запрос: `EMPTY_SELECTION`.

## Автоматические проверки

Из корня репозитория; нужны FPC 3.2.2 с FCL (fcl-web, fcl-json) и Node.js >= 18.
Обе проверки выполняются в CI (`.github/workflows/nodtests.yml`,
шаг «Export selection regression»).

### 1. Логика команды — `test_export_selection.pas`

```sh
mkdir -p /tmp/zcad-export1478
fpc -Fucad_source/zcad/velec/IPCserver -FU/tmp/zcad-export1478 \
  -FE/tmp/zcad-export1478 experiments/issue1478/test_export_selection.pas
/tmp/zcad-export1478/test_export_selection
```

Проверяет модуль `uzvhttpexportselection` без ZCAD: значения по умолчанию,
разбор тела и ошибки типов, приоритет `out_path` → `ZCAD_EXPORT_DIR` →
`ZCAD_MCP_WORKSPACE`, нормализацию путей (в том числе кириллических),
allowlist и `..`, диапазон таймаута, формат JSON успеха и ошибок, удаление
XDATA. При любой ошибке процесс завершается с кодом 1.

### 2. HTTP-маршрут — `test_export_route.mjs`

```sh
node experiments/issue1478/test_export_route.mjs
```

Скрипт извлекает **текущие** `SetCORSHeaders` и `HandleRequest` из
`uzvhttpserver.pas` и собирает `route_fixture.lpr` с настоящими модулями
`uzvhttpipc`, `uzvhttpexportroute` и `uzvhttpexportselection`. HTTP-сервер
работает в фоновом потоке, главный поток обслуживает очередь IPC, как
`TzcMainForm.idle`. Заменены только журнал (`stubs/uzclog.pas`) и запись
чертежа: обработчик пишет небольшой DXF по тем же правилам пути.

Проверяемые сценарии:

* `{}` и пустое тело → `ZCAD_EXPORT_DIR/selection.dxf`, повторный вызов
  перезаписывает файл;
* кириллический `out_path` и query-строка → каталог создаётся, путь в ответе
  совпадает;
* `overwrite=false` → `FILE_EXISTS`; путь вне allowlist и `..` →
  `INVALID_PATH`, файл не создаётся; неверный JSON/тип → `INVALID_REQUEST`;
  текстовая ошибка обработчика → `INTERNAL_ERROR`;
* медленная выгрузка → `TIMEOUT` через `ZCAD_EXPORT_TIMEOUT=1`; команда,
  стоящая в очереди за занятым главным потоком, получает `TIMEOUT` и
  отменяется (файл не появляется); после этого сервер продолжает работать;
* `POST /ipc` с `cmd=EXPORT_SELECTION` → тот же JSON внутри конверта IPC;
* `GET` → 405; существующие `/ipc` и 404 не изменились;
* обработчик выполняется в главном потоке.

Маршрут также добавлен в регрессию CORS `experiments/issue1472/test_httpserver.mjs`.

### 3. Сквозная проверка на собранном ZCAD — `e2e/`

Настоящий ZCAD под Xvfb, сценарии без чертежа, без выделения и с выделением,
проверка файлов через `ezdxf`. Не входит в CI (нужна полная сборка ZCAD и
X-сервер); порядок запуска и результаты — `e2e/README.md`.
