# POST /api/export_selection — выгрузка выделенного фрагмента в DXF

Команда HTTP-сервера ZCAD (issue #1478). Записывает выделенные объекты активного
чертежа в отдельный DXF-файл и **после того, как файл записан и закрыт**,
возвращает JSON с путём к файлу. Предназначена для MCP-инструмента
`zcad_export_selection`, который затем читает файл через `ezdxf`.

Существующие маршруты (`/ipc`, `/widgets/*`, Grist-маршруты) не изменены.

## Модули

| Файл | Назначение |
|------|------------|
| `uzvhttpexportselection.pas` | Чистая логика: разбор тела, конфигурация из окружения, путь и allowlist, JSON ответа и ошибок, удаление XDATA |
| `uzvhttpexportroute.pas` | REST-маршрут: HTTP-поток передаёт команду `EXPORT_SELECTION` в очередь главного потока и ждёт её с таймаутом |
| `uzvhttpcmdexportselection.pas` | Обработчик `EXPORT_SELECTION` внутри ZCAD: копирование выделения в служебный чертёж и запись штатным DXF-писателем |
| `uzvhttpipc.pas` | Очередь HTTP IPC: таймаут на команду, отмена ещё не начатой команды, `HTTPIPCExecuteCommand` |

## Запрос

```
POST /api/export_selection
Content-Type: application/json
```

Все поля необязательны; пустое тело и `{}` допустимы.

```json
{
  "out_path": "C:/zcad_export/selection.dxf",
  "scope": "selection",
  "overwrite": true,
  "include_xdata": true
}
```

| Поле | По умолчанию | Описание |
|------|--------------|----------|
| `out_path` | `ZCAD_EXPORT_DIR/ZCAD_EXPORT_FILENAME` | Абсолютный путь или путь относительно каталога выгрузки. Только расширение `.dxf` |
| `scope` | `selection` | `selection` — выделенные объекты; `all` — все объекты пространства модели |
| `overwrite` | `true` | `false` и существующий файл → `FILE_EXISTS` |
| `include_xdata` | `true` | `false` — из файла удаляются группы XDATA (коды 1000–1071) |

## Успешный ответ (HTTP 200)

```json
{
  "ok": true,
  "command": "export_selection",
  "path": "/home/user/zcad_export/selection.dxf",
  "scope": "selection",
  "entity_count": 12,
  "bbox": [0.0, 0.0, 12000.0, 8000.0],
  "units": "mm",
  "include_xdata": true,
  "file_size": 48213,
  "duration_ms": 37,
  "message": "selection exported"
}
```

* `path` — нормализованный абсолютный путь (UTF-8, кириллица допустима);
* `bbox` — `[minx, miny, maxx, maxy]` в мировой системе координат чертежа;
* `units` — значение `$INSUNITS` чертежа (`mm`, `cm`, `m`, `in`, …);
  поле отсутствует, если единицы не заданы.

## Ошибка (HTTP 200, `ok=false`)

```json
{
  "ok": false,
  "command": "export_selection",
  "error": { "code": "EMPTY_SELECTION", "message": "No entities selected" }
}
```

| Код | Когда |
|-----|-------|
| `NO_DOCUMENT` | Нет активного чертежа |
| `EMPTY_SELECTION` | Ничего не выделено (или чертёж пуст при `scope=all`) |
| `INVALID_PATH` | Пустой путь, не `.dxf`, недопустимые символы, путь — каталог, путь вне allowlist |
| `PATH_NOT_WRITABLE` | Не удалось создать каталог, записать или заменить файл |
| `FILE_EXISTS` | Файл существует и `overwrite=false` |
| `EXPORT_FAILED` | Исключение DXF-писателя, пустой результат, сбой удаления XDATA |
| `TIMEOUT` | ZCAD не выполнил команду за `ZCAD_EXPORT_TIMEOUT` секунд |
| `INTERNAL_ERROR` | Прочие непредвиденные ошибки |
| `INVALID_REQUEST` | Тело не JSON-объект, поле неверного типа, неизвестный `scope` |

`GET` и другие методы возвращают HTTP 405 с кодом `INVALID_REQUEST`.

## Конфигурация (переменные окружения)

Читаются при каждом запросе, перезапуск ZCAD не нужен.

| Переменная | По умолчанию | Описание |
|------------|--------------|----------|
| `ZCAD_EXPORT_DIR` | `ZCAD_MCP_WORKSPACE`, иначе `<TEMP>/zcad_export` | Каталог выгрузки по умолчанию |
| `ZCAD_MCP_WORKSPACE` | — | Рабочий каталог MCP-сервера; используется как каталог выгрузки, если `ZCAD_EXPORT_DIR` не задан, и всегда входит в allowlist |
| `ZCAD_EXPORT_FILENAME` | `selection.dxf` | Имя файла, если `out_path` не передан |
| `ZCAD_EXPORT_TIMEOUT` | `60` | Таймаут выполнения в секундах, ограничен диапазоном 1…600 |
| `ZCAD_EXPORT_ROOT` | каталог выгрузки + `ZCAD_MCP_WORKSPACE` | Разрешённые корни через разделитель `PATH` (`;` в Windows, `:` в Linux). `*` снимает ограничение |

Приоритет пути: `out_path` → `ZCAD_EXPORT_DIR` + `ZCAD_EXPORT_FILENAME`.
Относительный `out_path` считается от каталога выгрузки; `..` раскрывается
до проверки allowlist, поэтому выход за пределы корня даёт `INVALID_PATH`.
Родительский каталог создаётся автоматически.

## Поведение

* **Поток.** HTTP-поток только разбирает тело и ставит команду в очередь;
  выгрузка выполняется главным потоком ZCAD (`TzcMainForm.idle` →
  `HTTPIPCProcessPendingCommands`), как и остальные команды HTTP IPC.
* **Содержимое DXF.** Используется механизм `CopyClip`: выделенные объекты
  копируются через `drawings.CopyEnt` в служебный чертёж (вместе с нужными
  слоями, стилями и определениями блоков), который записывается штатным
  `savedxf20XX` с шаблоном версии DXF по умолчанию. Исходный чертёж и его
  выделение не изменяются.
* **Атомарность.** Файл пишется во временный `<out_path>.<pid>-<tick>.tmp`,
  затем заменяет целевой. Ответ отправляется после закрытия файла, поэтому
  клиент может сразу открыть его — блокировок ZCAD не держит. Повторный
  вызов с `overwrite=true` перезаписывает файл (идемпотентно).
* **Таймаут.** Если команда ещё стоит в очереди, по таймауту она отменяется и
  не выполняется. Если выгрузка уже началась, клиент получает `TIMEOUT`, а
  ZCAD доводит запись до конца (прервать писатель DXF безопасно нельзя);
  файл появится по `path` позже.
* **Журнал.** Каждая выгрузка пишет строку уровня `LM_Info`:
  `export_selection: path="…" entity_count=N duration_ms=M code=OK|<код ошибки>`.
* **Ошибки** не роняют ZCAD: любые исключения превращаются в JSON с кодом.

## Через /ipc

Команда доступна и в конверте HTTP IPC; JSON ответа передаётся строкой
в `result` (успех) или `error` (ошибка):

```bash
curl -s -X POST http://127.0.0.1:5000/ipc \
  -d '{"id":"1","cmd":"EXPORT_SELECTION","args":[{"out_path":"/tmp/zcad_export/a.dxf"}]}'
# {"id":"1","status":"ok","result":"{\"ok\":true,\"command\":\"export_selection\",...}"}
```

## Примеры

```bash
# Выгрузка в каталог по умолчанию
curl -s -X POST http://127.0.0.1:5000/api/export_selection -d '{}'

# Явный путь без перезаписи
curl -s -X POST http://127.0.0.1:5000/api/export_selection \
  -H 'Content-Type: application/json' \
  -d '{"out_path":"/tmp/zcad_export/Проект/фрагмент.dxf","overwrite":false}'
```

## MCP-инструмент `zcad_export_selection`

Минимальная реализация на стороне MCP-сервера (Python, `requests` + `ezdxf`):

```python
import os
import requests
import ezdxf

ZCAD_URL = os.environ.get("ZCAD_HTTP_URL", "http://127.0.0.1:5000")
TIMEOUT_SEC = int(os.environ.get("ZCAD_EXPORT_TIMEOUT", "60"))


def zcad_export_selection(out_path=None, overwrite=True, include_xdata=True):
    """Выгружает выделение ZCAD в DXF и возвращает краткую сводку."""
    body = {"overwrite": overwrite, "include_xdata": include_xdata}
    if out_path:
        body["out_path"] = out_path
    # HTTP-таймаут клиента должен быть больше ZCAD_EXPORT_TIMEOUT
    reply = requests.post(f"{ZCAD_URL}/api/export_selection",
                          json=body, timeout=TIMEOUT_SEC + 10).json()
    if not reply["ok"]:
        raise RuntimeError(f'{reply["error"]["code"]}: {reply["error"]["message"]}')
    doc = ezdxf.readfile(reply["path"])  # файл уже закрыт ZCAD
    msp = doc.modelspace()
    return {
        "path": reply["path"],
        "entity_count": reply["entity_count"],
        "bbox": reply.get("bbox"),
        "units": reply.get("units"),
        "types": sorted({e.dxftype() for e in msp}),
        "layers": sorted({e.dxf.layer for e in msp}),
    }
```

Рекомендуется задать MCP-серверу и ZCAD одинаковый `ZCAD_MCP_WORKSPACE`, тогда
файлы по умолчанию попадают в рабочий каталог MCP и проходят allowlist.

## Ограничения

* `scope="all"` выгружает все объекты пространства модели активного чертежа.
* Символические ссылки при проверке allowlist не раскрываются: проверяется
  нормализованный путь, а не его реальное расположение.
* Через `/ipc` ответ команды — JSON в виде строки (особенность конверта IPC).

## Тесты

См. `experiments/issue1478/README.md`. В CI (`.github/workflows/nodtests.yml`)
выполняются модульные проверки логики и HTTP-регрессия маршрута.
