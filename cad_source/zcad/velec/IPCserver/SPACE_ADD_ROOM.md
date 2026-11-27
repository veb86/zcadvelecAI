# SPACE_ADD_ROOM / SPACE_ADD_POLY — черчение зоны помещения с номером

Команды HTTP-сервера ZCAD (issue #1480). Позволяют внешнему клиенту (ИИ,
MCP-инструменту, скрипту) начертить зону помещения — замкнутый контур
с номером, наименованием, категорией и другими атрибутами — **так же, как
это делает пользователь кнопкой «Помещение» панели «Пространства»**
(команда `SpaceAddPoly`).

| Команда | Назначение |
|---------|------------|
| `SPACE_ADD_ROOM` | Зона помещения (`kind = room`). Номер (`number`) обязателен |
| `SPACE_ADD_POLY` | Любая зона панели «Пространства»: `room`, `floor`, `section`, `building` или `custom` с произвольной строкой операндов |

Существующие команды и маршруты (`/ipc`, `/widgets/*`, Grist-маршруты,
`EXPORT_SELECTION`, …) и интерактивные команды `SpaceAddPoly`/`spaceadd`
не изменены.

## Как работает SpaceAddPoly

`SpaceAddPoly` — это команда `3DPoly` с обработчиком стадий создания
примитива (`p3dplESP`). Кнопки панели вызывают её с операндами вида

```
<цвет>,'<слой>',<переменная>,'<имя для пользователя>',<тип>[, ...]
```

например, кнопка «Помещение» (`vspaceactionscontent.xml`):

```
SpaceAddPoly(5,'spaceRoom',Space_RoomPos, 'RoomPos', String, Space_RoomName,
  'RoomName', String, Space_RoomArea, 'RoomArea', String, Space_RoomCategory,
  'RoomCategory', String, Space_RoomIllumination, 'RoomIllumination Лк', Integer)
```

Операнды разбирает `ParseOperandsToStruct` (`uzvcommand_spaceutils`).
После того как пользователь указал вершины, на стадии `ESSSetEntity`
к замкнутой 3D-полилинии применяются:

* расширение `extdrVariables` и переменные из операндов (значение `0`);
* расширение `extdrIncludingVolume` (объём/принадлежность устройств зоне);
* слой из операндов (создаётся при отсутствии, цвет слоя — из операндов);
* цвет примитива из операндов, толщина линии «ПоСлою», `Closed = True`.

Значения переменных (номер помещения и т.д.) пользователь затем вводит
в инспекторе. HTTP-команда делает то же самое за один вызов: тот же
разбор операндов `ParseOperandsToStruct`, та же общая процедура
`ApplySpaceExtensions` (`uzvcommand_spaceadd`), а затем записывает
значения переменных, пришедшие в запросе.

## Модули

| Файл | Назначение |
|------|------------|
| `uzvhttpspaceroom.pas` | Чистая логика без ZCAD: пресеты операндов, разбор и проверка запроса, геометрия контура (площадь, периметр, габарит, центр), JSON ответа и ошибок |
| `uzvhttpcmdspace.pas` | Регистрация `SPACE_ADD_ROOM`/`SPACE_ADD_POLY` в HTTP IPC и вызов `SpaceAddPolyFromPoints` |
| `../space/uzcvcommand_spaceaddpoly.pas` | `SpaceAddPolyFromPoints` — создание зоны по вершинам без интерактива |
| `../space/uzvcommand_spaceadd.pas` | `ApplySpaceExtensions` — общие расширения зоны для интерактивной и HTTP-команды |

## Запрос

Через конверт HTTP IPC (`POST /ipc`); единственный аргумент — JSON-объект
(или строка с JSON-объектом):

```json
{
  "id": "1",
  "cmd": "SPACE_ADD_ROOM",
  "args": [{
    "points": [[0, 0], [6000, 0], [6000, 4000], [0, 4000]],
    "number": "101",
    "name": "Кабинет",
    "category": "Д",
    "illumination": 300
  }]
}
```

| Поле | По умолчанию | Описание |
|------|--------------|----------|
| `points` | — (обязательно) | Вершины контура в координатах чертежа: `[x, y]`, `[x, y, z]` или `{"x":..,"y":..,"z":..}`. Не менее 3 различных вершин, не более 4096. Повтор первой вершины в конце и подряд идущие дубли отбрасываются. Контур не должен самопересекаться и иметь нулевую площадь |
| `kind` | `room` | Только `SPACE_ADD_POLY`: `room`, `floor`, `section`, `building`, `custom`. Для `SPACE_ADD_ROOM` допустим только `room` |
| `operands` | операнды кнопки вида | Строка операндов как у кнопки панели. Если задана без `kind`, вид — `custom` |
| `layer` | слой из операндов | Переопределение слоя |
| `color` | цвет из операндов | Цвет 0..256 (256 — ПоСлою) |
| `number` | — | Номер зоны: `Space_RoomPos` (room), `space_Floor` (floor), `space_Section` (section), `space_Building` (building). **Обязателен для `SPACE_ADD_ROOM`** |
| `name`, `area`, `category`, `illumination` | — | Только room: `Space_RoomName`, `Space_RoomArea`, `Space_RoomCategory`, `Space_RoomIllumination` |
| `height`, `scale` | — | Только floor: `space_FloorHeight`, `space_FloorScale` |
| `values` | — | Произвольные переменные `{"ИмяПеременной": значение}`. Необъявленные в операндах переменные добавляются (тип по значению JSON: строка, целое, вещественное, логическое). Сокращения (`number`, `name`, …) важнее `values` |
| `select` | `false` | `true` — выделить созданную зону (как после интерактивной команды) |

Неизвестные поля и сокращения, не относящиеся к виду зоны, — ошибка
`INVALID_REQUEST` (защита от опечаток клиента). Значения проверяются по
типу переменной из операндов: `Integer` — целое, `Double` — число (строка
допускает и `,`, и `.`), `Boolean` — `true/false`, `String` — одна строка.

Площадь зоны ZCAD не вычисляет сам: если нужна переменная `Space_RoomArea`,
передайте её (например, `"area": "24.0"`); значение `area` из ответа —
площадь контура в квадратных единицах чертежа (для мм — мм²).

## Успешный ответ

`result` конверта — JSON-объект в виде строки:

```json
{
  "ok": true,
  "command": "space_add_room",
  "kind": "room",
  "layer": "spaceRoom",
  "color": 5,
  "operands": "5,'spaceRoom',Space_RoomPos,'RoomPos',String,...",
  "closed": true,
  "vertex_count": 4,
  "points": [[0, 0, 0], [6000, 0, 0], [6000, 4000, 0], [0, 4000, 0]],
  "area": 24000000,
  "perimeter": 20000,
  "bbox": { "min": [0, 0], "max": [6000, 4000] },
  "centroid": [3000, 2000],
  "variables": {
    "Space_RoomPos": "101",
    "Space_RoomName": "Кабинет",
    "Space_RoomArea": "0",
    "Space_RoomCategory": "Д",
    "Space_RoomIllumination": "300"
  },
  "batch": false,
  "message": "Zone created"
}
```

* `operands` — итоговая строка операндов, с которой создана зона (в том
  числе добавленные переменные). Ею же можно повторить зону вручную:
  `SpaceAddPoly(<operands>)`;
* `variables` — все переменные зоны; незаданные получают `0`, как
  в интерактивной команде;
* числа FPC печатает в экспоненциальной форме (`2.4E+007`), это обычный JSON.

## Ошибка

`status = "error"`, `error` конверта — JSON-объект в виде строки:

```json
{
  "ok": false,
  "command": "space_add_room",
  "error": { "code": "SELF_INTERSECTING", "message": "Zone contour must not intersect itself" }
}
```

| Код | Причина |
|-----|---------|
| `INVALID_REQUEST` | `args` не один объект, неверный JSON, неизвестное поле, неверный тип поля, сокращение не для этого вида |
| `INVALID_KIND` | Неизвестный `kind`, либо `kind` ≠ `room` у `SPACE_ADD_ROOM` |
| `INVALID_OPERANDS` | Пустые/неразбираемые операнды, `custom` без `operands`, недопустимые имя/тип переменной |
| `INVALID_POINTS` | Нет `points`, координата не число, меньше 3 вершин, больше 4096, нулевая площадь |
| `SELF_INTERSECTING` | Контур самопересекается |
| `INVALID_VALUE` | Значение не соответствует типу переменной, многострочная строка, имя переменной не латинский идентификатор |
| `MISSING_NUMBER` | `SPACE_ADD_ROOM` без непустого номера |
| `NO_DOCUMENT` | Нет открытого чертежа |
| `CREATE_FAILED` | ZCAD не смог создать примитив (сообщение исключения в `message`) |
| `INTERNAL_ERROR` | Непредвиденное исключение |

Ошибки не роняют ZCAD: при любой ошибке зона в чертёж не добавляется.

## Пакетный режим

Между `BEGIN_BATCH` и `END_BATCH` зона добавляется в `ConstructRoot`
и попадает в чертёж одной операцией отмены при `END_BATCH` (как `LINE`,
`CIRCLE` и т.д.); в ответе `"batch": true`. Вне пакета зона добавляется
с записью в стек отмены (`Ctrl+Z` удаляет её) и чертёж перерисовывается.

## Примеры

```bash
# Помещение 101
curl -s -X POST http://127.0.0.1:5000/ipc -d '{"id":"1","cmd":"SPACE_ADD_ROOM",
  "args":[{"points":[[0,0],[6000,0],[6000,4000],[0,4000]],"number":"101",
  "name":"Кабинет"}]}'

# Г-образное помещение, выделить после создания
curl -s -X POST http://127.0.0.1:5000/ipc -d '{"id":"2","cmd":"SPACE_ADD_ROOM",
  "args":[{"points":[[0,0],[8000,0],[8000,3000],[3000,3000],[3000,6000],[0,6000]],
  "number":"102","select":true}]}'

# Этаж
curl -s -X POST http://127.0.0.1:5000/ipc -d '{"id":"3","cmd":"SPACE_ADD_POLY",
  "args":[{"kind":"floor","points":[[0,0],[30000,0],[30000,12000],[0,12000]],
  "number":"1","height":3.3}]}'

# Произвольная зона со своими операндами
curl -s -X POST http://127.0.0.1:5000/ipc -d '{"id":"4","cmd":"SPACE_ADD_POLY",
  "args":[{"operands":"3,'"'"'spaceZone'"'"',Zone_Num,'"'"'Num'"'"',String",
  "points":[[0,0],[1000,0],[1000,1000]],"values":{"Zone_Num":"A1"}}]}'
```

## Python-клиент (MCP-инструмент `zcad_add_room`)

```python
import json
import os
import requests

ZCAD_URL = os.environ.get("ZCAD_HTTP_URL", "http://127.0.0.1:5000")


def zcad_add_room(points, number, name=None, category=None, **values):
    """Чертит зону помещения с номером и возвращает ответ ZCAD."""
    args = {"points": points, "number": str(number)}
    if name is not None:
        args["name"] = name
    if category is not None:
        args["category"] = category
    if values:
        args["values"] = values
    reply = requests.post(f"{ZCAD_URL}/ipc", timeout=30, json={
        "id": "room", "cmd": "SPACE_ADD_ROOM", "args": [args]}).json()
    if reply["status"] != "ok":
        error = json.loads(reply["error"])["error"]
        raise RuntimeError(f'{error["code"]}: {error["message"]}')
    return json.loads(reply["result"])


room = zcad_add_room([[0, 0], [6000, 0], [6000, 4000], [0, 4000]], 101,
                     name="Кабинет")
print(room["area"], room["variables"]["Space_RoomPos"])
```

## Ограничения

* Контур — 3D-полилиния, как у `SpaceAddPoly`; дуги не поддерживаются.
* Самопересечение проверяется в плоскости XY.
* Если в момент HTTP-вызова в ZCAD выполняется интерактивная команда
  черчения, её завершение очищает стек отмены от своей позиции
  (`UndoStack.ClearFrom`), и запись отмены зоны, созданной во время этой
  команды, может пропасть (сама зона остаётся в чертеже).
* `HTTPStart` требует открытого чертежа; `NO_DOCUMENT` возвращается, если
  все чертежи закрыты после запуска сервера.

## Тесты

См. `experiments/issue1480/README.md`. В CI (`.github/workflows/nodtests.yml`,
шаг «Space room regression») выполняются модульные проверки
`uzvhttpspaceroom`.
