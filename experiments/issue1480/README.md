# HTTP Server: черчение зоны помещения с номером (#1480)

Документация команд: `cad_source/zcad/velec/IPCserver/SPACE_ADD_ROOM.md`.

## Воспроизведение в ZCAD

1. Собрать ZCAD с электротехнической конфигурацией
   (`make cleanzcadelectrotech LP=... PCP=...`), открыть чертёж и запустить
   `HTTPStart` (по умолчанию `127.0.0.1:5000`).
2. Выполнить:

   ```sh
   curl -s -X POST http://127.0.0.1:5000/ipc -d '{"id":"1","cmd":"SPACE_ADD_ROOM",
     "args":[{"points":[[0,0],[6000,0],[6000,4000],[0,4000]],"number":"101",
     "name":"Кабинет"}]}'
   ```

3. До изменения команды не было (`Unknown command`). Теперь ответ содержит
   `"ok":true`, площадь, габарит и переменные зоны. На слое `spaceRoom`
   появляется замкнутая полилиния цвета 5; в инспекторе у неё те же
   расширения и переменные, что у зоны, начерченной кнопкой «Помещение»,
   и заполнены `Space_RoomPos = 101`, `Space_RoomName = Кабинет`.
4. `Ctrl+Z` удаляет зону одной операцией.
5. Повторить запрос без `number` — `MISSING_NUMBER`; с самопересекающимся
   контуром `[[0,0],[10,10],[10,0],[0,10]]` — `SELF_INTERSECTING`; в обоих
   случаях чертёж не меняется.
6. Кнопка «Помещение» панели «Пространства» (интерактивная `SpaceAddPoly`)
   работает как прежде.

## Автоматические проверки

Из корня репозитория; нужен FPC 3.2.2 с FCL (fcl-json). Проверка выполняется
в CI (`.github/workflows/nodtests.yml`, шаг «Space room regression»).

### Логика команды — `test_space_room.pas`

```sh
mkdir -p /tmp/zcad-space1480
fpc -Fucad_source/zcad/velec/IPCserver -FU/tmp/zcad-space1480 \
  -FE/tmp/zcad-space1480 experiments/issue1480/test_space_room.pas
/tmp/zcad-space1480/test_space_room
```

Проверяет модуль `uzvhttpspaceroom` без ZCAD:

* пресеты операндов `room`/`floor`/`section`/`building` совпадают
  с кнопками `vspaceactionscontent.xml` (файл читается тестом);
* разбор операндов как в `ParseOperandsToStruct` (цвет, слой, тройки
  переменных, кавычки, кириллица) и обратная сборка строки операндов;
* контур: формы вершин, отбрасывание дублей и замыкающей вершины, минимум
  вершин, самопересечение, нулевая площадь, площадь/периметр/габарит/центр;
* значения переменных: сокращения по видам, `values`, приоритет сокращений,
  приведение к `String`/`Integer`/`Double`/`Boolean`, ошибки типов;
* коды ошибок и формат JSON успеха и ошибок, режим пакета.

При любой ошибке процесс завершается с кодом 1.

### Примеры ответов — `print_sample.pas`

```sh
fpc -Fucad_source/zcad/velec/IPCserver -FU/tmp/zcad-space1480 \
  -FE/tmp/zcad-space1480 experiments/issue1480/print_sample.pas
/tmp/zcad-space1480/print_sample
```

Печатает ответы для помещения, этажа и двух ошибочных запросов (по ним
составлены примеры в `SPACE_ADD_ROOM.md`).

### Сборка ZCAD

Код на стороне ZCAD (`uzvhttpcmdspace`, `SpaceAddPolyFromPoints`,
`ApplySpaceExtensions`) проверяется полной сборкой
`make cleanzcadelectrotech`; в CI ZCAD не собирается.
