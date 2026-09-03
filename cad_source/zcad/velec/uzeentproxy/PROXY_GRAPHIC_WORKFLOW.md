# ProxyEntity: извлечение OpCode, разбор и отрисовка

Документ описывает текущую реализацию `ACAD_PROXY_ENTITY` в папке
`cad_source/zcad/velec/uzeentproxy`. Основная цепочка такая:

1. `GDBObjAcdProxy.LoadFromDXF` читает DXF-сущность `ACAD_PROXY_ENTITY`.
2. Все группы `310` склеиваются в hex-строку и превращаются в `FProxyDataBytes`.
3. `GDBObjAcdProxy.FormatEntity` вызывает `ParseProxyData`.
4. `ParseProxyData` создает `TProxyGraphicParser` и передает ему сырые байты.
5. `TProxyGraphicParser` читает бинарный поток команд Proxy Graphic.
6. Системные OpCode меняют состояние парсера, геометрические OpCode уходят в
   зарегистрированные обработчики `TProxyOpCodeDispatcher`.
7. Результат разбора сохраняется как набор контуров `FContours` и текстовых
   элементов `FTextItems`.
8. На стадии `EFDraw` контуры и текст добавляются в `Representation`.
9. `DrawGeometry` уже не разбирает proxy graphic, а вызывает стандартный
   `Representation.DrawGeometry`.

## Где хранится исходный Proxy Graphic

Главный объект - `GDBObjAcdProxy` в `uzeentacdproxy.pas`.

В `LoadFromDXF` читаются стандартные данные сущности через
`LoadFromDXFObjShared`, а proxy-специфичные DXF-коды обрабатываются отдельно:

| DXF-код | Назначение |
|---|---|
| `90` | ID proxy-класса |
| `91` | ID класса приложения |
| `92`, `160` | размер proxy graphic, сейчас пропускается и пересчитывается |
| `93` | размер entity data |
| `94` | размер object data |
| `95` | формат чертежа |
| `70` | формат исходных данных |
| `310` | hex-чанк бинарного Proxy Graphic |

Все `310` накапливаются в `HexAccum`, затем `HexStringToBytes` превращает их в
массив байтов `FProxyDataBytes`. Именно эти байты дальше разбираются и они же
используются при сохранении в `SaveToDXF` для round-trip записи.

## Формат бинарного потока

Поток читает `TProxyByteStream` из `uzeentproxystream.pas`. Это маленькая
обертка над `TBytes` с текущим индексом и методами чтения little-endian значений:

- `ReadInt32`, `ReadUInt32`, `ReadDouble`, `ReadFloat`, `ReadByte`;
- `ReadVertex` / `ReadVector` - три `Double`, то есть 24 байта;
- `ReadPoint2D` - два `Double`;
- ANSI и Unicode строки.

`TProxyGraphicParser.ParseHeader` ожидает начало блока:

```text
[ChunkSize: Int32] [CommandCount: Int32]
```

Дальше `CommandCount` раз вызывается `ParseCommand`, где каждая команда имеет
заголовок:

```text
[CommandSize: Int32] [OpCode: Int32] [Data...]
```

`CommandSize` считается от начала команды и включает оба поля заголовка.
После обработки `ParseCommand` сверяет текущую позицию с `ExpectedEnd`. Если
обработчик прочитал не все байты команды, остаток пропускается. Это важно для
команд с дополнительными хвостовыми данными: следующий OpCode должен читаться
строго с границы следующей команды.

## Системные OpCode

Системные команды обрабатываются прямо в `TProxyGraphicParser`, без отдельного
модуля-парсера. Они не создают контуры, а меняют состояние разбора:

| OpCode | Обработчик | Что делает |
|---:|---|---|
| `1` | `HandleExtents` | читает Min/Max BBox из файла |
| `14` | `HandleSetColor` | устанавливает текущий цвет |
| `16` | `HandleSetLayer` | устанавливает текущий слой как строковый индекс |
| `18` | `HandleSetLinetype` | устанавливает текущий тип линии как строковый индекс |
| `19` | `HandleSetMarker` | читает и игнорирует selection marker |
| `20` | `HandleSetFill` | включает/выключает заливку для следующего примитива |
| `22` | `HandleSetTrueColor` | устанавливает RGB true color |
| `23` | `HandleSetLineweight` | устанавливает текущий lineweight |
| `24` | `HandleSetLtScale` | устанавливает масштаб типа линии |
| `25` | `HandleSetThickness` | устанавливает thickness |
| `29`, `30` | `HandlePushMatrix` | читает 4x4 матрицу и кладет ее в стек |
| `31` | `HandlePopMatrix` | снимает матрицу со стека |

Текущее состояние хранится в `TProxyGraphicState`: цвет, слой, тип линии, вес
линии, масштаб типа линии, толщина, true color. При добавлении контура эти
значения копируются в `TProxyContour`, чтобы у каждого примитива были свои
атрибуты, действовавшие на момент его создания.

Флаг заливки `FFillActive` живет отдельно. Если перед замкнутым контуром пришел
`SetFill(1)`, то этот контур помечается как `Filled=True`. После каждого
успешного графического примитива флаг сбрасывается.

## Регистрация и диспетчеризация OpCode

Регистрация геометрических команд находится в `uzeentproxymanager.pas`.

`TProxyOpCodeDispatcher` содержит таблицу `FTable[0..255]`, где индекс - это
числовой OpCode. Каждый модуль конкретного примитива в секции `initialization`
вызывает:

```pascal
TProxyOpCodeDispatcher.RegisterOpCode(OpCode, Name, Handler);
```

Поэтому подключение модулей в `uses` у `uzeentacdproxy.pas` одновременно
включает поддержку соответствующих OpCode. Если убрать модуль из `uses`, его
`initialization` не выполнится, обработчик не зарегистрируется, и команда будет
пропущена как неизвестная.

В `ParseCommand` логика такая:

1. Если OpCode системный - вызвать локальный `Handle...`.
2. Иначе проверить `TProxyOpCodeDispatcher.IsRegistered(OpCode)`.
3. Если обработчик есть - вызвать `HandleOpCode`.
4. Если обработчика нет - пропустить `CommandSize - 8` байт.
5. Если обработчик вернул `Valid=True`, применить активную матрицу, слить BBox,
   добавить контур и/или текстовый элемент.

## Результат обработчика примитива

Каждый обработчик заполняет `TProxyHandlerResult`:

| Поле | Назначение |
|---|---|
| `Valid` | команда успешно разобрана |
| `Vertices` | вершины контура для отрисовки |
| `HasVertices` | вершины действительно есть |
| `Closed` | контур замкнут |
| `Filled` | контур уже помечен как заполненный |
| `BBoxMin`, `BBoxMax`, `HasBBox` | BBox примитива |
| `TextItem`, `HasTextItem` | данные текста, если это текстовый OpCode |

Геометрия внутри proxy не превращается в полноценные ZCAD-сущности
`GDBObjLine`, `GDBObjCircle` и т.п. В текущей реализации она приводится к
простым промежуточным структурам:

- контуры `TProxyContour` с массивом точек `GDBPoint3DArray`;
- текстовые элементы `TProxyTextItem`;
- суммарный BBox.

## Поддерживаемые геометрические OpCode

| OpCode | Модуль | Что читает | Что отдает для отрисовки |
|---:|---|---|---|
| `2` | `uzeentproxyparsercircle.pas` | Center, Radius, Normal | замкнутую тесселированную окружность |
| `4` | `uzeentproxyparserarc.pas` | Center, Radius, Normal, StartVector, SweepAngle, ArcType | тесселированную дугу |
| `6` | `uzeentproxyparserpolyline.pas` | VertexCount и 3D-вершины | незамкнутую полилинию |
| `7` | `uzeentproxyparserpolygon.pas` | VertexCount и 3D-вершины | замкнутый полигон с повтором первой точки |
| `9` | `uzeentproxyparsershell.pas` | вершины, face entries, индексы граней | набор замкнутых контуров граней |
| `10` | `uzeentproxyparsertext.pas` | ANSI text, insert, normal, direction, height, width factor | `TextItem`, без вершин |
| `32` | `uzeentproxyparserpolylinewithnormals.pas` | VertexCount, 3D-вершины, общая нормаль | незамкнутую полилинию |
| `33` | `uzeentproxyparserlwpolyline.pas` | flags, elevation, normal, 2D-вершины, bulge/width | 2D полилинию в Z=elevation |
| `38` | `uzeentproxyparsertext.pas` | UnicodeText2 с расширенными полями и font name | `TextItem`, без вершин |
| `44` | `uzeentproxyparserellipse.pas` | Center, Normal, MajorAxisVector, MinorAxisRatio, StartParam, EndParam | тесселированную эллиптическую дугу |

Важная особенность: кривые сейчас отрисовываются не как аналитические окружности,
дуги или эллипсы, а как набор сегментов полилинии. Это относится к кругу, дуге и
эллипсу. `LwPolyline` читает bulge, но тесселяция bulge-сегментов сейчас не
реализована: значение пропускается.

## Матрицы и координаты

Внутри Proxy Graphic могут встречаться `PushMatrix` / `PopMatrix`.
`HandlePushMatrix` читает 16 `Double`, транспонирует данные в формат матрицы
ZCAD и кладет матрицу в стек. После успешного разбора примитива
`TransformHandlerVertices` применяет верхнюю матрицу стека:

- ко всем вершинам контура;
- к `BBoxMin` / `BBoxMax`;
- к точке вставки текста.

После полного разбора `GDBObjAcdProxy.FormatEntity` дополнительно проверяет
`bp.ListPos.owner`. Если proxy находится внутри блока, `ApplyOwnerMatrix`
применяет матрицу владельца к контурам, BBox и точкам вставки текста. Это
переводит локальные координаты блока в WCS.

## Как строится BBox

BBox нужен для видимости, выбора и контрольной точки.

Источники BBox:

1. `OpCode=1 Extents` может задать начальный BBox из файла.
2. Каждый поддержанный примитив возвращает свой `BBoxMin/BBoxMax`.
3. `MergeHandlerBBox` объединяет BBox примитивов в общий `FResult`.
4. `ParseProxyData` копирует общий BBox в `FBBoxMinInOCS/FBBoxMaxInOCS`.
5. `FormatEntity` записывает его в `vp.BoundingBox`.

Если ни один поддержанный примитив не найден, `FBBoxLoaded=False`, BBox
сбрасывается в `NulVertex`, а объект фактически не имеет нормальной геометрии
для отображения.

## Чем выполняется отрисовка

Отрисовка идет через стандартное представление ZCAD `Representation`, а не
через прямые вызовы `DC.drawer.DrawLine3DInModelSpace`.

### Контуры

На стадии `EFDraw` `GDBObjAcdProxy.FormatEntity` очищает `Representation`, затем
проходит по `FContours`.

Для каждого контура вызывается:

```pascal
Representation.DrawPolyLineWithLT(
  DC, FContours[I].Vertices, ContourVP,
  FContours[I].Closed, True);
```

То есть основная графическая примитива отрисовки proxy-контуров - полилиния с
поддержкой типа линии (`DrawPolyLineWithLT`). Через нее рисуются:

- линии и полилинии;
- контуры полигонов;
- тесселированные круги;
- тесселированные дуги;
- тесселированные эллипсы;
- ребра shell/polyface.

Для веса линии берется `FContours[I].LineWeight`, если он не равен
`LnWtByLayer`, `LnWtByBlock` или `LnWtByLwDefault`. В таком случае он
подставляется в локальную копию visual properties `ContourVP`.

### Заливки

Если у контура `Filled=True`, перед обводкой создается triangulator:

```pascal
Triangulator.BeginPolygon(@Representation, HatchTess);
Triangulator.BeginContour(HatchTess);
Triangulator.TessVertex(HatchTess, pV^);
Triangulator.EndContour(HatchTess);
Triangulator.EndPolygon(HatchTess);
```

Заливка формируется в `Representation.Geometry` через `uzeTriangulator`,
аналогично SOLID-заливке hatch. После заливки контур все равно обводится через
`DrawPolyLineWithLT`.

### Текст

Текстовые OpCode не создают полилинию. Парсер текста заполняет `TProxyTextItem`.
После контуров `FormatEntity` вызывает `DrawTextItems`.

`DrawTextItems`:

1. ищет текстовый стиль по `FontName`;
2. если не найден, берет `Standard`;
3. строит матрицы переноса, поворота и масштаба;
4. вызывает:

```pascal
Representation.DrawTextContent(
  DC.drawer,
  Item.Text,
  TXTStyle^.pfont,
  DrawMatrix,
  ObjMatrix,
  Item.Height,
  TextOutbound);
```

То есть текст рисуется стандартным текстовым механизмом `Representation`, а не
тесселируется вручную в линии внутри proxy-парсера.

### Финальная отрисовка

`GDBObjAcdProxy.DrawGeometry` делает только:

```pascal
Representation.DrawGeometry(DC, vp.BoundingBox, inFrustumState);
inherited;
```

Вся подготовка геометрии происходит раньше в `FormatEntity`. Поэтому если нужно
понять, что реально попадет на экран, смотреть нужно на заполнение
`Representation` в `FormatEntity`, а не на `DrawGeometry`.

## Что сейчас не реализовано или ограничено

- Неизвестные OpCode просто пропускаются.
- `LwPolyline` читает `Bulge`, `StartWidth`, `EndWidth`, но bulge не
  тесселируется.
- Атрибуты слоя и типа линии сохраняются как строковые индексы из proxy graphic;
  полноценного поиска имен слоев/типов линий по таблицам нет.
- `TransformAt` у `GDBObjAcdProxy` пока не изменяет `FProxyDataBytes` и не
  трансформирует сохраненную proxy-геометрию.
- Большинство примитивов превращается в полилинейные аппроксимации, а не в
  нативные аналитические сущности ZCAD.
- `CalcTrueInFrustum` использует только две точки BBox, что достаточно для
  простого отсечения, но не является полным тестом всех углов повернутого BBox.

## Краткая карта файлов

| Файл | Роль |
|---|---|
| `uzeentacdproxy.pas` | DXF-сущность `ACAD_PROXY_ENTITY`, хранение байтов, запуск парсера, заполнение `Representation` |
| `uzeentproxygraphicparser.pas` | общий разбор блока Proxy Graphic, системные OpCode, состояние, матрицы, сбор результата |
| `uzeentproxystream.pas` | чтение бинарного little-endian потока |
| `uzeentproxymanager.pas` | регистрация и вызов обработчиков OpCode |
| `uzeentproxytypes.pas` | перечисления OpCode, состояния и структуры данных |
| `uzeentproxyparsercircle.pas` | OpCode `2` |
| `uzeentproxyparserarc.pas` | OpCode `4` |
| `uzeentproxyparserpolyline.pas` | OpCode `6` |
| `uzeentproxyparserpolygon.pas` | OpCode `7` |
| `uzeentproxyparsershell.pas` | OpCode `9` |
| `uzeentproxyparsertext.pas` | OpCode `10`, `38` |
| `uzeentproxyparserpolylinewithnormals.pas` | OpCode `32` |
| `uzeentproxyparserlwpolyline.pas` | OpCode `33` |
| `uzeentproxyparserellipse.pas` | OpCode `44` |

