# Техническое задание: Команда proxytoblock

## 1. Цель

Реализовать команду `proxytoblock`, которая конвертирует выделенные
`ACAD_PROXY_ENTITY` в блоки ZCAD с уникальными именами.

Proxy-объекты содержат бинарные данные AutoCAD (Proxy Graphic в формате
AcGiWorldDraw), которые уже разбираются модулем `uzeentproxygraphicparser`
на составные примитивы (линии, круги, дуги, полигоны, текст и т.д.).
Команда использует результаты этого разбора для создания эквивалентных
сущностей ZCAD внутри блока.

## 2. Пользовательский сценарий

1. Пользователь открывает чертёж, содержащий `ACAD_PROXY_ENTITY`.
2. Пользователь выделяет нужные прокси-объекты (один или несколько).
3. Пользователь вводит команду `proxytoblock`.
4. Для каждого выделенного прокси-объекта команда:
   - Разбирает proxy-данные на составные примитивы.
   - Создаёт новое определение блока (`GDBObjBlockdef`) с уникальным
     именем `ZPE1`, `ZPE2`, ... `ZPEn`.
   - Добавляет примитивы (линии, круги, дуги, полигоны, текст)
     в определение блока.
   - Вставляет блок (`GDBObjBlockInsert`) в чертёж на том же самом
     месте с той же базовой точкой, что и исходный прокси-объект.
   - Удаляет исходный прокси-объект из чертежа.
5. Пользователь видит результат: вместо прокси-объектов — блоки
   с редактируемыми примитивами.

## 3. Алгоритм генерации уникальных имён

- Префикс имени блока: `ZPE` (ZCAD Proxy Entity).
- Суффикс: натуральное число, начиная с 1.
- Перед началом обработки определяется следующий свободный номер:
  перебираются все определения блоков (`BlockDefArray`) текущего чертежа.
  Для каждого блока, чьё имя начинается с `ZPE`, извлекается числовой
  суффикс и находится максимальный. Следующий номер = максимальный + 1.
- Это гарантирует уникальность при повторных вызовах команды.

## 4. Базовая точка блока

Базовой точкой каждого блока является центр BBox прокси-объекта
(среднее арифметическое минимального и максимального углов BBox).
Все координаты примитивов внутри блока сохраняются в абсолютных
координатах (мировая система), а точка вставки блока совпадает
с базовой точкой, масштаб = 1.0, угол = 0.0.

## 5. Конвертация примитивов

### 5.1. Контуры (FContours)

Каждый контур из `TProxyGraphicParseResult.Contours` содержит набор
вершин (`GDBPoint3DArray`), флаг замкнутости (`Closed`) и флаг заливки
(`Filled`).

- **Незамкнутый контур из 2 вершин** → `ENTF_CreateLine`
  (линия от вершины 0 до вершины 1).
- **Незамкнутый контур из 3+ вершин** → серия `ENTF_CreateLine`
  (от вершины [i] до вершины [i+1]).
- **Замкнутый контур** → серия `ENTF_CreateLine` + замыкающая линия
  от последней вершины к первой.

### 5.2. Текстовые примитивы (FTextItems)

Каждый `TProxyTextItem` содержит точку вставки, текст, высоту,
масштаб по ширине, угол и имя шрифта.

- Создаётся через `ENTF_CreateMText` (если будет доступна) или
  через фабрику сущностей (`_StandartMTextCreateProcedure`).

### 5.3. Заполненные контуры (Filled)

- На данном этапе заполненные контуры конвертируются как обычные
  замкнутые полилинии из линий (без SOLID заливки).

## 6. Удаление прокси-объекта

После успешного создания блока и его вставки исходный прокси-объект
удаляется из чертежа с поддержкой Undo/Redo:
- Используется `PushMultiObjectCreateCommand` + `GoodRemoveMiFromArray`
  аналогично реализации команды `Erase`.

## 7. Архитектура модулей

Расположение: `cad_source/zcad/velec/proxytoblock/`

| Файл | Назначение |
|------|------------|
| `uzvproxytoblock.pas` | Основной модуль команды `proxytoblock` |
| `proxytoblock_spec.md` | Данное техническое задание |

### Структура `uzvproxytoblock.pas`:

```
unit uzvproxytoblock;

interface
uses ...;

implementation

{ Находит следующий свободный номер для имени ZPEn }
function FindNextFreeZPENumber(...): Integer;

{ Создаёт определение блока и заполняет его примитивами из прокси }
function CreateBlockDefFromProxy(...): PGDBObjBlockdef;

{ Вставляет блок в чертёж на позицию прокси-объекта }
function InsertBlockAtProxy(...): PGDBObjBlockInsert;

{ Главная функция команды }
function proxytoblock_com(...): TCommandResult;

initialization
  CreateZCADCommand(@proxytoblock_com, 'proxytoblock', CADWG, 0);
finalization
end.
```

## 8. Зависимости

- `uzeentacdproxy` — класс `GDBObjAcdProxy` (доступ к полям `FContours`,
  `FTextItems`, `FBBoxMinInOCS`, `FBBoxMaxInOCS` через новые
  публичные свойства или методы).
- `uzeentitiesmanager` — функции `ENTF_CreateLine`, `ENTF_CreateCircle`
  и др.
- `uzeblockdef` — класс `GDBObjBlockdef`.
- `uzeentblockinsert` — класс `GDBObjBlockInsert`.
- `UGDBObjBlockdefArray` — массив определений блоков.
- `uzcdrawings` — доступ к текущему чертежу.
- `uzccommandsimpl` — регистрация команды.
- `uzclog` — логирование.

## 9. Доступ к данным прокси-объекта

Поля `FContours`, `FTextItems`, `FBBoxMinInOCS`, `FBBoxMaxInOCS`,
`FHasCenterPoint`, `FCenterPoint` объекта `GDBObjAcdProxy` объявлены
как `private`. Для доступа из команды `proxytoblock` необходимо
добавить публичные свойства:

```pascal
property ContourCount: Integer read FContourCount;
property BBoxMin: TzePoint3d read FBBoxMinInOCS;
property BBoxMax: TzePoint3d read FBBoxMaxInOCS;
property BBoxLoaded: Boolean read FBBoxLoaded;
property CenterPoint: TzePoint3d read FCenterPoint;
property HasCenterPoint: Boolean read FHasCenterPoint;
```

И публичные методы для доступа к контурам и текстам:

```pascal
function GetContour(Index: Integer): TProxyContour;
function GetTextItemCount: Integer;
function GetTextItem(Index: Integer): TProxyTextItem;
procedure EnsureParsed(var drawing: TDrawingDef; var DC: TDrawContext);
```

## 10. Обработка ошибок

- Если ни один объект не выделен — сообщение в историю команд.
- Если выделенный объект не является `GDBObjAcdProxy` — пропускается.
- Если прокси-объект не содержит данных (пустой Proxy Graphic) —
  пропускается с сообщением.
- Если ни один примитив не удалось распарсить — объект пропускается.
- Все действия логируются через `programlog.LogOutFormatStr`.

## 11. Ограничения первой версии

- Круги и дуги конвертируются как полилинии из линий (аппроксимация
  уже выполнена парсером в вершины контуров).
- Текстовые примитивы в первой версии не конвертируются (будет
  добавлено в следующей итерации, когда будет доступна
  фабрика создания текстовых сущностей).
- SOLID заливка не конвертируется — заполненные контуры
  отображаются как замкнутые полилинии.
- Undo/Redo для удаления прокси-объекта реализуется по аналогии
  с командой `Erase`.
