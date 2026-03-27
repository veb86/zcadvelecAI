# План реализации сохранения Proxy-объектов и кастомных объектов

## Описание задачи

При открытии DXF-файла в ZCAD прокси-объекты (ACAD_PROXY_ENTITY) загружаются
и отображаются (круги, текст). Однако при сохранении в новый DXF-файл
прокси-объекты не записываются — метод `SaveToDXF` содержит только TODO-заглушку.

**Цель:** реализовать сохранение прокси-объектов так, чтобы при повторном
открытии файла в AutoCAD они сохраняли своё отображение как proxy-объекты.
Кастомные объекты при сохранении становятся proxy-объектами (теряют управление,
но сохраняют визуальное представление).

---

## Формат ACAD_PROXY_ENTITY в DXF

Структура записи прокси-объекта в DXF (формат R2000):

```
  0
ACAD_PROXY_ENTITY
  5
<handle>
100
AcDbEntity
  8
<layer>
100
AcDbProxyEntity
 90
<proxy_entity_class_id>     ← ID класса прокси (обычно 498)
 91
<application_entity_class_id> ← ID класса приложения
 92
<graphics_data_size>        ← размер графических данных в байтах
310
<hex_data_chunk_1>          ← бинарные данные (чанки по ~254 hex-символа)
310
<hex_data_chunk_2>
...
 93
<entity_data_size>          ← размер данных сущности (0 для чисто визуальных)
 94
0                           ← размер данных объекта (обычно 0)
 95
<object_drawing_format>     ← формат чертежа (R2000=15)
 70
<original_data_format>      ← формат исходных данных (0)
```

---

## Архитектура решения

### Принцип: Round-trip сохранение

Основная идея — сохранить все оригинальные DXF-коды, прочитанные при загрузке,
и записать их обратно при сохранении. Это обеспечивает максимальную совместимость
с AutoCAD без необходимости понимать внутреннюю логику каждого поля.

### Что уже есть

| Компонент | Статус |
|-----------|--------|
| `LoadFromDXF` — чтение кода 310 | ✅ Работает |
| `FProxyDataBytes` — хранение сырых байтов | ✅ Работает |
| `ParseProxyData` — разбор proxy graphic | ✅ Работает |
| `FormatEntity` — отображение | ✅ Работает |
| `SaveToDXFObjPrefix` — запись заголовка | ✅ Есть в базовом классе |
| `SaveToDXF` — запись данных | ❌ TODO-заглушка |
| Хранение кодов 90,91,92,93,94,95,70 | ❌ Пропускаются при загрузке |

### Что нужно реализовать

1. **Расширить `LoadFromDXF`** — сохранять коды 90, 91, 93, 94, 95, 70
   (код 92 вычисляется из длины `FProxyDataBytes`)

2. **Реализовать `SaveToDXF`** — записывать полную структуру ACAD_PROXY_ENTITY:
   - Вызвать `SaveToDXFObjPrefix('ACAD_PROXY_ENTITY', 'AcDbProxyEntity')`
   - Записать метаданные (коды 90, 91)
   - Вычислить и записать размер графических данных (код 92)
   - Записать `FProxyDataBytes` как hex-строки в код 310 (чанки по 127 байт)
   - Записать оставшиеся метаданные (коды 93, 94, 95, 70)

3. **Добавить функцию `BytesToHexString`** — обратное преобразование байт → hex

---

## Детальный план реализации

### Шаг 1: Добавить поля для хранения метаданных DXF

```pascal
{ Метаданные ACAD_PROXY_ENTITY из DXF }
FProxyClassID: Integer;       // Код 90: ID класса прокси
FAppClassID: Integer;         // Код 91: ID класса приложения
FEntityDataSize: Integer;     // Код 93: размер данных сущности
FObjectDataSize: Integer;     // Код 94: размер данных объекта
FDrawingFormat: Integer;      // Код 95: формат чертежа
FOriginalDataFormat: Integer; // Код 70: формат исходных данных
```

### Шаг 2: Расширить LoadFromDXF

Добавить обработку кодов 90, 91, 93, 94, 95, 70 в цикл чтения.
Значения по умолчанию:
- Код 90 = 498 (стандартный ID прокси-класса)
- Код 91 = 499 (стандартный ID класса приложения)
- Код 93 = 0 (нет данных сущности)
- Код 94 = 0 (нет данных объекта)
- Код 95 = 15 (формат R2000)
- Код 70 = 0 (стандартный формат)

### Шаг 3: Реализовать BytesToHexString

```pascal
function BytesToHexString(const Data: TBytes): string;
```

Преобразует массив байт обратно в hex-строку для записи в код 310.

### Шаг 4: Реализовать SaveToDXF

```pascal
procedure GDBObjAcdProxy.SaveToDXF(...);
var
  HexStr, Chunk: string;
  GraphicsSize, Offset, ChunkLen: Integer;
begin
  SaveToDXFObjPrefix(outStream, 'ACAD_PROXY_ENTITY',
    'AcDbProxyEntity', IODXFContext);

  { Метаданные прокси }
  dxfIntegerout(outStream, 90, FProxyClassID);
  dxfIntegerout(outStream, 91, FAppClassID);

  { Размер и данные proxy graphic }
  GraphicsSize := Length(FProxyDataBytes);
  dxfIntegerout(outStream, 92, GraphicsSize);

  if GraphicsSize > 0 then
  begin
    HexStr := BytesToHexString(FProxyDataBytes);
    { Записываем чанками по 254 hex-символа (127 байт) }
    Offset := 1;
    while Offset <= Length(HexStr) do
    begin
      ChunkLen := Min(254, Length(HexStr) - Offset + 1);
      Chunk := Copy(HexStr, Offset, ChunkLen);
      dxfStringWithoutEncodeOut(outStream, 310, Chunk);
      Inc(Offset, ChunkLen);
    end;
  end;

  { Оставшиеся метаданные }
  dxfIntegerout(outStream, 93, FEntityDataSize);
  dxfIntegerout(outStream, 94, FObjectDataSize);
  dxfIntegerout(outStream, 95, FDrawingFormat);
  dxfIntegerout(outStream, 70, FOriginalDataFormat);
end;
```

### Шаг 5: Обновить конструкторы и Clone

Инициализировать новые поля значениями по умолчанию.
В `Clone` — копировать `FProxyDataBytes` и метаданные.

---

## Обработка кастомных объектов

Кастомные объекты (неизвестные ZCAD сущности, которые зарегистрированы
в фабрике как ACAD_PROXY_ENTITY) при сохранении автоматически становятся
proxy-объектами, поскольку:

1. При загрузке они уже создаются как `GDBObjAcdProxy`
2. Их визуальные данные сохраняются в `FProxyDataBytes` (код 310)
3. При сохранении записываются как `ACAD_PROXY_ENTITY`
4. AutoCAD откроет их как proxy — визуальное представление сохранится,
   но специфическое поведение кастомного объекта будет утрачено

Это соответствует требованию задачи: «Кастомный объект пусть потеряет свою
возможность управления, а станет просто proxy объектом».

---

## Риски и ограничения

1. **Секция CLASSES** — в DXF есть секция CLASSES, где регистрируются
   классы кастомных объектов. Текущий шаблон сохранения не поддерживает
   динамическое добавление классов. Для полной совместимости может
   потребоваться доработка шаблона.

2. **Entity Data (код 93)** — некоторые proxy-объекты хранят
   дополнительные данные сущности помимо graphic data. Текущая реализация
   LoadFromDXF не различает graphic data и entity data (оба идут в код 310).
   Это может потребовать доработки в будущем.

3. **Трансформации** — метод `TransformAt` пока не реализован.
   Перемещение/вращение прокси-объекта не изменяет `FProxyDataBytes`.

---

## Файлы для изменения

| Файл | Изменения |
|------|-----------|
| `uzeentacdproxy.pas` | Основные изменения: поля, LoadFromDXF, SaveToDXF, Clone, конструкторы |

---

## Тестирование

1. Открыть DXF с proxy-объектами в ZCAD
2. Сохранить как новый DXF
3. Открыть новый DXF в ZCAD — proxy-объекты должны отображаться
4. Открыть новый DXF в AutoCAD — proxy-объекты должны быть видны
5. Проверить round-trip: открыть → сохранить → открыть повторно
