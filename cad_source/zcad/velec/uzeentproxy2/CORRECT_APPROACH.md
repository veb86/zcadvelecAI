# Правильный подход к парсингу прокси-объектов

## Почему предыдущий подход был неправильным

Создание специфичных парсеров для каждого типа объекта (`TProxySPDSMarkReader`, `TProxyCircleReader`) - это:
- ❌ **Масштабируемость**: Нужно создавать парсер для КАЖДОГО нового типа
- ❌ **Поддержка**: Тысячи типов объектов (СПДС, Civil3D, Architecture, и т.д.)
- ❌ **Непрактично**: Невозможно реверс-инженерить все форматы

## Правильный подход

**Универсальный парсер** извлекает геометрию через стандартные форматы хранения:

```
Прокси-объект (любой тип)
    ├── ACIS SAT данные → Тесселяция → Меш
    ├── Display Mesh →直接使用
    └── Proxy Graphics → 2D → 3D конвертация
```

### Преимущества
- ✅ **Один парсер** для всех типов объектов
- ✅ **Не зависит** от приложения-создателя (СПДС, Civil3D, и т.д.)
- ✅ **Расширяемость**: Новые типы работают автоматически
- ✅ **Простота**: Не нужно знать специфику каждого формата

## Структура данных

### ACIS SAT (если есть)
```
Заголовок:
  [0-3]:   "ACIS" сигнатура ($43495341)
  [4-7]:   Версия ACIS
  [8-11]:  Количество записей
  [12-15]: Размер заголовка
  [16-63]: BBox (6 double: MinX,Y,Z MaxX,Y,Z)

Тело:
  - Entities (face, edge, vertex, shell)
  - Топология (coedge, loop, use)
```

### Display Mesh (универсальный)
```
[0-3]:   Количество вершин (N)
[4+]:    Вершины: N * (Double X, Double Y, Double Z)
[...]:   Количество граней (M)
[...]:   Грани: M * (Int V1, Int V2, Int V3)
```

## Реализация

### TProxyObjectParser
```pascal
TProxyObjectParser = class
  function InitFromHex(hexData: string): Boolean;
  function Parse: TProxyParseResult;
  function GetMeshForDisplay: TProxyMeshData;
end;
```

### TProxyMeshData
```pascal
TProxyMeshData = class
  procedure AddVertex(X, Y, Z: Double);
  procedure AddTriangle(V1, V2, V3: Integer);
  procedure CalcBBox;
  
  property VertexCount: Integer;
  property FaceCount: Integer;
  property Vertices[Index: Integer]: TProxyVertex;
  property Faces[Index: Integer]: TProxyFace;
end;
```

## Интеграция

```pascal
procedure GDBObjAcdProxy.LoadFromDXF(...);
var
  parser: TProxyObjectParser;
begin
  parser := TProxyObjectParser.Create;
  try
    if parser.InitFromHex(hexData) then begin
      Result := parser.Parse;
      if Result.Valid then begin
        FBBoxMin := Result.BBoxMin;
        FBBoxMax := Result.BBoxMax;
        FMesh := parser.GetMeshForDisplay;
      end;
    end;
  finally
    parser.Free;
  end;
end;

procedure GDBObjAcdProxy.FormatEntity(...);
begin
  if FMesh <> nil then
    DrawMesh(DC, FMesh)  // Отрисовка геометрии
  else if FBBoxLoaded then
    DrawBBox(DC);        // Fallback: только BBox
end;
```

## Что нужно реализовать

### Приоритет 1: Базовая функциональность
- [x] TProxyObjectParser (структура)
- [x] TProxyMeshData (хранение меша)
- [ ] Парсинг Display Mesh (полная реализация)
- [ ] Интеграция с GDBObjAcdProxy
- [ ] Отрисовка меша

### Приоритет 2: ACIS поддержка
- [ ] ACIS SAT парсер (заголовок + bbox)
- [ ] Простая тесселяция (bounding box как mesh)
- [ ] Полная тесселяция ACIS → Mesh

### Приоритет 3: Оптимизация
- [ ] Кэширование распарсенного меша
- [ ] LOD (Level of Detail) для больших мешей
- [ ] Пакетная отрисовка

## Тестирование

### Критерии успеха
1. **Загрузка**: Файл загружается без ошибок
2. **BBox**: Габариты вычисляются корректно
3. **Отображение**: Геометрия видна (меш или BBox)
4. **Производительность**: Загрузка < 1 секунды для файла 1MB

### Тестовые файлы
- `testspdseasy.dxf` - СПДС маркер
- `acadpolyface.dxf` - Полигональная грань
- `3dpolyline.dxf` - 3D полилиния
- Любой DWG с proxy-объектами из Civil3D/Revit

## Итог

**Вместо**: "Парсер для СПДС" + "Парсер для Civil3D" + "Парсер для Architecture" + ...

**Используем**: "Один универсальный парсер ACIS/Display Mesh"

Это работает потому что:
- Все 3D прокси-объекты хранят геометрию в ACIS SAT или Display Mesh
- Все 2D прокси-объекты хранят Proxy Graphics
- Нам не нужно знать тип объекта - нужно извлечь геометрию
