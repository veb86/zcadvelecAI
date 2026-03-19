# Универсальный парсер прокси-объектов DXF

## Проблема
Прокси-объекты создаются разными приложениями (Civil 3D, СПДС GraphiCS, Architectural Desktop, и т.д.). Каждый тип объекта имеет свою структуру данных. Реинжениринг каждого формата индивидуально - неэффективен.

## Решение
**Универсальный парсер**, который извлекает геометрию из ЛЮБОГО прокси-объекта через стандартные форматы:
1. **ACIS SAT** - точная B-rep геометрия
2. **Display Mesh** - тесселированная сетка
3. **Proxy Graphics** - 2D представление

## Архитектура

```
uzeentproxyparser.pas
├── TProxyObjectParser      - Универсальный парсер
├── TProxyMeshData          - Данные полигонального меша
└── TProxyParseResult       - Результат парсинга
```

## Алгоритм работы

### 1. Чтение заголовка
```
[0-3]:   Размер данных
[4-7]:   Версия
[8-11]:  Сигнатура ("ACIS" или другая)
[12-15]: Смещение до геометрии
```

### 2. Определение типа геометрии
- Если сигнатура "ACIS" ($43495341) → парсим ACIS SAT
- Иначе → пытаемся парсим как Display Mesh

### 3. Парсинг ACIS SAT
```pascal
function ParseACISData(offset: Integer): Boolean;
begin
  { ACIS SAT структура:
    - Заголовок (версия, bbox)
    - Список entities (face, edge, vertex)
    - Топология (coedge, loop, shell) }
  
  { Извлекаем BBox из заголовка }
  BBoxMin := ReadDouble(offset + 16);
  BBoxMax := ReadDouble(offset + 40);
  
  { Сохраняем сырые данные для последующей тесселяции }
  SATData := Copy(Buffer, offset, Length - offset);
end;
```

### 4. Парсинг Display Mesh
```pascal
function ParseDisplayMesh(offset: Integer): Boolean;
begin
  { Предполагаемая структура:
    [0-3]:   Количество вершин
    [4+]:    Версии (N * 24 байт: X, Y, Z - double)
    [...]:   Количество граней
    [...]:   Грани (M * 12 байт: 3 индекса) }
  
  VertexCount := ReadInteger(offset);
  for i := 0 to VertexCount-1 do
    AddVertex(ReadDouble, ReadDouble, ReadDouble);
  
  FaceCount := ReadInteger;
  for i := 0 to FaceCount-1 do
    AddTriangle(ReadInteger, ReadInteger, ReadInteger);
end;
```

### 5. Тесселяция ACIS (TODO)
```pascal
procedure TessellateACIS;
begin
  { Конвертация ACIS B-rep в полигональный меш
    Алгоритм:
    1. Парсинг ACIS entities
    2. Для каждой FACE:
       - Если平面 → триангуляция полигона
       - Если кривая → тесселяция поверхности
    3. Сборка меша из треугольников }
end;
```

## Интеграция с ZCAD

### Загрузка из DXF
```pascal
procedure GDBObjAcdProxy.LoadFromDXF(...);
var
  parser: TProxyObjectParser;
  parseResult: TProxyParseResult;
begin
  { Читаем бинарные данные из кода 310 }
  hexData := rdr.ParseString;
  
  { Парсим универсальным парсером }
  parser := TProxyObjectParser.Create;
  try
    if parser.InitFromHex(hexData) then begin
      parseResult := parser.Parse;
      
      if parseResult.Valid then begin
        { Сохраняем BBox }
        FBBoxMinInOCS := parseResult.BBoxMin;
        FBBoxMaxInOCS := parseResult.BBoxMax;
        FBBoxLoaded := True;
        
        { Сохраняем меш для отрисовки }
        FMeshData := parser.GetMeshForDisplay;
      end;
    end;
  finally
    parser.Free;
  end;
end;
```

### Отрисовка
```pascal
procedure GDBObjAcdProxy.FormatEntity(...);
var
  i: Integer;
  v1, v2, v3: TzePoint3d;
  face: TProxyFace;
begin
  if FMeshData <> nil then begin
    { Рисуем треугольники меша }
    for i := 0 to FMeshData.FaceCount - 1 do begin
      face := FMeshData.Faces[i];
      v1 := Point3D(FMeshData.Vertices[face.VertexIndices[0]].X, ...);
      v2 := Point3D(FMeshData.Vertices[face.VertexIndices[1]].X, ...);
      v3 := Point3D(FMeshData.Vertices[face.VertexIndices[2]].X, ...);
      
      DrawTriangle(DC, v1, v2, v3);
    end;
  end else if FBBoxLoaded then begin
    { Fallback: рисуем BBox }
    DrawBBox(DC, FBBoxMinInOCS, FBBoxMaxInOCS);
  end;
end;
```

## Поддерживаемые типы объектов

### ACIS-based объекты
- [x] 3D Solids (Body)
- [ ] Regions
- [ ] Faces (ACIS face)

### Mesh-based объекты
- [x] Display mesh (любой прокси с сеткой)
- [ ] AEC objects (walls, doors, windows)
- [ ] Civil 3D objects (surfaces, alignments)

### Proxy Graphics
- [ ] 2D proxy graphics
- [ ] СПДС объекты (планы, фасады)

## План развития

### Этап 1: Базовая поддержка
- [x] Универсальная структура парсера
- [x] Определение типа геометрии
- [ ] Парсинг ACIS SAT (заголовок, bbox)
- [x] Парсинг Display Mesh
- [ ] Тесселяция ACIS → Mesh

### Этап 2: Отрисовка
- [ ] Интеграция с `GDBObjAcdProxy`
- [ ] Отрисовка меша через `FormatEntity`
- [ ] Оптимизация (кэширование, LOD)

### Этап 3: Расширение
- [ ] Полноценный ACIS SAT парсер
- [ ] Поддержка AEC объектов
- [ ] Конвертация в нативные объекты ZCAD

## Тестирование

### Файлы для тестирования
1. `testspdseasy.dxf` - СПДС маркер (круг + текст)
2. `acadpolyface.dxf` - ACAD_PROXY_ENTITY (полигрань)
3. `3dpolyline.dxf` - 3D полилиния

### Критерии успеха
- Загрузка файла без ошибок
- Отображение геометрии (меш или BBox)
- Корректный BBox для view extents

## Преимущества подхода

1. **Универсальность**: Один парсер для всех типов прокси
2. **Масштабируемость**: Легко добавить поддержку новых форматов
3. **Независимость**: Не требует знания конкретного типа объекта
4. **Производительность**: Display Mesh парсится быстро

## Ограничения

1. **Точность**: Display Mesh менее точен, чем ACIS B-rep
2. **Тесселяция**: Требует реализации ACIS → Mesh конвертера
3. **Сложные объекты**: AEC объекты могут требовать специальной обработки

## Ссылки

- [ACIS SAT Format Specification](https://paulbourke.net/dataformats/sat/)
- [DXF Proxy Entity Reference](https://help.autodesk.com/cloudhelp/2020/ENU/AutoCAD-DXF/files/GUID-89A690F9-E859-4D57-89EA-750F3FB76C6B.htm)
- [ObjectARX Proxy Entity Class](https://help.autodesk.com/view/OARX/2020/ENU/?guid=OARX-AcDbProxyEntity)
