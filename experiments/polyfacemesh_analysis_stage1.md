# Анализ Polyface Mesh для ZCAD - Этап 1

## 1. Выводы о представлении Polyface Mesh в DXF

### Ключевые характеристики:
- **Polyface Mesh** - это вариант **POLYLINE** с установленным флагом `64` в группе `70`
- Структура: последовательность VERTEX с координатами, затем VERTEX с определениями граней
- Флаг 70 = 64 + опционально 1 (closed) = 65 для закрытого polyface mesh
- Группа 71: количество вершин (опционально)
- Группа 72: количество граней (опционально)

### Формат вершин (Vertex entities):
1. **Координатные вершины** - обычные VERTEX с 3D координатами
2. **Face-вершины** - VERTEX с специальным форматом в группе 70:
   - Вершина грани имеет флаг 128 в группе 70
   - Координаты (X,Y,Z) содержат **индексы вершин**:
     - X = индекс 1-й вершины
     - Y = индекс 2-й вершины  
     - Z = индекс 3-й вершины
     - W (иногда используется) = индекс 4-й вершины

### Ограничения:
- Максимум **4 вершины на грань** (ограничение формата DXF)
- Сложные полигоны требуют триангуляции
- Индексы вершин начинаются с 1

## 2. Отличия от 3D Polyline

| Параметр | 3D Polyline | Polyface Mesh |
|----------|--------------|---------------|
| Флаг 70 | 8 (3D polyline) | 64 (polyface mesh) |
| Структура данных | Последовательность вершин | Вершины + грани (индексы) |
| Назначение | Линия/контур | Поверхность/многогранник |
| Вершины | Координатные | Координатные + индексные |
| Визуализация | Линии | Полигоны/грани |

## 3. Описание DXF структуры

### Пример структуры в DXF:
```
  0
POLYLINE
  8
MyLayer
 70
64        ; флаг polyface mesh
 71
5         ; 5 вершин (опционально)
 72
3         ; 3 грани (опционально)
  0
VERTEX
 10
1.0       ; X координата
 20
2.0       ; Y координата  
 30
0.0       ; Z координата
... еще 4 вершин ...
  0
VERTEX
 70
128       ; флаг вершины грани
 10
1.0       ; индекс вершины 1
 20
2.0       ; индекс вершины 2
 30
3.0       ; индекс вершины 3
... еще 2 грани ...
  0
SEQEND
```

## 4. Анализ текущей архитектуры ZCAD

### Иерархия классов:
```
GDBObjEntity (базовый)
├── GDBObj3d 
    ├── GDBObjCurve
    │   ├── GDBObjPolyline (существующий)
    │   └── GDBObjPolyFaceMesh (нужно создать)
    └── GDBObjLWPolyline
```

### Ключевые файлы:
- `uzeentpolyline.pas` - реализация 3D polyline
- `uzeentcurve.pas` - базовый класс для кривых
- `uzeentityfactory.pas` - фабрика и регистрация сущностей
- `uzeconsts.pas` - константы и ID сущностей
- `uzeffdxfsupport.pas` - DXF поддержка

### Регистрация сущностей:
```pascal
// В uzeconsts.pas добавить:
GDBPolyFaceMeshID = 17; // следующий доступный ID

// В регистрации:
RegisterDXFEntity(GDBPolyFaceMeshID, 'POLYLINE', 'PolyFaceMesh', 
                  @AllocPolyFaceMesh, @AllocAndInitPolyFaceMesh);
```

## 5. Требуемая структура данных

### GDBObjPolyFaceMesh:
```pascal
GDBObjPolyFaceMesh = object(GDBObjCurve)
  // Унаследовано из GDBObjCurve:
  // VertexArrayInOCS: GDBPoint3dArray - координатные вершины
  
  // Новые поля для Polyface:
  FaceArray: GDBFaceArray;           // массив граней с индексами
  FaceCount: integer;                 // количество граней
  IsPolyFaceMesh: boolean;            // флаг отличия от обычного polyline
  
  // Переопределенные методы:
  procedure LoadFromDXF(...); override;
  procedure SaveToDXF(...); override;
  procedure DrawGeometry(...); override;
  // ... другие методы
end;
```

### GDBFaceRecord:
```pascal
GDBFaceRecord = record
  VertexIndices: array[0..3] of integer; // до 4 индексов вершин
  VertexCount: integer;                  // фактическое количество вершин (3-4)
  EdgeFlags: array[0..3] of boolean;    // видимость рёбер
end;
```

## 6. Список функций/классов для реализации

### Основные классы:
1. **GDBObjPolyFaceMesh** - основной класс объекта
2. **GDBFaceRecord** - запись для одной грани
3. **GDBFaceArray** - контейнер для граней

### Ключевые методы GDBObjPolyFaceMesh:
1. `LoadFromDXF()` - парсинг DXF с определением типа VERTEX
2. `SaveToDXF()` - сохранение в формате DXF polyface mesh
3. `DrawGeometry()` - отрисовка полигонов вместо линий
4. `CalcTrueInFrustum()` - проверка видимости для полигонов
5. `Clone()` - копирование объекта
6. `GetObjType()` - возврат ID типа
7. `GetObjTypeName()` - возврат имени типа

### Вспомогательные функции:
1. `ParseFaceVertex()` - обработка vertex-записей граней
2. `IsFaceVertex()` - определение типа вершины
3. `TriangulateFace()` - триангуляция сложных полигонов
4. `BuildFaceArray()` - построение массива граней из индексов

## 7. Чеклист обязательных методов для GDBObjPolyFaceMesh

### Базовые методы (унаследованы из GDBObjEntity):
- [x] `init()` / `initnul()` - инициализация
- [ ] `GetObjType()` - возврат ID сущности  
- [ ] `GetObjTypeName()` - возврат имени сущности
- [ ] `Clone()` - клонирование объекта

### DXF методы:
- [ ] `LoadFromDXF()` - загрузка из DXF формата
- [ ] `SaveToDXF()` - сохранение в DXF формат

### Отрисовка и геометрия:
- [ ] `FormatEntity()` - форматирование для отрисовки
- [ ] `DrawGeometry()` - отрисовка геометрии
- [ ] `CalcTrueInFrustum()` - проверка видимости
- [ ] `getoutbound()` - вычисление границ

### Интерактивность:
- [ ] `onmouse()` - обработка мыши
- [ ] `onpoint()` - попадание точки
- [ ] `addcontrolpoints()` - контрольные точки
- [ ] `rtmodifyonepoint()` - модификация точек
- [ ] `remaponecontrolpoint()` - пересчет контрольных точек

### Snap и привязки:
- [ ] `startsnap()` / `endsnap()` - начало/конец привязок
- [ ] `getsnap()` - расчет точек привязки
- [ ] `AddOnTrackAxis()` - отслеживание осей

### Трансформации:
- [ ] `transform()` - трансформация объекта
- [ ] `TransformAt()` - трансформация в точке

### Статические методы:
- [ ] `CreateInstance()` - создание экземпляра
- [ ] `AllocPolyFaceMesh()` - аллокация памяти
- [ ] `AllocAndInitPolyFaceMesh()` - аллокация и инициализация

## Следующие шаги (Этап 2 реализации)

1. **Создать базовый файл uzeentpolyfacemesh.pas**
2. **Реализовать базовую структуру GDBObjPolyFaceMesh**
3. **Добавить регистрацию в системе сущностей**
4. **Реализовать базовые методы LoadFromDXF/SaveToDXF**
5. **Тестирование на простых DXF файлах**

---

*Документ подготовлен на основе анализа спецификаций Autodesk DXF и текущей архитектуры ZCAD*