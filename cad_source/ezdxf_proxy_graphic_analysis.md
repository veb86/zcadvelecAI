# Анализ реализации Proxy Graphic в ezdxf (Python)

## Обзор проекта ezdxf

**ezdxf** — это библиотека Python для работы с DXF файлами AutoCAD.

**Ключевые файлы для анализа:**
- `src/ezdxf/entities/acad_proxy_entity.py` — класс ACADProxyEntity
- `src/ezdxf/proxygraphic.py` — парсер Proxy Graphic (984 строки)
- `notes/pages/Proxy Graphic Binary Chunk.md` — документация формата

---

## Архитектура обработки прокси-объектов

### 1. ACADProxyEntity (acad_proxy_entity.py)

```python
@factory.register_entity
class ACADProxyEntity(DXFGraphic):
    """READ ONLY ACAD_PROXY_ENTITY CLASS! DO NOT MODIFY!"""
    
    DXFTYPE = "ACAD_PROXY_ENTITY"
    MIN_DXF_VERSION_FOR_EXPORT = const.DXF2000
```

**Ключевые особенности:**
- **READ ONLY** — класс только для чтения, нельзя модифицировать
- **Нельзя копировать** — `copy()` выбрасывает `CopyNotSupported`
- **Нельзя трансформировать** — internals неизвестны

### 2. Загрузка данных из DXF

```python
def load_dxf_attribs(self, processor: Optional[SubclassProcessor]) -> DXFNamespace:
    dxf = super().load_dxf_attribs(processor)
    if processor:
        self.acdb_proxy_entity = processor.subclass_by_index(2)
        self.load_proxy_graphic()  # ← Извлекаем proxy graphic
    return dxf
```

**Важно:** Proxy graphic хранится в `AcDbProxyEntity` (subclass 2), а не в `AcDbEntity` как обычно!

### 3. Извлечение бинарных данных

```python
def load_proxy_graphic(self) -> None:
    if self.acdb_proxy_entity is None:
        return
    
    # Пробуем разные коды длины (92 для R2010-, 160 для R2010+)
    for length_code in (92, 160):
        proxy_graphic = load_proxy_data(
            self.acdb_proxy_entity, 
            length_code, 
            310  # код бинарных данных
        )
        if proxy_graphic:
            self.proxy_graphic = proxy_graphic
            return
```

**Формат хранения в DXF:**
```
92: 944          ← размер данных (для R2010-)
310: <hex-данные> ← бинарные данные (чанки по 127 байт)
310: <hex-данные>
310: <hex-данные>
...
```

Для R2010+:
```
160: 944         ← размер данных (для R2010+)
310: <hex-данные>
```

---

## Формат Proxy Graphic Binary Chunk

### Общая структура

```
{Header}[{Command} [{Command} [{Command} [...]]]]
```

### Заголовок (8 байт)

| Байт | Тип | Описание |
|------|-----|----------|
| 0-3 | int32 | Длина бинарного чанка |
| 4-7 | int32 | Количество команд |

**Пример из ezdxf:**
```python
class ProxyGraphic:
    def __init__(self, data: bytes, ...):
        self._buffer: bytes = data
        self._index: int = 8  # ← Пропускаем 8-байтный заголовок
```

### Структура команды

```
{Command Packet Length} {Command OPCODE} [{Command Argument} ...]
```

| Смещение | Тип | Описание |
|----------|-----|----------|
| 0-3 | int32 | Длина пакета команды |
| 4-7 | int32 | OPCODE (тип команды) |
| 8+ | ... | Аргументы команды |

---

## Таблица OPCODE (ProxyGraphicTypes)

ezdxf определяет следующие типы команд:

| Код | Имя | Описание | Аргументы |
|-----|-----|----------|-----------|
| 1 | EXTENTS | Границы объекта | Min Point3d, Max Point3d |
| 2 | CIRCLE | Круг | Center Point3d, Radius double, Normal Point3d |
| 3 | CIRCLE_3P | Круг по 3 точкам | Point1, Point2, Point3 |
| 4 | CIRCULAR_ARC | Дуга | Center, Radius, Normal, StartVector, SweepAngle, ArcType |
| 5 | CIRCULAR_ARC_3P | Дуга по 3 точкам | Point1, Point2, Point3, ArcType |
| 6 | POLYLINE | Полилиния | Count int32, Vertex[] Point3d |
| 7 | POLYGON | Полигон | Count int32, Vertex[] Point3d |
| 8 | MESH | Меш | Rows, Columns, Vertex[], EdgeData, FaceData, VertexData |
| 9 | SHELL | Оболочка | VertexCount, Vertex[], FaceCount, FaceList[], ... |
| 10 | TEXT | Текст | Position, Normal, Direction, Height, Width, Oblique, Text string |
| 11 | TEXT2 | Текст (расширенный) | Position, Normal, Direction, Text, Length, Raw, TextStyle |
| 12 | XLINE | Конструкционная линия | Point1, Point2 |
| 13 | RAY | Луч | StartPoint, ThroughPoint |
| 14 | ATTRIBUTE_COLOR | Установить цвет | ColorIndex uint32 |
| 15 | UNUSED_15 | Не используется | — |
| 16 | ATTRIBUTE_LAYER | Установить слой | LayerID uint32 |
| 17 | UNUSED_17 | Не используется | — |
| 18 | ATTRIBUTE_LINETYPE | Установить тип линии | LinetypeID uint32 |
| 19 | ATTRIBUTE_MARKER | Маркер выбора | MarkerID uint32 |
| 20 | ATTRIBUTE_FILL | Заполнение | FillType uint32 |
| 21 | UNUSED_21 | Не используется | — |
| 22 | ATTRIBUTE_TRUE_COLOR | True цвет | TrueColor uint32 |
| 23 | ATTRIBUTE_LINEWEIGHT | Вес линии | LineWeight uint32 |
| 24 | ATTRIBUTE_LTSCALE | Масштаб типа линии | Ltscale double |
| 25 | ATTRIBUTE_THICKNESS | Толщина | Thickness double |
| 26 | ATTRIBUTE_PLOT_STYLE_NAME | Стиль печати | PlotStyleType, ID |
| 27 | PUSH_CLIP | Начать клипирование | ClipBoundary struct |
| 28 | POP_CLIP | Закончить клипирование | — |
| 29 | PUSH_MATRIX | Начать трансформацию | Matrix 16×double |
| 30 | PUSH_MATRIX2 | Начать трансформацию (v2) | Matrix 16×double |
| 31 | POP_MATRIX | Закончить трансформацию | — |
| 32 | POLYLINE_WITH_NORMALS | Полилиния с нормалями | Count, Vertex[], Normal |
| 33 | LWPOLYLINE | 2D полилиния | См. ниже |
| 34 | ATTRIBUTE_MATERIAL | Материал | MaterialID |
| 35 | ATTRIBUTE_MAPPER | Mapper | MapperData |
| 36 | UNICODE_TEXT | Текст Unicode | Position, Normal, Direction, Height, Width, Oblique, Text (UTF-16) |
| 37 | UNKNOWN_37 | Неизвестно | — |
| 38 | UNICODE_TEXT2 | Текст Unicode (расширенный) | Как TEXT2 но UTF-16 |
| 44 | ELLIPTIC_ARC | Эллиптическая дуга | Center, Extrusion, MajorLength, MinorLength, StartParam, EndParam, Angle |

---

## Детальная реализация парсера

### 1. Инициализация ProxyGraphic

```python
class ProxyGraphic:
    def __init__(self, data: bytes, doc: Optional[Drawing] = None):
        self._buffer: bytes = data
        self._index: int = 8  # Пропускаем заголовок
        
        # Состояние (атрибуты по умолчанию)
        self.color: int = const.BYLAYER
        self.layer: str = "0"
        self.linetype: str = "BYLAYER"
        self.fill: bool = False
        self.true_color: Optional[int] = None
        self.lineweight: int = const.LINEWEIGHT_DEFAULT
        self.ltscale: float = 1.0
        self.thickness: float = 0.0
        
        # Словари для ссылок
        self.layers: list[str] = [...]  # Из документа
        self.linetypes: list[str] = [...]  # Из документа
        self.textstyles: dict[str, str] = {...}  # font → style name
        
        # Стек трансформаций
        self.matrices: list[Matrix44] = []
```

### 2. Парсинг команд

```python
def unsafe_virtual_entities(self) -> Iterable[DXFGraphic]:
    index = self._index
    buffer = self._buffer
    
    while index < len(buffer):
        # Читаем заголовок команды
        size, type_ = struct.unpack_from("<2L", buffer, offset=index)
        
        if size < 8:
            raise ValueError("chunk size < 8 bytes")
        
        # Получаем имя команды
        try:
            name = ProxyGraphicTypes(type_).name.lower()
        except ValueError:
            logger.debug(f"Unsupported Type Code: {type_}")
            index += size
            continue
        
        # Вызываем метод парсинга
        method = getattr(self, name, None)
        if method:
            result = method(buffer[index + 8 : index + size])
            if result:
                yield transform(result)  # Применяем матрицу
        
        index += size
```

### 3. Пример: Парсинг круга (OPCODE=2)

```python
def circle(self, data: bytes):
    bs = ByteStream(data)
    attribs = self._build_dxf_attribs()
    
    # Читаем данные
    center = Vec3(bs.read_vertex())      # 3 doubles (24 байта)
    radius = bs.read_float()             # 1 float (4 байта)
    normal = Vec3(bs.read_vertex())      # 3 doubles (24 байта)
    
    # Обработка OCS (Object Coordinate System)
    if not normal.isclose(Z_AXIS):
        ocs = OCS(normal)
        center = ocs.from_wcs(center)
    
    attribs["center"] = center
    attribs["radius"] = radius
    attribs["extrusion"] = normal
    
    return self._factory("CIRCLE", dxfattribs=attribs)
```

### 4. Пример: Парсинг текста (OPCODE=10)

```python
def text(self, data: bytes):
    bs = ByteStream(data)
    
    # Читаем геометрию
    start_point = Vec3(bs.read_vertex())
    normal = Vec3(bs.read_vertex())
    text_direction = Vec3(bs.read_vertex())
    height, width_factor, oblique_angle = bs.read_struct("<3d")
    
    # Читаем текст (кодировка зависит от версии DXF)
    text = ""
    try:
        text = bs.read_padded_string(self.encoding)
    except UnicodeDecodeError as e:
        logger.debug(f"ProxyGraphic._text; {str(e)}")
    
    attribs = self._build_dxf_attribs()
    attribs["insert"] = start_point
    attribs["text"] = text
    attribs["height"] = height
    attribs["rotation"] = text_direction.angle_deg
    attribs["oblique"] = math.degrees(oblique_angle)
    attribs["extrusion"] = normal
    
    return self._factory("TEXT", dxfattribs=attribs)
```

### 5. Пример: Атрибуты (состояние)

```python
def attribute_color(self, data: bytes):
    self.reset_colors()
    self.color = struct.unpack("<L", data)[0]
    if self.color < 0 or self.color > 256:
        self.color = const.BYLAYER

def attribute_layer(self, data: bytes):
    if self._doc:
        index = struct.unpack("<L", data)[0]
        if index < len(self.layers):
            self.layer = self.layers[index]

def attribute_linetype(self, data: bytes):
    if self._doc:
        index = struct.unpack("<L", data)[0]
        try:
            # Первые 2 записи (ByLayer, ByBlock) не включены
            self.linetype = self.linetypes[index + 2]
        except IndexError:
            if index == 32766:
                self.linetype = "BYBLOCK"
            else:
                self.linetype = "BYLAYER"
```

### 6. Пример: Трансформации

```python
def push_matrix(self, data: bytes):
    # Читаем матрицу 4×4 (16 doubles)
    values = struct.unpack("<16d", data)
    m = Matrix44(values)
    m.transpose()  # Транспонируем для правильного порядка
    self.matrices.append(m)

def pop_matrix(self, data: bytes):
    if self.matrices:
        self.matrices.pop()
```

---

## Сравнение с реализацией ZCAD

### ezdxf vs uzeentacdproxy.pas

| Характеристика | ezdxf (Python) | ZCAD (Pascal) |
|---------------|----------------|---------------|
| **Подход** | Командный (OPCODE) | Универсальный парсер |
| **Идентификация типа** | OPCODE (2=CIRCLE, 10=TEXT) | Версия формата (24=SPDSPOLYMORPHMARK) |
| **Хранение геометрии** | Команды AcGiWorldDraw | Бинарная структура |
| **Текстовые параметры** | Нет (только geometry) | Есть (коды 300/301/40) |
| **Поддержка СПДС** | Нет | Да (SPDSPOLYMORPHMARK) |
| **Размер кода** | ~1000 строк | ~400 строк |

### Ключевые отличия

1. **ezdxf использует AcGiWorldDraw команды**
   - Proxy graphic — это последовательность команд отрисовки
   - Каждая команда имеет OPCODE и аргументы
   - ezdxf парсит эти команды и создаёт DXF примитивы

2. **ZCAD использует универсальный парсер**
   - Извлекает ACIS SAT или Display Mesh
   - Не зависит от типа объекта
   - Работает на более низком уровне

3. **СПДС GraphiCS использует другой формат**
   - Бинарные данные содержат геометрию + метаданные
   - Текстовые параметры (Shape, Text, Size) хранятся отдельно (коды 300/301/40)
   - Версия формата (24) определяет тип объекта

---

## Структуры данных ezdxf

### ByteStream (вспомогательный класс)

```python
class ByteStream:
    def __init__(self, data: bytes):
        self.data = data
        self.index = 0
    
    def read_vertex(self) -> Vec3:
        """Читает 3 doubles (24 байта)"""
        x, y, z = struct.unpack_from("<3d", self.data, self.index)
        self.index += 24
        return Vec3(x, y, z)
    
    def read_float(self) -> float:
        """Читает float (4 байта)"""
        value = struct.unpack_from("<f", self.data, self.index)[0]
        self.index += 4
        return value
    
    def read_long(self) -> int:
        """Читает int32 (4 байта)"""
        value = struct.unpack_from("<L", self.data, self.index)[0]
        self.index += 4
        return value
    
    def read_struct(self, format: str):
        """Читает структуру по формату"""
        size = struct.calcsize(format)
        value = struct.unpack_from(format, self.data, self.index)
        self.index += size
        return value
```

### BitStream (для LWPOLYLINE)

```python
class BitStream:
    """Чтение битовых полей из байтового потока"""
    
    def read_bit(self) -> bool: ...
    def read_bit_short(self) -> int: ...  # 2 байта
    def read_bit_long(self) -> int: ...   # 4 байта
    def read_bit_double(self) -> float: ...  # 8 байт
    def read_bit_double_default(self, default: float) -> float: ...
```

---

## LWPOLYLINE парсинг (сложный случай)

```python
def lwpolyline(self, data: bytes):
    # OpenDesign Specs LWPLINE: 20.4.85 Page 211
    attribs = self._build_dxf_attribs()
    bs = BitStream(data)
    
    # Читаем заголовок
    num_data_bytes = bs.read_unsigned_long()
    flag = bs.read_bit_short()
    
    # Опциональные поля (зависят от флагов)
    if flag & 4:
        attribs["const_width"] = bs.read_bit_double()
    if flag & 8:
        attribs["elevation"] = bs.read_bit_double()
    if flag & 2:
        attribs["thickness"] = bs.read_bit_double()
    if flag & 1:
        attribs["extrusion"] = Vec3(bs.read_bit_double(3))
    if flag & 512:
        is_closed = True
    
    num_points = bs.read_bit_long()
    
    # Bulge (для дуг)
    if flag & 16:
        num_bulges = bs.read_bit_long()
    
    # Читаем вершины
    vertices = [bs.read_raw_double(2)]
    for _ in range(num_points - 1):
        x = bs.read_bit_double_default(default=prev_point[0])
        y = bs.read_bit_double_default(default=prev_point[1])
        vertices.append((x, y))
    
    # Читаем bulge, widths, vertex IDs
    bulges = [bs.read_bit_double() for _ in range(num_bulges)]
    ...
    
    return self._factory("LWPOLYLINE", dxfattribs=attribs)
```

---

## MESH и SHELL парсинг

### MESH структура

```python
def mesh(self, data: bytes):
    bs = ByteStream(data)
    
    # Размеры сетки
    rows, columns = bs.read_struct("<2L")
    
    # Вычисляем количество элементов
    total_vertex_count = rows * columns
    total_edge_count = (rows - 1) * columns + (columns - 1) * rows
    total_face_count = (rows - 1) * (columns - 1)
    
    # Читаем вершины
    vertices = [Vec3(bs.read_vertex()) for _ in range(total_vertex_count)]
    
    # Читаем traits (дополнительные данные)
    traits = read_mesh_traits(
        bs, 
        total_edge_count, 
        total_face_count, 
        vertex_count=0
    )
    
    # Создаём PolyMesh entity
    attribs = self._build_dxf_attribs()
    attribs["m_count"] = rows
    attribs["n_count"] = columns
    attribs["flags"] = const.POLYLINE_3D_POLYMESH
    
    polymesh = self._factory("POLYLINE", dxfattribs=attribs)
    polymesh.append_vertices(vertices)
    return polymesh
```

### read_mesh_traits

```python
def read_mesh_traits(bs, edge_count, face_count, vertex_count):
    traits = dict()
    
    # Edge traits
    edge_flags = bs.read_long()
    if has_prim_traits(edge_flags):
        traits["edges"] = read_prim_traits(
            bs,
            ["colors", "layers", "linetypes", "markers", "visibilities"],
            edge_flags,
            edge_count
        )
    
    # Face traits
    face_flags = bs.read_long()
    if has_prim_traits(face_flags):
        traits["faces"] = read_prim_traits(
            bs,
            ["colors", "layers", "markers", "normals", "visibilities"],
            face_flags,
            face_count
        )
    
    # Vertex traits
    if vertex_count > 0:
        vertex_flags = bs.read_long()
        if has_prim_traits(vertex_flags):
            vertices = dict()
            if prims_have_normals(vertex_flags):
                vertices["normals"] = [Vec3(bs.read_vertex()) for _ in range(vertex_count)]
            if prims_have_orientation(vertex_flags):
                vertices["orientation"] = bool(bs.read_long())
            traits["vertices"] = vertices
    
    return traits
```

---

## Выводы для ZCAD

### Что можно заимствовать из ezdxf

1. **Структура парсера команд**
   - OPCODE → метод парсинга
   - Чёткое разделение геометрии и атрибутов
   - Стек трансформаций (матрицы)

2. **Обработка OCS (Object Coordinate System)**
   ```python
   if not normal.isclose(Z_AXIS):
       ocs = OCS(normal)
       center = ocs.from_wcs(center)
   ```

3. **Атрибуты по умолчанию**
   - color = BYLAYER
   - layer = "0"
   - linetype = BYLAYER

4. **Поддержка разных версий DXF**
   - Кодировка: cp1252 для R2000-, UTF-8 для R2007+
   - Коды длины: 92 для R2010-, 160 для R2010+

### Что НЕ подходит для ZCAD

1. **AcGiWorldDraw формат** — это для AutoCAD proxy graphic
   - СПДС GraphiCS использует другой формат
   - Версия формата (24) вместо OPCODE

2. **Сложные traits для MESH** — в СПДС простые объекты

3. **BitStream для LWPOLYLINE** — в СПДС нет LWPOLYLINE в proxy

---

## Рекомендации для ZCAD

### 1. Добавить поддержку командного формата

Если СПДС начнёт использовать AcGiWorldDraw формат:

```pascal
type
  TProxyGraphicCommand = (
    pgcExtents = 1,
    pgcCircle = 2,
    pgcCircle3P = 3,
    pgcArc = 4,
    pgcArc3P = 5,
    pgcPolyline = 6,
    pgcPolygon = 7,
    pgcText = 10,
    pgcText2 = 11,
    pgcSetColor = 14,
    pgcSetLayer = 16,
    pgcSetLinetype = 18,
    pgcPushMatrix = 29,
    pgcPopMatrix = 31
  );

procedure TProxyGraphicParser.ParseCommand;
var
  Size, OpCode: Integer;
begin
  Size := ReadInt32;
  OpCode := ReadInt32;
  
  case TProxyGraphicCommand(OpCode) of
    pgcCircle: ParseCircle;
    pgcPolygon: ParsePolygon;
    pgcText: ParseText;
    pgcSetColor: ParseSetColor;
    pgcPushMatrix: ParsePushMatrix;
    pgcPopMatrix: ParsePopMatrix;
  end;
end;
```

### 2. Использовать ByteStream из ezdxf как образец

```pascal
type
  TByteStream = class
  private
    FData: TBytes;
    FIndex: Integer;
  public
    function ReadVertex: TPoint3D;  // 3 doubles
    function ReadFloat: Double;      // 1 float
    function ReadInt32: Integer;     // 4 bytes
    function ReadString(Encoding: TEncoding): string;
  end;
```

### 3. Добавить поддержку OCS

```pascal
function TransformToOCS(const Point: TPoint3D; const Normal: TPoint3D): TPoint3D;
var
  OCS: TOCS;
begin
  if not IsClose(Normal, Z_AXIS) then
  begin
    OCS := TOCS.Create(Normal);
    Result := OCS.FromWCS(Point);
  end
  else
    Result := Point;
end;
```

---

## Приложения

### A. Ссылки

- [ezdxf GitHub](https://github.com/ezdxf/ezdxf)
- [Proxy Graphic Binary Chunk](https://adndevblog.typepad.com/autocad/2013/02/proxy-graphic-in-dxf-binary-chunk-interpretation.html)
- [ODA DWG Docs](https://www.opendesign.com/guestfiles/oda_file_format)

### B. Файлы для изучения

- `src/ezdxf/proxygraphic.py` — основной парсер
- `src/ezdxf/entities/acad_proxy_entity.py` — класс прокси
- `notes/pages/Proxy Graphic Binary Chunk.md` — документация OPCODE

### C. Тестовые файлы

- `examples_dxf/proxy_entities.dxf` — примеры прокси-объектов
- `integration_tests/test_proxy_graphic.py` — тесты парсера

---

*Документ создан на основе анализа ezdxf-master (Python)*
