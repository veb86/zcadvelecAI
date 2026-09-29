# Техническое задание (ТЗ) на внедрение поддержки растровых изображений (IMAGE / IMAGEDEF) в САПР ZCAD

* **Проект:** САПР ZCAD ([https://github.com/veb86/zcadvelecAI](https://github.com/veb86/zcadvelecAI))
* **Язык разработки:** Free Pascal / Lazarus (Object Pascal)
* **Цель:** Обеспечение полноценного чтения, отображения (OpenGL / Canvas) и записи (экспорта) растровых изображений (внешних ссылок на картинки — `IMAGE`, `IMAGEDEF`, `ACAD_IMAGE_DICT`) в формате DXF с гарантией 100% совместимости с Autodesk AutoCAD.

---

## 1. Анализ текущего эталонного решения: В каких файлах реализована работа с растрами

В нашем эталонном проекте чтение, анализ и отображение растров распределены по модулям в соответствии с принципами чистой архитектуры:

| Файл проекта | Язык / Технология | Роль и выполняемые функции |
|---|---|---|
| **`/src/services/dxfParser.ts`** | TypeScript | **Ядро чтения DXF**: <br>1. Потоковый токенизатор пар тегов (`parseDXFTags`).<br>2. Парсинг объектов `IMAGEDEF` и `RASTERVARIABLES` в секции `OBJECTS`.<br>3. Парсинг примитива `IMAGE` (`parseImageEntity`) в секции `ENTITIES` (извлечение точки вставки, векторов $U$ и $V$, пиксельного размера, флагов, контура подрезки).<br>4. **Отложенное связывание (Deferred Linking)**: пост-процессинг связи `IMAGE -> IMAGEDEF` по коду 340.<br>5. Расчет габаритов и учет растров в общих границах чертежа (`extents`). |
| **`/src/types/dxf.ts`** | TypeScript | **Модель данных**: Описание интерфейсов `DXFImageEntity`, `DXFImageDefInfo`, `DXFExternalReference` и включение растра в `DXFRenderableEntity`. |
| **`/src/components/CADViewer.tsx`** | React / Canvas 2D | **Модуль визуализации (аналог рендера ZCAD)**: <br>1. Кэширование растровых текстур (`imageCache`).<br>2. Аффинная трансформация из мировой системы координат (WCS) в экранную с учетом масштаба шага пикселей и угла поворота.<br>3. Отрисовка рамки растра (`IMAGEFRAME`), маркерных узлов (grips) и текстовых бейджей.<br>4. Применение параметров яркости, контрастности и слияния (`filter: brightness/contrast/opacity`).<br>5. Интерактивный выбор и кликабельность растра на чертеже. |
| **`/src/components/ExternalReferenceManager.tsx`** | React | **Диспетчер внешних ссылок (XREF Palette)**: отображение статуса ссылок, абсолютных/относительных путей, подгрузка и замена файла на диске. |
| **`/table_reader.py`** | Python 3 | **Глубокий консольный инспектор**: процедура `analyze_images_and_xrefs_in_dxf` и отчет `print_xrefs_report` для диагностики корректности DXF структуры и флагов. |
| **`/dxf_parser.py`** | Python 3 | **Низкоуровневый токенизатор DXF**: методы `extract_entities_by_type("IMAGE")` и `extract_objects_by_type("IMAGEDEF")`. |
| **`/dxf_saver.py`** | Python 3 | **Ядро генерации и инъекции DXF**: управление дескрипторами (Handles), внедрение классов `CLASSES`, системных словарей `DICTIONARY` и объектов `OBJECTS`. |

---

## 2. Как устроена запись (сохранение / экспорт) растровых изображений в DXF

Для того чтобы созданный или экспортированный ZCAD DXF-файл открывался в AutoCAD **без предупреждений о прокси-объектах и без потери ссылок**, запись растрового изображения должна строго формировать 4 взаимосвязанных узла в разных секциях файла:

```
                  ┌──────────────────────────────────────────────┐
                  │               СЕКЦИЯ CLASSES                 │
                  │  CLASS: IMAGEDEF (AcDbRasterImageDef)        │
                  │  CLASS: IMAGE    (AcDbRasterImage)           │
                  └──────────────────────────────────────────────┘
                                          │
                  ┌──────────────────────────────────────────────┐
                  │              СЕКЦИЯ ENTITIES                 │
                  │  IMAGE (Handle: 0xB88)                       │
                  │  - Точка вставки (Group 10, 20, 30)          │
                  │  - Вектор шага U (Group 11, 21, 31)          │
                  │  - Вектор шага V (Group 12, 22, 32)          │
                  │  - Размер в px   (Group 13, 23)              │
                  │  - Указатель 340 ───┐                        │
                  │  - Указатель 360 ───┼──────────┐             │
                  └─────────────────────┼──────────┼─────────────┘
                                        │          │
                  ┌─────────────────────┼──────────┼─────────────┐
                  │              СЕКЦИЯ OBJECTS    │             │
                  │                     │          │             │
                  │  DICTIONARY (NOD, Handle: C)   │             │
                  │    - Key "ACAD_IMAGE_DICT" ─> 0xB84          │
                  │    - Key "ACAD_IMAGE_VARS" ─> 0xB85          │
                  │                                │             │
                  │  DICTIONARY (ACAD_IMAGE_DICT, Handle: 0xB84) │
                  │    - Key "testimage" ─> 0xB86  │             │
                  │                                │             │
                  │  IMAGEDEF (Handle: 0xB86) <────┘             │
                  │    - Путь к файлу (Group 1: .\testimage.png) │
                  │    - Размер px    (Group 10, 20: 248, 72)    │
                  │    - Шаг пикселя  (Group 11, 21: 0.2646)     │
                  │    - Статус       (Group 280: 1)             │
                  │                                              │
                  │  IMAGEDEF_REACTOR (Handle: 0xB87) <──────────┘
                  │    - Владелец/Ссылка (Group 330: 0xB88)      │
                  │                                              │
                  │  RASTERVARIABLES (Handle: 0xB85)             │
                  │    - IMAGEFRAME (Group 70: 1)                │
                  └──────────────────────────────────────────────┘
```

### Пошаговый регламент записи примитива при экспорте в DXF:

1. **В секции `CLASSES`**:
   * Обязательно объявить класс `IMAGEDEF` (`AcDbRasterImageDef`, библиотека `ISM`, флаги `90=0, 91=1, 280=0, 281=0`).
   * Объявить класс `IMAGE` (`AcDbRasterImage`, библиотека `ISM`, флаги `90=2175, 91=1, 280=0, 281=1`).

2. **В секции `ENTITIES`**:
   * Записать примитив `IMAGE`:
     ```dxf
       0
     IMAGE
       5
     <Image_Handle>
     330
     <Model_Space_Handle>
     100
     AcDbEntity
       8
     <LayerName>
     100
     AcDbRasterImage
      90
             0
      10
     <Insert_X>
      20
     <Insert_Y>
      30
     <Insert_Z>
      11
     <U_Step_X>
      21
     <U_Step_Y>
      31
     0.0
      12
     <V_Step_X>
      22
     <V_Step_Y>
      32
     0.0
      13
     <Pixel_Width>
      23
     <Pixel_Height>
     340
     <ImageDef_Handle>
      70
          7
     280
          0
     281
         50
     282
         50
     283
          0
     360
     <ImageDef_Reactor_Handle>
      71
          1
      91
          2
      14
     -0.5
      24
     -0.5
      14
     <Pixel_Width - 0.5>
      24
     <Pixel_Height - 0.5>
     ```

3. **В секции `OBJECTS`**:
   * В корневой словарь **NOD (дескриптор `C`)** добавить записи:
     * `3: ACAD_IMAGE_DICT` -> `350: <ImageDict_Handle>`
     * `3: ACAD_IMAGE_VARS` -> `350: <RasterVars_Handle>`
   * Записать подсловарь **`ACAD_IMAGE_DICT`**:
     * `3: <ImageName_Without_Ext>` -> `350: <ImageDef_Handle>`
   * Записать объект **`IMAGEDEF`**:
     * `1: <FilePath>` (например, относительный путь `.\testimage.png`)
     * `10, 20: <PixelWidth>, <PixelHeight>`
     * `11, 21: <PixelSizeInUnits>`
     * `280: 1` (активен/загружен)
     * `281: 2` (единицы: 1 = мм, 2 = см)
   * Записать объект **`IMAGEDEF_REACTOR`**:
     * `330: <Image_Handle>`
     * `100: AcDbRasterImageDefReactor`
     * `90: 2`
     * `330: <Image_Handle>`
   * Записать объект **`RASTERVARIABLES`**:
     * `70: 1` (`IMAGEFRAME` включен)
     * `71: 1` (высокое качество)
     * `72: 5` (единицы)

---

## 3. Архитектурное решение для ZCAD (Lazarus / Free Pascal)

Изучить кодовую базу ZCAD и внедрить путем расширения объектной иерархии графических примитивов и модуля экспорта/импорта DXF на базе NOD.

### 3.1. Создание нового модуля примитива: `uzeentimage.pas`

В архитектуре ZCAD каждый примитив наследуется от базового графического сущностного класса `uzeEntity`.

Данный код носит Только информативный характер и не требует строгого соответствия. Сначало надо изучить структуру ZCAD, потом модифицировать код с учетом существующей реализации:

```pascal
unit uzeentimage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uzcgeo, uzcentity, uzcdrawing,
  Graphics, FPimage, IntfGraphics;

type
  { Определение растрового ресурса чертежа (IMAGEDEF) }
  TZCADImageDef = class
  public
    Handle: string;              // Дескриптор DXF 5
    FilePath: string;            // Относительный или абсолютный путь DXF 1
    PixelWidth: Double;          // Ширина в пикселях DXF 10
    PixelHeight: Double;         // Высота в пикселях DXF 20
    PixelSizeX: Double;          // Шаг пикселя по умолчанию DXF 11
    PixelSizeY: Double;          // Шаг пикселя по умолчанию DXF 21
    IsLoaded: Boolean;           // Статус загрузки DXF 280
    Units: Integer;              // Единицы разрешения DXF 281
    ResolvedPath: string;        // Фактический путь к файлу на диске
    TextureID: Cardinal;         // Идентификатор OpenGL текстуры
    RasterData: TBitmap;         // Загруженный в память растр
    constructor Create;
    destructor Destroy; override;
    function LoadRasterFile(const BaseDir: string): Boolean;
  end;

  { Графический примитив растрового изображения на чертеже (IMAGE) }
  TZCImage = class(TZCEntity)
  private
    FInsertPoint: TPoint3D;      // Точка вставки левого нижнего угла (DXF 10, 20, 30)
    FUVector: TPoint3D;          // Шаг одного пикселя по U (DXF 11, 21, 31)
    FVVector: TPoint3D;          // Шаг одного пикселя по V (DXF 12, 22, 32)
    FPixelWidth: Double;         // Размер растра в пикселях U (DXF 13)
    FPixelHeight: Double;        // Размер растра в пикселях V (DXF 23)
    FImageDefHandle: string;     // Ссылка на IMAGEDEF (DXF 340)
    FImageDef: TZCADImageDef;    // Разрешенный указатель на ресурс
    FDisplayProps: Integer;      // Флаги отображения (DXF 70)
    FBrightness: Integer;        // Яркость 0..100 (DXF 281)
    FContrast: Integer;          // Контрастность 0..100 (DXF 282)
    FFade: Integer;              // Слияние 0..100 (DXF 283)
    FShowFrame: Boolean;         // Видимость рамки изображения
  public
    constructor Create; override;
    destructor Destroy; override;

    // Расчет 4 угловых точек в WCS
    function GetCornerPointBL: TPoint3D; // Левый нижний (P0)
    function GetCornerPointBR: TPoint3D; // Правый нижний (P0 + W*U)
    function GetCornerPointTR: TPoint3D; // Правый верхний (P0 + W*U + H*V)
    function GetCornerPointTL: TPoint3D; // Левый верхний (P0 + H*V)

    function GetCadWidth: Double;  // Физическая ширина (мм)
    function GetCadHeight: Double; // Физическая высота (мм)
    function GetRotationAngleRad: Double;

    // Базовые полиморфные методы ZCAD
    procedure Draw(const ADrawContext: TDrawContext); override;
    procedure GetBoundingBox(var ABoundingBox: TBoundingBox); override;
    function FormatEntityName: string; override;

    property InsertPoint: TPoint3D read FInsertPoint write FInsertPoint;
    property UVector: TPoint3D read FUVector write FUVector;
    property VVector: TPoint3D read FVVector write FVVector;
    property PixelWidth: Double read FPixelWidth write FPixelWidth;
    property PixelHeight: Double read FPixelHeight write FPixelHeight;
    property ImageDefHandle: string read FImageDefHandle write FImageDefHandle;
    property ImageDef: TZCADImageDef read FImageDef write FImageDef;
  end;

implementation

{ ... реализация методов ... }
```

---

### 3.2. Математика отрисовки (OpenGL / Render Context)
Сначало надо изучить структуру ZCAD, затем дополнить сущетсвующие механизмы или вынести добавляемое в отдельный файл для более гибкого развития отображения. Какой вариант выбрать остается за разработчиком

---

### 3.3. Модификация модуля чтения DXF.
В ZCAD добавлена систем чтения с использованием NOD. Для чтения и записи картинок использовать эту систему. 

---

### 3.4. Модификация модуля записи DXF:
В ZCAD добавлена систем чтения с использованием NOD. Для чтения и записи картинок использовать эту систему. 


