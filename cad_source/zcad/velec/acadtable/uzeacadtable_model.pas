{
*****************************************************************************
*                                                                           *
*  This file is part of the ZCAD                                            *
*                                                                           *
*  See the file COPYING.txt, included in this distribution,                 *
*  for details about the copyright.                                         *
*                                                                           *
*  This program is distributed in the hope that it will be useful,          *
*  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
*                                                                           *
*****************************************************************************
}
{
@author(Vladimir Bobrov)
}

{
  Модуль: uzeacadtable_model
  Назначение: Основная сущность GDBObjAcadTable — таблица AutoCAD
  из формата DXF. Хранит данные таблицы (точку вставки, размеры,
  тексты ячеек, стили, объединения) и делегирует логику
  специализированным модулям.
  Зависимости: uzeacadtable_types, uzeacadtable_styles,
               uzeacadtable_cell, uzeacadtable_merge,
               uzeacadtable_layout, uzeacadtable_stylemanager,
               uzeacadtable_dxf_read, uzeacadtable_dxf_write,
               uzeentcomplex, uzeentityfactory, uzeconsts и др.
}

unit uzeacadtable_model;
{$Mode delphi}{$H+}
{$INCLUDE zengineconfig.inc}

interface

uses
  uzgldrawcontext, uzedrawingdef, uzeentityfactory, uzeentcomplex,
  uzeentline, uzeentmtext, uzeentsubordinated, uzeentabstracttext,
  uzeentity, uzctnrVectorBytesStream, uzeTypes, uzeconsts,
  uzegeometry, uzegeometrytypes, uzeffdxfsupport, uzMVReader,
  uzbLogIntf, uzclog, SysUtils, Math, uzctnrvectordouble,
  uzestylestablesdxf, gzctnrVectorTypes, Types, uzestylestexts,
  uzeacadtable_types, uzeacadtable_styles,
  uzeacadtable_cell, uzeacadtable_merge,
  uzeacadtable_layout, uzeacadtable_stylemanager,
  uzeacadtable_dxf_read, uzeacadtable_dxf_write;

type
  // Тип указателя на GDBObjAcadTable
  PGDBObjAcadTable = ^GDBObjAcadTable;

  // Данные одной части (фрагмента) разделённой по ширине таблицы.
  // AutoCAD сохраняет разбитую таблицу в DXF как несколько ACAD_TABLE
  // сущностей (ROUNDTRIP_2008). Первая часть — это сам объект, остальные
  // хранятся здесь как данные и отображаются в одном составном объекте
  // со смещением относительно точки вставки главной части (issue #1300).
  TAcadTablePart = record
    InsertPoint: TzePoint3d;
    // Логический индекс первой строки части в полной таблице. Нужен для
    // базового стиля Title/Header/Data, когда строки смещаются при удалении
    // повторяющихся верхних меток (issue #1311).
    RowBaseIndex: Integer;
    RowCount: Integer;
    ColCount: Integer;
    RowHeights: TZctnrVectorDouble;
    ColWidths: TZctnrVectorDouble;
    CellTexts: TTableTextArray;
    TableFlags: Integer;
    TableStyle: TTableStyle;
    // Хэндл DXF-стиля таблицы этой части (group code 342). Нужен, чтобы
    // применить тот же табличный стиль, что и к главной части: иначе у
    // продолжений остаётся стиль по умолчанию и текст рендерится высотой
    // CAcadTableDefaultTextHeight вместо высоты из DXF (issue #1300).
    TableStyleHandle: String;
    Rows: TTableRowArray;
    Cols: TTableColumnArray;
    Cells: TTableCellArray;
    Merges: TMergeRangeArray;
    BreakEnabled: Boolean;
    BreakDirection: TAcadTableBreakDirection;
    BreakRepeatTopLabels: Boolean;
    BreakRepeatBottomLabels: Boolean;
    BreakManualPosition: Boolean;
    BreakManualHeight: Boolean;
    BreakSpacing: Double;
    BreakHeight: Double;
  end;

  // Сущность ACAD_TABLE — таблица AutoCAD из формата DXF.
  // Хранит геометрию таблицы и текстовое содержимое ячеек.
  // При форматировании строит визуальное представление из линий и текста.
  GDBObjAcadTable = object(GDBObjComplex)
  private
    // Точка вставки таблицы
    FInsertPoint: TzePoint3d;
    // Количество строк
    FRowCount: Integer;
    // Количество столбцов
    FColCount: Integer;
    // Высоты строк (один элемент на строку)
    FRowHeights: TZctnrVectorDouble;
    // Ширины столбцов (один элемент на столбец)
    FColWidths: TZctnrVectorDouble;
    // Тексты ячеек: индекс = строка * FColCount + столбец
    FCellTexts: TTableTextArray;
    // Признак того, что геометрия уже была построена
    FGeometryBuilt: Boolean;
    // Хэндл DXF-стиля таблицы (group code 342)
    FTableStyleHandle: String;
    // Флаги свойств таблицы (group code 90)
    FTableFlags: Integer;
    // Параметры разрыва таблицы
    FBreakEnabled: Boolean;
    FBreakDirection: TAcadTableBreakDirection;
    FBreakRepeatTopLabels: Boolean;
    FBreakRepeatBottomLabels: Boolean;
    FBreakManualPosition: Boolean;
    FBreakManualHeight: Boolean;
    FBreakSpacing: Double;
    // Высота разбиения (break height): после какой суммарной высоты строк
    // строки начинают переноситься в следующую часть разделённой таблицы.
    // Читается из XRECORD ACAD_ROUNDTRIP_2008_TABLE_ENTITY (issue #1307).
    FBreakHeight: Double;
    // Масштаб и поворот объекта (как у вставки блока). Хранятся отдельно,
    // потому что базовый CalcObjMatrixWithoutOwner восстанавливает ось OX
    // из OZ и теряет поворот в плоскости и масштаб. Благодаря этим полям
    // перенос, поворот и масштабирование таблицы отображаются корректно
    // (issue #1305).
    FScale: TzePoint3d;
    FRotate: Double;
    // Стиль таблицы
    FTableStyle: TTableStyle;
    // Строки, столбцы, ячейки и объединения
    FRows: TTableRowArray;
    FCols: TTableColumnArray;
    FCells: TTableCellArray;
    FMerges: TMergeRangeArray;
    // Части (фрагменты) разделённой по ширине таблицы, объединённые
    // в один объект AcadTable. Главная часть — это сам объект, а
    // продолжения хранятся здесь как данные и отображаются со смещением
    // относительно точки вставки главной части (issue #1300).
    FContinuationParts: array of TAcadTablePart;

    // Обёртки для делегирования к модулю layout
    function GetRowHeightLocal(RowIndex: Integer): Double;
    function GetColWidthLocal(ColIndex: Integer): Double;
    function GetTotalHeight: Double;
    function GetTotalWidth: Double;
    function GetCellTextLocal(
      RowIdx, ColIdx: Integer): String;
    // Строит визуальное представление текущих полей таблицы в
    // ConstObjArray со смещением (ABaseX, ABaseY) в OCS.
    // ARowBaseIndex задаёт логический индекс первой строки для выбора
    // базового стиля строки.
    procedure RenderCurrentTable(
      var ADrawing: TDrawingDef; var ADC: TDrawContext;
      ABaseX, ABaseY: Double; ARowBaseIndex: Integer);
    // Строит визуальное представление всей таблицы (главная часть и
    // все продолжения) в ConstObjArray
    procedure BuildVisualRepresentation(
      var ADrawing: TDrawingDef; var ADC: TDrawContext);
    // Обмен данными рендеринга между Self и частью-продолжением
    procedure SwapTableData(var APart: TAcadTablePart);
    // Глубокое копирование данных другой таблицы в часть-продолжение
    procedure CaptureTableDataToPart(
      var ASource: GDBObjAcadTable; var APart: TAcadTablePart);
    // Освобождение ресурсов части-продолжения
    procedure ClearPart(var APart: TAcadTablePart);
    // Глубокое копирование части-продолжения (для Clone)
    procedure CopyTablePart(
      const ASource: TAcadTablePart; var ADest: TAcadTablePart);
    // Вычисляет bounding box таблицы
    procedure getoutbound(var DC: TDrawContext);
    // Раскладывает objmatrix на точку вставки, базис и масштаб (issue #1305)
    procedure decomposite;
    // Поворачивает objmatrix вокруг оси Z на угол r (issue #1305)
    procedure setrot(r: Double);
    // Возвращает имя стиля таблицы
    function GetTableStyleName: String;
    // Возвращает количество частей-продолжений
    function GetContinuationPartCount: Integer;
    // Чтение/запись вычисляемого признака разрыва таблицы (issue #1305)
    function GetBreakEnabled: Boolean;
    procedure SetBreakEnabled(AValue: Boolean);
    // Чтение/запись направления разрыва таблицы (issue #1315)
    procedure SetBreakDirection(AValue: TAcadTableBreakDirection);
    // Чтение/запись интервала между частями и высоты разбиения (issue #1307)
    function GetBreakSpacing: Double;
    procedure SetBreakSpacing(AValue: Double);
    function GetBreakHeight: Double;
    procedure SetBreakHeight(AValue: Double);
    // Объединяет все части-продолжения в главную часть (строки сверху вниз).
    procedure MergeAllContinuationPartsIntoMain;
    // Пересегментирует объединённую главную часть по высоте разбиения,
    // вынося хвостовые строки в новые части-продолжения (issue #1307).
    procedure SplitMainTableByBreakHeight(AThreshold: Double);
    // Копирует диапазон строк [AStart..AEnd] исходной части в целевую часть.
    procedure SlicePartFromPart(const ASource: TAcadTablePart;
      AStart, AEnd: Integer; var ADest: TAcadTablePart);
    // Пересчитывает точки вставки частей-продолжений из текущего интервала
    // и направления разрыва (issue #1307).
    procedure RepositionContinuationParts;
    // Верхняя граница зоны ведущих строк-меток (Title/Header) по индексу
    // строки: 0, 1 или 2. Тип строки определяется индексом (issue #1309).
    function ComputeTopLabelRowCount: Integer;
    // Фактическое число ведущих строк-меток, одинаково повторяющихся во всех
    // частях-продолжениях; 0, если повтора нет (по содержимому, issue #1309).
    function EffectiveRepeatTopRowCount: Integer;
    // Проверяет, повторяет ли часть-продолжение первые L строк-меток главной
    // части (тексты ячеек первых L строк совпадают) (issue #1309).
    function PartRepeatsTopLabels(
      const APart: TAcadTablePart; L: Integer): Boolean;
    // Обновляет логические индексы первых строк продолжений для выбора
    // базового стиля Title/Header/Data (issue #1311).
    procedure UpdateContinuationRowBaseIndexes;
    // Определяет значение BreakRepeatTopLabels по данным частей-продолжений:
    // если все части повторяют ведущие строки-метки главной части, значит
    // таблица была разорвана с повтором верхних меток (issue #1309).
    procedure DetectBreakRepeatTopLabels;
    // Чтение/запись признака повтора верхних меток (issue #1309).
    function GetBreakRepeatTopLabels: Boolean;
    procedure SetBreakRepeatTopLabels(AValue: Boolean);
    // Удаляет первые L строк-меток из каждой части-продолжения (issue #1309).
    procedure RemoveTopLabelsFromParts(L: Integer);
    // Добавляет первые L строк-меток главной части в начало каждой
    // части-продолжения, если их там ещё нет (issue #1309).
    procedure AddTopLabelsToParts(L: Integer);
    // Вставляет первые L строк-меток главной части в начало одной
    // части-продолжения (issue #1309).
    procedure PrependTopLabelsToPart(L: Integer; var APart: TAcadTablePart);

  public
    constructor initnul(
      AOwner: PGDBObjGenericWithSubordinated);
    destructor done; virtual;

    procedure LoadFromDXF(var ARdr: TZMemReader;
      APtu: PExtensionData;
      var ADrawing: TDrawingDef;
      var AContext: TIODXFLoadContext); virtual;
    procedure SaveToDXF(var AOutStream: TZctnrVectorBytes;
      var ADrawing: TDrawingDef;
      var AIODXFContext: TIODXFSaveContext); virtual;
    procedure SaveToDXFFollow(
      var AOutStream: TZctnrVectorBytes;
      var ADrawing: TDrawingDef;
      var AIODXFContext: TIODXFSaveContext); virtual;
    procedure BuildGeometry(
      var ADrawing: TDrawingDef); virtual;
    procedure FormatEntity(var ADrawing: TDrawingDef;
      var ADC: TDrawContext;
      AStage: TEFStages = EFAllStages); virtual;
    function IsStagedFormatEntity: Boolean; virtual;
    // Трансформация объекта: масштаб и поворот учитываются как у вставки
    // блока, чтобы перенос/поворот/масштабирование перестраивали таблицу
    // (issue #1305, часть 1).
    procedure CalcObjMatrix(pdrawing: PTDrawingDef = nil); virtual;
    procedure ReCalcFromObjMatrix; virtual;
    procedure rtsave(refp: Pointer); virtual;
    function Clone(AOwn: Pointer): PGDBObjEntity; virtual;
    function GetObjType: TObjID; virtual;
    function GetObjTypeName: String; virtual;
    function DXFDelayedBuildGeometry: Boolean; virtual;
    // Поглощает продолжение разделённой таблицы как ещё одну часть
    // этого же объекта AcadTable (issue #1300). Возвращает True, если
    // AOther поглощён и вызывающая сторона может его освободить.
    function TryMergeContinuation(
      AOther: PGDBObjEntity): Boolean; virtual;
    // Сохраняет параметры разбиения (интервал и высоту), прочитанные
    // загрузчиком DXF из XRECORD ACAD_ROUNDTRIP_2008_TABLE_ENTITY
    // (issue #1307). Возвращает True.
    function SetTableBreakData(
      ASpacing, ABreakHeight: Double): Boolean; virtual;

    // Публичные свойства для инспектора объектов
    property InsertPoint: TzePoint3d read FInsertPoint;
    property RowCount: Integer read FRowCount;
    property ColCount: Integer read FColCount;
    property Width: Double read GetTotalWidth;
    property Height: Double read GetTotalHeight;
    property TableStyleName: String read GetTableStyleName;
    // Признак разрыва вычисляется: учитывает как DXF-флаг, так и
    // поглощённые части-продолжения (issue #1305, часть 2a). Запись
    // в False объединяет разорванную таблицу сверху вниз (часть 2b).
    property BreakEnabled: Boolean read GetBreakEnabled write SetBreakEnabled;
    property BreakDirection: TAcadTableBreakDirection
      read FBreakDirection write SetBreakDirection;
    // Повтор верхних меток в каждой части разорванной таблицы (issue #1309).
    // Чтение возвращает определённое при загрузке значение; запись в False
    // удаляет повторяющиеся строки-метки из всех частей-продолжений, запись
    // в True — добавляет их обратно.
    property BreakRepeatTopLabels: Boolean
      read GetBreakRepeatTopLabels write SetBreakRepeatTopLabels;
    property BreakRepeatBottomLabels: Boolean
      read FBreakRepeatBottomLabels;
    property BreakManualPosition: Boolean
      read FBreakManualPosition;
    property BreakManualHeight: Boolean
      read FBreakManualHeight;
    // Интервал между частями разделённой таблицы (issue #1307). Чтение
    // возвращает значение из XRECORD; запись перестраивает расстояние
    // между всеми частями на чертеже.
    property BreakSpacing: Double
      read GetBreakSpacing write SetBreakSpacing;
    // Высота разбиения (issue #1307). Чтение возвращает значение из
    // XRECORD; запись пересчитывает число строк в каждой части
    // (автоматически определяя необходимое число частей).
    property BreakHeight: Double
      read GetBreakHeight write SetBreakHeight;
    // Количество поглощённых частей-продолжений (issue #1300).
    // Для неразделённой таблицы равно 0.
    property ContinuationPartCount: Integer read GetContinuationPartCount;
    // Число строк в части-продолжении по индексу (для инспекции/тестов,
    // issue #1309). Для некорректного индекса возвращает -1.
    function ContinuationPartRowCount(AIndex: Integer): Integer;
    // Текст ячейки части-продолжения (для тестов, issue #1309). Для
    // некорректных индексов возвращает пустую строку.
    function ContinuationPartCellText(
      APartIndex, ARow, ACol: Integer): string;
    // Повторно определяет признак повтора верхних меток по текущим данным
    // частей-продолжений и возвращает результат (issue #1309). Используется
    // для проверки детекции после программных изменений модели.
    function RecomputeBreakRepeatTopLabels: Boolean;
  end;

function AllocAcadTable: Pointer;
function AllocAndInitAcadTable(
  AOwner: PGDBObjGenericWithSubordinated): PGDBObjAcadTable;

implementation

// --- Обёртки для делегирования ---

function GDBObjAcadTable.GetRowHeightLocal(
  RowIndex: Integer): Double;
begin
  Result := uzeacadtable_layout.GetRowHeight(
    RowIndex, FRowHeights);
end;

function GDBObjAcadTable.GetColWidthLocal(
  ColIndex: Integer): Double;
begin
  Result := uzeacadtable_layout.GetColWidth(
    ColIndex, FColWidths);
end;

function GDBObjAcadTable.GetTotalHeight: Double;
begin
  Result := uzeacadtable_layout.GetTotalHeight(
    FRowCount, FRowHeights);
end;

function GDBObjAcadTable.GetTotalWidth: Double;
begin
  Result := uzeacadtable_layout.GetTotalWidth(
    FColCount, FColWidths);
end;

function GDBObjAcadTable.GetTableStyleName: String;
begin
  Result := FTableStyle.Name;
end;

function GDBObjAcadTable.GetCellTextLocal(
  RowIdx, ColIdx: Integer): String;
var
  CellIndex: Integer;
begin
  Result := '';
  if FColCount <= 0 then Exit;
  CellIndex := RowIdx * FColCount + ColIdx;
  if (CellIndex >= 0) and
     (CellIndex <= High(FCellTexts)) then
    Result := FCellTexts[CellIndex];
end;

// --- Конструктор и деструктор ---

constructor GDBObjAcadTable.initnul(
  AOwner: PGDBObjGenericWithSubordinated);
begin
  inherited initnul;
  FInsertPoint := NulVertex;
  FRowCount := 0;
  FColCount := 0;
  FRowHeights.initnul;
  FColWidths.initnul;
  System.SetLength(FCellTexts, 0);
  FGeometryBuilt := False;
  FTableStyleHandle := '';
  FTableFlags := 0;
  FBreakEnabled := False;
  FBreakDirection := atbdRight;
  FBreakRepeatTopLabels := False;
  FBreakRepeatBottomLabels := False;
  FBreakManualPosition := False;
  FBreakManualHeight := False;
  FBreakSpacing := 0;
  FBreakHeight := 0;
  // Трансформация по умолчанию: единичный масштаб, без поворота (issue #1305)
  FScale := ScaleOne;
  FRotate := 0;
  InitTableStyle(FTableStyle);
  System.SetLength(FRows, 0);
  System.SetLength(FCols, 0);
  System.SetLength(FCells, 0, 0);
  System.SetLength(FMerges, 0);
  System.SetLength(FContinuationParts, 0);
end;

destructor GDBObjAcadTable.done;
var
  PartIdx: Integer;
begin
  FRowHeights.done;
  FColWidths.done;
  System.SetLength(FCellTexts, 0);
  System.SetLength(FRows, 0);
  System.SetLength(FCols, 0);
  System.SetLength(FCells, 0, 0);
  System.SetLength(FMerges, 0);
  for PartIdx := 0 to High(FContinuationParts) do
    ClearPart(FContinuationParts[PartIdx]);
  System.SetLength(FContinuationParts, 0);
  inherited done;
end;

// --- Загрузка из DXF ---

procedure GDBObjAcadTable.LoadFromDXF(
  var ARdr: TZMemReader; APtu: PExtensionData;
  var ADrawing: TDrawingDef;
  var AContext: TIODXFLoadContext);
var
  GroupCode: Integer;
  SubclassMarker: String;
  DXFData: TAcadTableDXFData;
  RowIdx, ColIdx, CellIndex: Integer;
  MergeRange: TMergeRange;
begin
  programlog.LogOutStr(
    'AcadTable: model: LoadFromDXF START', LM_Info);

  GroupCode := ARdr.ParseInteger;

  // Обрабатываем общие коды (handle, слой, цвет и т.п.) до первого 100.
  // Важно: handle сущности (группа 5) и владелец (330) идут ДО маркера
  // подкласса AcDbEntity. Если их просто пропускать, PExtAttrib^.dwgHandle
  // остаётся равным 0, и логика отбрасывания продолжений разделённой
  // таблицы (TableContinuationHandles в uzeffdxf) не срабатывает — части
  // одной таблицы загружаются как отдельные ACAD_TABLE (issue #1300).
  while (GroupCode <> 0) and (GroupCode <> 100) do
  begin
    if not LoadFromDXFObjShared(
      ARdr, GroupCode, APtu, ADrawing, AContext) then
      ARdr.SkipString;
    GroupCode := ARdr.ParseInteger;
  end;

  // AcDbEntity
  if GroupCode = 100 then
  begin
    SubclassMarker := ARdr.ParseString;
    GroupCode := ARdr.ParseInteger;
  end;

  // Пропускаем AcDbEntity до AcDbBlockReference
  while (GroupCode <> 0) and (GroupCode <> 100) do
  begin
    if not LoadFromDXFObjShared(
      ARdr, GroupCode, APtu, ADrawing, AContext) then
      ARdr.SkipString;
    GroupCode := ARdr.ParseInteger;
  end;

  // AcDbBlockReference
  if GroupCode = 100 then
  begin
    SubclassMarker := ARdr.ParseString;
    GroupCode := ARdr.ParseInteger;
    while (GroupCode <> 0) and (GroupCode <> 100) do
    begin
      case GroupCode of
        10: FInsertPoint.x := ARdr.ParseDouble;
        20: FInsertPoint.y := ARdr.ParseDouble;
        30: FInsertPoint.z := ARdr.ParseDouble;
      else
        if not LoadFromDXFObjShared(
          ARdr, GroupCode, APtu, ADrawing, AContext) then
          ARdr.SkipString;
      end;
      GroupCode := ARdr.ParseInteger;
    end;
  end;

  // AcDbTable — читаем маркер подкласса
  if GroupCode = 100 then
  begin
    SubclassMarker := ARdr.ParseString;
  end;

  // Инициализируем структуру для чтения DXF-данных
  DXFData.InsertPoint := FInsertPoint;
  DXFData.RowCount := 0;
  DXFData.ColCount := 0;
  DXFData.RowHeights.initnul;
  DXFData.ColWidths.initnul;
  DXFData.TableStyleHandle := '';
  DXFData.TableFlags := 0;
  DXFData.BreakEnabled := False;
  DXFData.BreakDirection := atbdRight;
  DXFData.BreakRepeatTopLabels := False;
  DXFData.BreakRepeatBottomLabels := False;
  DXFData.BreakManualPosition := False;
  DXFData.BreakManualHeight := False;
  DXFData.BreakSpacing := 0;

  // Делегируем чтение данных таблицы модулю dxf_read
  ReadAcadTableFromDXF(ARdr, AContext, DXFData);

  // Копируем результат в поля объекта
  FInsertPoint := DXFData.InsertPoint;
  FRowCount := DXFData.RowCount;
  FColCount := DXFData.ColCount;
  FTableStyleHandle := DXFData.TableStyleHandle;
  FTableFlags := DXFData.TableFlags;
  FBreakEnabled := DXFData.BreakEnabled;
  FBreakDirection := DXFData.BreakDirection;
  FBreakRepeatTopLabels := DXFData.BreakRepeatTopLabels;
  FBreakRepeatBottomLabels := DXFData.BreakRepeatBottomLabels;
  FBreakManualPosition := DXFData.BreakManualPosition;
  FBreakManualHeight := DXFData.BreakManualHeight;
  FBreakSpacing := DXFData.BreakSpacing;

  // Копируем высоты и ширины
  for RowIdx := 0 to DXFData.RowHeights.Count - 1 do
    FRowHeights.PushBackData(
      DXFData.RowHeights.getData(RowIdx));
  for ColIdx := 0 to DXFData.ColWidths.Count - 1 do
    FColWidths.PushBackData(
      DXFData.ColWidths.getData(ColIdx));

  // Копируем тексты ячеек
  System.SetLength(FCellTexts, Length(DXFData.CellTexts));
  for CellIndex := 0 to High(DXFData.CellTexts) do
    FCellTexts[CellIndex] := DXFData.CellTexts[CellIndex];

  // Точка вставки для Local
  Local.P_insert := FInsertPoint;

  // Инициализируем табличный стиль
  InitTableStyle(FTableStyle);

  // Инициализируем строки, столбцы и ячейки
  if (FRowCount > 0) and (FColCount > 0) then
  begin
    System.SetLength(FRows, FRowCount);
    for RowIdx := 0 to FRowCount - 1 do
    begin
      FRows[RowIdx].Height := GetRowHeightLocal(RowIdx);
      InitCellStyle(FRows[RowIdx].Style);
    end;

    System.SetLength(FCols, FColCount);
    for ColIdx := 0 to FColCount - 1 do
    begin
      FCols[ColIdx].Width := GetColWidthLocal(ColIdx);
      InitCellStyle(FCols[ColIdx].Style);
    end;

    System.SetLength(FCells, FRowCount, FColCount);
    for RowIdx := 0 to FRowCount - 1 do
      for ColIdx := 0 to FColCount - 1 do
      begin
        FCells[RowIdx][ColIdx].DataType := cdtText;
        FCells[RowIdx][ColIdx].Text :=
          GetCellTextLocal(RowIdx, ColIdx);
        FCells[RowIdx][ColIdx].Value := 0;
        FCells[RowIdx][ColIdx].Formula := '';
        FCells[RowIdx][ColIdx].BlockName := '';
        FCells[RowIdx][ColIdx].CellAlignment := 0;
        FCells[RowIdx][ColIdx].ColSpan := 1;
        FCells[RowIdx][ColIdx].RowSpan := 1;
        InitCellStyle(FCells[RowIdx][ColIdx].Style);
        CellIndex := RowIdx * FColCount + ColIdx;
        if CellIndex < Length(DXFData.CellAlignments) then
          FCells[RowIdx][ColIdx].CellAlignment :=
            DXFData.CellAlignments[CellIndex];
        if CellIndex < Length(DXFData.CellColSpans) then
          FCells[RowIdx][ColIdx].ColSpan :=
            DXFData.CellColSpans[CellIndex];
        if CellIndex < Length(DXFData.CellRowSpans) then
          FCells[RowIdx][ColIdx].RowSpan :=
            DXFData.CellRowSpans[CellIndex];
      end;

    // Строим массив объединений FMerges
    System.SetLength(FMerges, 0);
    for RowIdx := 0 to FRowCount - 1 do
      for ColIdx := 0 to FColCount - 1 do
      begin
        CellIndex := RowIdx * FColCount + ColIdx;
        if (CellIndex < Length(DXFData.CellVirtualFlags))
           and DXFData.CellVirtualFlags[CellIndex] then
          Continue;
        if (FCells[RowIdx][ColIdx].ColSpan > 1) or
           (FCells[RowIdx][ColIdx].RowSpan > 1) then
        begin
          MergeRange.Row1 := RowIdx;
          MergeRange.Col1 := ColIdx;
          MergeRange.Row2 :=
            RowIdx + FCells[RowIdx][ColIdx].RowSpan - 1;
          MergeRange.Col2 :=
            ColIdx + FCells[RowIdx][ColIdx].ColSpan - 1;
          if MergeRange.Row2 >= FRowCount then
            MergeRange.Row2 := FRowCount - 1;
          if MergeRange.Col2 >= FColCount then
            MergeRange.Col2 := FColCount - 1;
          System.SetLength(FMerges, Length(FMerges) + 1);
          FMerges[High(FMerges)] := MergeRange;
          programlog.LogOutFormatStr(
            'AcadTable: model: LoadFromDXF Merge[%d] ' +
            '(%d,%d)-(%d,%d)',
            [High(FMerges), MergeRange.Row1,
             MergeRange.Col1, MergeRange.Row2,
             MergeRange.Col2], LM_Info);
        end;
      end;
  end;

  // Освобождаем временные данные
  DXFData.RowHeights.done;
  DXFData.ColWidths.done;

  programlog.LogOutFormatStr(
    'AcadTable: model: LoadFromDXF END rows=%d cols=%d ' +
    'cells=%d merges=%d',
    [FRowCount, FColCount, Length(FCellTexts),
     Length(FMerges)], LM_Info);
end;

// --- Построение визуального представления ---

procedure GDBObjAcadTable.RenderCurrentTable(
  var ADrawing: TDrawingDef; var ADC: TDrawContext;
  ABaseX, ABaseY: Double; ARowBaseIndex: Integer);
var
  RowIdx, ColIdx, SegmentIdx, SegmentCount: Integer;
  CurrentY, CurrentX: Double;
  TotalWidth, TotalHeight, SegmentHeight: Double;
  RowH, ColW: Double;
  PLine: PGDBObjLine;
  PMText: PGDBObjMText;
  CellStr: String;
  LineCount, TextCount: Integer;
  CellStyleLocal: TCellStyle;
  TextHeightLocal: Double;
  RenderSegments: array[0..255] of TAcadTableRenderSegment;
  SegmentOffsetX, SegmentOffsetY: Double;
  MergeRootPt: TPoint;
  InlineBreakEnabled: Boolean;
begin
  programlog.LogOutFormatStr(
    'AcadTable: model: RenderCurrentTable START ' +
    'rows=%d cols=%d baseX=%g baseY=%g',
    [FRowCount, FColCount, ABaseX, ABaseY], LM_Info);

  if (FRowCount <= 0) or (FColCount <= 0) then
  begin
    programlog.LogOutStr(
      'AcadTable: model: RenderCurrentTable — ' +
      'таблица пуста', LM_Info);
    Exit;
  end;

  TotalWidth := GetTotalWidth;
  TotalHeight := GetTotalHeight;
  LineCount := 0;
  TextCount := 0;

  // Если разбиение уже представлено частями-продолжениями или сохранённой
  // BreakHeight-моделью, не запускаем дополнительное inline-разбиение
  // текущего фрагмента при отрисовке (issue #1313).
  InlineBreakEnabled :=
    FBreakEnabled and (FBreakHeight <= 0) and
    (Length(FContinuationParts) = 0);

  uzeacadtable_layout.BuildRenderSegments(
    FRowCount, FRowHeights, FColWidths, FColCount,
    FTableFlags, InlineBreakEnabled, FBreakDirection,
    FBreakRepeatTopLabels, FBreakManualHeight,
    FBreakSpacing, RenderSegments, SegmentCount);
  if SegmentCount <= 0 then Exit;

  // --- Горизонтальные линии ---
  for SegmentIdx := 0 to SegmentCount - 1 do
  begin
    SegmentOffsetX := RenderSegments[SegmentIdx].OffsetX + ABaseX;
    SegmentOffsetY := RenderSegments[SegmentIdx].OffsetY + ABaseY;
    CurrentY := 0;
    for RowIdx := RenderSegments[SegmentIdx].StartRow to
                  RenderSegments[SegmentIdx].EndRow + 1 do
    begin
      if (RowIdx = RenderSegments[SegmentIdx].StartRow) or
         (RowIdx =
           RenderSegments[SegmentIdx].EndRow + 1) then
      begin
        pointer(PLine) :=
          ConstObjArray.CreateInitObj(GDBLineID, @Self);
        PLine^.CoordInOCS.lBegin.x := SegmentOffsetX;
        PLine^.CoordInOCS.lBegin.y :=
          SegmentOffsetY - CurrentY;
        PLine^.CoordInOCS.lBegin.z := 0;
        PLine^.CoordInOCS.lEnd.x :=
          SegmentOffsetX + TotalWidth;
        PLine^.CoordInOCS.lEnd.y :=
          SegmentOffsetY - CurrentY;
        PLine^.CoordInOCS.lEnd.z := 0;
        CopyVPto(PLine^);
        PLine^.FormatEntity(ADrawing, ADC);
        Inc(LineCount);
      end
      else
      begin
        CurrentX := 0;
        for ColIdx := 0 to FColCount - 1 do
        begin
          ColW := GetColWidthLocal(ColIdx);
          if uzeacadtable_merge.IsRowBorderVisible(
            RowIdx - 1, ColIdx, FMerges) then
          begin
            pointer(PLine) :=
              ConstObjArray.CreateInitObj(GDBLineID, @Self);
            PLine^.CoordInOCS.lBegin.x :=
              SegmentOffsetX + CurrentX;
            PLine^.CoordInOCS.lBegin.y :=
              SegmentOffsetY - CurrentY;
            PLine^.CoordInOCS.lBegin.z := 0;
            PLine^.CoordInOCS.lEnd.x :=
              SegmentOffsetX + CurrentX + ColW;
            PLine^.CoordInOCS.lEnd.y :=
              SegmentOffsetY - CurrentY;
            PLine^.CoordInOCS.lEnd.z := 0;
            CopyVPto(PLine^);
            PLine^.FormatEntity(ADrawing, ADC);
            Inc(LineCount);
          end;
          CurrentX := CurrentX + ColW;
        end;
      end;

      if RowIdx <= RenderSegments[SegmentIdx].EndRow then
        CurrentY :=
          CurrentY + GetRowHeightLocal(RowIdx);
    end;
  end;

  // --- Вертикальные линии ---
  LineCount := 0;
  for SegmentIdx := 0 to SegmentCount - 1 do
  begin
    SegmentOffsetX := RenderSegments[SegmentIdx].OffsetX + ABaseX;
    SegmentOffsetY := RenderSegments[SegmentIdx].OffsetY + ABaseY;
    SegmentHeight := 0;
    for RowIdx := RenderSegments[SegmentIdx].StartRow to
                  RenderSegments[SegmentIdx].EndRow do
      SegmentHeight :=
        SegmentHeight + GetRowHeightLocal(RowIdx);

    CurrentX := 0;
    for ColIdx := 0 to FColCount do
    begin
      if (ColIdx = 0) or (ColIdx = FColCount) then
      begin
        pointer(PLine) :=
          ConstObjArray.CreateInitObj(GDBLineID, @Self);
        PLine^.CoordInOCS.lBegin.x :=
          SegmentOffsetX + CurrentX;
        PLine^.CoordInOCS.lBegin.y := SegmentOffsetY;
        PLine^.CoordInOCS.lBegin.z := 0;
        PLine^.CoordInOCS.lEnd.x :=
          SegmentOffsetX + CurrentX;
        PLine^.CoordInOCS.lEnd.y :=
          SegmentOffsetY - SegmentHeight;
        PLine^.CoordInOCS.lEnd.z := 0;
        CopyVPto(PLine^);
        PLine^.FormatEntity(ADrawing, ADC);
        Inc(LineCount);
      end
      else
      begin
        CurrentY := 0;
        for RowIdx := RenderSegments[SegmentIdx].StartRow
          to RenderSegments[SegmentIdx].EndRow do
        begin
          RowH := GetRowHeightLocal(RowIdx);
          if uzeacadtable_merge.IsColBorderVisible(
            RowIdx, ColIdx - 1, FMerges) then
          begin
            pointer(PLine) :=
              ConstObjArray.CreateInitObj(
                GDBLineID, @Self);
            PLine^.CoordInOCS.lBegin.x :=
              SegmentOffsetX + CurrentX;
            PLine^.CoordInOCS.lBegin.y :=
              SegmentOffsetY - CurrentY;
            PLine^.CoordInOCS.lBegin.z := 0;
            PLine^.CoordInOCS.lEnd.x :=
              SegmentOffsetX + CurrentX;
            PLine^.CoordInOCS.lEnd.y :=
              SegmentOffsetY - (CurrentY + RowH);
            PLine^.CoordInOCS.lEnd.z := 0;
            CopyVPto(PLine^);
            PLine^.FormatEntity(ADrawing, ADC);
            Inc(LineCount);
          end;
          CurrentY := CurrentY + RowH;
        end;
      end;

      if ColIdx < FColCount then
        CurrentX := CurrentX + GetColWidthLocal(ColIdx);
    end;
  end;

  // --- Текст ячеек ---
  TextCount := 0;
  for SegmentIdx := 0 to SegmentCount - 1 do
  begin
    SegmentOffsetX := RenderSegments[SegmentIdx].OffsetX + ABaseX;
    SegmentOffsetY := RenderSegments[SegmentIdx].OffsetY + ABaseY;
    CurrentY := 0;
    for RowIdx := RenderSegments[SegmentIdx].StartRow to
                  RenderSegments[SegmentIdx].EndRow do
    begin
      RowH := GetRowHeightLocal(RowIdx);
      CurrentX := 0;

      for ColIdx := 0 to FColCount - 1 do
      begin
        ColW := GetColWidthLocal(ColIdx);

        if uzeacadtable_merge.IsCellMerged(
          RowIdx, ColIdx, FMerges) then
        begin
          MergeRootPt := uzeacadtable_merge.GetMergeRoot(
            RowIdx, ColIdx, FMerges);
          if not ((RowIdx = MergeRootPt.Y) and
                  (ColIdx = MergeRootPt.X)) then
          begin
            CurrentX := CurrentX + ColW;
            Continue;
          end;
        end;

        CellStr := '';
        if (Length(FCells) > RowIdx) and
           (Length(FCells[RowIdx]) > ColIdx) then
          CellStr := FCells[RowIdx][ColIdx].Text
        else
          CellStr := GetCellTextLocal(RowIdx, ColIdx);

        ColW := uzeacadtable_merge.GetMergedCellWidth(
          RowIdx, ColIdx, FMerges, GetColWidthLocal);
        RowH := uzeacadtable_merge.GetMergedCellHeight(
          RowIdx, ColIdx, FMerges, GetRowHeightLocal);

        if CellStr <> '' then
        begin
          CellStyleLocal := uzeacadtable_cell.ResolveCellStyleForBaseRow(
            ARowBaseIndex + RowIdx, RowIdx, ColIdx, FTableStyle,
            FRows, FCols, FCells,
            FRowCount, FColCount, FTableFlags);

          pointer(PMText) :=
            ConstObjArray.CreateInitObj(GDBMTextID, @Self);
          PMText^.Template := UTF8ToString(CellStr);

          if CellStyleLocal.TextHeight > 0 then
            PMText^.textprop.size :=
              CellStyleLocal.TextHeight
          else
            PMText^.textprop.size :=
              CAcadTableDefaultTextHeight;

          PMText^.linespacef := 1;
          PMText^.WrapMode := mwmByWordThenChar;
          PMText^.Width := ColW * 0.9;

          // Выравнивание justify
          case CellStyleLocal.VertAlign of
            vaTop:
              case CellStyleLocal.HorzAlign of
                haLeft: PMText^.textprop.justify := jstl;
                haCenter: PMText^.textprop.justify := jstc;
                haRight: PMText^.textprop.justify := jstr;
              else
                PMText^.textprop.justify := jstl;
              end;
            vaMiddle:
              case CellStyleLocal.HorzAlign of
                haLeft: PMText^.textprop.justify := jsml;
                haCenter: PMText^.textprop.justify := jsmc;
                haRight: PMText^.textprop.justify := jsmr;
              else
                PMText^.textprop.justify := jsml;
              end;
            vaBottom:
              case CellStyleLocal.HorzAlign of
                haLeft: PMText^.textprop.justify := jsbl;
                haCenter: PMText^.textprop.justify := jsbc;
                haRight: PMText^.textprop.justify := jsbr;
              else
                PMText^.textprop.justify := jsbl;
              end;
          else
            PMText^.textprop.justify := jstl;
          end;

          TextHeightLocal := PMText^.textprop.size;

          // Позиция X
          case CellStyleLocal.HorzAlign of
            haLeft:
              PMText^.Local.P_insert.x :=
                SegmentOffsetX + CurrentX +
                TextHeightLocal * 0.5;
            haCenter:
              PMText^.Local.P_insert.x :=
                SegmentOffsetX + CurrentX + ColW / 2;
            haRight:
              PMText^.Local.P_insert.x :=
                SegmentOffsetX + CurrentX +
                ColW - TextHeightLocal * 0.5;
          else
            PMText^.Local.P_insert.x :=
              SegmentOffsetX + CurrentX +
              TextHeightLocal * 0.5;
          end;

          // Позиция Y
          case CellStyleLocal.VertAlign of
            vaTop:
              PMText^.Local.P_insert.y :=
                SegmentOffsetY -
                (CurrentY + TextHeightLocal * 0.5);
            vaMiddle:
              PMText^.Local.P_insert.y :=
                SegmentOffsetY -
                (CurrentY + RowH / 2);
            vaBottom:
              PMText^.Local.P_insert.y :=
                SegmentOffsetY -
                (CurrentY + RowH -
                 TextHeightLocal * 0.5);
          else
            PMText^.Local.P_insert.y :=
              SegmentOffsetY -
              (CurrentY + RowH / 2);
          end;

          PMText^.Local.P_insert.z := 0;
          PMText^.TXTStyle :=
            uzeacadtable_stylemanager.ResolveTextStyle(
              CellStyleLocal.TextStyle, ADrawing);
          CopyVPto(PMText^);
          PMText^.FormatEntity(ADrawing, ADC);
          Inc(TextCount);
        end;

        CurrentX := CurrentX + GetColWidthLocal(ColIdx);
      end;

      CurrentY := CurrentY + GetRowHeightLocal(RowIdx);
    end;
  end;

  programlog.LogOutFormatStr(
    'AcadTable: model: RenderCurrentTable OK ' +
    'rows=%d cols=%d texts=%d TotalObj=%d',
    [FRowCount, FColCount, TextCount,
     ConstObjArray.Count], LM_Info);
end;

// Обмен данными рендеринга между Self и частью-продолжением.
// TZctnrVectorDouble и динамические массивы обмениваются целиком
// (3-сторонний обмен), что переносит владение буферами без копирования.
procedure GDBObjAcadTable.SwapTableData(var APart: TAcadTablePart);
var
  TmpVec: TZctnrVectorDouble;
  TmpInt: Integer;
  TmpStyle: TTableStyle;
  TmpFlags: Integer;
  TmpBreakEnabled, TmpRepeatTop, TmpRepeatBottom: Boolean;
  TmpManualPos, TmpManualHeight: Boolean;
  TmpDir: TAcadTableBreakDirection;
  TmpSpacing, TmpHeight: Double;
  TmpTexts: TTableTextArray;
  TmpRows: TTableRowArray;
  TmpCols: TTableColumnArray;
  TmpCells: TTableCellArray;
  TmpMerges: TMergeRangeArray;
begin
  TmpInt := FRowCount; FRowCount := APart.RowCount; APart.RowCount := TmpInt;
  TmpInt := FColCount; FColCount := APart.ColCount; APart.ColCount := TmpInt;

  TmpVec := FRowHeights; FRowHeights := APart.RowHeights; APart.RowHeights := TmpVec;
  TmpVec := FColWidths;  FColWidths := APart.ColWidths;   APart.ColWidths := TmpVec;

  TmpTexts := FCellTexts; FCellTexts := APart.CellTexts; APart.CellTexts := TmpTexts;

  TmpFlags := FTableFlags; FTableFlags := APart.TableFlags; APart.TableFlags := TmpFlags;
  TmpStyle := FTableStyle; FTableStyle := APart.TableStyle; APart.TableStyle := TmpStyle;

  TmpRows := FRows;   FRows := APart.Rows;     APart.Rows := TmpRows;
  TmpCols := FCols;   FCols := APart.Cols;     APart.Cols := TmpCols;
  TmpCells := FCells; FCells := APart.Cells;   APart.Cells := TmpCells;
  TmpMerges := FMerges; FMerges := APart.Merges; APart.Merges := TmpMerges;

  TmpBreakEnabled := FBreakEnabled; FBreakEnabled := APart.BreakEnabled; APart.BreakEnabled := TmpBreakEnabled;
  TmpDir := FBreakDirection; FBreakDirection := APart.BreakDirection; APart.BreakDirection := TmpDir;
  TmpRepeatTop := FBreakRepeatTopLabels; FBreakRepeatTopLabels := APart.BreakRepeatTopLabels; APart.BreakRepeatTopLabels := TmpRepeatTop;
  TmpRepeatBottom := FBreakRepeatBottomLabels; FBreakRepeatBottomLabels := APart.BreakRepeatBottomLabels; APart.BreakRepeatBottomLabels := TmpRepeatBottom;
  TmpManualPos := FBreakManualPosition; FBreakManualPosition := APart.BreakManualPosition; APart.BreakManualPosition := TmpManualPos;
  TmpManualHeight := FBreakManualHeight; FBreakManualHeight := APart.BreakManualHeight; APart.BreakManualHeight := TmpManualHeight;
  TmpSpacing := FBreakSpacing; FBreakSpacing := APart.BreakSpacing; APart.BreakSpacing := TmpSpacing;
  TmpHeight := FBreakHeight; FBreakHeight := APart.BreakHeight; APart.BreakHeight := TmpHeight;
end;

// Строит визуальное представление всей таблицы: сначала главная часть
// в нулевом смещении, затем каждое продолжение со смещением, равным
// разнице точек вставки (продолжение минус главная). Все части
// помещаются в ConstObjArray одного объекта AcadTable (issue #1300).
procedure GDBObjAcadTable.BuildVisualRepresentation(
  var ADrawing: TDrawingDef; var ADC: TDrawContext);
var
  PartIdx: Integer;
  BaseX, BaseY: Double;
begin
  programlog.LogOutFormatStr(
    'AcadTable: model: BuildVisualRepresentation START ' +
    'rows=%d cols=%d parts=%d',
    [FRowCount, FColCount, Length(FContinuationParts)], LM_Info);
  ConstObjArray.Free;

  // Главная часть в собственной системе координат
  RenderCurrentTable(ADrawing, ADC, 0, 0, 0);

  // Части-продолжения со смещением относительно главной точки вставки
  for PartIdx := 0 to High(FContinuationParts) do
  begin
    BaseX := FContinuationParts[PartIdx].InsertPoint.x - FInsertPoint.x;
    BaseY := FContinuationParts[PartIdx].InsertPoint.y - FInsertPoint.y;
    SwapTableData(FContinuationParts[PartIdx]);
    // Часть-продолжение поглощается до того, как для неё вызывается
    // BuildGeometry, поэтому её FTableStyle остаётся стилем по умолчанию.
    // Применяем DXF-стиль части (тот же handle 342, что и у главной части),
    // иначе текст рендерится высотой CAcadTableDefaultTextHeight и
    // «разъезжается» относительно ячеек (issue #1300).
    uzeacadtable_stylemanager.ApplyDXFTableStyle(
      FTableStyle, FContinuationParts[PartIdx].TableStyleHandle, ADrawing);
    RenderCurrentTable(
      ADrawing, ADC, BaseX, BaseY,
      FContinuationParts[PartIdx].RowBaseIndex);
    SwapTableData(FContinuationParts[PartIdx]);
  end;

  programlog.LogOutFormatStr(
    'AcadTable: model: BuildVisualRepresentation OK ' +
    'parts=%d TotalObj=%d',
    [Length(FContinuationParts), ConstObjArray.Count], LM_Info);
end;

// Глубокое копирование данных рендеринга исходной таблицы в
// часть-продолжение. Вызывается до освобождения исходного объекта,
// поэтому все буферы копируются, а не разделяются.
procedure GDBObjAcadTable.CaptureTableDataToPart(
  var ASource: GDBObjAcadTable; var APart: TAcadTablePart);
var
  Idx, Idx2: Integer;
begin
  APart.InsertPoint := ASource.FInsertPoint;
  APart.RowBaseIndex := 0;
  APart.RowCount := ASource.FRowCount;
  APart.ColCount := ASource.FColCount;
  APart.TableFlags := ASource.FTableFlags;
  APart.TableStyle := ASource.FTableStyle;
  APart.TableStyleHandle := ASource.FTableStyleHandle;
  APart.BreakEnabled := ASource.FBreakEnabled;
  APart.BreakDirection := ASource.FBreakDirection;
  APart.BreakRepeatTopLabels := ASource.FBreakRepeatTopLabels;
  APart.BreakRepeatBottomLabels := ASource.FBreakRepeatBottomLabels;
  APart.BreakManualPosition := ASource.FBreakManualPosition;
  APart.BreakManualHeight := ASource.FBreakManualHeight;
  APart.BreakSpacing := ASource.FBreakSpacing;
  APart.BreakHeight := ASource.FBreakHeight;

  APart.RowHeights.initnul;
  for Idx := 0 to ASource.FRowHeights.Count - 1 do
    APart.RowHeights.PushBackData(ASource.FRowHeights.getData(Idx));
  APart.ColWidths.initnul;
  for Idx := 0 to ASource.FColWidths.Count - 1 do
    APart.ColWidths.PushBackData(ASource.FColWidths.getData(Idx));

  System.SetLength(APart.CellTexts, Length(ASource.FCellTexts));
  for Idx := 0 to High(ASource.FCellTexts) do
    APart.CellTexts[Idx] := ASource.FCellTexts[Idx];

  System.SetLength(APart.Rows, Length(ASource.FRows));
  for Idx := 0 to High(ASource.FRows) do
    APart.Rows[Idx] := ASource.FRows[Idx];

  System.SetLength(APart.Cols, Length(ASource.FCols));
  for Idx := 0 to High(ASource.FCols) do
    APart.Cols[Idx] := ASource.FCols[Idx];

  System.SetLength(APart.Cells, Length(ASource.FCells));
  for Idx := 0 to High(ASource.FCells) do
  begin
    System.SetLength(APart.Cells[Idx], Length(ASource.FCells[Idx]));
    for Idx2 := 0 to High(ASource.FCells[Idx]) do
      APart.Cells[Idx][Idx2] := ASource.FCells[Idx][Idx2];
  end;

  System.SetLength(APart.Merges, Length(ASource.FMerges));
  for Idx := 0 to High(ASource.FMerges) do
    APart.Merges[Idx] := ASource.FMerges[Idx];
end;

// Глубокое копирование части-продолжения (часть -> часть), для Clone.
procedure GDBObjAcadTable.CopyTablePart(
  const ASource: TAcadTablePart; var ADest: TAcadTablePart);
var
  Idx, Idx2: Integer;
begin
  ADest.InsertPoint := ASource.InsertPoint;
  ADest.RowBaseIndex := ASource.RowBaseIndex;
  ADest.RowCount := ASource.RowCount;
  ADest.ColCount := ASource.ColCount;
  ADest.TableFlags := ASource.TableFlags;
  ADest.TableStyle := ASource.TableStyle;
  ADest.TableStyleHandle := ASource.TableStyleHandle;
  ADest.BreakEnabled := ASource.BreakEnabled;
  ADest.BreakDirection := ASource.BreakDirection;
  ADest.BreakRepeatTopLabels := ASource.BreakRepeatTopLabels;
  ADest.BreakRepeatBottomLabels := ASource.BreakRepeatBottomLabels;
  ADest.BreakManualPosition := ASource.BreakManualPosition;
  ADest.BreakManualHeight := ASource.BreakManualHeight;
  ADest.BreakSpacing := ASource.BreakSpacing;
  ADest.BreakHeight := ASource.BreakHeight;

  ADest.RowHeights.initnul;
  for Idx := 0 to ASource.RowHeights.Count - 1 do
    ADest.RowHeights.PushBackData(ASource.RowHeights.getData(Idx));
  ADest.ColWidths.initnul;
  for Idx := 0 to ASource.ColWidths.Count - 1 do
    ADest.ColWidths.PushBackData(ASource.ColWidths.getData(Idx));

  System.SetLength(ADest.CellTexts, Length(ASource.CellTexts));
  for Idx := 0 to High(ASource.CellTexts) do
    ADest.CellTexts[Idx] := ASource.CellTexts[Idx];

  System.SetLength(ADest.Rows, Length(ASource.Rows));
  for Idx := 0 to High(ASource.Rows) do
    ADest.Rows[Idx] := ASource.Rows[Idx];

  System.SetLength(ADest.Cols, Length(ASource.Cols));
  for Idx := 0 to High(ASource.Cols) do
    ADest.Cols[Idx] := ASource.Cols[Idx];

  System.SetLength(ADest.Cells, Length(ASource.Cells));
  for Idx := 0 to High(ASource.Cells) do
  begin
    System.SetLength(ADest.Cells[Idx], Length(ASource.Cells[Idx]));
    for Idx2 := 0 to High(ASource.Cells[Idx]) do
      ADest.Cells[Idx][Idx2] := ASource.Cells[Idx][Idx2];
  end;

  System.SetLength(ADest.Merges, Length(ASource.Merges));
  for Idx := 0 to High(ASource.Merges) do
    ADest.Merges[Idx] := ASource.Merges[Idx];
end;

// Освобождение ресурсов части-продолжения.
procedure GDBObjAcadTable.ClearPart(var APart: TAcadTablePart);
begin
  APart.TableStyleHandle := '';
  APart.RowBaseIndex := 0;
  APart.RowHeights.done;
  APart.ColWidths.done;
  System.SetLength(APart.CellTexts, 0);
  System.SetLength(APart.Rows, 0);
  System.SetLength(APart.Cols, 0);
  System.SetLength(APart.Cells, 0, 0);
  System.SetLength(APart.Merges, 0);
end;

function GDBObjAcadTable.GetContinuationPartCount: Integer;
begin
  Result := Length(FContinuationParts);
end;

function GDBObjAcadTable.ContinuationPartRowCount(AIndex: Integer): Integer;
begin
  if (AIndex >= 0) and (AIndex <= High(FContinuationParts)) then
    Result := FContinuationParts[AIndex].RowCount
  else
    Result := -1;
end;

function GDBObjAcadTable.RecomputeBreakRepeatTopLabels: Boolean;
begin
  DetectBreakRepeatTopLabels;
  Result := FBreakRepeatTopLabels;
end;

function GDBObjAcadTable.ContinuationPartCellText(
  APartIndex, ARow, ACol: Integer): string;
var
  Idx: Integer;
begin
  Result := '';
  if (APartIndex < 0) or (APartIndex > High(FContinuationParts)) then Exit;
  with FContinuationParts[APartIndex] do
  begin
    if (ARow < 0) or (ARow >= RowCount) then Exit;
    if (ACol < 0) or (ACol >= ColCount) then Exit;
    Idx := ARow * ColCount + ACol;
    if (Idx >= 0) and (Idx <= High(CellTexts)) then
      Result := CellTexts[Idx];
  end;
end;

// Признак разрыва вычисляется по двум источникам (issue #1305, часть 2a):
//  - FBreakEnabled: флаг разрыва из одиночного DXF ACAD_TABLE;
//  - поглощённые части-продолжения: таблица, разорванная на несколько
//    ACAD_TABLE (ROUNDTRIP_2008), хранится как несколько частей одного
//    объекта, и DXF-флаг разрыва у неё не выставлен. В этом случае факт
//    разрыва определяется наличием частей-продолжений.
function GDBObjAcadTable.GetBreakEnabled: Boolean;
begin
  Result := FBreakEnabled or (Length(FContinuationParts) > 0);
end;

// Изменение признака разрыва (issue #1305, часть 2b). Установка в False
// для разорванной таблицы объединяет все части-продолжения с главной
// частью, выстраивая строки сверху вниз в единую непрерывную таблицу.
procedure GDBObjAcadTable.SetBreakEnabled(AValue: Boolean);
var
  L: Integer;
begin
  if AValue then
  begin
    FBreakEnabled := True;
    if (Length(FContinuationParts) = 0) and (FBreakHeight > 0) then
      SplitMainTableByBreakHeight(FBreakHeight)
    else
      RepositionContinuationParts;
    FGeometryBuilt := False;
    programlog.LogOutFormatStr(
      'AcadTable: model: SetBreakEnabled(True) rows=%d parts=%d',
      [FRowCount, Length(FContinuationParts)], LM_Info);
    Exit;
  end;

  // Снять разрыв: сначала удаляем повторённые верхние метки из
  // частей-продолжений, затем объединяем логические строки в главную таблицу.
  L := 0;
  if FBreakRepeatTopLabels then
    L := EffectiveRepeatTopRowCount;
  if L > 0 then
    RemoveTopLabelsFromParts(L);
  FBreakEnabled := False;
  MergeAllContinuationPartsIntoMain;

  // Геометрию нужно перестроить
  FGeometryBuilt := False;

  programlog.LogOutFormatStr(
    'AcadTable: model: SetBreakEnabled(False) merged into ' +
    'single table rows=%d cols=%d', [FRowCount, FColCount], LM_Info);
end;

// Изменение направления разбиения (issue #1315). Для уже разорванной таблицы
// смещает части-продолжения относительно главной части: Right/Left по ширине,
// Down по высоте. Для inline-разбиения без частей достаточно сбросить
// геометрию: RenderCurrentTable использует FBreakDirection напрямую.
procedure GDBObjAcadTable.SetBreakDirection(AValue: TAcadTableBreakDirection);
var
  PartIdx: Integer;
begin
  if AValue = FBreakDirection then
    Exit;

  FBreakDirection := AValue;
  for PartIdx := 0 to High(FContinuationParts) do
    FContinuationParts[PartIdx].BreakDirection := AValue;
  RepositionContinuationParts;
  FGeometryBuilt := False;

  programlog.LogOutFormatStr(
    'AcadTable: model: SetBreakDirection=%d parts=%d',
    [Ord(FBreakDirection), Length(FContinuationParts)], LM_Info);
end;

// Объединяет все части-продолжения в главную часть: строки выстраиваются
// сверху вниз в единую непрерывную таблицу. Части-продолжения
// освобождаются. Не трогает FBreakEnabled и FGeometryBuilt — это делает
// вызывающая сторона (issue #1305 часть 2b, issue #1307).
procedure GDBObjAcadTable.MergeAllContinuationPartsIntoMain;
var
  PartIdx, RowIdx, ColIdx, SrcCount: Integer;
  RowOffset: Integer;
  MergeBase: Integer;
begin
  if Length(FContinuationParts) = 0 then
    Exit;

  for PartIdx := 0 to High(FContinuationParts) do
  begin
    RowOffset := FRowCount;

    // Высоты строк добавляемой части
    for RowIdx := 0 to FContinuationParts[PartIdx].RowHeights.Count - 1 do
      FRowHeights.PushBackData(
        FContinuationParts[PartIdx].RowHeights.getData(RowIdx));

    // Тексты ячеек (тот же порядок строк, та же ширина по столбцам)
    SrcCount := Length(FContinuationParts[PartIdx].CellTexts);
    if SrcCount > 0 then
    begin
      MergeBase := Length(FCellTexts);
      System.SetLength(FCellTexts, MergeBase + SrcCount);
      for RowIdx := 0 to SrcCount - 1 do
        FCellTexts[MergeBase + RowIdx] :=
          FContinuationParts[PartIdx].CellTexts[RowIdx];
    end;

    // Формат строк
    MergeBase := Length(FRows);
    System.SetLength(FRows,
      MergeBase + Length(FContinuationParts[PartIdx].Rows));
    for RowIdx := 0 to High(FContinuationParts[PartIdx].Rows) do
      FRows[MergeBase + RowIdx] :=
        FContinuationParts[PartIdx].Rows[RowIdx];

    // Ячейки (двумерный массив [строка][столбец])
    MergeBase := Length(FCells);
    System.SetLength(FCells,
      MergeBase + Length(FContinuationParts[PartIdx].Cells));
    for RowIdx := 0 to High(FContinuationParts[PartIdx].Cells) do
    begin
      System.SetLength(FCells[MergeBase + RowIdx],
        Length(FContinuationParts[PartIdx].Cells[RowIdx]));
      for ColIdx := 0 to
          High(FContinuationParts[PartIdx].Cells[RowIdx]) do
        FCells[MergeBase + RowIdx][ColIdx] :=
          FContinuationParts[PartIdx].Cells[RowIdx][ColIdx];
    end;

    // Объединения ячеек со сдвигом номеров строк
    MergeBase := Length(FMerges);
    System.SetLength(FMerges,
      MergeBase + Length(FContinuationParts[PartIdx].Merges));
    for RowIdx := 0 to High(FContinuationParts[PartIdx].Merges) do
    begin
      FMerges[MergeBase + RowIdx] :=
        FContinuationParts[PartIdx].Merges[RowIdx];
      Inc(FMerges[MergeBase + RowIdx].Row1, RowOffset);
      Inc(FMerges[MergeBase + RowIdx].Row2, RowOffset);
    end;

    // Учитываем добавленные строки
    Inc(FRowCount, FContinuationParts[PartIdx].RowCount);
  end;

  // Освобождаем части-продолжения — таблица снова непрерывна
  for PartIdx := 0 to High(FContinuationParts) do
    ClearPart(FContinuationParts[PartIdx]);
  System.SetLength(FContinuationParts, 0);
end;

// --- Параметры разбиения: интервал и высота (issue #1307) ---

function GDBObjAcadTable.GetBreakSpacing: Double;
begin
  Result := FBreakSpacing;
end;

// Изменение интервала между частями (issue #1307, часть 1). Пересчитывает
// точки вставки всех частей-продолжений так, чтобы расстояние между
// соседними частями изменилось на чертеже.
procedure GDBObjAcadTable.SetBreakSpacing(AValue: Double);
begin
  if AValue = FBreakSpacing then
    Exit;
  FBreakSpacing := AValue;
  RepositionContinuationParts;
  FGeometryBuilt := False;
  programlog.LogOutFormatStr(
    'AcadTable: model: SetBreakSpacing=%g parts=%d',
    [FBreakSpacing, Length(FContinuationParts)], LM_Info);
end;

function GDBObjAcadTable.GetBreakHeight: Double;
begin
  Result := FBreakHeight;
end;

// Изменение высоты разбиения (issue #1307, часть 2). Сначала объединяет
// все части в единую таблицу, затем заново разбивает строки по новой
// высоте — автоматически определяя необходимое число частей.
procedure GDBObjAcadTable.SetBreakHeight(AValue: Double);
var
  L: Integer;
  WasBreakEnabled: Boolean;
begin
  if AValue = FBreakHeight then
    Exit;
  WasBreakEnabled := GetBreakEnabled;
  FBreakHeight := AValue;
  // Перебор строк имеет смысл только при положительной высоте и
  // включённом разбиении. При BreakEnabled=False параметры разбиения
  // сохраняются, но не влияют на непрерывную таблицу (issue #1313).
  if (AValue > 0) and WasBreakEnabled then
  begin
    FBreakEnabled := True;
    if FBreakRepeatTopLabels then
    begin
      L := EffectiveRepeatTopRowCount;
      if L > 0 then
        RemoveTopLabelsFromParts(L);
    end;
    MergeAllContinuationPartsIntoMain;
    SplitMainTableByBreakHeight(AValue);
  end;
  FGeometryBuilt := False;
  programlog.LogOutFormatStr(
    'AcadTable: model: SetBreakHeight=%g rows=%d parts=%d',
    [FBreakHeight, FRowCount, Length(FContinuationParts)], LM_Info);
end;

// Копирует диапазон строк [AStart..AEnd] исходной части в целевую часть.
// Столбцы, стиль и флаги копируются целиком; строки/высоты/тексты/ячейки/
// объединения вырезаются по диапазону. Точку вставки выставляет
// RepositionContinuationParts (issue #1307).
procedure GDBObjAcadTable.SlicePartFromPart(
  const ASource: TAcadTablePart;
  AStart, AEnd: Integer; var ADest: TAcadTablePart);
var
  RowIdx, ColIdx, ColCnt, SrcIdx, DstRows, TextBase: Integer;
begin
  DstRows := AEnd - AStart + 1;
  if DstRows < 0 then DstRows := 0;
  ColCnt := ASource.ColCount;

  ADest.InsertPoint := ASource.InsertPoint;
  ADest.RowBaseIndex := ASource.RowBaseIndex + AStart;
  ADest.RowCount := DstRows;
  ADest.ColCount := ColCnt;
  ADest.TableFlags := ASource.TableFlags;
  ADest.TableStyle := ASource.TableStyle;
  ADest.TableStyleHandle := ASource.TableStyleHandle;
  ADest.BreakEnabled := ASource.BreakEnabled;
  ADest.BreakDirection := ASource.BreakDirection;
  ADest.BreakRepeatTopLabels := ASource.BreakRepeatTopLabels;
  ADest.BreakRepeatBottomLabels := ASource.BreakRepeatBottomLabels;
  ADest.BreakManualPosition := ASource.BreakManualPosition;
  ADest.BreakManualHeight := ASource.BreakManualHeight;
  ADest.BreakSpacing := ASource.BreakSpacing;
  ADest.BreakHeight := ASource.BreakHeight;

  // Высоты строк диапазона
  ADest.RowHeights.initnul;
  for RowIdx := AStart to AEnd do
    if (RowIdx >= 0) and (RowIdx < ASource.RowHeights.Count) then
      ADest.RowHeights.PushBackData(ASource.RowHeights.getData(RowIdx));

  // Ширины столбцов — те же, что у источника
  ADest.ColWidths.initnul;
  for ColIdx := 0 to ASource.ColWidths.Count - 1 do
    ADest.ColWidths.PushBackData(ASource.ColWidths.getData(ColIdx));

  // Тексты ячеек (плоский массив, индекс = строка * ColCount + столбец)
  System.SetLength(ADest.CellTexts, DstRows * ColCnt);
  if ColCnt > 0 then
    for RowIdx := 0 to DstRows - 1 do
      for ColIdx := 0 to ColCnt - 1 do
      begin
        SrcIdx := (AStart + RowIdx) * ColCnt + ColIdx;
        TextBase := RowIdx * ColCnt + ColIdx;
        if (SrcIdx >= 0) and (SrcIdx <= High(ASource.CellTexts)) then
          ADest.CellTexts[TextBase] := ASource.CellTexts[SrcIdx]
        else
          ADest.CellTexts[TextBase] := '';
      end;

  // Формат строк
  System.SetLength(ADest.Rows, DstRows);
  for RowIdx := 0 to DstRows - 1 do
    if (AStart + RowIdx) <= High(ASource.Rows) then
      ADest.Rows[RowIdx] := ASource.Rows[AStart + RowIdx];

  // Столбцы — те же, что у источника
  System.SetLength(ADest.Cols, Length(ASource.Cols));
  for ColIdx := 0 to High(ASource.Cols) do
    ADest.Cols[ColIdx] := ASource.Cols[ColIdx];

  // Ячейки (двумерный массив [строка][столбец])
  System.SetLength(ADest.Cells, DstRows);
  for RowIdx := 0 to DstRows - 1 do
    if (AStart + RowIdx) <= High(ASource.Cells) then
    begin
      System.SetLength(ADest.Cells[RowIdx],
        Length(ASource.Cells[AStart + RowIdx]));
      for ColIdx := 0 to High(ASource.Cells[AStart + RowIdx]) do
        ADest.Cells[RowIdx][ColIdx] :=
          ASource.Cells[AStart + RowIdx][ColIdx];
    end;

  // Объединения ячеек, попадающие целиком в диапазон, со сдвигом строк
  System.SetLength(ADest.Merges, 0);
  for RowIdx := 0 to High(ASource.Merges) do
    if (ASource.Merges[RowIdx].Row1 >= AStart) and
       (ASource.Merges[RowIdx].Row2 <= AEnd) then
    begin
      ColIdx := Length(ADest.Merges);
      System.SetLength(ADest.Merges, ColIdx + 1);
      ADest.Merges[ColIdx] := ASource.Merges[RowIdx];
      Dec(ADest.Merges[ColIdx].Row1, AStart);
      Dec(ADest.Merges[ColIdx].Row2, AStart);
    end;
end;

// Пересегментирует объединённую главную часть по высоте разбиения:
// строки набираются в сегмент, пока их суммарная высота не превысит
// порог AThreshold; затем начинается новый сегмент. Первый сегмент
// остаётся в главной части, остальные выносятся в части-продолжения.
// Если BreakRepeatTopLabels=True, верхние метки не считаются отдельными
// строками логической таблицы в продолжениях: они добавляются поверх каждого
// продолжения и уменьшают доступную высоту сегмента (issue #1311).
// Число частей определяется автоматически (issue #1307).
procedure GDBObjAcadTable.SplitMainTableByBreakHeight(AThreshold: Double);
var
  FullData, TmpPart: TAcadTablePart;
  SegStart, SegEnd: array of Integer;
  SegCount, StartRow, EndRow, PartIdx, RepeatRows, RowIdx: Integer;
  CurHeight, NextHeight, RepeatHeight: Double;
begin
  if (FRowCount <= 0) or (AThreshold <= 0) then
    Exit;

  // Снимок текущей (объединённой) главной части
  CaptureTableDataToPart(Self, FullData);
  try
    RepeatRows := 0;
    RepeatHeight := 0;
    if FBreakRepeatTopLabels then
    begin
      RepeatRows := ComputeTopLabelRowCount;
      if RepeatRows > FullData.RowCount then
        RepeatRows := FullData.RowCount;
      for RowIdx := 0 to RepeatRows - 1 do
        RepeatHeight := RepeatHeight +
          uzeacadtable_layout.GetRowHeight(RowIdx, FullData.RowHeights);
    end;

    // Вычисляем границы сегментов по высоте строк
    SegCount := 0;
    StartRow := 0;
    while StartRow < FullData.RowCount do
    begin
      EndRow := StartRow;
      CurHeight := 0;
      if (SegCount > 0) and (RepeatRows > 0) then
        CurHeight := RepeatHeight;
      while EndRow < FullData.RowCount do
      begin
        NextHeight := CurHeight +
          uzeacadtable_layout.GetRowHeight(EndRow, FullData.RowHeights);
        if (EndRow > StartRow) and
           (NextHeight > AThreshold + 1e-9) then
          Break;
        CurHeight := NextHeight;
        Inc(EndRow);
      end;
      if EndRow = StartRow then
        Inc(EndRow);
      if (SegCount = 0) and (RepeatRows > 0) and
         (EndRow < RepeatRows) then
        EndRow := RepeatRows;

      System.SetLength(SegStart, SegCount + 1);
      System.SetLength(SegEnd, SegCount + 1);
      SegStart[SegCount] := StartRow;
      SegEnd[SegCount] := EndRow - 1;
      Inc(SegCount);
      StartRow := EndRow;
      if (RepeatRows > 0) and (StartRow < RepeatRows) then
        StartRow := RepeatRows;
    end;

    // Части-продолжения для сегментов 1..SegCount-1
    for PartIdx := 0 to High(FContinuationParts) do
      ClearPart(FContinuationParts[PartIdx]);
    System.SetLength(FContinuationParts, SegCount - 1);
    for PartIdx := 1 to SegCount - 1 do
    begin
      SlicePartFromPart(FullData, SegStart[PartIdx], SegEnd[PartIdx],
        FContinuationParts[PartIdx - 1]);
      if RepeatRows > 0 then
        PrependTopLabelsToPart(RepeatRows, FContinuationParts[PartIdx - 1]);
    end;

    // Сегмент 0 -> главная часть. Готовим временную часть и меняемся
    // данными с собой (точка вставки главной части сохраняется).
    SlicePartFromPart(FullData, SegStart[0], SegEnd[0], TmpPart);
    SwapTableData(TmpPart);
    ClearPart(TmpPart);

    RepositionContinuationParts;
  finally
    ClearPart(FullData);
  end;
end;

// Пересчитывает точки вставки частей-продолжений из текущего интервала
// (FBreakSpacing) и направления разрыва (FBreakDirection). Расстояние
// между соседними частями = (размер предыдущего сегмента вдоль оси
// разрыва) + интервал (issue #1307).
procedure GDBObjAcadTable.RepositionContinuationParts;
var
  PartIdx: Integer;
  Horizontal: Boolean;
  CumOffset, PrevExtent: Double;
begin
  if Length(FContinuationParts) = 0 then
    Exit;

  Horizontal := FBreakDirection in [atbdRight, atbdLeft];
  if Horizontal then
    PrevExtent := GetTotalWidth
  else
    PrevExtent := GetTotalHeight;

  CumOffset := 0;
  for PartIdx := 0 to High(FContinuationParts) do
  begin
    CumOffset := CumOffset + PrevExtent + FBreakSpacing;

    FContinuationParts[PartIdx].InsertPoint := FInsertPoint;
    case FBreakDirection of
      atbdDown:
        FContinuationParts[PartIdx].InsertPoint.y :=
          FInsertPoint.y - CumOffset;
      atbdLeft:
        FContinuationParts[PartIdx].InsertPoint.x :=
          FInsertPoint.x - CumOffset;
    else // atbdRight
      FContinuationParts[PartIdx].InsertPoint.x :=
        FInsertPoint.x + CumOffset;
    end;

    if Horizontal then
      PrevExtent := uzeacadtable_layout.GetTotalWidth(
        FContinuationParts[PartIdx].ColCount,
        FContinuationParts[PartIdx].ColWidths)
    else
      PrevExtent := uzeacadtable_layout.GetTotalHeight(
        FContinuationParts[PartIdx].RowCount,
        FContinuationParts[PartIdx].RowHeights);
  end;
end;

// --- Повтор верхних строк-меток в частях разорванной таблицы (issue #1309) ---

// Максимальное число ведущих строк-меток (Title/Header), которые могут
// повторяться в начале каждой части разорванной таблицы. Тип строки в таблице
// AutoCAD определяется её индексом: строка 0 — Title, строка 1 — Header,
// строки >=2 — Data. Зона повтора — это ведущие строки Title/Header от начала
// таблицы до первой строки Data, то есть не более двух строк (issue #1309).
//
// Прежняя реализация опиралась на биты подавления TableFlags (бит 2 — Title,
// бит 4 — Header), но реальные DXF (например, test/tablerazdel.dxf) приходят с
// TableFlags=22, где эти биты установлены, хотя строки Title и Header реально
// присутствуют и повторяются в каждой части. Поэтому признак повтора
// определяется не флагами, а сравнением содержимого строк частей с главной
// частью (см. EffectiveRepeatTopRowCount), а здесь возвращается лишь верхняя
// граница зоны меток по индексу строки. Возвращает 0, 1 или 2.
function GDBObjAcadTable.ComputeTopLabelRowCount: Integer;
begin
  Result := 0;
  if FRowCount > 0 then
    Inc(Result);
  if FRowCount > 1 then
    Inc(Result);
end;

// Фактическое число ведущих строк-меток, которые ОДИНАКОВО повторяются в начале
// КАЖДОЙ части-продолжения. Ищется наибольшее L в диапазоне
// [1..ComputeTopLabelRowCount], при котором все части повторяют первые L строк
// главной части. Возвращает 0, если частей нет или повтора нет. Подход опирается
// на содержимое (issue #1309), а не на флаги подавления заголовков.
function GDBObjAcadTable.EffectiveRepeatTopRowCount: Integer;
var
  L, MaxL, PartIdx: Integer;
  AllRepeat: Boolean;
begin
  Result := 0;
  if Length(FContinuationParts) = 0 then
    Exit;
  MaxL := ComputeTopLabelRowCount;
  for L := MaxL downto 1 do
  begin
    AllRepeat := True;
    for PartIdx := 0 to High(FContinuationParts) do
      if not PartRepeatsTopLabels(FContinuationParts[PartIdx], L) then
      begin
        AllRepeat := False;
        Break;
      end;
    if AllRepeat then
    begin
      Result := L;
      Exit;
    end;
  end;
end;

// Проверяет, повторяет ли часть-продолжение первые L строк-меток главной
// части: число столбцов должно совпадать, у части должно быть не меньше L
// строк, а тексты ячеек первых L строк должны быть идентичны главной части.
function GDBObjAcadTable.PartRepeatsTopLabels(
  const APart: TAcadTablePart; L: Integer): Boolean;
var
  RowIdx, ColIdx, MainIdx, PartTextIdx: Integer;
begin
  Result := False;
  if L <= 0 then Exit;
  if APart.ColCount <> FColCount then Exit;
  if APart.RowCount < L then Exit;
  for RowIdx := 0 to L - 1 do
    for ColIdx := 0 to FColCount - 1 do
    begin
      MainIdx := RowIdx * FColCount + ColIdx;
      PartTextIdx := RowIdx * APart.ColCount + ColIdx;
      if (MainIdx > High(FCellTexts)) or
         (PartTextIdx > High(APart.CellTexts)) then
        Exit;
      if FCellTexts[MainIdx] <> APart.CellTexts[PartTextIdx] then
        Exit;
    end;
  Result := True;
end;

// Обновляет логический индекс первой строки продолжений после чтения DXF или
// автоопределения RepeatTop. Если верхние метки повторяются, визуальные строки
// части начинаются с Title/Header и базовый стиль должен идти от нуля; иначе
// первая визуальная строка продолжения получает стиль по логической позиции.
procedure GDBObjAcadTable.UpdateContinuationRowBaseIndexes;
var
  PartIdx, BaseIdx, RepeatRows, LogicalRows: Integer;
begin
  BaseIdx := FRowCount;
  RepeatRows := 0;
  if FBreakRepeatTopLabels then
    RepeatRows := EffectiveRepeatTopRowCount;
  for PartIdx := 0 to High(FContinuationParts) do
  begin
    if (RepeatRows > 0) and
       PartRepeatsTopLabels(FContinuationParts[PartIdx], RepeatRows) then
    begin
      FContinuationParts[PartIdx].RowBaseIndex := 0;
      LogicalRows := FContinuationParts[PartIdx].RowCount - RepeatRows;
    end
    else
    begin
      FContinuationParts[PartIdx].RowBaseIndex := BaseIdx;
      LogicalRows := FContinuationParts[PartIdx].RowCount;
    end;
    if LogicalRows > 0 then
      Inc(BaseIdx, LogicalRows);
  end;
end;

// Определяет признак повтора верхних меток по данным частей-продолжений.
// Без частей сохраняется значение, прочитанное из DXF. При наличии частей
// признак равен True тогда и только тогда, когда КАЖДАЯ часть повторяет
// ведущие строки-метки главной части (issue #1309). Решение принимается по
// содержимому строк, а не по флагам подавления заголовков.
procedure GDBObjAcadTable.DetectBreakRepeatTopLabels;
begin
  if Length(FContinuationParts) = 0 then
    Exit;
  FBreakRepeatTopLabels := EffectiveRepeatTopRowCount > 0;
  UpdateContinuationRowBaseIndexes;
end;

function GDBObjAcadTable.GetBreakRepeatTopLabels: Boolean;
begin
  Result := FBreakRepeatTopLabels;
end;

// Изменение признака повтора верхних меток (issue #1309). Установка в False
// удаляет повторяющиеся ведущие строки-метки из всех частей-продолжений;
// установка в True — добавляет их обратно. Точки вставки частей и геометрия
// пересчитываются.
procedure GDBObjAcadTable.SetBreakRepeatTopLabels(AValue: Boolean);
var
  L: Integer;
begin
  if AValue = FBreakRepeatTopLabels then
    Exit;
  L := 0;
  if Length(FContinuationParts) > 0 then
  begin
    if AValue then
    begin
      // Добавляем ведущие строки-метки главной части в каждую часть.
      L := ComputeTopLabelRowCount;
      FBreakRepeatTopLabels := True;
      if FBreakHeight > 0 then
      begin
        MergeAllContinuationPartsIntoMain;
        SplitMainTableByBreakHeight(FBreakHeight);
      end
      else if L > 0 then
        AddTopLabelsToParts(L);
    end
    else
    begin
      // Удаляем ровно те ведущие строки, которые сейчас реально повторяются.
      L := EffectiveRepeatTopRowCount;
      if L > 0 then
        RemoveTopLabelsFromParts(L);
      FBreakRepeatTopLabels := False;
      if FBreakHeight > 0 then
      begin
        MergeAllContinuationPartsIntoMain;
        SplitMainTableByBreakHeight(FBreakHeight);
      end;
    end;
    if (FBreakHeight <= 0) and (L > 0) then
    begin
      RepositionContinuationParts;
    end;
    FGeometryBuilt := False;
  end
  else
    FBreakRepeatTopLabels := AValue;
  programlog.LogOutFormatStr(
    'AcadTable: model: SetBreakRepeatTopLabels=%d L=%d parts=%d',
    [Ord(AValue), L, Length(FContinuationParts)], LM_Info);
end;

// Удаляет первые L строк-меток из каждой части-продолжения, которая их
// действительно повторяет. Часть пересобирается из своего диапазона строк
// [L..RowCount-1] через SlicePartFromPart.
procedure GDBObjAcadTable.RemoveTopLabelsFromParts(L: Integer);
var
  PartIdx, Last: Integer;
  Snap: TAcadTablePart;
begin
  if L <= 0 then Exit;
  for PartIdx := 0 to High(FContinuationParts) do
  begin
    if not PartRepeatsTopLabels(FContinuationParts[PartIdx], L) then
      Continue;
    Last := FContinuationParts[PartIdx].RowCount - 1;
    // Независимый снимок части, затем пересоздание из диапазона без меток.
    CopyTablePart(FContinuationParts[PartIdx], Snap);
    ClearPart(FContinuationParts[PartIdx]);
    SlicePartFromPart(Snap, L, Last, FContinuationParts[PartIdx]);
    ClearPart(Snap);
  end;
end;

// Добавляет первые L строк-меток главной части в начало каждой
// части-продолжения, в которой их ещё нет.
procedure GDBObjAcadTable.AddTopLabelsToParts(L: Integer);
var
  PartIdx: Integer;
begin
  if L <= 0 then Exit;
  for PartIdx := 0 to High(FContinuationParts) do
    if not PartRepeatsTopLabels(FContinuationParts[PartIdx], L) then
      PrependTopLabelsToPart(L, FContinuationParts[PartIdx]);
end;

// Вставляет первые L строк-меток главной части в начало части-продолжения.
// Часть пересобирается: сверху — строки-метки главной части (0..L-1), затем
// собственные строки части. Высоты, тексты, формат строк, ячейки и
// объединения переносятся со сдвигом номеров строк на L.
procedure GDBObjAcadTable.PrependTopLabelsToPart(
  L: Integer; var APart: TAcadTablePart);
var
  Src: TAcadTablePart;
  ColCnt, DstRows, RowIdx, ColIdx, Idx, SrcRow: Integer;
begin
  if L <= 0 then Exit;
  ColCnt := FColCount;
  // Независимый снимок части без меток, затем пересоздание части с нуля.
  CopyTablePart(APart, Src);
  ClearPart(APart);

  DstRows := L + Src.RowCount;
  APart.InsertPoint := Src.InsertPoint;
  APart.RowBaseIndex := 0;
  APart.RowCount := DstRows;
  APart.ColCount := ColCnt;
  APart.TableFlags := Src.TableFlags;
  APart.TableStyle := Src.TableStyle;
  APart.TableStyleHandle := Src.TableStyleHandle;
  APart.BreakEnabled := Src.BreakEnabled;
  APart.BreakDirection := Src.BreakDirection;
  APart.BreakRepeatTopLabels := Src.BreakRepeatTopLabels;
  APart.BreakRepeatBottomLabels := Src.BreakRepeatBottomLabels;
  APart.BreakManualPosition := Src.BreakManualPosition;
  APart.BreakManualHeight := Src.BreakManualHeight;
  APart.BreakSpacing := Src.BreakSpacing;
  APart.BreakHeight := Src.BreakHeight;

  // Высоты строк: сверху L строк главной части, затем строки исходной части.
  APart.RowHeights.initnul;
  for RowIdx := 0 to L - 1 do
    APart.RowHeights.PushBackData(GetRowHeightLocal(RowIdx));
  for RowIdx := 0 to Src.RowHeights.Count - 1 do
    APart.RowHeights.PushBackData(Src.RowHeights.getData(RowIdx));

  // Ширины столбцов — те же, что у части.
  APart.ColWidths.initnul;
  for ColIdx := 0 to Src.ColWidths.Count - 1 do
    APart.ColWidths.PushBackData(Src.ColWidths.getData(ColIdx));

  // Тексты ячеек (плоский массив, индекс = строка * ColCnt + столбец).
  System.SetLength(APart.CellTexts, DstRows * ColCnt);
  if ColCnt > 0 then
    for RowIdx := 0 to DstRows - 1 do
      for ColIdx := 0 to ColCnt - 1 do
      begin
        Idx := RowIdx * ColCnt + ColIdx;
        if RowIdx < L then
        begin
          if (RowIdx * FColCount + ColIdx) <= High(FCellTexts) then
            APart.CellTexts[Idx] := FCellTexts[RowIdx * FColCount + ColIdx]
          else
            APart.CellTexts[Idx] := '';
        end
        else
        begin
          SrcRow := RowIdx - L;
          if (SrcRow * ColCnt + ColIdx) <= High(Src.CellTexts) then
            APart.CellTexts[Idx] := Src.CellTexts[SrcRow * ColCnt + ColIdx]
          else
            APart.CellTexts[Idx] := '';
        end;
      end;

  // Формат строк.
  System.SetLength(APart.Rows, DstRows);
  for RowIdx := 0 to L - 1 do
    if RowIdx <= High(FRows) then
      APart.Rows[RowIdx] := FRows[RowIdx];
  for RowIdx := 0 to Src.RowCount - 1 do
    if RowIdx <= High(Src.Rows) then
      APart.Rows[L + RowIdx] := Src.Rows[RowIdx];

  // Столбцы — те же, что у части.
  System.SetLength(APart.Cols, Length(Src.Cols));
  for ColIdx := 0 to High(Src.Cols) do
    APart.Cols[ColIdx] := Src.Cols[ColIdx];

  // Ячейки (двумерный массив [строка][столбец]).
  System.SetLength(APart.Cells, DstRows);
  for RowIdx := 0 to L - 1 do
    if RowIdx <= High(FCells) then
    begin
      System.SetLength(APart.Cells[RowIdx], Length(FCells[RowIdx]));
      for ColIdx := 0 to High(FCells[RowIdx]) do
        APart.Cells[RowIdx][ColIdx] := FCells[RowIdx][ColIdx];
    end;
  for RowIdx := 0 to Src.RowCount - 1 do
    if RowIdx <= High(Src.Cells) then
    begin
      System.SetLength(APart.Cells[L + RowIdx], Length(Src.Cells[RowIdx]));
      for ColIdx := 0 to High(Src.Cells[RowIdx]) do
        APart.Cells[L + RowIdx][ColIdx] := Src.Cells[RowIdx][ColIdx];
    end;

  // Объединения: сначала объединения главной части, попадающие в строки
  // [0..L-1], затем объединения исходной части со сдвигом строк на L.
  System.SetLength(APart.Merges, 0);
  for RowIdx := 0 to High(FMerges) do
    if (FMerges[RowIdx].Row1 >= 0) and (FMerges[RowIdx].Row2 <= L - 1) then
    begin
      Idx := Length(APart.Merges);
      System.SetLength(APart.Merges, Idx + 1);
      APart.Merges[Idx] := FMerges[RowIdx];
    end;
  for RowIdx := 0 to High(Src.Merges) do
  begin
    Idx := Length(APart.Merges);
    System.SetLength(APart.Merges, Idx + 1);
    APart.Merges[Idx] := Src.Merges[RowIdx];
    Inc(APart.Merges[Idx].Row1, L);
    Inc(APart.Merges[Idx].Row2, L);
  end;

  ClearPart(Src);
end;

// Поглощает продолжение разделённой таблицы как часть этого объекта.
function GDBObjAcadTable.TryMergeContinuation(
  AOther: PGDBObjEntity): Boolean;
var
  PartIdx: Integer;
begin
  Result := False;
  if AOther = nil then Exit;
  if AOther^.GetObjType <> GDBAcadTableID then Exit;

  PartIdx := Length(FContinuationParts);
  System.SetLength(FContinuationParts, PartIdx + 1);
  CaptureTableDataToPart(
    PGDBObjAcadTable(AOther)^, FContinuationParts[PartIdx]);
  // Геометрию нужно перестроить с учётом новой части
  FGeometryBuilt := False;

  // По данным частей определяем, повторяются ли верхние строки-метки
  // (issue #1309): если первые строки каждой части совпадают с верхними
  // строками-метками главной части, то таблица разорвана с RepeatTop=True.
  DetectBreakRepeatTopLabels;

  programlog.LogOutFormatStr(
    'AcadTable: model: TryMergeContinuation merged part %d ' +
    '(rows=%d cols=%d) repeattop=%d',
    [PartIdx, FContinuationParts[PartIdx].RowCount,
     FContinuationParts[PartIdx].ColCount,
     Ord(FBreakRepeatTopLabels)], LM_Info);
  Result := True;
end;

// Сохраняет параметры разбиения, прочитанные загрузчиком DXF из XRECORD
// ACAD_ROUNDTRIP_2008_TABLE_ENTITY (issue #1307). Значения только
// сохраняются — геометрия частей уже расставлена по точкам вставки из DXF,
// перестройка не требуется.
function GDBObjAcadTable.SetTableBreakData(
  ASpacing, ABreakHeight: Double): Boolean;
begin
  FBreakSpacing := ASpacing;
  FBreakHeight := ABreakHeight;
  programlog.LogOutFormatStr(
    'AcadTable: model: SetTableBreakData spacing=%g breakheight=%g',
    [FBreakSpacing, FBreakHeight], LM_Info);
  Result := True;
end;

// --- Трансформация объекта (issue #1305, часть 1) ---
// Таблица AcadTable реализует ту же модель трансформации, что и вставка
// блока (uzeentblockinsert): масштаб и поворот хранятся отдельными полями
// (FScale/FRotate) и восстанавливаются из objmatrix, чтобы перенос,
// поворот и масштабирование объекта корректно отображались.

// Раскладывает objmatrix на точку вставки, базис и масштаб.
procedure GDBObjAcadTable.decomposite;
var
  BX, BY, BZ, T: TzePoint3d;
  Mtr: TzeTypedMatrix4d;
begin
  Mtr := objMatrix;
  BX := PzePoint3d(@Mtr.mtr.v[0])^;
  BY := PzePoint3d(@Mtr.mtr.v[1])^;
  BZ := PzePoint3d(@Mtr.mtr.v[2])^;
  T := PzePoint3d(@Mtr.mtr.v[3])^;
  Local := GetPointInOCSByBasis(BX, BY, BZ, T, FScale);
end;

// Поворачивает objmatrix вокруг оси Z на угол r.
procedure GDBObjAcadTable.setrot(r: Double);
var
  m1: TzeTypedMatrix4d;
begin
  m1 := CreateRotationMatrixZ(r);
  objMatrix := MatrixMultiply(m1, objMatrix);
end;

// Восстанавливает точку вставки, масштаб и угол поворота из objmatrix
// после трансформации (перенос/поворот/масштабирование).
procedure GDBObjAcadTable.ReCalcFromObjMatrix;
var
  ox: TzePoint3d;
  tv: TzePoint3d;
begin
  inherited;
  decomposite;
  ox := GetXfFromZ(Local.basis.oz);
  tv := Local.basis.ox;
  if FScale.x < -eps then
    tv := VertexMulOnSc(tv, -1);
  FRotate := scalardot(tv, ox);
  FRotate := arccos(FRotate);
  if scalardot(tv, VectorDot(Local.basis.oz,
       GetXfFromZ(Local.basis.oz))) < -eps then
    FRotate := 2 * pi - FRotate;
end;

// Строит objmatrix с учётом точки вставки, поворота и масштаба.
procedure GDBObjAcadTable.CalcObjMatrix(pdrawing: PTDrawingDef = nil);
var
  m1: TzeTypedMatrix4d;
begin
  inherited CalcObjMatrix(pdrawing);
  setrot(FRotate);
  m1 := CreateScaleMatrix(FScale);
  objMatrix := MatrixMultiply(m1, objMatrix);
end;

// Сохраняет масштаб и поворот в опорный объект (real-time трансформация).
procedure GDBObjAcadTable.rtsave(refp: Pointer);
begin
  inherited;
  PGDBObjAcadTable(refp)^.FRotate := FRotate;
  PGDBObjAcadTable(refp)^.FScale := FScale;
end;

// Вычисляет bounding box
procedure GDBObjAcadTable.getoutbound(var DC: TDrawContext);
var
  TotalWidthVal, TotalHeightVal: Double;
  MinX, MinY, MaxX, MaxY: Double;
  PartIdx: Integer;
  BaseX, BaseY, PartW, PartH: Double;
  PartMinX, PartMaxX, PartMinY, PartMaxY: Double;
begin
  // Если геометрия уже построена, берём bounding box из дочерних
  // объектов: их WCS-координаты учитывают перенос/поворот/масштаб-
  // ирование objmatrix, поэтому рамка следует за трансформацией
  // (issue #1305, часть 1).
  if ConstObjArray.Count > 0 then
  begin
    vp.BoundingBox := ConstObjArray.getoutbound(DC);
    Exit;
  end;

  if (FRowCount <= 0) or (FColCount <= 0) then
  begin
    vp.BoundingBox.LBN :=
      VertexAdd(Local.P_insert,
        CreateVertex(-0.01, -0.01, 0));
    vp.BoundingBox.RTF :=
      VertexAdd(Local.P_insert,
        CreateVertex(0.01, 0.01, 0));
    Exit;
  end;

  TotalWidthVal := GetTotalWidth;
  TotalHeightVal := GetTotalHeight;
  MinX := Local.P_insert.x;
  MaxX := Local.P_insert.x + TotalWidthVal;
  MinY := Local.P_insert.y - TotalHeightVal;
  MaxY := Local.P_insert.y;

  // Расширяем bounding box на все части-продолжения (issue #1300)
  for PartIdx := 0 to High(FContinuationParts) do
  begin
    BaseX := FContinuationParts[PartIdx].InsertPoint.x - FInsertPoint.x;
    BaseY := FContinuationParts[PartIdx].InsertPoint.y - FInsertPoint.y;
    PartW := uzeacadtable_layout.GetTotalWidth(
      FContinuationParts[PartIdx].ColCount,
      FContinuationParts[PartIdx].ColWidths);
    PartH := uzeacadtable_layout.GetTotalHeight(
      FContinuationParts[PartIdx].RowCount,
      FContinuationParts[PartIdx].RowHeights);
    PartMinX := Local.P_insert.x + BaseX;
    PartMaxX := PartMinX + PartW;
    PartMaxY := Local.P_insert.y + BaseY;
    PartMinY := PartMaxY - PartH;
    if PartMinX < MinX then MinX := PartMinX;
    if PartMaxX > MaxX then MaxX := PartMaxX;
    if PartMinY < MinY then MinY := PartMinY;
    if PartMaxY > MaxY then MaxY := PartMaxY;
  end;

  vp.BoundingBox.LBN :=
    CreateVertex(MinX, MinY, Local.P_insert.z);
  vp.BoundingBox.RTF :=
    CreateVertex(MaxX, MaxY, Local.P_insert.z);
end;

// --- Методы сущности ---

procedure GDBObjAcadTable.BuildGeometry(
  var ADrawing: TDrawingDef);
var
  DC: TDrawContext;
begin
  programlog.LogOutFormatStr(
    'AcadTable: model: BuildGeometry START built=%d ' +
    'rows=%d cols=%d',
    [Ord(FGeometryBuilt), FRowCount, FColCount], LM_Info);

  if not FGeometryBuilt then
  begin
    // Применяем DXF-стиль таблицы
    uzeacadtable_stylemanager.ApplyDXFTableStyle(
      FTableStyle, FTableStyleHandle, ADrawing);

    DC := ADrawing.CreateDrawingRC;
    BuildVisualRepresentation(ADrawing, DC);
    FGeometryBuilt := True;
    inherited BuildGeometry(ADrawing);
  end;

  programlog.LogOutStr(
    'AcadTable: model: BuildGeometry END', LM_Info);
end;

procedure GDBObjAcadTable.FormatEntity(
  var ADrawing: TDrawingDef; var ADC: TDrawContext;
  AStage: TEFStages = EFAllStages);
begin
  if EFCalcEntityCS in AStage then
  begin
    if Assigned(EntExtensions) then
      EntExtensions.RunOnBeforeEntityFormat(
        @Self, ADrawing, ADC);
    CalcObjMatrix;
  end;
  // Раскладка таблицы в OCS строится один раз и защищена флагом
  // FGeometryBuilt. Сами дочерние объекты (линии и тексты) каждый
  // кадр переформатируются ниже, чтобы перенос/поворот/масштаб-
  // ирование объекта отображались — дочерние объекты пересчитывают
  // свои WCS-координаты из objmatrix владельца (issue #1305, часть 1).
  BuildGeometry(ADrawing);
  ConstObjArray.FormatEntity(ADrawing, ADC, AStage);
  if EFCalcEntityCS in AStage then
  begin
    getoutbound(ADC);
    // Перестраиваем пространственное дерево по новым координатам
    // дочерних объектов, иначе перерисовки на месте не происходит.
    inherited BuildGeometry(ADrawing);
  end;
  CalcActualVisible(ADC.DrawingContext.VActuality);
  if EFDraw in AStage then
  begin
    if Assigned(EntExtensions) then
      EntExtensions.RunOnAfterEntityFormat(
        @Self, ADrawing, ADC);
  end;
end;

function GDBObjAcadTable.IsStagedFormatEntity: Boolean;
begin
  Result := True;
end;

function GDBObjAcadTable.DXFDelayedBuildGeometry: Boolean;
begin
  Result := True;
end;

procedure GDBObjAcadTable.SaveToDXF(
  var AOutStream: TZctnrVectorBytes;
  var ADrawing: TDrawingDef;
  var AIODXFContext: TIODXFSaveContext);
begin
  uzeacadtable_dxf_write.WriteAcadTableToDXF(
    AOutStream, ADrawing, AIODXFContext);
end;

procedure GDBObjAcadTable.SaveToDXFFollow(
  var AOutStream: TZctnrVectorBytes;
  var ADrawing: TDrawingDef;
  var AIODXFContext: TIODXFSaveContext);
begin
  // Пустая реализация
end;

function GDBObjAcadTable.Clone(
  AOwn: Pointer): PGDBObjEntity;
var
  NewTable: PGDBObjAcadTable;
  Idx, Idx2: Integer;
begin
  GetMem(Pointer(NewTable), SizeOf(GDBObjAcadTable));
  NewTable^.initnul(AOwn);
  CopyVPto(NewTable^);
  CopyExtensionsTo(NewTable^);

  NewTable^.FInsertPoint := FInsertPoint;
  NewTable^.FRowCount := FRowCount;
  NewTable^.FColCount := FColCount;
  NewTable^.FTableStyleHandle := FTableStyleHandle;
  NewTable^.FTableFlags := FTableFlags;
  NewTable^.FBreakEnabled := FBreakEnabled;
  NewTable^.FBreakDirection := FBreakDirection;
  NewTable^.FBreakRepeatTopLabels := FBreakRepeatTopLabels;
  NewTable^.FBreakRepeatBottomLabels :=
    FBreakRepeatBottomLabels;
  NewTable^.FBreakManualPosition := FBreakManualPosition;
  NewTable^.FBreakManualHeight := FBreakManualHeight;
  NewTable^.FBreakSpacing := FBreakSpacing;
  NewTable^.FBreakHeight := FBreakHeight;
  // Трансформация объекта (issue #1305, часть 1)
  NewTable^.FScale := FScale;
  NewTable^.FRotate := FRotate;

  for Idx := 0 to FRowHeights.Count - 1 do
    NewTable^.FRowHeights.PushBackData(
      FRowHeights.getData(Idx));
  for Idx := 0 to FColWidths.Count - 1 do
    NewTable^.FColWidths.PushBackData(
      FColWidths.getData(Idx));

  System.SetLength(NewTable^.FCellTexts,
    Length(FCellTexts));
  for Idx := 0 to High(FCellTexts) do
    NewTable^.FCellTexts[Idx] := FCellTexts[Idx];

  NewTable^.FTableStyle := FTableStyle;

  System.SetLength(NewTable^.FRows, Length(FRows));
  for Idx := 0 to High(FRows) do
    NewTable^.FRows[Idx] := FRows[Idx];

  System.SetLength(NewTable^.FCols, Length(FCols));
  for Idx := 0 to High(FCols) do
    NewTable^.FCols[Idx] := FCols[Idx];

  System.SetLength(NewTable^.FCells, Length(FCells));
  for Idx := 0 to High(FCells) do
  begin
    System.SetLength(NewTable^.FCells[Idx],
      Length(FCells[Idx]));
    for Idx2 := 0 to High(FCells[Idx]) do
      NewTable^.FCells[Idx][Idx2] :=
        FCells[Idx][Idx2];
  end;

  System.SetLength(NewTable^.FMerges, Length(FMerges));
  for Idx := 0 to High(FMerges) do
    NewTable^.FMerges[Idx] := FMerges[Idx];

  // Глубокое копирование частей-продолжений (issue #1300)
  System.SetLength(NewTable^.FContinuationParts,
    Length(FContinuationParts));
  for Idx := 0 to High(FContinuationParts) do
    NewTable^.CopyTablePart(
      FContinuationParts[Idx],
      NewTable^.FContinuationParts[Idx]);

  NewTable^.bp.ListPos.Owner := AOwn;
  Result := NewTable;
end;

function GDBObjAcadTable.GetObjType: TObjID;
begin
  Result := GDBAcadTableID;
end;

function GDBObjAcadTable.GetObjTypeName: String;
begin
  Result := ObjN_GDBObjAcadTable;
end;

// --- Функции выделения памяти ---

function AllocAcadTable: Pointer;
begin
  GetMem(Result, SizeOf(GDBObjAcadTable));
end;

function AllocAndInitAcadTable(
  AOwner: PGDBObjGenericWithSubordinated): PGDBObjAcadTable;
begin
  GetMem(Pointer(Result), SizeOf(GDBObjAcadTable));
  Result^.initnul(AOwner);
  Result^.bp.ListPos.Owner := AOwner;
end;

initialization
  // Регистрация сущности ACAD_TABLE с привязкой к DXF-имени
  RegisterDXFEntity(
    GDBAcadTableID,
    'ACAD_TABLE',
    'AcadTable',
    @AllocAcadTable,
    @AllocAndInitAcadTable
  );

end.
