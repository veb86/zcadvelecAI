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
  Модуль: uzeentacadtable
  Назначение: Реализация сущности ACAD_TABLE — таблицы AutoCAD в формате DXF.
  Этап 1: чтение содержимого таблицы (строки, столбцы, текст) и отображение.
  Зависимости: uzeentityfactory, uzeentcomplex, uzeentmtext, uzeentline,
               uzeconsts, uzeffdxfsupport, uzMVReader, uzclog
}

unit uzeentacadtable;
{$Mode delphi}{$H+}
{$INCLUDE zengineconfig.inc}

interface

uses
  uzgldrawcontext, uzedrawingdef, uzeentityfactory, uzeentcomplex,
  uzeentline, uzeentmtext, uzeentsubordinated, uzeentabstracttext,
  uzeentity, uzctnrVectorBytesStream, uzeTypes, uzeconsts,
  uzegeometry, uzegeometrytypes, uzeffdxfsupport, uzMVReader,
  uzbLogIntf, uzclog, SysUtils, uzctnrvectordouble;

const
  // Высота строки по умолчанию (в единицах чертежа)
  CAcadTableDefaultRowHeight = 10.0;
  // Ширина столбца по умолчанию (в единицах чертежа)
  CAcadTableDefaultColWidth = 30.0;
  // Высота текста по умолчанию (в единицах чертежа)
  CAcadTableDefaultTextHeight = 2.5;
  // Максимальное количество строк/столбцов — защита от некорректных данных
  CAcadTableMaxDimension = 1000;
  // Максимальное количество ячеек — защита памяти
  CAcadTableMaxCells = 100000;

type
  // Тип указателя на GDBObjAcadTable
  PGDBObjAcadTable = ^GDBObjAcadTable;

  // Сущность ACAD_TABLE — таблица AutoCAD из формата DXF.
  // Хранит геометрию таблицы (точку вставки, размеры строк и столбцов)
  // и текстовое содержимое ячеек.
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
    // Тексты ячеек в плоском виде: индекс = строка * FColCount + столбец
    FCellTexts: array of String;
    // Признак того, что геометрия уже была построена
    FGeometryBuilt: Boolean;

    // Возвращает высоту строки по индексу или значение по умолчанию
    function GetRowHeight(RowIndex: Integer): Double;
    // Возвращает ширину столбца по индексу или значение по умолчанию
    function GetColWidth(ColIndex: Integer): Double;
    // Возвращает суммарную высоту всех строк
    function GetTotalHeight: Double;
    // Возвращает суммарную ширину всех столбцов
    function GetTotalWidth: Double;
    // Возвращает текст ячейки (строка rowIdx, столбец colIdx)
    function GetCellText(RowIdx, ColIdx: Integer): String;
    // Строит визуальное представление в ConstObjArray
    procedure BuildVisualRepresentation(var ADrawing: TDrawingDef;
      var ADC: TDrawContext);
    // Добавляет ячейку в массив текстов по её линейному индексу
    procedure SetCellTextByIndex(CellIdx: Integer; const AText: String);
    // Вычисляет bounding box таблицы на основе её размеров
    procedure getoutbound(var DC: TDrawContext);

  public
    constructor initnul(AOwner: PGDBObjGenericWithSubordinated);
    destructor done; virtual;

    procedure LoadFromDXF(var ARdr: TZMemReader; APtu: PExtensionData;
      var ADrawing: TDrawingDef;
      var AContext: TIODXFLoadContext); virtual;
    procedure SaveToDXF(var AOutStream: TZctnrVectorBytes;
      var ADrawing: TDrawingDef;
      var AIODXFContext: TIODXFSaveContext); virtual;
    procedure SaveToDXFFollow(var AOutStream: TZctnrVectorBytes;
      var ADrawing: TDrawingDef;
      var AIODXFContext: TIODXFSaveContext); virtual;
    procedure BuildGeometry(var ADrawing: TDrawingDef); virtual;
    procedure FormatEntity(var ADrawing: TDrawingDef;
      var ADC: TDrawContext;
      AStage: TEFStages = EFAllStages); virtual;
    function IsStagedFormatEntity: Boolean; virtual;
    function Clone(AOwn: Pointer): PGDBObjEntity; virtual;
    function GetObjType: TObjID; virtual;
    function GetObjTypeName: String; virtual;
    function DXFDelayedBuildGeometry: Boolean; virtual;
  end;

function AllocAcadTable: Pointer;
function AllocAndInitAcadTable(
  AOwner: PGDBObjGenericWithSubordinated): PGDBObjAcadTable;

implementation

// --- Вспомогательные приватные методы ---

// Возвращает высоту строки по индексу.
// При выходе за границы возвращает константу по умолчанию.
function GDBObjAcadTable.GetRowHeight(RowIndex: Integer): Double;
begin
  if (RowIndex >= 0) and (RowIndex < FRowHeights.Count) then
    Result := FRowHeights.parray^[RowIndex]
  else
    Result := CAcadTableDefaultRowHeight;
end;

// Возвращает ширину столбца по индексу.
// При выходе за границы возвращает константу по умолчанию.
function GDBObjAcadTable.GetColWidth(ColIndex: Integer): Double;
begin
  if (ColIndex >= 0) and (ColIndex < FColWidths.Count) then
    Result := FColWidths.parray^[ColIndex]
  else
    Result := CAcadTableDefaultColWidth;
end;

// Возвращает суммарную высоту всех строк таблицы.
function GDBObjAcadTable.GetTotalHeight: Double;
var
  RowIdx: Integer;
begin
  Result := 0;
  for RowIdx := 0 to FRowCount - 1 do
    Result := Result + GetRowHeight(RowIdx);
end;

// Возвращает суммарную ширину всех столбцов таблицы.
function GDBObjAcadTable.GetTotalWidth: Double;
var
  ColIdx: Integer;
begin
  Result := 0;
  for ColIdx := 0 to FColCount - 1 do
    Result := Result + GetColWidth(ColIdx);
end;

// Возвращает текст ячейки по координатам строки и столбца.
// При некорректных индексах возвращает пустую строку.
function GDBObjAcadTable.GetCellText(RowIdx, ColIdx: Integer): String;
var
  CellIndex: Integer;
begin
  Result := '';
  if FColCount <= 0 then
    Exit;
  CellIndex := RowIdx * FColCount + ColIdx;
  if (CellIndex >= 0) and (CellIndex <= High(FCellTexts)) then
    Result := FCellTexts[CellIndex];
end;

// Расширяет массив FCellTexts до нужного индекса и записывает текст.
procedure GDBObjAcadTable.SetCellTextByIndex(CellIdx: Integer;
  const AText: String);
begin
  if CellIdx < 0 then
    Exit;
  // Защита от слишком большого индекса
  if CellIdx >= CAcadTableMaxCells then
  begin
    programlog.LogOutFormatStr(
      'uzeentacadtable: SetCellTextByIndex — индекс %d превышает лимит %d',
      [CellIdx, CAcadTableMaxCells], LM_Info);
    Exit;
  end;
  if CellIdx > High(FCellTexts) then
    System.SetLength(FCellTexts, CellIdx + 1);
  FCellTexts[CellIdx] := AText;
end;

// --- Конструктор и деструктор ---

// Инициализирует сущность с нулевыми значениями.
constructor GDBObjAcadTable.initnul(AOwner: PGDBObjGenericWithSubordinated);
begin
  inherited initnul;
  FInsertPoint := NulVertex;
  FRowCount := 0;
  FColCount := 0;
  FRowHeights.initnul;
  FColWidths.initnul;
  System.SetLength(FCellTexts, 0);
  FGeometryBuilt := False;
end;

// Освобождает динамически выделенные ресурсы.
destructor GDBObjAcadTable.done;
begin
  FRowHeights.done;
  FColWidths.done;
  System.SetLength(FCellTexts, 0);
  inherited done;
end;

// --- Загрузка из DXF ---

// Загружает данные таблицы из DXF-потока.
// Формат ACAD_TABLE: точка вставки (10,20,30), количество строк (91),
// количество столбцов (92), высоты строк (141), ширины столбцов (142),
// текст ячеек (1). Чтение завершается при достижении кода 0.
procedure GDBObjAcadTable.LoadFromDXF(var ARdr: TZMemReader;
  APtu: PExtensionData; var ADrawing: TDrawingDef;
  var AContext: TIODXFLoadContext);
var
  GroupCode: Integer;
  CellText: String;
  DoubleVal: Double;
  CellIndex: Integer;
  SubclassMarker: String;
  RowHeightsRead, ColWidthsRead: Boolean;
  InCellData: Boolean;
  CellTextRead: Boolean;
  RowHeightCount, ColWidthCount: Integer;
begin
  programlog.LogOutStr('uzeentacadtable: LoadFromDXF START', LM_Info);

  CellIndex := 0;
  GroupCode := ARdr.ParseInteger;
  RowHeightsRead := False;
  ColWidthsRead := False;
  InCellData := False;
  CellTextRead := False;
  RowHeightCount := 0;
  ColWidthCount := 0;

  // Пропускаем до первого 100
  while (GroupCode <> 0) and (GroupCode <> 100) do
  begin
    ARdr.SkipString;
    GroupCode := ARdr.ParseInteger;
  end;

  // Читаем первый маркер подкласса (AcDbEntity)
  if GroupCode = 100 then
  begin
    SubclassMarker := ARdr.ParseString;
    GroupCode := ARdr.ParseInteger;
  end;

  // Пропускаем AcDbEntity до AcDbBlockReference
  while (GroupCode <> 0) and (GroupCode <> 100) do
  begin
    if not LoadFromDXFObjShared(ARdr, GroupCode, APtu, ADrawing, AContext) then
      ARdr.SkipString;
    GroupCode := ARdr.ParseInteger;
  end;

  // Читаем AcDbBlockReference
  if GroupCode = 100 then
  begin
    SubclassMarker := ARdr.ParseString;
    GroupCode := ARdr.ParseInteger;

    // Читаем данные AcDbBlockReference (включая точку вставки)
    while (GroupCode <> 0) and (GroupCode <> 100) do
    begin
      case GroupCode of
        10: FInsertPoint.x := ARdr.ParseDouble;
        20: FInsertPoint.y := ARdr.ParseDouble;
        30: FInsertPoint.z := ARdr.ParseDouble;
      else
        if not LoadFromDXFObjShared(ARdr, GroupCode, APtu, ADrawing, AContext) then
          ARdr.SkipString;
      end;
      GroupCode := ARdr.ParseInteger;
    end;
  end;

  // Читаем AcDbTable (настоящие данные таблицы)
  if GroupCode = 100 then
  begin
    SubclassMarker := ARdr.ParseString;
    GroupCode := ARdr.ParseInteger;
  end;

  // Теперь читаем данные таблицы
  while GroupCode <> 0 do
  begin
    // Пробуем общую обработку (хэндл, слой, цвет и т.д.)
    if not LoadFromDXFObjShared(ARdr, GroupCode, APtu, ADrawing, AContext) then
    begin
      case GroupCode of
        // Точка вставки
        10: FInsertPoint.x := ARdr.ParseDouble;
        20: FInsertPoint.y := ARdr.ParseDouble;
        30: FInsertPoint.z := ARdr.ParseDouble;

        // Количество строк — читаем ТОЛЬКО первый раз (до высот строк)
        91:
        begin
          if not RowHeightsRead then
          begin
            FRowCount := ARdr.ParseInteger;
            if (FRowCount < 0) or (FRowCount > CAcadTableMaxDimension) then
              FRowCount := 0;
            programlog.LogOutFormatStr(
              'uzeentacadtable: LoadFromDXF RowCount=%d',
              [FRowCount], LM_Info);
          end
          else
            ARdr.SkipString; // Это флаг override ячейки
        end;

        // Количество столбцов — читаем ТОЛЬКО первый раз (до высот строк)
        92:
        begin
          if not RowHeightsRead then
          begin
            FColCount := ARdr.ParseInteger;
            if (FColCount < 0) or (FColCount > CAcadTableMaxDimension) then
              FColCount := 0;
            programlog.LogOutFormatStr(
              'uzeentacadtable: LoadFromDXF ColCount=%d',
              [FColCount], LM_Info);
          end
          else
            ARdr.SkipString; // Это флаг override ячейки
        end;

        // Высота строки (повторяется FRowCount раз)
        141:
        begin
          DoubleVal := ARdr.ParseDouble;
          if DoubleVal <= 0 then
            DoubleVal := CAcadTableDefaultRowHeight;
          FRowHeights.PushBackData(DoubleVal);
          RowHeightsRead := True;
          Inc(RowHeightCount);
          programlog.LogOutFormatStr(
            'uzeentacadtable: LoadFromDXF RowHeight[%d]=%.2f',
            [RowHeightCount - 1, DoubleVal], LM_Info);
        end;

        // Ширина столбца (повторяется FColCount раз)
        142:
        begin
          DoubleVal := ARdr.ParseDouble;
          if DoubleVal <= 0 then
            DoubleVal := CAcadTableDefaultColWidth;
          FColWidths.PushBackData(DoubleVal);
          ColWidthsRead := True;
          Inc(ColWidthCount);
          programlog.LogOutFormatStr(
            'uzeentacadtable: LoadFromDXF ColWidth[%d]=%.2f',
            [ColWidthCount - 1, DoubleVal], LM_Info);
        end;

        // Начало данных ячейки — код 171 (тип ячейки)
        171:
        begin
          InCellData := True;
          CellTextRead := False;
          ARdr.SkipString;
        end;

        // Текст ячейки — код 302 (DXF 2007+) — приоритет
        302:
        begin
          if InCellData and not CellTextRead then
          begin
            dxfLoadString(ARdr, CellText, AContext.Header);
            SetCellTextByIndex(CellIndex, CellText);
            programlog.LogOutFormatStr(
              'uzeentacadtable: LoadFromDXF Cell[%d] text="%s"',
              [CellIndex, CellText], LM_Info);
            Inc(CellIndex);
            CellTextRead := True;
          end
          else
            ARdr.SkipString;
        end;

        // Текст ячейки (старый формат, код 1) — читаем только если 302 не было
        1:
        begin
          if InCellData and not CellTextRead then
          begin
            dxfLoadString(ARdr, CellText, AContext.Header);
            SetCellTextByIndex(CellIndex, CellText);
            programlog.LogOutFormatStr(
              'uzeentacadtable: LoadFromDXF Cell[%d] text="%s"',
              [CellIndex, CellText], LM_Info);
            Inc(CellIndex);
            CellTextRead := True;
          end
          else
            ARdr.SkipString;
        end;

        // Пропускаем служебные коды ячеек
        11, 21, 31, 90, 93, 94, 95, 96, 170, 172, 173, 174, 175, 176, 177, 178,
        145, 300, 301, 304, 274, 275, 276, 278, 279, 280, 281, 283, 284, 285, 286,
        288, 289, 63, 64, 65, 66, 68, 69, 70, 40, 41, 342, 343, 344, 340:
          ARdr.SkipString;

      else
        // Неизвестный код — пропускаем значение
        ARdr.SkipString;
      end;
    end;

    GroupCode := ARdr.ParseInteger;
  end;

  // Координаты вставки переносим в Local для корректного позиционирования
  Local.P_insert := FInsertPoint;

  programlog.LogOutFormatStr(
    'uzeentacadtable: LoadFromDXF END rows=%d cols=%d cells=%d row_heights=%d col_widths=%d',
    [FRowCount, FColCount, Length(FCellTexts), FRowHeights.Count, FColWidths.Count], LM_Info);
end;

// --- Построение визуального представления ---

// Строит линии сетки и тексты ячеек в ConstObjArray.
// Горизонтальные линии — разделители строк (FRowCount + 1 линия).
// Вертикальные линии — разделители столбцов (FColCount + 1 линия).
// Текст добавляется только для непустых ячеек.
// Каждая созданная сущность форматируется через FormatEntity.
procedure GDBObjAcadTable.BuildVisualRepresentation(
  var ADrawing: TDrawingDef;
  var ADC: TDrawContext);
var
  RowIdx, ColIdx: Integer;
  CurrentY, CurrentX: Double;
  TotalWidth, TotalHeight: Double;
  RowH, ColW: Double;
  PLine: PGDBObjLine;
  PMText: PGDBObjMText;
  CellStr: String;
  LineCount, TextCount: Integer;
begin
  programlog.LogOutFormatStr(
    'uzeentacadtable: BuildVisualRepresentation START rows=%d cols=%d',
    [FRowCount, FColCount], LM_Info);
  programlog.LogOutFormatStr(
    'uzeentacadtable: BuildVisualRepresentation ConstObjArray before clear=%d',
    [ConstObjArray.Count], LM_Info);
  ConstObjArray.Free;
  programlog.LogOutFormatStr(
    'uzeentacadtable: BuildVisualRepresentation ConstObjArray after clear=%d',
    [ConstObjArray.Count], LM_Info);

  // Пустая таблица — ничего не рисуем
  if (FRowCount <= 0) or (FColCount <= 0) then
  begin
    programlog.LogOutStr(
      'uzeentacadtable: BuildVisualRepresentation — таблица пуста',
      LM_Info);
    Exit;
  end;

  TotalWidth := GetTotalWidth;
  TotalHeight := GetTotalHeight;
  programlog.LogOutFormatStr(
    'uzeentacadtable: BuildVisualRepresentation TotalWidth=%.2f TotalHeight=%.2f',
    [TotalWidth, TotalHeight], LM_Info);

  LineCount := 0;
  TextCount := 0;

  // --- Горизонтальные линии (верхняя, разделители, нижняя) ---
  CurrentY := 0;
  for RowIdx := 0 to FRowCount do
  begin
    pointer(PLine) := ConstObjArray.CreateInitObj(GDBLineID, @Self);
    PLine^.CoordInOCS.lBegin.x := 0;
    PLine^.CoordInOCS.lBegin.y := -CurrentY;
    PLine^.CoordInOCS.lBegin.z := 0;
    PLine^.CoordInOCS.lEnd.x := TotalWidth;
    PLine^.CoordInOCS.lEnd.y := -CurrentY;
    PLine^.CoordInOCS.lEnd.z := 0;
    programlog.LogOutFormatStr(
      'uzeentacadtable: BuildVisualRepresentation HLine[%d] from(%.2f,%.2f) to(%.2f,%.2f)',
      [RowIdx, PLine^.CoordInOCS.lBegin.x, PLine^.CoordInOCS.lBegin.y,
       PLine^.CoordInOCS.lEnd.x, PLine^.CoordInOCS.lEnd.y], LM_Info);
    CopyVPto(PLine^);
    PLine^.FormatEntity(ADrawing, ADC);
    Inc(LineCount);

    if RowIdx < FRowCount then
      CurrentY := CurrentY + GetRowHeight(RowIdx);
  end;
  programlog.LogOutFormatStr(
    'uzeentacadtable: BuildVisualRepresentation created %d horizontal lines',
    [LineCount], LM_Info);

  // --- Вертикальные линии (левая, разделители, правая) ---
  CurrentX := 0;
  LineCount := 0;
  for ColIdx := 0 to FColCount do
  begin
    pointer(PLine) := ConstObjArray.CreateInitObj(GDBLineID, @Self);
    PLine^.CoordInOCS.lBegin.x := CurrentX;
    PLine^.CoordInOCS.lBegin.y := 0;
    PLine^.CoordInOCS.lBegin.z := 0;
    PLine^.CoordInOCS.lEnd.x := CurrentX;
    PLine^.CoordInOCS.lEnd.y := -TotalHeight;
    PLine^.CoordInOCS.lEnd.z := 0;
    programlog.LogOutFormatStr(
      'uzeentacadtable: BuildVisualRepresentation VLine[%d] from(%.2f,%.2f) to(%.2f,%.2f)',
      [ColIdx, PLine^.CoordInOCS.lBegin.x, PLine^.CoordInOCS.lBegin.y,
       PLine^.CoordInOCS.lEnd.x, PLine^.CoordInOCS.lEnd.y], LM_Info);
    CopyVPto(PLine^);
    PLine^.FormatEntity(ADrawing, ADC);
    Inc(LineCount);

    if ColIdx < FColCount then
      CurrentX := CurrentX + GetColWidth(ColIdx);
  end;
  programlog.LogOutFormatStr(
    'uzeentacadtable: BuildVisualRepresentation created %d vertical lines',
    [LineCount], LM_Info);

  // --- Текст ячеек ---
  CurrentY := 0;
  TextCount := 0;
  for RowIdx := 0 to FRowCount - 1 do
  begin
    RowH := GetRowHeight(RowIdx);
    CurrentX := 0;

    for ColIdx := 0 to FColCount - 1 do
    begin
      ColW := GetColWidth(ColIdx);
      CellStr := GetCellText(RowIdx, ColIdx);

      programlog.LogOutFormatStr(
        'uzeentacadtable: BuildVisualRepresentation Cell[%d,%d] text="%s" ColW=%.2f RowH=%.2f',
        [RowIdx, ColIdx, CellStr, ColW, RowH], LM_Info);

      if CellStr <> '' then
      begin
        pointer(PMText) := ConstObjArray.CreateInitObj(GDBMTextID, @Self);
        PMText^.Template := UTF8ToString(CellStr);
        PMText^.textprop.size := CAcadTableDefaultTextHeight;
        PMText^.linespacef := 1;
        PMText^.Width := ColW * 0.9;
        PMText^.textprop.justify := jstl;
        PMText^.Local.P_insert.x :=
          CurrentX + CAcadTableDefaultTextHeight * 0.5;
        PMText^.Local.P_insert.y :=
          -(CurrentY + RowH * 0.5);
        PMText^.Local.P_insert.z := 0;
        PMText^.TXTStyle :=
          pointer(ADrawing.GetTextStyleTable^.getDataMutable(0));
        programlog.LogOutFormatStr(
          'uzeentacadtable: BuildVisualRepresentation MText[%d,%d] insert(%.2f,%.2f) text="%s"',
          [RowIdx, ColIdx, PMText^.Local.P_insert.x, PMText^.Local.P_insert.y,
           PMText^.Template], LM_Info);
        CopyVPto(PMText^);
        PMText^.FormatEntity(ADrawing, ADC);
        Inc(TextCount);
      end;

      CurrentX := CurrentX + ColW;
    end;

    CurrentY := CurrentY + RowH;
  end;
  programlog.LogOutFormatStr(
    'uzeentacadtable: BuildVisualRepresentation created %d text objects',
    [TextCount], LM_Info);

  programlog.LogOutFormatStr(
    'uzeentacadtable: BuildVisualRepresentation OK ' +
    'rows=%d cols=%d W=%.2f H=%.2f TotalObj=%d',
    [FRowCount, FColCount, TotalWidth, TotalHeight, ConstObjArray.Count], LM_Info);
end;

// Вычисляет bounding box таблицы на основе её размеров.
// Вызывается до BuildGeometry, когда ConstObjArray ещё пуст.
procedure GDBObjAcadTable.getoutbound(var DC: TDrawContext);
var
  TotalWidth, TotalHeight: Double;
  MinX, MinY, MaxX, MaxY: Double;
begin
  if (FRowCount <= 0) or (FColCount <= 0) then
  begin
    // Пустая таблица — минимальный bounding box
    vp.BoundingBox.LBN := VertexAdd(Local.P_insert, CreateVertex(-0.01, -0.01, 0));
    vp.BoundingBox.RTF := VertexAdd(Local.P_insert, CreateVertex(0.01, 0.01, 0));
    Exit;
  end;

  TotalWidth := GetTotalWidth;
  TotalHeight := GetTotalHeight;

  // Таблица строится от (0,0) до (TotalWidth, -TotalHeight) в локальных координатах
  // Затем применяется трансформация (Local.P_insert)
  MinX := Local.P_insert.x;
  MaxX := Local.P_insert.x + TotalWidth;
  MinY := Local.P_insert.y - TotalHeight;
  MaxY := Local.P_insert.y;

  vp.BoundingBox.LBN := CreateVertex(MinX, MinY, Local.P_insert.z);
  vp.BoundingBox.RTF := CreateVertex(MaxX, MaxY, Local.P_insert.z);

  programlog.LogOutFormatStr(
    'uzeentacadtable: getoutbound BB=(%.2f,%.2f)-(%.2f,%.2f) W=%.2f H=%.2f',
    [MinX, MinY, MaxX, MaxY, TotalWidth, TotalHeight], LM_Info);
end;

// --- Методы сущности ---

// Строит геометрию таблицы. Вызывается после добавления в чертёж.
// ObjMatrix уже содержит мировую трансформацию из CalcObjMatrix,
// поэтому FormatEntity дочерних объектов вычислит экранные координаты
// в мировом пространстве — так же, как это делает GDBObjTable.Build.
procedure GDBObjAcadTable.BuildGeometry(var ADrawing: TDrawingDef);
var
  DC: TDrawContext;
begin
  programlog.LogOutFormatStr(
    'uzeentacadtable: BuildGeometry START built=%d rows=%d cols=%d Insert=(%.2f,%.2f)',
    [Ord(FGeometryBuilt), FRowCount, FColCount, FInsertPoint.x, FInsertPoint.y], LM_Info);

  if not FGeometryBuilt then
  begin
    DC := ADrawing.CreateDrawingRC;
    programlog.LogOutFormatStr(
      'uzeentacadtable: BuildGeometry created DC, ConstObjArray count before=%d',
      [ConstObjArray.Count], LM_Info);

    // Строим визуальное представление (линии и текст)
    BuildVisualRepresentation(ADrawing, DC);

    programlog.LogOutFormatStr(
      'uzeentacadtable: BuildGeometry after BuildVisualRepresentation ConstObjArray count=%d',
      [ConstObjArray.Count], LM_Info);

    FGeometryBuilt := True;

    programlog.LogOutStr(
      'uzeentacadtable: BuildGeometry building ObjTree',
      LM_Info);
    // Строим дерево отрисовки — аналогично GDBObjComplex.BuildGeometry
    inherited BuildGeometry(ADrawing);
    programlog.LogOutFormatStr(
      'uzeentacadtable: BuildGeometry END ObjTree.NodeCount=%d',
      [ConstObjArray.ObjTree.nul.Count], LM_Info);
  end
  else
  begin
    programlog.LogOutStr(
      'uzeentacadtable: BuildGeometry SKIPPED - already built',
      LM_Info);
  end;

  programlog.LogOutStr(
    'uzeentacadtable: BuildGeometry END',
    LM_Info);
end;

// Форматирует сущность для отображения (расчёт матриц и отрисовка).
procedure GDBObjAcadTable.FormatEntity(var ADrawing: TDrawingDef;
  var ADC: TDrawContext; AStage: TEFStages = EFAllStages);
begin
  programlog.LogOutFormatStr(
    'uzeentacadtable: FormatEntity START CalcCS=%d Draw=%d rows=%d cols=%d Visible=%d',
    [Ord(EFCalcEntityCS in AStage), Ord(EFDraw in AStage), FRowCount, FColCount, Ord(Visible)], LM_Info);
  if EFCalcEntityCS in AStage then
  begin
    if Assigned(EntExtensions) then
      EntExtensions.RunOnBeforeEntityFormat(@Self, ADrawing, ADC);
    CalcObjMatrix;
    programlog.LogOutFormatStr(
      'uzeentacadtable: FormatEntity after CalcObjMatrix Local=(%.2f,%.2f,%.2f)',
      [Local.P_insert.x, Local.P_insert.y, Local.P_insert.z], LM_Info);
    // Вычисляем bounding box вручную до BuildGeometry (ConstObjArray ещё пуст)
    getoutbound(ADC);
    programlog.LogOutFormatStr(
      'uzeentacadtable: FormatEntity after getoutbound BB=(%.2f,%.2f)-(%.2f,%.2f)',
      [vp.BoundingBox.LBN.x, vp.BoundingBox.LBN.y, vp.BoundingBox.RTF.x, vp.BoundingBox.RTF.y], LM_Info);
  end;
  CalcActualVisible(ADC.DrawingContext.VActuality);
  programlog.LogOutFormatStr(
    'uzeentacadtable: FormatEntity after CalcActualVisible Visible=%d',
    [Ord(Visible)], LM_Info);
  if EFDraw in AStage then
  begin
    programlog.LogOutStr(
      'uzeentacadtable: FormatEntity calling BuildGeometry',
      LM_Info);
    // Строим геометрию только один раз (при первой отрисовке)
    BuildGeometry(ADrawing);
    programlog.LogOutStr(
      'uzeentacadtable: FormatEntity after BuildGeometry',
      LM_Info);
    if Assigned(EntExtensions) then
      EntExtensions.RunOnAfterEntityFormat(@Self, ADrawing, ADC);
  end;
  programlog.LogOutStr(
    'uzeentacadtable: FormatEntity END',
    LM_Info);
end;

// Использует отложенное форматирование (стадийный FormatEntity).
function GDBObjAcadTable.IsStagedFormatEntity: Boolean;
begin
  Result := True;
end;

// Откладывает построение геометрии: таблица будет построена после загрузки.
function GDBObjAcadTable.DXFDelayedBuildGeometry: Boolean;
begin
  Result := True;
end;

// Сохраняет таблицу в DXF-поток.
// TODO (этап 3): добавить полноценный экспорт в формат ACAD_TABLE.
procedure GDBObjAcadTable.SaveToDXF(var AOutStream: TZctnrVectorBytes;
  var ADrawing: TDrawingDef; var AIODXFContext: TIODXFSaveContext);
begin
  // Этап 1: сохранение не реализовано
  programlog.LogOutStr(
    'uzeentacadtable: SaveToDXF — экспорт не реализован (этап 3)',
    LM_Info);
end;

// Дополнительные данные после основного блока (не используются на этапе 1).
procedure GDBObjAcadTable.SaveToDXFFollow(var AOutStream: TZctnrVectorBytes;
  var ADrawing: TDrawingDef; var AIODXFContext: TIODXFSaveContext);
begin
  // Пустая реализация
end;

// Создаёт полную копию сущности, включая все данные таблицы.
function GDBObjAcadTable.Clone(AOwn: Pointer): PGDBObjEntity;
var
  NewTable: PGDBObjAcadTable;
  Idx: Integer;
begin
  GetMem(Pointer(NewTable), SizeOf(GDBObjAcadTable));
  NewTable^.initnul(AOwn);
  CopyVPto(NewTable^);
  CopyExtensionsTo(NewTable^);

  NewTable^.FInsertPoint := FInsertPoint;
  NewTable^.FRowCount := FRowCount;
  NewTable^.FColCount := FColCount;

  // Копируем высоты строк
  for Idx := 0 to FRowHeights.Count - 1 do
    NewTable^.FRowHeights.PushBackData(FRowHeights.parray^[Idx]);

  // Копируем ширины столбцов
  for Idx := 0 to FColWidths.Count - 1 do
    NewTable^.FColWidths.PushBackData(FColWidths.parray^[Idx]);

  // Копируем тексты ячеек
  System.SetLength(NewTable^.FCellTexts, Length(FCellTexts));
  for Idx := 0 to High(FCellTexts) do
    NewTable^.FCellTexts[Idx] := FCellTexts[Idx];

  NewTable^.bp.ListPos.Owner := AOwn;
  Result := NewTable;
end;

// Возвращает числовой идентификатор типа объекта.
function GDBObjAcadTable.GetObjType: TObjID;
begin
  Result := GDBAcadTableID;
end;

// Возвращает строковое имя типа объекта.
function GDBObjAcadTable.GetObjTypeName: String;
begin
  Result := ObjN_GDBObjAcadTable;
end;

// --- Функции выделения памяти ---

// Выделяет память для объекта без инициализации.
function AllocAcadTable: Pointer;
begin
  GetMem(Result, SizeOf(GDBObjAcadTable));
end;

// Выделяет и инициализирует новый объект GDBObjAcadTable.
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
