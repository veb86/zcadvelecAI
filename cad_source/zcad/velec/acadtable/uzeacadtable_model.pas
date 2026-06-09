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
  uzbLogIntf, uzclog, SysUtils, uzctnrvectordouble,
  uzestylestablesdxf, gzctnrVectorTypes, Types, uzestylestexts,
  uzeacadtable_types, uzeacadtable_styles,
  uzeacadtable_cell, uzeacadtable_merge,
  uzeacadtable_layout, uzeacadtable_stylemanager,
  uzeacadtable_dxf_read, uzeacadtable_dxf_write;

type
  // Тип указателя на GDBObjAcadTable
  PGDBObjAcadTable = ^GDBObjAcadTable;

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
    FCellTexts: array of String;
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
    // Стиль таблицы
    FTableStyle: TTableStyle;
    // Строки, столбцы, ячейки и объединения
    FRows: array of TTableRow;
    FCols: array of TTableColumn;
    FCells: array of array of TTableCell;
    FMerges: array of TMergeRange;

    // Обёртки для делегирования к модулю layout
    function GetRowHeightLocal(RowIndex: Integer): Double;
    function GetColWidthLocal(ColIndex: Integer): Double;
    function GetTotalHeight: Double;
    function GetTotalWidth: Double;
    function GetCellTextLocal(
      RowIdx, ColIdx: Integer): String;
    // Строит визуальное представление в ConstObjArray
    procedure BuildVisualRepresentation(
      var ADrawing: TDrawingDef; var ADC: TDrawContext);
    // Вычисляет bounding box таблицы
    procedure getoutbound(var DC: TDrawContext);
    // Возвращает имя стиля таблицы
    function GetTableStyleName: String;

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
    function Clone(AOwn: Pointer): PGDBObjEntity; virtual;
    function GetObjType: TObjID; virtual;
    function GetObjTypeName: String; virtual;
    function DXFDelayedBuildGeometry: Boolean; virtual;

    // Публичные свойства для инспектора объектов
    property InsertPoint: TzePoint3d read FInsertPoint;
    property RowCount: Integer read FRowCount;
    property ColCount: Integer read FColCount;
    property Width: Double read GetTotalWidth;
    property Height: Double read GetTotalHeight;
    property TableStyleName: String read GetTableStyleName;
    property BreakEnabled: Boolean read FBreakEnabled;
    property BreakDirection: TAcadTableBreakDirection
      read FBreakDirection;
    property BreakRepeatTopLabels: Boolean
      read FBreakRepeatTopLabels;
    property BreakRepeatBottomLabels: Boolean
      read FBreakRepeatBottomLabels;
    property BreakManualPosition: Boolean
      read FBreakManualPosition;
    property BreakManualHeight: Boolean
      read FBreakManualHeight;
    property BreakSpacing: Double read FBreakSpacing;
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
  InitTableStyle(FTableStyle);
  System.SetLength(FRows, 0);
  System.SetLength(FCols, 0);
  System.SetLength(FCells, 0, 0);
  System.SetLength(FMerges, 0);
end;

destructor GDBObjAcadTable.done;
begin
  FRowHeights.done;
  FColWidths.done;
  System.SetLength(FCellTexts, 0);
  System.SetLength(FRows, 0);
  System.SetLength(FCols, 0);
  System.SetLength(FCells, 0, 0);
  System.SetLength(FMerges, 0);
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
      DXFData.RowHeights.parray^[RowIdx]);
  for ColIdx := 0 to DXFData.ColWidths.Count - 1 do
    FColWidths.PushBackData(
      DXFData.ColWidths.parray^[ColIdx]);

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

procedure GDBObjAcadTable.BuildVisualRepresentation(
  var ADrawing: TDrawingDef; var ADC: TDrawContext);
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
begin
  programlog.LogOutFormatStr(
    'AcadTable: model: BuildVisualRepresentation START ' +
    'rows=%d cols=%d',
    [FRowCount, FColCount], LM_Info);
  ConstObjArray.Free;

  if (FRowCount <= 0) or (FColCount <= 0) then
  begin
    programlog.LogOutStr(
      'AcadTable: model: BuildVisualRepresentation — ' +
      'таблица пуста', LM_Info);
    Exit;
  end;

  TotalWidth := GetTotalWidth;
  TotalHeight := GetTotalHeight;
  LineCount := 0;
  TextCount := 0;

  uzeacadtable_layout.BuildRenderSegments(
    FRowCount, FRowHeights, FColWidths, FColCount,
    FTableFlags, FBreakEnabled, FBreakDirection,
    FBreakRepeatTopLabels, FBreakManualHeight,
    FBreakSpacing, RenderSegments, SegmentCount);
  if SegmentCount <= 0 then Exit;

  // --- Горизонтальные линии ---
  for SegmentIdx := 0 to SegmentCount - 1 do
  begin
    SegmentOffsetX := RenderSegments[SegmentIdx].OffsetX;
    SegmentOffsetY := RenderSegments[SegmentIdx].OffsetY;
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
    SegmentOffsetX := RenderSegments[SegmentIdx].OffsetX;
    SegmentOffsetY := RenderSegments[SegmentIdx].OffsetY;
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
    SegmentOffsetX := RenderSegments[SegmentIdx].OffsetX;
    SegmentOffsetY := RenderSegments[SegmentIdx].OffsetY;
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
          CellStyleLocal := uzeacadtable_cell.ResolveCellStyle(
            RowIdx, ColIdx, FTableStyle,
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
    'AcadTable: model: BuildVisualRepresentation OK ' +
    'rows=%d cols=%d texts=%d TotalObj=%d',
    [FRowCount, FColCount, TextCount,
     ConstObjArray.Count], LM_Info);
end;

// Вычисляет bounding box
procedure GDBObjAcadTable.getoutbound(var DC: TDrawContext);
var
  TotalWidthVal, TotalHeightVal: Double;
  MinX, MinY, MaxX, MaxY: Double;
begin
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
    getoutbound(ADC);
  end;
  CalcActualVisible(ADC.DrawingContext.VActuality);
  if EFDraw in AStage then
  begin
    BuildGeometry(ADrawing);
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

  for Idx := 0 to FRowHeights.Count - 1 do
    NewTable^.FRowHeights.PushBackData(
      FRowHeights.parray^[Idx]);
  for Idx := 0 to FColWidths.Count - 1 do
    NewTable^.FColWidths.PushBackData(
      FColWidths.parray^[Idx]);

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
