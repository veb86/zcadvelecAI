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
  Модуль: uzeacadtable_dxf_write
  Назначение: Экспорт данных таблицы ACAD_TABLE в DXF-поток.
  Зависимости: uzeacadtable_types, uzctnrVectorBytesStream,
               uzedrawingdef, uzedrawingsimple, uzeffdxfsupport,
               uzclog
}

unit uzeacadtable_dxf_write;
{$Mode delphi}{$H+}
{$INCLUDE zengineconfig.inc}

interface

uses
  uzeacadtable_types,
  uzegeometrytypes, uzctnrVectorBytesStream,
  uzedrawingdef, uzedrawingsimple, uzeffdxfsupport,gzctnrVectorTypes,
  uzeTypes, uzeconsts, uzclog, uzbLogIntf;

type
  TAcadTableDoubleArray = array of Double;

  TAcadTableDXFWritePart = record
    HandleKey: Pointer;
    LayerName: String;
    Color: Integer;
    LineWeight: Integer;
    LineTypeName: String;
    LineTypeScale: Double;
    InsertPoint: TzePoint3d;
    Direction: TzePoint3d;
    RowCount: Integer;
    ColCount: Integer;
    RowHeights: TAcadTableDoubleArray;
    ColWidths: TAcadTableDoubleArray;
    CellTexts: TTableTextArray;
    Cells: TTableCellArray;
    Merges: TMergeRangeArray;
    TableStyleHandle: String;
    // Имя стиля таблицы. Хэндл TABLESTYLE перенумеровывается при сохранении,
    // поэтому TableStyleHandle (хэндл из загруженного файла) для ссылки 342
    // непригоден — хэндл резолвится по имени через
    // AIODXFContext.TableStyleNameHandleMap (issue #1409).
    TableStyleName: String;
    // Имя анонимного блока таблицы (group code 2 / связанный BLOCK_RECORD
    // через group code 343). Для модельного пути записи (issue #1381) сюда
    // подставляется имя сгенерированного перед сохранением блока с геометрией
    // этой части, чтобы AutoCAD отрисовал каждую часть разорванной таблицы как
    // самостоятельную таблицу. Пустая строка — блок не сгенерирован, тогда
    // writer использует историческое имя *T<n> и не пишет group code 343.
    BlockName: String;
    TableFlags: Integer;
    BreakEnabled: Boolean;
    BreakDirection: TAcadTableBreakDirection;
    BreakRepeatTopLabels: Boolean;
    BreakRepeatBottomLabels: Boolean;
    BreakManualPosition: Boolean;
    BreakManualHeight: Boolean;
    BreakSpacing: Double;
    BreakHeight: Double;
  end;

  TAcadTableDXFWritePartArray = array of TAcadTableDXFWritePart;

procedure ResetAcadTableDXFWriteState;

procedure WriteAcadTableToDXF(
  var AOutStream: TZctnrVectorBytes;
  var ADrawing: TDrawingDef;
  var AIODXFContext: TIODXFSaveContext;
  const APart: TAcadTableDXFWritePart;
  APartIndex: Integer = 0);

procedure WriteAcadTableContinuationPartsToDXF(
  var AOutStream: TZctnrVectorBytes;
  var ADrawing: TDrawingDef;
  var AIODXFContext: TIODXFSaveContext;
  const AMainPart: TAcadTableDXFWritePart;
  const AParts: TAcadTableDXFWritePartArray);

function WriteRawAcadTablePartsToDXF(
  var AOutStream: TZctnrVectorBytes;
  var AIODXFContext: TIODXFSaveContext;
  const AMainRawEntity: String;
  const AContinuationRawEntities: array of String;
  ABreakSpacing, ABreakHeight: Double;
  ABreakManualPosition, ABreakManualHeight: Boolean;
  const ATableStyleName: String): Boolean;

procedure WriteAcadTableRoundTripObjectsToDXF(
  var AOutStream: TZctnrVectorBytes;
  var ADrawing: TSimpleDrawing;
  var AIODXFContext: TIODXFSaveContext);

implementation

uses
  SysUtils, Classes, uzeffdxfout, uzeentity;

const
  { Идентификаторы стилей ячеек AutoCAD (CELLSTYLEMAP / TABLECELL_BEGIN.90):
    1 = _TITLE, 2 = _HEADER, 3 = _DATA. Внутренние StyleType таблицы ZCAD
    нумеруются с нуля (0 = Title, 1 = Header, 2 = Data). }
  CAcadCellStyleIdTitle = 1;

type
  TAcadTableIntegerArray = array of Integer;
  TAcadTableStringArray = array of String;

  { Данные для объекта TABLECONTENT (round-trip AutoCAD 2008+), в котором
    хранится ИНДИВИДУАЛЬНЫЙ стиль каждой ячейки (issue #1409). В самой
    сущности ACAD_TABLE стиль ячейки записать негде: группа 172 — это
    cell flag value, а не стиль, поэтому AutoCAD её игнорирует и раскрашивает
    таблицу по своему правилу «строка 0 — заголовок, строка 1 — шапка,
    остальные — данные». Реальные стили лежат в TABLECONTENT:
      TABLEROW_BEGIN  / 90 — стиль строки,
      TABLECELL_BEGIN / 90 — стиль конкретной ячейки. }
  TAcadTableContentRecord = record
    EntityHandle: TDWGHandle;
    DictionaryHandle: TDWGHandle;
    XRecordHandle: TDWGHandle;
    ContentHandle: TDWGHandle;
    TableStyleHandle: String;
    TableStyleName: String;
    RowCount: Integer;
    ColCount: Integer;
    RowHeights: TAcadTableDoubleArray;
    ColWidths: TAcadTableDoubleArray;
    RowStyleIds: TAcadTableIntegerArray;
    CellStyleIds: TAcadTableIntegerArray;
    CellTexts: TAcadTableStringArray;
  end;

  TAcadTableRoundTripRecord = record
    MainHandle: TDWGHandle;
    ContinuationHandles: array of TDWGHandle;
    BreakSpacing: Double;
    BreakHeight: Double;
    { Признаки ручного управления разрывами (issue #1339). Влияют на
      BreakOption-флаг (первая группа 90) в split-XRECORD таблицы:
        8  (AllowManualPositions) <- BreakManualPosition,
        16 (AllowManualHeights)   <- BreakManualHeight. }
    BreakManualPosition: Boolean;
    BreakManualHeight: Boolean;
  end;

var
  RoundTripRecords: array of TAcadTableRoundTripRecord;
  ContentRecords: array of TAcadTableContentRecord;

procedure WriteAcadTableClassRecord(
  var AOutStream: TZctnrVectorBytes;
  const ADXFName, ACppName: String;
  AProxyFlags, AInstanceCount, AIsEntity: Integer);
begin
  dxfStringWithoutEncodeOut(AOutStream, 0, 'CLASS');
  dxfStringWithoutEncodeOut(AOutStream, 1, ADXFName);
  dxfStringWithoutEncodeOut(AOutStream, 2, ACppName);
  dxfStringWithoutEncodeOut(AOutStream, 3, 'ObjectDBX Classes');
  dxfIntegerout(AOutStream, 90, AProxyFlags);
  dxfIntegerout(AOutStream, 91, AInstanceCount);
  dxfIntegerout(AOutStream, 280, 0);
  dxfIntegerout(AOutStream, 281, AIsEntity);
end;

{ Объявляет прикладные классы, экземпляры которых этот модуль пишет в
  ENTITIES и OBJECTS. Без этих CLASS-записей AutoCAD не связывает
  TABLECONTENT/CELLSTYLEMAP с реализацией ObjectDBX и восстанавливает стили
  ячеек по встроенному правилу строк (issue #1409).

  В эталоне AutoCAD TABLECONTENT имеет два экземпляра класса на одну таблицу:
  данные самой таблицы и связанное содержимое сущности. }
procedure WriteAcadTableClassesToDXF(
  var AOutStream: TZctnrVectorBytes;
  var ADrawing: TSimpleDrawing;
  var AIODXFContext: TIODXFSaveContext);
var
  AcadTableCount, CellStyleMapCount: Integer;
  Entity: PGDBObjEntity;
  Iter: itrec;
begin
  AcadTableCount:=0;
  if ADrawing.pObjRoot<>nil then
  begin
    Entity:=ADrawing.pObjRoot^.ObjArray.beginiterate(Iter);
    while Entity<>nil do
    begin
      if Entity^.GetObjType=GDBAcadTableID then
        Inc(AcadTableCount);
      Entity:=ADrawing.pObjRoot^.ObjArray.iterate(Iter);
    end;
  end;
  CellStyleMapCount:=ADrawing.DXFTableStyleTable.Count;

  if AcadTableCount>0 then
  begin
    WriteAcadTableClassRecord(AOutStream,
      'ACAD_TABLE', 'AcDbTable', 1025, AcadTableCount, 1);
    WriteAcadTableClassRecord(AOutStream,
      'TABLECONTENT', 'AcDbTableContent', 1152,
      AcadTableCount*2, 0);
  end;
  if CellStyleMapCount>0 then
    WriteAcadTableClassRecord(AOutStream,
      'CELLSTYLEMAP', 'AcDbCellStyleMap', 1152,
      CellStyleMapCount, 0);
end;

procedure ResetAcadTableDXFWriteState;
var
  I: Integer;
begin
  for I := 0 to High(RoundTripRecords) do
    System.SetLength(RoundTripRecords[I].ContinuationHandles, 0);
  System.SetLength(RoundTripRecords, 0);
  for I := 0 to High(ContentRecords) do
  begin
    System.SetLength(ContentRecords[I].RowHeights, 0);
    System.SetLength(ContentRecords[I].ColWidths, 0);
    System.SetLength(ContentRecords[I].RowStyleIds, 0);
    System.SetLength(ContentRecords[I].CellStyleIds, 0);
    System.SetLength(ContentRecords[I].CellTexts, 0);
  end;
  System.SetLength(ContentRecords, 0);
end;

function BoolToDXF(AValue: Boolean): Integer;
begin
  if AValue then
    Result := 1
  else
    Result := 0;
end;

function BreakDirectionToDXF(
  ADirection: TAcadTableBreakDirection): Integer;
begin
  case ADirection of
    atbdDown:
      Result := 2;
    atbdLeft:
      Result := 3;
  else
    Result := 1;
  end;
end;

function NextAnonymousHandle(
  var AIODXFContext: TIODXFSaveContext): TDWGHandle;
begin
  Result := AIODXFContext.handle;
  Inc(AIODXFContext.handle);
end;

function EntityHandleForPart(
  const APart: TAcadTableDXFWritePart;
  var AIODXFContext: TIODXFSaveContext): TDWGHandle;
begin
  if APart.HandleKey <> nil then
    AIODXFContext.p2h.MyGetOrCreateValue(
      APart.HandleKey, AIODXFContext.handle, Result)
  else
    Result := NextAnonymousHandle(AIODXFContext);
end;

function RowHeightAt(
  const APart: TAcadTableDXFWritePart;
  ARow: Integer): Double;
begin
  if (ARow >= 0) and (ARow <= High(APart.RowHeights)) then
    Result := APart.RowHeights[ARow]
  else
    Result := CAcadTableDefaultRowHeight;
  if Result <= 0 then
    Result := CAcadTableDefaultRowHeight;
end;

function ColWidthAt(
  const APart: TAcadTableDXFWritePart;
  ACol: Integer): Double;
begin
  if (ACol >= 0) and (ACol <= High(APart.ColWidths)) then
    Result := APart.ColWidths[ACol]
  else
    Result := CAcadTableDefaultColWidth;
  if Result <= 0 then
    Result := CAcadTableDefaultColWidth;
end;

function CellTextAt(
  const APart: TAcadTableDXFWritePart;
  ARow, ACol: Integer): String;
var
  CellIndex: Integer;
begin
  Result := '';
  if APart.ColCount <= 0 then
    Exit;
  CellIndex := ARow * APart.ColCount + ACol;
  if (CellIndex >= 0) and
     (CellIndex <= High(APart.CellTexts)) then
    Result := APart.CellTexts[CellIndex];
  if (Result = '') and
     (ARow >= 0) and (ARow <= High(APart.Cells)) and
     (ACol >= 0) and (ACol <= High(APart.Cells[ARow])) then
    Result := APart.Cells[ARow][ACol].Text;
end;

function CellAlignmentAt(
  const APart: TAcadTableDXFWritePart;
  ARow, ACol: Integer): Integer;
begin
  Result := 0;
  if (ARow >= 0) and (ARow <= High(APart.Cells)) and
     (ACol >= 0) and (ACol <= High(APart.Cells[ARow])) then
    Result := APart.Cells[ARow][ACol].CellAlignment;
end;

function CellStyleTypeAt(
  const APart: TAcadTableDXFWritePart;
  ARow, ACol: Integer): Integer;
begin
  Result := -1;
  if (ARow >= 0) and (ARow <= High(APart.Cells)) and
     (ACol >= 0) and (ACol <= High(APart.Cells[ARow])) then
    Result := APart.Cells[ARow][ACol].StyleType;
  if (Result < 0) or (Result > 2) then
    if ARow < 2 then
      Result := ARow
    else
      Result := 2;
end;

function FindMergeForCell(
  const APart: TAcadTableDXFWritePart;
  ARow, ACol: Integer;
  out AMerge: TMergeRange): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to High(APart.Merges) do
    if (ARow >= APart.Merges[I].Row1) and
       (ARow <= APart.Merges[I].Row2) and
       (ACol >= APart.Merges[I].Col1) and
       (ACol <= APart.Merges[I].Col2) then
    begin
      AMerge := APart.Merges[I];
      Result := True;
      Exit;
    end;
end;

procedure GetCellSpanAndVirtualFlag(
  const APart: TAcadTableDXFWritePart;
  ARow, ACol: Integer;
  out AColSpan, ARowSpan: Integer;
  out AIsVirtual: Boolean);
var
  MergeRange: TMergeRange;
begin
  AColSpan := 1;
  ARowSpan := 1;
  AIsVirtual := False;

  if FindMergeForCell(APart, ARow, ACol, MergeRange) then
  begin
    if (ARow = MergeRange.Row1) and
       (ACol = MergeRange.Col1) then
    begin
      AColSpan := MergeRange.Col2 - MergeRange.Col1 + 1;
      ARowSpan := MergeRange.Row2 - MergeRange.Row1 + 1;
    end
    else
      AIsVirtual := True;
  end
  else if (ARow >= 0) and (ARow <= High(APart.Cells)) and
          (ACol >= 0) and (ACol <= High(APart.Cells[ARow])) then
  begin
    if APart.Cells[ARow][ACol].ColSpan > 1 then
      AColSpan := APart.Cells[ARow][ACol].ColSpan;
    if APart.Cells[ARow][ACol].RowSpan > 1 then
      ARowSpan := APart.Cells[ARow][ACol].RowSpan;
  end;
end;

// Преобразует внутренний StyleType ячейки (0=Title, 1=Header, 2=Data)
// в идентификатор стиля ячейки AutoCAD (1=_TITLE, 2=_HEADER, 3=_DATA).
function AcadCellStyleId(AStyleType: Integer): Integer;
begin
  if (AStyleType < 0) or (AStyleType > 2) then
    AStyleType := 2;
  Result := AStyleType + CAcadCellStyleIdTitle;
end;

// Собирает данные для объекта TABLECONTENT данной части таблицы и выдаёт
// хэндлы расширенного словаря, XRECORD и самого TABLECONTENT (issue #1409).
// Возвращает False, если для части нечего писать (пустая таблица).
function AddAcadTableContentRecord(
  const APart: TAcadTableDXFWritePart;
  AEntityHandle: TDWGHandle;
  var AIODXFContext: TIODXFSaveContext;
  out ADictionaryHandle: TDWGHandle): Boolean;
var
  RecIdx, RowIdx, ColIdx: Integer;
begin
  Result := False;
  ADictionaryHandle := 0;
  if (APart.RowCount <= 0) or (APart.ColCount <= 0) then
    Exit;

  RecIdx := Length(ContentRecords);
  System.SetLength(ContentRecords, RecIdx + 1);
  with ContentRecords[RecIdx] do
  begin
    EntityHandle := AEntityHandle;
    DictionaryHandle := NextAnonymousHandle(AIODXFContext);
    XRecordHandle := NextAnonymousHandle(AIODXFContext);
    ContentHandle := NextAnonymousHandle(AIODXFContext);
    TableStyleHandle := APart.TableStyleHandle;
    TableStyleName := APart.TableStyleName;
    RowCount := APart.RowCount;
    ColCount := APart.ColCount;

    System.SetLength(RowHeights, RowCount);
    System.SetLength(RowStyleIds, RowCount);
    System.SetLength(ColWidths, ColCount);
    System.SetLength(CellStyleIds, RowCount * ColCount);
    System.SetLength(CellTexts, RowCount * ColCount);

    for ColIdx := 0 to ColCount - 1 do
      ColWidths[ColIdx] := ColWidthAt(APart, ColIdx);

    for RowIdx := 0 to RowCount - 1 do
    begin
      RowHeights[RowIdx] := RowHeightAt(APart, RowIdx);
      { Стиль строки берём по первой ячейке — он всё равно перекрывается
        индивидуальным стилем каждой ячейки ниже. }
      RowStyleIds[RowIdx] :=
        AcadCellStyleId(CellStyleTypeAt(APart, RowIdx, 0));
      for ColIdx := 0 to ColCount - 1 do
      begin
        CellStyleIds[RowIdx * ColCount + ColIdx] :=
          AcadCellStyleId(CellStyleTypeAt(APart, RowIdx, ColIdx));
        CellTexts[RowIdx * ColCount + ColIdx] :=
          CellTextAt(APart, RowIdx, ColIdx);
      end;
    end;

    ADictionaryHandle := DictionaryHandle;
  end;
  Result := True;
end;

// Возвращает актуальный хэндл объекта TABLESTYLE для ссылок 342/340.
// Приоритет — карта «имя стиля -> новый хэндл», заполняемая при записи
// секции OBJECTS: хэндлы стилей перенумеровываются при сохранении, поэтому
// сохранённый в модели TableStyleHandle почти всегда устарел (issue #1409).
function ResolveTableStyleHandle(
  var AIODXFContext: TIODXFSaveContext;
  const ATableStyleName, AFallbackHandle: String): String;
var
  MappedValue: String;
begin
  if ATableStyleName <> '' then
    if AIODXFContext.TableStyleNameHandleMap.MyGetValue(
         ATableStyleName, MappedValue) then
      if MappedValue <> '' then
      begin
        Result := MappedValue;
        Exit;
      end;
  Result := AFallbackHandle;
end;

procedure WriteEntityPrefix(
  var AOutStream: TZctnrVectorBytes;
  var AIODXFContext: TIODXFSaveContext;
  const APart: TAcadTableDXFWritePart;
  out AHandle: TDWGHandle);
var
  DictionaryHandle: TDWGHandle;
begin
  AHandle := EntityHandleForPart(APart, AIODXFContext);

  dxfStringWithoutEncodeOut(AOutStream, 0, 'ACAD_TABLE');
  dxfStringWithoutEncodeOut(AOutStream, 5, IntToHex(AHandle, 0));
  { Расширенный словарь сущности со ссылкой на round-trip XRECORD, через
    который AutoCAD получает TABLECONTENT с индивидуальными стилями ячеек
    (issue #1409). Сами объекты пишутся позже, в секцию OBJECTS. }
  if AddAcadTableContentRecord(APart, AHandle, AIODXFContext,
       DictionaryHandle) then
  begin
    dxfStringWithoutEncodeOut(AOutStream, 102, '{ACAD_XDICTIONARY');
    dxfStringWithoutEncodeOut(AOutStream, 360,
      IntToHex(DictionaryHandle, 0));
    dxfStringWithoutEncodeOut(AOutStream, 102, '}');
  end;
  // Владелец сущности — BLOCK_RECORD пространства модели. Без группы 330
  // AutoCAD не принимает round-trip таблицу (issue #1409).
  if AIODXFContext.AcadTableOwnerHandle > 0 then
    dxfStringWithoutEncodeOut(AOutStream, 330,
      IntToHex(AIODXFContext.AcadTableOwnerHandle, 0));
  dxfStringWithoutEncodeOut(AOutStream, 100, dxfName_AcDbEntity);
  if APart.LayerName <> '' then
    dxfStringout(AOutStream, 8, APart.LayerName,
      AIODXFContext.Header)
  else
    dxfStringout(AOutStream, 8, '0', AIODXFContext.Header);
  if APart.Color <> ClByLayer then
    dxfIntegerout(AOutStream, 62, APart.Color);
  if APart.LineWeight <> -1 then
    dxfIntegerout(AOutStream, 370, APart.LineWeight);
  if APart.LineTypeName <> '' then
    dxfStringout(AOutStream, 6, APart.LineTypeName,
      AIODXFContext.Header);
  if APart.LineTypeScale <> 1 then
    dxfDoubleout(AOutStream, 48, APart.LineTypeScale);
end;

procedure WriteBlockReference(
  var AOutStream: TZctnrVectorBytes;
  const APart: TAcadTableDXFWritePart;
  APartIndex: Integer);
var
  BlockName: String;
begin
  dxfStringWithoutEncodeOut(AOutStream, 100, 'AcDbBlockReference');
  // Имя блока (group code 2). Если перед сохранением был сгенерирован
  // персональный блок этой части (issue #1381), используем его имя — тогда
  // AutoCAD отрисует часть по геометрии этого блока. Иначе — историческое
  // имя *T<n> (part index + 1).
  if APart.BlockName <> '' then
    BlockName := APart.BlockName
  else
    BlockName := Format('*T%d', [APartIndex + 1]);
  dxfStringWithoutEncodeOut(AOutStream, 2, BlockName);
  dxfvertexout(AOutStream, 10, APart.InsertPoint);
end;

procedure WriteTableHeader(
  var AOutStream: TZctnrVectorBytes;
  var AIODXFContext: TIODXFSaveContext;
  const APart: TAcadTableDXFWritePart);
var
  BlockRecordHandle, TableStyleHandle: String;
begin
  dxfStringWithoutEncodeOut(AOutStream, 100, 'AcDbTable');
  TableStyleHandle := ResolveTableStyleHandle(AIODXFContext,
    APart.TableStyleName, APart.TableStyleHandle);
  if TableStyleHandle <> '' then
    dxfStringWithoutEncodeOut(AOutStream, 342, TableStyleHandle);
  // Ссылка на BLOCK_RECORD анонимного блока таблицы (group code 343). AutoCAD
  // отрисовывает proxy/roundtrip AcDbTable по связанному блоку, поэтому без
  // 343 части разорванной таблицы открываются пустыми/битыми (issue #1381).
  // Хэндл берём из карты «имя блока → хэндл BLOCK_RECORD», заполняемой при
  // записи секции BLOCK_RECORD (до секции ENTITIES).
  if APart.BlockName <> '' then
    if AIODXFContext.BlockNameHandleMap.MyGetValue(
         APart.BlockName, BlockRecordHandle) then
      dxfStringWithoutEncodeOut(AOutStream, 343, BlockRecordHandle);
  dxfvertexout(AOutStream, 11, APart.Direction);
  dxfIntegerout(AOutStream, 90, APart.TableFlags);
  dxfIntegerout(AOutStream, 91, APart.RowCount);
  dxfIntegerout(AOutStream, 92, APart.ColCount);
  dxfIntegerout(AOutStream, 93, 0);
  dxfIntegerout(AOutStream, 94, 0);
  dxfIntegerout(AOutStream, 95, 0);
  dxfIntegerout(AOutStream, 96, 0);

  if APart.BreakEnabled or (APart.BreakSpacing <> 0) then
  begin
    dxfIntegerout(AOutStream, 292, BoolToDXF(APart.BreakEnabled));
    dxfIntegerout(AOutStream, 282,
      BreakDirectionToDXF(APart.BreakDirection));
    dxfIntegerout(AOutStream, 291,
      BoolToDXF(APart.BreakRepeatTopLabels));
    dxfIntegerout(AOutStream, 294,
      BoolToDXF(APart.BreakRepeatBottomLabels));
    dxfIntegerout(AOutStream, 293,
      BoolToDXF(APart.BreakManualPosition));
    dxfIntegerout(AOutStream, 295,
      BoolToDXF(APart.BreakManualHeight));
    dxfDoubleout(AOutStream, 146, APart.BreakSpacing);
  end;
end;

procedure WriteTableDimensions(
  var AOutStream: TZctnrVectorBytes;
  const APart: TAcadTableDXFWritePart);
var
  I: Integer;
begin
  for I := 0 to APart.RowCount - 1 do
    dxfDoubleout(AOutStream, 141, RowHeightAt(APart, I));
  for I := 0 to APart.ColCount - 1 do
    dxfDoubleout(AOutStream, 142, ColWidthAt(APart, I));
end;

procedure WriteCell(
  var AOutStream: TZctnrVectorBytes;
  var AIODXFContext: TIODXFSaveContext;
  const APart: TAcadTableDXFWritePart;
  ARow, ACol: Integer);
var
  CellText: String;
  ColSpan, RowSpan: Integer;
  IsVirtual: Boolean;
  Alignment, StyleType: Integer;
begin
  CellText := CellTextAt(APart, ARow, ACol);
  GetCellSpanAndVirtualFlag(APart, ARow, ACol,
    ColSpan, RowSpan, IsVirtual);
  Alignment := CellAlignmentAt(APart, ARow, ACol);
  StyleType := CellStyleTypeAt(APart, ARow, ACol);

  dxfIntegerout(AOutStream, 171, 1);
  dxfIntegerout(AOutStream, 172, StyleType);
  dxfIntegerout(AOutStream, 173, BoolToDXF(IsVirtual));
  dxfIntegerout(AOutStream, 174, 0);
  dxfIntegerout(AOutStream, 175, ColSpan);
  dxfIntegerout(AOutStream, 176, RowSpan);
  // The low bit marks that the cell has an explicit property override.
  // Without it AutoCAD ignores group 170 and falls back to the table style.
  if Alignment <> 0 then
    dxfIntegerout(AOutStream, 91, 262145)
  else
    dxfIntegerout(AOutStream, 91, 262144);
  dxfIntegerout(AOutStream, 178, 0);
  dxfDoubleout(AOutStream, 145, 0);
  // AutoCAD хранит выравнивание после служебных данных ячейки и перед
  // group 92. Ранняя запись group 170 игнорировалась при открытии файла,
  // поэтому ячейка возвращалась к выравниванию стиля таблицы (issue #1396).
  if Alignment <> 0 then
    dxfIntegerout(AOutStream, 170, Alignment);
  dxfIntegerout(AOutStream, 92, 0);
  dxfStringWithoutEncodeOut(AOutStream, 301, 'CELL_VALUE');
  if CellText <> '' then
    dxfIntegerout(AOutStream, 93, 6)
  else
    dxfIntegerout(AOutStream, 93, 0);
  dxfIntegerout(AOutStream, 90, 4);
  if CellText <> '' then
    dxfStringout(AOutStream, 1, CellText, AIODXFContext.Header);
  dxfIntegerout(AOutStream, 94, 0);
  dxfStringWithoutEncodeOut(AOutStream, 300, '');
  dxfStringout(AOutStream, 302, CellText, AIODXFContext.Header);
  dxfStringWithoutEncodeOut(AOutStream, 304, 'ACVALUE_END');
end;

procedure WriteTableCells(
  var AOutStream: TZctnrVectorBytes;
  var AIODXFContext: TIODXFSaveContext;
  const APart: TAcadTableDXFWritePart);
var
  RowIdx, ColIdx: Integer;
begin
  for RowIdx := 0 to APart.RowCount - 1 do
    for ColIdx := 0 to APart.ColCount - 1 do
      WriteCell(AOutStream, AIODXFContext, APart,
        RowIdx, ColIdx);
end;

procedure AddRoundTripRecord(
  AMainHandle: TDWGHandle;
  const AContinuationHandles: array of TDWGHandle;
  ABreakSpacing, ABreakHeight: Double;
  ABreakManualPosition, ABreakManualHeight: Boolean);
var
  RecIdx, HandleIdx: Integer;
begin
  if Length(AContinuationHandles) = 0 then
    Exit;

  RecIdx := Length(RoundTripRecords);
  System.SetLength(RoundTripRecords, RecIdx + 1);
  RoundTripRecords[RecIdx].MainHandle := AMainHandle;
  RoundTripRecords[RecIdx].BreakSpacing := ABreakSpacing;
  RoundTripRecords[RecIdx].BreakHeight := ABreakHeight;
  RoundTripRecords[RecIdx].BreakManualPosition := ABreakManualPosition;
  RoundTripRecords[RecIdx].BreakManualHeight := ABreakManualHeight;
  System.SetLength(
    RoundTripRecords[RecIdx].ContinuationHandles,
    Length(AContinuationHandles));
  for HandleIdx := 0 to High(AContinuationHandles) do
    RoundTripRecords[RecIdx].ContinuationHandles[HandleIdx] :=
      AContinuationHandles[HandleIdx];
end;

function RawAcadTableHandle(
  const ARawEntity: String; out AHandle: TDWGHandle): Boolean;
var
  Lines: TStringList;
  I: Integer;
begin
  Result := False;
  AHandle := 0;
  if ARawEntity = '' then
    Exit;

  Lines := TStringList.Create;
  try
    Lines.Text := ARawEntity;
    I := 0;
    while I < Lines.Count - 1 do
    begin
      if Trim(Lines[I]) = '5' then
      begin
        try
          AHandle := DXFHandle(Trim(Lines[I + 1]));
          Result := AHandle <> 0;
        except
          AHandle := 0;
          Result := False;
        end;
        Exit;
      end;
      Inc(I, 2);
    end;
  finally
    Lines.Free;
  end;
end;

// Записывает сырую (raw) сущность ACAD_TABLE, переписывая ссылки на хэндлы
// под актуальную (перенумерованную) нумерацию сохраняемого файла (issue #1339):
//   5   — хэндл самой сущности (перенумеровывается в ANewHandle, issue #1344);
//   330 — владелец сущности (*Model_Space);
//   342 — стиль таблицы (по имени стиля ATableStyleName);
//   343 — анонимный блок таблицы (по имени блока из предшествующей пары 2);
//   блок расширенного словаря 102/ACAD_XDICTIONARY удаляется, т.к. он не
//        круглорейсится и иначе оставляет висячую ссылку (360).
procedure WriteRawEntityText(
  var AOutStream: TZctnrVectorBytes;
  var AIODXFContext: TIODXFSaveContext;
  const ATableStyleName: String;
  const ARawEntity: String;
  ANewHandle: TDWGHandle);
var
  Lines: TStringList;
  I: Integer;
  Code, OutValue, LastBlockName, MappedValue: String;
  HandleRewritten: Boolean;
begin
  Lines := TStringList.Create;
  try
    Lines.Text := ARawEntity;
    LastBlockName := '';
    HandleRewritten := False;
    I := 0;
    while I < Lines.Count do
    begin
      Code := Trim(Lines[I]);

      { Удаляем блок расширенного словаря целиком (вместе с висячим 360). }
      if (Code = '102') and (I + 1 < Lines.Count)
         and (Trim(Lines[I + 1]) = '{ACAD_XDICTIONARY') then
      begin
        Inc(I, 2);
        while I + 1 < Lines.Count do
        begin
          if (Trim(Lines[I]) = '102') and (Trim(Lines[I + 1]) = '}') then
          begin
            Inc(I, 2);
            Break;
          end;
          Inc(I, 2);
        end;
        Continue;
      end;

      if I + 1 >= Lines.Count then
      begin
        { Непарная завершающая строка — пишем как есть. }
        AOutStream.TXTAddStringEOL(Lines[I]);
        Inc(I);
        Continue;
      end;

      OutValue := Lines[I + 1];

      if Code = '2' then
        LastBlockName := Trim(Lines[I + 1])
      else if (Code = '5') and (not HandleRewritten) then
      begin
        { Перенумеровываем хэндл самой сущности (группа 5) под актуальную
          нумерацию сохраняемого файла (issue #1344). Исходный хэндл из
          файла AutoCAD может совпасть с хэндлом, который уже был выдан
          анонимному блоку таблицы при записи секции BLOCKS (там хэндлы
          назначаются заново, последовательно). При совпадении AutoCAD
          сообщает «Неверная метка <handle>: уже используется» и
          отказывается открывать файл. Свежий хэндл из общего счётчика
          документа исключает это столкновение. Переписываем только первый
          код 5 (хэндл сущности), значения ячеек (302="5") не затрагиваем. }
        OutValue := IntToHex(ANewHandle, 0);
        HandleRewritten := True;
      end
      else if Code = '330' then
      begin
        if AIODXFContext.AcadTableOwnerHandle <> 0 then
          OutValue := IntToHex(AIODXFContext.AcadTableOwnerHandle, 0);
      end
      else if Code = '342' then
      begin
        if (ATableStyleName <> '')
           and AIODXFContext.TableStyleNameHandleMap.MyGetValue(
             ATableStyleName, MappedValue) then
          OutValue := MappedValue;
      end
      else if Code = '343' then
      begin
        if (LastBlockName <> '')
           and AIODXFContext.BlockNameHandleMap.MyGetValue(
             LastBlockName, MappedValue) then
          OutValue := MappedValue;
      end;

      AOutStream.TXTAddStringEOL(Lines[I]);
      AOutStream.TXTAddStringEOL(OutValue);
      Inc(I, 2);
    end;
  finally
    Lines.Free;
  end;
end;

procedure WriteAcadTablePartToDXF(
  var AOutStream: TZctnrVectorBytes;
  var ADrawing: TDrawingDef;
  var AIODXFContext: TIODXFSaveContext;
  const APart: TAcadTableDXFWritePart;
  APartIndex: Integer;
  out AEntityHandle: TDWGHandle);
begin
  WriteEntityPrefix(AOutStream, AIODXFContext, APart,
    AEntityHandle);
  WriteBlockReference(AOutStream, APart, APartIndex);
  WriteTableHeader(AOutStream, AIODXFContext, APart);
  WriteTableDimensions(AOutStream, APart);
  WriteTableCells(AOutStream, AIODXFContext, APart);

  programlog.LogOutFormatStr(
    'AcadTable: dxf_write: exported rows=%d cols=%d handle=%s',
    [APart.RowCount, APart.ColCount, IntToHex(AEntityHandle, 0)],
    LM_Info);
end;

procedure WriteAcadTableToDXF(
  var AOutStream: TZctnrVectorBytes;
  var ADrawing: TDrawingDef;
  var AIODXFContext: TIODXFSaveContext;
  const APart: TAcadTableDXFWritePart;
  APartIndex: Integer = 0);
var
  EntityHandle: TDWGHandle;
begin
  WriteAcadTablePartToDXF(AOutStream, ADrawing, AIODXFContext,
    APart, APartIndex, EntityHandle);
end;

procedure WriteAcadTableContinuationPartsToDXF(
  var AOutStream: TZctnrVectorBytes;
  var ADrawing: TDrawingDef;
  var AIODXFContext: TIODXFSaveContext;
  const AMainPart: TAcadTableDXFWritePart;
  const AParts: TAcadTableDXFWritePartArray);
var
  MainHandle: TDWGHandle;
  ContinuationHandles: array of TDWGHandle;
  ContinuationHandle: TDWGHandle;
  PartIdx: Integer;
begin
  if Length(AParts) = 0 then
    Exit;

  if AMainPart.HandleKey <> nil then
    AIODXFContext.p2h.MyGetOrCreateValue(
      AMainPart.HandleKey, AIODXFContext.handle, MainHandle)
  else
    Exit;

  System.SetLength(ContinuationHandles, Length(AParts));
  for PartIdx := 0 to High(AParts) do
  begin
    WriteAcadTablePartToDXF(AOutStream, ADrawing, AIODXFContext,
      AParts[PartIdx], PartIdx + 1, ContinuationHandle);
    ContinuationHandles[PartIdx] := ContinuationHandle;
  end;

  AddRoundTripRecord(MainHandle, ContinuationHandles,
    AMainPart.BreakSpacing, AMainPart.BreakHeight,
    AMainPart.BreakManualPosition, AMainPart.BreakManualHeight);
end;

function WriteRawAcadTablePartsToDXF(
  var AOutStream: TZctnrVectorBytes;
  var AIODXFContext: TIODXFSaveContext;
  const AMainRawEntity: String;
  const AContinuationRawEntities: array of String;
  ABreakSpacing, ABreakHeight: Double;
  ABreakManualPosition, ABreakManualHeight: Boolean;
  const ATableStyleName: String): Boolean;
var
  MainHandle: TDWGHandle;
  ContinuationHandles: array of TDWGHandle;
  PartIdx: Integer;
begin
  Result := False;
  { Валидируем raw-сущности: у каждой части должен быть исходный хэндл
    (группа 5). Если нет — это не пригодная для round-trip сущность, и
    управление возвращается обычному (модельному) сохранению. Сами
    исходные значения хэндлов далее не используются — каждая часть
    получает свежий хэндл (issue #1344). }
  if not RawAcadTableHandle(AMainRawEntity, MainHandle) then
    Exit;

  System.SetLength(ContinuationHandles, Length(AContinuationRawEntities));
  for PartIdx := 0 to High(AContinuationRawEntities) do
    if not RawAcadTableHandle(
      AContinuationRawEntities[PartIdx],
      ContinuationHandles[PartIdx]) then
      Exit;

  { Выдаём свежие хэндлы из общего счётчика документа. К моменту записи
    секции ENTITIES счётчик уже прошёл секцию BLOCKS, поэтому новые хэндлы
    гарантированно не сталкиваются с хэндлами анонимных блоков таблицы
    (issue #1344). Эти же значения попадают в round-trip XRECORD (360/330/
    361), оставаясь согласованными с группой 5 сущностей. }
  MainHandle := NextAnonymousHandle(AIODXFContext);
  WriteRawEntityText(AOutStream, AIODXFContext, ATableStyleName,
    AMainRawEntity, MainHandle);

  for PartIdx := 0 to High(AContinuationRawEntities) do
  begin
    ContinuationHandles[PartIdx] := NextAnonymousHandle(AIODXFContext);
    WriteRawEntityText(AOutStream, AIODXFContext, ATableStyleName,
      AContinuationRawEntities[PartIdx], ContinuationHandles[PartIdx]);
  end;

  AddRoundTripRecord(MainHandle, ContinuationHandles,
    ABreakSpacing, ABreakHeight,
    ABreakManualPosition, ABreakManualHeight);
  Result := True;
end;

procedure WriteRoundTripRecordToDXF(
  var AOutStream: TZctnrVectorBytes;
  var AIODXFContext: TIODXFSaveContext;
  const ARecord: TAcadTableRoundTripRecord);
const
  { Базовые биты BreakOption: EnableBreaks(1) + RepeatTopLabels(2) = 3. }
  cTableBreakBaseFlags = 3;
  cTableBreakAllowManualPositions = 8;
  cTableBreakAllowManualHeights = 16;
var
  ObjectHandle: TDWGHandle;
  HandleIdx: Integer;
  BreakOptionFlags: Integer;
begin
  ObjectHandle := NextAnonymousHandle(AIODXFContext);

  { Восстанавливаем BreakOption-флаг из признаков ручного управления
    разрывами (issue #1339). Раньше здесь была жёстко зашита 3, из-за чего
    при пересохранении терялись AllowManualPositions/AllowManualHeights. }
  BreakOptionFlags := cTableBreakBaseFlags;
  if ARecord.BreakManualPosition then
    BreakOptionFlags := BreakOptionFlags or cTableBreakAllowManualPositions;
  if ARecord.BreakManualHeight then
    BreakOptionFlags := BreakOptionFlags or cTableBreakAllowManualHeights;

  dxfStringWithoutEncodeOut(AOutStream, 0, 'XRECORD');
  dxfStringWithoutEncodeOut(AOutStream, 5, IntToHex(ObjectHandle, 0));
  dxfStringWithoutEncodeOut(AOutStream, 330, '0');
  dxfStringWithoutEncodeOut(AOutStream, 100, 'AcDbXrecord');
  dxfIntegerout(AOutStream, 280, 1);
  { Пишем приватный маркер ZCAD вместо «родного» ACAD-маркера (issue #1381).
    AutoCAD не распознаёт его и показывает части разорванной таблицы как
    несколько отдельных таблиц (как и было сохранено в ZCAD), а не
    пересобирает их в одну цельную. ZCAD по этому маркеру восстанавливает
    единую разорванную таблицу при повторной загрузке. }
  dxfStringWithoutEncodeOut(AOutStream, 102,
    CAcadTableSplitMarkerName);
  dxfStringWithoutEncodeOut(AOutStream, 360,
    IntToHex(ARecord.MainHandle, 0));
  dxfIntegerout(AOutStream, 70, 1);
  dxfIntegerout(AOutStream, 90, BreakOptionFlags);
  dxfIntegerout(AOutStream, 90, 1);
  dxfDoubleout(AOutStream, 40, ARecord.BreakSpacing);
  dxfIntegerout(AOutStream, 90, 2);
  dxfIntegerout(AOutStream, 90, 0);
  dxfIntegerout(AOutStream, 90, 1);
  dxfDoubleout(AOutStream, 10, 0);
  dxfDoubleout(AOutStream, 20, 0);
  dxfDoubleout(AOutStream, 30, 0);
  dxfDoubleout(AOutStream, 40, ARecord.BreakHeight);
  for HandleIdx := 0 to High(ARecord.ContinuationHandles) do
    dxfStringWithoutEncodeOut(AOutStream, 330,
      IntToHex(ARecord.ContinuationHandles[HandleIdx], 0));
  dxfStringWithoutEncodeOut(AOutStream, 361,
    IntToHex(ARecord.MainHandle, 0));
end;

// Нейтральный (без переопределений) блок CONTENTFORMAT: содержимое ячейки
// полностью наследует свойства стиля ячейки.
procedure WriteContentFormat(var AOutStream: TZctnrVectorBytes);
begin
  dxfStringWithoutEncodeOut(AOutStream, 300, 'CONTENTFORMAT');
  dxfStringWithoutEncodeOut(AOutStream, 1, 'CONTENTFORMAT_BEGIN');
  dxfIntegerout(AOutStream, 90, 0);
  dxfIntegerout(AOutStream, 91, 0);
  dxfIntegerout(AOutStream, 92, 512);
  dxfIntegerout(AOutStream, 93, 0);
  dxfStringWithoutEncodeOut(AOutStream, 300, '');
  dxfDoubleout(AOutStream, 40, 0);
  dxfDoubleout(AOutStream, 140, 1);
  dxfIntegerout(AOutStream, 94, 1);
  dxfIntegerout(AOutStream, 62, 0);
  dxfStringWithoutEncodeOut(AOutStream, 340, '0');
  dxfDoubleout(AOutStream, 144, 0.18);
  dxfStringWithoutEncodeOut(AOutStream, 309, 'CONTENTFORMAT_END');
end;

// Пустой DATAMAP (нет пользовательских данных).
procedure WriteEmptyDataMap(var AOutStream: TZctnrVectorBytes);
begin
  dxfStringWithoutEncodeOut(AOutStream, 301, 'CUSTOMDATA');
  dxfStringWithoutEncodeOut(AOutStream, 1, 'DATAMAP_BEGIN');
  dxfIntegerout(AOutStream, 90, 0);
  dxfStringWithoutEncodeOut(AOutStream, 309, 'DATAMAP_END');
end;

// Контрольная сумма содержимого ячейки, которую AutoCAD хранит в
// ACAD_ROUNDTRIP_2008_CELL_CHECKSUM: сумма кодов символов текста ячейки.
// Проверено на файле, пересохранённом AutoCAD: для односимвольных ячеек
// значение равно коду символа Unicode ('1'=49, 'ф'=1092 и т.д.).
function CellTextChecksum(const AText: String): Int64;
var
  U: UnicodeString;
  I: Integer;
begin
  Result := 0;
  U := UTF8Decode(AText);
  for I := 1 to Length(U) do
    Result := Result + Ord(U[I]);
end;

// DATAMAP ячейки с контрольной суммой её содержимого. AutoCAD сверяет эту
// величину при открытии round-trip таблицы (issue #1409).
procedure WriteCellChecksumDataMap(
  var AOutStream: TZctnrVectorBytes; const AText: String);
begin
  dxfStringWithoutEncodeOut(AOutStream, 301, 'CUSTOMDATA');
  dxfStringWithoutEncodeOut(AOutStream, 1, 'DATAMAP_BEGIN');
  dxfIntegerout(AOutStream, 90, 1);
  dxfStringWithoutEncodeOut(AOutStream, 300,
    'ACAD_ROUNDTRIP_2008_CELL_CHECKSUM');
  dxfStringWithoutEncodeOut(AOutStream, 301, 'DATAMAP_VALUE');
  dxfIntegerout(AOutStream, 93, 2);
  dxfIntegerout(AOutStream, 90, 2);
  dxfDoubleout(AOutStream, 140, CellTextChecksum(AText));
  dxfIntegerout(AOutStream, 94, 0);
  dxfStringWithoutEncodeOut(AOutStream, 300, '');
  dxfStringWithoutEncodeOut(AOutStream, 302, '');
  dxfStringWithoutEncodeOut(AOutStream, 304, 'ACVALUE_END');
  dxfStringWithoutEncodeOut(AOutStream, 309, 'DATAMAP_END');
end;

// Нейтральный TABLEFORMAT (170 = 0: переопределений свойств нет).
// AType: 1 — ячейка, 2 — строка, 3 — столбец, 4 — таблица.
procedure WriteEmptyTableFormat(
  var AOutStream: TZctnrVectorBytes; AType: Integer);
begin
  dxfStringWithoutEncodeOut(AOutStream, 1, 'TABLEFORMAT_BEGIN');
  dxfIntegerout(AOutStream, 90, AType);
  dxfIntegerout(AOutStream, 170, 0);
  dxfStringWithoutEncodeOut(AOutStream, 309, 'TABLEFORMAT_END');
end;

procedure WriteTableContentCell(
  var AOutStream: TZctnrVectorBytes;
  var AIODXFContext: TIODXFSaveContext;
  const AText: String;
  ACellStyleId: Integer);
begin
  dxfStringWithoutEncodeOut(AOutStream, 300, 'CELL');
  dxfStringWithoutEncodeOut(AOutStream, 1, 'LINKEDTABLEDATACELL_BEGIN');
  dxfIntegerout(AOutStream, 90, 0);
  dxfStringWithoutEncodeOut(AOutStream, 300, '');
  dxfIntegerout(AOutStream, 91, 0);
  WriteCellChecksumDataMap(AOutStream, AText);
  dxfIntegerout(AOutStream, 92, 0);
  if AText <> '' then
  begin
    dxfIntegerout(AOutStream, 95, 1);
    dxfStringWithoutEncodeOut(AOutStream, 302, 'CONTENT');
    dxfStringWithoutEncodeOut(AOutStream, 1, 'CELLCONTENT_BEGIN');
    dxfIntegerout(AOutStream, 90, 1);
    dxfStringWithoutEncodeOut(AOutStream, 300, 'VALUE');
    dxfIntegerout(AOutStream, 93, 6);
    dxfIntegerout(AOutStream, 90, 4);
    dxfStringout(AOutStream, 1, AText, AIODXFContext.Header);
    dxfIntegerout(AOutStream, 94, 0);
    dxfStringWithoutEncodeOut(AOutStream, 300, '');
    dxfStringout(AOutStream, 302, AText, AIODXFContext.Header);
    dxfStringWithoutEncodeOut(AOutStream, 304, 'ACVALUE_END');
    dxfIntegerout(AOutStream, 91, 0);
    dxfStringWithoutEncodeOut(AOutStream, 309, 'CELLCONTENT_END');
    dxfStringWithoutEncodeOut(AOutStream, 1,
      'FORMATTEDCELLCONTENT_BEGIN');
    dxfIntegerout(AOutStream, 170, 1);
    WriteContentFormat(AOutStream);
    dxfStringWithoutEncodeOut(AOutStream, 309,
      'FORMATTEDCELLCONTENT_END');
  end
  else
    dxfIntegerout(AOutStream, 95, 0);
  dxfStringWithoutEncodeOut(AOutStream, 309, 'LINKEDTABLEDATACELL_END');

  dxfStringWithoutEncodeOut(AOutStream, 1,
    'FORMATTEDTABLEDATACELL_BEGIN');
  dxfStringWithoutEncodeOut(AOutStream, 300, 'CELLTABLEFORMAT');
  WriteEmptyTableFormat(AOutStream, 1);
  dxfStringWithoutEncodeOut(AOutStream, 309,
    'FORMATTEDTABLEDATACELL_END');

  { Ключевая для issue #1409 запись: индивидуальный стиль ячейки. }
  dxfStringWithoutEncodeOut(AOutStream, 1, 'TABLECELL_BEGIN');
  dxfIntegerout(AOutStream, 90, ACellStyleId);
  dxfIntegerout(AOutStream, 91, 0);
  dxfStringWithoutEncodeOut(AOutStream, 309, 'TABLECELL_END');
end;

procedure WriteTableContentObject(
  var AOutStream: TZctnrVectorBytes;
  var AIODXFContext: TIODXFSaveContext;
  const ARecord: TAcadTableContentRecord);
var
  RowIdx, ColIdx: Integer;
  TableStyleHandle: String;
begin
  dxfStringWithoutEncodeOut(AOutStream, 0, 'TABLECONTENT');
  dxfStringWithoutEncodeOut(AOutStream, 5,
    IntToHex(ARecord.ContentHandle, 0));
  dxfStringWithoutEncodeOut(AOutStream, 330,
    IntToHex(ARecord.XRecordHandle, 0));
  dxfStringWithoutEncodeOut(AOutStream, 100, 'AcDbLinkedData');
  dxfStringWithoutEncodeOut(AOutStream, 1, '');
  dxfStringWithoutEncodeOut(AOutStream, 300, '');
  dxfStringWithoutEncodeOut(AOutStream, 100, 'AcDbLinkedTableData');

  dxfIntegerout(AOutStream, 90, ARecord.ColCount);
  for ColIdx := 0 to ARecord.ColCount - 1 do
  begin
    dxfStringWithoutEncodeOut(AOutStream, 300, 'COLUMN');
    dxfStringWithoutEncodeOut(AOutStream, 1,
      'LINKEDTABLEDATACOLUMN_BEGIN');
    dxfStringWithoutEncodeOut(AOutStream, 300, '');
    dxfIntegerout(AOutStream, 91, 0);
    WriteEmptyDataMap(AOutStream);
    dxfStringWithoutEncodeOut(AOutStream, 309,
      'LINKEDTABLEDATACOLUMN_END');
    dxfStringWithoutEncodeOut(AOutStream, 1,
      'FORMATTEDTABLEDATACOLUMN_BEGIN');
    dxfStringWithoutEncodeOut(AOutStream, 300, 'COLUMNTABLEFORMAT');
    WriteEmptyTableFormat(AOutStream, 3);
    dxfStringWithoutEncodeOut(AOutStream, 309,
      'FORMATTEDTABLEDATACOLUMN_END');
    dxfStringWithoutEncodeOut(AOutStream, 1, 'TABLECOLUMN_BEGIN');
    dxfIntegerout(AOutStream, 90, 0);
    dxfDoubleout(AOutStream, 40, ARecord.ColWidths[ColIdx]);
    dxfStringWithoutEncodeOut(AOutStream, 309, 'TABLECOLUMN_END');
  end;

  dxfIntegerout(AOutStream, 91, ARecord.RowCount);
  for RowIdx := 0 to ARecord.RowCount - 1 do
  begin
    dxfStringWithoutEncodeOut(AOutStream, 301, 'ROW');
    dxfStringWithoutEncodeOut(AOutStream, 1,
      'LINKEDTABLEDATAROW_BEGIN');
    dxfIntegerout(AOutStream, 90, ARecord.ColCount);
    for ColIdx := 0 to ARecord.ColCount - 1 do
      WriteTableContentCell(AOutStream, AIODXFContext,
        ARecord.CellTexts[RowIdx * ARecord.ColCount + ColIdx],
        ARecord.CellStyleIds[RowIdx * ARecord.ColCount + ColIdx]);
    dxfIntegerout(AOutStream, 91, 0);
    WriteEmptyDataMap(AOutStream);
    dxfStringWithoutEncodeOut(AOutStream, 309,
      'LINKEDTABLEDATAROW_END');
    dxfStringWithoutEncodeOut(AOutStream, 1,
      'FORMATTEDTABLEDATAROW_BEGIN');
    dxfStringWithoutEncodeOut(AOutStream, 300, 'ROWTABLEFORMAT');
    WriteEmptyTableFormat(AOutStream, 2);
    dxfStringWithoutEncodeOut(AOutStream, 309,
      'FORMATTEDTABLEDATAROW_END');
    dxfStringWithoutEncodeOut(AOutStream, 1, 'TABLEROW_BEGIN');
    dxfIntegerout(AOutStream, 90, ARecord.RowStyleIds[RowIdx]);
    dxfDoubleout(AOutStream, 40, ARecord.RowHeights[RowIdx]);
    dxfStringWithoutEncodeOut(AOutStream, 309, 'TABLEROW_END');
  end;
  dxfIntegerout(AOutStream, 92, 0);

  dxfStringWithoutEncodeOut(AOutStream, 100, 'AcDbFormattedTableData');
  dxfStringWithoutEncodeOut(AOutStream, 300, 'TABLEFORMAT');
  WriteEmptyTableFormat(AOutStream, 4);
  // Переопределений форматирования на уровне таблицы нет (AutoCAD пишет
  // здесь одиночное 90|0; наш прежний набор 90..94 ломал разбор).
  dxfIntegerout(AOutStream, 90, 0);

  dxfStringWithoutEncodeOut(AOutStream, 100, 'AcDbTableContent');
  // Ссылка на TABLESTYLE, в расширенном словаре которого лежит CELLSTYLEMAP
  // с определениями _TITLE/_HEADER/_DATA (issue #1409).
  TableStyleHandle := ResolveTableStyleHandle(AIODXFContext,
    ARecord.TableStyleName, ARecord.TableStyleHandle);
  if TableStyleHandle <> '' then
    dxfStringWithoutEncodeOut(AOutStream, 340, TableStyleHandle)
  else
    dxfStringWithoutEncodeOut(AOutStream, 340, '0');
end;

// Пишет тройку объектов round-trip AutoCAD 2008 для одной таблицы:
// расширенный словарь сущности -> XRECORD -> TABLECONTENT (issue #1409).
procedure WriteAcadTableContentToDXF(
  var AOutStream: TZctnrVectorBytes;
  var AIODXFContext: TIODXFSaveContext;
  const ARecord: TAcadTableContentRecord);
begin
  dxfStringWithoutEncodeOut(AOutStream, 0, 'DICTIONARY');
  dxfStringWithoutEncodeOut(AOutStream, 5,
    IntToHex(ARecord.DictionaryHandle, 0));
  dxfStringWithoutEncodeOut(AOutStream, 330,
    IntToHex(ARecord.EntityHandle, 0));
  dxfStringWithoutEncodeOut(AOutStream, 100, 'AcDbDictionary');
  dxfIntegerout(AOutStream, 280, 1);
  dxfIntegerout(AOutStream, 281, 1);
  dxfStringWithoutEncodeOut(AOutStream, 3, 'ACAD_XREC_ROUNDTRIP');
  dxfStringWithoutEncodeOut(AOutStream, 360,
    IntToHex(ARecord.XRecordHandle, 0));

  dxfStringWithoutEncodeOut(AOutStream, 0, 'XRECORD');
  dxfStringWithoutEncodeOut(AOutStream, 5,
    IntToHex(ARecord.XRecordHandle, 0));
  dxfStringWithoutEncodeOut(AOutStream, 330,
    IntToHex(ARecord.DictionaryHandle, 0));
  dxfStringWithoutEncodeOut(AOutStream, 100, 'AcDbXrecord');
  dxfIntegerout(AOutStream, 280, 1);
  dxfStringWithoutEncodeOut(AOutStream, 102,
    'ACAD_ROUNDTRIP_2008_TABLE_ENTITY');
  dxfStringWithoutEncodeOut(AOutStream, 360,
    IntToHex(ARecord.ContentHandle, 0));
  dxfIntegerout(AOutStream, 70, 2);
  dxfIntegerout(AOutStream, 90, 1);
  dxfDoubleout(AOutStream, 10, 0);
  dxfDoubleout(AOutStream, 20, 0);
  dxfDoubleout(AOutStream, 30, 0);
  dxfIntegerout(AOutStream, 90, 0);
  dxfIntegerout(AOutStream, 90, 5);

  WriteTableContentObject(AOutStream, AIODXFContext, ARecord);
end;

procedure WriteAcadTableRoundTripObjectsToDXF(
  var AOutStream: TZctnrVectorBytes;
  var ADrawing: TSimpleDrawing;
  var AIODXFContext: TIODXFSaveContext);
var
  RecIdx: Integer;
begin
  for RecIdx := 0 to High(ContentRecords) do
    WriteAcadTableContentToDXF(
      AOutStream, AIODXFContext, ContentRecords[RecIdx]);
  for RecIdx := 0 to High(RoundTripRecords) do
    WriteRoundTripRecordToDXF(
      AOutStream, AIODXFContext, RoundTripRecords[RecIdx]);
  ResetAcadTableDXFWriteState;
end;

procedure ResetAcadTableDXFWriteStateBeforeSave(
  var ADrawing: TSimpleDrawing);
begin
  ResetAcadTableDXFWriteState;
end;

initialization
  RegisterBeforeSaveDxfProc(@ResetAcadTableDXFWriteStateBeforeSave);
  RegisterClassesSaveDxfProc(@WriteAcadTableClassesToDXF);
  RegisterObjectsSaveDxfProc(@WriteAcadTableRoundTripObjectsToDXF);

finalization
  ResetAcadTableDXFWriteState;

end.
