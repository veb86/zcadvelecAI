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
  uzedrawingdef, uzedrawingsimple, uzeffdxfsupport,
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
  const AMainExtDictSubtree, ARawTextStyleHandleMap: String;
  ABreakSpacing, ABreakHeight: Double;
  ABreakManualPosition, ABreakManualHeight: Boolean;
  const ATableStyleName: String): Boolean;

procedure WriteAcadTableRoundTripObjectsToDXF(
  var AOutStream: TZctnrVectorBytes;
  var ADrawing: TSimpleDrawing;
  var AIODXFContext: TIODXFSaveContext);

implementation

uses
  SysUtils, Classes, uzeffdxfout;

type
  TAcadTableHandleRemap = record
    OldHandle: String;
    NewHandle: String;
  end;

  TAcadTableHandleRemapArray = array of TAcadTableHandleRemap;

  TAcadTableRoundTripRecord = record
    MainHandle: TDWGHandle;
    ContinuationHandles: array of TDWGHandle;
    RawExtDictSubtree: String;
    RawExtDictSubtreeValid: Boolean;
    RawTextStyleHandleMap: String;
    HandleRemaps: TAcadTableHandleRemapArray;
    ExtDictHandleNew: String;
    MainTableStyleHandle: String;
    TableStyleName: String;
    BreakSpacing: Double;
    BreakHeight: Double;
    { Признаки ручного управления разрывами (issue #1339). Влияют на
      BreakOption-флаг (первая группа 90) в ACAD_ROUNDTRIP_2008_TABLE_ENTITY:
        8  (AllowManualPositions) <- BreakManualPosition,
        16 (AllowManualHeights)   <- BreakManualHeight. }
    BreakManualPosition: Boolean;
    BreakManualHeight: Boolean;
  end;

var
  RoundTripRecords: array of TAcadTableRoundTripRecord;

procedure ResetAcadTableDXFWriteState;
var
  I: Integer;
begin
  for I := 0 to High(RoundTripRecords) do
  begin
    System.SetLength(RoundTripRecords[I].ContinuationHandles, 0);
    System.SetLength(RoundTripRecords[I].HandleRemaps, 0);
    RoundTripRecords[I].RawExtDictSubtree := '';
    RoundTripRecords[I].RawTextStyleHandleMap := '';
  end;
  System.SetLength(RoundTripRecords, 0);
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

procedure WriteEntityPrefix(
  var AOutStream: TZctnrVectorBytes;
  var AIODXFContext: TIODXFSaveContext;
  const APart: TAcadTableDXFWritePart;
  out AHandle: TDWGHandle);
begin
  AHandle := EntityHandleForPart(APart, AIODXFContext);

  dxfStringWithoutEncodeOut(AOutStream, 0, 'ACAD_TABLE');
  dxfStringWithoutEncodeOut(AOutStream, 5, IntToHex(AHandle, 0));
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
begin
  dxfStringWithoutEncodeOut(AOutStream, 100, 'AcDbBlockReference');
  dxfStringWithoutEncodeOut(AOutStream, 2,
    Format('*T%d', [APartIndex + 1]));
  dxfvertexout(AOutStream, 10, APart.InsertPoint);
end;

procedure WriteTableHeader(
  var AOutStream: TZctnrVectorBytes;
  const APart: TAcadTableDXFWritePart);
begin
  dxfStringWithoutEncodeOut(AOutStream, 100, 'AcDbTable');
  if APart.TableStyleHandle <> '' then
    dxfStringWithoutEncodeOut(AOutStream, 342, APart.TableStyleHandle);
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
  Alignment: Integer;
begin
  CellText := CellTextAt(APart, ARow, ACol);
  GetCellSpanAndVirtualFlag(APart, ARow, ACol,
    ColSpan, RowSpan, IsVirtual);
  Alignment := CellAlignmentAt(APart, ARow, ACol);

  dxfIntegerout(AOutStream, 171, 1);
  dxfIntegerout(AOutStream, 172, 0);
  dxfIntegerout(AOutStream, 173, BoolToDXF(IsVirtual));
  if Alignment <> 0 then
    dxfIntegerout(AOutStream, 170, Alignment);
  dxfIntegerout(AOutStream, 174, 0);
  dxfIntegerout(AOutStream, 175, ColSpan);
  dxfIntegerout(AOutStream, 176, RowSpan);
  dxfIntegerout(AOutStream, 91, 262144);
  dxfIntegerout(AOutStream, 178, 0);
  dxfDoubleout(AOutStream, 145, 0);
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

function NormalizeRawHandle(const S: String): String; forward;

procedure AddRoundTripRecord(
  AMainHandle: TDWGHandle;
  const AContinuationHandles: array of TDWGHandle;
  const ARawExtDictSubtree, ARawTextStyleHandleMap: String;
  const AHandleRemaps: TAcadTableHandleRemapArray;
  const AExtDictHandleNew, AMainTableStyleHandle, ATableStyleName: String;
  ABreakSpacing, ABreakHeight: Double;
  ABreakManualPosition, ABreakManualHeight: Boolean);
var
  RecIdx, HandleIdx: Integer;
begin
  if (Length(AContinuationHandles) = 0) and (ARawExtDictSubtree = '') then
    Exit;

  RecIdx := Length(RoundTripRecords);
  System.SetLength(RoundTripRecords, RecIdx + 1);
  RoundTripRecords[RecIdx].MainHandle := AMainHandle;
  RoundTripRecords[RecIdx].RawExtDictSubtree := ARawExtDictSubtree;
  RoundTripRecords[RecIdx].RawExtDictSubtreeValid := ARawExtDictSubtree <> '';
  RoundTripRecords[RecIdx].RawTextStyleHandleMap := ARawTextStyleHandleMap;
  RoundTripRecords[RecIdx].ExtDictHandleNew := NormalizeRawHandle(AExtDictHandleNew);
  RoundTripRecords[RecIdx].MainTableStyleHandle :=
    NormalizeRawHandle(AMainTableStyleHandle);
  RoundTripRecords[RecIdx].TableStyleName := ATableStyleName;
  RoundTripRecords[RecIdx].BreakSpacing := ABreakSpacing;
  RoundTripRecords[RecIdx].BreakHeight := ABreakHeight;
  RoundTripRecords[RecIdx].BreakManualPosition := ABreakManualPosition;
  RoundTripRecords[RecIdx].BreakManualHeight := ABreakManualHeight;
  System.SetLength(
    RoundTripRecords[RecIdx].HandleRemaps,
    Length(AHandleRemaps));
  for HandleIdx := 0 to High(AHandleRemaps) do
    RoundTripRecords[RecIdx].HandleRemaps[HandleIdx] :=
      AHandleRemaps[HandleIdx];
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

procedure BumpHandleAfterRawEntity(
  var AIODXFContext: TIODXFSaveContext; AHandle: TDWGHandle);
begin
  if AHandle >= AIODXFContext.handle then
    AIODXFContext.handle := AHandle + 1;
end;

function NormalizeRawHandle(const S: String): String;
var
  I: Integer;
begin
  Result := UpperCase(Trim(S));
  I := 1;
  while (I < Length(Result)) and (Result[I] = '0') do
    Inc(I);
  if I > 1 then
    Result := Copy(Result, I, Length(Result) - I + 1);
end;

function RawDxfGroupValue(
  const ARawText, AGroupCode: String; out AValue: String): Boolean;
var
  Lines: TStringList;
  I: Integer;
begin
  Result := False;
  AValue := '';
  if ARawText = '' then
    Exit;

  Lines := TStringList.Create;
  try
    Lines.Text := ARawText;
    I := 0;
    while I < Lines.Count - 1 do
    begin
      if Trim(Lines[I]) = AGroupCode then
      begin
        AValue := NormalizeRawHandle(Lines[I + 1]);
        Result := AValue <> '';
        Exit;
      end;
      Inc(I, 2);
    end;
  finally
    Lines.Free;
  end;
end;

function RawAcadTableExtDictHandle(
  const ARawEntity: String; out AHandle: String): Boolean;
var
  Lines: TStringList;
  I: Integer;
  Code, Value: String;
  InExtDict: Boolean;
begin
  Result := False;
  AHandle := '';
  if ARawEntity = '' then
    Exit;

  Lines := TStringList.Create;
  try
    Lines.Text := ARawEntity;
    InExtDict := False;
    I := 0;
    while I < Lines.Count - 1 do
    begin
      Code := Trim(Lines[I]);
      Value := Trim(Lines[I + 1]);

      if (Code = '102') and (Value = '{ACAD_XDICTIONARY') then
        InExtDict := True
      else if InExtDict and (Code = '102') and (Value = '}') then
        Break
      else if InExtDict and (Code = '360') then
      begin
        AHandle := NormalizeRawHandle(Value);
        Result := AHandle <> '';
        Break;
      end;

      Inc(I, 2);
    end;
  finally
    Lines.Free;
  end;
end;

procedure AddHandleRemap(
  var AHandleRemaps: TAcadTableHandleRemapArray;
  const AOldHandle, ANewHandle: String);
var
  Idx: Integer;
begin
  if (AOldHandle = '') or (ANewHandle = '') then
    Exit;

  Idx := Length(AHandleRemaps);
  System.SetLength(AHandleRemaps, Idx + 1);
  AHandleRemaps[Idx].OldHandle := NormalizeRawHandle(AOldHandle);
  AHandleRemaps[Idx].NewHandle := NormalizeRawHandle(ANewHandle);
end;

function FindHandleRemap(
  const AHandleRemaps: TAcadTableHandleRemapArray;
  const AOldHandle: String; out ANewHandle: String): Boolean;
var
  Idx: Integer;
  OldHandle: String;
begin
  Result := False;
  ANewHandle := '';
  OldHandle := NormalizeRawHandle(AOldHandle);
  for Idx := 0 to High(AHandleRemaps) do
    if AHandleRemaps[Idx].OldHandle = OldHandle then
    begin
      ANewHandle := AHandleRemaps[Idx].NewHandle;
      Result := ANewHandle <> '';
      Exit;
    end;
end;

function CollectRawExtDictHandleRemaps(
  const ARawObjects, AExtDictHandleOld: String;
  var AIODXFContext: TIODXFSaveContext;
  out AHandleRemaps: TAcadTableHandleRemapArray;
  out AExtDictHandleNew: String): Boolean;
var
  Lines: TStringList;
  I, J, K, ObjectStart, ObjectEnd: Integer;
  OldHandle, NewHandle: String;
  NewHandleValue: TDWGHandle;
begin
  Result := False;
  AExtDictHandleNew := '';
  System.SetLength(AHandleRemaps, 0);
  if ARawObjects = '' then
    Exit;

  Lines := TStringList.Create;
  try
    Lines.Text := ARawObjects;
    I := 0;
    while I < Lines.Count - 1 do
    begin
      if Trim(Lines[I]) = '0' then
      begin
        ObjectStart := I;
        ObjectEnd := Lines.Count;
        J := I + 2;
        while J < Lines.Count - 1 do
        begin
          if Trim(Lines[J]) = '0' then
          begin
            ObjectEnd := J;
            Break;
          end;
          Inc(J, 2);
        end;

        OldHandle := '';
        K := ObjectStart + 2;
        while K < ObjectEnd - 1 do
        begin
          if Trim(Lines[K]) = '5' then
          begin
            OldHandle := NormalizeRawHandle(Lines[K + 1]);
            Break;
          end;
          Inc(K, 2);
        end;

        if OldHandle <> '' then
        begin
          NewHandleValue := NextAnonymousHandle(AIODXFContext);
          NewHandle := IntToHex(NewHandleValue, 0);
          AddHandleRemap(AHandleRemaps, OldHandle, NewHandle);
          if OldHandle = NormalizeRawHandle(AExtDictHandleOld) then
            AExtDictHandleNew := NewHandle;
        end;

        I := ObjectEnd;
        Continue;
      end;
      Inc(I);
    end;
  finally
    Lines.Free;
  end;

  Result := (Length(AHandleRemaps) > 0) and (AExtDictHandleNew <> '');
end;

function DxfRawDoubleString(const AValue: Double): String;
begin
  Str(AValue:10:10, Result);
end;

function AcadTableBreakOptionFlags(
  ABreakManualPosition, ABreakManualHeight: Boolean): Integer;
const
  { Базовые биты BreakOption: EnableBreaks(1) + RepeatTopLabels(2) = 3. }
  cTableBreakBaseFlags = 3;
  cTableBreakAllowManualPositions = 8;
  cTableBreakAllowManualHeights = 16;
begin
  Result := cTableBreakBaseFlags;
  if ABreakManualPosition then
    Result := Result or cTableBreakAllowManualPositions;
  if ABreakManualHeight then
    Result := Result or cTableBreakAllowManualHeights;
end;

function IsHandleCode(const ACode: String): Boolean;
begin
  Result :=
    (ACode = '5') or (ACode = '330') or (ACode = '340') or
    (ACode = '350') or (ACode = '360') or (ACode = '361');
end;

function IsAcadTableRoundTripMarkerValue(const AValue: String): Boolean;
begin
  Result :=
    UpperCase(Trim(AValue)) = 'ACAD_ROUNDTRIP_2008_TABLE_ENTITY';
end;

// Записывает сырую (raw) сущность ACAD_TABLE, переписывая ссылки на хэндлы
// под актуальную (перенумерованную) нумерацию сохраняемого файла (issue #1339):
//   330 — владелец сущности (*Model_Space);
//   342 — стиль таблицы (по имени стиля ATableStyleName);
//   343 — анонимный блок таблицы (по имени блока из предшествующей пары 2);
//   102/ACAD_XDICTIONARY — либо переписывается на новый хэндл сохранённого
//        поддерева TABLECONTENT/TABLEGEOMETRY, либо удаляется, чтобы не
//        оставить висячую ссылку (360).
// Хэндл самой сущности (группа 5) намеренно не трогаем.
procedure WriteRawEntityText(
  var AOutStream: TZctnrVectorBytes;
  var AIODXFContext: TIODXFSaveContext;
  const ATableStyleName: String;
  const ARawEntity: String;
  AKeepExtDict: Boolean;
  const AExtDictHandleNew: String);
var
  Lines: TStringList;
  I: Integer;
  Code, OutValue, LastBlockName, MappedValue: String;
begin
  Lines := TStringList.Create;
  try
    Lines.Text := ARawEntity;
    LastBlockName := '';
    I := 0;
    while I < Lines.Count do
    begin
      Code := Trim(Lines[I]);

      { Удаляем блок расширенного словаря целиком (вместе с висячим 360). }
      if (Code = '102') and (I + 1 < Lines.Count)
         and (Trim(Lines[I + 1]) = '{ACAD_XDICTIONARY') then
      begin
        if AKeepExtDict and (AExtDictHandleNew <> '') then
        begin
          AOutStream.TXTAddStringEOL(Lines[I]);
          AOutStream.TXTAddStringEOL(Lines[I + 1]);
          Inc(I, 2);
          while I + 1 < Lines.Count do
          begin
            Code := Trim(Lines[I]);
            OutValue := Lines[I + 1];
            if Code = '360' then
              OutValue := AExtDictHandleNew;
            AOutStream.TXTAddStringEOL(Lines[I]);
            AOutStream.TXTAddStringEOL(OutValue);
            Inc(I, 2);
            if (Code = '102') and (Trim(OutValue) = '}') then
              Break;
          end;
          Continue;
        end;

        { Нет сохранённого поддерева — удаляем блок расширенного словаря
          целиком (вместе с висячим 360). }
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

function RemapRawExtDictHandleValue(
  var AIODXFContext: TIODXFSaveContext;
  const ARecord: TAcadTableRoundTripRecord;
  const ACode, AValue: String;
  ATextStyleHandleNames: TStringList;
  out AOutValue: String): Boolean;
var
  NormalizedValue, MappedValue, StyleName: String;
  TextStyleIdx: Integer;
begin
  Result := False;
  AOutValue := AValue;
  NormalizedValue := NormalizeRawHandle(AValue);
  if NormalizedValue = '' then
    Exit;

  if FindHandleRemap(
    ARecord.HandleRemaps, NormalizedValue, MappedValue) then
  begin
    AOutValue := MappedValue;
    Result := True;
    Exit;
  end;

  if ACode <> '340' then
    Exit;

  if (ARecord.MainTableStyleHandle <> '') and
     (NormalizedValue = ARecord.MainTableStyleHandle) and
     (ARecord.TableStyleName <> '') and
     AIODXFContext.TableStyleNameHandleMap.MyGetValue(
       ARecord.TableStyleName, MappedValue) then
  begin
    AOutValue := MappedValue;
    Result := True;
    Exit;
  end;

  if ATextStyleHandleNames <> nil then
  begin
    TextStyleIdx := ATextStyleHandleNames.IndexOfName(NormalizedValue);
    if TextStyleIdx >= 0 then
    begin
      StyleName := ATextStyleHandleNames.ValueFromIndex[TextStyleIdx];
      if (StyleName <> '') and
         AIODXFContext.TextStyleNameHandleMap.MyGetValue(
           StyleName, MappedValue) then
      begin
        AOutValue := MappedValue;
        Result := True;
      end;
    end;
  end;
end;

procedure WriteRawExtDictSubtreeToDXF(
  var AOutStream: TZctnrVectorBytes;
  var AIODXFContext: TIODXFSaveContext;
  const ARecord: TAcadTableRoundTripRecord);
var
  Lines, TextStyleHandleNames: TStringList;
  I, BreakDoubleCount, BreakOptionFlags: Integer;
  Code, OutValue: String;
  InRoundTripBlock, BreakOptionWritten: Boolean;
begin
  if (not ARecord.RawExtDictSubtreeValid) or
     (ARecord.RawExtDictSubtree = '') then
    Exit;

  Lines := TStringList.Create;
  TextStyleHandleNames := TStringList.Create;
  try
    Lines.Text := ARecord.RawExtDictSubtree;
    TextStyleHandleNames.CaseSensitive := False;
    TextStyleHandleNames.Text := ARecord.RawTextStyleHandleMap;

    InRoundTripBlock := False;
    BreakOptionWritten := False;
    BreakDoubleCount := 0;
    BreakOptionFlags := AcadTableBreakOptionFlags(
      ARecord.BreakManualPosition, ARecord.BreakManualHeight);

    I := 0;
    while I < Lines.Count do
    begin
      if I + 1 >= Lines.Count then
      begin
        AOutStream.TXTAddStringEOL(Lines[I]);
        Inc(I);
        Continue;
      end;

      Code := Trim(Lines[I]);
      OutValue := Lines[I + 1];

      if Code = '0' then
        InRoundTripBlock := False;

      if IsHandleCode(Code) then
        RemapRawExtDictHandleValue(
          AIODXFContext, ARecord, Code, OutValue,
          TextStyleHandleNames, OutValue);

      if (Code = '102') and IsAcadTableRoundTripMarkerValue(OutValue) then
      begin
        InRoundTripBlock := True;
        BreakOptionWritten := False;
        BreakDoubleCount := 0;
      end
      else if InRoundTripBlock and (Code = '90') and
              (not BreakOptionWritten) then
      begin
        OutValue := IntToStr(BreakOptionFlags);
        BreakOptionWritten := True;
      end
      else if InRoundTripBlock and (Code = '40') and
              (BreakDoubleCount < 2) then
      begin
        if BreakDoubleCount = 0 then
          OutValue := DxfRawDoubleString(ARecord.BreakSpacing)
        else
          OutValue := DxfRawDoubleString(ARecord.BreakHeight);
        Inc(BreakDoubleCount);
      end;

      AOutStream.TXTAddStringEOL(Lines[I]);
      AOutStream.TXTAddStringEOL(OutValue);
      Inc(I, 2);
    end;
  finally
    TextStyleHandleNames.Free;
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
  WriteTableHeader(AOutStream, APart);
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
  EmptyHandleRemaps: TAcadTableHandleRemapArray;
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

  System.SetLength(EmptyHandleRemaps, 0);
  AddRoundTripRecord(MainHandle, ContinuationHandles,
    '', '', EmptyHandleRemaps, '', '', '',
    AMainPart.BreakSpacing, AMainPart.BreakHeight,
    AMainPart.BreakManualPosition, AMainPart.BreakManualHeight);
end;

function WriteRawAcadTablePartsToDXF(
  var AOutStream: TZctnrVectorBytes;
  var AIODXFContext: TIODXFSaveContext;
  const AMainRawEntity: String;
  const AContinuationRawEntities: array of String;
  const AMainExtDictSubtree, ARawTextStyleHandleMap: String;
  ABreakSpacing, ABreakHeight: Double;
  ABreakManualPosition, ABreakManualHeight: Boolean;
  const ATableStyleName: String): Boolean;
var
  MainHandle: TDWGHandle;
  ContinuationHandles: array of TDWGHandle;
  PartIdx: Integer;
  MainExtDictHandleOld, MainExtDictHandleNew: String;
  MainTableStyleHandle: String;
  HandleRemaps: TAcadTableHandleRemapArray;
  PreserveExtDict: Boolean;
  RawExtDictSubtreeToWrite, RawTextStyleMapToWrite: String;
begin
  Result := False;
  if not RawAcadTableHandle(AMainRawEntity, MainHandle) then
    Exit;

  System.SetLength(ContinuationHandles, Length(AContinuationRawEntities));
  for PartIdx := 0 to High(AContinuationRawEntities) do
    if not RawAcadTableHandle(
      AContinuationRawEntities[PartIdx],
      ContinuationHandles[PartIdx]) then
      Exit;

  BumpHandleAfterRawEntity(AIODXFContext, MainHandle);
  for PartIdx := 0 to High(ContinuationHandles) do
    BumpHandleAfterRawEntity(
      AIODXFContext, ContinuationHandles[PartIdx]);

  RawDxfGroupValue(AMainRawEntity, '342', MainTableStyleHandle);
  PreserveExtDict := False;
  MainExtDictHandleOld := '';
  MainExtDictHandleNew := '';
  System.SetLength(HandleRemaps, 0);
  if (AMainExtDictSubtree <> '') and
     RawAcadTableExtDictHandle(AMainRawEntity, MainExtDictHandleOld) then
    PreserveExtDict := CollectRawExtDictHandleRemaps(
      AMainExtDictSubtree, MainExtDictHandleOld, AIODXFContext,
      HandleRemaps, MainExtDictHandleNew);

  WriteRawEntityText(AOutStream, AIODXFContext, ATableStyleName,
    AMainRawEntity, PreserveExtDict, MainExtDictHandleNew);

  for PartIdx := 0 to High(AContinuationRawEntities) do
  begin
    WriteRawEntityText(AOutStream, AIODXFContext, ATableStyleName,
      AContinuationRawEntities[PartIdx], False, '');
  end;

  if PreserveExtDict then
  begin
    RawExtDictSubtreeToWrite := AMainExtDictSubtree;
    RawTextStyleMapToWrite := ARawTextStyleHandleMap;
  end
  else
  begin
    RawExtDictSubtreeToWrite := '';
    RawTextStyleMapToWrite := '';
  end;

  AddRoundTripRecord(MainHandle, ContinuationHandles,
    RawExtDictSubtreeToWrite, RawTextStyleMapToWrite,
    HandleRemaps, MainExtDictHandleNew, MainTableStyleHandle,
    ATableStyleName,
    ABreakSpacing, ABreakHeight,
    ABreakManualPosition, ABreakManualHeight);
  System.SetLength(HandleRemaps, 0);
  Result := True;
end;

procedure WriteRoundTripRecordToDXF(
  var AOutStream: TZctnrVectorBytes;
  var AIODXFContext: TIODXFSaveContext;
  const ARecord: TAcadTableRoundTripRecord);
var
  ObjectHandle: TDWGHandle;
  HandleIdx: Integer;
  BreakOptionFlags: Integer;
begin
  if ARecord.RawExtDictSubtreeValid then
  begin
    WriteRawExtDictSubtreeToDXF(
      AOutStream, AIODXFContext, ARecord);
    Exit;
  end;

  ObjectHandle := NextAnonymousHandle(AIODXFContext);

  { Восстанавливаем BreakOption-флаг из признаков ручного управления
    разрывами (issue #1339). Раньше здесь была жёстко зашита 3, из-за чего
    при пересохранении терялись AllowManualPositions/AllowManualHeights. }
  BreakOptionFlags := AcadTableBreakOptionFlags(
    ARecord.BreakManualPosition, ARecord.BreakManualHeight);

  dxfStringWithoutEncodeOut(AOutStream, 0, 'XRECORD');
  dxfStringWithoutEncodeOut(AOutStream, 5, IntToHex(ObjectHandle, 0));
  dxfStringWithoutEncodeOut(AOutStream, 330, '0');
  dxfStringWithoutEncodeOut(AOutStream, 100, 'AcDbXrecord');
  dxfIntegerout(AOutStream, 280, 1);
  dxfStringWithoutEncodeOut(AOutStream, 102,
    'ACAD_ROUNDTRIP_2008_TABLE_ENTITY');
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

procedure WriteAcadTableRoundTripObjectsToDXF(
  var AOutStream: TZctnrVectorBytes;
  var ADrawing: TSimpleDrawing;
  var AIODXFContext: TIODXFSaveContext);
var
  RecIdx: Integer;
begin
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
  RegisterObjectsSaveDxfProc(@WriteAcadTableRoundTripObjectsToDXF);

finalization
  ResetAcadTableDXFWriteState;

end.
