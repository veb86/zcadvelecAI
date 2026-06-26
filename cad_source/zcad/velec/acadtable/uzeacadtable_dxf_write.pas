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
    { Признак главной части разорванной таблицы (есть части-продолжения).
      Только для такой части пишется расширенный словарь ACAD_XDICTIONARY,
      через который round-trip XRECORD (ACAD_ROUNDTRIP_2008_TABLE_ENTITY)
      привязывается к сущности (issue #1378). }
    HasContinuations: Boolean;
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
  SysUtils, Classes, uzeffdxfout;

type
  TAcadTableRoundTripRecord = record
    MainHandle: TDWGHandle;
    ContinuationHandles: array of TDWGHandle;
    BreakSpacing: Double;
    BreakHeight: Double;
    { Признаки ручного управления разрывами (issue #1339). Влияют на
      BreakOption-флаг (первая группа 90) в ACAD_ROUNDTRIP_2008_TABLE_ENTITY:
        8  (AllowManualPositions) <- BreakManualPosition,
        16 (AllowManualHeights)   <- BreakManualHeight. }
    BreakManualPosition: Boolean;
    BreakManualHeight: Boolean;
  end;

  { Связь главной сущности таблицы с её расширенным словарём (issue #1378).
    Хэндл словаря выделяется при записи самой сущности (секция ENTITIES),
    а используется позже при записи round-trip XRECORD (секция OBJECTS),
    поэтому хранится между проходами в модульной таблице. }
  TAcadTableMainEntityDict = record
    MainHandle: TDWGHandle;
    DictHandle: TDWGHandle;
  end;

var
  RoundTripRecords: array of TAcadTableRoundTripRecord;
  MainEntityDicts: array of TAcadTableMainEntityDict;

procedure ResetAcadTableDXFWriteState;
var
  I: Integer;
begin
  for I := 0 to High(RoundTripRecords) do
    System.SetLength(RoundTripRecords[I].ContinuationHandles, 0);
  System.SetLength(RoundTripRecords, 0);
  System.SetLength(MainEntityDicts, 0);
end;

{ Регистрирует пару «главная сущность таблицы -> её расширенный словарь».
  Вызывается при записи главной сущности (issue #1378). }
procedure RegisterMainEntityDict(AMainHandle, ADictHandle: TDWGHandle);
var
  Idx: Integer;
begin
  Idx := Length(MainEntityDicts);
  System.SetLength(MainEntityDicts, Idx + 1);
  MainEntityDicts[Idx].MainHandle := AMainHandle;
  MainEntityDicts[Idx].DictHandle := ADictHandle;
end;

{ Возвращает хэндл расширенного словаря главной сущности, если он был
  зарегистрирован при записи сущности (issue #1378). }
function LookupMainEntityDict(
  AMainHandle: TDWGHandle; out ADictHandle: TDWGHandle): Boolean;
var
  Idx: Integer;
begin
  Result := False;
  ADictHandle := 0;
  for Idx := 0 to High(MainEntityDicts) do
    if MainEntityDicts[Idx].MainHandle = AMainHandle then
    begin
      ADictHandle := MainEntityDicts[Idx].DictHandle;
      Result := True;
      Exit;
    end;
end;

{ Пишет ссылку на расширенный словарь сущности (issue #1378):
    102 {ACAD_XDICTIONARY 360 <dict> 102 }
  Через этот словарь AutoCAD находит round-trip XRECORD с высотой разбиения.
  Без него запись остаётся «висячей», разрыв не применяется и таблица
  открывается цельной «длинной простынёй». }
procedure WriteExtensionDictionaryRef(
  var AOutStream: TZctnrVectorBytes; ADictHandle: TDWGHandle);
begin
  dxfStringWithoutEncodeOut(AOutStream, 102, '{ACAD_XDICTIONARY');
  dxfStringWithoutEncodeOut(AOutStream, 360, IntToHex(ADictHandle, 0));
  dxfStringWithoutEncodeOut(AOutStream, 102, '}');
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
var
  DictHandle: TDWGHandle;
begin
  AHandle := EntityHandleForPart(APart, AIODXFContext);

  dxfStringWithoutEncodeOut(AOutStream, 0, 'ACAD_TABLE');
  dxfStringWithoutEncodeOut(AOutStream, 5, IntToHex(AHandle, 0));
  { Главная часть разорванной таблицы получает расширенный словарь, к которому
    привязывается round-trip XRECORD с высотой разбиения (issue #1378). Хэндл
    словаря выделяется здесь и переиспользуется в секции OBJECTS. }
  if APart.HasContinuations then
  begin
    DictHandle := NextAnonymousHandle(AIODXFContext);
    WriteExtensionDictionaryRef(AOutStream, DictHandle);
    RegisterMainEntityDict(AHandle, DictHandle);
  end;
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
//   исходный блок расширенного словаря 102/ACAD_XDICTIONARY всегда удаляется
//        (его хэндлы не круглорейсятся). Если ADictHandle<>0 (главная часть
//        разорванной таблицы, issue #1378), взамен пишется свежая ссылка на
//        словарь, через который привязывается round-trip XRECORD с высотой
//        разбиения — иначе AutoCAD открывает таблицу цельной.
procedure WriteRawEntityText(
  var AOutStream: TZctnrVectorBytes;
  var AIODXFContext: TIODXFSaveContext;
  const ATableStyleName: String;
  const ARawEntity: String;
  ANewHandle: TDWGHandle;
  ADictHandle: TDWGHandle);
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
        { Сразу после хэндла сущности — ссылка на расширенный словарь главной
          части разорванной таблицы (issue #1378). Исходный 102/ACAD_XDICTIONARY
          уже удалён выше, поэтому дубля не будет. }
        if ADictHandle <> 0 then
        begin
          AOutStream.TXTAddStringEOL(Lines[I]);
          AOutStream.TXTAddStringEOL(OutValue);
          WriteExtensionDictionaryRef(AOutStream, ADictHandle);
          Inc(I, 2);
          Continue;
        end;
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
  DictHandle: TDWGHandle;
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
  { Главная часть разорванной таблицы получает расширенный словарь, к которому
    в секции OBJECTS привязывается round-trip XRECORD (issue #1378). Части без
    продолжений (Length=0) обрабатываются обычным сохранением и сюда не
    попадают, поэтому словарь нужен всегда, когда есть продолжения. }
  if Length(AContinuationRawEntities) > 0 then
  begin
    DictHandle := NextAnonymousHandle(AIODXFContext);
    RegisterMainEntityDict(MainHandle, DictHandle);
  end
  else
    DictHandle := 0;
  WriteRawEntityText(AOutStream, AIODXFContext, ATableStyleName,
    AMainRawEntity, MainHandle, DictHandle);

  for PartIdx := 0 to High(AContinuationRawEntities) do
  begin
    ContinuationHandles[PartIdx] := NextAnonymousHandle(AIODXFContext);
    WriteRawEntityText(AOutStream, AIODXFContext, ATableStyleName,
      AContinuationRawEntities[PartIdx], ContinuationHandles[PartIdx], 0);
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
  DictHandle: TDWGHandle;
  HasDict: Boolean;
  HandleIdx: Integer;
  BreakOptionFlags: Integer;
begin
  ObjectHandle := NextAnonymousHandle(AIODXFContext);

  { Если для главной сущности был выделен расширенный словарь (issue #1378),
    пишем его как объект-владелец XRECORD и привязываем запись к сущности:
      сущность --(102 ACAD_XDICTIONARY 360)--> DICTIONARY
      DICTIONARY --(3 ACAD_XREC_ROUNDTRIP 360)--> XRECORD
      XRECORD --(330 владелец)--> DICTIONARY.
    Раньше XRECORD писался «висячим» (330 0), AutoCAD его не находил, высота
    разбиения терялась и таблица открывалась цельной. }
  HasDict := LookupMainEntityDict(ARecord.MainHandle, DictHandle);
  if HasDict then
  begin
    dxfStringWithoutEncodeOut(AOutStream, 0, 'DICTIONARY');
    dxfStringWithoutEncodeOut(AOutStream, 5, IntToHex(DictHandle, 0));
    dxfStringWithoutEncodeOut(AOutStream, 330,
      IntToHex(ARecord.MainHandle, 0));
    dxfStringWithoutEncodeOut(AOutStream, 100, 'AcDbDictionary');
    dxfIntegerout(AOutStream, 280, 1);
    dxfIntegerout(AOutStream, 281, 1);
    dxfStringWithoutEncodeOut(AOutStream, 3, 'ACAD_XREC_ROUNDTRIP');
    dxfStringWithoutEncodeOut(AOutStream, 360, IntToHex(ObjectHandle, 0));
  end;

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
  if HasDict then
  begin
    dxfStringWithoutEncodeOut(AOutStream, 102, '{ACAD_REACTORS');
    dxfStringWithoutEncodeOut(AOutStream, 330, IntToHex(DictHandle, 0));
    dxfStringWithoutEncodeOut(AOutStream, 102, '}');
    dxfStringWithoutEncodeOut(AOutStream, 330, IntToHex(DictHandle, 0));
  end
  else
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
