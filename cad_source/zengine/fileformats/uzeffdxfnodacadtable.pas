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
  Модуль: uzeffdxfnodacadtable
  Назначение: данные разрыва таблиц ACAD_TABLE в модели NOD (issue #1465,
  исследование cad_source/zengine/TZ_AcadTable_NOD_issue1465.md).

  AutoCAD хранит разрыв таблицы не в самой сущности, а в объектах секции
  OBJECTS: расширенный словарь основной сущности → ключ ACAD_XREC_ROUNDTRIP →
  XRECORD с маркером 102 ACAD_ROUNDTRIP_2008_TABLE_ENTITY. После маркера:
    360 TABLECONTENT (полная логическая таблица без повторов заголовков),
    70  1 — таблица с разрывом (2 — без разрыва),
    90  флаги разрыва (1 вкл., 2 повтор верха, 4 повтор низа,
        8 ручные позиции, 16 ручные высоты),
    90  направление, 40 промежуток,
    90  число повторяемых верхних строк, 90 — нижних,
    90  N высот, далее N × (10/20/30 позиция, 40 высота, 90 флаги 1|2),
    90  M диапазонов, далее M × (10/20/30 смещение от точки вставки
        основной части, 90 первая и 90 последняя логическая строка),
    90  K продолжений, K × 330 хэндл сущности-продолжения,
    361 TABLEGEOMETRY.
  Корневой словарь дополнительно содержит ACDB_RECOMPOSE_DATA — XRECORD со
  ссылками 330 на стили таблиц и основные сущности разорванных таблиц.

  Модуль строит индекс «хэндл сущности → данные разрыва» по модели NOD
  (TZAcadTableNODIndex) вместо прежних глобальных сканов текста OBJECTS,
  которые брали только первую запись файла. Поддерживается и прежняя
  запись ZCAD (#1339/#1381: XRECORD без владельца, маркер
  ZCAD_SPLIT_TABLE_ENTITY, 360 — основная сущность) — её разбор
  эвристический. Обратная сборка пар XRECORD (BuildAcadTableSplitPairs)
  используется записью DXF и тестами.
}
unit uzeffdxfnodacadtable;
{$Mode delphi}{$H+}
{$INCLUDE zengineconfig.inc}
interface

uses
  SysUtils,
  Generics.Collections,
  uzeTypes,
  uzeffdxfobjects,
  uzeffdxfnod;

const
  { Ключ расширенного словаря сущности с round-trip записью }
  CAcadTableXRecRoundTripKey = 'ACAD_XREC_ROUNDTRIP';
  { Маркер round-trip записи таблицы AutoCAD 2008 }
  CAcadTableRoundTripMarker = 'ACAD_ROUNDTRIP_2008_TABLE_ENTITY';
  { Приватный маркер прежней записи ZCAD (#1381), только чтение }
  CAcadTableZCADSplitMarker = 'ZCAD_SPLIT_TABLE_ENTITY';
  { Ключ NOD со ссылками на разорванные таблицы }
  CAcadTableRecomposeKey = 'ACDB_RECOMPOSE_DATA';
  { Маркер подкласса XRECORD }
  CAcadTableXRecordSubclass = 'AcDbXrecord';

  { Значения группы 70 round-trip записи }
  CAcadTableLayoutSplit = 1;
  CAcadTableLayoutSingle = 2;

  { Биты флагов разрыва (первая группа 90) }
  CAcadTableBreakEnable = 1;
  CAcadTableBreakRepeatTop = 2;
  CAcadTableBreakRepeatBottom = 4;
  CAcadTableBreakManualPositions = 8;
  CAcadTableBreakManualHeights = 16;

  { Биты флагов записи высоты части }
  CAcadTableHeightHasPosition = 1;
  CAcadTableHeightHasHeight = 2;

  { Направление разрыва (TableBreakFlowDirection AutoCAD): вправо — по
    умолчанию, вниз (вертикально), влево }
  CAcadTableBreakDirectionRight = 1;
  CAcadTableBreakDirectionDown = 2;
  CAcadTableBreakDirectionLeft = 4;

  { Тип строки в нотации ZCAD (FRowStyleTypes модели таблицы) }
  CAcadTableRowTypeUnknown = -1;
  CAcadTableRowTypeTitle = 0;
  CAcadTableRowTypeHeader = 1;
  CAcadTableRowTypeData = 2;

type
  { Запись высоты/позиции части таблицы }
  TZAcadTableBreakHeight = record
    X, Y, Z: Double;
    Height: Double;
    Flags: Integer;
  end;
  TZAcadTableBreakHeights = array of TZAcadTableBreakHeight;

  { Диапазон логических строк одной части и её смещение }
  TZAcadTableRowRange = record
    OffsetX, OffsetY, OffsetZ: Double;
    StartRow, EndRow: Integer;
  end;
  TZAcadTableRowRanges = array of TZAcadTableRowRange;

  TZAcadTableRowTypes = array of Integer;

  { Данные round-trip записи одной таблицы }
  TZAcadTableSplitInfo = record
    { Основная сущность ACAD_TABLE }
    EntityHandle: TDWGHandle;
    XRecordHandle: TDWGHandle;
    ContentHandle: TDWGHandle;
    GeometryHandle: TDWGHandle;
    { CAcadTableLayoutSplit / CAcadTableLayoutSingle (0 — неизвестно) }
    Layout: Integer;
    { Прежняя запись ZCAD: надёжны только флаги, промежуток, высота и
      продолжения, диапазонов строк нет }
    Legacy: Boolean;
    BreakFlags: Integer;
    BreakDirection: Integer;
    BreakSpacing: Double;
    TopLabelRows: Integer;
    BottomLabelRows: Integer;
    Heights: TZAcadTableBreakHeights;
    RowRanges: TZAcadTableRowRanges;
    Continuations: TZDXFHandleArray;
    { Типы логических строк из TABLECONTENT (нотация ZCAD) }
    RowTypes: TZAcadTableRowTypes;
  end;
  TZAcadTableSplitInfos = array of TZAcadTableSplitInfo;

  { Индекс round-trip записей таблиц по модели NOD. Модель только
    читается; индекс владеет лишь собственными копиями данных. }
  TZAcadTableNODIndex = class
  private
    FInfos: TZAcadTableSplitInfos;
    FByEntity: TDictionary<TDWGHandle, Integer>;
    FContinuationOwner: TDictionary<TDWGHandle, TDWGHandle>;
    FRecomposeHandle: TDWGHandle;
    FRecomposeRefs: TZDXFHandleArray;
    procedure AddInfo(const AInfo: TZAcadTableSplitInfo);
    procedure Build(AModel: TZNODModel);
    procedure LoadRecompose(AModel: TZNODModel);
    function GetCount: Integer;
    function GetInfo(AIndex: Integer): TZAcadTableSplitInfo;
  public
    constructor Create(AModel: TZNODModel);
    destructor Destroy; override;
    { Данные таблицы по хэндлу её основной сущности }
    function FindByEntity(AEntity: TDWGHandle;
      out AInfo: TZAcadTableSplitInfo): Boolean;
    { Является ли сущность продолжением; AMain — основная сущность }
    function IsContinuation(AEntity: TDWGHandle;
      out AMain: TDWGHandle): Boolean;
    property Count: Integer read GetCount;
    property Infos[AIndex: Integer]: TZAcadTableSplitInfo read GetInfo;
    { XRECORD ACDB_RECOMPOSE_DATA корневого словаря (0 — нет) }
    property RecomposeHandle: TDWGHandle read FRecomposeHandle;
    property RecomposeRefs: TZDXFHandleArray read FRecomposeRefs;
  end;

{ Маркер round-trip записи таблицы (AutoCAD или прежний ZCAD) }
function IsAcadTableSplitMarker(const AValue: string): Boolean;
{ Таблица разорвана: запись с разрывом и хотя бы одно продолжение }
function IsAcadTableSplit(const AInfo: TZAcadTableSplitInfo): Boolean;
{ Разбирает XRECORD round-trip записи. False — объект не такой запись.
  AOwnerDict — словарь-владелец (nil — XRECORD без словаря). }
function ParseAcadTableSplitXRecord(AObject: TZDXFRawObject;
  AOwnerDict: TZDXFDictionary; out AInfo: TZAcadTableSplitInfo): Boolean;
{ Типы строк TABLECONTENT (TABLEROW_BEGIN → 90) в нотации ZCAD }
function ReadAcadTableContentRowTypes(
  AContent: TZDXFRawObject): TZAcadTableRowTypes;
{ Тип строки TABLECONTENT (1 заголовок, 2 шапка, 3 данные) → ZCAD }
function AcadTableRowTypeFromDXF(AValue: Integer): Integer;
{ Тип строки ZCAD → TABLECONTENT (неизвестный тип пишется как данные) }
function AcadTableRowTypeToDXF(AValue: Integer): Integer;
{ Добавляет в AObject пары тела round-trip записи с разрывом (после
  100 AcDbXrecord): 280, 102, 360 … 361 в формате AutoCAD. }
procedure BuildAcadTableSplitPairs(const AInfo: TZAcadTableSplitInfo;
  AObject: TZDXFRawObject);
{ Вещественное значение в формате записи DXF ZCAD (как dxfDoubleout) }
function AcadTableFloatToDXF(AValue: Double): string;

implementation

uses
  uzeffdxfnodlog;

const
  { Предел числа элементов в списках записи: защита от повреждённых
    файлов, где счётчик содержит мусор }
  CMaxSplitListCount = 1000000;
  { Метка строки TABLECONTENT, после которой идёт группа 90 с типом }
  CTableRowBeginLabel = 'TABLEROW_BEGIN';
  { Коды групп записи }
  CCodeMarker = 102;
  CCodeContent = 360;
  CCodeLayout = 70;
  CCodeInt = 90;
  CCodeReal = 40;
  CCodeX = 10;
  CCodeY = 20;
  CCodeZ = 30;
  CCodeContinuation = 330;
  CCodeGeometry = 361;
  CCodeSubclass = 100;
  CCodeFlag = 280;
  CCodeLabel = 1;
  { Значение группы 280 в записи }
  CXRecordCloneFlag = 1;
  { Типы строк TABLECONTENT }
  CDXFRowTypeTitle = 1;
  CDXFRowTypeHeader = 2;
  CDXFRowTypeData = 3;
  { Формат вещественного числа dxfDoubleout: str(v:10:10) }
  CFloatWidth = 10;
  CFloatDecimals = 10;

var
  DXFFormat: TFormatSettings;

type
  { Строгий курсор по парам XRECORD: каждая следующая пара обязана иметь
    ожидаемый код, иначе Ok = False и разбор переходит к эвристике }
  TZSplitCursor = record
    Obj: TZDXFRawObject;
    Pos: Integer;
    Ok: Boolean;
  end;

function IsAcadTableSplitMarker(const AValue: string): Boolean;
var
  S: string;
begin
  S := Trim(AValue);
  Result := (S = CAcadTableRoundTripMarker) or
    (S = CAcadTableZCADSplitMarker);
end;

function IsAcadTableSplit(const AInfo: TZAcadTableSplitInfo): Boolean;
begin
  Result := (AInfo.Layout = CAcadTableLayoutSplit) and
    (Length(AInfo.Continuations) > 0);
end;

function AcadTableFloatToDXF(AValue: Double): string;
begin
  Str(AValue: CFloatWidth: CFloatDecimals, Result);
end;

function AcadTableRowTypeFromDXF(AValue: Integer): Integer;
begin
  case AValue of
    CDXFRowTypeTitle: Result := CAcadTableRowTypeTitle;
    CDXFRowTypeHeader: Result := CAcadTableRowTypeHeader;
    CDXFRowTypeData: Result := CAcadTableRowTypeData;
  else
    Result := CAcadTableRowTypeUnknown;
  end;
end;

function AcadTableRowTypeToDXF(AValue: Integer): Integer;
begin
  case AValue of
    CAcadTableRowTypeTitle: Result := CDXFRowTypeTitle;
    CAcadTableRowTypeHeader: Result := CDXFRowTypeHeader;
  else
    Result := CDXFRowTypeData;
  end;
end;

function TryStrToDXFFloat(const S: string; out AValue: Double): Boolean;
begin
  Result := TryStrToFloat(Trim(S), AValue, DXFFormat);
  if not Result then
    AValue := 0;
end;

function TryStrToDXFInt(const S: string; out AValue: Integer): Boolean;
begin
  Result := TryStrToInt(Trim(S), AValue);
  if not Result then
    AValue := 0;
end;

{ Индекс первой пары после 100 AcDbXrecord (-1 — подкласса нет) }
function XRecordBodyStart(AObject: TZDXFRawObject): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to AObject.PairCount - 1 do
    if (AObject.Pairs[I].Code = CCodeSubclass) and
       (Trim(AObject.Pairs[I].Value) = CAcadTableXRecordSubclass) then
      Exit(I + 1);
end;

{ Маркер записи: первая группа 102 тела XRECORD ('' — нет) }
function XRecordMarker(AObject: TZDXFRawObject; AStart: Integer): string;
var
  I: Integer;
begin
  Result := '';
  I := AObject.IndexOfCode(CCodeMarker, AStart);
  if I >= 0 then
    Result := Trim(AObject.Pairs[I].Value);
end;

function CursorTake(var C: TZSplitCursor; ACode: Integer;
  out AValue: string): Boolean;
begin
  Result := C.Ok and (C.Pos < C.Obj.PairCount) and
    (C.Obj.Pairs[C.Pos].Code = ACode);
  if Result then
  begin
    AValue := C.Obj.Pairs[C.Pos].Value;
    Inc(C.Pos);
  end
  else
  begin
    AValue := '';
    C.Ok := False;
  end;
end;

function CursorInt(var C: TZSplitCursor; ACode: Integer): Integer;
var
  S: string;
begin
  Result := 0;
  if CursorTake(C, ACode, S) and not TryStrToDXFInt(S, Result) then
    C.Ok := False;
end;

function CursorFloat(var C: TZSplitCursor; ACode: Integer): Double;
var
  S: string;
begin
  Result := 0;
  if CursorTake(C, ACode, S) and not TryStrToDXFFloat(S, Result) then
    C.Ok := False;
end;

function CursorHandle(var C: TZSplitCursor; ACode: Integer): TDWGHandle;
var
  S: string;
begin
  Result := 0;
  if CursorTake(C, ACode, S) and not TryDXFStrToHandle(S, Result) then
    C.Ok := False;
end;

{ Счётчик списка записи с проверкой границ }
function CursorCount(var C: TZSplitCursor): Integer;
begin
  Result := CursorInt(C, CCodeInt);
  if (Result < 0) or (Result > CMaxSplitListCount) then
  begin
    C.Ok := False;
    Result := 0;
  end;
end;

procedure CursorReadHeights(var C: TZSplitCursor;
  var AInfo: TZAcadTableSplitInfo);
var
  I: Integer;
begin
  SetLength(AInfo.Heights, CursorCount(C));
  for I := 0 to High(AInfo.Heights) do
  begin
    AInfo.Heights[I].X := CursorFloat(C, CCodeX);
    AInfo.Heights[I].Y := CursorFloat(C, CCodeY);
    AInfo.Heights[I].Z := CursorFloat(C, CCodeZ);
    AInfo.Heights[I].Height := CursorFloat(C, CCodeReal);
    AInfo.Heights[I].Flags := CursorInt(C, CCodeInt);
  end;
end;

procedure CursorReadRanges(var C: TZSplitCursor;
  var AInfo: TZAcadTableSplitInfo);
var
  I: Integer;
begin
  SetLength(AInfo.RowRanges, CursorCount(C));
  for I := 0 to High(AInfo.RowRanges) do
  begin
    AInfo.RowRanges[I].OffsetX := CursorFloat(C, CCodeX);
    AInfo.RowRanges[I].OffsetY := CursorFloat(C, CCodeY);
    AInfo.RowRanges[I].OffsetZ := CursorFloat(C, CCodeZ);
    AInfo.RowRanges[I].StartRow := CursorInt(C, CCodeInt);
    AInfo.RowRanges[I].EndRow := CursorInt(C, CCodeInt);
  end;
end;

procedure CursorReadContinuations(var C: TZSplitCursor;
  var AInfo: TZAcadTableSplitInfo);
var
  I: Integer;
begin
  SetLength(AInfo.Continuations, CursorCount(C));
  for I := 0 to High(AInfo.Continuations) do
    AInfo.Continuations[I] := CursorHandle(C, CCodeContinuation);
end;

{ Тело записи с разрывом (после 70 1) в формате AutoCAD }
procedure CursorReadSplitBody(var C: TZSplitCursor;
  var AInfo: TZAcadTableSplitInfo);
begin
  AInfo.BreakFlags := CursorInt(C, CCodeInt);
  AInfo.BreakDirection := CursorInt(C, CCodeInt);
  AInfo.BreakSpacing := CursorFloat(C, CCodeReal);
  AInfo.TopLabelRows := CursorInt(C, CCodeInt);
  AInfo.BottomLabelRows := CursorInt(C, CCodeInt);
  CursorReadHeights(C, AInfo);
  CursorReadRanges(C, AInfo);
  CursorReadContinuations(C, AInfo);
  AInfo.GeometryHandle := CursorHandle(C, CCodeGeometry);
end;

procedure ClearSplitInfo(out AInfo: TZAcadTableSplitInfo);
begin
  AInfo := Default(TZAcadTableSplitInfo);
  AInfo.BreakDirection := CAcadTableBreakDirectionRight;
end;

{ Строгий разбор формата AutoCAD. False — структура не совпала. }
function ParseStrict(AObject: TZDXFRawObject; AStart: Integer;
  var AInfo: TZAcadTableSplitInfo): Boolean;
var
  C: TZSplitCursor;
  S: string;
begin
  C.Obj := AObject;
  C.Pos := AStart;
  C.Ok := True;
  CursorInt(C, CCodeFlag);
  CursorTake(C, CCodeMarker, S);
  C.Ok := C.Ok and (Trim(S) = CAcadTableRoundTripMarker);
  AInfo.ContentHandle := CursorHandle(C, CCodeContent);
  AInfo.Layout := CursorInt(C, CCodeLayout);
  if C.Ok and (AInfo.Layout = CAcadTableLayoutSplit) then
    CursorReadSplitBody(C, AInfo)
  else if C.Ok then
    TryDXFStrToHandle(AObject.ValueOf(CCodeGeometry), AInfo.GeometryHandle);
  Result := C.Ok;
end;

{ Одна пара эвристического разбора прежней записи ZCAD }
procedure LegacyTakePair(const APair: TZDXFGroupPair;
  var AInfo: TZAcadTableSplitInfo; var AIntSeen, ARealSeen: Integer);
var
  H: TDWGHandle;
  N: Integer;
  F: Double;
begin
  case APair.Code of
    CCodeContent:
      if AInfo.ContentHandle = 0 then
        TryDXFStrToHandle(APair.Value, AInfo.ContentHandle);
    CCodeLayout:
      TryStrToDXFInt(APair.Value, AInfo.Layout);
    CCodeInt:
      begin
        if (AIntSeen = 0) and TryStrToDXFInt(APair.Value, N) then
          AInfo.BreakFlags := N;
        Inc(AIntSeen);
      end;
    CCodeReal:
      begin
        if TryStrToDXFFloat(APair.Value, F) then
          if ARealSeen = 0 then
            AInfo.BreakSpacing := F
          else if ARealSeen = 1 then
            AInfo.Heights[0].Height := F;
        Inc(ARealSeen);
      end;
    CCodeContinuation:
      if TryDXFStrToHandle(APair.Value, H) and (H <> 0) then
        Insert(H, AInfo.Continuations, Length(AInfo.Continuations));
  end;
end;

{ Прежняя запись ZCAD (#1339/#1381): 360 — основная сущность, первая
  группа 90 — флаги, первая 40 — промежуток, вторая — высота, 330 после
  подкласса — продолжения. Диапазонов строк нет. }
procedure ParseLegacy(AObject: TZDXFRawObject; AStart: Integer;
  var AInfo: TZAcadTableSplitInfo);
var
  I, IntSeen, RealSeen: Integer;
begin
  AInfo.Legacy := True;
  AInfo.Layout := CAcadTableLayoutSplit;
  SetLength(AInfo.Heights, 1);
  AInfo.Heights[0].Flags := CAcadTableHeightHasHeight;
  IntSeen := 0;
  RealSeen := 0;
  for I := AStart to AObject.PairCount - 1 do
    LegacyTakePair(AObject.Pairs[I], AInfo, IntSeen, RealSeen);
end;

function ParseAcadTableSplitXRecord(AObject: TZDXFRawObject;
  AOwnerDict: TZDXFDictionary; out AInfo: TZAcadTableSplitInfo): Boolean;
var
  Start: Integer;
begin
  ClearSplitInfo(AInfo);
  Result := False;
  if (AObject = nil) or (AObject.ObjType <> 'XRECORD') then
    Exit;
  Start := XRecordBodyStart(AObject);
  if (Start < 0) or not IsAcadTableSplitMarker(
    XRecordMarker(AObject, Start)) then
    Exit;
  if (AOwnerDict = nil) or not ParseStrict(AObject, Start, AInfo) then
  begin
    ClearSplitInfo(AInfo);
    ParseLegacy(AObject, Start, AInfo);
    { Без словаря-владельца первая 360 — основная сущность (#1381),
      в словаре сущности — TABLECONTENT, как у AutoCAD }
    if AOwnerDict = nil then
    begin
      AInfo.EntityHandle := AInfo.ContentHandle;
      AInfo.ContentHandle := 0;
    end;
  end;
  AInfo.XRecordHandle := AObject.Handle;
  if AOwnerDict <> nil then
    AInfo.EntityHandle := AOwnerDict.OwnerHandle;
  Result := AInfo.EntityHandle <> 0;
end;

function ReadAcadTableContentRowTypes(
  AContent: TZDXFRawObject): TZAcadTableRowTypes;
var
  I, N: Integer;
begin
  Result := nil;
  if AContent = nil then
    Exit;
  for I := 0 to AContent.PairCount - 2 do
    if (AContent.Pairs[I].Code = CCodeLabel) and
       (Trim(AContent.Pairs[I].Value) = CTableRowBeginLabel) and
       (AContent.Pairs[I + 1].Code = CCodeInt) then
    begin
      if not TryStrToDXFInt(AContent.Pairs[I + 1].Value, N) then
        N := 0;
      Insert(AcadTableRowTypeFromDXF(N), Result, Length(Result));
    end;
end;

procedure AddIntPair(AObject: TZDXFRawObject; ACode, AValue: Integer);
begin
  AObject.AddPair(ACode, IntToStr(AValue));
end;

procedure AddPointPairs(AObject: TZDXFRawObject; AX, AY, AZ: Double);
begin
  AObject.AddPair(CCodeX, AcadTableFloatToDXF(AX));
  AObject.AddPair(CCodeY, AcadTableFloatToDXF(AY));
  AObject.AddPair(CCodeZ, AcadTableFloatToDXF(AZ));
end;

procedure BuildHeightsAndRanges(const AInfo: TZAcadTableSplitInfo;
  AObject: TZDXFRawObject);
var
  I: Integer;
begin
  AddIntPair(AObject, CCodeInt, Length(AInfo.Heights));
  for I := 0 to High(AInfo.Heights) do
  begin
    with AInfo.Heights[I] do
      AddPointPairs(AObject, X, Y, Z);
    AObject.AddPair(CCodeReal, AcadTableFloatToDXF(AInfo.Heights[I].Height));
    AddIntPair(AObject, CCodeInt, AInfo.Heights[I].Flags);
  end;
  AddIntPair(AObject, CCodeInt, Length(AInfo.RowRanges));
  for I := 0 to High(AInfo.RowRanges) do
  begin
    with AInfo.RowRanges[I] do
      AddPointPairs(AObject, OffsetX, OffsetY, OffsetZ);
    AddIntPair(AObject, CCodeInt, AInfo.RowRanges[I].StartRow);
    AddIntPair(AObject, CCodeInt, AInfo.RowRanges[I].EndRow);
  end;
end;

procedure BuildAcadTableSplitPairs(const AInfo: TZAcadTableSplitInfo;
  AObject: TZDXFRawObject);
var
  I: Integer;
begin
  AddIntPair(AObject, CCodeFlag, CXRecordCloneFlag);
  AObject.AddPair(CCodeMarker, CAcadTableRoundTripMarker);
  AObject.AddPair(CCodeContent, DXFHandleToStr(AInfo.ContentHandle));
  AddIntPair(AObject, CCodeLayout, CAcadTableLayoutSplit);
  AddIntPair(AObject, CCodeInt, AInfo.BreakFlags);
  AddIntPair(AObject, CCodeInt, AInfo.BreakDirection);
  AObject.AddPair(CCodeReal, AcadTableFloatToDXF(AInfo.BreakSpacing));
  AddIntPair(AObject, CCodeInt, AInfo.TopLabelRows);
  AddIntPair(AObject, CCodeInt, AInfo.BottomLabelRows);
  BuildHeightsAndRanges(AInfo, AObject);
  AddIntPair(AObject, CCodeInt, Length(AInfo.Continuations));
  for I := 0 to High(AInfo.Continuations) do
    AObject.AddPair(CCodeContinuation,
      DXFHandleToStr(AInfo.Continuations[I]));
  AObject.AddPair(CCodeGeometry, DXFHandleToStr(AInfo.GeometryHandle));
end;

{ TZAcadTableNODIndex }

constructor TZAcadTableNODIndex.Create(AModel: TZNODModel);
begin
  inherited Create;
  FByEntity := TDictionary<TDWGHandle, Integer>.Create;
  FContinuationOwner := TDictionary<TDWGHandle, TDWGHandle>.Create;
  if AModel <> nil then
  begin
    Build(AModel);
    LoadRecompose(AModel);
  end;
end;

destructor TZAcadTableNODIndex.Destroy;
begin
  FContinuationOwner.Free;
  FByEntity.Free;
  inherited Destroy;
end;

{ Сливает запись разрыва ASrc в ADst той же сущности: прежний ZCAD писал
  отдельную запись разрыва рядом с записью 70 2 расширенного словаря }
procedure MergeSplitInfo(var ADst: TZAcadTableSplitInfo;
  const ASrc: TZAcadTableSplitInfo);
var
  Content, Geometry: TDWGHandle;
begin
  Content := ADst.ContentHandle;
  Geometry := ADst.GeometryHandle;
  if ASrc.Layout = CAcadTableLayoutSplit then
    ADst := ASrc;
  if ADst.ContentHandle = 0 then
    ADst.ContentHandle := Content;
  if ADst.GeometryHandle = 0 then
    ADst.GeometryHandle := Geometry;
end;

procedure TZAcadTableNODIndex.AddInfo(const AInfo: TZAcadTableSplitInfo);
var
  Idx, I: Integer;
begin
  if FByEntity.TryGetValue(AInfo.EntityHandle, Idx) then
    MergeSplitInfo(FInfos[Idx], AInfo)
  else
  begin
    Idx := Length(FInfos);
    Insert(AInfo, FInfos, Idx);
    FByEntity.Add(AInfo.EntityHandle, Idx);
  end;
  for I := 0 to High(AInfo.Continuations) do
    FContinuationOwner.AddOrSetValue(AInfo.Continuations[I],
      AInfo.EntityHandle);
end;

{ Словарь-владелец XRECORD, если запись лежит под ключом
  ACAD_XREC_ROUNDTRIP (nil — запись без словаря) }
function RoundTripOwnerDict(AModel: TZNODModel;
  AObject: TZDXFRawObject): TZDXFDictionary;
var
  Entry: TZDXFDictEntry;
begin
  Result := AModel.FindDictionary(AObject.OwnerHandle);
  if (Result <> nil) and not (Result.FindEntry(CAcadTableXRecRoundTripKey,
    Entry) and (Entry.TargetHandle = AObject.Handle)) then
    Result := nil;
end;

procedure TZAcadTableNODIndex.Build(AModel: TZNODModel);
var
  I: Integer;
  Obj: TZDXFRawObject;
  Info: TZAcadTableSplitInfo;
begin
  for I := 0 to AModel.Objects.Count - 1 do
  begin
    Obj := AModel.Objects[I];
    if (Obj.ObjType = 'XRECORD') and ParseAcadTableSplitXRecord(Obj,
      RoundTripOwnerDict(AModel, Obj), Info) then
      AddInfo(Info);
  end;
  for I := 0 to High(FInfos) do
  begin
    FInfos[I].RowTypes := ReadAcadTableContentRowTypes(
      AModel.FindObject(FInfos[I].ContentHandle));
    NODLogTraceFormatStr('NOD: ACAD_TABLE %s: layout=%d flags=%d ' +
      'continuations=%d ranges=%d rows=%d legacy=%s',
      [DXFHandleToStr(FInfos[I].EntityHandle), FInfos[I].Layout,
       FInfos[I].BreakFlags, Length(FInfos[I].Continuations),
       Length(FInfos[I].RowRanges), Length(FInfos[I].RowTypes),
       BoolToStr(FInfos[I].Legacy, True)]);
  end;
end;

procedure TZAcadTableNODIndex.LoadRecompose(AModel: TZNODModel);
var
  Entry: TZDXFDictEntry;
  Obj: TZDXFRawObject;
  I, Start: Integer;
  H: TDWGHandle;
begin
  if (AModel.NOD = nil) or
     not AModel.NOD.FindEntry(CAcadTableRecomposeKey, Entry) then
    Exit;
  Obj := AModel.FindObject(Entry.TargetHandle);
  if Obj = nil then
    Exit;
  FRecomposeHandle := Obj.Handle;
  Start := XRecordBodyStart(Obj);
  if Start < 0 then
    Exit;
  for I := Start to Obj.PairCount - 1 do
    if (Obj.Pairs[I].Code = CCodeContinuation) and
       TryDXFStrToHandle(Obj.Pairs[I].Value, H) and (H <> 0) then
      Insert(H, FRecomposeRefs, Length(FRecomposeRefs));
end;

function TZAcadTableNODIndex.FindByEntity(AEntity: TDWGHandle;
  out AInfo: TZAcadTableSplitInfo): Boolean;
var
  Idx: Integer;
begin
  Result := FByEntity.TryGetValue(AEntity, Idx);
  if Result then
    AInfo := FInfos[Idx]
  else
    ClearSplitInfo(AInfo);
end;

function TZAcadTableNODIndex.IsContinuation(AEntity: TDWGHandle;
  out AMain: TDWGHandle): Boolean;
begin
  Result := FContinuationOwner.TryGetValue(AEntity, AMain);
  if not Result then
    AMain := 0;
end;

function TZAcadTableNODIndex.GetCount: Integer;
begin
  Result := Length(FInfos);
end;

function TZAcadTableNODIndex.GetInfo(AIndex: Integer): TZAcadTableSplitInfo;
begin
  Result := FInfos[AIndex];
end;

initialization
  DXFFormat := DefaultFormatSettings;
  DXFFormat.DecimalSeparator := '.';
  DXFFormat.ThousandSeparator := #0;
end.
