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
  Чистая логика HTTP-команд SPACE_ADD_ROOM и SPACE_ADD_POLY (issue #1480).

  Команды рисуют зону (пространство) так же, как интерактивная команда
  SpaceAddPoly: замкнутая 3D-полилиния со слоем, цветом, расширениями
  extdrVariables/extdrIncludingVolume и переменными из строки операндов
  кнопки панели. Отличие только в том, что вершины и значения переменных
  (например, номер помещения Space_RoomPos) приходят по HTTP.

  Модуль не зависит от ZCAD: здесь пресеты операндов, разбор и проверка
  запроса, геометрия контура и JSON-ответы. Поэтому он проверяется обычным
  fpc в CI, а создание примитива вынесено в uzvhttpcmdspace и
  uzCVCommand_SpaceAddPoly.
}

{$mode objfpc}{$H+}

unit uzvhttpspaceroom;

interface

uses
  SysUtils, Classes, Math, fpjson, jsonparser;

const
  { Имена команд HTTP IPC и операций в JSON-ответе }
  SPACE_IPC_ADD_ROOM = 'SPACE_ADD_ROOM';
  SPACE_IPC_ADD_POLY = 'SPACE_ADD_POLY';
  SPACE_COMMAND_ADD_ROOM = 'space_add_room';
  SPACE_COMMAND_ADD_POLY = 'space_add_poly';

  { Коды ошибок }
  SPACE_ERR_INVALID_REQUEST = 'INVALID_REQUEST';
  SPACE_ERR_INVALID_KIND = 'INVALID_KIND';
  SPACE_ERR_INVALID_OPERANDS = 'INVALID_OPERANDS';
  SPACE_ERR_INVALID_POINTS = 'INVALID_POINTS';
  SPACE_ERR_SELF_INTERSECTING = 'SELF_INTERSECTING';
  SPACE_ERR_INVALID_VALUE = 'INVALID_VALUE';
  SPACE_ERR_MISSING_NUMBER = 'MISSING_NUMBER';
  SPACE_ERR_NO_DOCUMENT = 'NO_DOCUMENT';
  SPACE_ERR_CREATE_FAILED = 'CREATE_FAILED';
  SPACE_ERR_INTERNAL_ERROR = 'INTERNAL_ERROR';

  { Виды зон: совпадают с кнопками панели «Пространства» }
  SPACE_KIND_ROOM = 'room';
  SPACE_KIND_FLOOR = 'floor';
  SPACE_KIND_SECTION = 'section';
  SPACE_KIND_BUILDING = 'building';
  SPACE_KIND_CUSTOM = 'custom';

  { Операнды кнопок панели (vspaceactionscontent.xml) без изменений.
    Именно с ними пользователь вызывает SpaceAddPoly из интерфейса. }
  SPACE_OPERANDS_ROOM = '5,''spaceRoom'',Space_RoomPos, ''RoomPos'', String, ' +
    'Space_RoomName, ''RoomName'', String, Space_RoomArea, ''RoomArea'', String, ' +
    'Space_RoomCategory, ''RoomCategory'', String, Space_RoomIllumination, ' +
    '''RoomIllumination Лк'', Integer';
  SPACE_OPERANDS_FLOOR = '4,''spaceFloor'',space_Floor, ''FloorNum'', string,' +
    'space_FloorHeight, ''HeightFloor'', Double, space_FloorScale, ' +
    '''ScaleFloor'', Double';
  SPACE_OPERANDS_SECTION = '6,''spaceSection'',space_Section, ''Section'', string';
  SPACE_OPERANDS_BUILDING = '3,''spaceBuild'',space_Building, ''Build'', string';

  { Значения по умолчанию, как в ParseOperandsToStruct }
  SPACE_COLOR_BYLAYER = 256;
  SPACE_COLOR_MIN = 0;
  SPACE_COLOR_MAX = 256;

  { Ограничения контура }
  SPACE_MIN_VERTICES = 3;
  SPACE_MAX_VERTICES = 4096;
  SPACE_EPSILON = 1e-9;

  { Типы переменных, которые принимает HTTP-команда }
  SPACE_TYPE_STRING = 'String';
  SPACE_TYPE_INTEGER = 'Integer';
  SPACE_TYPE_DOUBLE = 'Double';
  SPACE_TYPE_BOOLEAN = 'Boolean';

type
  { Ошибка разбора или выполнения с кодом для JSON-ответа }
  ESpaceRoom = class(Exception)
  public
    Code: string;
    constructor CreateCode(const ACode, AMessage: string);
  end;

  { Вершина контура в координатах чертежа }
  TSpacePoint = record
    X, Y, Z: Double;
  end;
  TSpacePoints = array of TSpacePoint;

  { Объявление переменной: тройка операндов var,'username',type }
  TSpaceParam = record
    VarName: string;
    UserName: string;
    TypeName: string;
  end;
  TSpaceParams = array of TSpaceParam;

  { Разобранная строка операндов SpaceAddPoly }
  TSpaceOperands = record
    IndexColor: Integer;
    LayerName: string;
    Params: TSpaceParams;
  end;

  { Значение переменной в виде строки для SetValueFromString }
  TSpaceValue = record
    VarName: string;
    TypeName: string;
    Value: string;
  end;
  TSpaceValues = array of TSpaceValue;

  { Геометрия контура в плоскости XY }
  TSpaceGeometry = record
    Area: Double;
    Perimeter: Double;
    MinX, MinY, MaxX, MaxY: Double;
    CentroidX, CentroidY: Double;
  end;

  { Полностью проверенный запрос: всё, что нужно для создания зоны }
  TSpaceRequest = record
    Command: string;
    Kind: string;
    Operands: string;
    Parsed: TSpaceOperands;
    Points: TSpacePoints;
    Values: TSpaceValues;
    Geometry: TSpaceGeometry;
    Select: Boolean;
  end;

function SpaceCommandName(const AIPCCommand: string): string;
function SpaceKindOperands(const AKind: string; out AOperands: string): Boolean;
function SpaceNumberVar(const AKind: string): string;
function SpaceParseOperands(const AOperands: string): TSpaceOperands;
function SpaceBuildOperands(const AOperands: TSpaceOperands): string;
function SpaceNormalizeType(const ATypeName: string; out ANormalized: string): Boolean;
function SpaceFindParam(const AOperands: TSpaceOperands; const AVarName: string): Integer;
function SpaceNormalizePoints(const APoints: TSpacePoints): TSpacePoints;
function SpaceIsSelfIntersecting(const APoints: TSpacePoints): Boolean;
function SpaceGeometryOf(const APoints: TSpacePoints): TSpaceGeometry;
function SpaceFloatToStr(AValue: Double): string;
function SpaceRequestParse(const AIPCCommand: string; AArgs: TJSONArray): TSpaceRequest;
function SpaceSuccessJSON(const ARequest: TSpaceRequest; ABatch: Boolean): string;
function SpaceErrorJSON(const ACommand, ACode, AMessage: string): string;
function SpaceErrorCodeOf(const AJSON: string): string;

implementation

type
  { Сокращение запроса: поле JSON -> переменная зоны своего вида }
  TSpaceShortcut = record
    Kind: string;
    Field: string;
    VarName: string;
  end;

const
  { «number» — идентифицирующая переменная зоны любого вида }
  SPACE_SHORTCUTS: array[0..9] of TSpaceShortcut = (
    (Kind: SPACE_KIND_ROOM; Field: 'number'; VarName: 'Space_RoomPos'),
    (Kind: SPACE_KIND_ROOM; Field: 'name'; VarName: 'Space_RoomName'),
    (Kind: SPACE_KIND_ROOM; Field: 'area'; VarName: 'Space_RoomArea'),
    (Kind: SPACE_KIND_ROOM; Field: 'category'; VarName: 'Space_RoomCategory'),
    (Kind: SPACE_KIND_ROOM; Field: 'illumination'; VarName: 'Space_RoomIllumination'),
    (Kind: SPACE_KIND_FLOOR; Field: 'number'; VarName: 'space_Floor'),
    (Kind: SPACE_KIND_FLOOR; Field: 'height'; VarName: 'space_FloorHeight'),
    (Kind: SPACE_KIND_FLOOR; Field: 'scale'; VarName: 'space_FloorScale'),
    (Kind: SPACE_KIND_SECTION; Field: 'number'; VarName: 'space_Section'),
    (Kind: SPACE_KIND_BUILDING; Field: 'number'; VarName: 'space_Building'));

  { Поля-сокращения в порядке применения }
  SPACE_SHORTCUT_FIELDS: array[0..6] of string = (
    'number', 'name', 'area', 'category', 'illumination', 'height', 'scale');

  { Остальные поля верхнего уровня, которые понимает команда }
  SPACE_KNOWN_FIELDS: array[0..6] of string = (
    'kind', 'points', 'operands', 'layer', 'color', 'values', 'select');

var
  { Точка как десятичный разделитель независимо от локали }
  SpaceFormat: TFormatSettings;

constructor ESpaceRoom.CreateCode(const ACode, AMessage: string);
begin
  inherited Create(AMessage);
  Code := ACode;
end;

procedure Fail(const ACode, AMessage: string);
begin
  raise ESpaceRoom.CreateCode(ACode, AMessage);
end;

function SpaceCommandName(const AIPCCommand: string): string;
begin
  if SameText(AIPCCommand, SPACE_IPC_ADD_POLY) then
    Result := SPACE_COMMAND_ADD_POLY
  else
    Result := SPACE_COMMAND_ADD_ROOM;
end;

function SpaceKindOperands(const AKind: string; out AOperands: string): Boolean;
begin
  Result := True;
  if SameText(AKind, SPACE_KIND_ROOM) then
    AOperands := SPACE_OPERANDS_ROOM
  else if SameText(AKind, SPACE_KIND_FLOOR) then
    AOperands := SPACE_OPERANDS_FLOOR
  else if SameText(AKind, SPACE_KIND_SECTION) then
    AOperands := SPACE_OPERANDS_SECTION
  else if SameText(AKind, SPACE_KIND_BUILDING) then
    AOperands := SPACE_OPERANDS_BUILDING
  else begin
    AOperands := '';
    Result := False;
  end;
end;

{ Переменная, в которую пишется поле-сокращение для вида зоны, или '' }
function ShortcutVar(const AKind, AField: string): string;
var
  Shortcut: TSpaceShortcut;
begin
  Result := '';
  for Shortcut in SPACE_SHORTCUTS do
    if SameText(Shortcut.Kind, AKind) and (Shortcut.Field = AField) then
      Exit(Shortcut.VarName);
end;

function SpaceNumberVar(const AKind: string): string;
begin
  Result := ShortcutVar(AKind, 'number');
end;

{ Снимает внешние кавычки так же, как RemoveQuotes в uzvcommand_spaceutils }
function StripQuotes(const S: string): string;
begin
  Result := S;
  if (Length(Result) >= 2) and (Result[1] = '''') and
     (Result[Length(Result)] = '''') then
    Result := Copy(Result, 2, Length(Result) - 2);
  if (Length(Result) >= 2) and (Result[1] = '"') and
     (Result[Length(Result)] = '"') then
    Result := Copy(Result, 2, Length(Result) - 2);
end;

{ Повторяет ParseOperandsToStruct: цвет, слой и тройки var,'username',type }
function SpaceParseOperands(const AOperands: string): TSpaceOperands;
var
  Parts: TStringList;
  I, Color: Integer;
begin
  Result.IndexColor := SPACE_COLOR_BYLAYER;
  Result.LayerName := '';
  Result.Params := nil;
  if Trim(AOperands) = '' then
    Exit;
  Parts := TStringList.Create;
  try
    Parts.Delimiter := ',';
    Parts.StrictDelimiter := True;
    Parts.DelimitedText := AOperands;
    if Parts.Count >= 1 then begin
      Color := StrToIntDef(Trim(Parts[0]), SPACE_COLOR_BYLAYER);
      if (Color < SPACE_COLOR_MIN) or (Color > SPACE_COLOR_MAX) then
        Color := SPACE_COLOR_BYLAYER;
      Result.IndexColor := Color;
    end;
    if Parts.Count >= 2 then
      Result.LayerName := StripQuotes(Trim(Parts[1]));
    I := 2;
    while I + 2 < Parts.Count do begin
      SetLength(Result.Params, Length(Result.Params) + 1);
      with Result.Params[High(Result.Params)] do begin
        VarName := Trim(Parts[I]);
        UserName := StripQuotes(Trim(Parts[I + 1]));
        TypeName := Trim(Parts[I + 2]);
      end;
      Inc(I, 3);
    end;
  finally
    Parts.Free;
  end;
end;

function SpaceBuildOperands(const AOperands: TSpaceOperands): string;
var
  Param: TSpaceParam;
begin
  Result := Format('%d,''%s''', [AOperands.IndexColor, AOperands.LayerName]);
  for Param in AOperands.Params do
    Result := Result + Format(',%s,''%s'',%s',
      [Param.VarName, Param.UserName, Param.TypeName]);
end;

function SpaceNormalizeType(const ATypeName: string; out ANormalized: string): Boolean;
const
  TYPES: array[0..3] of string = (SPACE_TYPE_STRING, SPACE_TYPE_INTEGER,
    SPACE_TYPE_DOUBLE, SPACE_TYPE_BOOLEAN);
var
  TypeName: string;
begin
  for TypeName in TYPES do
    if SameText(TypeName, Trim(ATypeName)) then begin
      ANormalized := TypeName;
      Exit(True);
    end;
  ANormalized := '';
  Result := False;
end;

function SpaceFindParam(const AOperands: TSpaceOperands; const AVarName: string): Integer;
var
  I: Integer;
begin
  for I := 0 to High(AOperands.Params) do
    if SameText(AOperands.Params[I].VarName, AVarName) then
      Exit(I);
  Result := -1;
end;

{ Имя переменной: латинский идентификатор, как в скриптах ZCAD }
function IsIdentifier(const S: string): Boolean;
var
  I: Integer;
begin
  Result := (S <> '') and (S[1] in ['A'..'Z', 'a'..'z', '_']);
  if not Result then
    Exit;
  for I := 2 to Length(S) do
    if not (S[I] in ['A'..'Z', 'a'..'z', '0'..'9', '_']) then
      Exit(False);
end;

{ Текст, который безопасно вставить в строку операндов между кавычками }
function IsSafeOperandText(const S: string): Boolean;
begin
  Result := (Pos(',', S) = 0) and (Pos('''', S) = 0) and (Pos('"', S) = 0) and
    (Pos(#10, S) = 0) and (Pos(#13, S) = 0);
end;

{ Проверяет операнды: каждая переменная объявлена корректно и один раз }
procedure ValidateOperands(const AOperands: TSpaceOperands);
var
  I: Integer;
  Normalized: string;
begin
  if not IsSafeOperandText(AOperands.LayerName) then
    Fail(SPACE_ERR_INVALID_OPERANDS, Format(
      'Layer name "%s" must not contain commas or quotes', [AOperands.LayerName]));
  for I := 0 to High(AOperands.Params) do
    with AOperands.Params[I] do begin
      if not IsIdentifier(VarName) then
        Fail(SPACE_ERR_INVALID_OPERANDS, Format(
          'Variable name "%s" must be a latin identifier', [VarName]));
      if not IsSafeOperandText(UserName) then
        Fail(SPACE_ERR_INVALID_OPERANDS, Format(
          'Description of "%s" must not contain commas or quotes', [VarName]));
      if not SpaceNormalizeType(TypeName, Normalized) then
        Fail(SPACE_ERR_INVALID_OPERANDS, Format(
          'Variable "%s" has unsupported type "%s" (String, Integer, Double, Boolean)',
          [VarName, TypeName]));
      if SpaceFindParam(AOperands, VarName) <> I then
        Fail(SPACE_ERR_INVALID_OPERANDS, Format(
          'Variable "%s" is declared twice', [VarName]));
    end;
end;

function SamePoint(const A, B: TSpacePoint): Boolean;
begin
  Result := (Abs(A.X - B.X) <= SPACE_EPSILON) and (Abs(A.Y - B.Y) <= SPACE_EPSILON);
end;

function SpaceNormalizePoints(const APoints: TSpacePoints): TSpacePoints;
var
  P: TSpacePoint;
  Count: Integer;
begin
  Result := nil;
  SetLength(Result, Length(APoints));
  Count := 0;
  for P in APoints do begin
    if (Count > 0) and SamePoint(Result[Count - 1], P) then
      Continue;
    Result[Count] := P;
    Inc(Count);
  end;
  { Замыкающая вершина не нужна: полилиния замыкается флагом Closed }
  while (Count > 1) and SamePoint(Result[0], Result[Count - 1]) do
    Dec(Count);
  SetLength(Result, Count);
end;

{ Знак векторного произведения (B - A) x (C - A) }
function Orientation(const A, B, C: TSpacePoint): Integer;
var
  Cross: Double;
begin
  Cross := (B.X - A.X) * (C.Y - A.Y) - (B.Y - A.Y) * (C.X - A.X);
  if Cross > SPACE_EPSILON then
    Result := 1
  else if Cross < -SPACE_EPSILON then
    Result := -1
  else
    Result := 0;
end;

{ Точка C лежит в прямоугольнике отрезка AB (для коллинеарного случая) }
function OnSegment(const A, B, C: TSpacePoint): Boolean;
begin
  Result := (C.X <= Max(A.X, B.X) + SPACE_EPSILON) and
    (C.X >= Min(A.X, B.X) - SPACE_EPSILON) and
    (C.Y <= Max(A.Y, B.Y) + SPACE_EPSILON) and
    (C.Y >= Min(A.Y, B.Y) - SPACE_EPSILON);
end;

function SegmentsIntersect(const A, B, C, D: TSpacePoint): Boolean;
var
  O1, O2, O3, O4: Integer;
begin
  O1 := Orientation(A, B, C);
  O2 := Orientation(A, B, D);
  O3 := Orientation(C, D, A);
  O4 := Orientation(C, D, B);
  if (O1 <> O2) and (O3 <> O4) then
    Exit(True);
  Result := ((O1 = 0) and OnSegment(A, B, C)) or ((O2 = 0) and OnSegment(A, B, D)) or
    ((O3 = 0) and OnSegment(C, D, A)) or ((O4 = 0) and OnSegment(C, D, B));
end;

function SpaceIsSelfIntersecting(const APoints: TSpacePoints): Boolean;
var
  N, I, J: Integer;
begin
  Result := False;
  N := Length(APoints);
  if N < 4 then
    Exit;
  for I := 0 to N - 1 do
    for J := I + 1 to N - 1 do begin
      { Соседние рёбра имеют общую вершину и не считаются пересечением }
      if (J = I + 1) or ((I = 0) and (J = N - 1)) then
        Continue;
      if SegmentsIntersect(APoints[I], APoints[(I + 1) mod N],
        APoints[J], APoints[(J + 1) mod N]) then
        Exit(True);
    end;
end;

function SpaceGeometryOf(const APoints: TSpacePoints): TSpaceGeometry;
var
  N, I: Integer;
  A, B: TSpacePoint;
  Cross, Signed: Double;
begin
  Result := Default(TSpaceGeometry);
  N := Length(APoints);
  if N = 0 then
    Exit;
  Result.MinX := APoints[0].X;
  Result.MaxX := APoints[0].X;
  Result.MinY := APoints[0].Y;
  Result.MaxY := APoints[0].Y;
  Signed := 0;
  for I := 0 to N - 1 do begin
    A := APoints[I];
    B := APoints[(I + 1) mod N];
    Cross := A.X * B.Y - B.X * A.Y;
    Signed := Signed + Cross;
    Result.CentroidX := Result.CentroidX + (A.X + B.X) * Cross;
    Result.CentroidY := Result.CentroidY + (A.Y + B.Y) * Cross;
    Result.Perimeter := Result.Perimeter + Hypot(B.X - A.X, B.Y - A.Y);
    Result.MinX := Min(Result.MinX, A.X);
    Result.MaxX := Max(Result.MaxX, A.X);
    Result.MinY := Min(Result.MinY, A.Y);
    Result.MaxY := Max(Result.MaxY, A.Y);
  end;
  Signed := Signed / 2;
  Result.Area := Abs(Signed);
  if Abs(Signed) > SPACE_EPSILON then begin
    Result.CentroidX := Result.CentroidX / (6 * Signed);
    Result.CentroidY := Result.CentroidY / (6 * Signed);
  end else begin
    Result.CentroidX := (Result.MinX + Result.MaxX) / 2;
    Result.CentroidY := (Result.MinY + Result.MaxY) / 2;
  end;
end;

function SpaceFloatToStr(AValue: Double): string;
begin
  Result := FloatToStr(AValue, SpaceFormat);
end;

{ Число из JSON: целое и дробное значения, конечное }
function JSONNumber(AData: TJSONData; out AValue: Double): Boolean;
begin
  Result := (AData <> nil) and (AData.JSONType = jtNumber);
  if not Result then
    Exit;
  AValue := AData.AsFloat;
  Result := not (IsNan(AValue) or IsInfinite(AValue));
end;

// Вершина: [x,y], [x,y,z] или объект с полями x, y и необязательным z
function ParsePoint(AData: TJSONData; AIndex: Integer): TSpacePoint;
var
  Arr: TJSONArray;
  Obj: TJSONObject;

  function Coord(AItem: TJSONData; const AName: string; ARequired: Boolean): Double;
  begin
    Result := 0;
    if (AItem = nil) and not ARequired then
      Exit;
    if not JSONNumber(AItem, Result) then
      Fail(SPACE_ERR_INVALID_POINTS, Format(
        'Point %d: coordinate %s must be a number', [AIndex, AName]));
  end;

begin
  if AData.JSONType = jtArray then begin
    Arr := TJSONArray(AData);
    if (Arr.Count < 2) or (Arr.Count > 3) then
      Fail(SPACE_ERR_INVALID_POINTS, Format(
        'Point %d must be [x, y] or [x, y, z]', [AIndex]));
    Result.X := Coord(Arr.Items[0], 'x', True);
    Result.Y := Coord(Arr.Items[1], 'y', True);
    if Arr.Count = 3 then
      Result.Z := Coord(Arr.Items[2], 'z', True)
    else
      Result.Z := 0;
  end else if AData.JSONType = jtObject then begin
    Obj := TJSONObject(AData);
    Result.X := Coord(Obj.Find('x'), 'x', True);
    Result.Y := Coord(Obj.Find('y'), 'y', True);
    Result.Z := Coord(Obj.Find('z'), 'z', False);
  end else
    Fail(SPACE_ERR_INVALID_POINTS, Format(
      'Point %d must be [x, y] or {"x": .., "y": ..}', [AIndex]));
end;

function ParsePoints(AData: TJSONData): TSpacePoints;
var
  Arr: TJSONArray;
  I: Integer;
begin
  Result := nil;
  if (AData = nil) or (AData.JSONType <> jtArray) then
    Fail(SPACE_ERR_INVALID_POINTS, 'Field "points" must be an array of vertices');
  Arr := TJSONArray(AData);
  if Arr.Count > SPACE_MAX_VERTICES then
    Fail(SPACE_ERR_INVALID_POINTS, Format(
      'Too many vertices: %d (maximum %d)', [Arr.Count, SPACE_MAX_VERTICES]));
  SetLength(Result, Arr.Count);
  for I := 0 to Arr.Count - 1 do
    Result[I] := ParsePoint(Arr.Items[I], I);
  Result := SpaceNormalizePoints(Result);
  if Length(Result) < SPACE_MIN_VERTICES then
    Fail(SPACE_ERR_INVALID_POINTS, Format(
      'A zone needs at least %d distinct vertices', [SPACE_MIN_VERTICES]));
  if SpaceIsSelfIntersecting(Result) then
    Fail(SPACE_ERR_SELF_INTERSECTING, 'Zone contour must not intersect itself');
  if SpaceGeometryOf(Result).Area <= SPACE_EPSILON then
    Fail(SPACE_ERR_INVALID_POINTS, 'Zone contour has zero area');
end;

{ Тип переменной, которую не объявили операнды, по значению JSON }
function InferType(AData: TJSONData): string;
begin
  case AData.JSONType of
    jtBoolean: Result := SPACE_TYPE_BOOLEAN;
    jtNumber:
      if TJSONNumber(AData).NumberType = ntFloat then
        Result := SPACE_TYPE_DOUBLE
      else
        Result := SPACE_TYPE_INTEGER;
  else
    Result := SPACE_TYPE_STRING;
  end;
end;

{ Приводит значение JSON к строке, которую примет SetValueFromString типа }
function ValueToString(const AVarName, ATypeName: string; AData: TJSONData): string;
var
  TypeName, Text: string;
  IntValue: Int64;
  FloatValue: Double;
  BoolValue: Boolean;

  procedure Bad;
  begin
    Fail(SPACE_ERR_INVALID_VALUE, Format(
      'Value of "%s" is not a valid %s', [AVarName, TypeName]));
  end;

begin
  SpaceNormalizeType(ATypeName, TypeName);
  if not (AData.JSONType in [jtString, jtNumber, jtBoolean]) then
    Bad;
  if TypeName = SPACE_TYPE_STRING then begin
    Result := AData.AsString;
    if (Pos(#10, Result) > 0) or (Pos(#13, Result) > 0) then
      Fail(SPACE_ERR_INVALID_VALUE, Format(
        'Value of "%s" must be a single line', [AVarName]));
  end else if TypeName = SPACE_TYPE_INTEGER then begin
    if AData.JSONType = jtNumber then begin
      if TJSONNumber(AData).NumberType = ntFloat then begin
        FloatValue := AData.AsFloat;
        if (Frac(FloatValue) <> 0) or (Abs(FloatValue) > High(LongInt)) then
          Bad;
        IntValue := Trunc(FloatValue);
      end else
        IntValue := AData.AsInt64;
    end else if not TryStrToInt64(Trim(AData.AsString), IntValue) then
      Bad;
    if (IntValue < Low(LongInt)) or (IntValue > High(LongInt)) then
      Bad;
    Result := IntToStr(IntValue);
  end else if TypeName = SPACE_TYPE_DOUBLE then begin
    if AData.JSONType = jtNumber then
      FloatValue := AData.AsFloat
    else begin
      Text := StringReplace(Trim(AData.AsString), ',', '.', [rfReplaceAll]);
      if not TryStrToFloat(Text, FloatValue, SpaceFormat) then
        Bad;
    end;
    if IsNan(FloatValue) or IsInfinite(FloatValue) then
      Bad;
    Result := SpaceFloatToStr(FloatValue);
  end else begin
    if AData.JSONType = jtBoolean then
      BoolValue := AData.AsBoolean
    else if not TryStrToBool(Trim(AData.AsString), BoolValue) then
      Bad;
    Result := BoolToStr(BoolValue, True);
  end;
end;

{ Добавляет или заменяет значение переменной; объявляет её при необходимости }
procedure PutValue(var ARequest: TSpaceRequest; const AVarName: string;
  AData: TJSONData);
var
  Index, I: Integer;
  Param: TSpaceParam;
  Value: TSpaceValue;
begin
  if (AData = nil) or (AData.JSONType = jtNull) then
    Exit;
  Index := SpaceFindParam(ARequest.Parsed, AVarName);
  if Index < 0 then begin
    if not IsIdentifier(AVarName) then
      Fail(SPACE_ERR_INVALID_VALUE, Format(
        'Variable name "%s" must be a latin identifier', [AVarName]));
    if not (AData.JSONType in [jtString, jtNumber, jtBoolean]) then
      Fail(SPACE_ERR_INVALID_VALUE, Format(
        'Value of "%s" must be a string, number or boolean', [AVarName]));
    Param.VarName := AVarName;
    Param.UserName := AVarName;
    Param.TypeName := InferType(AData);
    SetLength(ARequest.Parsed.Params, Length(ARequest.Parsed.Params) + 1);
    ARequest.Parsed.Params[High(ARequest.Parsed.Params)] := Param;
  end else
    Param := ARequest.Parsed.Params[Index];
  Value.VarName := Param.VarName;
  SpaceNormalizeType(Param.TypeName, Value.TypeName);
  Value.Value := ValueToString(Param.VarName, Param.TypeName, AData);
  for I := 0 to High(ARequest.Values) do
    if SameText(ARequest.Values[I].VarName, Value.VarName) then begin
      ARequest.Values[I] := Value;
      Exit;
    end;
  SetLength(ARequest.Values, Length(ARequest.Values) + 1);
  ARequest.Values[High(ARequest.Values)] := Value;
end;

{ Объект параметров: args[0] — объект или строка с JSON объекта }
function ParamsObject(AArgs: TJSONArray; out AOwned: TJSONData): TJSONObject;
var
  Item: TJSONData;
begin
  AOwned := nil;
  if (AArgs = nil) or (AArgs.Count <> 1) then
    Fail(SPACE_ERR_INVALID_REQUEST,
      'args must contain one JSON object with zone parameters');
  Item := AArgs.Items[0];
  if Item.JSONType = jtString then begin
    try
      AOwned := GetJSON(Item.AsString);
    except
      on E: Exception do
        Fail(SPACE_ERR_INVALID_REQUEST, 'args[0] is not valid JSON: ' + E.Message);
    end;
    Item := AOwned;
  end;
  if (Item = nil) or (Item.JSONType <> jtObject) then begin
    FreeAndNil(AOwned);
    Fail(SPACE_ERR_INVALID_REQUEST,
      'args must contain one JSON object with zone parameters');
  end;
  Result := TJSONObject(Item);
end;

function IsKnownField(const AName: string): Boolean;
var
  Field: string;
begin
  for Field in SPACE_KNOWN_FIELDS do
    if Field = AName then
      Exit(True);
  for Field in SPACE_SHORTCUT_FIELDS do
    if Field = AName then
      Exit(True);
  Result := False;
end;

function StringField(AParams: TJSONObject; const AName: string;
  out AValue: string): Boolean;
var
  Data: TJSONData;
begin
  Data := AParams.Find(AName);
  Result := (Data <> nil) and (Data.JSONType <> jtNull);
  if not Result then
    Exit;
  if Data.JSONType <> jtString then
    Fail(SPACE_ERR_INVALID_REQUEST, Format('Field "%s" must be a string', [AName]));
  AValue := Data.AsString;
end;

procedure ParseRequestObject(var ARequest: TSpaceRequest; AParams: TJSONObject;
  const AIPCCommand: string);
var
  Text, NumberVar: string;
  Data: TJSONData;
  Values: TJSONObject;
  I: Integer;
  Field: string;
begin
  for I := 0 to AParams.Count - 1 do
    if not IsKnownField(AParams.Names[I]) then
      Fail(SPACE_ERR_INVALID_REQUEST, Format('Unknown field "%s"', [AParams.Names[I]]));

  { Вид зоны: SPACE_ADD_ROOM всегда рисует помещение }
  if SameText(AIPCCommand, SPACE_IPC_ADD_ROOM) then begin
    ARequest.Kind := SPACE_KIND_ROOM;
    if StringField(AParams, 'kind', Text) and not SameText(Text, SPACE_KIND_ROOM) then
      Fail(SPACE_ERR_INVALID_KIND, 'SPACE_ADD_ROOM draws only kind "room"');
  end else if StringField(AParams, 'kind', Text) then
    ARequest.Kind := LowerCase(Trim(Text))
  else if AParams.Find('operands') <> nil then
    ARequest.Kind := SPACE_KIND_CUSTOM
  else
    ARequest.Kind := SPACE_KIND_ROOM;

  { Операнды: пресет вида или строка как у кнопки панели }
  if StringField(AParams, 'operands', Text) then begin
    if Trim(Text) = '' then
      Fail(SPACE_ERR_INVALID_OPERANDS, 'Field "operands" must not be empty');
    ARequest.Operands := Text;
  end else if ARequest.Kind = SPACE_KIND_CUSTOM then
    Fail(SPACE_ERR_INVALID_OPERANDS, 'Kind "custom" requires field "operands"')
  else if not SpaceKindOperands(ARequest.Kind, ARequest.Operands) then
    Fail(SPACE_ERR_INVALID_KIND, Format(
      'Unknown kind "%s" (room, floor, section, building, custom)', [ARequest.Kind]));
  ARequest.Parsed := SpaceParseOperands(ARequest.Operands);

  { Переопределение слоя и цвета }
  if StringField(AParams, 'layer', Text) then
    ARequest.Parsed.LayerName := Trim(Text);
  Data := AParams.Find('color');
  if (Data <> nil) and (Data.JSONType <> jtNull) then begin
    if (Data.JSONType <> jtNumber) or (TJSONNumber(Data).NumberType = ntFloat) or
       (Data.AsInt64 < SPACE_COLOR_MIN) or (Data.AsInt64 > SPACE_COLOR_MAX) then
      Fail(SPACE_ERR_INVALID_REQUEST, Format(
        'Field "color" must be an integer %d..%d (256 = ByLayer)',
        [SPACE_COLOR_MIN, SPACE_COLOR_MAX]));
    ARequest.Parsed.IndexColor := Data.AsInteger;
  end;
  ValidateOperands(ARequest.Parsed);

  ARequest.Points := ParsePoints(AParams.Find('points'));

  { Значения: сначала values, затем сокращения (они важнее) }
  Data := AParams.Find('values');
  if (Data <> nil) and (Data.JSONType <> jtNull) then begin
    if Data.JSONType <> jtObject then
      Fail(SPACE_ERR_INVALID_REQUEST, 'Field "values" must be an object');
    Values := TJSONObject(Data);
    for I := 0 to Values.Count - 1 do
      PutValue(ARequest, Values.Names[I], Values.Items[I]);
  end;
  for Field in SPACE_SHORTCUT_FIELDS do begin
    Data := AParams.Find(Field);
    if (Data = nil) or (Data.JSONType = jtNull) then
      Continue;
    NumberVar := ShortcutVar(ARequest.Kind, Field);
    { Сокращение, не подходящее виду зоны, — вероятная ошибка клиента }
    if NumberVar = '' then
      Fail(SPACE_ERR_INVALID_REQUEST, Format(
        'Field "%s" is not used by kind "%s"; use "values"', [Field, ARequest.Kind]));
    PutValue(ARequest, NumberVar, Data);
  end;

  { Номер обязателен для SPACE_ADD_ROOM: ради него команда и существует }
  if SameText(AIPCCommand, SPACE_IPC_ADD_ROOM) then begin
    NumberVar := SpaceNumberVar(SPACE_KIND_ROOM);
    Text := '';
    for I := 0 to High(ARequest.Values) do
      if SameText(ARequest.Values[I].VarName, NumberVar) then
        Text := Trim(ARequest.Values[I].Value);
    if Text = '' then
      Fail(SPACE_ERR_MISSING_NUMBER,
        'SPACE_ADD_ROOM requires a non-empty room number ("number")');
  end;

  Data := AParams.Find('select');
  if (Data <> nil) and (Data.JSONType <> jtNull) then begin
    if Data.JSONType <> jtBoolean then
      Fail(SPACE_ERR_INVALID_REQUEST, 'Field "select" must be a boolean');
    ARequest.Select := Data.AsBoolean;
  end;

  ARequest.Operands := SpaceBuildOperands(ARequest.Parsed);
  ARequest.Geometry := SpaceGeometryOf(ARequest.Points);
end;

function SpaceRequestParse(const AIPCCommand: string; AArgs: TJSONArray): TSpaceRequest;
var
  Params: TJSONObject;
  Owned: TJSONData;
begin
  Result := Default(TSpaceRequest);
  Result.Command := SpaceCommandName(AIPCCommand);
  Params := ParamsObject(AArgs, Owned);
  try
    ParseRequestObject(Result, Params, AIPCCommand);
  finally
    Owned.Free;
  end;
end;

function PointJSON(AX, AY: Double): TJSONArray;
begin
  Result := TJSONArray.Create;
  Result.Add(AX);
  Result.Add(AY);
end;

function SpaceSuccessJSON(const ARequest: TSpaceRequest; ABatch: Boolean): string;
var
  Response, BBox, Variables: TJSONObject;
  Vertices: TJSONArray;
  Value: TSpaceValue;
  Param: TSpaceParam;
  P: TSpacePoint;
  Vertex: TJSONArray;
begin
  Response := TJSONObject.Create;
  try
    Response.Add('ok', True);
    Response.Add('command', ARequest.Command);
    Response.Add('kind', ARequest.Kind);
    Response.Add('layer', ARequest.Parsed.LayerName);
    Response.Add('color', ARequest.Parsed.IndexColor);
    Response.Add('operands', ARequest.Operands);
    Response.Add('closed', True);
    Response.Add('vertex_count', Length(ARequest.Points));
    Vertices := TJSONArray.Create;
    for P in ARequest.Points do begin
      Vertex := PointJSON(P.X, P.Y);
      Vertex.Add(P.Z);
      Vertices.Add(Vertex);
    end;
    Response.Add('points', Vertices);
    Response.Add('area', ARequest.Geometry.Area);
    Response.Add('perimeter', ARequest.Geometry.Perimeter);
    BBox := TJSONObject.Create;
    BBox.Add('min', PointJSON(ARequest.Geometry.MinX, ARequest.Geometry.MinY));
    BBox.Add('max', PointJSON(ARequest.Geometry.MaxX, ARequest.Geometry.MaxY));
    Response.Add('bbox', BBox);
    Response.Add('centroid', PointJSON(ARequest.Geometry.CentroidX,
      ARequest.Geometry.CentroidY));
    { Все переменные зоны: незаданные получают '0', как в SpaceAddPoly }
    Variables := TJSONObject.Create;
    for Param in ARequest.Parsed.Params do
      Variables.Add(Param.VarName, '0');
    for Value in ARequest.Values do
      Variables.Strings[Value.VarName] := Value.Value;
    Response.Add('variables', Variables);
    Response.Add('batch', ABatch);
    if ABatch then
      Response.Add('message', 'Zone queued, it will be added by END_BATCH')
    else
      Response.Add('message', 'Zone created');
    Result := Response.AsJSON;
  finally
    Response.Free;
  end;
end;

function SpaceErrorJSON(const ACommand, ACode, AMessage: string): string;
var
  Response, Error: TJSONObject;
begin
  Response := TJSONObject.Create;
  try
    Error := TJSONObject.Create;
    Error.Add('code', ACode);
    Error.Add('message', AMessage);
    Response.Add('ok', False);
    Response.Add('command', ACommand);
    Response.Add('error', Error);
    Result := Response.AsJSON;
  finally
    Response.Free;
  end;
end;

function SpaceErrorCodeOf(const AJSON: string): string;
var
  Data, Code: TJSONData;
begin
  Result := '';
  try
    Data := GetJSON(AJSON);
  except
    Exit;
  end;
  if Data = nil then
    Exit;
  try
    if Data.JSONType = jtObject then begin
      Code := TJSONObject(Data).FindPath('error.code');
      if (Code <> nil) and (Code.JSONType = jtString) then
        Result := Code.AsString;
    end;
  finally
    Data.Free;
  end;
end;

initialization
  SpaceFormat := DefaultFormatSettings;
  SpaceFormat.DecimalSeparator := '.';
  SpaceFormat.ThousandSeparator := #0;

end.
