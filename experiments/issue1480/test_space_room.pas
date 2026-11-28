{
  Регрессия #1480: чистая логика HTTP-команд SPACE_ADD_ROOM / SPACE_ADD_POLY.
  Проверяет пресеты операндов (совпадают с кнопками панели «Пространства»),
  разбор операндов как в ParseOperandsToStruct, проверку контура,
  приведение значений переменных и контракты JSON-ответов.
  Исходник в UTF-8 без $codepage: литералы остаются байтами UTF-8, как в ZCAD.
  Запуск: fpc -Fu../../cad_source/zcad/velec/IPCserver test_space_room.pas
}
program test_space_room;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, Math, fpjson, jsonparser, uzvhttpspaceroom;

var
  Failures: Integer = 0;
  Checks: Integer = 0;

{ Фиксирует результат одной проверки }
procedure Check(ACondition: Boolean; const AName: string);
begin
  Inc(Checks);
  if ACondition then
    Exit;
  Inc(Failures);
  WriteLn('FAIL: ', AName);
end;

procedure CheckEq(const AActual, AExpected, AName: string);
begin
  Check(AActual = AExpected, Format('%s: expected "%s", got "%s"',
    [AName, AExpected, AActual]));
end;

// Разбирает args так же, как диспетчер POST /ipc
function ParseIPC(const ACommand, AArgsJSON: string): TSpaceRequest;
var
  Args: TJSONArray;
begin
  Args := TJSONArray(GetJSON(AArgsJSON));
  try
    Result := SpaceRequestParse(ACommand, Args);
  finally
    Args.Free;
  end;
end;

{ Возвращает код ошибки разбора или '' при успехе }
function ErrorOf(const ACommand, AArgsJSON: string): string;
begin
  Result := '';
  try
    ParseIPC(ACommand, AArgsJSON);
  except
    on E: ESpaceRoom do
      Result := E.Code;
  end;
end;

function ValueOf(const ARequest: TSpaceRequest; const AVarName: string): string;
var
  Value: TSpaceValue;
begin
  Result := '<none>';
  for Value in ARequest.Values do
    if Value.VarName = AVarName then
      Exit(Value.Value);
end;

function TypeOf(const ARequest: TSpaceRequest; const AVarName: string): string;
var
  Index: Integer;
begin
  Index := SpaceFindParam(ARequest.Parsed, AVarName);
  if Index < 0 then
    Exit('<none>');
  Result := ARequest.Parsed.Params[Index].TypeName;
end;

function Pts(const ACoords: array of Double): TSpacePoints;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(ACoords) div 2);
  for I := 0 to High(Result) do begin
    Result[I].X := ACoords[2 * I];
    Result[I].Y := ACoords[2 * I + 1];
    Result[I].Z := 0;
  end;
end;

const
  SQUARE = '[[0,0],[4000,0],[4000,3000],[0,3000]]';

procedure TestPresets;
var
  Ops: TSpaceOperands;
  Text: string;
begin
  { Пресет помещения — строка кнопки панели без изменений }
  Ops := SpaceParseOperands(SPACE_OPERANDS_ROOM);
  Check(Ops.IndexColor = 5, 'room preset color 5');
  CheckEq(Ops.LayerName, 'spaceRoom', 'room preset layer');
  Check(Length(Ops.Params) = 5, 'room preset has 5 variables');
  CheckEq(Ops.Params[0].VarName, 'Space_RoomPos', 'room var 0');
  CheckEq(Ops.Params[0].UserName, 'RoomPos', 'room user 0');
  CheckEq(Ops.Params[0].TypeName, 'String', 'room type 0');
  CheckEq(Ops.Params[4].VarName, 'Space_RoomIllumination', 'room var 4');
  CheckEq(Ops.Params[4].UserName, 'RoomIllumination Лк', 'room user 4 (cyrillic)');
  CheckEq(Ops.Params[4].TypeName, 'Integer', 'room type 4');

  Ops := SpaceParseOperands(SPACE_OPERANDS_FLOOR);
  Check((Ops.IndexColor = 4) and (Ops.LayerName = 'spaceFloor') and
    (Length(Ops.Params) = 3), 'floor preset');
  CheckEq(Ops.Params[1].VarName, 'space_FloorHeight', 'floor height var');
  CheckEq(Ops.Params[2].TypeName, 'Double', 'floor scale type');

  Ops := SpaceParseOperands(SPACE_OPERANDS_SECTION);
  Check((Ops.IndexColor = 6) and (Ops.LayerName = 'spaceSection') and
    (Length(Ops.Params) = 1) and (Ops.Params[0].VarName = 'space_Section'),
    'section preset');
  Ops := SpaceParseOperands(SPACE_OPERANDS_BUILDING);
  Check((Ops.IndexColor = 3) and (Ops.LayerName = 'spaceBuild') and
    (Length(Ops.Params) = 1) and (Ops.Params[0].VarName = 'space_Building'),
    'building preset');

  Check(SpaceKindOperands('ROOM', Text) and (Text = SPACE_OPERANDS_ROOM),
    'kind is case-insensitive');
  Check(not SpaceKindOperands('garage', Text), 'unknown kind');
  CheckEq(SpaceNumberVar(SPACE_KIND_ROOM), 'Space_RoomPos', 'room number var');
  CheckEq(SpaceNumberVar(SPACE_KIND_FLOOR), 'space_Floor', 'floor number var');
  CheckEq(SpaceNumberVar(SPACE_KIND_CUSTOM), '', 'custom has no number var');

  { Поведение ParseOperandsToStruct: цвет вне 0..256 и мусор -> ByLayer }
  Check(SpaceParseOperands('').IndexColor = 256, 'empty operands -> ByLayer');
  Check(SpaceParseOperands('999,''L''').IndexColor = 256, 'color 999 -> ByLayer');
  Check(SpaceParseOperands('abc,''L''').IndexColor = 256, 'bad color -> ByLayer');
  Check(Length(SpaceParseOperands('1,''L'',A,''a''').Params) = 0,
    'incomplete triplet ignored');

  { Сборка операндов обратима для ParseOperandsToStruct }
  Ops := SpaceParseOperands(SPACE_OPERANDS_ROOM);
  Text := SpaceBuildOperands(Ops);
  Ops := SpaceParseOperands(Text);
  Check((Ops.IndexColor = 5) and (Ops.LayerName = 'spaceRoom') and
    (Length(Ops.Params) = 5) and (Ops.Params[4].UserName = 'RoomIllumination Лк'),
    'build/parse round trip');
end;

const
  { Кнопки панели «Пространства»: источник операндов SpaceAddPoly }
  TOOLBAR_XML = 'environment/runtimefiles/AllCPU-AllOS/zcadelectrotech/' +
    'data/preload/vspace/vspaceactionscontent.xml';
  TOOLBAR_PREFIX = 'Command="spaceaddpoly(';
  TOOLBAR_SUFFIX = ')"';

{ Операнды кнопки spaceaddpoly(...) с заданным слоем из XML панели }
function ToolbarOperands(AXml: TStrings; const ALayer: string): string;
var
  Line: string;
  Start, Stop: Integer;
begin
  Result := '';
  for Line in AXml do begin
    Start := Pos(TOOLBAR_PREFIX, Line);
    if (Start = 0) or (Pos(QuotedStr(ALayer), Line) = 0) then
      Continue;
    Start := Start + Length(TOOLBAR_PREFIX);
    Stop := Pos(TOOLBAR_SUFFIX, Copy(Line, Start, MaxInt));
    if Stop > 0 then
      Exit(Copy(Line, Start, Stop - 1));
  end;
end;

{ Пресеты видов зон совпадают с кнопками панели (файл читается из репозитория) }
procedure TestPresetsMatchToolbar;
var
  Xml: TStringList;
  FileName: string;
begin
  FileName := TOOLBAR_XML;
  if not FileExists(FileName) then
    FileName := '../../' + TOOLBAR_XML;
  Check(FileExists(FileName), 'toolbar xml found: ' + TOOLBAR_XML);
  if not FileExists(FileName) then
    Exit;
  Xml := TStringList.Create;
  try
    Xml.LoadFromFile(FileName);
    CheckEq(ToolbarOperands(Xml, 'spaceRoom'), SPACE_OPERANDS_ROOM, 'toolbar room');
    CheckEq(ToolbarOperands(Xml, 'spaceFloor'), SPACE_OPERANDS_FLOOR, 'toolbar floor');
    CheckEq(ToolbarOperands(Xml, 'spaceSection'), SPACE_OPERANDS_SECTION,
      'toolbar section');
    CheckEq(ToolbarOperands(Xml, 'spaceBuild'), SPACE_OPERANDS_BUILDING,
      'toolbar building');
  finally
    Xml.Free;
  end;
end;

procedure TestGeometry;
var
  P: TSpacePoints;
  G: TSpaceGeometry;
begin
  P := SpaceNormalizePoints(Pts([0, 0, 0, 0, 10, 0, 10, 10, 0, 10, 0, 0]));
  Check(Length(P) = 4, 'duplicates and closing vertex removed');

  G := SpaceGeometryOf(Pts([0, 0, 4000, 0, 4000, 3000, 0, 3000]));
  Check(SameValue(G.Area, 12000000), 'rectangle area');
  Check(SameValue(G.Perimeter, 14000), 'rectangle perimeter');
  Check(SameValue(G.CentroidX, 2000) and SameValue(G.CentroidY, 1500),
    'rectangle centroid');
  Check((G.MinX = 0) and (G.MaxX = 4000) and (G.MinY = 0) and (G.MaxY = 3000),
    'rectangle bbox');

  { Обход по часовой стрелке даёт ту же площадь и центр }
  G := SpaceGeometryOf(Pts([0, 3000, 4000, 3000, 4000, 0, 0, 0]));
  Check(SameValue(G.Area, 12000000) and SameValue(G.CentroidX, 2000),
    'clockwise contour');

  { Г-образное помещение }
  G := SpaceGeometryOf(Pts([0, 0, 6, 0, 6, 2, 2, 2, 2, 6, 0, 6]));
  Check(SameValue(G.Area, 20), 'L-shaped area');

  Check(SpaceIsSelfIntersecting(Pts([0, 0, 10, 10, 10, 0, 0, 10])), 'bow-tie');
  Check(not SpaceIsSelfIntersecting(Pts([0, 0, 6, 0, 6, 2, 2, 2, 2, 6, 0, 6])),
    'L-shape is simple');
  Check(SpaceIsSelfIntersecting(Pts([0, 0, 10, 0, 10, 10, 5, 0, 0, 10])),
    'vertex touching an edge');
  Check(not SpaceIsSelfIntersecting(Pts([0, 0, 1, 0, 1, 1])), 'triangle');

  CheckEq(SpaceFloatToStr(2.5), '2.5', 'float uses dot');
end;

procedure TestRoomRequest;
var
  R: TSpaceRequest;
begin
  R := ParseIPC(SPACE_IPC_ADD_ROOM, '[{"points":' + SQUARE +
    ',"number":"101","name":"Кабинет","category":"В4","illumination":300}]');
  CheckEq(R.Command, SPACE_COMMAND_ADD_ROOM, 'room command');
  CheckEq(R.Kind, SPACE_KIND_ROOM, 'room kind');
  Check(R.Parsed.IndexColor = 5, 'room color from preset');
  CheckEq(R.Parsed.LayerName, 'spaceRoom', 'room layer from preset');
  Check(Length(R.Points) = 4, 'room has 4 vertices');
  CheckEq(ValueOf(R, 'Space_RoomPos'), '101', 'room number');
  CheckEq(ValueOf(R, 'Space_RoomName'), 'Кабинет', 'room name (cyrillic)');
  CheckEq(ValueOf(R, 'Space_RoomCategory'), 'В4', 'room category');
  CheckEq(ValueOf(R, 'Space_RoomIllumination'), '300', 'room illumination');
  CheckEq(ValueOf(R, 'Space_RoomArea'), '<none>', 'area stays default');
  Check(SameValue(R.Geometry.Area, 12000000), 'room geometry area');
  Check(not R.Select, 'select defaults to false');
  Check(Length(R.Parsed.Params) = 5, 'no extra variables');

  { Номер-число превращается в строку, values и сокращения совместимы }
  R := ParseIPC(SPACE_IPC_ADD_ROOM, '[{"points":' + SQUARE +
    ',"number":7,"values":{"Space_RoomName":"Склад","space_RoomArea":"12.0",' +
    '"Note":"ok","Count":3,"Ratio":0.5,"Flag":true},"select":true}]');
  CheckEq(ValueOf(R, 'Space_RoomPos'), '7', 'numeric number -> string');
  CheckEq(ValueOf(R, 'Space_RoomName'), 'Склад', 'values field');
  CheckEq(ValueOf(R, 'Space_RoomArea'), '12.0', 'values match preset case-insensitively');
  CheckEq(TypeOf(R, 'Note'), 'String', 'inferred String');
  CheckEq(TypeOf(R, 'Count'), 'Integer', 'inferred Integer');
  CheckEq(TypeOf(R, 'Ratio'), 'Double', 'inferred Double');
  CheckEq(TypeOf(R, 'Flag'), 'Boolean', 'inferred Boolean');
  CheckEq(ValueOf(R, 'Flag'), 'True', 'boolean value');
  CheckEq(ValueOf(R, 'Ratio'), '0.5', 'double value');
  Check(Length(R.Parsed.Params) = 9, 'extra variables declared');
  Check(R.Select, 'select true');
  Check(Pos(',Note,''Note'',String', R.Operands) > 0, 'extra var in operands');

  { Сокращение важнее values }
  R := ParseIPC(SPACE_IPC_ADD_ROOM, '[{"points":' + SQUARE +
    ',"values":{"Space_RoomPos":"1"},"number":"2"}]');
  CheckEq(ValueOf(R, 'Space_RoomPos'), '2', 'shortcut overrides values');

  { Номер можно передать только через values }
  R := ParseIPC(SPACE_IPC_ADD_ROOM, '[{"points":' + SQUARE +
    ',"values":{"Space_RoomPos":"A-12"}}]');
  CheckEq(ValueOf(R, 'Space_RoomPos'), 'A-12', 'number via values');

  { Аргумент строкой с JSON (клиенты, которые шлют только строки) }
  R := ParseIPC(SPACE_IPC_ADD_ROOM,
    '["{\"points\":[[0,0],[1,0],[1,1]],\"number\":\"5\"}"]');
  CheckEq(ValueOf(R, 'Space_RoomPos'), '5', 'args[0] as JSON string');

  { Объектные вершины и Z }
  R := ParseIPC(SPACE_IPC_ADD_ROOM, '[{"points":[{"x":0,"y":0,"z":3000},' +
    '{"x":10,"y":0,"z":3000},{"x":10,"y":10,"z":3000}],"number":"1"}]');
  Check((Length(R.Points) = 3) and (R.Points[2].Z = 3000), 'object points with z');

  { Слой и цвет переопределяются }
  R := ParseIPC(SPACE_IPC_ADD_ROOM, '[{"points":' + SQUARE +
    ',"number":"1","layer":"Помещения","color":1}]');
  CheckEq(R.Parsed.LayerName, 'Помещения', 'layer override');
  Check(R.Parsed.IndexColor = 1, 'color override');
  CheckEq(SpaceParseOperands(R.Operands).LayerName, 'Помещения',
    'operands carry layer override');
end;

procedure TestPolyRequest;
var
  R: TSpaceRequest;
begin
  R := ParseIPC(SPACE_IPC_ADD_POLY, '[{"kind":"floor","points":' + SQUARE +
    ',"number":"2","height":"3,3","scale":100}]');
  CheckEq(R.Command, SPACE_COMMAND_ADD_POLY, 'poly command');
  CheckEq(R.Parsed.LayerName, 'spaceFloor', 'floor layer');
  CheckEq(ValueOf(R, 'space_Floor'), '2', 'floor number');
  CheckEq(ValueOf(R, 'space_FloorHeight'), '3.3', 'comma decimal accepted');
  CheckEq(ValueOf(R, 'space_FloorScale'), '100', 'integer as double');

  R := ParseIPC(SPACE_IPC_ADD_POLY, '[{"kind":"Building","points":' + SQUARE +
    ',"number":"Корпус 1"}]');
  CheckEq(R.Kind, SPACE_KIND_BUILDING, 'kind lowercased');
  CheckEq(ValueOf(R, 'space_Building'), 'Корпус 1', 'building number');

  R := ParseIPC(SPACE_IPC_ADD_POLY, '[{"kind":"section","points":' + SQUARE + '}]');
  CheckEq(R.Parsed.LayerName, 'spaceSection', 'section without values');
  Check(Length(R.Values) = 0, 'section has no values');

  R := ParseIPC(SPACE_IPC_ADD_POLY, '[{"points":' + SQUARE + ',"number":"3"}]');
  CheckEq(R.Kind, SPACE_KIND_ROOM, 'poly defaults to room');

  { Операнды как у кнопки панели }
  R := ParseIPC(SPACE_IPC_ADD_POLY, '[{"operands":"2,''zone'',Zone_Id, ''Id'', ' +
    'Integer","points":' + SQUARE + ',"values":{"Zone_Id":"42"}}]');
  CheckEq(R.Kind, SPACE_KIND_CUSTOM, 'operands -> custom');
  CheckEq(R.Parsed.LayerName, 'zone', 'custom layer');
  Check(R.Parsed.IndexColor = 2, 'custom color');
  CheckEq(ValueOf(R, 'Zone_Id'), '42', 'custom integer value');

  { Пресет можно передать операндами: результат как у вида room }
  R := ParseIPC(SPACE_IPC_ADD_POLY, '[{"operands":' +
    TJSONString.Create(SPACE_OPERANDS_ROOM).AsJSON + ',"points":' + SQUARE + '}]');
  Check((R.Parsed.IndexColor = 5) and (Length(R.Parsed.Params) = 5),
    'toolbar operands accepted verbatim');
end;

procedure TestErrors;
begin
  CheckEq(ErrorOf(SPACE_IPC_ADD_ROOM, '[]'), SPACE_ERR_INVALID_REQUEST, 'no args');
  CheckEq(ErrorOf(SPACE_IPC_ADD_ROOM, '[1,2]'), SPACE_ERR_INVALID_REQUEST, 'positional');
  CheckEq(ErrorOf(SPACE_IPC_ADD_ROOM, '["{bad"]'), SPACE_ERR_INVALID_REQUEST, 'bad json');
  CheckEq(ErrorOf(SPACE_IPC_ADD_ROOM, '[{"points":' + SQUARE + '}]'),
    SPACE_ERR_MISSING_NUMBER, 'room without number');
  CheckEq(ErrorOf(SPACE_IPC_ADD_ROOM, '[{"points":' + SQUARE + ',"number":"  "}]'),
    SPACE_ERR_MISSING_NUMBER, 'blank number');
  CheckEq(ErrorOf(SPACE_IPC_ADD_ROOM, '[{"points":' + SQUARE +
    ',"number":"1","kind":"floor"}]'), SPACE_ERR_INVALID_KIND, 'room with other kind');
  CheckEq(ErrorOf(SPACE_IPC_ADD_ROOM, '[{"points":' + SQUARE +
    ',"number":"1","nubmer":"1"}]'), SPACE_ERR_INVALID_REQUEST, 'unknown field');
  CheckEq(ErrorOf(SPACE_IPC_ADD_POLY, '[{"kind":"garage","points":' + SQUARE + '}]'),
    SPACE_ERR_INVALID_KIND, 'unknown kind');
  CheckEq(ErrorOf(SPACE_IPC_ADD_POLY, '[{"kind":"custom","points":' + SQUARE + '}]'),
    SPACE_ERR_INVALID_OPERANDS, 'custom without operands');
  CheckEq(ErrorOf(SPACE_IPC_ADD_POLY, '[{"kind":"floor","points":' + SQUARE +
    ',"name":"x"}]'), SPACE_ERR_INVALID_REQUEST, 'shortcut of another kind');

  { Вершины }
  CheckEq(ErrorOf(SPACE_IPC_ADD_ROOM, '[{"number":"1"}]'),
    SPACE_ERR_INVALID_POINTS, 'no points');
  CheckEq(ErrorOf(SPACE_IPC_ADD_ROOM, '[{"number":"1","points":[[0,0],[1,1]]}]'),
    SPACE_ERR_INVALID_POINTS, 'two points');
  CheckEq(ErrorOf(SPACE_IPC_ADD_ROOM,
    '[{"number":"1","points":[[0,0],[1,0],[1,0],[0,0]]}]'),
    SPACE_ERR_INVALID_POINTS, 'duplicates leave two points');
  CheckEq(ErrorOf(SPACE_IPC_ADD_ROOM,
    '[{"number":"1","points":[[0,0],[1,1],[2,2]]}]'),
    SPACE_ERR_INVALID_POINTS, 'collinear points');
  CheckEq(ErrorOf(SPACE_IPC_ADD_ROOM,
    '[{"number":"1","points":[[0,0],["1",0],[1,1]]}]'),
    SPACE_ERR_INVALID_POINTS, 'string coordinate');
  CheckEq(ErrorOf(SPACE_IPC_ADD_ROOM,
    '[{"number":"1","points":[[0,0],[1],[1,1]]}]'),
    SPACE_ERR_INVALID_POINTS, 'one coordinate');
  CheckEq(ErrorOf(SPACE_IPC_ADD_ROOM,
    '[{"number":"1","points":[[0,0],[10,10],[10,0],[0,10]]}]'),
    SPACE_ERR_SELF_INTERSECTING, 'bow-tie rejected');

  { Значения }
  CheckEq(ErrorOf(SPACE_IPC_ADD_ROOM, '[{"points":' + SQUARE +
    ',"number":"1","illumination":"много"}]'), SPACE_ERR_INVALID_VALUE, 'bad integer');
  CheckEq(ErrorOf(SPACE_IPC_ADD_ROOM, '[{"points":' + SQUARE +
    ',"number":"1","illumination":2.5}]'), SPACE_ERR_INVALID_VALUE, 'fractional integer');
  CheckEq(ErrorOf(SPACE_IPC_ADD_ROOM, '[{"points":' + SQUARE +
    ',"number":"1","illumination":9999999999}]'), SPACE_ERR_INVALID_VALUE,
    'integer overflow');
  CheckEq(ErrorOf(SPACE_IPC_ADD_ROOM, '[{"points":' + SQUARE +
    ',"number":["1"]}]'), SPACE_ERR_INVALID_VALUE, 'array value');
  CheckEq(ErrorOf(SPACE_IPC_ADD_ROOM, '[{"points":' + SQUARE +
    ',"number":"1\n2"}]'), SPACE_ERR_INVALID_VALUE, 'multiline value');
  CheckEq(ErrorOf(SPACE_IPC_ADD_ROOM, '[{"points":' + SQUARE +
    ',"number":"1","values":{"Имя":"x"}}]'), SPACE_ERR_INVALID_VALUE,
    'non-latin variable name');
  CheckEq(ErrorOf(SPACE_IPC_ADD_POLY, '[{"kind":"floor","points":' + SQUARE +
    ',"height":"высокий"}]'), SPACE_ERR_INVALID_VALUE, 'bad double');

  { Слой, цвет и операнды }
  CheckEq(ErrorOf(SPACE_IPC_ADD_ROOM, '[{"points":' + SQUARE +
    ',"number":"1","color":300}]'), SPACE_ERR_INVALID_REQUEST, 'color out of range');
  CheckEq(ErrorOf(SPACE_IPC_ADD_ROOM, '[{"points":' + SQUARE +
    ',"number":"1","color":"red"}]'), SPACE_ERR_INVALID_REQUEST, 'color not number');
  CheckEq(ErrorOf(SPACE_IPC_ADD_ROOM, '[{"points":' + SQUARE +
    ',"number":"1","layer":"a,b"}]'), SPACE_ERR_INVALID_OPERANDS, 'comma in layer');
  CheckEq(ErrorOf(SPACE_IPC_ADD_POLY, '[{"operands":"1,''L'',X,''x'',Matrix",' +
    '"points":' + SQUARE + '}]'), SPACE_ERR_INVALID_OPERANDS, 'unsupported type');
  CheckEq(ErrorOf(SPACE_IPC_ADD_POLY, '[{"operands":"1,''L'',X,''x'',String,' +
    'x,''y'',String","points":' + SQUARE + '}]'), SPACE_ERR_INVALID_OPERANDS,
    'duplicate variable');
  CheckEq(ErrorOf(SPACE_IPC_ADD_POLY, '[{"operands":"  ","points":' + SQUARE + '}]'),
    SPACE_ERR_INVALID_OPERANDS, 'blank operands');
  CheckEq(ErrorOf(SPACE_IPC_ADD_ROOM, '[{"points":' + SQUARE +
    ',"number":"1","select":"yes"}]'), SPACE_ERR_INVALID_REQUEST, 'select not boolean');
end;

procedure TestJSON;
var
  R: TSpaceRequest;
  Data: TJSONData;
  Obj: TJSONObject;
begin
  R := ParseIPC(SPACE_IPC_ADD_ROOM, '[{"points":' + SQUARE +
    ',"number":"101","name":"Кабинет"}]');
  Data := GetJSON(SpaceSuccessJSON(R, False));
  try
    Obj := TJSONObject(Data);
    Check(Obj.Booleans['ok'], 'json ok');
    CheckEq(Obj.Strings['command'], 'space_add_room', 'json command');
    CheckEq(Obj.Strings['kind'], 'room', 'json kind');
    CheckEq(Obj.Strings['layer'], 'spaceRoom', 'json layer');
    Check(Obj.Integers['color'] = 5, 'json color');
    Check(Obj.Integers['vertex_count'] = 4, 'json vertex_count');
    Check(SameValue(Obj.Floats['area'], 12000000), 'json area');
    Check(SameValue(Obj.Floats['perimeter'], 14000), 'json perimeter');
    CheckEq(Obj.Objects['variables'].Strings['Space_RoomPos'], '101', 'json number');
    CheckEq(Obj.Objects['variables'].Strings['Space_RoomName'], 'Кабинет', 'json name');
    CheckEq(Obj.Objects['variables'].Strings['Space_RoomArea'], '0',
      'json default value as SpaceAddPoly');
    Check(SameValue(Obj.Arrays['centroid'].Floats[0], 2000), 'json centroid');
    Check(SameValue(Obj.Objects['bbox'].Arrays['max'].Floats[1], 3000), 'json bbox');
    Check(Obj.Arrays['points'].Arrays[1].Count = 3, 'json point has z');
    Check(not Obj.Booleans['batch'], 'json batch false');
  finally
    Data.Free;
  end;

  Data := GetJSON(SpaceSuccessJSON(R, True));
  try
    Check(TJSONObject(Data).Booleans['batch'], 'json batch true');
  finally
    Data.Free;
  end;

  CheckEq(SpaceErrorCodeOf(SpaceErrorJSON('space_add_room', 'NO_DOCUMENT', 'x')),
    'NO_DOCUMENT', 'error json code');
  CheckEq(SpaceErrorCodeOf(SpaceSuccessJSON(R, False)), '', 'success has no code');
  CheckEq(SpaceErrorCodeOf('not json'), '', 'garbage has no code');
end;

begin
  { ZCAD работает с UTF-8 строками RTL (LazUTF8); повторяем это в тесте }
  SetMultiByteConversionCodePage(CP_UTF8);
  DefaultSystemCodePage := CP_UTF8;
  TestPresets;
  TestPresetsMatchToolbar;
  TestGeometry;
  TestRoomRequest;
  TestPolyRequest;
  TestErrors;
  TestJSON;
  WriteLn(Format('%d checks, %d failures', [Checks, Failures]));
  if Failures > 0 then
    Halt(1);
end.
