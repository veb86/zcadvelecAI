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
  Модуль: uzctentproxy
  Назначение: Тесты загрузки кастомных (proxy) сущностей из DXF-файла.
  Проверяет, что неизвестные сущности (например, SPDSPOLYMORPHMARK) корректно
  загружаются как GDBObjAcdProxy.
  Автор: Vladimir Bobrov
  Дата: 2026-03-18
  Зависимости: fpcunit, testregistry, uzeffdxf, uzeentacdproxy,
               uzedrawingsimple, uzeffmanager, uzgldrawcontext, uzeconsts
}
unit uzctentproxy;
{$Codepage UTF8}
{$Mode delphi}{$H+}

interface

uses
  SysUtils,
  Math,
  Classes,
  fpcunit,
  testregistry,
  // Сущности для регистрации в фабрике
  uzeentacdproxy,
  uzeentline,
  uzeentproxygraphicparser,
  uzeentproxymanager,
  uzegeometry,
  uzegeometrytypes,
  UGDBPoint3DArray,
  // Основной загрузчик DXF
  uzeffdxf,
  // Инфраструктура чертежа
  uzedrawingsimple,
  uzeffmanager,
  uzgldrawcontext,
  uzeconsts,
  uzeentgenericsubentry,
  // Нужен для инициализации LCL-зависимых модулей
  Interfaces;

type
  { Тест загрузки кастомных proxy-сущностей из DXF }
  TProxyEntityLoadTest = class(TTestCase)
  published
    { Проверяет, что неизвестная сущность SPDSPOLYMORPHMARK загружается как proxy }
    procedure CustomEntityLoadedAsProxy;
    { Проверяет, что обычная известная сущность LINE загружается корректно }
    procedure KnownEntityLoadedNormally;
    { Проверяет поддержку OpCode=32 в proxy графике mleaderblock.dxf }
    procedure MLeaderBlockProxyContainsShelfDivider;
    { Проверяет сохранение слоя у кастомного proxy-объекта SPDSDOOR }
    procedure SpdsDoorProxyKeepsEntityLayer;
    { Проверяет, что контуры без SetLineweight остаются ByLayer }
    procedure ProxyGraphicDefaultLineweightIsByLayer;
    { Проверяет сохранение lineweight внутри proxy graphic SPDSDOOR }
    procedure SpdsDoorProxyGraphicKeepsPrimitiveLineweight;
    { Проверяет BBox и ручку proxy-объекта SPDSDOOR }
    procedure SpdsDoorProxyUsesGraphicBBoxForGripCenter;
    { Проверяет локальную СК proxy-объекта относительно ручки }
    procedure SpdsDoorProxySubEntitiesAreLocalToGripCenter;
    { Проверяет сохранение атрибутов отрисовки в логируемых контурах }
    procedure ProxyGraphicContourStoresDrawingAttributesForLog;
  end;

implementation

{ Минимальный DXF с одной кастомной сущностью SPDSPOLYMORPHMARK.
  Содержит: заголовок, секцию ENTITIES с одной кастомной сущностью
  с двумя точками (код 10 и 11) для формирования bounding box. }
const
  // Каждая строка в DXF — это код группы, затем значение
  DXF_CUSTOM_ENTITY_CONTENT =
    '  0'  + #13#10 + 'SECTION'                + #13#10 +
    '  2'  + #13#10 + 'HEADER'                 + #13#10 +
    '  9'  + #13#10 + '$ACADVER'               + #13#10 +
    '  1'  + #13#10 + 'AC1015'                 + #13#10 +
    '  0'  + #13#10 + 'ENDSEC'                 + #13#10 +
    '  0'  + #13#10 + 'SECTION'                + #13#10 +
    '  2'  + #13#10 + 'ENTITIES'               + #13#10 +
    '  0'  + #13#10 + 'SPDSPOLYMORPHMARK'      + #13#10 +
    '  5'  + #13#10 + '2C4'                    + #13#10 +
    '330'  + #13#10 + '1F'                     + #13#10 +
    '100'  + #13#10 + 'AcDbEntity'             + #13#10 +
    '  8'  + #13#10 + '0'                      + #13#10 +
    ' 10'  + #13#10 + '1.0'                    + #13#10 +
    ' 20'  + #13#10 + '2.0'                    + #13#10 +
    ' 30'  + #13#10 + '0.0'                    + #13#10 +
    ' 11'  + #13#10 + '10.0'                   + #13#10 +
    ' 21'  + #13#10 + '20.0'                   + #13#10 +
    ' 31'  + #13#10 + '0.0'                    + #13#10 +
    '  0'  + #13#10 + 'ENDSEC'                 + #13#10 +
    '  0'  + #13#10 + 'EOF'                    + #13#10;

  { Минимальный DXF с одной LINE-сущностью }
  DXF_LINE_ENTITY_CONTENT =
    '  0'  + #13#10 + 'SECTION'                + #13#10 +
    '  2'  + #13#10 + 'HEADER'                 + #13#10 +
    '  9'  + #13#10 + '$ACADVER'               + #13#10 +
    '  1'  + #13#10 + 'AC1015'                 + #13#10 +
    '  0'  + #13#10 + 'ENDSEC'                 + #13#10 +
    '  0'  + #13#10 + 'SECTION'                + #13#10 +
    '  2'  + #13#10 + 'ENTITIES'               + #13#10 +
    '  0'  + #13#10 + 'LINE'                   + #13#10 +
    '  5'  + #13#10 + '100'                    + #13#10 +
    '330'  + #13#10 + '1F'                     + #13#10 +
    '100'  + #13#10 + 'AcDbEntity'             + #13#10 +
    '  8'  + #13#10 + '0'                      + #13#10 +
    ' 10'  + #13#10 + '0.0'                    + #13#10 +
    ' 20'  + #13#10 + '0.0'                    + #13#10 +
    ' 30'  + #13#10 + '0.0'                    + #13#10 +
    ' 11'  + #13#10 + '5.0'                    + #13#10 +
    ' 21'  + #13#10 + '5.0'                    + #13#10 +
    ' 31'  + #13#10 + '0.0'                    + #13#10 +
    '  0'  + #13#10 + 'ENDSEC'                 + #13#10 +
    '  0'  + #13#10 + 'EOF'                    + #13#10;

{ Сохраняет строку во временный файл и возвращает имя файла }
function WriteTempDXF(const content: string): string;
var
  tmpFile: string;
  f: TextFile;
begin
  tmpFile := GetTempDir + 'test_proxy_' + IntToStr(Random(MaxInt)) + '.dxf';
  AssignFile(f, tmpFile);
  Rewrite(f);
  Write(f, content);
  CloseFile(f);
  Result := tmpFile;
end;

{ Загружает DXF-содержимое в чертёж и возвращает количество загруженных сущностей }
function LoadDXFContent(const content: string; var drawing: TSimpleDrawing): Integer;
var
  tmpFile: string;
  dc: TDrawContext;
  zdc: TZDrawingContext;
begin
  tmpFile := WriteTempDXF(content);
  try
    drawing.init(nil);
    dc := drawing.CreateDrawingRC;
    zdc.CreateRec(drawing, drawing.pObjRoot^, TLOLoad, dc);
    AddFromDXF(tmpFile, zdc);
    Result := drawing.pObjRoot^.ObjArray.Count;
  finally
    SysUtils.DeleteFile(tmpFile);
  end;
end;

function ExtractProxyGraphicHexFromDXF(const FileName, EntityName: string): string;
var
  Lines: TStringList;
  I: Integer;
  InsideEntity: Boolean;
  ExpectHexValue: Boolean;
begin
  Result := '';
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(FileName);
    InsideEntity := False;
    ExpectHexValue := False;
    I := 0;
    while I < Lines.Count do
    begin
      if not InsideEntity then
      begin
        if (Trim(Lines[I]) = '0') and (I + 1 < Lines.Count)
          and (Trim(Lines[I + 1]) = EntityName) then
        begin
          InsideEntity := True;
          Inc(I, 2);
          Continue;
        end;
      end
      else
      begin
        if ExpectHexValue then
        begin
          Result := Result + Trim(Lines[I]);
          ExpectHexValue := False;
        end
        else if Trim(Lines[I]) = '310' then
          ExpectHexValue := True
        else if (Trim(Lines[I]) = '100') and (I + 1 < Lines.Count)
          and (Trim(Lines[I + 1]) = 'AcDbMLeader') then
          Break;
      end;
      Inc(I);
    end;
  finally
    Lines.Free;
  end;
end;

function HexToBytes(const Hex: string): TBytes;
var
  I: Integer;
begin
  SetLength(Result, Length(Hex) div 2);
  for I := 0 to High(Result) do
    Result[I] := StrToInt('$' + Copy(Hex, I * 2 + 1, 2));
end;

function PrimitiveHasSegment(const Vertices: GDBPoint3DArray;
  const X1, Y1, X2, Y2: Double): Boolean;
var
  I: Integer;
  P1, P2: GDBvertex;
const
  EPS = 1e-6;
begin
  Result := False;
  if Vertices.Count < 2 then
    Exit;
  for I := 0 to Vertices.Count - 2 do
  begin
    P1 := Vertices.getDataMutable(I)^;
    P2 := Vertices.getDataMutable(I + 1)^;
    if SameValue(P1.x, X1, EPS) and SameValue(P1.y, Y1, EPS)
      and SameValue(P2.x, X2, EPS) and SameValue(P2.y, Y2, EPS) then
      Exit(True);
    if SameValue(P1.x, X2, EPS) and SameValue(P1.y, Y2, EPS)
      and SameValue(P2.x, X1, EPS) and SameValue(P2.y, Y1, EPS) then
      Exit(True);
  end;
end;

procedure AppendBytes(var Data: TBytes; const Count: Integer);
var
  OldLen: Integer;
begin
  OldLen := Length(Data);
  SetLength(Data, OldLen + Count);
end;

procedure AppendInt32(var Data: TBytes; const Value: Integer);
var
  OldLen: Integer;
begin
  OldLen := Length(Data);
  AppendBytes(Data, SizeOf(Value));
  Move(Value, Data[OldLen], SizeOf(Value));
end;

procedure AppendDouble(var Data: TBytes; const Value: Double);
var
  OldLen: Integer;
begin
  OldLen := Length(Data);
  AppendBytes(Data, SizeOf(Value));
  Move(Value, Data[OldLen], SizeOf(Value));
end;

procedure AppendVertex(var Data: TBytes; const X, Y, Z: Double);
begin
  AppendDouble(Data, X);
  AppendDouble(Data, Y);
  AppendDouble(Data, Z);
end;

procedure AppendCommand(var Data: TBytes; const OpCode: Integer;
  const Payload: TBytes);
begin
  AppendInt32(Data, Length(Payload) + 8);
  AppendInt32(Data, OpCode);
  if Length(Payload) > 0 then
  begin
    AppendBytes(Data, Length(Payload));
    Move(Payload[0], Data[Length(Data) - Length(Payload)], Length(Payload));
  end;
end;

function BuildProxyGraphicWithAttributedPolyline: TBytes;
var
  Body, Payload: TBytes;
begin
  SetLength(Body, 0);

  SetLength(Payload, 0);
  AppendInt32(Payload, 7);
  AppendCommand(Body, 14, Payload);

  SetLength(Payload, 0);
  AppendInt32(Payload, 5);
  AppendCommand(Body, 16, Payload);

  SetLength(Payload, 0);
  AppendInt32(Payload, 3);
  AppendCommand(Body, 18, Payload);

  SetLength(Payload, 0);
  AppendInt32(Payload, 1122867);
  AppendCommand(Body, 22, Payload);

  SetLength(Payload, 0);
  AppendInt32(Payload, 60);
  AppendCommand(Body, 23, Payload);

  SetLength(Payload, 0);
  AppendDouble(Payload, 2.5);
  AppendCommand(Body, 24, Payload);

  SetLength(Payload, 0);
  AppendDouble(Payload, 1.25);
  AppendCommand(Body, 25, Payload);

  SetLength(Payload, 0);
  AppendInt32(Payload, 2);
  AppendVertex(Payload, 1.0, 2.0, 0.0);
  AppendVertex(Payload, 3.0, 4.0, 0.0);
  AppendCommand(Body, 6, Payload);

  SetLength(Result, 0);
  AppendInt32(Result, Length(Body) + 8);
  AppendInt32(Result, 8);
  if Length(Body) > 0 then
  begin
    AppendBytes(Result, Length(Body));
    Move(Body[0], Result[Length(Result) - Length(Body)], Length(Body));
  end;
end;

procedure TProxyEntityLoadTest.ProxyGraphicContourStoresDrawingAttributesForLog;
var
  Parser: TProxyGraphicParser;
  ParseResult: TProxyGraphicParseResult;
begin
  Parser := TProxyGraphicParser.Create(BuildProxyGraphicWithAttributedPolyline);
  try
    ParseResult := Parser.Parse;
  finally
    Parser.Free;
  end;

  CheckEquals(1, Length(ParseResult.Primitives),
    'Тестовый proxy graphic должен содержать один примитив');
  CheckEquals(60, ParseResult.Primitives[0].LineWeight,
    'Примитив должен хранить вес линии для детального лога');
  CheckEquals(7, ParseResult.Primitives[0].Color,
    'Примитив должен хранить текущий цвет');
  CheckEquals(1122867, ParseResult.Primitives[0].TrueColor,
    'Примитив должен хранить текущий true color');
  CheckEquals('5', ParseResult.Primitives[0].Layer,
    'Примитив должен хранить индекс слоя из proxy graphic');
  CheckEquals('3', ParseResult.Primitives[0].Linetype,
    'Примитив должен хранить индекс типа линии из proxy graphic');
  CheckEquals(2.5, ParseResult.Primitives[0].LtScale, 1e-9,
    'Примитив должен хранить масштаб типа линии');
  CheckEquals(1.25, ParseResult.Primitives[0].Thickness, 1e-9,
    'Примитив должен хранить толщину');
end;

procedure TProxyEntityLoadTest.ProxyGraphicDefaultLineweightIsByLayer;
var
  HexData: string;
  Parser: TProxyGraphicParser;
  ParseResult: TProxyGraphicParseResult;
  I: Integer;
begin
  HexData := ExtractProxyGraphicHexFromDXF('cad_source/test/mleaderblock.dxf', 'MULTILEADER');
  CheckNotEquals('', HexData, 'Не удалось извлечь proxy graphic из mleaderblock.dxf');

  Parser := TProxyGraphicParser.Create(HexToBytes(HexData));
  try
    ParseResult := Parser.Parse;
  finally
    Parser.Free;
  end;

  Check(Length(ParseResult.Primitives) > 0,
    'Proxy graphic mleaderblock.dxf должен содержать примитивы');
  for I := 0 to High(ParseResult.Primitives) do
    CheckEquals(LnWtByLayer, ParseResult.Primitives[I].LineWeight,
      'Примитивы без явного SetLineweight должны сохранять LineWeight=ByLayer');
end;

{ Проверяет, что кастомная сущность SPDSPOLYMORPHMARK загружается как прокси-объект.
  До исправления: сущность игнорировалась, ObjArray.Count = 0.
  После исправления: сущность загружается как GDBObjAcdProxy, Count = 1. }
procedure TProxyEntityLoadTest.CustomEntityLoadedAsProxy;
var
  drawing: TSimpleDrawing;
  entityCount: Integer;
  entityTypeName: string;
begin
  entityCount := LoadDXFContent(DXF_CUSTOM_ENTITY_CONTENT, drawing);
  try
    // Проверяем, что сущность была загружена
    CheckEquals(1, entityCount,
      'Кастомная сущность SPDSPOLYMORPHMARK должна загружаться как proxy-объект');

    // Проверяем тип загруженной сущности
    if entityCount > 0 then begin
      entityTypeName := PGDBObjEntity(drawing.pObjRoot^.ObjArray.GetData(0))^.GetObjTypeName;
      CheckEquals(ObjN_GDBObjAcdProxy, entityTypeName,
        'Тип загруженной сущности должен быть GDBObjAcdProxy, получено: ' + entityTypeName);
    end;
  finally
    drawing.done;
  end;
end;

{ Проверяет, что стандартная сущность LINE загружается нормально,
  и изменения не нарушили существующую логику. }
procedure TProxyEntityLoadTest.KnownEntityLoadedNormally;
var
  drawing: TSimpleDrawing;
  entityCount: Integer;
begin
  entityCount := LoadDXFContent(DXF_LINE_ENTITY_CONTENT, drawing);
  try
    CheckEquals(1, entityCount,
      'Стандартная сущность LINE должна загружаться корректно');
  finally
    drawing.done;
  end;
end;

procedure TProxyEntityLoadTest.MLeaderBlockProxyContainsShelfDivider;
var
  HexData: string;
  Parser: TProxyGraphicParser;
  ParseResult: TProxyGraphicParseResult;
  I: Integer;
  FoundShelf: Boolean;
begin
  HexData := ExtractProxyGraphicHexFromDXF('cad_source/test/mleaderblock.dxf', 'MULTILEADER');
  CheckNotEquals('', HexData, 'Не удалось извлечь proxy graphic из mleaderblock.dxf');

  Parser := TProxyGraphicParser.Create(HexToBytes(HexData));
  try
    ParseResult := Parser.Parse;
  finally
    Parser.Free;
  end;

  FoundShelf := False;
  for I := 0 to High(ParseResult.Primitives) do
    if ParseResult.Primitives[I].HandlerResult.HasVertices
      and PrimitiveHasSegment(
        ParseResult.Primitives[I].HandlerResult.Vertices,
        1097.291017048187, 1381.615369802081,
        1113.291017048187, 1381.615369802081) then
    begin
      FoundShelf := True;
      Break;
    end;

  CheckTrue(FoundShelf,
    'Proxy graphic должен содержать полку-разделитель из mleaderblock.dxf');
end;

procedure TProxyEntityLoadTest.SpdsDoorProxyKeepsEntityLayer;
var
  drawing: TSimpleDrawing;
  dc: TDrawContext;
  zdc: TZDrawingContext;
  entity: PGDBObjEntity;
begin
  drawing.init(nil);
  try
    dc := drawing.CreateDrawingRC;
    zdc.CreateRec(drawing, drawing.pObjRoot^, TLOLoad, dc);
    AddFromDXF('cad_source/test/spdsdoor.dxf', zdc);

    CheckEquals(1, drawing.pObjRoot^.ObjArray.Count,
      'spdsdoor.dxf должен загружать один proxy-объект');

    entity := PGDBObjEntity(drawing.pObjRoot^.ObjArray.GetData(0));
    CheckEquals(ObjN_GDBObjAcdProxy, entity^.GetObjTypeName,
      'SPDSDOOR должен загружаться как proxy-объект');
    CheckNotNull(entity^.vp.Layer, 'У proxy-объекта должен быть назначен слой');
    CheckEquals('АР ДВЕРИ', entity^.vp.Layer^.Name,
      'Proxy-объект должен сохранять слой из DXF-сущности');
  finally
    drawing.done;
  end;
end;

procedure TProxyEntityLoadTest.SpdsDoorProxyGraphicKeepsPrimitiveLineweight;
var
  drawing: TSimpleDrawing;
  dc: TDrawContext;
  zdc: TZDrawingContext;
  entity: PGDBObjEntity;
  HexData: string;
  Parser: TProxyGraphicParser;
  ParseResult: TProxyGraphicParseResult;
  I: Integer;
  ExplicitLineweightCount: Integer;
begin
  drawing.init(nil);
  try
    dc := drawing.CreateDrawingRC;
    zdc.CreateRec(drawing, drawing.pObjRoot^, TLOLoad, dc);
    AddFromDXF('cad_source/test/spdsdoor.dxf', zdc);

    CheckEquals(1, drawing.pObjRoot^.ObjArray.Count,
      'spdsdoor.dxf должен загружать один proxy-объект');

    entity := PGDBObjEntity(drawing.pObjRoot^.ObjArray.GetData(0));
    CheckEquals(LnWtByLayer, entity^.vp.LineWeight,
      'Сама DXF-сущность SPDSDOOR остаётся ByLayer; явный вес хранится внутри proxy graphic');
  finally
    drawing.done;
  end;

  HexData := ExtractProxyGraphicHexFromDXF('cad_source/test/spdsdoor.dxf', 'SPDSDOOR');
  CheckNotEquals('', HexData, 'Не удалось извлечь proxy graphic из spdsdoor.dxf');

  Parser := TProxyGraphicParser.Create(HexToBytes(HexData));
  try
    ParseResult := Parser.Parse;
  finally
    Parser.Free;
  end;

  ExplicitLineweightCount := 0;
  for I := 0 to High(ParseResult.Primitives) do
    if ParseResult.Primitives[I].LineWeight = 60 then
      Inc(ExplicitLineweightCount);

  CheckEquals(4, Length(ParseResult.Primitives),
    'Proxy graphic SPDSDOOR должен содержать 4 примитива двери');
  CheckEquals(Length(ParseResult.Primitives), ExplicitLineweightCount,
    'Каждый примитив SPDSDOOR должен сохранять явный вес линии 0.60 мм (DXF 60)');
end;

procedure TProxyEntityLoadTest.SpdsDoorProxyUsesGraphicBBoxForGripCenter;
var
  drawing: TSimpleDrawing;
  dc: TDrawContext;
  zdc: TZDrawingContext;
  entity: PGDBObjEntity;
  Center: GDBvertex;
begin
  drawing.init(nil);
  try
    dc := drawing.CreateDrawingRC;
    zdc.CreateRec(drawing, drawing.pObjRoot^, TLOLoad, dc);
    AddFromDXF('cad_source/test/spdsdoor.dxf', zdc);

    CheckEquals(1, drawing.pObjRoot^.ObjArray.Count,
      'spdsdoor.dxf должен загружать один proxy-объект');

    entity := PGDBObjEntity(drawing.pObjRoot^.ObjArray.GetData(0));
    CheckEquals(ObjN_GDBObjAcdProxy, entity^.GetObjTypeName,
      'SPDSDOOR должен загружаться как proxy-объект');

    CheckEquals(1795.907681873614, entity^.vp.BoundingBox.LBN.x, 1e-6,
      'BBox proxy-объекта SPDSDOOR не должен захватывать X=0');
    CheckEquals(5921.182196811287, entity^.vp.BoundingBox.RTF.x, 1e-6,
      'BBox proxy-объекта SPDSDOOR должен брать максимум из proxy graphic');
    CheckEquals(858.0508144791978, entity^.vp.BoundingBox.LBN.y, 1e-6,
      'BBox proxy-объекта SPDSDOOR не должен захватывать Y=0');
    CheckEquals(1054.218138855069, entity^.vp.BoundingBox.RTF.y, 1e-6,
      'BBox proxy-объекта SPDSDOOR должен брать максимум из proxy graphic');

    Center := entity^.GetCenterPoint;
    CheckEquals(3858.54493934245, Center.x, 1e-6,
      'Ручка proxy-объекта SPDSDOOR должна быть в геометрическом центре BBox, а не в (0,0,0)');
    CheckEquals(956.1344766671334, Center.y, 1e-6,
      'Ручка proxy-объекта SPDSDOOR должна быть в геометрическом центре BBox, а не в (0,0,0)');
    CheckEquals(0.0, Center.z, 1e-9,
      'SPDSDOOR лежит в плоскости Z=0');
  finally
    drawing.done;
  end;
end;

procedure TProxyEntityLoadTest.SpdsDoorProxySubEntitiesAreLocalToGripCenter;
var
  drawing: TSimpleDrawing;
  dc: TDrawContext;
  zdc: TZDrawingContext;
  entity: PGDBObjEntity;
  proxy: PGDBObjAcdProxy;
  line: PGDBObjLine;
  center, delta: GDBvertex;
  moveMatrix: TzeTypedMatrix4d;
begin
  drawing.init(nil);
  try
    dc := drawing.CreateDrawingRC;
    zdc.CreateRec(drawing, drawing.pObjRoot^, TLOLoad, dc);
    AddFromDXF('cad_source/test/spdsdoor.dxf', zdc);

    CheckEquals(1, drawing.pObjRoot^.ObjArray.Count,
      'spdsdoor.dxf должен загружать один proxy-объект');

    entity := PGDBObjEntity(drawing.pObjRoot^.ObjArray.GetData(0));
    CheckEquals(ObjN_GDBObjAcdProxy, entity^.GetObjTypeName,
      'SPDSDOOR должен загружаться как proxy-объект');

    proxy := PGDBObjAcdProxy(entity);
    Check(proxy^.ConstObjArray.Count > 0,
      'Proxy graphic должен построить подпримитивы');

    line := PGDBObjLine(proxy^.ConstObjArray.GetData(0));
    center := proxy^.GetCenterPoint;

    CheckEquals(line^.CoordInOCS.lBegin.x + center.x,
      line^.CoordInWCS.lBegin.x, 1e-6,
      'Начало подпримитива должно храниться локально относительно grip center');
    CheckEquals(line^.CoordInOCS.lBegin.y + center.y,
      line^.CoordInWCS.lBegin.y, 1e-6,
      'Начало подпримитива должно храниться локально относительно grip center');

    delta := CreateVertex(100.0, 50.0, 0.0);
    moveMatrix := CreateTranslationMatrix(delta);
    entity^.TransformAt(entity, @moveMatrix);
    entity^.FormatEntity(drawing, dc);
    line := PGDBObjLine(proxy^.ConstObjArray.GetData(0));

    CheckEquals(center.x + delta.x, proxy^.GetCenterPoint.x, 1e-6,
      'После смещения grip center должен смещаться на вектор трансформации');
    CheckEquals(center.y + delta.y, proxy^.GetCenterPoint.y, 1e-6,
      'После смещения grip center должен смещаться на вектор трансформации');
    CheckEquals(line^.CoordInOCS.lBegin.x + proxy^.GetCenterPoint.x,
      line^.CoordInWCS.lBegin.x, 1e-6,
      'После смещения подпримитив должен оставаться локальным к grip center');
    CheckEquals(line^.CoordInOCS.lBegin.y + proxy^.GetCenterPoint.y,
      line^.CoordInWCS.lBegin.y, 1e-6,
      'После смещения подпримитив должен оставаться локальным к grip center');
  finally
    drawing.done;
  end;
end;

begin
  RegisterTests([TProxyEntityLoadTest]);
end.
