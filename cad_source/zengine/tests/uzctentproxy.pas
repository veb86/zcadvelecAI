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

function ContourHasSegment(const Contour: TProxyContour;
  const X1, Y1, X2, Y2: Double): Boolean;
var
  I: Integer;
  P1, P2: GDBvertex;
const
  EPS = 1e-6;
begin
  Result := False;
  for I := 0 to Contour.Vertices.Count - 2 do
  begin
    P1 := Contour.Vertices.getDataMutable(I)^;
    P2 := Contour.Vertices.getDataMutable(I + 1)^;
    if SameValue(P1.x, X1, EPS) and SameValue(P1.y, Y1, EPS)
      and SameValue(P2.x, X2, EPS) and SameValue(P2.y, Y2, EPS) then
      Exit(True);
    if SameValue(P1.x, X2, EPS) and SameValue(P1.y, Y2, EPS)
      and SameValue(P2.x, X1, EPS) and SameValue(P2.y, Y1, EPS) then
      Exit(True);
  end;
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
  for I := 0 to High(ParseResult.Contours) do
    if ContourHasSegment(ParseResult.Contours[I],
      1097.291017048187, 1381.615369802081,
      1113.291017048187, 1381.615369802081) then
    begin
      FoundShelf := True;
      Break;
    end;

  CheckTrue(FoundShelf,
    'Proxy graphic должен содержать полку-разделитель из mleaderblock.dxf');
end;

begin
  RegisterTests([TProxyEntityLoadTest]);
end.
