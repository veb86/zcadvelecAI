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
  // Базовые типы и сущности
  uzetypes,
  uzeentity,
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
  // Модули для тестов конвертации proxy -> block
  UGDBObjBlockdefArray,
  uzeblockdef,
  // Для тестов расчленения (issue #988)
  uzeentcircle,
  uzeenttext,
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
    { Проверяет, что OpCode=38 UnicodeText2 сохраняет TypeFace, FontName
      и BigFont в HandlerResult.TextItem. }
    procedure ProxyGraphicUnicodeText2StoresFontFields;
    { Проверяет, что вращение proxy-объекта меняет положение подпримитивов
      согласно повороту, а grip center остаётся в той же точке. }
    procedure SpdsDoorProxyRotationTransformsSubEntities;
    { Проверяет, что однородное масштабирование proxy-объекта корректно
      масштабирует подпримитивы и их bbox расширяется в то же число раз. }
    procedure SpdsDoorProxyUniformScaleTransformsSubEntities;
    { Проверяет, что GenerateUniqueProxyBlockName возвращает имя с
      префиксом "PE" и числом в диапазоне [0..1_000_000_000]. }
    procedure GeneratedProxyBlockNameHasPrefixAndRange;
    { Проверяет, что несколько вызовов GenerateUniqueProxyBlockName
      возвращают имена, отсутствующие в BlockDefArray. }
    procedure GeneratedProxyBlockNamesAreUniqueInArray;
    { Проверяет, что EnsureConvertedBlockDef создаёт блок в
      BlockDefArray и заполняет его подпримитивами proxy-объекта. }
    procedure EnsureConvertedBlockDefCreatesBlockWithSubEntities;
    { Проверяет, что повторный вызов EnsureConvertedBlockDef не
      создаёт новых блоков — возвращает ранее сгенерированное имя. }
    procedure EnsureConvertedBlockDefIsIdempotent;
    { Проверяет, что ConvertProxyEntitiesToBlocks обходит дерево и
      для каждого ProxyEntity добавляет блок в BlockDefArray. }
    procedure ConvertProxyEntitiesToBlocksAddsBlocksForAllProxies;
    { Проверяет, что ConvertProxyEntitiesToBlocks конвертирует
      proxy-сущности, лежащие внутри определения блока
      (proxyinblock.dxf): добавляет PE<N>-блок на каждую такую
      proxy-сущность и заполняет FConvertedBlockName. }
    procedure ConvertProxyEntitiesToBlocksHandlesProxiesInsideBlockDef;
    { Проверяет, что ProxyEntity читает DXF group code 48 (linetype
      scale) и переносит его на подпримитивы. В spdsconstructionline.dxf
      SPDSCONSTRUCTIONLINE имеет "48 100.0", а парсер до исправления
      записывал 1.0. }
    procedure SpdsConstructionLineProxyKeepsLineTypeScale;
    { Проверяет, что ProxyEntity корректно распространяет DXF code 48
      на все подпримитивы для нескольких SPDSCONSTRUCTIONLINE в одном
      файле (spdsconstructionline2.dxf: scale 77 и 100). После
      FormatEntity каждый подпримитив должен наследовать LineTypeScale
      своего владельца. }
    procedure SpdsConstructionLine2ProxyPropagatesScaleToAllSubEntities;
    { Регрессия на issue #971: в файлах DXF 2000/2004 «широкие» строки
      внутри бинарной Proxy Graphic хранятся в однобайтовой ANSI-кодировке,
      а не в UTF-16. Парсер должен читать их корректно, если поток создан
      в режиме UnicodeText=False. }
    procedure ProxyGraphicAnsiUnicodeText2ParsesInDxf2000Mode;
    { Регрессия на issue #971: DXF 2000 использует OpCode=11 (pgcText2)
      для текста внутри Proxy Graphic. Раньше он не был зарегистрирован
      и пропускался, из-за чего текст терялся. После исправления он
      должен читаться как ANSI-текст с шрифтом (без TypeFace). }
    procedure ProxyGraphicText2ParsesDxf2000Format;
    { Регрессия на issue #971: загрузка spdsconstructionline2000.dxf должна
      давать тот же BBox, что и загрузка spdsconstructionline2007.dxf.
      Это доказывает, что после исправления текст в прокси-графике читается
      корректно в обоих форматах. }
    procedure SpdsConstructionLine2000AndDxf2007ProduceSameBBox;
    { Регрессия на issue #973: подпримитивы внутри ProxyEntity должны
      повторять цветовое поведение примитивов внутри BlockInsert:
      ByLayer (DXF=256 или PROXY_DEFAULT=-1) — цвет из слоя,
      ByBlock (0) — цвет владельца (прокси-объекта),
      явный индекс 1..255 — используется как есть. До исправления все
      подпримитивы всегда наследовали цвет прокси-объекта. }
    procedure ProxyGraphicByLayerPrimitivesResolveToByLayer;
    procedure ProxyGraphicByBlockPrimitivesInheritOwnerColor;
    procedure ProxyGraphicExplicitPrimitiveColorIsPreserved;
    { Интеграционный тест: в spdsconstructionlineCOLOR.dxf два кастомных
      объекта SPDSCONSTRUCTIONLINE с цветами 3 (зелёный) и 1 (красный),
      внутри которых все примитивы имеют цвет ByLayer. После загрузки
      подпримитивы каждого прокси должны получить ClByLayer, а не цвет
      самого прокси. }
    procedure SpdsConstructionLineColorSubEntitiesUseByLayer;
    { Регрессия на issue #988: при расчленении ProxyEntity клонированный
      GDBObjCircle после transform(proxy.objMatrix) должен сохранять
      ненулевой радиус и позицию центра. До исправления GDBObjWithLocalCS.
      transform вызывал ReCalcFromObjMatrix на мусорной objMatrix (Clone
      использует GetMem+init), что обнуляло Radius и искажало P_insert. }
    procedure ProxyExplodeCircleTransformPreservesPositionAndRadius;
    { Регрессия на issue #988: при расчленении ProxyEntity клонированный
      GDBObjText после transform(proxy.objMatrix) должен сохранять корректную
      точку вставки. До исправления GDBObjAbstractText.transform вызывал
      inherited (GDBObjWithLocalCS.transform → ReCalcFromObjMatrix на
      OneMatrix), что обнуляло Local.P_insert (сдвигало текст в начало
      координат). }
    procedure ProxyExplodeTextTransformPreservesInsertPoint;
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
  P1, P2: TzePoint3d;
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

{ Кодирует строку как UTF-16LE, завершает двумя нулевыми байтами и
  выравнивает позицию данных до границы 4 байт. Формат совместим с
  TProxyByteStream.ReadPaddedUnicodeString и соответствует тому, как
  AutoCAD пишет строки внутри OpCode=38 (UnicodeText2). }
procedure AppendPaddedUnicodeString(var Data: TBytes; const Value: UnicodeString);
var
  I, OldLen, ByteLen: Integer;
  W: Word;
begin
  ByteLen := Length(Value) * 2;
  OldLen := Length(Data);
  AppendBytes(Data, ByteLen + 2);
  for I := 1 to Length(Value) do
  begin
    W := Word(Value[I]);
    Data[OldLen + (I - 1) * 2]     := Byte(W and $FF);
    Data[OldLen + (I - 1) * 2 + 1] := Byte((W shr 8) and $FF);
  end;
  Data[OldLen + ByteLen]     := 0;
  Data[OldLen + ByteLen + 1] := 0;

  { Выравнивание до границы DWORD }
  while (Length(Data) mod 4) <> 0 do
  begin
    SetLength(Data, Length(Data) + 1);
    Data[High(Data)] := 0;
  end;
end;

{ Кодирует строку как ANSI (однобайтовую), завершает нулевым байтом и
  выравнивает позицию данных до границы 4 байт. Формат соответствует тому,
  как AutoCAD пишет строки внутри OpCode=38 (UnicodeText2) в DXF 2000/2004
  (версии до AC1021), где «широкие» строки на самом деле однобайтовые.
  Совместим с TProxyByteStream.ReadPaddedUnicodeString в режиме UnicodeText=False. }
procedure AppendPaddedAnsiString(var Data: TBytes; const Value: AnsiString);
var
  I, OldLen, ByteLen: Integer;
begin
  ByteLen := Length(Value);
  OldLen := Length(Data);
  AppendBytes(Data, ByteLen + 1);
  for I := 1 to ByteLen do
    Data[OldLen + (I - 1)] := Byte(Value[I]);
  Data[OldLen + ByteLen] := 0;

  { Выравнивание до границы DWORD }
  while (Length(Data) mod 4) <> 0 do
  begin
    SetLength(Data, Length(Data) + 1);
    Data[High(Data)] := 0;
  end;
end;

{ Собирает payload для OpCode=38 (UnicodeText2) в формате DXF 2000/2004,
  где «широкие» строки на самом деле однобайтовые (ANSI). Используется
  тестом регрессии для бага «ProxyEntity. Не считывается текст из DXF 2000». }
procedure AppendAnsiUnicodeText2Payload(var Payload: TBytes;
  const Text, TypeFace, FontFile, BigFont: AnsiString);
begin
  { Точка вставки, нормаль, направление }
  AppendVertex(Payload, 0.0, 0.0, 0.0);
  AppendVertex(Payload, 0.0, 0.0, 1.0);
  AppendVertex(Payload, 1.0, 0.0, 0.0);

  { Строка текста (ANSI, как в DXF 2000) }
  AppendPaddedAnsiString(Payload, Text);

  { IgnoreLength, Raw }
  AppendInt32(Payload, 0);
  AppendInt32(Payload, 0);

  { Height, WidthFactor }
  AppendDouble(Payload, 2.5);
  AppendDouble(Payload, 1.0);

  { ObliqueAngle, TrackingPercentage }
  AppendDouble(Payload, 0.0);
  AppendDouble(Payload, 1.0);

  { Флаги: IsBackward, IsUpsideDown, IsVertical, IsUnderlined, IsOverlined }
  AppendInt32(Payload, 0);
  AppendInt32(Payload, 0);
  AppendInt32(Payload, 0);
  AppendInt32(Payload, 0);
  AppendInt32(Payload, 0);

  { IsBold, IsItalic, Charset, Pitch }
  AppendInt32(Payload, 0);
  AppendInt32(Payload, 0);
  AppendInt32(Payload, 0);
  AppendInt32(Payload, 0);

  { TypeFace, FontFilename, BigFontFilename (ANSI) }
  AppendPaddedAnsiString(Payload, TypeFace);
  AppendPaddedAnsiString(Payload, FontFile);
  AppendPaddedAnsiString(Payload, BigFont);
end;

{ Собирает payload для OpCode=38 (UnicodeText2) с заданным текстом,
  TypeFace, FontFile и BigFont. Используется тестом для проверки,
  что парсер корректно читает и сохраняет эти три поля шрифта. }
procedure AppendUnicodeText2Payload(var Payload: TBytes;
  const Text, TypeFace, FontFile, BigFont: UnicodeString);
begin
  { Точка вставки, нормаль, направление }
  AppendVertex(Payload, 0.0, 0.0, 0.0);
  AppendVertex(Payload, 0.0, 0.0, 1.0);
  AppendVertex(Payload, 1.0, 0.0, 0.0);

  { Строка текста }
  AppendPaddedUnicodeString(Payload, Text);

  { IgnoreLength, Raw }
  AppendInt32(Payload, 0);
  AppendInt32(Payload, 0);

  { Height, WidthFactor }
  AppendDouble(Payload, 2.5);
  AppendDouble(Payload, 1.0);

  { ObliqueAngle, TrackingPercentage }
  AppendDouble(Payload, 0.0);
  AppendDouble(Payload, 1.0);

  { Флаги: IsBackward, IsUpsideDown, IsVertical, IsUnderlined, IsOverlined }
  AppendInt32(Payload, 0);
  AppendInt32(Payload, 0);
  AppendInt32(Payload, 0);
  AppendInt32(Payload, 0);
  AppendInt32(Payload, 0);

  { IsBold, IsItalic, Charset, Pitch }
  AppendInt32(Payload, 0);
  AppendInt32(Payload, 0);
  AppendInt32(Payload, 0);
  AppendInt32(Payload, 0);

  { TypeFace, FontFilename, BigFontFilename }
  AppendPaddedUnicodeString(Payload, TypeFace);
  AppendPaddedUnicodeString(Payload, FontFile);
  AppendPaddedUnicodeString(Payload, BigFont);
end;

{ Собирает proxy graphic с двумя UnicodeText2-примитивами:
  - один с TypeFace="Times New Roman" и пустым FontFile (стиль newtext),
  - один с пустым TypeFace и FontFile="txt.shx" (стиль Standard).
  Это воспроизводит реальные данные из tablerazdel2.dxf, где строки
  таблицы используют разные стили текста. }
function BuildProxyGraphicWithTwoUnicodeText2: TBytes;
var
  Body, Payload: TBytes;
begin
  SetLength(Body, 0);

  SetLength(Payload, 0);
  AppendUnicodeText2Payload(Payload, 'Times row', 'Times New Roman', '', '');
  AppendCommand(Body, 38, Payload);

  SetLength(Payload, 0);
  AppendUnicodeText2Payload(Payload, 'Shx row', '', 'txt.shx', '');
  AppendCommand(Body, 38, Payload);

  SetLength(Result, 0);
  AppendInt32(Result, Length(Body) + 8);
  AppendInt32(Result, 2);
  if Length(Body) > 0 then
  begin
    AppendBytes(Result, Length(Body));
    Move(Body[0], Result[Length(Result) - Length(Body)], Length(Body));
  end;
end;

{ Собирает proxy graphic с двумя UnicodeText2-примитивами в формате
  DXF 2000 (AC1015), где «широкие» строки кодируются в однобайтовой ANSI.
  Воспроизводит содержимое spdsconstructionline2000.dxf, где текст оси
  и имя shx-шрифта хранятся в ANSI, а не в UTF-16. }
function BuildProxyGraphicWithAnsiUnicodeText2: TBytes;
var
  Body, Payload: TBytes;
begin
  SetLength(Body, 0);

  SetLength(Payload, 0);
  AppendAnsiUnicodeText2Payload(Payload, '15', '', 'CS_Gost2304.shx', '');
  AppendCommand(Body, 38, Payload);

  SetLength(Payload, 0);
  AppendAnsiUnicodeText2Payload(Payload, 'Axis', 'Times New Roman', '', '');
  AppendCommand(Body, 38, Payload);

  SetLength(Result, 0);
  AppendInt32(Result, Length(Body) + 8);
  AppendInt32(Result, 2);
  if Length(Body) > 0 then
  begin
    AppendBytes(Result, Length(Body));
    Move(Body[0], Result[Length(Result) - Length(Body)], Length(Body));
  end;
end;

{ Собирает payload для OpCode=11 (Text2) в формате DXF 2000/2004,
  где все строки однобайтовые (ANSI). Отличие от OpCode=38: нет поля
  TypeFace и нет Bold/Italic/Charset/Pitch. Используется тестом регрессии
  на issue #971. }
procedure AppendText2Payload(var Payload: TBytes;
  const Text, FontFile, BigFont: AnsiString);
begin
  { Точка вставки, нормаль, направление }
  AppendVertex(Payload, 0.0, 0.0, 0.0);
  AppendVertex(Payload, 0.0, 0.0, 1.0);
  AppendVertex(Payload, 1.0, 0.0, 0.0);

  { Строка текста (ANSI) }
  AppendPaddedAnsiString(Payload, Text);

  { Length, Raw }
  AppendInt32(Payload, -1);
  AppendInt32(Payload, 0);

  { Height, WidthFactor }
  AppendDouble(Payload, 350.0);
  AppendDouble(Payload, 1.0);

  { ObliqueAngle, TrackingPercentage }
  AppendDouble(Payload, 0.0);
  AppendDouble(Payload, 1.0);

  { Флаги: IsBackward, IsUpsideDown, IsVertical, IsUnderlined, IsOverlined }
  AppendInt32(Payload, 0);
  AppendInt32(Payload, 0);
  AppendInt32(Payload, 0);
  AppendInt32(Payload, 0);
  AppendInt32(Payload, 0);

  { FontFilename, BigFontFilename (ANSI).
    В OpCode=11 нет полей TypeFace, IsBold, IsItalic, Charset, Pitch. }
  AppendPaddedAnsiString(Payload, FontFile);
  AppendPaddedAnsiString(Payload, BigFont);
end;

{ Собирает proxy graphic с двумя OpCode=11 (Text2) примитивами
  в формате DXF 2000. Воспроизводит содержимое
  spdsconstructionline2000.dxf, где текст оси "15" записан именно
  с OpCode=11 (а не OpCode=38). }
function BuildProxyGraphicWithText2: TBytes;
var
  Body, Payload: TBytes;
begin
  SetLength(Body, 0);

  SetLength(Payload, 0);
  AppendText2Payload(Payload, '15', 'CS_Gost2304.shx', '');
  AppendCommand(Body, 11, Payload);

  SetLength(Payload, 0);
  AppendText2Payload(Payload, 'Axis', 'txt.shx', '');
  AppendCommand(Body, 11, Payload);

  SetLength(Result, 0);
  AppendInt32(Result, Length(Body) + 8);
  AppendInt32(Result, 2);
  if Length(Body) > 0 then
  begin
    AppendBytes(Result, Length(Body));
    Move(Body[0], Result[Length(Result) - Length(Body)], Length(Body));
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
    CheckTrue(entity^.vp.Layer <> nil, 'У proxy-объекта должен быть назначен слой');
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
  Center: TzePoint3d;
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
  center, delta: TzePoint3d;
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

procedure TProxyEntityLoadTest.ProxyGraphicUnicodeText2StoresFontFields;
var
  Parser: TProxyGraphicParser;
  ParseResult: TProxyGraphicParseResult;
  TimesIdx, ShxIdx, I: Integer;
begin
  Parser := TProxyGraphicParser.Create(BuildProxyGraphicWithTwoUnicodeText2);
  try
    ParseResult := Parser.Parse;
  finally
    Parser.Free;
  end;

  CheckEquals(2, Length(ParseResult.Primitives),
    'Тестовый proxy graphic должен содержать два UnicodeText2 примитива');

  TimesIdx := -1;
  ShxIdx := -1;
  for I := 0 to High(ParseResult.Primitives) do
  begin
    CheckTrue(ParseResult.Primitives[I].HandlerResult.HasTextItem,
      'Каждый UnicodeText2 должен заполнять TextItem в HandlerResult');
    if ParseResult.Primitives[I].HandlerResult.TextItem.Text = 'Times row' then
      TimesIdx := I
    else if ParseResult.Primitives[I].HandlerResult.TextItem.Text = 'Shx row' then
      ShxIdx := I;
  end;

  Check(TimesIdx >= 0, 'Должен быть примитив с текстом "Times row"');
  Check(ShxIdx >= 0, 'Должен быть примитив с текстом "Shx row"');

  CheckEquals('Times New Roman',
    ParseResult.Primitives[TimesIdx].HandlerResult.TextItem.TypeFace,
    'TypeFace должен сохраняться из OpCode=38 для подбора стиля по FontFamily');
  CheckEquals('',
    ParseResult.Primitives[TimesIdx].HandlerResult.TextItem.FontName,
    'Пустой FontFile должен оставаться пустым, а не заменяться TypeFace');

  CheckEquals('',
    ParseResult.Primitives[ShxIdx].HandlerResult.TextItem.TypeFace,
    'Отсутствие TypeFace должно приводить к пустой строке');
  CheckEquals('txt.shx',
    ParseResult.Primitives[ShxIdx].HandlerResult.TextItem.FontName,
    'FontFile должен сохраняться независимо от TypeFace');
end;

{ Помощник: запустить прокси из spdsdoor.dxf, вернуть указатели. }
procedure LoadSpdsDoorProxy(var drawing: TSimpleDrawing;
  var dc: TDrawContext; out proxy: PGDBObjAcdProxy);
var
  zdc: TZDrawingContext;
  entity: PGDBObjEntity;
begin
  drawing.init(nil);
  dc := drawing.CreateDrawingRC;
  zdc.CreateRec(drawing, drawing.pObjRoot^, TLOLoad, dc);
  AddFromDXF('cad_source/test/spdsdoor.dxf', zdc);
  entity := PGDBObjEntity(drawing.pObjRoot^.ObjArray.GetData(0));
  proxy := PGDBObjAcdProxy(entity);
end;

procedure TProxyEntityLoadTest.SpdsDoorProxyRotationTransformsSubEntities;
var
  drawing: TSimpleDrawing;
  dc: TDrawContext;
  proxy: PGDBObjAcdProxy;
  line: PGDBObjLine;
  center: TzePoint3d;
  beforeLocal, afterLocal: TzePoint3d;
  beforeWCS, afterWCS: TzePoint3d;
  rotMatrix: TzeTypedMatrix4d;
  angle: double;
  cosA, sinA: double;
begin
  LoadSpdsDoorProxy(drawing, dc, proxy);
  try
    Check(proxy^.ConstObjArray.Count > 0,
      'Proxy graphic должен построить подпримитивы');

    line := PGDBObjLine(proxy^.ConstObjArray.GetData(0));
    center := proxy^.GetCenterPoint;
    beforeLocal := line^.CoordInOCS.lBegin;
    beforeWCS := line^.CoordInWCS.lBegin;

    CheckEquals(beforeLocal.x + center.x, beforeWCS.x, 1e-6,
      'До поворота WCS-координата = локальная + центр');

    { Поворачиваем на 90° вокруг grip center }
    angle := pi / 2;
    rotMatrix := CreateRotationMatrixZ(angle);
    proxy^.TransformAt(PGDBObjEntity(proxy), @rotMatrix);
    proxy^.FormatEntity(drawing, dc);

    line := PGDBObjLine(proxy^.ConstObjArray.GetData(0));
    afterLocal := line^.CoordInOCS.lBegin;
    afterWCS := line^.CoordInWCS.lBegin;

    { Локальные координаты подпримитивов НЕ изменяются (rotation применяется
      через owner matrix). Изменяется только WCS. }
    CheckEquals(beforeLocal.x, afterLocal.x, 1e-6,
      'Локальные координаты подпримитива не должны меняться при повороте владельца');
    CheckEquals(beforeLocal.y, afterLocal.y, 1e-6,
      'Локальные координаты подпримитива не должны меняться при повороте владельца');

    { TransformAt умножает objmatrix на t_matrix справа — значит финальная
      WCS-позиция точки есть её прежняя WCS-позиция, повёрнутая вокруг
      начала мировых координат на angle вокруг Z. }
    cosA := cos(angle);
    sinA := sin(angle);
    CheckEquals(cosA * beforeWCS.x - sinA * beforeWCS.y,
      afterWCS.x, 1e-6,
      'После поворота WCS должен отражать rotZ вокруг начала координат');
    CheckEquals(sinA * beforeWCS.x + cosA * beforeWCS.y,
      afterWCS.y, 1e-6,
      'После поворота WCS должен отражать rotZ вокруг начала координат');
  finally
    drawing.done;
  end;
end;

procedure TProxyEntityLoadTest.SpdsDoorProxyUniformScaleTransformsSubEntities;
var
  drawing: TSimpleDrawing;
  dc: TDrawContext;
  proxy: PGDBObjAcdProxy;
  line: PGDBObjLine;
  center: TzePoint3d;
  beforeLocal: TzePoint3d;
  afterWCS: TzePoint3d;
  scaleMatrix: TzeTypedMatrix4d;
  scaleFactor: double;
begin
  LoadSpdsDoorProxy(drawing, dc, proxy);
  try
    Check(proxy^.ConstObjArray.Count > 0,
      'Proxy graphic должен построить подпримитивы');

    line := PGDBObjLine(proxy^.ConstObjArray.GetData(0));
    center := proxy^.GetCenterPoint;
    beforeLocal := line^.CoordInOCS.lBegin;

    scaleFactor := 2.0;
    scaleMatrix := CreateScaleMatrix(scaleFactor);
    proxy^.TransformAt(PGDBObjEntity(proxy), @scaleMatrix);
    proxy^.FormatEntity(drawing, dc);

    line := PGDBObjLine(proxy^.ConstObjArray.GetData(0));
    afterWCS := line^.CoordInWCS.lBegin;

    { После масштабирования X2 вокруг grip center:
      newWCS = grip_center + scale * (beforeLocal) }
    CheckEquals(center.x * scaleFactor + scaleFactor * beforeLocal.x,
      afterWCS.x, 1e-6,
      'После масштабирования WCS подпримитива должен быть умножен на scale относительно начала координат WCS');
    CheckEquals(center.y * scaleFactor + scaleFactor * beforeLocal.y,
      afterWCS.y, 1e-6,
      'После масштабирования WCS подпримитива должен быть умножен на scale относительно начала координат WCS');
  finally
    drawing.done;
  end;
end;

{ Вспомогательная процедура: создаёт корректно инициализированный
  drawing и загружает в него spdsdoor.dxf, возвращая proxy и dc.
  В отличие от LoadSpdsDoorProxy оставляет drawing в валидном
  состоянии, готовом для использования GetBlockDefArraySimple. }
procedure LoadSpdsDoorDrawing(var drawing: TSimpleDrawing;
  var dc: TDrawContext; out proxy: PGDBObjAcdProxy);
var
  zdc: TZDrawingContext;
  entity: PGDBObjEntity;
begin
  drawing.init(nil);
  dc := drawing.CreateDrawingRC;
  zdc.CreateRec(drawing, drawing.pObjRoot^, TLOLoad, dc);
  AddFromDXF('cad_source/test/spdsdoor.dxf', zdc);
  entity := PGDBObjEntity(drawing.pObjRoot^.ObjArray.GetData(0));
  proxy := PGDBObjAcdProxy(entity);
end;

procedure TProxyEntityLoadTest.GeneratedProxyBlockNameHasPrefixAndRange;
var
  drawing: TSimpleDrawing;
  dc: TDrawContext;
  proxy: PGDBObjAcdProxy;
  Name: string;
  NumStr: string;
  N, I: Integer;
begin
  LoadSpdsDoorDrawing(drawing, dc, proxy);
  try
    { Генерируем несколько имён подряд и проверяем формат }
    for I := 0 to 9 do
    begin
      Name := GenerateUniqueProxyBlockName(drawing);
      CheckTrue(Length(Name) > 2,
        'Сгенерированное имя должно содержать префикс + число');
      CheckEquals('PE', Copy(Name, 1, 2),
        'Имя блока должно начинаться с префикса "PE"');
      NumStr := Copy(Name, 3, Length(Name) - 2);
      N := StrToIntDef(NumStr, -1);
      CheckTrue(N >= 0,
        'После "PE" должно следовать неотрицательное целое');
      CheckTrue(N <= 1000000000,
        'Число после "PE" должно быть в диапазоне [0..1_000_000_000]');
    end;
  finally
    drawing.done;
  end;
end;

procedure TProxyEntityLoadTest.GeneratedProxyBlockNamesAreUniqueInArray;
var
  drawing: TSimpleDrawing;
  dc: TDrawContext;
  proxy: PGDBObjAcdProxy;
  BlockArr: PGDBObjBlockdefArray;
  Name: string;
begin
  LoadSpdsDoorDrawing(drawing, dc, proxy);
  try
    BlockArr := PGDBObjBlockdefArray(drawing.GetBlockDefArraySimple);
    { Имя должно отсутствовать в массиве до создания блока }
    Name := GenerateUniqueProxyBlockName(drawing);
    CheckTrue(BlockArr^.getindex(Name) < 0,
      'Сгенерированное имя не должно совпадать с существующими блоками');
    { Создаём блок с этим именем, следующее имя должно быть другим }
    BlockArr^.create(Name);
    CheckTrue(GenerateUniqueProxyBlockName(drawing) <> Name,
      'Повторный вызов после создания блока должен вернуть другое имя');
  finally
    drawing.done;
  end;
end;

procedure TProxyEntityLoadTest.EnsureConvertedBlockDefCreatesBlockWithSubEntities;
var
  drawing: TSimpleDrawing;
  dc: TDrawContext;
  proxy: PGDBObjAcdProxy;
  BlockArr: PGDBObjBlockdefArray;
  BlockName: string;
  Idx: Integer;
  BlockDef: PGDBObjBlockdef;
  CountBefore, CountAfter: Integer;
begin
  LoadSpdsDoorDrawing(drawing, dc, proxy);
  try
    BlockArr := PGDBObjBlockdefArray(drawing.GetBlockDefArraySimple);
    CountBefore := BlockArr^.Count;

    BlockName := proxy^.EnsureConvertedBlockDef(drawing);

    CountAfter := BlockArr^.Count;
    CheckEquals(CountBefore + 1, CountAfter,
      'EnsureConvertedBlockDef должен добавить ровно один блок в BlockDefArray');
    CheckEquals('PE', Copy(BlockName, 1, 2),
      'Имя сгенерированного блока должно начинаться с "PE"');

    Idx := BlockArr^.getindex(BlockName);
    CheckTrue(Idx >= 0,
      'Созданный блок должен быть доступен по имени через getindex');
    BlockDef := BlockArr^.getDataMutable(Idx);
    CheckTrue(BlockDef <> nil,
      'BlockDef по индексу не должен быть nil');
    CheckTrue(BlockDef^.ObjArray.Count > 0,
      'Блок должен содержать скопированные подпримитивы proxy-объекта');
    CheckEquals(proxy^.ConstObjArray.Count, BlockDef^.ObjArray.Count,
      'Количество сущностей блока должно совпадать с ConstObjArray proxy');
  finally
    drawing.done;
  end;
end;

procedure TProxyEntityLoadTest.EnsureConvertedBlockDefIsIdempotent;
var
  drawing: TSimpleDrawing;
  dc: TDrawContext;
  proxy: PGDBObjAcdProxy;
  BlockArr: PGDBObjBlockdefArray;
  Name1, Name2: string;
  CountAfterFirst, CountAfterSecond: Integer;
begin
  LoadSpdsDoorDrawing(drawing, dc, proxy);
  try
    BlockArr := PGDBObjBlockdefArray(drawing.GetBlockDefArraySimple);

    Name1 := proxy^.EnsureConvertedBlockDef(drawing);
    CountAfterFirst := BlockArr^.Count;

    Name2 := proxy^.EnsureConvertedBlockDef(drawing);
    CountAfterSecond := BlockArr^.Count;

    CheckEquals(Name1, Name2,
      'Повторный вызов EnsureConvertedBlockDef должен вернуть то же имя');
    CheckEquals(CountAfterFirst, CountAfterSecond,
      'Повторный вызов EnsureConvertedBlockDef не должен создавать новый блок');

    CheckEquals(Name1, proxy^.GetConvertedBlockName,
      'GetConvertedBlockName должен возвращать кэшированное имя');
  finally
    drawing.done;
  end;
end;

procedure TProxyEntityLoadTest.ConvertProxyEntitiesToBlocksAddsBlocksForAllProxies;
var
  drawing: TSimpleDrawing;
  dc: TDrawContext;
  proxy: PGDBObjAcdProxy;
  BlockArr: PGDBObjBlockdefArray;
  CountBefore, CountAfter: Integer;
  ConvertedName: string;
begin
  LoadSpdsDoorDrawing(drawing, dc, proxy);
  try
    BlockArr := PGDBObjBlockdefArray(drawing.GetBlockDefArraySimple);
    CountBefore := BlockArr^.Count;

    ConvertProxyEntitiesToBlocks(drawing);

    CountAfter := BlockArr^.Count;
    CheckTrue(CountAfter > CountBefore,
      'ConvertProxyEntitiesToBlocks должен добавить блок(и) для proxy-объектов');
    ConvertedName := proxy^.GetConvertedBlockName;
    CheckTrue(Length(ConvertedName) > 2,
      'Proxy должен получить имя сгенерированного блока после обхода');
    CheckEquals('PE', Copy(ConvertedName, 1, 2),
      'Имя сгенерированного блока должно начинаться с "PE"');
    CheckTrue(BlockArr^.getindex(ConvertedName) >= 0,
      'Соответствующий блок должен присутствовать в BlockDefArray');
  finally
    drawing.done;
  end;
end;

{ Регрессионный тест для issue #965: ранее ConvertProxyEntitiesToBlocks
  обходил только корневой ObjArray и не обрабатывал proxy-сущности
  внутри определений блоков. В результате при сохранении DXF в
  BLOCKS-секции появлялись INSERT PE<N>, но самих PE<N>-блоков там
  не было — при повторном открытии BlockInsert.BuildGeometry падал
  на assert(index >= 0) в uzeentblockinsert.pas:321.

  Тест использует proxyinblock.dxf, где proxy-сущности лежат
  внутри определения блока "Оси_блок Б", и проверяет, что после
  ConvertProxyEntitiesToBlocks для каждой такой proxy-сущности в
  BlockDefArray появляется соответствующий PE<N>-блок. }
procedure TProxyEntityLoadTest.ConvertProxyEntitiesToBlocksHandlesProxiesInsideBlockDef;
var
  drawing: TSimpleDrawing;
  dc: TDrawContext;
  zdc: TZDrawingContext;
  BlockArr: PGDBObjBlockdefArray;
  I, J, NestedProxyCount, InitialBlockCount: Integer;
  BlockDef: PGDBObjBlockdef;
  Ent: PGDBObjEntity;
  NestedProxies: array of PGDBObjAcdProxy;
begin
  drawing.init(nil);
  try
    dc := drawing.CreateDrawingRC;
    zdc.CreateRec(drawing, drawing.pObjRoot^, TLOLoad, dc);
    AddFromDXF('cad_source/test/proxyinblock.dxf', zdc);

    { Собираем proxy-сущности, лежащие внутри определений блоков }
    BlockArr := PGDBObjBlockdefArray(drawing.GetBlockDefArraySimple);
    InitialBlockCount := BlockArr^.Count;
    NestedProxyCount := 0;
    SetLength(NestedProxies, 0);
    for I := 0 to InitialBlockCount - 1 do
    begin
      BlockDef := BlockArr^.getDataMutable(I);
      if BlockDef = nil then
        Continue;
      for J := 0 to BlockDef^.ObjArray.Count - 1 do
      begin
        Ent := PGDBObjEntity(BlockDef^.ObjArray.GetData(J));
        if (Ent <> nil) and (Ent^.GetObjType = GDBAcdProxyID) then
        begin
          SetLength(NestedProxies, NestedProxyCount + 1);
          NestedProxies[NestedProxyCount] := PGDBObjAcdProxy(Ent);
          Inc(NestedProxyCount);
        end;
      end;
    end;
    CheckTrue(NestedProxyCount > 0,
      'Тестовый файл proxyinblock.dxf должен содержать proxy-сущности внутри блоков');

    ConvertProxyEntitiesToBlocks(drawing);

    { После конвертации для каждой найденной proxy в BlockDefArray
      должен появиться PE<N>-блок и proxy должна запомнить его имя. }
    for I := 0 to NestedProxyCount - 1 do
    begin
      CheckTrue(Length(NestedProxies[I]^.GetConvertedBlockName) > 2,
        'Каждой вложенной proxy должно быть присвоено имя PE<N>');
      CheckEquals('PE',
        Copy(NestedProxies[I]^.GetConvertedBlockName, 1, 2),
        'Имя сгенерированного блока должно начинаться с "PE"');
      CheckTrue(
        BlockArr^.getindex(
          NestedProxies[I]^.GetConvertedBlockName) >= 0,
        'PE-блок должен присутствовать в BlockDefArray');
    end;
  finally
    drawing.done;
  end;
end;

procedure TProxyEntityLoadTest.SpdsConstructionLineProxyKeepsLineTypeScale;
var
  drawing: TSimpleDrawing;
  dc: TDrawContext;
  zdc: TZDrawingContext;
  entity: PGDBObjEntity;
  proxy: PGDBObjAcdProxy;
  sub: PGDBObjEntity;
  I: Integer;
begin
  drawing.init(nil);
  try
    dc := drawing.CreateDrawingRC;
    zdc.CreateRec(drawing, drawing.pObjRoot^, TLOLoad, dc);
    AddFromDXF('cad_source/test/spdsconstructionline.dxf', zdc);

    { Файл содержит один SPDSCONSTRUCTIONLINE внутри ENTITIES. }
    CheckTrue(drawing.pObjRoot^.ObjArray.Count >= 1,
      'spdsconstructionline.dxf должен загружать как минимум одну сущность');

    entity := nil;
    for I := 0 to drawing.pObjRoot^.ObjArray.Count - 1 do
    begin
      entity := PGDBObjEntity(drawing.pObjRoot^.ObjArray.GetData(I));
      if (entity <> nil)
        and (entity^.GetObjTypeName = ObjN_GDBObjAcdProxy) then
        Break;
      entity := nil;
    end;
    Check(entity <> nil,
      'SPDSCONSTRUCTIONLINE должен загружаться как proxy-объект');

    { DXF group code 48 = 100.0 в spdsconstructionline.dxf: прокси-объект
      должен прочитать этот код и сохранить его в vp.LineTypeScale. }
    CheckEquals(100.0, entity^.vp.LineTypeScale, 1e-9,
      'Proxy должен читать DXF code 48 и сохранять значение 100.0');

    { Явно форматируем прокси, чтобы триггернуть BuildSubEntities:
      без этого ConstObjArray остаётся пустым, так как DXF-загрузка
      не вызывает FormatEntity для прокси-объектов. }
    entity^.FormatEntity(drawing, dc);

    proxy := PGDBObjAcdProxy(entity);
    Check(proxy^.ConstObjArray.Count > 0,
      'Proxy graphic должен построить подпримитивы');

    { Каждый подпримитив должен наследовать LineTypeScale владельца
      (или его произведение с per-primitive LtScale, но не стандартное 1.0). }
    for I := 0 to proxy^.ConstObjArray.Count - 1 do
    begin
      sub := PGDBObjEntity(proxy^.ConstObjArray.GetData(I));
      if sub = nil then
        Continue;
      CheckTrue(sub^.vp.LineTypeScale >= 100.0 - 1e-6,
        'Подпримитив proxy-объекта должен наследовать LineTypeScale владельца (>=100)');
    end;
  finally
    drawing.done;
  end;
end;

procedure TProxyEntityLoadTest.SpdsConstructionLine2ProxyPropagatesScaleToAllSubEntities;
var
  drawing: TSimpleDrawing;
  dc: TDrawContext;
  zdc: TZDrawingContext;
  entity: PGDBObjEntity;
  proxy: PGDBObjAcdProxy;
  sub: PGDBObjEntity;
  Proxies: array of PGDBObjAcdProxy;
  FoundScale77, FoundScale100: Boolean;
  I, J, ProxyIdx: Integer;
begin
  drawing.init(nil);
  try
    dc := drawing.CreateDrawingRC;
    zdc.CreateRec(drawing, drawing.pObjRoot^, TLOLoad, dc);
    AddFromDXF('cad_source/test/spdsconstructionline2.dxf', zdc);

    { Файл содержит две SPDSCONSTRUCTIONLINE сущности: первая
      имеет DXF code 48 = 77.0, вторая — 48 = 100.0. Собираем их
      указатели и для каждой проверяем, что LineTypeScale прочитан
      и корректно распространён на все подпримитивы. }
    SetLength(Proxies, 0);
    ProxyIdx := 0;
    for I := 0 to drawing.pObjRoot^.ObjArray.Count - 1 do
    begin
      entity := PGDBObjEntity(drawing.pObjRoot^.ObjArray.GetData(I));
      if (entity <> nil)
        and (entity^.GetObjTypeName = ObjN_GDBObjAcdProxy) then
      begin
        SetLength(Proxies, ProxyIdx + 1);
        Proxies[ProxyIdx] := PGDBObjAcdProxy(entity);
        Inc(ProxyIdx);
      end;
    end;
    CheckEquals(2, Length(Proxies),
      'spdsconstructionline2.dxf должен содержать 2 SPDSCONSTRUCTIONLINE');

    FoundScale77 := False;
    FoundScale100 := False;
    for I := 0 to High(Proxies) do
    begin
      if Abs(Proxies[I]^.vp.LineTypeScale - 77.0) < 1e-6 then
        FoundScale77 := True;
      if Abs(Proxies[I]^.vp.LineTypeScale - 100.0) < 1e-6 then
        FoundScale100 := True;
    end;
    CheckTrue(FoundScale77,
      'Один из proxy должен иметь LineTypeScale = 77.0 (DXF code 48)');
    CheckTrue(FoundScale100,
      'Один из proxy должен иметь LineTypeScale = 100.0 (DXF code 48)');

    { Явно форматируем каждый прокси, чтобы триггернуть BuildSubEntities. }
    for I := 0 to High(Proxies) do
      PGDBObjEntity(Proxies[I])^.FormatEntity(drawing, dc);

    for I := 0 to High(Proxies) do
    begin
      proxy := Proxies[I];
      Check(proxy^.ConstObjArray.Count > 0,
        'Каждый proxy должен построить подпримитивы после FormatEntity');

      { Все подпримитивы данного прокси должны иметь LineTypeScale,
        унаследованный от владельца. ResolveLineTypeScale умножает
        OwnerLineTypeScale на PrimitiveLineTypeScale (>=1), поэтому
        результат не может быть меньше vp.LineTypeScale владельца
        (с точностью до 1e-6). }
      for J := 0 to proxy^.ConstObjArray.Count - 1 do
      begin
        sub := PGDBObjEntity(proxy^.ConstObjArray.GetData(J));
        if sub = nil then
          Continue;
        CheckTrue(
          sub^.vp.LineTypeScale >= proxy^.vp.LineTypeScale - 1e-6,
          'LineTypeScale подпримитива должен быть не меньше LineTypeScale владельца');
        { Подпримитив не должен остаться со стандартным значением 1.0
          (если OwnerLineTypeScale != 1), иначе масштаб типа линии не
          будет корректно применяться при отрисовке штрихов. }
        CheckTrue(
          Abs(sub^.vp.LineTypeScale - 1.0) > 1e-6,
          'Подпримитив не должен сохранять LineTypeScale = 1 при ненулевом масштабе владельца');
      end;
    end;
  finally
    drawing.done;
  end;
end;

{ Регрессия на issue #971: в DXF 2000/2004 «широкие» строки внутри
  бинарной Proxy Graphic хранятся в однобайтовой ANSI-кодировке, а не
  в UTF-16. Если парсер интерпретирует их как UTF-16, то каждый второй
  байт используется как старший байт символа, и вместо "15" получается
  мусор — текст оказывается некорректным.

  Тест собирает proxy graphic с двумя OpCode=38 UnicodeText2 примитивами
  в ANSI-формате и создаёт парсер с AUnicodeText=False. После исправления
  текст и шрифт должны читаться корректно. }
procedure TProxyEntityLoadTest.ProxyGraphicAnsiUnicodeText2ParsesInDxf2000Mode;
var
  Parser: TProxyGraphicParser;
  ParseResult: TProxyGraphicParseResult;
  AxisIdx, LabelIdx, I: Integer;
begin
  Parser := TProxyGraphicParser.Create(
    BuildProxyGraphicWithAnsiUnicodeText2, False);
  try
    ParseResult := Parser.Parse;
  finally
    Parser.Free;
  end;

  CheckEquals(2, Length(ParseResult.Primitives),
    'Тестовый proxy graphic должен содержать два UnicodeText2 примитива');

  AxisIdx := -1;
  LabelIdx := -1;
  for I := 0 to High(ParseResult.Primitives) do
  begin
    CheckTrue(ParseResult.Primitives[I].HandlerResult.HasTextItem,
      'Каждый UnicodeText2 должен заполнять TextItem в HandlerResult');
    if ParseResult.Primitives[I].HandlerResult.TextItem.Text = '15' then
      AxisIdx := I
    else if ParseResult.Primitives[I].HandlerResult.TextItem.Text = 'Axis' then
      LabelIdx := I;
  end;

  Check(AxisIdx >= 0,
    'В DXF 2000 режиме должен распознаваться текст оси "15" (ANSI)');
  Check(LabelIdx >= 0,
    'В DXF 2000 режиме должен распознаваться текст "Axis" (ANSI)');

  CheckEquals('CS_Gost2304.shx',
    ParseResult.Primitives[AxisIdx].HandlerResult.TextItem.FontName,
    'FontFile в ANSI-формате должен читаться как "CS_Gost2304.shx"');
  CheckEquals('Times New Roman',
    ParseResult.Primitives[LabelIdx].HandlerResult.TextItem.TypeFace,
    'TypeFace в ANSI-формате должен читаться как "Times New Roman"');
end;

{ Регрессия на issue #971: AutoCAD/nanoCAD в DXF 2000 записывают текст
  внутри Proxy Graphic через OpCode=11 (pgcText2) — укороченный вариант
  OpCode=38 без TypeFace и без Bold/Italic/Charset/Pitch, все строки
  однобайтовые (ANSI). До исправления этот OpCode не был зарегистрирован
  и пропускался диспетчером, из-за чего текст «15» терялся и
  SPDSCONSTRUCTIONLINE отображался без размерного числа. Тест собирает
  proxy graphic с двумя OpCode=11 примитивами и проверяет, что парсер
  их корректно распознаёт и заполняет TextItem. }
procedure TProxyEntityLoadTest.ProxyGraphicText2ParsesDxf2000Format;
var
  Parser: TProxyGraphicParser;
  ParseResult: TProxyGraphicParseResult;
  AxisIdx, LabelIdx, I: Integer;
begin
  Parser := TProxyGraphicParser.Create(
    BuildProxyGraphicWithText2, False);
  try
    ParseResult := Parser.Parse;
  finally
    Parser.Free;
  end;

  CheckEquals(2, Length(ParseResult.Primitives),
    'Тестовый proxy graphic должен содержать два Text2 примитива');

  AxisIdx := -1;
  LabelIdx := -1;
  for I := 0 to High(ParseResult.Primitives) do
  begin
    CheckTrue(ParseResult.Primitives[I].HandlerResult.HasTextItem,
      'Каждый Text2 должен заполнять TextItem в HandlerResult');
    if ParseResult.Primitives[I].HandlerResult.TextItem.Text = '15' then
      AxisIdx := I
    else if ParseResult.Primitives[I].HandlerResult.TextItem.Text = 'Axis' then
      LabelIdx := I;
  end;

  Check(AxisIdx >= 0,
    'OpCode=11 должен распознавать текст оси "15" (ANSI)');
  Check(LabelIdx >= 0,
    'OpCode=11 должен распознавать текст "Axis" (ANSI)');

  CheckEquals('CS_Gost2304.shx',
    ParseResult.Primitives[AxisIdx].HandlerResult.TextItem.FontName,
    'FontName в OpCode=11 должен читаться как "CS_Gost2304.shx"');
  CheckEquals('',
    ParseResult.Primitives[AxisIdx].HandlerResult.TextItem.TypeFace,
    'TypeFace в OpCode=11 отсутствует и должен оставаться пустым');
  CheckEquals('txt.shx',
    ParseResult.Primitives[LabelIdx].HandlerResult.TextItem.FontName,
    'FontName в OpCode=11 должен читаться как "txt.shx"');
end;

{ Регрессия на issue #971: загрузка spdsconstructionline2000.dxf и
  spdsconstructionline2007.dxf должна строить прокси-объекты с одинаковой
  геометрией (текст парсится по-разному в этих форматах, но сама
  графика должна давать один и тот же bounding box с точностью до
  миллиметра). До исправления текст в DXF 2000 не читался, из-за чего
  подпримитивы располагались некорректно. }
procedure TProxyEntityLoadTest.SpdsConstructionLine2000AndDxf2007ProduceSameBBox;
var
  drawing2000, drawing2007: TSimpleDrawing;
  dc2000, dc2007: TDrawContext;
  zdc2000, zdc2007: TZDrawingContext;
  entity2000, entity2007: PGDBObjEntity;
begin
  drawing2000.init(nil);
  drawing2007.init(nil);
  try
    dc2000 := drawing2000.CreateDrawingRC;
    zdc2000.CreateRec(drawing2000, drawing2000.pObjRoot^, TLOLoad, dc2000);
    AddFromDXF('cad_source/test/spdsconstructionline2000.dxf', zdc2000);

    dc2007 := drawing2007.CreateDrawingRC;
    zdc2007.CreateRec(drawing2007, drawing2007.pObjRoot^, TLOLoad, dc2007);
    AddFromDXF('cad_source/test/spdsconstructionline2007.dxf', zdc2007);

    CheckEquals(1, drawing2000.pObjRoot^.ObjArray.Count,
      'spdsconstructionline2000.dxf должен загружать один proxy-объект');
    CheckEquals(1, drawing2007.pObjRoot^.ObjArray.Count,
      'spdsconstructionline2007.dxf должен загружать один proxy-объект');

    entity2000 := PGDBObjEntity(drawing2000.pObjRoot^.ObjArray.GetData(0));
    entity2007 := PGDBObjEntity(drawing2007.pObjRoot^.ObjArray.GetData(0));

    CheckEquals(ObjN_GDBObjAcdProxy, entity2000^.GetObjTypeName,
      'SPDSCONSTRUCTIONLINE должен загружаться как proxy-объект');
    CheckEquals(ObjN_GDBObjAcdProxy, entity2007^.GetObjTypeName,
      'SPDSCONSTRUCTIONLINE должен загружаться как proxy-объект');

    { BBox должен совпадать с точностью до 1 мм: графика в обоих файлах
      одна и та же, различаются только кодировки текстовых строк. }
    CheckEquals(entity2007^.vp.BoundingBox.LBN.x,
                entity2000^.vp.BoundingBox.LBN.x, 1e-3,
      'BBox.LBN.x прокси-объекта должен совпадать для DXF 2000 и DXF 2007');
    CheckEquals(entity2007^.vp.BoundingBox.LBN.y,
                entity2000^.vp.BoundingBox.LBN.y, 1e-3,
      'BBox.LBN.y прокси-объекта должен совпадать для DXF 2000 и DXF 2007');
    CheckEquals(entity2007^.vp.BoundingBox.RTF.x,
                entity2000^.vp.BoundingBox.RTF.x, 1e-3,
      'BBox.RTF.x прокси-объекта должен совпадать для DXF 2000 и DXF 2007');
    CheckEquals(entity2007^.vp.BoundingBox.RTF.y,
                entity2000^.vp.BoundingBox.RTF.y, 1e-3,
      'BBox.RTF.y прокси-объекта должен совпадать для DXF 2000 и DXF 2007');
  finally
    drawing2000.done;
    drawing2007.done;
  end;
end;

{ === Issue #973: цветовое поведение подпримитивов внутри ProxyEntity === }

{ Собирает минимальный proxy graphic из ExtentsCommand, PushMatrix и одной
  полилинии. Если ColorOpCode = True — перед полилинией записывается
  SetColor(ColorValue), иначе состояние остаётся дефолтным (BYLAYER=-1).
  Возвращает полный блок (заголовок + тело), готовый для подачи в
  TProxyGraphicParser.Create. }
function BuildProxyGraphicWithPolylineColor(
  const ColorOpCode: Boolean; const ColorValue: Integer): TBytes;
var
  Body, Payload: TBytes;
  CmdCount: Integer;
begin
  SetLength(Body, 0);
  CmdCount := 0;

  if ColorOpCode then
  begin
    SetLength(Payload, 0);
    AppendInt32(Payload, ColorValue);
    AppendCommand(Body, 14, Payload);
    Inc(CmdCount);
  end;

  { Полилиния из двух точек — минимум для успешного парсинга OpCode=6. }
  SetLength(Payload, 0);
  AppendInt32(Payload, 2);
  AppendVertex(Payload, 0.0, 0.0, 0.0);
  AppendVertex(Payload, 1.0, 0.0, 0.0);
  AppendCommand(Body, 6, Payload);
  Inc(CmdCount);

  SetLength(Result, 0);
  AppendInt32(Result, Length(Body) + 8);
  AppendInt32(Result, CmdCount);
  if Length(Body) > 0 then
  begin
    AppendBytes(Result, Length(Body));
    Move(Body[0], Result[Length(Result) - Length(Body)], Length(Body));
  end;
end;

{ Регрессия на issue #973: если в Proxy Graphic нет SetColor, примитивы
  имеют состояние color = PROXY_DEFAULT_COLOR (-1 = ByLayer). После
  исправления подпримитив должен получить цвет ClByLayer (256), а не
  цвет прокси-объекта. }
procedure TProxyEntityLoadTest.ProxyGraphicByLayerPrimitivesResolveToByLayer;
var
  Parser: TProxyGraphicParser;
  ParseResult: TProxyGraphicParseResult;
begin
  { В потоке нет SetColor → FState.Color остаётся -1 (PROXY_DEFAULT_COLOR). }
  Parser := TProxyGraphicParser.Create(
    BuildProxyGraphicWithPolylineColor(False, 0));
  try
    ParseResult := Parser.Parse;
  finally
    Parser.Free;
  end;

  CheckEquals(1, Length(ParseResult.Primitives),
    'Тестовый proxy graphic должен содержать один примитив');
  CheckEquals(-1, ParseResult.Primitives[0].Color,
    'Без SetColor в потоке цвет примитива должен остаться ByLayer (-1)');
end;

{ Регрессия на issue #973: SetColor(0) в Proxy Graphic означает ByBlock.
  После исправления подпримитив должен наследовать цвет прокси-объекта
  (OwnerColor), что соответствует поведению BlockInsert. }
procedure TProxyEntityLoadTest.ProxyGraphicByBlockPrimitivesInheritOwnerColor;
var
  Parser: TProxyGraphicParser;
  ParseResult: TProxyGraphicParseResult;
begin
  Parser := TProxyGraphicParser.Create(
    BuildProxyGraphicWithPolylineColor(True, 0));
  try
    ParseResult := Parser.Parse;
  finally
    Parser.Free;
  end;

  CheckEquals(1, Length(ParseResult.Primitives),
    'Тестовый proxy graphic должен содержать один примитив');
  CheckEquals(0, ParseResult.Primitives[0].Color,
    'SetColor(0) должен сохраниться как ByBlock (0) в примитиве');
end;

{ Регрессия на issue #973: SetColor(3) в Proxy Graphic задаёт явный
  индекс палитры (зелёный). После исправления подпримитив должен получить
  именно этот цвет, а не цвет прокси-объекта. }
procedure TProxyEntityLoadTest.ProxyGraphicExplicitPrimitiveColorIsPreserved;
var
  Parser: TProxyGraphicParser;
  ParseResult: TProxyGraphicParseResult;
begin
  Parser := TProxyGraphicParser.Create(
    BuildProxyGraphicWithPolylineColor(True, 3));
  try
    ParseResult := Parser.Parse;
  finally
    Parser.Free;
  end;

  CheckEquals(1, Length(ParseResult.Primitives),
    'Тестовый proxy graphic должен содержать один примитив');
  CheckEquals(3, ParseResult.Primitives[0].Color,
    'SetColor(3) должен сохраниться как явный цвет 3 в примитиве');
end;

{ Интеграционный регрессионный тест для issue #973.
  Файл spdsconstructionlineCOLOR.dxf содержит два SPDSCONSTRUCTIONLINE:
    - первый  с цветом 3 (зелёный) в DXF-сущности,
    - второй  с цветом 1 (красный) в DXF-сущности.
  Внутри обоих прокси-график все примитивы нарисованы как ByLayer.
  После исправления все подпримитивы каждого прокси должны иметь
  vp.Color = ClByLayer (256) — не цвет владельца (3 или 1). }
procedure TProxyEntityLoadTest.SpdsConstructionLineColorSubEntitiesUseByLayer;
var
  drawing: TSimpleDrawing;
  dc: TDrawContext;
  zdc: TZDrawingContext;
  entity: PGDBObjEntity;
  proxy: PGDBObjAcdProxy;
  sub: PGDBObjEntity;
  Proxies: array of PGDBObjAcdProxy;
  FoundColor3, FoundColor1: Boolean;
  I, J, ProxyIdx: Integer;
begin
  drawing.init(nil);
  try
    dc := drawing.CreateDrawingRC;
    zdc.CreateRec(drawing, drawing.pObjRoot^, TLOLoad, dc);
    AddFromDXF('cad_source/test/spdsconstructionlineCOLOR.dxf', zdc);

    SetLength(Proxies, 0);
    ProxyIdx := 0;
    for I := 0 to drawing.pObjRoot^.ObjArray.Count - 1 do
    begin
      entity := PGDBObjEntity(drawing.pObjRoot^.ObjArray.GetData(I));
      if (entity <> nil)
        and (entity^.GetObjTypeName = ObjN_GDBObjAcdProxy) then
      begin
        SetLength(Proxies, ProxyIdx + 1);
        Proxies[ProxyIdx] := PGDBObjAcdProxy(entity);
        Inc(ProxyIdx);
      end;
    end;
    CheckEquals(2, Length(Proxies),
      'spdsconstructionlineCOLOR.dxf должен содержать 2 SPDSCONSTRUCTIONLINE');

    { Проверяем, что прокси-объекты получили разные цвета из DXF (3 и 1). }
    FoundColor3 := False;
    FoundColor1 := False;
    for I := 0 to High(Proxies) do
    begin
      if Integer(Proxies[I]^.vp.Color) = 3 then
        FoundColor3 := True;
      if Integer(Proxies[I]^.vp.Color) = 1 then
        FoundColor1 := True;
    end;
    CheckTrue(FoundColor3,
      'Один из proxy должен иметь цвет 3 (зелёный, DXF code 62)');
    CheckTrue(FoundColor1,
      'Второй proxy должен иметь цвет 1 (красный, DXF code 62)');

    { После исправления подпримитивы каждого прокси должны иметь
      vp.Color = ClByLayer (256), а не цвет владельца. Это означает,
      что при изменении цвета прокси-объекта цвет подпримитивов не
      меняется — они подчиняются цвету слоя, как и в BlockInsert. }
    for I := 0 to High(Proxies) do
    begin
      proxy := Proxies[I];
      Check(proxy^.ConstObjArray.Count > 0,
        'Каждый proxy должен построить подпримитивы после FormatEntity');

      for J := 0 to proxy^.ConstObjArray.Count - 1 do
      begin
        sub := PGDBObjEntity(proxy^.ConstObjArray.GetData(J));
        if sub = nil then
          Continue;
        CheckEquals(ClByLayer, Integer(sub^.vp.Color),
          'Подпримитив ProxyEntity с ByLayer в потоке должен иметь'
          + ' цвет ClByLayer (256), а не цвет владельца');
      end;
    end;
  finally
    drawing.done;
  end;
end;

{ Регрессия на issue #988: клонирование GDBObjCircle из ConstObjArray прокси
  с последующим вызовом transform(proxy.objMatrix) не должно портить Radius
  и Local.p_insert.

  До исправления GDBObjCircle не имел override-метода transform. Базовый
  GDBObjWithLocalCS.transform игнорировал t_matrix и вызывал
  ReCalcFromObjMatrix; при этом objMatrix содержала мусор (Clone использует
  GetMem+init без нулевой инициализации), что обнуляло Radius и искажало
  Local.p_insert. В результате круг «исчезал» после расчленения.

  Тест загружает testspds3entity2008.dxf (SPDSPOLYMORPHMARK содержит Circle),
  форматирует прокси, находит кружное подпримитив, клонирует его и применяет
  матрицу трансформации. Ожидаемый результат: Radius и позиция не равны нулю. }
procedure TProxyEntityLoadTest.ProxyExplodeCircleTransformPreservesPositionAndRadius;
var
  drawing: TSimpleDrawing;
  dc: TDrawContext;
  zdc: TZDrawingContext;
  entity: PGDBObjEntity;
  proxy: PGDBObjAcdProxy;
  sub: PGDBObjEntity;
  cloned: PGDBObjEntity;
  circle: PGDBObjCircle;
  origRadius, clonedRadius: Double;
  clonedInsert: TzePoint3d;
  transform: TzeTypedMatrix4d;
  i: Integer;
  found: Boolean;
begin
  drawing.init(nil);
  dc := drawing.CreateDrawingRC;
  try
    zdc.CreateRec(drawing, drawing.pObjRoot^, TLOLoad, dc);
    AddFromDXF('cad_source/test/testspds3entity2008.dxf', zdc);
    Check(drawing.pObjRoot^.ObjArray.Count > 0,
      'testspds3entity2008.dxf должен загружать хотя бы один объект');

    entity := PGDBObjEntity(drawing.pObjRoot^.ObjArray.GetData(0));
    Check(entity <> nil, 'Первый объект не должен быть nil');
    Check(entity^.GetObjType = GDBAcdProxyID,
      'Первый объект должен быть ProxyEntity (GDBAcdProxyID)');

    proxy := PGDBObjAcdProxy(entity);
    proxy^.FormatEntity(drawing, dc);

    Check(proxy^.ConstObjArray.Count > 0,
      'После FormatEntity proxy должен иметь подпримитивы');

    { Ищем первый кружной подпримитив }
    found := False;
    for i := 0 to proxy^.ConstObjArray.Count - 1 do
    begin
      sub := PGDBObjEntity(proxy^.ConstObjArray.GetData(i));
      if (sub <> nil) and (sub^.GetObjType = GDBCircleID) then
      begin
        circle := PGDBObjCircle(sub);
        origRadius := circle^.Radius;

        CheckTrue(origRadius > 0,
          'Исходный Radius подпримитива-круга должен быть положительным');

        { Клонируем круг и применяем трансформацию прокси (имитируем explode) }
        proxy^.CalcObjMatrix(PTDrawingDef(@drawing));
        transform := proxy^.objMatrix;
        cloned := sub^.Clone(drawing.GetCurrentROOT);
        Check(cloned <> nil, 'Clone не должен вернуть nil');

        cloned^.transform(transform);
        circle := PGDBObjCircle(cloned);
        clonedRadius := circle^.Radius;
        clonedInsert := circle^.Local.p_insert;

        CheckTrue(clonedRadius > 0,
          'После transform(proxy.objMatrix) Radius клона не должен быть равен 0 '
          + '(регрессия issue #988: мусорная objMatrix после Clone обнуляла Radius)');

        { Вектор tranlation матрицы прокси — позиция его grip center.
          После transform Local.p_insert должна содержать WCS-позицию центра
          круга, которая не должна быть нулевой, если grip center не в нуле. }
        CheckFalse(
          (abs(clonedInsert.x) < 1e-6) and
          (abs(clonedInsert.y) < 1e-6),
          'После transform(proxy.objMatrix) центр круга не должен быть в начале '
          + 'координат (регрессия issue #988: ReCalcFromObjMatrix на OneMatrix '
          + 'или мусорной objMatrix сбрасывала позицию в ноль)');

        found := True;
        Break;
      end;
    end;

    CheckTrue(found,
      'testspds3entity2008.dxf должен содержать хотя бы один Circle-подпримитив '
      + 'в ProxyEntity; убедитесь, что uzeentproxyparsercircle зарегистрирован');
  finally
    drawing.done;
  end;
end;

{ Регрессия на issue #988: клонирование GDBObjText из ConstObjArray прокси
  с последующим вызовом transform(proxy.objMatrix) не должно сбрасывать
  Local.P_insert в начало координат.

  До исправления GDBObjAbstractText.transform вызывал inherited (который
  в итоге звал ReCalcFromObjMatrix на OneMatrix — потому что GDBObjText.Clone
  использует initnul), что устанавливало Local.P_insert в NulVertex.
  В результате текст после расчленения оказывался в начале координат.

  Тест загружает testspds3entity2008.dxf, форматирует прокси, находит
  текстовый подпримитив, клонирует его и применяет матрицу трансформации.
  Ожидаемый результат: Local.P_insert не в начале координат. }
procedure TProxyEntityLoadTest.ProxyExplodeTextTransformPreservesInsertPoint;
var
  drawing: TSimpleDrawing;
  dc: TDrawContext;
  zdc: TZDrawingContext;
  entity: PGDBObjEntity;
  proxy: PGDBObjAcdProxy;
  sub: PGDBObjEntity;
  cloned: PGDBObjEntity;
  txt: PGDBObjText;
  clonedInsert: TzePoint3d;
  transform: TzeTypedMatrix4d;
  i: Integer;
  found: Boolean;
begin
  drawing.init(nil);
  dc := drawing.CreateDrawingRC;
  try
    zdc.CreateRec(drawing, drawing.pObjRoot^, TLOLoad, dc);
    AddFromDXF('cad_source/test/testspds3entity2008.dxf', zdc);
    Check(drawing.pObjRoot^.ObjArray.Count > 0,
      'testspds3entity2008.dxf должен загружать хотя бы один объект');

    entity := PGDBObjEntity(drawing.pObjRoot^.ObjArray.GetData(0));
    Check(entity <> nil, 'Первый объект не должен быть nil');
    Check(entity^.GetObjType = GDBAcdProxyID,
      'Первый объект должен быть ProxyEntity (GDBAcdProxyID)');

    proxy := PGDBObjAcdProxy(entity);
    proxy^.FormatEntity(drawing, dc);

    Check(proxy^.ConstObjArray.Count > 0,
      'После FormatEntity proxy должен иметь подпримитивы');

    { Ищем первый текстовый подпримитив }
    found := False;
    for i := 0 to proxy^.ConstObjArray.Count - 1 do
    begin
      sub := PGDBObjEntity(proxy^.ConstObjArray.GetData(i));
      if (sub <> nil) and (sub^.GetObjType = GDBtextID) then
      begin
        txt := PGDBObjText(sub);

        { Клонируем текст и применяем трансформацию прокси (имитируем explode) }
        proxy^.CalcObjMatrix(PTDrawingDef(@drawing));
        transform := proxy^.objMatrix;
        cloned := sub^.Clone(drawing.GetCurrentROOT);
        Check(cloned <> nil, 'Clone не должен вернуть nil');

        cloned^.transform(transform);
        txt := PGDBObjText(cloned);
        clonedInsert := txt^.Local.P_insert;

        { Вектор translation матрицы прокси даёт WCS-позицию grip center.
          После transform Local.P_insert должна содержать WCS-позицию текста,
          которая не должна быть нулевой, если grip center не в нуле. }
        CheckFalse(
          (abs(clonedInsert.x) < 1e-6) and
          (abs(clonedInsert.y) < 1e-6),
          'После transform(proxy.objMatrix) точка вставки текста не должна '
          + 'быть в начале координат (регрессия issue #988: '
          + 'GDBObjAbstractText.transform вызывал inherited → '
          + 'ReCalcFromObjMatrix на OneMatrix → P_insert := NulVertex)');

        found := True;
        Break;
      end;
    end;

    CheckTrue(found,
      'testspds3entity2008.dxf должен содержать хотя бы один Text-подпримитив '
      + 'в ProxyEntity; убедитесь, что uzeentproxyparsertext зарегистрирован');
  finally
    drawing.done;
  end;
end;

begin
  RegisterTests([TProxyEntityLoadTest]);
end.
