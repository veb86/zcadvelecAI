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
  Модуль: uzeentproxyparsertext
  Назначение: Автономный парсер текста из Proxy Graphic (OPCODE=10)
  
  АРХИТЕКТУРА:
  - Сам парсит данные из потока
  - Сам отрисовывает текст
  - Сам вычисляет BBox
  - Можно отключить из проекта - текст перестанет парситься
  
  Формат данных (AcGiWorldDraw OPCODE=10):
  - Position (3 doubles) - позиция вставки
  - Normal (3 doubles) - нормаль
  - Direction (3 doubles) - направление текста
  - Height (double) - высота
  - Width (double) - ширина (коэффициент)
  - Oblique (double) - угол наклона
  - Text (string) - текст (null-terminated)
}

unit uzeentproxyparsertext;
{$Mode delphi}{$H+}
{$INCLUDE zengineconfig.inc}

interface

uses
  SysUtils,
  uzeentproxystream,
  uzgldrawcontext,
  uzeTypes,
  uzeGeometryTypes,
  uzedrawingdef,
  uzegeometry;

type
  { Результат парсинга текста }
  TProxyTextParseResult = record
    Valid: Boolean;
    Insert: TzePoint3d;       { Позиция вставки }
    Normal: TzePoint3d;       { Нормаль }
    Direction: TzePoint3d;    { Направление текста }
    Height: Double;           { Высота текста }
    WidthFactor: Double;      { Коэффициент ширины }
    ObliqueAngle: Double;     { Угол наклона }
    Text: string;             { Текст }
    BBoxMin: TzePoint3d;
    BBoxMax: TzePoint3d;
    HasBBox: Boolean;
  end;

  { Автономный парсер текста }
  TProxyTextParser = class
  public
    { Парсит и отрисовывает текст (OPCODE=10 - Text1) }
    class procedure ParseAndDraw(Stream: TProxyByteStream; 
      var Drawing: TDrawingDef;
      var DC: TDrawContext; 
      out ParseResult: TProxyTextParseResult);
      
    { Парсит и отрисовывает Unicode Text v2 (OPCODE=38 - UnicodeText2) }
    class procedure ParseUnicodeText2(Stream: TProxyByteStream; 
      var Drawing: TDrawingDef;
      var DC: TDrawContext; 
      out ParseResult: TProxyTextParseResult);
  end;

implementation

uses
  uzcLog,
  uzeenttext;

const
  PROXY_X_AXIS: TzePoint3d = (x: 1.0; y: 0.0; z: 0.0);
  PROXY_Y_AXIS: TzePoint3d = (x: 0.0; y: 1.0; z: 0.0);
  PROXY_Z_AXIS: TzePoint3d = (x: 0.0; y: 0.0; z: 1.0);

{ Вспомогательная функция для проверки близости векторов }
function VectorIsClose(const V1, V2: TzePoint3d; const Epsilon: Double = 1e-9): Boolean;
begin
  Result := (Abs(V1.x - V2.x) <= Epsilon) and
            (Abs(V1.y - V2.y) <= Epsilon) and
            (Abs(V1.z - V2.z) <= Epsilon);
end;

{ Вспомогательная функция для вычисления векторного произведения }
function CrossProduct(const V1, V2: TzePoint3d): TzePoint3d;
begin
  Result.x := V1.y * V2.z - V1.z * V2.y;
  Result.y := V1.z * V2.x - V1.x * V2.z;
  Result.z := V1.x * V2.y - V1.y * V2.x;
end;

{ Преобразование точки в OCS }
function TransformToOCS(const Point, Normal: TzePoint3d): TzePoint3d;
var
  XAxis, YAxis, ZAxis: TzePoint3d;
begin
  ZAxis := NormalizeVertex(Normal);
  
  if Abs(ZAxis.x) < 0.9 then
    XAxis := NormalizeVertex(PROXY_X_AXIS * ZAxis.z - ZAxis * PROXY_X_AXIS.z)
  else
    XAxis := NormalizeVertex(PROXY_Y_AXIS * ZAxis.z - ZAxis * PROXY_Y_AXIS.z);
    
  YAxis := NormalizeVertex(ZAxis * XAxis.x - XAxis * ZAxis.x);
  
  Result.x := scalarDot(Point, XAxis);
  Result.y := scalarDot(Point, YAxis);
  Result.z := scalarDot(Point, ZAxis);
end;

{ Парсит и отрисовывает текст }
class procedure TProxyTextParser.ParseAndDraw(Stream: TProxyByteStream; 
  var Drawing: TDrawingDef;
  var DC: TDrawContext; 
  out ParseResult: TProxyTextParseResult);
var
  Insert: TzePoint3d;
  Normal: TzePoint3d;
  Direction: TzePoint3d;
  Height, WidthFactor, ObliqueAngle: Double;
begin
  ParseResult.Valid := False;
  ParseResult.HasBBox := False;
  ParseResult.Text := '';
  
  try
    programlog.LogOutFormatStr('uzeentproxyparsertext: ParseAndDraw START', [], LM_Info);
    
    { ========================================================================
      ФОРМАТ ДАННЫХ TEXT (OPCODE=10 - kAcGiOpText1)
      Источник: AutoCAD DevBlog - Proxy Graphic Binary Chunk Interpretation
                ezdxf/proxygraphic.py - метод text()
      
      Структура данных (AcGiWorldDraw):
      ┌─────────────────────────────────────────────────────────────────────┐
      │ Offset  │ Size    │ Type          │ Описание                        │
      ├─────────┼─────────┼───────────────┼─────────────────────────────────┤
      │ 0       │ 24      │ AcGePoint3d   │ Position (Insert) - позиция     │
      │         │         │ (3 doubles)   │ вставки текста                  │
      ├─────────┼─────────┼───────────────┼─────────────────────────────────┤
      │ 24      │ 24      │ AcGeVector3d  │ Normal - нормаль (ось Z         │
      │         │         │ (3 doubles)   │ локальной СК)                   │
      ├─────────┼─────────┼───────────────┼─────────────────────────────────┤
      │ 48      │ 24      │ AcGeVector3d  │ Direction - направление текста  │
      │         │         │ (3 doubles)   │ (вектор вдоль базовой линии)    │
      ├─────────┼─────────┼───────────────┼─────────────────────────────────┤
      │ 72      │ 8       │ double        │ Height - высота текста          │
      ├─────────┼─────────┼───────────────┼─────────────────────────────────┤
      │ 80      │ 8       │ double        │ WidthFactor - коэффициент       │
      │         │         │               │ ширины (масштаб по X)           │
      ├─────────┼─────────┼───────────────┼─────────────────────────────────┤
      │ 88      │ 8       │ double        │ ObliqueAngle - угол наклона     │
      │         │         │               │ (в радианах, положительный -    │
      │         │         │               │ наклон вправо)                  │
      ├─────────┼─────────┼───────────────┼─────────────────────────────────┤
      │ 96      │ var     │ char*         │ Text - строка текста            │
      │         │         │ (null-        │ (ANSI encoding, terminated      │
      │         │         │ terminated)   │ нулевым байтом 0x00)            │
      └─────────────────────────────────────────────────────────────────────┘
      
      Общий размер заголовка: 96 байт + длина строки текста
      
      Пример данных (hex):
      00 00 00 00 00 00 F0 3F  ...  // Position.x = 1.0
      00 00 00 00 00 00 00 40  ...  // Position.y = 2.0
      00 00 00 00 00 00 00 00  ...  // Position.z = 0.0
      00 00 00 00 00 00 00 00  ...  // Normal = (0, 0, 1)
      ...
      00 00 00 00 00 00 F0 3F  ...  // Direction = (1, 0, 0)
      00 00 00 00 00 00 00 40  ...  // Height = 2.5
      00 00 00 00 00 00 F0 3F  ...  // WidthFactor = 1.0
      00 00 00 00 00 00 00 00  ...  // ObliqueAngle = 0.0
      48 65 6C 6C 6F 00              // Text = "Hello\0"
      ======================================================================== }
    
    { 1. Читаем данные: Position + Normal + Direction + Height + Width + Oblique + Text }
    Insert := Stream.ReadVertex;      { 24 байта: Position (X, Y, Z) }
    Normal := Stream.ReadVector;      { 24 байта: Normal (X, Y, Z) }
    Direction := Stream.ReadVector;   { 24 байта: Direction (X, Y, Z) }
    Height := Stream.ReadDouble;      { 8 байт: Height }
    WidthFactor := Stream.ReadDouble; { 8 байт: WidthFactor }
    ObliqueAngle := Stream.ReadDouble;{ 8 байт: ObliqueAngle }
    ParseResult.Text := Stream.ReadString(TEncoding.ANSI); { var байт: Text + null terminator }
    
    programlog.LogOutFormatStr('uzeentproxyparsertext: Read Insert=(%.6f,%.6f,%.6f) Height=%.6f Text="%s"', 
      [Insert.x, Insert.y, Insert.z, Height, ParseResult.Text], LM_Info);
    
    { 2. Проверяем валидность }
    if (Height <= 0) or (ParseResult.Text = '') then
    begin
      programlog.LogOutFormatStr('uzeentproxyparsertext: Invalid height %.6f or empty text, skipping', [Height], LM_Warning);
      Exit;
    end;
    
    { 3. Преобразуем в OCS если нормаль не Z }
    if not VectorIsClose(Normal, PROXY_Z_AXIS) then
      Insert := TransformToOCS(Insert, Normal);
    
    { 4. Вычисляем BBox (примерно) }
    ParseResult.BBoxMin := CreateVertex(
      Insert.x, 
      Insert.y, 
      Insert.z);
    ParseResult.BBoxMax := CreateVertex(
      Insert.x + Length(ParseResult.Text) * Height * WidthFactor, 
      Insert.y + Height, 
      Insert.z + Height);
    ParseResult.HasBBox := True;
    
    { 5. Сохраняем результат }
    ParseResult.Insert := Insert;
    ParseResult.Normal := Normal;
    ParseResult.Direction := Direction;
    ParseResult.Height := Height;
    ParseResult.WidthFactor := WidthFactor;
    ParseResult.ObliqueAngle := ObliqueAngle;
    ParseResult.Valid := True;

    { 6. TODO: Отрисовка текста будет реализована отдельно }
    { Пока просто логируем }
    programlog.LogOutFormatStr('uzeentproxyparsertext: Text parsed (rendering not yet implemented)', [], LM_Info);
    
    programlog.LogOutFormatStr('uzeentproxyparsertext: ParseAndDraw COMPLETE - Insert=(%.3f,%.3f,%.3f) Text="%s"', 
      [ParseResult.Insert.x, ParseResult.Insert.y, ParseResult.Insert.z, ParseResult.Text], LM_Info);
    
  except
    on E: Exception do
    begin
      ParseResult.Valid := False;
      programlog.LogOutFormatStr('uzeentproxyparsertext: ParseAndDraw EXCEPTION: %s', [E.Message], LM_Error);
    end;
  end;
end;

{ ============================================================================
  PARSE UNICODE TEXT V2 (OPCODE=38)
  Формат данных (AcGiWorldDraw):
  - Position (3 doubles) - позиция вставки
  - Normal (3 doubles) - нормаль
  - Direction (3 doubles) - направление текста
  - Text (Unicode string) - текст (UTF-16, null-terminated)
  - IgnoreLength (int32) - игнорируемая длина
  - Raw (int32) - raw флаг
  - Height (double) - высота
  - WidthFactor (double) - ширина
  - ObliqueAngle (double) - угол наклона
  - TrackingPercentage (double) - трекинг
  - IsBackward (uint32) - флаг назад
  - IsUpsideDown (uint32) - флаг вверх ногами
  - IsVertical (uint32) - флаг вертикально
  - IsUnderlined (uint32) - флаг подчёркнутый
  - IsOverlined (uint32) - флаг надчёркнутый
  - FontName (string) - имя шрифта
  - BigFontName (string) - имя большого шрифта
  ============================================================================ }
class procedure TProxyTextParser.ParseUnicodeText2(Stream: TProxyByteStream; 
  var Drawing: TDrawingDef;
  var DC: TDrawContext; 
  out ParseResult: TProxyTextParseResult);
var
  Insert: TzePoint3d;
  Normal: TzePoint3d;
  Direction: TzePoint3d;
  Height, WidthFactor, ObliqueAngle: Double;
  IgnoreLength, Raw: Integer;
  IsBackward, IsUpsideDown, IsVertical, IsUnderlined, IsOverlined: Cardinal;
  TrackingPercentage: Double;
  FontName, BigFontName: string;
begin
  ParseResult.Valid := False;
  ParseResult.HasBBox := False;
  ParseResult.Text := '';
  
  try
    programlog.LogOutFormatStr('uzeentproxyparsertext: ParseUnicodeText2 START', [], LM_Info);
    
    { Читаем данные Unicode Text v2 }
    Insert := Stream.ReadVertex;           { 24 байта: Position }
    Normal := Stream.ReadVector;           { 24 байта: Normal }
    Direction := Stream.ReadVector;        { 24 байта: Direction }
    
    { Читаем Unicode текст }
    try
      ParseResult.Text := Stream.ReadPaddedUnicodeString;
    except
      ParseResult.Text := '[Unicode decode error]';
    end;
    
    IgnoreLength := Stream.ReadInt32;      { 4 байта: игнорируемая длина }
    Raw := Stream.ReadInt32;               { 4 байта: raw флаг }
    
    Height := Stream.ReadDouble;           { 8 байт: высота }
    WidthFactor := Stream.ReadDouble;      { 8 байт: ширина }
    ObliqueAngle := Stream.ReadDouble;     { 8 байт: угол наклона }
    TrackingPercentage := Stream.ReadDouble; { 8 байт: трекинг }
    
    IsBackward := Stream.ReadUInt32;       { 4 байта: флаг назад }
    IsUpsideDown := Stream.ReadUInt32;     { 4 байта: флаг вверх ногами }
    IsVertical := Stream.ReadUInt32;       { 4 байта: флаг вертикально }
    IsUnderlined := Stream.ReadUInt32;     { 4 байта: флаг подчёркнутый }
    IsOverlined := Stream.ReadUInt32;      { 4 байта: флаг надчёркнутый }
    
    FontName := Stream.ReadString(TEncoding.ANSI);      { var байт: имя шрифта }
    BigFontName := Stream.ReadString(TEncoding.ANSI);   { var байт: имя большого шрифта }
    
    programlog.LogOutFormatStr('uzeentproxyparsertext: Read Insert=(%.6f,%.6f,%.6f) Height=%.6f Text="%s" Font="%s"', 
      [Insert.x, Insert.y, Insert.z, Height, ParseResult.Text, FontName], LM_Info);
    
    { Проверяем валидность }
    if (Height <= 0) or (ParseResult.Text = '') then
    begin
      programlog.LogOutFormatStr('uzeentproxyparsertext: Invalid height %.6f or empty text, skipping', [Height], LM_Warning);
      Exit;
    end;
    
    { Преобразуем в OCS если нормаль не Z }
    if not VectorIsClose(Normal, PROXY_Z_AXIS) then
      Insert := TransformToOCS(Insert, Normal);
    
    { Вычисляем BBox }
    ParseResult.BBoxMin := CreateVertex(
      Insert.x, 
      Insert.y, 
      Insert.z);
    ParseResult.BBoxMax := CreateVertex(
      Insert.x + Length(ParseResult.Text) * Height * WidthFactor, 
      Insert.y + Height, 
      Insert.z + Height);
    ParseResult.HasBBox := True;
    
    { Сохраняем результат }
    ParseResult.Insert := Insert;
    ParseResult.Normal := Normal;
    ParseResult.Direction := Direction;
    ParseResult.Height := Height;
    ParseResult.WidthFactor := WidthFactor;
    ParseResult.ObliqueAngle := ObliqueAngle;
    ParseResult.Valid := True;
    
    { TODO: Отрисовка текста будет реализована отдельно }
    { Пока просто вычисляем BBox для отображения габаритной рамки }
    programlog.LogOutFormatStr('uzeentproxyparsertext: Text parsed (rendering not yet implemented)', [], LM_Info);
    
    { Вычисляем BBox }
    ParseResult.BBoxMin := CreateVertex(
      Insert.x, 
      Insert.y, 
      Insert.z);
    ParseResult.BBoxMax := CreateVertex(
      Insert.x + Length(ParseResult.Text) * Height * WidthFactor, 
      Insert.y + Height, 
      Insert.z + Height);
    ParseResult.HasBBox := True;
    
    programlog.LogOutFormatStr('uzeentproxyparsertext: ParseUnicodeText2 COMPLETE - Insert=(%.3f,%.3f,%.3f) Text="%s" BBox=(%.3f,%.3f,%.3f)-(%.3f,%.3f,%.3f)', 
      [ParseResult.Insert.x, ParseResult.Insert.y, ParseResult.Insert.z, 
       ParseResult.Text,
       ParseResult.BBoxMin.x, ParseResult.BBoxMin.y, ParseResult.BBoxMin.z,
       ParseResult.BBoxMax.x, ParseResult.BBoxMax.y, ParseResult.BBoxMax.z], LM_Info);
    
  except
    on E: Exception do
    begin
      ParseResult.Valid := False;
      programlog.LogOutFormatStr('uzeentproxyparsertext: ParseUnicodeText2 EXCEPTION: %s', [E.Message], LM_Error);
    end;
  end;
end;

end.
