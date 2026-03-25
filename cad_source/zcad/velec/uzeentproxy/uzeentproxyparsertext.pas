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
  Модуль: uzeentproxyparsertext
  Назначение: Парсер текстовых примитивов для прокси-объектов.
              Обрабатывает два вида текста:
              - OpCode = 10 (pgcText)   — ANSI строка (kAcGiOpText1)
              - OpCode = 38 (pgcUnicodeText2) — Unicode строка с расширенными атрибутами

  Архитектура:
  - Секция initialization регистрирует обработчики в TProxyOpCodeDispatcher
  - Для отключения парсинга текста достаточно исключить этот файл из проекта

  Формат OpCode=10 (Text1):
    Position  — 3 × double (позиция вставки)
    Normal    — 3 × double (нормаль)
    Direction — 3 × double (направление текста)
    Height    — double (высота символов)
    WidthFactor — double (масштаб по ширине)
    ObliqueAngle — double (угол наклона, радианы)
    Text      — null-terminated ANSI строка

  Формат OpCode=38 (UnicodeText2):
    Position  — 3 × double
    Normal    — 3 × double
    Direction — 3 × double
    Text      — UTF-16 null-terminated строка (выровнена по 4 байтам)
    IgnoreLen — int32
    Raw       — int32
    Height    — double
    WidthFactor — double
    ObliqueAngle — double
    TrackingPercentage — double
    IsBackward, IsUpsideDown, IsVertical, IsUnderlined, IsOverlined — uint32
    FontName  — null-terminated ANSI
    BigFontName — null-terminated ANSI

  Текущая реализация:
  - BBox вычисляется аппроксимационно: ширина = len(text) × height × wfactor
  - Рендер текста пока не реализован (TODO: интеграция с GDBObjMText/GDBObjText)
  - HandlerResult.HasVertices = False (текст не тесселируется в полилинию)
}

unit uzeentproxyparsertext;
{$Mode delphi}{$H+}
{$INCLUDE zengineconfig.inc}

interface

{ Регистрация происходит автоматически в секции initialization }

implementation

uses
  SysUtils,
  uzeentproxystream,
  uzeentproxymanager,
  uzegeometrytypes,
  uzegeometry,
  UGDBPoint3DArray,
  uzcLog;

const
  { OpCode ANSI-текста }
  TEXT_OPCODE         = 10;
  { OpCode расширенного Unicode-текста }
  UNICODE_TEXT2_OPCODE = 38;

  { Порог параллельности для алгоритма произвольной оси }
  TEXT_AXIS_THRESHOLD = 0.9;

  { Нормаль WCS по оси Z }
  TEXT_Z_AXIS: TzePoint3d = (x: 0.0; y: 0.0; z: 1.0);

{ --- Вспомогательные процедуры --- }

function VectorsAreEqual(const V1, V2: TzePoint3d;
  const Epsilon: Double = 1e-9): Boolean;
begin
  Result := (Abs(V1.x - V2.x) <= Epsilon)
    and (Abs(V1.y - V2.y) <= Epsilon)
    and (Abs(V1.z - V2.z) <= Epsilon);
end;

{ Переводит точку из WCS в OCS по нормали Normal }
function TransformPointToOCS(const Point, Normal: TzePoint3d): TzePoint3d;
const
  AuxX: TzePoint3d = (x: 1.0; y: 0.0; z: 0.0);
  AuxY: TzePoint3d = (x: 0.0; y: 1.0; z: 0.0);
var
  ZAxis, XAxis, YAxis: TzePoint3d;
begin
  ZAxis := NormalizeVertex(Normal);

  if Abs(ZAxis.x) < TEXT_AXIS_THRESHOLD then
    XAxis := NormalizeVertex(AuxX * ZAxis.z - ZAxis * AuxX.z)
  else
    XAxis := NormalizeVertex(AuxY * ZAxis.z - ZAxis * AuxY.z);

  YAxis := NormalizeVertex(ZAxis * XAxis.x - XAxis * ZAxis.x);

  Result.x := scalarDot(Point, XAxis);
  Result.y := scalarDot(Point, YAxis);
  Result.z := scalarDot(Point, ZAxis);
end;

{ Вычисляет аппроксимационный BBox текста.
  Ширина = количество символов × высота × коэффициент ширины. }
procedure CalcTextBBox(const Insert: TzePoint3d;
  const Text: string; const Height, WidthFactor: Double;
  out BBoxMin, BBoxMax: TzePoint3d);
var
  TextWidth: Double;
begin
  TextWidth := Length(Text) * Height * WidthFactor;
  BBoxMin.x := Insert.x;
  BBoxMin.y := Insert.y;
  BBoxMin.z := Insert.z;
  BBoxMax.x := Insert.x + TextWidth;
  BBoxMax.y := Insert.y + Height;
  BBoxMax.z := Insert.z + Height;
end;

{ --- Обработчики OpCode --- }

{ Читает текстовый примитив формата OpCode=10 (ANSI Text1).
  Возвращает BBox; вершины не генерируются (рендер текста — TODO). }
procedure HandleText(
  Stream: TProxyByteStream;
  out HandlerResult: TProxyHandlerResult);
var
  Insert, Normal: TzePoint3d;
  Height, WidthFactor: Double;
  Text: string;
begin
  HandlerResult.Valid := False;
  HandlerResult.HasVertices := False;
  HandlerResult.HasBBox := False;

  { Читаем геометрию }
  Insert := Stream.ReadVertex;
  Normal := Stream.ReadVector;
  Stream.ReadVector;    { Direction — пока не используется }

  Height := Stream.ReadDouble;
  WidthFactor := Stream.ReadDouble;
  Stream.ReadDouble;    { ObliqueAngle — пока не используется }

  Text := Stream.ReadString(TEncoding.ANSI);

  programlog.LogOutFormatStr(
    'uzeentproxyparsertext: Text1 Insert=(%.3f,%.3f,%.3f) H=%.3f Text="%s"',
    [Insert.x, Insert.y, Insert.z, Height, Text], LM_Info);

  if (Height <= 0) or (Text = '') then
    Exit;

  { Переводим в OCS если нормаль не совпадает с Z }
  if not VectorsAreEqual(Normal, TEXT_Z_AXIS) then
    Insert := TransformPointToOCS(Insert, Normal);

  { Аппроксимационный BBox }
  CalcTextBBox(Insert, Text, Height, WidthFactor,
    HandlerResult.BBoxMin, HandlerResult.BBoxMax);
  HandlerResult.HasBBox := True;
  HandlerResult.Valid := True;

  { TODO: тесселировать или создать GDBObjText для отрисовки }
end;

{ Читает расширенный Unicode-текст формата OpCode=38 (UnicodeText2).
  Возвращает BBox; вершины не генерируются (рендер текста — TODO). }
procedure HandleUnicodeText2(
  Stream: TProxyByteStream;
  out HandlerResult: TProxyHandlerResult);
var
  Insert, Normal: TzePoint3d;
  Height, WidthFactor: Double;
  Text, FontName: string;
begin
  HandlerResult.Valid := False;
  HandlerResult.HasVertices := False;
  HandlerResult.HasBBox := False;

  { Геометрия }
  Insert := Stream.ReadVertex;
  Normal := Stream.ReadVector;
  Stream.ReadVector;   { Direction — пока не используется }

  { Текст (UTF-16, выровнен по 4 байтам) }
  try
    Text := Stream.ReadPaddedUnicodeString;
  except
    Text := '';
  end;

  { Пропускаем IgnoreLength и Raw }
  Stream.ReadInt32;
  Stream.ReadInt32;

  Height := Stream.ReadDouble;
  WidthFactor := Stream.ReadDouble;

  { Пропускаем: ObliqueAngle, TrackingPercentage }
  Stream.ReadDouble;
  Stream.ReadDouble;

  { Пропускаем флаги: IsBackward, IsUpsideDown, IsVertical,
    IsUnderlined, IsOverlined }
  Stream.ReadUInt32;
  Stream.ReadUInt32;
  Stream.ReadUInt32;
  Stream.ReadUInt32;
  Stream.ReadUInt32;

  FontName := Stream.ReadString(TEncoding.ANSI);
  Stream.ReadString(TEncoding.ANSI); { BigFontName — пока не используется }

  programlog.LogOutFormatStr(
    'uzeentproxyparsertext: UnicodeText2 Insert=(%.3f,%.3f,%.3f) H=%.3f' +
    ' Text="%s" Font="%s"',
    [Insert.x, Insert.y, Insert.z, Height, Text, FontName], LM_Info);

  if (Height <= 0) or (Text = '') then
    Exit;

  { Переводим в OCS если нормаль не совпадает с Z }
  if not VectorsAreEqual(Normal, TEXT_Z_AXIS) then
    Insert := TransformPointToOCS(Insert, Normal);

  { Аппроксимационный BBox }
  CalcTextBBox(Insert, Text, Height, WidthFactor,
    HandlerResult.BBoxMin, HandlerResult.BBoxMax);
  HandlerResult.HasBBox := True;
  HandlerResult.Valid := True;

  { TODO: интеграция с GDBObjMText для отрисовки текста }
end;

initialization
  { Регистрируем оба обработчика текста.
    Исключение этого файла из проекта полностью отключает парсинг
    текстовых примитивов внутри прокси-объектов. }
  TProxyOpCodeDispatcher.RegisterOpCode(
    TEXT_OPCODE,
    'Text1 (ANSI)',
    @HandleText);

  TProxyOpCodeDispatcher.RegisterOpCode(
    UNICODE_TEXT2_OPCODE,
    'UnicodeText2',
    @HandleUnicodeText2);

end.
