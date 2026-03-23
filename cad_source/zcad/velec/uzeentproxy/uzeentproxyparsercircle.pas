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
  Модуль: uzeentproxyparsercircle
  Назначение: Автономный парсер круга из Proxy Graphic (OPCODE=2)
  
  АРХИТЕКТУРА:
  - Сам парсит данные из потока
  - Сам отрисовывает круг
  - Сам вычисляет BBox
  - Можно отключить из проекта - круги перестанут парситься
  
  Формат данных (AcGiWorldDraw):
  - Center (3 doubles) - центр круга
  - Radius (1 double) - радиус
  - Normal (3 doubles) - нормаль (ось Z локальной СК)
}

unit uzeentproxyparsercircle;
{$Mode delphi}{$H+}
{$INCLUDE zengineconfig.inc}

interface

uses
  SysUtils,
  uzeentproxystream,
  uzgldrawcontext,
  uzeTypes,
  uzeGeometryTypes,
  UGDBPoint3DArray,
  uzedrawingdef,
  gzctnrVectorTypes,
  uzegeometry;

type
  { Результат парсинга круга }
  TProxyCircleParseResult = record
    Valid: Boolean;
    Center: TzePoint3d;
    Radius: Double;
    Normal: TzePoint3d;
    BBoxMin: TzePoint3d;
    BBoxMax: TzePoint3d;
    HasBBox: Boolean;
    CircleVertices: GDBPoint3DArray;  { Вершины для отрисовки }
    HasCircleVertices: Boolean;
  end;

  { Автономный парсер круга }
  TProxyCircleParser = class
  public
    { Парсит и отрисовывает круг }
    class procedure ParseAndDraw(Stream: TProxyByteStream; 
      var Drawing: TDrawingDef;
      var DC: TDrawContext; 
      out ParseResult: TProxyCircleParseResult);
  end;

implementation

uses
  uzcLog;

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

{ Парсит и отрисовывает круг }
class procedure TProxyCircleParser.ParseAndDraw(Stream: TProxyByteStream; 
  var Drawing: TDrawingDef;
  var DC: TDrawContext; 
  out ParseResult: TProxyCircleParseResult);
var
  Center: TzePoint3d;
  Radius: Double;
  Normal: TzePoint3d;
  I, SegCount: Integer;
  Angle: Double;
  Pt: TzePoint3d;
  VertexArray: GDBPoint3DArray;
  ir: itrec;
  pV: PzePoint3d;
begin
  ParseResult.Valid := False;
  ParseResult.HasBBox := False;
  
  try
    programlog.LogOutFormatStr('uzeentproxyparsercircle: ParseAndDraw START', [], LM_Info);
    
    { 1. Читаем данные: Center + Radius + Normal }
    Center := Stream.ReadVertex;
    Radius := Stream.ReadDouble;
    Normal := Stream.ReadVector;
    
    programlog.LogOutFormatStr('uzeentproxyparsercircle: Read Center=(%.6f,%.6f,%.6f) Radius=%.6f', 
      [Center.x, Center.y, Center.z, Radius], LM_Info);
    
    { 2. Проверяем валидность }
    if Radius <= 0 then
    begin
      programlog.LogOutFormatStr('uzeentproxyparsercircle: Invalid radius %.6f, skipping', [Radius], LM_Warning);
      Exit;
    end;
    
    { 3. Преобразуем в OCS если нормаль не Z }
    if not VectorIsClose(Normal, PROXY_Z_AXIS) then
      Center := TransformToOCS(Center, Normal);
    
    { 4. Вычисляем BBox }
    ParseResult.BBoxMin := CreateVertex(Center.x - Radius, Center.y - Radius, Center.z);
    ParseResult.BBoxMax := CreateVertex(Center.x + Radius, Center.y + Radius, Center.z);
    ParseResult.HasBBox := True;
    
    { 5. Сохраняем результат }
    ParseResult.Center := Center;
    ParseResult.Radius := Radius;
    ParseResult.Normal := Normal;
    ParseResult.Valid := True;
    
    { 6. Тесселируем круг в вершины и сохраняем в результат }
    SegCount := 64;
    ParseResult.CircleVertices.init(SegCount);
    
    for I := 0 to SegCount - 1 do
    begin
      Angle := (I / SegCount) * 2 * Pi;
      Pt := CreateVertex(
        Center.x + Radius * Cos(Angle),
        Center.y + Radius * Sin(Angle),
        Center.z
      );
      ParseResult.CircleVertices.PushBackData(Pt);
    end;
    
    ParseResult.HasCircleVertices := True;
    
    programlog.LogOutFormatStr('uzeentproxyparsercircle: Tessellated circle with %d vertices', [SegCount], LM_Info);
    programlog.LogOutFormatStr('uzeentproxyparsercircle: ParseAndDraw COMPLETE - Center=(%.3f,%.3f,%.3f) Radius=%.3f', 
      [ParseResult.Center.x, ParseResult.Center.y, ParseResult.Center.z, ParseResult.Radius], LM_Info);
    
  except
    on E: Exception do
    begin
      ParseResult.Valid := False;
      programlog.LogOutFormatStr('uzeentproxyparsercircle: ParseAndDraw EXCEPTION: %s', [E.Message], LM_Error);
    end;
  end;
end;

end.
