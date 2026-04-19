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
  Модуль: uzeentproxyparsercircle
  Назначение: Парсер круга (OpCode=2) для примитивов внутри Proxy объектов.

  Архитектура:
  - Секция initialization регистрирует HandleCircle в TProxyOpCodeDispatcher
  - Чтобы отключить парсинг кругов — исключить этот файл из проекта
  - Изменений в главном модуле uzeentacdproxy.pas не требуется

  Формат данных (AcGiWorldDraw, OpCode = 2 = pgcCircle):
    Center  — 3 × double (24 байта) — центр в WCS
    Radius  — 1 × double (8 байт)  — радиус
    Normal  — 3 × double (24 байта) — нормаль (ось Z локальной СК)

  Тесселяция:
  - Круг аппроксимируется CIRCLE_SEGMENT_COUNT отрезками
  - Вершины сохраняются в TProxyHandlerResult.Vertices
  - Отрисовка выполняется в GDBObjAcdProxy.FormatEntity через
    Representation.DrawPolyLineWithLT
}

unit uzeentproxyparsercircle;
{$Mode delphi}{$H+}
{$INCLUDE zengineconfig.inc}

interface

{ Публичный интерфейс не нужен — регистрация происходит автоматически
  при загрузке модуля через секцию initialization }

implementation

uses
  SysUtils,
  Math,
  uzeentproxystream,
  uzeentproxymanager,
  uzeentproxysubentitybuilder,
  uzegeometrytypes,
  uzegeometry,
  UGDBPoint3DArray,
  uzcLog;

const
  { OpCode круга в формате AcGiWorldDraw }
  CIRCLE_OPCODE = 2;

  { Количество отрезков тесселяции окружности }
  CIRCLE_SEGMENT_COUNT = 64;

  { Нормаль по умолчанию совпадает с осью Z WCS }
  CIRCLE_Z_AXIS: TzePoint3d = (x: 0.0; y: 0.0; z: 1.0);

  { Порог параллельности для выбора вспомогательной оси }
  CIRCLE_AXIS_THRESHOLD = 0.9;

{ --- Вспомогательные процедуры --- }

{ Проверяет, совпадают ли два вектора с точностью Epsilon }
function VectorsAreEqual(const V1, V2: TzePoint3d;
  const Epsilon: Double = 1e-9): Boolean;
begin
  Result := (Abs(V1.x - V2.x) <= Epsilon)
    and (Abs(V1.y - V2.y) <= Epsilon)
    and (Abs(V1.z - V2.z) <= Epsilon);
end;

{ Вычисляет BBox круга в плоскости XY с учётом позиции Center и Radius }
procedure CalcCircleBBox(const Center: TzePoint3d; const Radius: Double;
  out BBoxMin, BBoxMax: TzePoint3d);
begin
  BBoxMin.x := Center.x - Radius;
  BBoxMin.y := Center.y - Radius;
  BBoxMin.z := Center.z;
  BBoxMax.x := Center.x + Radius;
  BBoxMax.y := Center.y + Radius;
  BBoxMax.z := Center.z;
end;

{ Тесселирует окружность в массив вершин.
  Center — центр, Radius — радиус в плоскости XY (Z = Center.z). }
procedure TessellateCircle(const Center: TzePoint3d; const Radius: Double;
  var Vertices: GDBPoint3DArray);
var
  I: Integer;
  Angle: Double;
  Pt: TzePoint3d;
begin
  Vertices.init(CIRCLE_SEGMENT_COUNT);
  for I := 0 to CIRCLE_SEGMENT_COUNT - 1 do
  begin
    Angle := (I / CIRCLE_SEGMENT_COUNT) * 2 * Pi;
    Pt.x := Center.x + Radius * Cos(Angle);
    Pt.y := Center.y + Radius * Sin(Angle);
    Pt.z := Center.z;
    Vertices.PushBackData(Pt);
  end;
end;

{ Преобразует точку Point из WCS в OCS по нормали Normal.
  Использует алгоритм произвольной оси AutoCAD. }
function TransformPointToOCS(const Point, Normal: TzePoint3d): TzePoint3d;
const
  AuxX: TzePoint3d = (x: 1.0; y: 0.0; z: 0.0);
  AuxY: TzePoint3d = (x: 0.0; y: 1.0; z: 0.0);
var
  ZAxis, XAxis, YAxis: TzePoint3d;
begin
  ZAxis := NormalizeVertex(Normal);

  { Выбираем вспомогательную ось для построения OCS }
  if Abs(ZAxis.x) < CIRCLE_AXIS_THRESHOLD then
    XAxis := NormalizeVertex(AuxX * ZAxis.z - ZAxis * AuxX.z)
  else
    XAxis := NormalizeVertex(AuxY * ZAxis.z - ZAxis * AuxY.z);

  YAxis := NormalizeVertex(ZAxis * XAxis.x - XAxis * ZAxis.x);

  { Проекция точки на оси OCS }
  Result.x := scalarDot(Point, XAxis);
  Result.y := scalarDot(Point, YAxis);
  Result.z := scalarDot(Point, ZAxis);
end;

{ --- Обработчик OpCode --- }

{ Читает данные круга из потока, вычисляет BBox и тесселирует контур.
  Регистрируется в TProxyOpCodeDispatcher как обработчик OpCode=2. }
procedure HandleCircle(
  Stream: TProxyByteStream;
  out HandlerResult: TProxyHandlerResult);
var
  Center: TzePoint3d;
  Radius: Double;
  Normal: TzePoint3d;
begin
  HandlerResult.Valid := False;
  HandlerResult.HasVertices := False;
  HandlerResult.HasBBox := False;

  { Читаем: Center (24 байта) + Radius (8 байт) + Normal (24 байта) }
  Center := Stream.ReadVertex;
  Radius := Stream.ReadDouble;
  Normal := Stream.ReadVector;

  programlog.LogOutFormatStr(
    'uzeentproxyparsercircle: Center=(%.4f,%.4f,%.4f) Radius=%.4f',
    [Center.x, Center.y, Center.z, Radius], LM_Info);

  { Радиус должен быть положительным }
  if Radius <= 0 then
  begin
    programlog.LogOutFormatStr(
      'uzeentproxyparsercircle: Radius=%.4f is invalid, skipping',
      [Radius], LM_Info);
    Exit;
  end;

  { Если нормаль отличается от Z, переводим центр в OCS }
  if not VectorsAreEqual(Normal, CIRCLE_Z_AXIS) then
    Center := TransformPointToOCS(Center, Normal);

  { Вычисляем BBox }
  CalcCircleBBox(Center, Radius, HandlerResult.BBoxMin, HandlerResult.BBoxMax);
  HandlerResult.HasBBox := True;

  { Тесселируем контур окружности }
  TessellateCircle(Center, Radius, HandlerResult.Vertices);
  HandlerResult.HasVertices := True;
  HandlerResult.Closed := True;

  HandlerResult.Valid := True;

  programlog.LogOutFormatStr(
    'uzeentproxyparsercircle: OK, %d vertices, BBox=(%.3f,%.3f)-(%.3f,%.3f)',
    [HandlerResult.Vertices.Count,
     HandlerResult.BBoxMin.x, HandlerResult.BBoxMin.y,
     HandlerResult.BBoxMax.x, HandlerResult.BBoxMax.y], LM_Info);
end;

{ --- Построитель подпримитивов --- }

{ Создаёт подпримитивы-отрезки (GDBObjLine) из тесселированных вершин
  окружности. Круг — замкнутый контур, поэтому строится и замыкающий
  отрезок (последняя → первая вершина).
  Если над окружностью была активна заливка — строятся подпримитивы
  GDBObjSolid через триангуляцию веером. }
procedure BuildCircleSubEntities(
  const HandlerResult: TProxyHandlerResult;
  const Context: TProxySubEntityContext);
begin
  if not HandlerResult.HasVertices then
    Exit;

  if HandlerResult.Filled then
    BuildSolidFromVertices(Context,
      HandlerResult.Vertices,
      Context.PrimitiveLineWeight);

  BuildLinesFromVertices(Context,
    HandlerResult.Vertices,
    HandlerResult.Closed,
    Context.PrimitiveLineWeight);
end;

initialization
  { Регистрируем обработчик OpCode=2 (Circle) и построитель подпримитивов.
    Если этот файл исключён из проекта — регистрация не происходит,
    круги внутри прокси-объектов перестают парситься без изменений в
    главном модуле. }
  TProxyOpCodeDispatcher.RegisterOpCode(
    CIRCLE_OPCODE,
    'Circle',
    @HandleCircle,
    @BuildCircleSubEntities);

end.
