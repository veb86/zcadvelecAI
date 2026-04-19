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
  Модуль: uzeentproxyparserarc
  Назначение: Парсер дуги (OpCode=4, pgcCircularArc) для примитивов внутри
              Proxy объектов.

  Архитектура:
  - Секция initialization регистрирует HandleArc в TProxyOpCodeDispatcher
  - Чтобы отключить парсинг дуг — исключить этот файл из проекта

  Формат данных (AcGiWorldDraw, OpCode = 4 = pgcCircularArc):
    Center      — 3 × double (24 байта) — центр дуги в WCS
    Radius      — 1 × double (8 байт)  — радиус
    Normal      — 3 × double (24 байта) — нормаль (ось Z локальной СК)
    StartVector — 3 × double (24 байта) — вектор начала дуги (в плоскости OCS)
    SweepAngle  — 1 × double (8 байт)  — угол раствора дуги (радианы)
    ArcType     — 1 × int32 (4 байта)  — тип дуги (0 — обычная)

  Тесселяция:
  - Дуга аппроксимируется ARC_SEGMENT_COUNT отрезками
  - Количество отрезков пропорционально углу раствора (минимум 4)
  - Вершины сохраняются в TProxyHandlerResult.Vertices
}

unit uzeentproxyparserarc;
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
  gzctnrVectorTypes,
  uzcLog;

const
  { OpCode дуги в формате AcGiWorldDraw }
  ARC_OPCODE = 4;

  { Максимальное количество отрезков тесселяции для полной окружности }
  ARC_MAX_SEGMENT_COUNT = 64;

  { Минимальное количество отрезков тесселяции }
  ARC_MIN_SEGMENT_COUNT = 4;

  { Нормаль по умолчанию совпадает с осью Z WCS }
  ARC_Z_AXIS: TzePoint3d = (x: 0.0; y: 0.0; z: 1.0);

  { Порог параллельности для выбора вспомогательной оси }
  ARC_AXIS_THRESHOLD = 0.9;

{ --- Вспомогательные процедуры --- }

{ Проверяет, совпадают ли два вектора с точностью Epsilon }
function VectorsAreEqual(const V1, V2: TzePoint3d;
  const Epsilon: Double = 1e-9): Boolean;
begin
  Result := (Abs(V1.x - V2.x) <= Epsilon)
    and (Abs(V1.y - V2.y) <= Epsilon)
    and (Abs(V1.z - V2.z) <= Epsilon);
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
  if Abs(ZAxis.x) < ARC_AXIS_THRESHOLD then
    XAxis := NormalizeVertex(AuxX * ZAxis.z - ZAxis * AuxX.z)
  else
    XAxis := NormalizeVertex(AuxY * ZAxis.z - ZAxis * AuxY.z);

  YAxis := NormalizeVertex(ZAxis * XAxis.x - XAxis * ZAxis.x);

  { Проекция точки на оси OCS }
  Result.x := scalarDot(Point, XAxis);
  Result.y := scalarDot(Point, YAxis);
  Result.z := scalarDot(Point, ZAxis);
end;

{ Вычисляет BBox дуги через тесселированные вершины }
procedure CalcArcBBoxFromVertices(const Vertices: GDBPoint3DArray;
  out BBoxMin, BBoxMax: TzePoint3d);
var
  Iter: itrec;
  Pt: PzePoint3d;
  Initialized: Boolean;
begin
  BBoxMin.x := 0; BBoxMin.y := 0; BBoxMin.z := 0;
  BBoxMax := BBoxMin;
  Initialized := False;

  Pt := Vertices.beginiterate(Iter);
  while Pt <> nil do
  begin
    if not Initialized then
    begin
      BBoxMin := Pt^;
      BBoxMax := Pt^;
      Initialized := True;
    end
    else
    begin
      if Pt^.x < BBoxMin.x then BBoxMin.x := Pt^.x;
      if Pt^.y < BBoxMin.y then BBoxMin.y := Pt^.y;
      if Pt^.z < BBoxMin.z then BBoxMin.z := Pt^.z;
      if Pt^.x > BBoxMax.x then BBoxMax.x := Pt^.x;
      if Pt^.y > BBoxMax.y then BBoxMax.y := Pt^.y;
      if Pt^.z > BBoxMax.z then BBoxMax.z := Pt^.z;
    end;
    Pt := Vertices.iterate(Iter);
  end;
end;

{ Вычисляет количество отрезков тесселяции пропорционально углу раствора }
function CalcSegmentCount(const SweepAngle: Double): Integer;
begin
  Result := Round(ARC_MAX_SEGMENT_COUNT * Abs(SweepAngle) / (2 * Pi));
  if Result < ARC_MIN_SEGMENT_COUNT then
    Result := ARC_MIN_SEGMENT_COUNT;
  if Result > ARC_MAX_SEGMENT_COUNT then
    Result := ARC_MAX_SEGMENT_COUNT;
end;

{ Тесселирует дугу в массив вершин.
  Center — центр, Radius — радиус, StartAngle — начальный угол (рад),
  SweepAngle — угол раствора (рад), Z — координата по оси Z. }
procedure TessellateArc(const Center: TzePoint3d; const Radius: Double;
  const StartAngle, SweepAngle: Double; var Vertices: GDBPoint3DArray);
var
  SegmentCount: Integer;
  I: Integer;
  Angle: Double;
  Pt: TzePoint3d;
begin
  SegmentCount := CalcSegmentCount(SweepAngle);
  Vertices.init(SegmentCount + 1);

  { Добавляем SegmentCount + 1 вершин (включая конечную точку дуги) }
  for I := 0 to SegmentCount do
  begin
    Angle := StartAngle + SweepAngle * I / SegmentCount;
    Pt.x := Center.x + Radius * Cos(Angle);
    Pt.y := Center.y + Radius * Sin(Angle);
    Pt.z := Center.z;
    Vertices.PushBackData(Pt);
  end;
end;

{ --- Обработчик OpCode --- }

{ Читает данные дуги из потока, вычисляет BBox и тесселирует контур.
  Регистрируется в TProxyOpCodeDispatcher как обработчик OpCode=4. }
procedure HandleArc(
  Stream: TProxyByteStream;
  out HandlerResult: TProxyHandlerResult);
var
  Center: TzePoint3d;
  Radius: Double;
  Normal: TzePoint3d;
  StartVector: TzePoint3d;
  SweepAngle: Double;
  StartAngle: Double;
begin
  HandlerResult.Valid := False;
  HandlerResult.HasVertices := False;
  HandlerResult.HasBBox := False;

  { Читаем: Center (24 байта) + Radius (8 байт) + Normal (24 байта)
            + StartVector (24 байта) + SweepAngle (8 байт) + ArcType (4 байта) }
  Center := Stream.ReadVertex;
  Radius := Stream.ReadDouble;
  Normal := Stream.ReadVector;
  StartVector := Stream.ReadVector;
  SweepAngle := Stream.ReadDouble;
  Stream.ReadInt32; { ArcType — тип дуги, пока не используется }

  programlog.LogOutFormatStr(
    'uzeentproxyparserarc: Center=(%.4f,%.4f,%.4f) R=%.4f Sweep=%.4f rad',
    [Center.x, Center.y, Center.z, Radius, SweepAngle], LM_Info);

  { Радиус должен быть положительным }
  if Radius <= 0 then
  begin
    programlog.LogOutFormatStr(
      'uzeentproxyparserarc: Radius=%.4f is invalid, skipping', [Radius], LM_Info);
    Exit;
  end;

  { Если нормаль отличается от Z, переводим центр в OCS }
  if not VectorsAreEqual(Normal, ARC_Z_AXIS) then
  begin
    Center := TransformPointToOCS(Center, Normal);
    StartVector := TransformPointToOCS(StartVector, Normal);
  end;

  { Вычисляем начальный угол из вектора StartVector }
  StartAngle := ArcTan2(StartVector.y, StartVector.x);

  { Тесселируем дугу }
  TessellateArc(Center, Radius, StartAngle, SweepAngle, HandlerResult.Vertices);
  HandlerResult.HasVertices := True;

  { Вычисляем BBox по тесселированным вершинам }
  CalcArcBBoxFromVertices(HandlerResult.Vertices,
    HandlerResult.BBoxMin, HandlerResult.BBoxMax);
  HandlerResult.HasBBox := True;

  HandlerResult.Valid := True;

  programlog.LogOutFormatStr(
    'uzeentproxyparserarc: OK, %d vertices, BBox=(%.3f,%.3f)-(%.3f,%.3f)',
    [HandlerResult.Vertices.Count,
     HandlerResult.BBoxMin.x, HandlerResult.BBoxMin.y,
     HandlerResult.BBoxMax.x, HandlerResult.BBoxMax.y], LM_Info);
end;

{ --- Построитель подпримитивов --- }

{ Создаёт подпримитивы-отрезки (GDBObjLine) из тесселированных вершин
  дуги. Дуга — открытый контур (не замыкается). }
procedure BuildArcSubEntities(
  const HandlerResult: TProxyHandlerResult;
  const Context: TProxySubEntityContext);
begin
  if not HandlerResult.HasVertices then
    Exit;
  BuildLinesFromVertices(Context,
    HandlerResult.Vertices,
    False,
    Context.PrimitiveLineWeight);
end;

initialization
  { Регистрируем обработчик OpCode=4 (CircularArc) и построитель
    подпримитивов. Если этот файл исключён из проекта — регистрация не
    происходит, дуги внутри прокси-объектов перестают парситься. }
  TProxyOpCodeDispatcher.RegisterOpCode(
    ARC_OPCODE,
    'CircularArc',
    @HandleArc,
    @BuildArcSubEntities);

end.
