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
  Модуль: uzeentproxyparserlwpolyline
  Назначение: Парсер 2D полилинии (OpCode=33, pgcLwPolyline) для примитивов
              внутри Proxy объектов. Соответствует примитиву LWPOLYLINE в DXF.

  Архитектура:
  - Секция initialization регистрирует HandleLwPolyline в TProxyOpCodeDispatcher
  - Чтобы отключить парсинг LWPOLYLINE — исключить этот файл из проекта

  Формат данных (AcGiWorldDraw, OpCode = 33 = pgcLwPolyline):
    Flags         — int32   — флаги (бит 0: замкнутая, бит 512: спланированная)
    ConstWidth    — double  — постоянная ширина (0 = нет)
    Elevation     — double  — высота Z (все вершины на этом Z)
    Thickness     — double  — толщина экструзии
    Normal        — 3 × double — нормаль (ось Z локальной СК)
    VertexCount   — int32   — количество вершин
    Для каждой вершины:
      Point2D     — 2 × double (16 байт) — координаты X,Y
      Bulge       — double   — выпуклость (0 = прямой сегмент)
      StartWidth  — double   — начальная ширина сегмента
      EndWidth    — double   — конечная ширина сегмента

  Текущая реализация:
  - Выпуклость (Bulge) не тесселируется — сегменты рисуются прямыми
  - Z всех вершин устанавливается из поля Elevation
  - BBox вычисляется по всем вершинам
}

unit uzeentproxyparserlwpolyline;
{$Mode delphi}{$H+}
{$INCLUDE zengineconfig.inc}

interface

{ Публичный интерфейс не нужен — регистрация происходит автоматически
  при загрузке модуля через секцию initialization }

implementation

uses
  SysUtils,
  uzeentproxystream,
  uzeentproxymanager,
  uzegeometrytypes,
  UGDBPoint3DArray,
  uzcLog;

const
  { OpCode 2D полилинии в формате AcGiWorldDraw }
  LWPOLYLINE_OPCODE = 33;

  { Минимальное допустимое количество вершин }
  LWPOLYLINE_MIN_VERTEX_COUNT = 2;

  { Максимально допустимое количество вершин (защита от повреждённых данных) }
  LWPOLYLINE_MAX_VERTEX_COUNT = 100000;

{ --- Вспомогательные процедуры --- }

{ Расширяет BBox точкой Vertex, обновляя BBoxMin и BBoxMax.
  При первом вызове (Initialized = False) инициализирует BBox. }
procedure UpdateBBoxWithVertex(const Vertex: TzePoint3d;
  var BBoxMin, BBoxMax: TzePoint3d; var Initialized: Boolean);
begin
  if not Initialized then
  begin
    BBoxMin := Vertex;
    BBoxMax := Vertex;
    Initialized := True;
    Exit;
  end;
  if Vertex.x < BBoxMin.x then BBoxMin.x := Vertex.x;
  if Vertex.y < BBoxMin.y then BBoxMin.y := Vertex.y;
  if Vertex.z < BBoxMin.z then BBoxMin.z := Vertex.z;
  if Vertex.x > BBoxMax.x then BBoxMax.x := Vertex.x;
  if Vertex.y > BBoxMax.y then BBoxMax.y := Vertex.y;
  if Vertex.z > BBoxMax.z then BBoxMax.z := Vertex.z;
end;

{ --- Обработчик OpCode --- }

{ Читает данные 2D полилинии из потока, заполняет вершины и BBox.
  Регистрируется в TProxyOpCodeDispatcher как обработчик OpCode=33. }
procedure HandleLwPolyline(
  Stream: TProxyByteStream;
  out HandlerResult: TProxyHandlerResult);
var
  VertexCount: Integer;
  Flags: Integer;
  Elevation: Double;
  I: Integer;
  Pt2D: TzePoint2d;
  Vertex: TzePoint3d;
  BBoxInitialized: Boolean;
begin
  HandlerResult.Valid := False;
  HandlerResult.HasVertices := False;
  HandlerResult.HasBBox := False;

  { Читаем заголовок LWPOLYLINE }
  Flags := Stream.ReadInt32; { Flags: бит 9 (512) = замкнутая полилиния }
  Stream.ReadDouble;  { ConstWidth: постоянная ширина (не используется в отрисовке) }
  Elevation := Stream.ReadDouble; { Elevation: Z-координата всех вершин }
  Stream.ReadDouble;  { Thickness: толщина экструзии (не используется) }
  Stream.ReadVector;  { Normal: нормаль (не применяется — вершины уже в OCS) }

  VertexCount := Stream.ReadInt32;

  programlog.LogOutFormatStr(
    'uzeentproxyparserlwpolyline: VertexCount=%d Elevation=%.4f',
    [VertexCount, Elevation], LM_Info);

  { Проверяем корректность количества вершин }
  if (VertexCount < LWPOLYLINE_MIN_VERTEX_COUNT)
    or (VertexCount > LWPOLYLINE_MAX_VERTEX_COUNT) then
  begin
    programlog.LogOutFormatStr(
      'uzeentproxyparserlwpolyline: VertexCount=%d is invalid, skipping',
      [VertexCount], LM_Info);
    Exit;
  end;

  HandlerResult.Vertices.init(VertexCount);
  BBoxInitialized := False;

  { Читаем все вершины }
  for I := 0 to VertexCount - 1 do
  begin
    Pt2D := Stream.ReadPoint2D;
    Stream.ReadDouble; { Bulge: выпуклость сегмента (тесселяция не реализована) }
    Stream.ReadDouble; { StartWidth: начальная ширина сегмента }
    Stream.ReadDouble; { EndWidth: конечная ширина сегмента }

    { Формируем 3D-вершину: Z берём из поля Elevation }
    Vertex.x := Pt2D.x;
    Vertex.y := Pt2D.y;
    Vertex.z := Elevation;

    HandlerResult.Vertices.PushBackData(Vertex);
    UpdateBBoxWithVertex(Vertex,
      HandlerResult.BBoxMin, HandlerResult.BBoxMax, BBoxInitialized);
  end;

  HandlerResult.HasVertices := True;
  HandlerResult.Closed := (Flags and 512) <> 0;
  HandlerResult.HasBBox := BBoxInitialized;
  HandlerResult.Valid := True;

  programlog.LogOutFormatStr(
    'uzeentproxyparserlwpolyline: OK, %d vertices, closed=%s, BBox=(%.3f,%.3f)-(%.3f,%.3f)',
    [HandlerResult.Vertices.Count,
     BoolToStr(HandlerResult.Closed, True),
     HandlerResult.BBoxMin.x, HandlerResult.BBoxMin.y,
     HandlerResult.BBoxMax.x, HandlerResult.BBoxMax.y], LM_Info);
end;

initialization
  { Регистрируем обработчик OpCode=33 (LwPolyline).
    Если этот файл исключён из проекта — регистрация не происходит,
    LWPOLYLINE внутри прокси-объектов перестают парситься. }
  TProxyOpCodeDispatcher.RegisterOpCode(
    LWPOLYLINE_OPCODE,
    'LwPolyline',
    @HandleLwPolyline);

end.
