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
  Модуль: uzeentproxyparserpolygon
  Назначение: Парсер заполненного полигона (OpCode=7, pgcPolygon) для примитивов
              внутри Proxy объектов. Соответствует примитиву HATCH (штриховка)
              или SOLID в DXF — замкнутый контур с заполнением.

  Архитектура:
  - Секция initialization регистрирует HandlePolygon в TProxyOpCodeDispatcher
  - Чтобы отключить парсинг полигонов — исключить этот файл из проекта

  Формат данных (AcGiWorldDraw, OpCode = 7 = pgcPolygon):
    VertexCount — int32   — количество вершин (>= 3)
    Vertices    — VertexCount × 3 double (по 24 байта каждая) — точки в WCS

  Отличие от Polyline (OpCode=6):
  - Контур всегда замкнутый (последняя вершина соединяется с первой)
  - Семантически представляет заполненную область (Hatch/Solid)

  Текущая реализация:
  - Контур отрисовывается как замкнутая полилиния через DrawPolyLineWithLT
  - Заполнение не реализовано (нет поддержки Hatch в Representation)
  - BBox вычисляется по всем вершинам
}

unit uzeentproxyparserpolygon;
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
  { OpCode заполненного полигона в формате AcGiWorldDraw }
  POLYGON_OPCODE = 7;

  { Минимальное допустимое количество вершин полигона }
  POLYGON_MIN_VERTEX_COUNT = 3;

  { Максимально допустимое количество вершин (защита от повреждённых данных) }
  POLYGON_MAX_VERTEX_COUNT = 100000;

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

{ Читает данные замкнутого полигона из потока, заполняет вершины и BBox.
  Добавляет первую вершину в конец для замыкания контура.
  Регистрируется в TProxyOpCodeDispatcher как обработчик OpCode=7. }
procedure HandlePolygon(
  Stream: TProxyByteStream;
  out HandlerResult: TProxyHandlerResult);
var
  VertexCount: Integer;
  I: Integer;
  Vertex: TzePoint3d;
  FirstVertex: TzePoint3d;
  BBoxInitialized: Boolean;
begin
  HandlerResult.Valid := False;
  HandlerResult.HasVertices := False;
  HandlerResult.HasBBox := False;

  { Читаем количество вершин }
  VertexCount := Stream.ReadInt32;

  programlog.LogOutFormatStr(
    'uzeentproxyparserpolygon: VertexCount=%d', [VertexCount], LM_Info);

  { Проверяем корректность количества вершин }
  if (VertexCount < POLYGON_MIN_VERTEX_COUNT)
    or (VertexCount > POLYGON_MAX_VERTEX_COUNT) then
  begin
    programlog.LogOutFormatStr(
      'uzeentproxyparserpolygon: VertexCount=%d is invalid, skipping',
      [VertexCount], LM_Info);
    Exit;
  end;

  { +1 для замыкающей вершины (копия первой) }
  HandlerResult.Vertices.init(VertexCount + 1);
  BBoxInitialized := False;

  { Читаем все вершины полигона }
  for I := 0 to VertexCount - 1 do
  begin
    Vertex := Stream.ReadVertex;
    HandlerResult.Vertices.PushBackData(Vertex);
    UpdateBBoxWithVertex(Vertex,
      HandlerResult.BBoxMin, HandlerResult.BBoxMax, BBoxInitialized);

    { Запоминаем первую вершину для замыкания контура }
    if I = 0 then
      FirstVertex := Vertex;
  end;

  { Замыкаем контур: добавляем первую вершину в конец }
  HandlerResult.Vertices.PushBackData(FirstVertex);

  HandlerResult.HasVertices := True;
  { Контур замкнут: первая вершина добавлена в конец, флаг Closed = True }
  HandlerResult.Closed := True;
  HandlerResult.HasBBox := BBoxInitialized;
  HandlerResult.Valid := True;

  programlog.LogOutFormatStr(
    'uzeentproxyparserpolygon: OK, %d vertices (closed), BBox=(%.3f,%.3f)-(%.3f,%.3f)',
    [HandlerResult.Vertices.Count,
     HandlerResult.BBoxMin.x, HandlerResult.BBoxMin.y,
     HandlerResult.BBoxMax.x, HandlerResult.BBoxMax.y], LM_Info);
end;

initialization
  { Регистрируем обработчик OpCode=7 (Polygon/Hatch).
    Если этот файл исключён из проекта — регистрация не происходит,
    полигоны (области штриховки) внутри прокси-объектов перестают парситься. }
  TProxyOpCodeDispatcher.RegisterOpCode(
    POLYGON_OPCODE,
    'Polygon/Hatch',
    @HandlePolygon);

end.
