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
  Модуль: uzeentproxyparsershell
  Назначение: Парсер оболочки Shell (OpCode=9, pgcShell) для примитивов
              внутри Proxy объектов. Shell — это PolyFace-сетка, состоящая
              из вершин и граней с индексами вершин.

  Архитектура:
  - Секция initialization регистрирует HandleShell
    в TProxyOpCodeDispatcher
  - Чтобы отключить парсинг Shell — исключить этот файл из проекта

  Формат данных (AcGiWorldDraw, OpCode = 9 = pgcShell):
    VertexCount    — int32 — количество вершин
    Vertices       — VertexCount × 3 double (24 байта) — точки в WCS
    FaceEntryCount — int32 — общее число записей в списке граней
    FaceList       — последовательность: [EdgeCount: int32]
                     [Index0..IndexN: uint32 × EdgeCount]
    EdgeTraitFlags — uint32 — битовая маска атрибутов рёбер
    FaceTraitFlags — uint32 — битовая маска атрибутов граней
    VertexTraitFlags — uint32 — битовая маска атрибутов вершин

  Текущая реализация:
  - Контуры граней отрисовываются как замкнутые полилинии
  - Атрибуты (traits) рёбер, граней и вершин пропускаются
  - BBox вычисляется по всем вершинам
}

unit uzeentproxyparsershell;
{$Mode delphi}{$H+}
{$INCLUDE zengineconfig.inc}

interface

implementation

uses
  SysUtils,
  uzeentproxystream,
  uzeentproxymanager,
  uzegeometrytypes,
  UGDBPoint3DArray,
  uzcLog;

const
  { OpCode оболочки в формате AcGiWorldDraw }
  SHELL_OPCODE = 9;

  { Ограничения для защиты от повреждённых данных }
  SHELL_MIN_VERTEX_COUNT = 3;
  SHELL_MAX_VERTEX_COUNT = 100000;
  SHELL_MAX_FACE_ENTRY_COUNT = 500000;
  SHELL_MAX_EDGES_PER_FACE = 10000;

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

{ Читает данные Shell из потока, извлекает контуры граней для отрисовки.
  Каждая грань отрисовывается как замкнутая полилиния по индексам вершин.
  Регистрируется в TProxyOpCodeDispatcher как обработчик OpCode=9. }
procedure HandleShell(
  Stream: TProxyByteStream;
  out HandlerResult: TProxyHandlerResult);
var
  VertexCount: Integer;
  FaceEntryCount: Integer;
  EdgeCount: Integer;
  VertexIndex: Integer;
  I, J: Integer;
  Vertex: TzePoint3d;
  FirstVertex: TzePoint3d;
  BBoxInitialized: Boolean;
  Vertices: array of TzePoint3d;
  EntriesRead: Integer;
begin
  HandlerResult.Valid := False;
  HandlerResult.HasVertices := False;
  HandlerResult.HasBBox := False;

  { Читаем количество вершин }
  VertexCount := Stream.ReadInt32;

  programlog.LogOutFormatStr(
    'uzeentproxyparsershell: VertexCount=%d',
    [VertexCount], LM_Info);

  if (VertexCount < SHELL_MIN_VERTEX_COUNT)
    or (VertexCount > SHELL_MAX_VERTEX_COUNT) then
  begin
    programlog.LogOutFormatStr(
      'uzeentproxyparsershell: VertexCount=%d is invalid, skipping',
      [VertexCount], LM_Info);
    Exit;
  end;

  { Читаем вершины и вычисляем BBox }
  SetLength(Vertices, VertexCount);
  BBoxInitialized := False;

  for I := 0 to VertexCount - 1 do
  begin
    Vertices[I] := Stream.ReadVertex;
    UpdateBBoxWithVertex(Vertices[I],
      HandlerResult.BBoxMin, HandlerResult.BBoxMax,
      BBoxInitialized);
  end;

  { Читаем количество записей в списке граней }
  FaceEntryCount := Stream.ReadInt32;

  programlog.LogOutFormatStr(
    'uzeentproxyparsershell: FaceEntryCount=%d',
    [FaceEntryCount], LM_Info);

  if (FaceEntryCount < 0)
    or (FaceEntryCount > SHELL_MAX_FACE_ENTRY_COUNT) then
  begin
    programlog.LogOutFormatStr(
      'uzeentproxyparsershell: FaceEntryCount=%d is invalid',
      [FaceEntryCount], LM_Info);
    Exit;
  end;

  { Оценка размера вершин для контуров: максимум = FaceEntryCount }
  HandlerResult.Vertices.init(FaceEntryCount + VertexCount);

  { Разбираем грани: каждая грань — EdgeCount + EdgeCount индексов }
  EntriesRead := 0;
  while EntriesRead < FaceEntryCount do
  begin
    if Stream.EndOfStream then
      Break;

    EdgeCount := Stream.ReadInt32;
    Inc(EntriesRead);

    { Пропускаем невалидные или нулевые грани }
    if (EdgeCount <= 0)
      or (EdgeCount > SHELL_MAX_EDGES_PER_FACE) then
      Continue;

    { Проверяем, что хватает записей для индексов }
    if EntriesRead + EdgeCount > FaceEntryCount then
      Break;

    { Читаем индексы вершин грани и добавляем контур }
    FirstVertex.x := 0;
    FirstVertex.y := 0;
    FirstVertex.z := 0;

    for J := 0 to EdgeCount - 1 do
    begin
      VertexIndex := Integer(Stream.ReadUInt32);
      Inc(EntriesRead);

      { Проверяем корректность индекса }
      if (VertexIndex < 0) or (VertexIndex >= VertexCount) then
        Continue;

      Vertex := Vertices[VertexIndex];
      HandlerResult.Vertices.PushBackData(Vertex);

      { Запоминаем первую вершину грани для замыкания }
      if J = 0 then
        FirstVertex := Vertex;
    end;

    { Замыкаем контур грани }
    HandlerResult.Vertices.PushBackData(FirstVertex);
  end;

  { Пропускаем оставшиеся данные (traits рёбер, граней, вершин).
    Traits идут после списка граней, но мы не используем их —
    остаток будет пропущен автоматически диспетчером, так как он знает
    CommandSize и перемещает указатель потока. }

  HandlerResult.HasVertices := (HandlerResult.Vertices.Count > 0);
  { Контуры граней уже замкнуты вручную (первая вершина добавлена в конец) }
  HandlerResult.HasBBox := BBoxInitialized;
  HandlerResult.Valid := True;

  programlog.LogOutFormatStr(
    'uzeentproxyparsershell: OK, %d source vertices, %d output vertices',
    [VertexCount, HandlerResult.Vertices.Count], LM_Info);
end;

initialization
  { Регистрируем обработчик OpCode=9 (Shell/PolyFace).
    Shell используется для отображения заполненных областей (стрелки,
    прямоугольники подложки текста и другие PolyFace-сетки) внутри
    прокси-объектов. }
  TProxyOpCodeDispatcher.RegisterOpCode(
    SHELL_OPCODE,
    'Shell/PolyFace',
    @HandleShell);

end.
