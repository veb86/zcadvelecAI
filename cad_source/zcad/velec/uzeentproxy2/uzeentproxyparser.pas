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
  Модуль: uzeentproxyparser
  Назначение: Универсальный парсер бинарных данных прокси-объектов
  
  Подход:
  1. Извлекаем ACIS SAT данные или Display Mesh из ЛЮБОГО прокси-объекта
  2. Конвертируем в геометрию ZCAD (через тесселяцию если нужно)
  3. Не зависим от конкретного типа объекта (SPDS, Civil3D, и т.д.)
}

unit uzeentproxyparser;
{$Mode delphi}{$H+}
{$INCLUDE zengineconfig.inc}

interface

uses
  SysUtils,
  Classes,
  uzeTypes,
  uzeGeometryTypes;

type
  { Тип геометрии в прокси-объекте }
  TProxyGeometryType = (
    pgtUnknown,
    pgtACIS_SAT,      // ACIS SAT solid data
    pgtDisplayMesh,   // Polygonal mesh for display
    pgtProxyGraphics  // 2D proxy graphics
  );

  { Вершина меша }
  TProxyVertex = record
    X, Y, Z: Double;
  end;
  PProxyVertex = ^TProxyVertex;

  { Грань меша (треугольник или полигон) }
  TProxyFace = record
    VertexCount: Integer;
    VertexIndices: array[0..2] of Integer;  // Для треугольников
  end;
  PProxyFace = ^TProxyFace;

  { Данные меша (набор вершин и граней) }
  TProxyMeshData = class
  private
    FVertices: array of TProxyVertex;
    FFaces: array of TProxyFace;
    FVertexCount: Integer;
    FFaceCount: Integer;
    FBBoxMin: TzePoint3d;
    FBBoxMax: TzePoint3d;
    function GetVertex(Idx: Integer): TProxyVertex;
    function GetFace(Idx: Integer): TProxyFace;
  public
    procedure Clear;
    procedure AddVertex(X, Y, Z: Double);
    procedure AddTriangle(V1, V2, V3: Integer);
    procedure CalcBBox;

    property VertexCount: Integer read FVertexCount;
    property FaceCount: Integer read FFaceCount;
    property Vertices[Idx: Integer]: TProxyVertex read GetVertex;
    property Faces[Idx: Integer]: TProxyFace read GetFace;
    property BBoxMin: TzePoint3d read FBBoxMin;
    property BBoxMax: TzePoint3d read FBBoxMax;
  end;

  { Результат парсинга }
  TProxyParseResult = record
    GeometryType: TProxyGeometryType;
    MeshData: TProxyMeshData;
    SATData: TBytes;  // Сырые ACIS SAT данные
    Valid: Boolean;
    BBoxMin: TzePoint3d;
    BBoxMax: TzePoint3d;
  end;

  { Универсальный парсер прокси-объектов }
  TProxyObjectParser = class
  private
    FBuffer: TBytes;
    FBytesLen: Integer;
    FResult: TProxyParseResult;
    
    { Чтение заголовка прокси-объекта }
    function ReadProxyHeader(out dataSize: Integer; 
      out geometryType: TProxyGeometryType): Boolean;
    
    { Парсинг ACIS SAT данных }
    function ParseACISData(offset: Integer): Boolean;
    
    { Парсинг Display Mesh }
    function ParseDisplayMesh(offset: Integer): Boolean;
    
    { Парсинг Proxy Graphics (2D) }
    function ParseProxyGraphics(offset: Integer): Boolean;
    
    { Тесселяция ACIS в меш }
    procedure TessellateACIS;
    
  public
    { Инициализация из hex-строки DXF }
    function InitFromHex(const hexData: string): Boolean;
    
    { Главный метод парсинга }
    function Parse: TProxyParseResult;
    
    { Извлечение геометрии для отрисовки }
    function GetMeshForDisplay: TProxyMeshData;
  end;

{ Вспомогательные функции }
function HexToBytes(const hexStr: string): TBytes;
function ReadDouble(const buffer: TBytes; offset: Integer): Double;
function ReadInteger(const buffer: TBytes; offset: Integer): Integer;
function ReadShortString(const buffer: TBytes; offset: Integer): string;

implementation

{ === Вспомогательные функции === }

function HexToBytes(const hexStr: string): TBytes;
var
  i, val: Integer;
begin
  SetLength(Result, Length(hexStr) div 2);
  for i := 0 to Length(Result) - 1 do begin
    if TryStrToInt('$' + Copy(hexStr, i * 2 + 1, 2), val) then
      Result[i] := Lo(val)
    else
      Result[i] := 0;
  end;
end;

function ReadDouble(const buffer: TBytes; offset: Integer): Double;
begin
  if (offset >= 0) and (offset + 8 <= Length(buffer)) then
    Move(buffer[offset], Result, SizeOf(Double))
  else
    Result := 0;
end;

function ReadInteger(const buffer: TBytes; offset: Integer): Integer;
begin
  if (offset >= 0) and (offset + 4 <= Length(buffer)) then
    Move(buffer[offset], Result, SizeOf(Integer))
  else
    Result := 0;
end;

function ReadShortString(const buffer: TBytes; offset: Integer): string;
var
  len: Integer;
begin
  if (offset >= 0) and (offset < Length(buffer)) then begin
    len := buffer[offset];
    if (offset + 1 + len <= Length(buffer)) then
      SetString(Result, PChar(@buffer[offset + 1]), len)
    else
      Result := '';
  end else
    Result := '';
end;

{ === TProxyMeshData === }

procedure TProxyMeshData.Clear;
begin
  SetLength(FVertices, 0);
  SetLength(FFaces, 0);
  FVertexCount := 0;
  FFaceCount := 0;
  FBBoxMin.x := 0; FBBoxMin.y := 0; FBBoxMin.z := 0;
  FBBoxMax.x := 0; FBBoxMax.y := 0; FBBoxMax.z := 0;
end;

function TProxyMeshData.GetVertex(Idx: Integer): TProxyVertex;
begin
  Result := FVertices[Idx];
end;

function TProxyMeshData.GetFace(Idx: Integer): TProxyFace;
begin
  Result := FFaces[Idx];
end;

procedure TProxyMeshData.AddVertex(X, Y, Z: Double);
var
  idx: Integer;
begin
  idx := FVertexCount;
  SetLength(FVertices, idx + 1);
  FVertices[idx].X := X;
  FVertices[idx].Y := Y;
  FVertices[idx].Z := Z;
  Inc(FVertexCount);
end;

procedure TProxyMeshData.AddTriangle(V1, V2, V3: Integer);
var
  idx: Integer;
begin
  idx := FFaceCount;
  SetLength(FFaces, idx + 1);
  FFaces[idx].VertexCount := 3;
  FFaces[idx].VertexIndices[0] := V1;
  FFaces[idx].VertexIndices[1] := V2;
  FFaces[idx].VertexIndices[2] := V3;
  Inc(FFaceCount);
end;

procedure TProxyMeshData.CalcBBox;
var
  i: Integer;
  v: TProxyVertex;
begin
  if FVertexCount = 0 then begin
    FBBoxMin.x := 0; FBBoxMin.y := 0; FBBoxMin.z := 0;
    FBBoxMax.x := 0; FBBoxMax.y := 0; FBBoxMax.z := 0;
    Exit;
  end;

  FBBoxMin.x := FVertices[0].X; FBBoxMin.y := FVertices[0].Y; FBBoxMin.z := FVertices[0].Z;
  FBBoxMax.x := FVertices[0].X; FBBoxMax.y := FVertices[0].Y; FBBoxMax.z := FVertices[0].Z;

  for i := 1 to FVertexCount - 1 do begin
    v := FVertices[i];
    if v.X < FBBoxMin.x then FBBoxMin.x := v.X;
    if v.Y < FBBoxMin.y then FBBoxMin.y := v.Y;
    if v.Z < FBBoxMin.z then FBBoxMin.z := v.Z;
    if v.X > FBBoxMax.x then FBBoxMax.x := v.X;
    if v.Y > FBBoxMax.y then FBBoxMax.y := v.Y;
    if v.Z > FBBoxMax.z then FBBoxMax.z := v.Z;
  end;
end;

{ === TProxyObjectParser === }

function TProxyObjectParser.InitFromHex(const hexData: string): Boolean;
begin
  FBuffer := HexToBytes(hexData);
  FBytesLen := Length(FBuffer);
  FResult.Valid := False;
  FResult.MeshData := TProxyMeshData.Create;
  Result := FBytesLen >= 16;  // Минимум для заголовка
end;

function TProxyObjectParser.ReadProxyHeader(out dataSize: Integer; 
  out geometryType: TProxyGeometryType): Boolean;
var
  signature: Integer;
  version: Integer;
begin
  Result := False;
  geometryType := pgtUnknown;
  
  if FBytesLen < 16 then
    Exit;
  
  { Читаем заголовок }
  dataSize := ReadInteger(FBuffer, 0);
  version := ReadInteger(FBuffer, 4);
  
  { Проверяем сигнатуру ACIS SAT ("ACIS" = $43495341) }
  signature := ReadInteger(FBuffer, 8);
  
  if signature = $43495341 then begin  // "ACIS" little-endian
    geometryType := pgtACIS_SAT;
    Result := True;
  end else begin
    { Проверяем наличие mesh данных }
    { Обычно начинаются с количества вершин }
    if (dataSize > 0) and (dataSize <= FBytesLen) then begin
      geometryType := pgtDisplayMesh;
      Result := True;
    end;
  end;
end;

function TProxyObjectParser.ParseACISData(offset: Integer): Boolean;
var
  i, j: Integer;
  vertexCount: Integer;
  faceCount: Integer;
  x, y, z: Double;
begin
  { Парсинг ACIS SAT данных
    Формат: заголовок, затем entities (faces, edges, vertices)
    Упрощенный парсер для базовой геометрии }
  
  Result := False;
  
  { Ищем секцию VERTEX в ACIS данных }
  { Формат: "vertex" X Y Z }
  // TODO: Реализовать полноценный ACIS SAT парсер

  { Временная реализация: извлекаем bounding box из ACIS заголовка }
  if FBytesLen >= offset + 64 then begin
    { ACIS хранит bbox в заголовке после версии }
    FResult.BBoxMin.x := ReadDouble(FBuffer, offset + 16);
    FResult.BBoxMin.y := ReadDouble(FBuffer, offset + 24);
    FResult.BBoxMin.z := ReadDouble(FBuffer, offset + 32);
    FResult.BBoxMax.x := ReadDouble(FBuffer, offset + 40);
    FResult.BBoxMax.y := ReadDouble(FBuffer, offset + 48);
    FResult.BBoxMax.z := ReadDouble(FBuffer, offset + 56);

    FResult.SATData := Copy(FBuffer, offset, FBytesLen - offset);
    FResult.GeometryType := pgtACIS_SAT;
    Result := True;
  end;
end;

function TProxyObjectParser.ParseDisplayMesh(offset: Integer): Boolean;
var
  i: Integer;
  vertexCount: Integer;
  faceCount: Integer;
  x, y, z: Double;
begin
  { Парсинг Display Mesh
    Формат (предположительный):
    - 4 байта: количество вершин
    - N * 12 байт: вершины (X, Y, Z - double)
    - 4 байта: количество граней
    - M * 12 байт: грани (3 индекса по 4 байта) }
  
  Result := False;
  
  if FBytesLen < offset + 4 then
    Exit;
  
  vertexCount := ReadInteger(FBuffer, offset);
  Inc(offset, 4);
  
  if (vertexCount <= 0) or (vertexCount > 1000000) then
    Exit;  // Неверное значение
  
  { Читаем вершины }
  for i := 0 to vertexCount - 1 do begin
    if FBytesLen < offset + 24 then
      Exit;
    
    x := ReadDouble(FBuffer, offset);
    y := ReadDouble(FBuffer, offset + 8);
    z := ReadDouble(FBuffer, offset + 16);
    
    FResult.MeshData.AddVertex(x, y, z);
    Inc(offset, 24);
  end;
  
  { Читаем грани }
  if FBytesLen >= offset + 4 then begin
    faceCount := ReadInteger(FBuffer, offset);
    Inc(offset, 4);
    
    if (faceCount > 0) and (faceCount < 1000000) then begin
      for i := 0 to faceCount - 1 do begin
        if FBytesLen >= offset + 12 then begin
          FResult.MeshData.AddTriangle(
            ReadInteger(FBuffer, offset),
            ReadInteger(FBuffer, offset + 4),
            ReadInteger(FBuffer, offset + 8)
          );
          Inc(offset, 12);
        end;
      end;
    end;
  end;
  
  FResult.MeshData.CalcBBox;
  FResult.BBoxMin := FResult.MeshData.BBoxMin;
  FResult.BBoxMax := FResult.MeshData.BBoxMax;
  FResult.GeometryType := pgtDisplayMesh;
  Result := (FResult.MeshData.VertexCount > 0);
end;

function TProxyObjectParser.ParseProxyGraphics(offset: Integer): Boolean;
begin
  { Парсинг 2D Proxy Graphics
    Содержит 2D примитивы для отображения в плане
    Упрощенная реализация }
  
  Result := False;
  // TODO: Реализовать парсинг 2D примитивов
end;

procedure TProxyObjectParser.TessellateACIS;
begin
  { Конвертация ACIS SAT в меш для отображения
    TODO: Реализовать тесселяцию ACIS }
  
  { Временно: создаем bounding box как mesh }
  if FResult.SATData <> nil then begin
    { Создаем 12 треугольников для параллелепипеда }
    // TODO: Реализовать
  end;
end;

function TProxyObjectParser.Parse: TProxyParseResult;
var
  dataSize: Integer;
  geometryType: TProxyGeometryType;
begin
  FResult.Valid := False;
  FResult.MeshData.Clear;
  
  { Читаем заголовок }
  if not ReadProxyHeader(dataSize, geometryType) then begin
    // Пытаемся парсить как mesh с начала данных
    geometryType := pgtDisplayMesh;
  end;
  
  case geometryType of
    pgtACIS_SAT:
      begin
        if ParseACISData(16) then begin
          TessellateACIS;
          FResult.Valid := True;
        end;
      end;
      
    pgtDisplayMesh:
      begin
        if ParseDisplayMesh(16) then
          FResult.Valid := True;
      end;
      
    pgtProxyGraphics:
      begin
        if ParseProxyGraphics(16) then
          FResult.Valid := True;
      end;
  end;
  
  Result := FResult;
end;

function TProxyObjectParser.GetMeshForDisplay: TProxyMeshData;
begin
  if FResult.Valid then begin
    case FResult.GeometryType of
      pgtACIS_SAT:
        begin
          { Возвращаем тесселированный меш из ACIS }
          // TODO: Реализовать
          Result := FResult.MeshData;
        end;
        
      pgtDisplayMesh:
        Result := FResult.MeshData;
        
      pgtProxyGraphics:
        begin
          { Конвертируем 2D в 3D mesh }
          // TODO: Реализовать
          Result := FResult.MeshData;
        end;
        
      else
        Result := nil;
    end;
  end else
    Result := nil;
end;

end.
