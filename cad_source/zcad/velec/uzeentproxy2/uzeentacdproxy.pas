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
  Модуль: uzeentacdproxy
  Назначение: Поддержка прокси-объектов AutoCAD (ACAD_PROXY_ENTITY) в ZCAD.
  Этап 1: Базовая визуализация через отрисовку габаритной рамки (Bounding Box).
  Зависимости: uzeentity, uzeconsts, uzeffdxfsupport, uzeentityfactory,
               uzegeometrytypes, uzegeometry, uzgldrawcontext, uzedrawingdef.
}

unit uzeentacdproxy;
{$Mode delphi}{$H+}
{$INCLUDE zengineconfig.inc}

interface

uses
  uzeentityfactory,
  uzgldrawcontext,
  uzedrawingdef,
  uzeentsubordinated,
  uzeent3d,
  uzeentity,
  uzctnrVectorBytesStream,
  uzeTypes,
  uzeconsts,
  uzglviewareadata,
  uzegeometrytypes,
  uzegeometry,
  uzeffdxfsupport,
  uzMVReader,
  uzCtnrVectorpBaseEntity,
  uzbLogIntf,
  uzclog,
  uzestyleslayers,
  uzecamera,
  SysUtils,
  Classes,
  Math,
  uzeentproxyparser;

type
  PGDBObjAcdProxy = ^GDBObjAcdProxy;

  { Прокси-объект AutoCAD (ACAD_PROXY_ENTITY).
    Этап 1: отображается как габаритная рамка.
    Этап 2: отображает геометрию из универсального парсера (ACIS/Display Mesh).
    Хранит точки минимума (LBN) и максимума (RTF) из DXF-данных объекта. }
  GDBObjAcdProxy = object(GDBObj3d)
  private
    { Координата минимального угла габаритной рамки в OCS }
    FBBoxMinInOCS: TzePoint3d;
    { Координата максимального угла габаритной рамки в OCS }
    FBBoxMaxInOCS: TzePoint3d;
    { Флаг: данные габаритной рамки успешно загружены }
    FBBoxLoaded: Boolean;
    { Данные меша из универсального парсера }
    FMeshData: TProxyMeshData;

    { Отрисовывает одно ребро габаритной рамки }
    procedure DrawBBoxEdge(var DC: TDrawContext;
      const ptFrom, ptTo: TzePoint3d);
    
    { Отрисовывает меш прокси-объекта }
    procedure DrawProxyMesh(var DC: TDrawContext);

  public
    constructor init(own: Pointer; layeraddres: PGDBLayerProp; LW: smallint);
    constructor initnul(owner: PGDBObjGenericWithSubordinated);

    { Загружает данные объекта из DXF-потока }
    procedure LoadFromDXF(var rdr: TZMemReader; ptu: PExtensionData;
      var drawing: TDrawingDef;
      var context: TIODXFLoadContext); virtual;

    { Сохраняет данные объекта в DXF-поток }
    procedure SaveToDXF(var outStream: TZctnrVectorBytes;
      var drawing: TDrawingDef;
      var IODXFContext: TIODXFSaveContext); virtual;

    { Рассчитывает визуальное представление объекта }
    procedure FormatEntity(var drawing: TDrawingDef;
      var DC: TDrawContext;
      Stage: TEFStages = EFAllStages); virtual;

    { Отрисовывает геометрию объекта напрямую (используется при трансформации) }
    procedure DrawGeometry(lw: integer; var DC: TDrawContext;
      const inFrustumState: TInBoundingVolume); virtual;

    { Вычисляет, попадает ли объект в усечённую пирамиду видимости }
    function CalcTrueInFrustum(
      const frustum: TzeFrustum): TInBoundingVolume; virtual;

    { Применяет матрицу трансформации из объекта-источника }
    procedure TransformAt(p: PGDBObjEntity;
      t_matrix: PzeTypedMatrix4d); virtual;

    { Возвращает наименование типа объекта }
    function GetObjTypeName: string; virtual;

    { Возвращает числовой идентификатор типа объекта }
    function GetObjType: TObjID; virtual;

    { Создаёт копию объекта }
    function Clone(own: Pointer): PGDBObjEntity; virtual;

    { Добавляет контрольные точки объекта }
    procedure addcontrolpoints(tdesc: Pointer); virtual;

    { Сохраняет состояние для real-time модификации }
    procedure rtsave(refp: Pointer); virtual;

    { Применяет real-time изменение одной точки }
    procedure rtmodifyonepoint(const rtmod: TRTModifyData); virtual;

    { Проверяет необходимость real-time модификации }
    function IsRTNeedModify(const Point: PControlPointDesc;
      p: Pointer): boolean; virtual;

    { Переносит контрольную точку на экранные координаты }
    procedure remaponecontrolpoint(pdesc: pcontrolpointdesc;
      ProjectProc: GDBProjectProc); virtual;

    { Вычисляет габаритный прямоугольник в экранных координатах }
    procedure getoutbound(var DC: TDrawContext); virtual;

    { Создаёт новый инициализированный экземпляр прокси-объекта }
    class function CreateInstance: PGDBObjAcdProxy; static;

    destructor done; virtual;
  end;

{ Выделяет память для нового прокси-объекта без инициализации }
function AllocAcdProxy: Pointer;

{ Выделяет и инициализирует новый прокси-объект }
function AllocAndInitAcdProxy(
  owner: PGDBObjGenericWithSubordinated): PGDBObjAcdProxy;
implementation
{ Преобразует hex-строку в байтовый буфер }
function HexStringToBytes(const hexStr: string; var buffer: array of Byte): Integer;
var
  i, j: Integer;
  valInt: Integer;
  code: Integer;
  hexPair: string;
begin
  Result := 0;
  i := 1;
  while i <= Length(hexStr) - 1 do begin
    if Result >= Length(buffer) then
      Exit;
    hexPair := '$' + Copy(hexStr, i, 2);
    Val(hexPair, valInt, code);
    if code = 0 then begin
      buffer[Result] := Byte(valInt);
      Inc(Result);
    end;
    Inc(i, 2);
  end;
end;

{ Извлекает 64-bit double из буфера (little-endian) }
function ReadDoubleFromBuffer(const buffer: array of Byte; offset: Integer): Double;
begin
  Move(buffer[offset], Result, SizeOf(Double));
end;

{ Парсит бинарные данные ACAD_PROXY_ENTITY для извлечения Bounding Box.

  Структура данных для SPDSPOLYMORPHMARK (круг + текст):
  - Заголовок ~20-29 байт
  - Данные включают параметры круга: centerX, centerY, centerZ, radius
  - BBox вычисляется из параметров круга: (centerX-radius, centerY-radius) - (centerX+radius, centerY+radius)

  Алгоритм:
  1. Ищем структуру из 4 double: centerX, centerY, centerZ(=0), radius
  2. Вычисляем BBox из этих параметров }
function ParseProxyBBox(const hexData: string; var bbMin, bbMax: TzePoint3d): Boolean;
var
  buffer: array[0..16384] of Byte;
  bytesLen: Integer;
  i: Integer;
  centerX, centerY, centerZ, radius: Double;
  found: Boolean;
  val: Double;
begin
  Result := False;
  bytesLen := HexStringToBytes(hexData, buffer);
  if bytesLen < 50 then
    Exit;

  { Ищем структуру круга: centerX, centerY, centerZ(=0), radius
    Эти значения идут последовательно с шагом 8 байт }
  found := False;
  for i := 20 to bytesLen - 32 do begin
    centerX := ReadDoubleFromBuffer(buffer, i);
    centerY := ReadDoubleFromBuffer(buffer, i + 8);
    centerZ := ReadDoubleFromBuffer(buffer, i + 16);
    radius := ReadDoubleFromBuffer(buffer, i + 24);

    { Проверяем, что это похоже на параметры круга:
      - centerX и centerY должны быть "большими" координатами (> 100)
      - centerZ близко к 0
      - radius положительный и разумного размера (10-10000) }
    if (Abs(centerX) > 100) and (Abs(centerX) < 1e7) and
       (Abs(centerY) > 100) and (Abs(centerY) < 1e7) and
       (Abs(centerZ) < 1e-3) and
       (radius > 10) and (radius < 10000) then begin
      found := True;
      Break;
    end;
  end;

  if found then begin
    { Вычисляем BBox из параметров круга }
    bbMin.x := centerX - radius;
    bbMin.y := centerY - radius;
    bbMin.z := centerZ;
    bbMax.x := centerX + radius;
    bbMax.y := centerY + radius;
    bbMax.z := centerZ;

    Result := True;
    programlog.LogOutFormatStr(
      'uzeentacdproxy: ParseProxyBBox CIRCLE FOUND centerX=%.4f centerY=%.4f radius=%.4f',
      [centerX, centerY, radius], LM_Info);
    programlog.LogOutFormatStr(
      'uzeentacdproxy: ParseProxyBBox Min=(%.4f;%.4f;%.4f) Max=(%.4f;%.4f;%.4f)',
      [bbMin.x, bbMin.y, bbMin.z, bbMax.x, bbMax.y, bbMax.z], LM_Info);
  end else begin
    { Если круг не найден, пытаемся найти BBox сканированием всех double }
    programlog.LogOutFormatStr('uzeentacdproxy: ParseProxyBBox circle not found, trying generic scan', [], LM_Info);

    { Инициализируем значения }
    bbMin.x := 1e30; bbMin.y := 1e30; bbMin.z := 1e30;
    bbMax.x := -1e30; bbMax.y := -1e30; bbMax.z := -1e30;
    found := False;

    { Сканируем все double значения в буфере }
    for i := 20 to bytesLen - 8 do begin
      val := ReadDoubleFromBuffer(buffer, i);

      { Проверяем, что значение разумное }
      if not (IsNan(val) or IsInfinite(val)) and (Abs(val) < 1e9) then begin
        { Игнорируем очень маленькие значения }
        if Abs(val) > 1e-6 then begin
          if val < bbMin.x then bbMin.x := val;
          if val > bbMax.x then bbMax.x := val;
          if val < bbMin.y then bbMin.y := val;
          if val > bbMax.y then bbMax.y := val;
          if val < bbMin.z then bbMin.z := val;
          if val > bbMax.z then bbMax.z := val;
          found := True;
        end;
      end;
    end;

    if found then begin
      { Для 2D объектов устанавливаем Z=0 }
      if Abs(bbMax.z - bbMin.z) < 1e-6 then begin
        bbMin.z := 0.0;
        bbMax.z := 0.0;
      end;

      Result := True;
      programlog.LogOutFormatStr(
        'uzeentacdproxy: ParseProxyBBox (generic) Min=(%.4f;%.4f;%.4f) Max=(%.4f;%.4f;%.4f)',
        [bbMin.x, bbMin.y, bbMin.z, bbMax.x, bbMax.y, bbMax.z], LM_Info);
    end else begin
      programlog.LogOutFormatStr('uzeentacdproxy: ParseProxyBBox failed - no valid values found', [], LM_Info);
    end;
  end;
end;


{ --- Вспомогательные процедуры --- }

{ Обновляет минимальную/максимальную точку габаритной рамки по заданной вершине.
  Используется при загрузке вершин из DXF для построения AABB. }
procedure ExpandBBox(var bbMin, bbMax: TzePoint3d;
  const pt: TzePoint3d; var initialized: Boolean);
begin
  if not initialized then begin
    bbMin := pt;
    bbMax := pt;
    initialized := True;
    Exit;
  end;
  if pt.x < bbMin.x then bbMin.x := pt.x;
  if pt.y < bbMin.y then bbMin.y := pt.y;
  if pt.z < bbMin.z then bbMin.z := pt.z;
  if pt.x > bbMax.x then bbMax.x := pt.x;
  if pt.y > bbMax.y then bbMax.y := pt.y;
  if pt.z > bbMax.z then bbMax.z := pt.z;
end;

{ --- Реализация GDBObjAcdProxy --- }

constructor GDBObjAcdProxy.init(own: Pointer; layeraddres: PGDBLayerProp;
  LW: smallint);
begin
  inherited init(own, layeraddres, LW);
  FBBoxMinInOCS := NulVertex;
  FBBoxMaxInOCS := NulVertex;
  FBBoxLoaded   := False;
  FMeshData     := nil;
end;

constructor GDBObjAcdProxy.initnul(owner: PGDBObjGenericWithSubordinated);
begin
  inherited initnul(owner);
  FBBoxMinInOCS := NulVertex;
  FBBoxMaxInOCS := NulVertex;
  FBBoxLoaded   := False;
  FMeshData     := nil;
end;

{ Загружает прокси-объект из DXF.
  Использует универсальный парсер (TProxyObjectParser) для извлечения геометрии.
  Поддерживает ACIS SAT и Display Mesh форматы.
  Читает базовые свойства (слой, цвет) через LoadFromDXFObjShared.
  Неизвестные коды пропускаются. }
procedure GDBObjAcdProxy.LoadFromDXF(var rdr: TZMemReader;
  ptu: PExtensionData; var drawing: TDrawingDef;
  var context: TIODXFLoadContext);
var
  byt: Integer;
  pt:  TzePoint3d;
  bbInitialized: Boolean;
  vertexCount: Integer;
  hexData: string;
  firstHexProcessed: Boolean;
  parser: TProxyObjectParser;
  parseResult: TProxyParseResult;
begin
  bbInitialized := False;
  FBBoxMinInOCS := NulVertex;
  FBBoxMaxInOCS := NulVertex;
  FBBoxLoaded   := False;
  vertexCount   := 0;
  firstHexProcessed := False;

  programlog.LogOutFormatStr('uzeentacdproxy: LoadFromDXF START', [], LM_Info);

  byt := rdr.ParseInteger;
  while byt <> 0 do begin
    programlog.LogOutFormatStr('uzeentacdproxy: LoadFromDXF read code=%d', [byt], LM_Info);
    { Общие свойства: слой, цвет, линия и прочее }
    if not LoadFromDXFObjShared(rdr, byt, ptu, drawing, context) then begin
      { Вершины из кода 10 (X) — группы 10..39 задают точки объекта.
        Используем их для построения ограничивающего параллелепипеда. }
      if dxfLoadGroupCodeVertex(rdr, 10, byt, pt) then begin
        Inc(vertexCount);
        programlog.LogOutFormatStr(
          'uzeentacdproxy: LoadFromDXF vertex code 10 processed #%d pt=(%.2f;%.2f;%.2f)',
          [vertexCount, pt.x, pt.y, pt.z], LM_Info);
        ExpandBBox(FBBoxMinInOCS, FBBoxMaxInOCS, pt, bbInitialized);
      end
      else if dxfLoadGroupCodeVertex(rdr, 11, byt, pt) then begin
        Inc(vertexCount);
        programlog.LogOutFormatStr(
          'uzeentacdproxy: LoadFromDXF vertex code 11 processed #%d pt=(%.2f;%.2f;%.2f)',
          [vertexCount, pt.x, pt.y, pt.z], LM_Info);
        ExpandBBox(FBBoxMinInOCS, FBBoxMaxInOCS, pt, bbInitialized);
      end
      else if dxfLoadGroupCodeVertex(rdr, 12, byt, pt) then begin
        Inc(vertexCount);
        programlog.LogOutFormatStr(
          'uzeentacdproxy: LoadFromDXF vertex code 12 processed #%d pt=(%.2f;%.2f;%.2f)',
          [vertexCount, pt.x, pt.y, pt.z], LM_Info);
        ExpandBBox(FBBoxMinInOCS, FBBoxMaxInOCS, pt, bbInitialized);
      end
      else if dxfLoadGroupCodeVertex(rdr, 13, byt, pt) then begin
        Inc(vertexCount);
        programlog.LogOutFormatStr(
          'uzeentacdproxy: LoadFromDXF vertex code 13 processed #%d pt=(%.2f;%.2f;%.2f)',
          [vertexCount, pt.x, pt.y, pt.z], LM_Info);
        ExpandBBox(FBBoxMinInOCS, FBBoxMaxInOCS, pt, bbInitialized);
      end
      { Код 92 - размер бинарных данных }
      else if byt = 92 then begin
        programlog.LogOutFormatStr('uzeentacdproxy: LoadFromDXF skip code=92 (binary size=%d)', [rdr.ParseInteger], LM_Info);
      end
      { Код 310 - бинарные данные прокси-объекта (hex-строка) }
      else if byt = 310 then begin
        hexData := rdr.ParseString;
        { Обрабатываем первую hex-строку через универсальный парсер }
        if not firstHexProcessed then begin
          { Создаем и инициализируем парсер }
          parser := TProxyObjectParser.Create;
          try
            if parser.InitFromHex(hexData) then begin
              parseResult := parser.Parse;
              
              if parseResult.Valid then begin
                { Сохраняем BBox из парсера }
                FBBoxMinInOCS := parseResult.BBoxMin;
                FBBoxMaxInOCS := parseResult.BBoxMax;
                bbInitialized := True;
                FBBoxLoaded := True;
                
                { Сохраняем меш для отрисовки }
                FMeshData := parser.GetMeshForDisplay;
                
                programlog.LogOutFormatStr(
                  'uzeentacdproxy: LoadFromDXF UNIVERSAL PARSER Success - GeometryType=%d Vertices=%d Faces=%d BBox Min=(%.2f;%.2f;%.2f) Max=(%.2f;%.2f;%.2f)',
                  [Ord(parseResult.GeometryType), 
                   IfThen(Assigned(FMeshData), FMeshData.VertexCount, 0),
                   IfThen(Assigned(FMeshData), FMeshData.FaceCount, 0),
                   FBBoxMinInOCS.x, FBBoxMinInOCS.y, FBBoxMinInOCS.z,
                   FBBoxMaxInOCS.x, FBBoxMaxInOCS.y, FBBoxMaxInOCS.z],
                  LM_Info);
              end else begin
                programlog.LogOutFormatStr('uzeentacdproxy: LoadFromDXF UNIVERSAL PARSER failed - no valid geometry', [], LM_Info);
              end;
            end else begin
              programlog.LogOutFormatStr('uzeentacdproxy: LoadFromDXF UNIVERSAL PARSER init failed', [], LM_Info);
            end;
          finally
            parser.Free;
          end;
          
          firstHexProcessed := True;
        end else begin
          programlog.LogOutFormatStr('uzeentacdproxy: LoadFromDXF skip code=310 (continuation)', [], LM_Info);
        end;
      end
      else begin
        { Любые другие коды — пропускаем значение }
        programlog.LogOutFormatStr('uzeentacdproxy: LoadFromDXF skip code=%d', [byt], LM_Info);
        rdr.ParseString;
      end;
    end;
    byt := rdr.ParseInteger;
  end;

  { Fallback: если парсер не сработал, пробуем старый метод }
  if not FBBoxLoaded and (vertexCount > 0) then begin
    FBBoxLoaded := bbInitialized;
    programlog.LogOutFormatStr(
      'uzeentacdproxy: LoadFromDXF END (fallback) vertexCount=%d BBoxLoaded=%d Min=(%.2f;%.2f;%.2f) Max=(%.2f;%.2f;%.2f)',
      [vertexCount, Ord(FBBoxLoaded),
       FBBoxMinInOCS.x, FBBoxMinInOCS.y, FBBoxMinInOCS.z,
       FBBoxMaxInOCS.x, FBBoxMaxInOCS.y, FBBoxMaxInOCS.z],
      LM_Info);
  end else begin
    programlog.LogOutFormatStr(
      'uzeentacdproxy: LoadFromDXF END vertexCount=%d BBoxLoaded=%d Min=(%.2f;%.2f;%.2f) Max=(%.2f;%.2f;%.2f)',
      [vertexCount, Ord(FBBoxLoaded),
       FBBoxMinInOCS.x, FBBoxMinInOCS.y, FBBoxMinInOCS.z,
       FBBoxMaxInOCS.x, FBBoxMaxInOCS.y, FBBoxMaxInOCS.z],
      LM_Info);
  end;
end;

{ Сохраняет прокси-объект в DXF.
  Пишет заголовок ACAD_PROXY_ENTITY с координатами двух угловых точек
  габаритной рамки. }
procedure GDBObjAcdProxy.SaveToDXF(var outStream: TZctnrVectorBytes;
  var drawing: TDrawingDef; var IODXFContext: TIODXFSaveContext);
begin
  SaveToDXFObjPrefix(outStream, 'ACAD_PROXY_ENTITY', 'AcDbProxyEntity',
    IODXFContext);
  { Сохраняем обе угловые точки габаритной рамки }
  if FBBoxLoaded then begin
    dxfvertexout(outStream, 10, FBBoxMinInOCS);
    dxfvertexout(outStream, 11, FBBoxMaxInOCS);
  end;
end;

{ Отрисовывает одно ребро габаритной рамки в Representation }
procedure GDBObjAcdProxy.DrawBBoxEdge(var DC: TDrawContext;
  const ptFrom, ptTo: TzePoint3d);
begin
  Representation.DrawLineWithoutLT(DC, ptFrom, ptTo);
end;

{ Отрисовывает меш прокси-объекта в Representation }
procedure GDBObjAcdProxy.DrawProxyMesh(var DC: TDrawContext);
var
  i: Integer;
  v1, v2, v3: TzePoint3d;
  face: TProxyFace;
begin
  if FMeshData = nil then
    Exit;
  
  { Рисуем треугольники меша }
  for i := 0 to FMeshData.FaceCount - 1 do begin
    face := FMeshData.Faces[i];
    if face.VertexCount = 3 then begin
      v1.x := FMeshData.Vertices[face.VertexIndices[0]].X;
      v1.y := FMeshData.Vertices[face.VertexIndices[0]].Y;
      v1.z := FMeshData.Vertices[face.VertexIndices[0]].Z;
      v2.x := FMeshData.Vertices[face.VertexIndices[1]].X;
      v2.y := FMeshData.Vertices[face.VertexIndices[1]].Y;
      v2.z := FMeshData.Vertices[face.VertexIndices[1]].Z;
      v3.x := FMeshData.Vertices[face.VertexIndices[2]].X;
      v3.y := FMeshData.Vertices[face.VertexIndices[2]].Y;
      v3.z := FMeshData.Vertices[face.VertexIndices[2]].Z;

      Representation.DrawLineWithoutLT(DC, v1, v2);
      Representation.DrawLineWithoutLT(DC, v2, v3);
      Representation.DrawLineWithoutLT(DC, v3, v1);
    end;
  end;
end;

{ Рассчитывает визуальное представление прокси-объекта.
  На этапе EFCalcEntityCS: пересчитываем BB из хранимых точек OCS.
  На этапе EFDraw:
    - Если есть меш из универсального парсера → рисуем геометрию
    - Иначе → рисуем 12 рёбер габаритной рамки (fallback) }
procedure GDBObjAcdProxy.FormatEntity(var drawing: TDrawingDef;
  var DC: TDrawContext; Stage: TEFStages);
var
  { 8 вершин параллелепипеда: LBN = Left-Bottom-Near, RTF = Right-Top-Far }
  v000, v100, v010, v110: TzePoint3d; { нижние 4 вершины }
  v001, v101, v011, v111: TzePoint3d; { верхние 4 вершины }
  bbMin, bbMax: TzePoint3d;
begin
  if assigned(EntExtensions) then
    EntExtensions.RunOnBeforeEntityFormat(@self, drawing, DC);

  { --- Этап расчёта геометрии --- }
  if (Stage = EFAllStages) or (EFCalcEntityCS in Stage) then begin
    { Устанавливаем BoundingBox для системы видимости и выбора }
    if FBBoxLoaded then begin
      vp.BoundingBox.LBN := FBBoxMinInOCS;
      vp.BoundingBox.RTF := FBBoxMaxInOCS;
    end else begin
      { Если нет данных — нулевой BB в начале координат }
      vp.BoundingBox.LBN := NulVertex;
      vp.BoundingBox.RTF := NulVertex;
    end;
    CalcActualVisible(DC.DrawingContext.VActuality);
  end;

  { --- Этап отрисовки --- }
  if ((Stage = EFAllStages) or (EFDraw in Stage))
    and (not (ESTemp in State))
    and (DCODrawable in DC.Options)
  then begin
    Representation.Clear;

    { Приоритет 1: Рисуем меш из универсального парсера }
    if (FMeshData <> nil) and (FMeshData.FaceCount > 0) then begin
      programlog.LogOutFormatStr('uzeentacdproxy: FormatEntity drawing MESH (Faces=%d)', [FMeshData.FaceCount], LM_Info);
      DrawProxyMesh(DC);
    end
    { Приоритет 2: Fallback - рисуем BBox }
    else if FBBoxLoaded then begin
      bbMin := FBBoxMinInOCS;
      bbMax := FBBoxMaxInOCS;

      { Вычисляем 8 вершин ограничивающего параллелепипеда }
      v000.x := bbMin.x; v000.y := bbMin.y; v000.z := bbMin.z;
      v100.x := bbMax.x; v100.y := bbMin.y; v100.z := bbMin.z;
      v010.x := bbMin.x; v010.y := bbMax.y; v010.z := bbMin.z;
      v110.x := bbMax.x; v110.y := bbMax.y; v110.z := bbMin.z;
      v001.x := bbMin.x; v001.y := bbMin.y; v001.z := bbMax.z;
      v101.x := bbMax.x; v101.y := bbMin.y; v101.z := bbMax.z;
      v011.x := bbMin.x; v011.y := bbMax.y; v011.z := bbMax.z;
      v111.x := bbMax.x; v111.y := bbMax.y; v111.z := bbMax.z;

      { Нижняя грань }
      DrawBBoxEdge(DC, v000, v100);
      DrawBBoxEdge(DC, v100, v110);
      DrawBBoxEdge(DC, v110, v010);
      DrawBBoxEdge(DC, v010, v000);

      { Верхняя грань }
      DrawBBoxEdge(DC, v001, v101);
      DrawBBoxEdge(DC, v101, v111);
      DrawBBoxEdge(DC, v111, v011);
      DrawBBoxEdge(DC, v011, v001);

      { Вертикальные рёбра }
      DrawBBoxEdge(DC, v000, v001);
      DrawBBoxEdge(DC, v100, v101);
      DrawBBoxEdge(DC, v110, v111);
      DrawBBoxEdge(DC, v010, v011);
      
      programlog.LogOutFormatStr('uzeentacdproxy: FormatEntity drawing BBOX (fallback)', [], LM_Info);
    end;
  end;

  if assigned(EntExtensions) then
    EntExtensions.RunOnAfterEntityFormat(@self, drawing, DC);
end;

{ Прямая отрисовка 12 рёбер габаритной рамки через drawer.
  Вызывается при перемещении/копировании, когда Representation устарело. }
procedure GDBObjAcdProxy.DrawGeometry(lw: integer; var DC: TDrawContext;
  const inFrustumState: TInBoundingVolume);
var
  v000, v100, v010, v110: TzePoint3d;
  v001, v101, v011, v111: TzePoint3d;
  bbMin, bbMax: TzePoint3d;
begin
  if not FBBoxLoaded then
    Exit;

  bbMin := FBBoxMinInOCS;
  bbMax := FBBoxMaxInOCS;

  v000.x := bbMin.x; v000.y := bbMin.y; v000.z := bbMin.z;
  v100.x := bbMax.x; v100.y := bbMin.y; v100.z := bbMin.z;
  v010.x := bbMin.x; v010.y := bbMax.y; v010.z := bbMin.z;
  v110.x := bbMax.x; v110.y := bbMax.y; v110.z := bbMin.z;
  v001.x := bbMin.x; v001.y := bbMin.y; v001.z := bbMax.z;
  v101.x := bbMax.x; v101.y := bbMin.y; v101.z := bbMax.z;
  v011.x := bbMin.x; v011.y := bbMax.y; v011.z := bbMax.z;
  v111.x := bbMax.x; v111.y := bbMax.y; v111.z := bbMax.z;

  { Нижняя грань }
  DC.drawer.DrawLine3DInModelSpace(v000, v100, DC.DrawingContext.matrixs);
  DC.drawer.DrawLine3DInModelSpace(v100, v110, DC.DrawingContext.matrixs);
  DC.drawer.DrawLine3DInModelSpace(v110, v010, DC.DrawingContext.matrixs);
  DC.drawer.DrawLine3DInModelSpace(v010, v000, DC.DrawingContext.matrixs);

  { Верхняя грань }
  DC.drawer.DrawLine3DInModelSpace(v001, v101, DC.DrawingContext.matrixs);
  DC.drawer.DrawLine3DInModelSpace(v101, v111, DC.DrawingContext.matrixs);
  DC.drawer.DrawLine3DInModelSpace(v111, v011, DC.DrawingContext.matrixs);
  DC.drawer.DrawLine3DInModelSpace(v011, v001, DC.DrawingContext.matrixs);

  { Вертикальные рёбра }
  DC.drawer.DrawLine3DInModelSpace(v000, v001, DC.DrawingContext.matrixs);
  DC.drawer.DrawLine3DInModelSpace(v100, v101, DC.DrawingContext.matrixs);
  DC.drawer.DrawLine3DInModelSpace(v110, v111, DC.DrawingContext.matrixs);
  DC.drawer.DrawLine3DInModelSpace(v010, v011, DC.DrawingContext.matrixs);
end;

{ Вычисляет попадание объекта в усечённую пирамиду по BB }
function GDBObjAcdProxy.CalcTrueInFrustum(
  const frustum: TzeFrustum): TInBoundingVolume;
begin
  Result := CalcAABBInFrustum(vp.BoundingBox, frustum);
end;

{ Применяет матрицу трансформации: пересчитывает угловые точки BB }
procedure GDBObjAcdProxy.TransformAt(p: PGDBObjEntity;
  t_matrix: PzeTypedMatrix4d);
var
  src: PGDBObjAcdProxy;
begin
  src := PGDBObjAcdProxy(p);
  FBBoxMinInOCS := VectorTransform3D(src^.FBBoxMinInOCS, t_matrix^);
  FBBoxMaxInOCS := VectorTransform3D(src^.FBBoxMaxInOCS, t_matrix^);
  FBBoxLoaded   := src^.FBBoxLoaded;
end;

function GDBObjAcdProxy.GetObjTypeName: string;
begin
  Result := ObjN_GDBObjAcdProxy;
end;

function GDBObjAcdProxy.GetObjType: TObjID;
begin
  Result := GDBAcdProxyID;
end;

{ Создаёт полную копию прокси-объекта }
function GDBObjAcdProxy.Clone(own: Pointer): PGDBObjEntity;
var
  newProxy: PGDBObjAcdProxy;
begin
  GetMem(Pointer(newProxy), SizeOf(GDBObjAcdProxy));
  newProxy^.init(own, vp.Layer, vp.LineWeight);
  CopyVPto(newProxy^);
  CopyExtensionsTo(newProxy^);
  newProxy^.FBBoxMinInOCS := FBBoxMinInOCS;
  newProxy^.FBBoxMaxInOCS := FBBoxMaxInOCS;
  newProxy^.FBBoxLoaded   := FBBoxLoaded;
  newProxy^.bp.ListPos.owner := own;
  Result := newProxy;
end;

{ Прокси-объект не имеет редактируемых контрольных точек на этапе 1 }
procedure GDBObjAcdProxy.addcontrolpoints(tdesc: Pointer);
begin
  { Контрольные точки не добавляются — объект не редактируется }
end;

procedure GDBObjAcdProxy.rtsave(refp: Pointer);
begin
  { Нет состояния для сохранения при real-time модификации }
end;

procedure GDBObjAcdProxy.rtmodifyonepoint(const rtmod: TRTModifyData);
begin
  { Real-time модификация точек не реализована в этапе 1 }
end;

function GDBObjAcdProxy.IsRTNeedModify(const Point: PControlPointDesc;
  p: Pointer): boolean;
begin
  Result := False;
end;

procedure GDBObjAcdProxy.remaponecontrolpoint(pdesc: pcontrolpointdesc;
  ProjectProc: GDBProjectProc);
begin
  { Контрольных точек нет — ничего не делаем }
end;

{ Вычисляет экранные габариты объекта на основе BB }
procedure GDBObjAcdProxy.getoutbound(var DC: TDrawContext);
begin
  vp.BoundingBox.LBN := FBBoxMinInOCS;
  vp.BoundingBox.RTF := FBBoxMaxInOCS;
end;

destructor GDBObjAcdProxy.done;
begin
  FMeshData.Free;
  inherited;
end;

class function GDBObjAcdProxy.CreateInstance: PGDBObjAcdProxy;
begin
  Result := AllocAndInitAcdProxy(nil);
end;

{ --- Фабричные функции --- }

function AllocAcdProxy: Pointer;
begin
  GetMem(Pointer(Result), SizeOf(GDBObjAcdProxy));
end;

function AllocAndInitAcdProxy(
  owner: PGDBObjGenericWithSubordinated): PGDBObjAcdProxy;
begin
  GetMem(Pointer(Result), SizeOf(GDBObjAcdProxy));
  Result^.initnul(owner);
  Result^.bp.ListPos.Owner := owner;
end;

{ --- Регистрация --- }
begin
  { Регистрируем как DXF-сущность: ACAD_PROXY_ENTITY будет создавать этот класс }
  RegisterDXFEntity(
    GDBAcdProxyID,
    'ACAD_PROXY_ENTITY',
    ObjN_GDBObjAcdProxy,
    @AllocAcdProxy,
    @AllocAndInitAcdProxy
  );
end.
