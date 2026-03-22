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
  
  НОВАЯ АРХИТЕКТУРА (модульная):
  - Использует TProxyPrimitiveManager для регистрации парсеров
  - Каждый примитив в отдельном модуле (uzeentproxyparsercircle.pas и др.)
  - Легко отключить примитив - исключить файл из проекта
  - Обратная совместимость через старый механизм удалена
  
  Текущая реализация:
  - Поддерживает только круги (pptCircle)
  - Остальные примитивы будут добавлены в следующих задачах
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
  Math,
  uzeentproxystream,
  uzeentproxytypes,
  uzeentproxymanager,
  uzeentproxygraphicparser,
  uzeentproxybaseparser,
  uzeentproxyparsercircle,
  UGDBSelectedObjArray,
  uzesnap,
  uzegeomentitiestree,
  gzctnrVectorTypes,
  gzctnrVector,
  UGDBPoint3DArray,
  UGDBVisibleTreeArray;

type
  PGDBObjAcdProxy = ^GDBObjAcdProxy;

  { Прокси-объект AutoCAD (ACAD_PROXY_ENTITY).
    Отображает прокси-графику через парсинг AcGiWorldDraw команд.
    
    НОВАЯ АРХИТЕКТУРА:
    - Использует интерфейс IProxyPrimitiveParser для всех примитивов
    - Круги: uzeentproxyparsercircle.pas (реализовано)
    - Остальные примитивы: будут добавлены в следующих задачах
    }
  GDBObjAcdProxy = object(GDBObj3d)
  private
    { Координата минимального угла габаритной рамки в OCS }
    FBBoxMinInOCS: TzePoint3d;
    { Координата максимального угла габаритной рамки в OCS }
    FBBoxMaxInOCS: TzePoint3d;
    { Флаг: данные габаритной рамки успешно загружены }
    FBBoxLoaded: Boolean;
    
    { Бинарные данные прокси-графики }
    FProxyDataBytes: TBytes;
    
    { Центр объекта для контрольной точки }
    FCenterPoint: TzePoint3d;
    FHasCenterPoint: Boolean;
    
    { Вершины круга для отрисовки }
    FCircleVertices: GDBPoint3DArray;
    FHasCircleVertices: Boolean;

    { Отрисовывает одно ребро габаритной рамки }
    procedure DrawBBoxEdge(var DC: TDrawContext;
      const ptFrom, ptTo: TzePoint3d);

  public
    constructor init(own: Pointer; layeraddres: PGDBLayerProp; LW: smallint);
    constructor initnul(owner: PGDBObjGenericWithSubordinated);
    destructor done; virtual;

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
  end;

{ Выделяет память для нового прокси-объекта без инициализации }
function AllocAcdProxy: Pointer;

{ Выделяет и инициализирует новый прокси-объект }
function AllocAndInitAcdProxy(
  owner: PGDBObjGenericWithSubordinated): PGDBObjAcdProxy;

implementation

{ Вспомогательная функция для отображения байтов в hex }
function HexDisplay(const Bytes: TBytes; Count: Integer): string;
var
  I: Integer;
begin
  Result := '';
  if Count > Length(Bytes) then
    Count := Length(Bytes);
    
  for I := 0 to Count - 1 do
  begin
    if I > 0 then
      Result := Result + ' ';
    Result := Result + IntToHex(Bytes[I], 2);
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
  end else begin
    if pt.x < bbMin.x then bbMin.x := pt.x;
    if pt.y < bbMin.y then bbMin.y := pt.y;
    if pt.z < bbMin.z then bbMin.z := pt.z;
    if pt.x > bbMax.x then bbMax.x := pt.x;
    if pt.y > bbMax.y then bbMax.y := pt.y;
    if pt.z > bbMax.z then bbMax.z := pt.z;
  end;
end;

{ --- GDBObjAcdProxy --- }

constructor GDBObjAcdProxy.init;
begin
  inherited init(own, layeraddres, LW);
  FBBoxLoaded := False;
  FBBoxMinInOCS := NulVertex;
  FBBoxMaxInOCS := NulVertex;
  FHasCenterPoint := False;
  FHasCircleVertices := False;
  FCircleVertices.init(0);
end;

constructor GDBObjAcdProxy.initnul;
begin
  inherited initnul(owner);
  FBBoxLoaded := False;
  FBBoxMinInOCS := NulVertex;
  FBBoxMaxInOCS := NulVertex;
  FHasCenterPoint := False;
  FHasCircleVertices := False;
  FCircleVertices.init(0);
end;

destructor GDBObjAcdProxy.done;
begin
  FCircleVertices.done;
  inherited done;
end;

{ Отрисовывает одно ребро габаритной рамки }
procedure GDBObjAcdProxy.DrawBBoxEdge(var DC: TDrawContext;
  const ptFrom, ptTo: TzePoint3d);
begin
  DC.drawer.DrawLine3DInModelSpace(ptFrom, ptTo, DC.DrawingContext.matrixs);
end;

{ Загружает данные объекта из DXF-потока }
procedure GDBObjAcdProxy.LoadFromDXF(var rdr: TZMemReader; ptu: PExtensionData;
  var drawing: TDrawingDef; var context: TIODXFLoadContext);
var
  ProxyDataHex: string;
  ProxyDataBytes: TBytes;
  I, Len: Integer;
  Code: Integer;
begin
  { Читаем бинарные данные прокси-графики (код 310) и базовые свойства }
  ProxyDataHex := '';
  Code := rdr.ParseInteger;
  while Code <> 0 do begin
    case Code of
      310: begin  { Бинарные данные прокси-графики }
          ProxyDataHex := ProxyDataHex + rdr.ParseString;
        end;
    else
      { Пропускаем неизвестные коды }
      rdr.SkipString;
    end;
    Code := rdr.ParseInteger;
  end;

  { Конвертируем hex-строку в байты }
  if Length(ProxyDataHex) > 0 then begin
    Len := Length(ProxyDataHex) div 2;
    SetLength(FProxyDataBytes, Len);
    for I := 0 to Len - 1 do begin
      FProxyDataBytes[I] := Lo(StrToIntDef('$' + Copy(ProxyDataHex, I * 2 + 1, 2), 0));
    end;

    { Сохраняем данные для последующего парсинга }
    FBBoxLoaded := True;

    programlog.LogOutFormatStr('uzeentacdproxy: LoadFromDXF - Loaded proxy data (%d bytes)', [Len], LM_Info);
  end else begin
    { Нет данных - нулевой BBox }
    FBBoxLoaded := False;
    FBBoxMinInOCS := NulVertex;
    FBBoxMaxInOCS := NulVertex;
    
    programlog.LogOutFormatStr('uzeentacdproxy: LoadFromDXF - No proxy data found', [], LM_Warning);
  end;
end;

{ Сохраняет данные объекта в DXF-поток }
procedure GDBObjAcdProxy.SaveToDXF(var outStream: TZctnrVectorBytes;
  var drawing: TDrawingDef; var IODXFContext: TIODXFSaveContext);
begin
  inherited SaveToDXF(outStream, drawing, IODXFContext);
  { TODO: Сохранить бинарные данные прокси-графики }
end;

{ Рассчитывает визуальное представление объекта }
procedure GDBObjAcdProxy.FormatEntity(var drawing: TDrawingDef;
  var DC: TDrawContext; Stage: TEFStages);
var
  ProxyParser: TProxyGraphicParser;
  ParseResult: TProxyGraphicParseResult;
  ir2: itrec;
  pV2: PzePoint3d;
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

    { НОВАЯ АРХИТЕКТУРА: Отрисовка через менеджер парсеров }
    { Поддерживаются только круги (pptCircle) }
    
    programlog.LogOutFormatStr('uzeentacdproxy: FormatEntity - Using NEW modular architecture', [], LM_Info);
    
    if FBBoxLoaded and (Length(FProxyDataBytes) > 0) then
    begin
      programlog.LogOutFormatStr('uzeentacdproxy: FormatEntity - Using NEW ProxyGraphicParser', [], LM_Info);
      programlog.LogOutFormatStr('uzeentacdproxy: FormatEntity - Proxy data length: %d bytes', [Length(FProxyDataBytes)], LM_Info);
      
      { Выводим первые 32 байта для отладки }
      if Length(FProxyDataBytes) >= 32 then
      begin
        programlog.LogOutFormatStr('uzeentacdproxy: FormatEntity - First 32 bytes: %s', 
          [HexDisplay(FProxyDataBytes, 32)], LM_Info);
      end;
      
      { Создаем полный парсер Proxy Graphic }
      ProxyParser := TProxyGraphicParser.Create(FProxyDataBytes, Drawing, DC, Stage);
      try
        ParseResult := ProxyParser.Parse;
        
        if ParseResult.CircleCount > 0 then
        begin
          programlog.LogOutFormatStr('uzeentacdproxy: FormatEntity - Successfully parsed %d circles', [ParseResult.CircleCount], LM_Info);
        end
        else
        begin
          programlog.LogOutFormatStr('uzeentacdproxy: FormatEntity - No circles found in proxy data', [], LM_Warning);
        end;
        
        { Обновляем BBox из результата парсинга ВСЕГДА }
        if ParseResult.BBoxLoaded then
        begin
          FBBoxMinInOCS := ParseResult.BBoxMin;
          FBBoxMaxInOCS := ParseResult.BBoxMax;
          FBBoxLoaded := True;
          vp.BoundingBox.LBN := FBBoxMinInOCS;
          vp.BoundingBox.RTF := FBBoxMaxInOCS;
          
          { Устанавливаем центр BBox для контрольной точки }
          FCenterPoint.x := (FBBoxMinInOCS.x + FBBoxMaxInOCS.x) / 2;
          FCenterPoint.y := (FBBoxMinInOCS.y + FBBoxMaxInOCS.y) / 2;
          FCenterPoint.z := (FBBoxMinInOCS.z + FBBoxMaxInOCS.z) / 2;
          FHasCenterPoint := True;
          
          { Сохраняем вершины круга для отрисовки }
          if ParseResult.HasCircleVertices then
          begin
            FCircleVertices.init(ParseResult.CircleVertices.Count);
            pV2 := ParseResult.CircleVertices.beginiterate(ir2);
            while pV2 <> nil do
            begin
              FCircleVertices.PushBackData(pV2^);
              pV2 := ParseResult.CircleVertices.iterate(ir2);
            end;
            FHasCircleVertices := True;
          end;
          
          programlog.LogOutFormatStr('uzeentacdproxy: FormatEntity - BBox updated from proxy data, Center=(%.3f,%.3f,%.3f)', 
            [FCenterPoint.x, FCenterPoint.y, FCenterPoint.z], LM_Info);
        end;
      finally
        ProxyParser.Free;
      end;
    end
    else
    begin
      programlog.LogOutFormatStr('uzeentacdproxy: FormatEntity - No proxy data to parse', [], LM_Warning);
    end;
    
    { Круги будут отрисованы в DrawGeometry }
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
  
  { Рисуем круг если вершины загружены }
  if FHasCircleVertices and (FCircleVertices.Count > 0) then
  begin
    programlog.LogOutFormatStr('uzeentacdproxy: DrawGeometry - Drawing %d circle vertices', [FCircleVertices.Count], LM_Info);
    DC.drawer.DrawContour3DInModelSpace(FCircleVertices, DC.DrawingContext.matrixs, True);
  end;
  
  { Рисуем контрольную точку в центре }
  if FHasCenterPoint then
  begin
    DC.drawer.DrawPoint3DInModelSpace(FCenterPoint, DC.DrawingContext.matrixs);
  end;
end;

function GDBObjAcdProxy.CalcTrueInFrustum(
  const frustum: TzeFrustum): TInBoundingVolume;
var
  I: Integer;
  D1, D2: Double;
begin
  { Проверяем попадание BBox во frustum по 6 плоскостям }
  for I := 0 to 5 do begin
    { Проверяем оба угла BBox }
    D1 := frustum.v[I].v[0] * FBBoxMinInOCS.x + frustum.v[I].v[1] * FBBoxMinInOCS.y +
          frustum.v[I].v[2] * FBBoxMinInOCS.z + frustum.v[I].v[3];
    D2 := frustum.v[I].v[0] * FBBoxMaxInOCS.x + frustum.v[I].v[1] * FBBoxMaxInOCS.y +
          frustum.v[I].v[2] * FBBoxMaxInOCS.z + frustum.v[I].v[3];
    
    { Если оба угла снаружи плоскости - BBox вне frustum }
    if (D1 < 0) and (D2 < 0) then
    begin
      Result := IREmpty;
      Exit;
    end;
  end;
  
  Result := IRFully;
end;

procedure GDBObjAcdProxy.TransformAt(p: PGDBObjEntity;
  t_matrix: PzeTypedMatrix4d);
begin
  { TODO: Применить матрицу трансформации }
end;

function GDBObjAcdProxy.GetObjTypeName: string;
begin
  Result := 'ACAD_PROXY_ENTITY';
end;

function GDBObjAcdProxy.GetObjType: TObjID;
begin
  Result := GDBAcdProxyID;
end;

function GDBObjAcdProxy.Clone(own: Pointer): PGDBObjEntity;
begin
  Result := CreateInstance;
  { TODO: Скопировать данные }
end;

procedure GDBObjAcdProxy.addcontrolpoints(tdesc: Pointer);
var
  pdesc: controlpointdesc;
begin
  if not FHasCenterPoint then
    Exit;
    
  { Создаем одну контрольную точку в центре }
  PSelectedObjDesc(tdesc)^.pcontrolpoint^.init(1);
  
  pdesc.selected := False;
  pdesc.PDrawable := nil;
  pdesc.pointtype := os_begin;  // Используем os_begin как тип точки
  pdesc.attr := [CPA_Strech];
  pdesc.worldcoord := FCenterPoint;
  
  PSelectedObjDesc(tdesc)^.pcontrolpoint^.PushBackData(pdesc);
  
  programlog.LogOutFormatStr('uzeentacdproxy: Added control point at (%.3f, %.3f, %.3f)', 
    [FCenterPoint.x, FCenterPoint.y, FCenterPoint.z], LM_Info);
end;

procedure GDBObjAcdProxy.rtsave(refp: Pointer);
begin
  { Прокси-объект не поддерживает real-time модификацию }
end;

procedure GDBObjAcdProxy.rtmodifyonepoint(const rtmod: TRTModifyData);
begin
  { Прокси-объект не поддерживает real-time модификацию }
end;

function GDBObjAcdProxy.IsRTNeedModify(const Point: PControlPointDesc;
  p: Pointer): boolean;
begin
  Result := False;
end;

procedure GDBObjAcdProxy.remaponecontrolpoint(pdesc: pcontrolpointdesc;
  ProjectProc: GDBProjectProc);
var
  tv: TzePoint3d;
begin
  if FHasCenterPoint then
  begin
    pdesc^.worldcoord := FCenterPoint;
    ProjectProc(pdesc^.worldcoord, tv);
    pdesc^.dispcoord := ToTzePoint2i(tv);
  end;
end;

procedure GDBObjAcdProxy.getoutbound(var DC: TDrawContext);
begin
  { Вычисляем габаритный прямоугольник в экранных координатах }
  if FBBoxLoaded then begin
    { TODO: Спроецировать BBox на экран }
  end;
end;

class function GDBObjAcdProxy.CreateInstance: PGDBObjAcdProxy;
begin
  Result := AllocAcdProxy;
  Result^.initnul(nil);
end;

function AllocAcdProxy: Pointer;
begin
  GetMem(Result, SizeOf(GDBObjAcdProxy));
end;

function AllocAndInitAcdProxy(
  owner: PGDBObjGenericWithSubordinated): PGDBObjAcdProxy;
begin
  Result := AllocAcdProxy;
  Result^.initnul(owner);
end;

initialization
  { Регистрация прокси-объекта в фабрике сущностей }
  RegisterDXFEntity(
    GDBAcdProxyID,
    'ACAD_PROXY_ENTITY',
    'ProxyEntity',
    @AllocAcdProxy,
    @AllocAndInitAcdProxy
  );
  
  programlog.LogOutFormatStr('uzeentacdproxy: Registered ACAD_PROXY_ENTITY (NEW modular architecture)', [], LM_Info);
  programlog.LogOutFormatStr('uzeentacdproxy: Registered primitives count: %d', [TProxyPrimitiveManager.GetRegisteredCount], LM_Info);
  
end.
