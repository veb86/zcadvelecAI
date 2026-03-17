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
  SysUtils;

type
  PGDBObjAcdProxy = ^GDBObjAcdProxy;

  { Прокси-объект AutoCAD (ACAD_PROXY_ENTITY).
    Этап 1: отображается как габаритная рамка.
    Хранит точки минимума (LBN) и максимума (RTF) из DXF-данных объекта. }
  GDBObjAcdProxy = object(GDBObj3d)
  private
    { Координата минимального угла габаритной рамки в OCS }
    FBBoxMinInOCS: TzePoint3d;
    { Координата максимального угла габаритной рамки в OCS }
    FBBoxMaxInOCS: TzePoint3d;
    { Флаг: данные габаритной рамки успешно загружены }
    FBBoxLoaded: Boolean;

    { Отрисовывает одно ребро габаритной рамки }
    procedure DrawBBoxEdge(var DC: TDrawContext;
      const ptFrom, ptTo: TzePoint3d);

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
end;

constructor GDBObjAcdProxy.initnul(owner: PGDBObjGenericWithSubordinated);
begin
  inherited initnul(owner);
  FBBoxMinInOCS := NulVertex;
  FBBoxMaxInOCS := NulVertex;
  FBBoxLoaded   := False;
end;

{ Загружает прокси-объект из DXF.
  Читает базовые свойства (слой, цвет) через LoadFromDXFObjShared.
  Все вершины из кодов 10–39 используются для построения габаритной рамки.
  Неизвестные коды пропускаются. }
procedure GDBObjAcdProxy.LoadFromDXF(var rdr: TZMemReader;
  ptu: PExtensionData; var drawing: TDrawingDef;
  var context: TIODXFLoadContext);
var
  byt: Integer;
  pt:  TzePoint3d;
  bbInitialized: Boolean;
begin
  bbInitialized := False;
  FBBoxMinInOCS := NulVertex;
  FBBoxMaxInOCS := NulVertex;
  FBBoxLoaded   := False;

  byt := rdr.ParseInteger;
  while byt <> 0 do begin
    { Общие свойства: слой, цвет, линия и прочее }
    if not LoadFromDXFObjShared(rdr, byt, ptu, drawing, context) then begin
      { Вершины из кода 10 (X) — группы 10..39 задают точки объекта.
        Используем их для построения ограничивающего параллелепипеда. }
      if dxfLoadGroupCodeVertex(rdr, 10, byt, pt) then
        ExpandBBox(FBBoxMinInOCS, FBBoxMaxInOCS, pt, bbInitialized)
      else if dxfLoadGroupCodeVertex(rdr, 11, byt, pt) then
        ExpandBBox(FBBoxMinInOCS, FBBoxMaxInOCS, pt, bbInitialized)
      else if dxfLoadGroupCodeVertex(rdr, 12, byt, pt) then
        ExpandBBox(FBBoxMinInOCS, FBBoxMaxInOCS, pt, bbInitialized)
      else if dxfLoadGroupCodeVertex(rdr, 13, byt, pt) then
        ExpandBBox(FBBoxMinInOCS, FBBoxMaxInOCS, pt, bbInitialized)
      else
        { Любые другие коды — пропускаем значение }
        rdr.ParseString;
    end;
    byt := rdr.ParseInteger;
  end;

  FBBoxLoaded := bbInitialized;
  programlog.LogOutFormatStr(
    'uzeentacdproxy: LoadFromDXF BBoxLoaded=%d Min=(%.2f;%.2f;%.2f) Max=(%.2f;%.2f;%.2f)',
    [Ord(FBBoxLoaded),
     FBBoxMinInOCS.x, FBBoxMinInOCS.y, FBBoxMinInOCS.z,
     FBBoxMaxInOCS.x, FBBoxMaxInOCS.y, FBBoxMaxInOCS.z],
    LM_Info);
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

{ Рассчитывает визуальное представление прокси-объекта.
  На этапе EFCalcEntityCS: пересчитываем BB из хранимых точек OCS.
  На этапе EFDraw: рисуем 12 рёбер габаритной рамки в Representation. }
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

    if FBBoxLoaded then begin
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
