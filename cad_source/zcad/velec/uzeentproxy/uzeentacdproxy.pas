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
  Модуль: uzeentacdproxy
  Назначение: Поддержка прокси-объектов AutoCAD (ACAD_PROXY_ENTITY) в ZCAD.
  
  Новая архитектура (на основе анализа ezdxf):
  1. Загружает бинарные данные из DXF (код 310)
  2. Парсит через TProxyGraphicParser (AcGiWorldDraw команды)
  3. Создаёт виртуальные сущности ZCAD (круг, дуга, текст, полилиния)
  4. Отрисовывает через FormatEntity
  
  Поддерживаемые команды (OPCODE):
  - 2: Circle → PGDBObjCircle
  - 4: CircularArc → PGDBObjArc
  - 6: Polyline → PGDBObjPolyline
  - 7: Polygon → PGDBObjPolyline (closed)
  - 10, 11, 36: Text → PGDBObjText / PGDBObjMText
  - 44: EllipticArc → PGDBObjEllipse
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
  uzeentproxyparser,
  uzeentproxytypes,
  uzeentcircle,
  uzeentarc,
  uzeentpolyline,
  uzeentlwpolyline,
  uzeenttext,
  uzeentmtext,
  uzeentellipse;

type
  PGDBObjAcdProxy = ^GDBObjAcdProxy;

  { Прокси-объект AutoCAD (ACAD_PROXY_ENTITY).
    Новая архитектура:
    - Парсит AcGiWorldDraw команды из бинарных данных
    - Создаёт виртуальные сущности ZCAD
    - Отрисовывает через стандартный механизм FormatEntity }
  GDBObjAcdProxy = object(GDBObj3d)
  private
    FBBoxMinInOCS: TzePoint3d;
    FBBoxMaxInOCS: TzePoint3d;
    FBBoxLoaded: Boolean;

    { Виртуальные сущности }
    FVirtualEntities: array of PGDBObjEntity;
    FEntityCount: Integer;

    { Слой по умолчанию для сущностей }
    FDefaultLayer: PGDBLayerProp;

    { Отрисовка виртуальных сущностей }
    procedure DrawVirtualEntities(var DC: TDrawContext; var drawing: TDrawingDef);

    { Вычисление BBox из виртуальных сущностей }
    procedure CalcBBoxFromEntities;

    { Конвертация результата парсинга в сущность ZCAD }
    function ConvertResultToEntity(const CmdResult: TProxyCommandResult; var drawing: TDrawingDef): PGDBObjEntity;
    
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
    
    { Отрисовывает геометрию объекта напрямую }
    procedure DrawGeometry(lw: integer; var DC: TDrawContext;
      const inFrustumState: TInBoundingVolume); virtual;
    
    { Вычисляет попадание во фрустум }
    function CalcTrueInFrustum(const frustum: TzeFrustum): TInBoundingVolume; virtual;
    
    { Применяет матрицу трансформации }
    procedure TransformAt(p: PGDBObjEntity; t_matrix: PzeTypedMatrix4d); virtual;
    
    { Возвращает наименование типа объекта }
    function GetObjTypeName: string; virtual;
    
    { Возвращает числовой идентификатор типа объекта }
    function GetObjType: TObjID; virtual;
    
    { Создаёт копию объекта }
    function Clone(own: Pointer): PGDBObjEntity; virtual;
    
    { Освобождает память }
    destructor done; virtual;
    
    { Публичные методы для доступа к виртуальным сущностям }
    function GetVirtualEntity(Index: Integer): PGDBObjEntity;
    function GetVirtualEntityCount: Integer;

    { Взрывает прокси-объект (заменяет виртуальными сущностями) }
    // procedure ExplodeToVirtualEntities(TargetLayout: PGDBLayout); // TODO: реализовать, когда PGDBLayout будет определён

    { Создаёт новый инициализированный экземпляр прокси-объекта }
    class function CreateInstance: PGDBObjAcdProxy; static;
  end;

{ Выделяет память для нового прокси-объекта }
function AllocAcdProxy: Pointer;

{ Выделяет и инициализирует новый прокси-объект }
function AllocAndInitAcdProxy(owner: PGDBObjGenericWithSubordinated): PGDBObjAcdProxy;

implementation

{ === Локальные функции создания сущностей === }
{ Создаём сущности напрямую, т.к. Alloc* функции не экспортированы }

function AllocAndInitCircleProxy(owner: PGDBObjGenericWithSubordinated): PGDBObjCircle;
begin
  GetMem(Pointer(Result), SizeOf(GDBObjCircle));
  Result^.initnul;
  if owner <> nil then
    Result^.bp.ListPos.Owner := owner;
end;

function AllocAndInitArcProxy(owner: PGDBObjGenericWithSubordinated): PGDBObjArc;
begin
  GetMem(Pointer(Result), SizeOf(GDBObjArc));
  Result^.initnul;
  if owner <> nil then
    Result^.bp.ListPos.Owner := owner;
end;

function AllocAndInitPolylineProxy(owner: PGDBObjGenericWithSubordinated): PGDBObjPolyline;
begin
  GetMem(Pointer(Result), SizeOf(GDBObjPolyline));
  Result^.initnul(owner);
end;

function AllocAndInitTextProxy(owner: PGDBObjGenericWithSubordinated): PGDBObjText;
begin
  GetMem(Pointer(Result), SizeOf(GDBObjText));
  Result^.initnul(owner);
end;

function AllocAndInitEllipseProxy(owner: PGDBObjGenericWithSubordinated): PGDBObjEllipse;
begin
  GetMem(Pointer(Result), SizeOf(GDBObjEllipse));
  Result^.initnul;
  if owner <> nil then
    Result^.bp.ListPos.Owner := owner;
end;

{ === GDBObjAcdProxy === }

constructor GDBObjAcdProxy.init(own: Pointer; layeraddres: PGDBLayerProp; LW: smallint);
begin
  inherited init(own, layeraddres, LW);
  FBBoxMinInOCS := NulVertex;
  FBBoxMaxInOCS := NulVertex;
  FBBoxLoaded := False;
  FEntityCount := 0;
  FDefaultLayer := layeraddres;
  SetLength(FVirtualEntities, 0);
end;

constructor GDBObjAcdProxy.initnul(owner: PGDBObjGenericWithSubordinated);
begin
  inherited initnul(owner);
  FBBoxMinInOCS := NulVertex;
  FBBoxMaxInOCS := NulVertex;
  FBBoxLoaded := False;
  FEntityCount := 0;
  SetLength(FVirtualEntities, 0);
end;

destructor GDBObjAcdProxy.done;
var
  I: Integer;
begin
  // Освобождаем виртуальные сущности
  for I := 0 to FEntityCount - 1 do begin
    if FVirtualEntities[I] <> nil then
      FVirtualEntities[I]^.done;
  end;
  
  SetLength(FVirtualEntities, 0);
  FEntityCount := 0;
  
  inherited done;
end;

{ Конвертация результата парсинга в сущность ZCAD }
function GDBObjAcdProxy.ConvertResultToEntity(const CmdResult: TProxyCommandResult; var drawing: TDrawingDef): PGDBObjEntity;
var
  Circle: PGDBObjCircle;
  Arc: PGDBObjArc;
  Polyline: PGDBObjPolyline;
  TextEntity: PGDBObjText;
  Ellipse: PGDBObjEllipse;
  I: Integer;
begin
  Result := nil;
  
  if not CmdResult.Valid then
    Exit;
  
  case CmdResult.PrimitiveType of
    pptCircle:
      begin
        Circle := AllocAndInitCircleProxy(nil);
        Circle^.Local.p_insert := CmdResult.CircleData.Center;
        Circle^.Radius := CmdResult.CircleData.Radius;
        Result := PGDBObjEntity(Circle);
      end;

    pptArc:
      begin
        Arc := AllocAndInitArcProxy(nil);
        Arc^.Local.p_insert := CmdResult.ArcData.Center;
        Arc^.R := CmdResult.ArcData.Radius;
        // Вычисляем углы из start vector и sweep angle
        Arc^.StartAngle := RadToDeg(ArcTan2(CmdResult.ArcData.StartVector.Y, CmdResult.ArcData.StartVector.X));
        Arc^.EndAngle := Arc^.StartAngle + RadToDeg(CmdResult.ArcData.SweepAngle);
        Result := PGDBObjEntity(Arc);
      end;

    pptPolyline, pptPolygon:
      begin
        Polyline := AllocAndInitPolylineProxy(nil);
        if CmdResult.PrimitiveType = pptPolygon then
          Polyline^.Closed := True
        else
          Polyline^.Closed := CmdResult.PolylineData.Closed;

        // Добавляем вершины
        for I := 0 to High(CmdResult.PolylineData.Vertices) do
          Polyline^.AddVertex(CmdResult.PolylineData.Vertices[I]);

        Result := PGDBObjEntity(Polyline);
      end;

    pptText:
      begin
        TextEntity := AllocAndInitTextProxy(nil);
        TextEntity^.P_drawInOCS := CmdResult.TextData.Insert;
        TextEntity^.Content := CmdResult.TextData.Text;
        TextEntity^.obj_height := CmdResult.TextData.Height;
        TextEntity^.setrot(RadToDeg(ArcTan2(CmdResult.TextData.Direction.Y, CmdResult.TextData.Direction.X)));
        Result := PGDBObjEntity(TextEntity);
      end;

    pptEllipse:
      begin
        Ellipse := AllocAndInitEllipseProxy(nil);
        Ellipse^.Local.p_insert := CmdResult.EllipticArcData.Center;
        // Умножаем вектор направления на длину большой оси
        with VectorNormalize(CmdResult.EllipticArcData.MajorAxisDirection) do begin
          Ellipse^.MajorAxis.x := CmdResult.EllipticArcData.MajorAxisLength * x;
          Ellipse^.MajorAxis.y := CmdResult.EllipticArcData.MajorAxisLength * y;
          Ellipse^.MajorAxis.z := CmdResult.EllipticArcData.MajorAxisLength * z;
        end;
        Ellipse^.Ratio := CmdResult.EllipticArcData.MinorAxisLength / CmdResult.EllipticArcData.MajorAxisLength;
        Ellipse^.StartAngle := CmdResult.EllipticArcData.StartParam;
        Ellipse^.EndAngle := CmdResult.EllipticArcData.EndParam;
        Result := PGDBObjEntity(Ellipse);
      end;
  end;

  // Применяем атрибуты (слой, цвет)
  if Result <> nil then begin
    Result^.vp.Layer := FDefaultLayer;
    // Цвет пока не применяем, так как FState недоступен
  end;
end;

{ Загружает прокси-объект из DXF }
procedure GDBObjAcdProxy.LoadFromDXF(var rdr: TZMemReader;
  ptu: PExtensionData; var drawing: TDrawingDef;
  var context: TIODXFLoadContext);
var
  HexData: string;
  Parser: TProxyGraphicParser;
  I: Integer;
  CmdResult: TProxyCommandResult;
  Entity: PGDBObjEntity;
  byt: Integer;
begin
  programlog.LogOutFormatStr('uzeentacdproxy: LoadFromDXF START', [], LM_Info);
  
  // Читаем коды DXF
  byt := rdr.ParseInteger;
  while byt <> 0 do begin
    case byt of
      92, 160: // Размер бинарных данных
        rdr.ParseInteger; // Пропускаем
      
      310: // Бинарные данные (hex-строка)
        begin
          HexData := rdr.ParseString;
          
          // Парсим через универсальный парсер
          Parser := TProxyGraphicParser.Create;
          try
            if Parser.InitFromHex(HexData) then begin
              if Parser.Parse then begin
                // Получаем результаты парсинга
                for I := 0 to Parser.GetResultCount - 1 do begin
                  CmdResult := Parser.GetResult(I);
                  
                  if CmdResult.Valid then begin
                    // Конвертируем в сущность ZCAD
                    Entity := ConvertResultToEntity(CmdResult, drawing);
                    
                    if Entity <> nil then begin
                      // Добавляем в массив виртуальных сущностей
                      SetLength(FVirtualEntities, FEntityCount + 1);
                      FVirtualEntities[FEntityCount] := Entity;
                      Inc(FEntityCount);
                    end;
                  end;
                end;
                
                // Вычисляем BBox
                if FEntityCount > 0 then
                  CalcBBoxFromEntities;
                
                programlog.LogOutFormatStr(
                  'uzeentacdproxy: LoadFromDXF Parsed %d entities from %d results',
                  [FEntityCount, Parser.GetResultCount], LM_Info);
              end else begin
                programlog.LogOutFormatStr('uzeentacdproxy: LoadFromDXF Parse failed', [], LM_Info);
              end;
            end else begin
              programlog.LogOutFormatStr('uzeentacdproxy: LoadFromDXF InitFromHex failed', [], LM_Info);
            end;
          finally
            Parser.Free;
          end;
        end;
      
      10, 11, 12, 13: // Вершины (fallback для BBox)
        begin
          // Пропускаем вершины (используем BBox из виртуальных сущностей)
          rdr.ParseString;
          rdr.ParseString;
          rdr.ParseString;
        end;
      
      else
        begin
          // Пропускаем неизвестные коды
          if (byt >= 10) and (byt < 40) then
            rdr.ParseString
          else if (byt >= 40) and (byt < 100) then
            rdr.ParseString
          else
            rdr.ParseString;
        end;
    end;
    
    byt := rdr.ParseInteger;
  end;
  
  programlog.LogOutFormatStr(
    'uzeentacdproxy: LoadFromDXF END (Entities=%d, BBoxLoaded=%d)',
    [FEntityCount, Ord(FBBoxLoaded)], LM_Info);
end;

{ Вычисляет BBox из виртуальных сущностей }
procedure GDBObjAcdProxy.CalcBBoxFromEntities;
var
  I: Integer;
  Entity: PGDBObjEntity;
  Min, Max: TzePoint3d;
begin
  FBBoxLoaded := False;
  
  if FEntityCount = 0 then
    Exit;
  
  // Инициализация из первой сущности
  FBBoxMinInOCS := FVirtualEntities[0]^.vp.BoundingBox.LBN;
  FBBoxMaxInOCS := FVirtualEntities[0]^.vp.BoundingBox.RTF;
  
  // Объединяем BBox всех сущностей
  for I := 1 to FEntityCount - 1 do begin
    Entity := FVirtualEntities[I];
    Min := Entity^.vp.BoundingBox.LBN;
    Max := Entity^.vp.BoundingBox.RTF;
    
    if Min.X < FBBoxMinInOCS.X then FBBoxMinInOCS.X := Min.X;
    if Min.Y < FBBoxMinInOCS.Y then FBBoxMinInOCS.Y := Min.Y;
    if Min.Z < FBBoxMinInOCS.Z then FBBoxMinInOCS.Z := Min.Z;
    if Max.X > FBBoxMaxInOCS.X then FBBoxMaxInOCS.X := Max.X;
    if Max.Y > FBBoxMaxInOCS.Y then FBBoxMaxInOCS.Y := Max.Y;
    if Max.Z > FBBoxMaxInOCS.Z then FBBoxMaxInOCS.Z := Max.Z;
  end;
  
  FBBoxLoaded := True;
end;

{ Отрисовка виртуальных сущностей }
procedure GDBObjAcdProxy.DrawVirtualEntities(var DC: TDrawContext; var drawing: TDrawingDef);
var
  I: Integer;
  Entity: PGDBObjEntity;
begin
  for I := 0 to FEntityCount - 1 do begin
    Entity := FVirtualEntities[I];
    if Entity <> nil then
      Entity^.FormatEntity(drawing, DC, [EFDraw]);
  end;
end;

{ Рассчитывает визуальное представление }
procedure GDBObjAcdProxy.FormatEntity(var drawing: TDrawingDef;
  var DC: TDrawContext; Stage: TEFStages);
begin
  if assigned(EntExtensions) then
    EntExtensions.RunOnBeforeEntityFormat(@self, drawing, DC);
  
  // Этап расчёта геометрии
  if (Stage = EFAllStages) or (EFCalcEntityCS in Stage) then begin
    if FBBoxLoaded then begin
      vp.BoundingBox.LBN := FBBoxMinInOCS;
      vp.BoundingBox.RTF := FBBoxMaxInOCS;
    end else begin
      vp.BoundingBox.LBN := NulVertex;
      vp.BoundingBox.RTF := NulVertex;
    end;
    CalcActualVisible(DC.DrawingContext.VActuality);
  end;
  
  // Этап отрисовки
  if ((Stage = EFAllStages) or (EFDraw in Stage))
    and (not (ESTemp in State))
    and (DCODrawable in DC.Options)
  then begin
    Representation.Clear;

    // Рисуем виртуальные сущности
    if FEntityCount > 0 then begin
      programlog.LogOutFormatStr(
        'uzeentacdproxy: FormatEntity drawing %d virtual entities',
        [FEntityCount], LM_Info);

      DrawVirtualEntities(DC, drawing);
    end;
  end;

  if assigned(EntExtensions) then
    EntExtensions.RunOnAfterEntityFormat(@self, drawing, DC);
end;

{ Отрисовка напрямую }
procedure GDBObjAcdProxy.DrawGeometry(lw: integer; var DC: TDrawContext;
  const inFrustumState: TInBoundingVolume);
begin
  // Отрисовка выполняется через FormatEntity
end;

{ Вычисляет попадание во фрустум }
function GDBObjAcdProxy.CalcTrueInFrustum(const frustum: TzeFrustum): TInBoundingVolume;
begin
  Result := CalcAABBInFrustum(vp.BoundingBox, frustum);
end;

{ Применяет матрицу трансформации }
procedure GDBObjAcdProxy.TransformAt(p: PGDBObjEntity; t_matrix: PzeTypedMatrix4d);
var
  src: PGDBObjAcdProxy;
  I: Integer;
begin
  src := PGDBObjAcdProxy(p);
  
  // Трансформируем BBox
  FBBoxMinInOCS := VectorTransform3D(src^.FBBoxMinInOCS, t_matrix^);
  FBBoxMaxInOCS := VectorTransform3D(src^.FBBoxMaxInOCS, t_matrix^);
  FBBoxLoaded := src^.FBBoxLoaded;
  
  // Трансформируем виртуальные сущности
  SetLength(FVirtualEntities, src^.FEntityCount);
  FEntityCount := src^.FEntityCount;
  
  for I := 0 to FEntityCount - 1 do begin
    if src^.FVirtualEntities[I] <> nil then
      FVirtualEntities[I] := src^.FVirtualEntities[I]^.Clone(nil);
  end;
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
  I: Integer;
begin
  GetMem(Pointer(newProxy), SizeOf(GDBObjAcdProxy));
  newProxy^.init(own, vp.Layer, vp.LineWeight);

  // Копируем BBox
  newProxy^.FBBoxMinInOCS := FBBoxMinInOCS;
  newProxy^.FBBoxMaxInOCS := FBBoxMaxInOCS;
  newProxy^.FBBoxLoaded := FBBoxLoaded;

  // Копируем виртуальные сущности
  SetLength(newProxy^.FVirtualEntities, FEntityCount);
  newProxy^.FEntityCount := FEntityCount;

  for I := 0 to FEntityCount - 1 do begin
    if FVirtualEntities[I] <> nil then
      newProxy^.FVirtualEntities[I] := FVirtualEntities[I]^.Clone(nil);
  end;

  Result := PGDBObjEntity(newProxy);
end;

{ Взрывает прокси-объект }
// procedure GDBObjAcdProxy.ExplodeToVirtualEntities(TargetLayout: PGDBLayout);
// var
//   I: Integer;
// begin
//   if TargetLayout = nil then
//     Exit;
//
//   // Добавляем виртуальные сущности в layout
//   for I := 0 to FEntityCount - 1 do begin
//     if FVirtualEntities[I] <> nil then begin
//       TargetLayout.AddEntity(FVirtualEntities[I]);
//       FVirtualEntities[I] := nil; // Передали владение
//     end;
//   end;
//
//   FEntityCount := 0;
// end;

function GDBObjAcdProxy.GetVirtualEntity(Index: Integer): PGDBObjEntity;
begin
  if (Index >= 0) and (Index < FEntityCount) then
    Result := FVirtualEntities[Index]
  else
    Result := nil;
end;

function GDBObjAcdProxy.GetVirtualEntityCount: Integer;
begin
  Result := FEntityCount;
end;

{ Сохраняет прокси-объект в DXF }
procedure GDBObjAcdProxy.SaveToDXF(var outStream: TZctnrVectorBytes;
  var drawing: TDrawingDef; var IODXFContext: TIODXFSaveContext);
begin
  SaveToDXFObjPrefix(outStream, 'ACAD_PROXY_ENTITY', 'AcDbProxyEntity',
    IODXFContext);
  
  { Сохраняем BBox }
  if FBBoxLoaded then begin
    dxfvertexout(outStream, 10, FBBoxMinInOCS);
    dxfvertexout(outStream, 11, FBBoxMaxInOCS);
  end;
end;

{ Создаёт новый экземпляр }
class function GDBObjAcdProxy.CreateInstance: PGDBObjAcdProxy;
begin
  Result := AllocAcdProxy;
  Result^.initnul(nil);
end;

{ === Вспомогательные функции === }

function AllocAcdProxy: Pointer;
begin
  GetMem(Result, SizeOf(GDBObjAcdProxy));
end;

function AllocAndInitAcdProxy(owner: PGDBObjGenericWithSubordinated): PGDBObjAcdProxy;
begin
  Result := AllocAcdProxy;
  Result^.initnul(owner);
end;

end.
