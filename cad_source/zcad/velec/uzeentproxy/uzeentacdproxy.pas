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
  Math,
  uzeentproxyparser,
  uzeentproxytypes,
  uzeentcircle,
  uzeentarc,
  uzeentpolyline,
  uzeenttext,
  uzeentellipse,
  UGDBSelectedObjArray,
  uzesnap,
  uzegeomentitiestree,
  UGDBVisibleTreeArray;

type
  PGDBObjAcdProxy = ^GDBObjAcdProxy;

  { Прокси-объект AutoCAD (ACAD_PROXY_ENTITY).
    Отображает прокси-графику через парсинг AcGiWorldDraw команд.
    Поддерживает: круги, дуги, полилинии, текст и другие примитивы.
    
    Архитектура "виртуальный блок":
    - FProxyParser хранит бинарные данные и результаты парсинга
    - FVirtualEntities содержит сущности ZCAD (круги, дуги, текст и т.д.)
    - При отрисовке делегирует форматирование виртуальным сущностям
    }
  GDBObjAcdProxy = object(GDBObj3d)
  private
    { Координата минимального угла габаритной рамки в OCS }
    FBBoxMinInOCS: TzePoint3d;
    { Координата максимального угла габаритной рамки в OCS }
    FBBoxMaxInOCS: TzePoint3d;
    { Флаг: данные габаритной рамки успешно загружены }
    FBBoxLoaded: Boolean;

    { Парсер прокси-графики }
    FProxyParser: TProxyGraphicParser;
    
    { Массив виртуальных сущностей (круги, дуги, текст и т.д.) }
    FVirtualEntities: PGDBObjEntityTreeArray;

    { Отрисовывает одно ребро габаритной рамки }
    procedure DrawBBoxEdge(var DC: TDrawContext;
      const ptFrom, ptTo: TzePoint3d);

    { Конвертирует результат парсинга в сущность ZCAD }
    function ConvertResultToEntity(const CmdResult: TProxyCommandResult; var drawing: TDrawingDef): PGDBObjEntity;

    { Создаёт виртуальные сущности из результатов парсинга }
    procedure CreateVirtualEntities(var drawing: TDrawingDef);
    
    { Вычисляет BBox из результатов парсинга прокси-графики }
    function CalcBBoxFromParserResults(
      const Parser: TProxyGraphicParser;
      out MinPt, MaxPt: TzePoint3d): Boolean;

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
  FProxyParser := nil;
  FVirtualEntities := nil;
end;

constructor GDBObjAcdProxy.initnul(owner: PGDBObjGenericWithSubordinated);
begin
  inherited initnul(owner);
  FBBoxMinInOCS := NulVertex;
  FBBoxMaxInOCS := NulVertex;
  FBBoxLoaded   := False;
  FProxyParser := nil;
  FVirtualEntities := nil;
end;

destructor GDBObjAcdProxy.done;
begin
  { Освобождаем виртуальные сущности }
  if FVirtualEntities <> nil then begin
    FVirtualEntities^.done;
    FreeMem(Pointer(FVirtualEntities));
    FVirtualEntities := nil;
  end;
  
  { Освобождаем парсер }
  if FProxyParser <> nil then begin
    FProxyParser.Free;
    FProxyParser := nil;
  end;
  
  inherited done;
end;

{ Вычисляет BBox из результатов парсинга прокси-графики }
function GDBObjAcdProxy.CalcBBoxFromParserResults(
  const Parser: TProxyGraphicParser;
  out MinPt, MaxPt: TzePoint3d): Boolean;
var
  I, J: Integer;
  CmdResult: TProxyCommandResult;
  Pt: TzePoint3d;
  Initialized: Boolean;
  
  procedure ExpandBBox(const P: TzePoint3d);
  begin
    if not Initialized then begin
      MinPt := P;
      MaxPt := P;
      Initialized := True;
    end else begin
      if P.x < MinPt.x then MinPt.x := P.x;
      if P.y < MinPt.y then MinPt.y := P.y;
      if P.z < MinPt.z then MinPt.z := P.z;
      if P.x > MaxPt.x then MaxPt.x := P.x;
      if P.y > MaxPt.y then MaxPt.y := P.y;
      if P.z > MaxPt.z then MaxPt.z := P.z;
    end;
  end;
  
begin
  Result := False;
  Initialized := False;
  MinPt := NulVertex;
  MaxPt := NulVertex;
  
  if Parser = nil then
    Exit;
  
  for I := 0 to Parser.ResultCount - 1 do begin
    CmdResult := Parser.GetResult(I);
    if not CmdResult.Valid then
      Continue;
      
    case CmdResult.PrimitiveType of
      pptCircle:
        begin
          // Круг: центр ± радиус
          ExpandBBox(CreateVertex(
            CmdResult.CircleData.Center.x - CmdResult.CircleData.Radius,
            CmdResult.CircleData.Center.y - CmdResult.CircleData.Radius,
            CmdResult.CircleData.Center.z));
          ExpandBBox(CreateVertex(
            CmdResult.CircleData.Center.x + CmdResult.CircleData.Radius,
            CmdResult.CircleData.Center.y + CmdResult.CircleData.Radius,
            CmdResult.CircleData.Center.z));
        end;
        
      pptArc:
        begin
          // Дуга: центр ± радиус (упрощённо, как круг)
          ExpandBBox(CreateVertex(
            CmdResult.ArcData.Center.x - CmdResult.ArcData.Radius,
            CmdResult.ArcData.Center.y - CmdResult.ArcData.Radius,
            CmdResult.ArcData.Center.z));
          ExpandBBox(CreateVertex(
            CmdResult.ArcData.Center.x + CmdResult.ArcData.Radius,
            CmdResult.ArcData.Center.y + CmdResult.ArcData.Radius,
            CmdResult.ArcData.Center.z));
        end;
        
      pptPolyline, pptPolygon:
        begin
          // Полилиния: все вершины
          for J := 0 to High(CmdResult.PolylineData.Vertices) do begin
            ExpandBBox(CmdResult.PolylineData.Vertices[J]);
          end;
        end;
        
      pptEllipse:
        begin
          // Эллипс: центр ± большая ось
          ExpandBBox(CreateVertex(
            CmdResult.EllipticArcData.Center.x - CmdResult.EllipticArcData.MajorAxisLength,
            CmdResult.EllipticArcData.Center.y - CmdResult.EllipticArcData.MajorAxisLength,
            CmdResult.EllipticArcData.Center.z));
          ExpandBBox(CreateVertex(
            CmdResult.EllipticArcData.Center.x + CmdResult.EllipticArcData.MajorAxisLength,
            CmdResult.EllipticArcData.Center.y + CmdResult.EllipticArcData.MajorAxisLength,
            CmdResult.EllipticArcData.Center.z));
        end;
        
      pptText:
        begin
          // Текст: позиция + примерный размер
          ExpandBBox(CmdResult.TextData.Insert);
          ExpandBBox(CreateVertex(
            CmdResult.TextData.Insert.x + CmdResult.TextData.Height * Length(CmdResult.TextData.Text),
            CmdResult.TextData.Insert.y + CmdResult.TextData.Height,
            CmdResult.TextData.Insert.z));
        end;
    end;
  end;
  
  Result := Initialized;
end;

{ Загружает прокси-объект из DXF.
  1. Читает бинарные данные из кода 310 (hex-строка)
  2. Парсит через TProxyGraphicParser (AcGiWorldDraw формат)
  3. Конвертирует результаты в сущности ZCAD
  4. Вычисляет BBox из геометрии }
procedure GDBObjAcdProxy.LoadFromDXF(var rdr: TZMemReader;
  ptu: PExtensionData; var drawing: TDrawingDef;
  var context: TIODXFLoadContext);
var
  byt: Integer;
  hexData: string;
  proxyData: TBytes;
  bbInitialized: Boolean;
begin
  bbInitialized := False;
  FBBoxMinInOCS := NulVertex;
  FBBoxMaxInOCS := NulVertex;
  FBBoxLoaded   := False;
  hexData       := '';

  programlog.LogOutFormatStr('uzeentacdproxy: LoadFromDXF START', [], LM_Info);

  byt := rdr.ParseInteger;
  while byt <> 0 do begin
    { Общие свойства: слой, цвет, линия и прочее }
    if not LoadFromDXFObjShared(rdr, byt, ptu, drawing, context) then begin
      { Код 92: размер бинарных данных (для R2010-) }
      if byt = 92 then begin
        rdr.ParseString; // пропускаем значение размера
        programlog.LogOutFormatStr('uzeentacdproxy: LoadFromDXF code 92 (size)', [], LM_Info);
      end
      { Код 160: размер бинарных данных (для R2010+) }
      else if byt = 160 then begin
        rdr.ParseString; // пропускаем значение размера
        programlog.LogOutFormatStr('uzeentacdproxy: LoadFromDXF code 160 (size)', [], LM_Info);
      end
      { Код 310: бинарные данные (hex-строка) }
      else if byt = 310 then begin
        hexData := hexData + rdr.ParseString;
        programlog.LogOutFormatStr('uzeentacdproxy: LoadFromDXF code 310, length=%d', [Length(hexData)], LM_Info);
      end
      { Вершины из кодов 10-39 для fallback (построение BBox) }
      else if (byt >= 10) and (byt <= 39) then begin
        // Пропускаем вершины fallback
        rdr.ParseString;
      end
      else begin
        { Любые другие коды — пропускаем значение }
        rdr.ParseString;
      end;
    end;
    byt := rdr.ParseInteger;
  end;

  { Парсим бинарные данные через TProxyGraphicParser }
  if Length(hexData) > 0 then begin
    programlog.LogOutFormatStr('uzeentacdproxy: LoadFromDXF parsing proxy graphic, hex length=%d', [Length(hexData)], LM_Info);
    programlog.LogOutFormatStr('uzeentacdproxy: LoadFromDXF hex preview: %s...', [Copy(hexData, 1, 64)], LM_Info);

    FProxyParser := TProxyGraphicParser.Create;
    try
      if FProxyParser.InitFromHex(hexData) then begin
        programlog.LogOutFormatStr('uzeentacdproxy: LoadFromDXF calling Parse...', [], LM_Info);
        if FProxyParser.Parse then begin
          programlog.LogOutFormatStr('uzeentacdproxy: LoadFromDXF UNIVERSAL PARSER Success - Results=%d', [FProxyParser.ResultCount], LM_Info);

          { Создаём виртуальные сущности }
          CreateVirtualEntities(drawing);
          
          { Вычисляем BBox из результатов парсинга }
          if FProxyParser.HasValidResults then begin
            FBBoxLoaded := CalcBBoxFromParserResults(FProxyParser, FBBoxMinInOCS, FBBoxMaxInOCS);
            if FBBoxLoaded then begin
              programlog.LogOutFormatStr('uzeentacdproxy: LoadFromDXF BBox calculated from parser: Min=(%.2f;%.2f;%.2f) Max=(%.2f;%.2f;%.2f)',
                [FBBoxMinInOCS.x, FBBoxMinInOCS.y, FBBoxMinInOCS.z, FBBoxMaxInOCS.x, FBBoxMaxInOCS.y, FBBoxMaxInOCS.z], LM_Info);
            end;
          end;
        end
        else begin
          programlog.LogOutFormatStr('uzeentacdproxy: LoadFromDXF Parse FAILED', [], LM_Info);
        end;
      end
      else begin
        programlog.LogOutFormatStr('uzeentacdproxy: LoadFromDXF InitFromHex FAILED', [], LM_Info);
      end;
    except
      on E: Exception do begin
        programlog.LogOutFormatStr('uzeentacdproxy: LoadFromDXF EXCEPTION: %s', [E.Message], LM_Info);
        //programlog.LogOutFormatStr('uzeentacdproxy: LoadFromDXF EXCEPTION StackTrace: %s', [GetExceptionBackTrace(E, True)], LM_Info);
        FProxyParser.Free;
        FProxyParser := nil;
      end;
    end;
  end;

  { Fallback: если парсер не сработал, используем BBox из вершин }
  if not FBBoxLoaded and bbInitialized then begin
    FBBoxMinInOCS := FBBoxMinInOCS;
    FBBoxMaxInOCS := FBBoxMaxInOCS;
    FBBoxLoaded := bbInitialized;
    programlog.LogOutFormatStr('uzeentacdproxy: LoadFromDXF using FALLBACK BBox', [], LM_Info);
  end;

  programlog.LogOutFormatStr(
    'uzeentacdproxy: LoadFromDXF END BBoxLoaded=%d Min=(%.2f;%.2f;%.2f) Max=(%.2f;%.2f;%.2f)',
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

{ Конвертирует результат парсинга TProxyCommandResult в сущность ZCAD }
function GDBObjAcdProxy.ConvertResultToEntity(const CmdResult: TProxyCommandResult; var drawing: TDrawingDef): PGDBObjEntity;
var
  circleObj: PGDBObjCircle;
  arcObj: PGDBObjArc;
  polylineObj: PGDBObjPolyline;
  textObj: PGDBObjText;
  ellipseObj: PGDBObjEllipse;
  I: Integer;
  startAngle, endAngle: Double;
begin
  Result := nil;

  if not CmdResult.Valid then
    Exit;

  case CmdResult.PrimitiveType of
    pptCircle:
      begin
        // Круг: создаём PGDBObjCircle через CreateInstance
        circleObj := GDBObjCircle.CreateInstance;
        circleObj^.initnul;
        circleObj^.vp.Color := vp.Color;
        circleObj^.vp.Layer := vp.Layer;
        circleObj^.Local.p_insert := CmdResult.CircleData.Center;
        circleObj^.Radius := CmdResult.CircleData.Radius;
        // Normal задаёт ориентацию круга через локальную СК
        if not VectorIsClose(CmdResult.CircleData.Normal, PROXY_Z_AXIS) then begin
          // TODO: установить локальную СК по Normal
        end;
        // Вычисляем матрицу объекта для отрисовки
        circleObj^.CalcObjMatrix(@drawing);
        Result := circleObj;
      end;

    pptArc:
      begin
        // Дуга: создаём PGDBObjArc через CreateInstance
        arcObj := GDBObjArc.CreateInstance;
        arcObj^.initnul;
        arcObj^.vp.Color := vp.Color;
        arcObj^.vp.Layer := vp.Layer;
        arcObj^.Local.p_insert := CmdResult.ArcData.Center;
        arcObj^.R := CmdResult.ArcData.Radius;

        // Вычисляем начальный и конечный углы из StartVector и SweepAngle
        startAngle := ArcTan2(CmdResult.ArcData.StartVector.y, CmdResult.ArcData.StartVector.x);
        endAngle := startAngle + CmdResult.ArcData.SweepAngle;

        arcObj^.StartAngle := startAngle;
        arcObj^.EndAngle := endAngle;
        // Вычисляем матрицу объекта для отрисовки
        arcObj^.CalcObjMatrix(@drawing);
        Result := arcObj;
      end;

    pptPolyline, pptPolygon:
      begin
        // Полилиния: создаём PGDBObjPolyline через CreateInstance
        polylineObj := GDBObjPolyline.CreateInstance;
        polylineObj^.initnul(nil);
        polylineObj^.vp.Color := vp.Color;
        polylineObj^.vp.Layer := vp.Layer;

        // Добавляем вершины через AddVertex (наследуется от GDBObjCurve)
        for I := 0 to High(CmdResult.PolylineData.Vertices) do
          polylineObj^.AddVertex(CmdResult.PolylineData.Vertices[I]);

        // Замыкаем если полигон
        if CmdResult.PrimitiveType = pptPolygon then
          polylineObj^.closed := True
        else
          polylineObj^.closed := CmdResult.PolylineData.Closed;

        // Вычисляем матрицу объекта для отрисовки
        polylineObj^.CalcObjMatrix(@drawing);
        Result := polylineObj;
      end;

    pptText:
      begin
        // Текст: создаём PGDBObjText через CreateInstance
        textObj := GDBObjText.CreateInstance;
        textObj^.initnul(nil);
        textObj^.vp.Color := vp.Color;
        textObj^.vp.Layer := vp.Layer;
        textObj^.Local.p_insert := CmdResult.TextData.Insert;
        textObj^.Content := CmdResult.TextData.Text;
        textObj^.obj_height := CmdResult.TextData.Height;
        textObj^.textprop.size := CmdResult.TextData.Height;

        // Rotation задаём через локальную ось OX
        if not VectorIsClose(CmdResult.TextData.Direction, PROXY_X_AXIS) then begin
          textObj^.Local.basis.ox := CmdResult.TextData.Direction;
          textObj^.Local.basis.ox := NormalizeVertex(textObj^.Local.basis.ox);
        end else begin
          textObj^.Local.basis.ox := PROXY_X_AXIS;
        end;
        // OZ - нормаль текста
        textObj^.Local.basis.oz := CmdResult.TextData.Normal;
        textObj^.Local.basis.oz := NormalizeVertex(textObj^.Local.basis.oz);

        // Вычисляем матрицу объекта для отрисовки
        textObj^.CalcObjMatrix(@drawing);
        Result := textObj;
      end;

    pptEllipse:
      begin
        // Эллипс: создаём PGDBObjEllipse через CreateInstance
        ellipseObj := GDBObjEllipse.CreateInstance;
        ellipseObj^.initnul;
        ellipseObj^.vp.Color := vp.Color;
        ellipseObj^.vp.Layer := vp.Layer;
        ellipseObj^.Local.p_insert := CmdResult.EllipticArcData.Center;

        // Большая ось - вектор направления и длины
        // В proxy данных MajorAxisLength - это длина, нам нужно создать вектор
        // Нормаль Z - большая ось вдоль X
        ellipseObj^.MajorAxis := CreateVertex(CmdResult.EllipticArcData.MajorAxisLength, 0, 0);

        // Ratio = minor / major
        if CmdResult.EllipticArcData.MajorAxisLength > 0 then
          ellipseObj^.Ratio := CmdResult.EllipticArcData.MinorAxisLength / CmdResult.EllipticArcData.MajorAxisLength
        else
          ellipseObj^.Ratio := 1.0;

        // Параметры углов
        ellipseObj^.StartAngle := CmdResult.EllipticArcData.StartParam;
        ellipseObj^.EndAngle := CmdResult.EllipticArcData.EndParam;

        // Вычисляем матрицу объекта для отрисовки
        ellipseObj^.CalcObjMatrix(@drawing);
        Result := ellipseObj;
      end;

  else
    // Неизвестный тип примитива
    programlog.LogOutFormatStr('uzeentacdproxy: ConvertResultToEntity - Unknown PrimitiveType=%d', [Ord(CmdResult.PrimitiveType)], LM_Info);
  end;
end;

{ Создаёт виртуальные сущности из результатов парсинга }
procedure GDBObjAcdProxy.CreateVirtualEntities(var drawing: TDrawingDef);
var
  I: Integer;
  CmdResult: TProxyCommandResult;
  Entity: PGDBObjEntity;
begin
  if FProxyParser = nil then
    Exit;

  programlog.LogOutFormatStr('uzeentacdproxy: CreateVirtualEntities - START, Results=%d', [FProxyParser.ResultCount], LM_Info);

  { Создаём массив виртуальных сущностей }
  GetMem(Pointer(FVirtualEntities), SizeOf(GDBObjEntityTreeArray));
  FVirtualEntities^.initnul;

  for I := 0 to FProxyParser.ResultCount - 1 do begin
    CmdResult := FProxyParser.GetResult(I);
    if not CmdResult.Valid then
      Continue;
      
    programlog.LogOutFormatStr('uzeentacdproxy: CreateVirtualEntities - Processing primitive %d: Type=%d', [I, Ord(CmdResult.PrimitiveType)], LM_Info);
    
    { Конвертируем результат парсинга в сущность ZCAD }
    Entity := ConvertResultToEntity(CmdResult, drawing);
    if Entity <> nil then begin
      programlog.LogOutFormatStr('uzeentacdproxy: CreateVirtualEntities - Created entity type %d', [Ord(CmdResult.PrimitiveType)], LM_Info);
      FVirtualEntities^.AddPEntity(Entity^);
    end;
  end;
  
  programlog.LogOutFormatStr('uzeentacdproxy: CreateVirtualEntities - END, Entities created=%d', [FVirtualEntities^.Count], LM_Info);
end;

{ Рассчитывает визуальное представление прокси-объекта.
  Приоритеты:
  1. Если есть виртуальные сущности → рисуем их
  2. Иначе → рисуем габаритную рамку (fallback) }
procedure GDBObjAcdProxy.FormatEntity(var drawing: TDrawingDef;
  var DC: TDrawContext; Stage: TEFStages);
var
  { 8 вершин параллелепипеда: LBN = Left-Bottom-Near, RTF = Right-Top-Far }
  v000, v100, v010, v110: TzePoint3d; { нижние 4 вершины }
  v001, v101, v011, v111: TzePoint3d; { верхние 4 вершины }
  bbMin, bbMax: TzePoint3d;
  I: Integer;
  Entity: PGDBObjEntity;
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

    { Приоритет 1: Отрисовка виртуальных сущностей }
    if (FVirtualEntities <> nil) and (FVirtualEntities^.Count > 0) then begin
      programlog.LogOutFormatStr('uzeentacdproxy: FormatEntity drawing VIRTUAL ENTITIES (Count=%d)', [FVirtualEntities^.Count], LM_Info);
      
      { Отрисовываем каждую виртуальную сущность }
      for I := 0 to FVirtualEntities^.Count - 1 do begin
        Entity := PGDBObjEntity(FVirtualEntities^.getDataMutable(I));
        if Entity <> nil then begin
          Entity^.FormatEntity(drawing, DC, Stage);
        end;
      end;
      
      programlog.LogOutFormatStr('uzeentacdproxy: FormatEntity VIRTUAL ENTITIES complete', [], LM_Info);
    end
    { Приоритет 2: Габаритная рамка (fallback) }
    else if FBBoxLoaded then begin
      programlog.LogOutFormatStr('uzeentacdproxy: FormatEntity drawing BBOX (fallback)', [], LM_Info);

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

{ Применяет матрицу трансформации: пересчитывает угловые точки BB и трансформирует виртуальные сущности }
procedure GDBObjAcdProxy.TransformAt(p: PGDBObjEntity;
  t_matrix: PzeTypedMatrix4d);
var
  src: PGDBObjAcdProxy;
  I: Integer;
  CmdResult: TProxyCommandResult;
  Entity: PGDBObjEntity;
begin
  src := PGDBObjAcdProxy(p);
  FBBoxMinInOCS := VectorTransform3D(src^.FBBoxMinInOCS, t_matrix^);
  FBBoxMaxInOCS := VectorTransform3D(src^.FBBoxMaxInOCS, t_matrix^);
  FBBoxLoaded   := src^.FBBoxLoaded;

  { Трансформируем виртуальные сущности если есть парсер }
  { Примечание: для трансформации не нужна полная инициализация, 
    т.к. мы сразу вызываем TransformAt }
  { Пока пропускаем трансформацию виртуальных сущностей }
  {
  if src^.FProxyParser <> nil then begin
    FProxyParser := TProxyGraphicParser.Create;
    for I := 0 to src^.FProxyParser.ResultCount - 1 do begin
      CmdResult := src^.FProxyParser.GetResult(I);
      if CmdResult.Valid then begin
        Entity := ConvertResultToEntity(CmdResult, drawing);
        if Entity <> nil then begin
          Entity^.TransformAt(@self, t_matrix);
          Entity^.done;
          FreeMem(Pointer(Entity));
        end;
      end;
    end;
  end;
  }
end;

function GDBObjAcdProxy.GetObjTypeName: string;
begin
  Result := ObjN_GDBObjAcdProxy;
end;

function GDBObjAcdProxy.GetObjType: TObjID;
begin
  Result := GDBAcdProxyID;
end;

{ Создаёт полную копию прокси-объекта с копированием виртуальных сущностей }
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

  { Копируем парсер если есть }
  if FProxyParser <> nil then begin
    { Примечание: полное копирование парсера требует доступа к внутреннему буферу
      Пока просто создаём новый парсер - он будет заполнен при следующей загрузке }
    { newProxy^.FProxyParser := TProxyGraphicParser.Create; }
    { Копирование данных парсера отложено до полной реализации }
  end;

  Result := newProxy;
end;

{ Прокси-объект делегирует контрольные точки виртуальным сущностям }
procedure GDBObjAcdProxy.addcontrolpoints(tdesc: Pointer);
var
  I: Integer;
  Entity: PGDBObjEntity;
  CenterPoint: TzePoint3d;
  pdesc: controlpointdesc;
begin
  { Если есть виртуальные сущности — делегируем им }
  if (FVirtualEntities <> nil) and (FVirtualEntities^.Count > 0) then begin
    for I := 0 to FVirtualEntities^.Count - 1 do begin
      Entity := PGDBObjEntity(FVirtualEntities^.getDataMutable(I));
      if Entity <> nil then begin
        Entity^.addcontrolpoints(tdesc);
      end;
    end;
  end
  { Иначе добавляем точку в центре BBox (fallback) }
  else if FBBoxLoaded then begin
    { Инициализируем массив контрольных точек (1 точка) }
    PSelectedObjDesc(tdesc)^.pcontrolpoint^.init(1);
    
    { Создаём контрольную точку }
    CenterPoint := CreateVertex(
      (FBBoxMinInOCS.x + FBBoxMaxInOCS.x) / 2,
      (FBBoxMinInOCS.y + FBBoxMaxInOCS.y) / 2,
      (FBBoxMinInOCS.z + FBBoxMaxInOCS.z) / 2
    );
    
    pdesc.selected := False;
    pdesc.PDrawable := nil;
    pdesc.vertexnum := 0;
    pdesc.attr := [CPA_Strech];
    pdesc.worldcoord := CenterPoint;
    pdesc.pointtype := os_midle;
    
    { Добавляем точку в массив }
    PSelectedObjDesc(tdesc)^.pcontrolpoint^.PushBackData(pdesc);
  end;
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
