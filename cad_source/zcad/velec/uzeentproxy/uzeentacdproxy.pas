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

  Архитектура:
  - Парсинг Proxy Graphic выполняется в FormatEntity через TProxyGraphicParser
  - Примитивы внутри прокси регистрируются в TProxyOpCodeDispatcher каждый
    в своём модуле (uzeentproxyparsercircle.pas, uzeentproxyparsertext.pas и т.д.)
  - Чтобы отключить конкретный примитив — исключить его .pas из проекта

  Рендеринг:
  - Контуры (полилинии) отрисовываются через Representation.DrawPolyLineWithLT,
    как принято в ZCAD (аналогично GDBObjCircle)
  - DrawGeometry вызывает Representation.DrawGeometry — стандартный путь

  BBox:
  - Вычисляется суммарно по всем успешно распаршенным примитивам
  - Если ни один примитив не поддерживается — объект пропускается
    с сообщением в лог

  Зависимости от примитивов:
  - uzeentproxyparsercircle  — парсер кругов (OpCode=2)
  - uzeentproxyparsertext    — парсер текста (OpCode=10, 38)
  Каждый из них регистрируется в TProxyOpCodeDispatcher при загрузке модуля.
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
  uzeentproxymanager,
  uzeentproxygraphicparser,
  { Подключаем модули-парсеры примитивов. Каждый регистрирует свой обработчик
    в TProxyOpCodeDispatcher при инициализации модуля (секция initialization).
    Чтобы отключить конкретный примитив — закомментировать его строку ниже:
    его initialization не выполнится и OpCode не зарегистрируется. }
  uzeentproxyparsercircle,  { OpCode=2: Circle }
  uzeentproxyparsertext,    { OpCode=10,38: Text, UnicodeText2 }
  UGDBSelectedObjArray,
  uzesnap,
  gzctnrVectorTypes,
  gzctnrVector,
  UGDBPoint3DArray,
  UGDBVisibleTreeArray;

type
  PGDBObjAcdProxy = ^GDBObjAcdProxy;

  { Прокси-объект AutoCAD (ACAD_PROXY_ENTITY).

    Загружает бинарный блок Proxy Graphic из DXF, разбирает его через
    TProxyGraphicParser и отображает результат через стандартный механизм
    Representation.DrawPolyLineWithLT.

    Каждый тип примитива обрабатывается отдельным модулем, который
    регистрирует свой обработчик при загрузке (секция initialization).
    Добавление нового примитива = создание нового .pas файла. }
  GDBObjAcdProxy = object(GDBObj3d)
  private
    { Минимальная точка суммарного BBox в OCS }
    FBBoxMinInOCS: TzePoint3d;
    { Максимальная точка суммарного BBox в OCS }
    FBBoxMaxInOCS: TzePoint3d;
    { Флаг: BBox вычислен хотя бы одним примитивом }
    FBBoxLoaded: Boolean;

    { Сырые байты Proxy Graphic (код 310 из DXF) }
    FProxyDataBytes: TBytes;

    { Вершины всех контуров для отрисовки, объединённые в один массив }
    FContourVertices: GDBPoint3DArray;
    { Флаг: FContourVertices заполнен }
    FHasContourVertices: Boolean;

    { Центр BBox — используется как контрольная точка }
    FCenterPoint: TzePoint3d;
    FHasCenterPoint: Boolean;

    { Разбирает FProxyDataBytes и заполняет контуры и BBox }
    procedure ParseProxyData;

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

    { Рассчитывает визуальное представление и заполняет Representation }
    procedure FormatEntity(var drawing: TDrawingDef;
      var DC: TDrawContext;
      Stage: TEFStages = EFAllStages); virtual;

    { Отрисовывает объект через Representation }
    procedure DrawGeometry(lw: integer; var DC: TDrawContext;
      const inFrustumState: TInBoundingVolume); virtual;

    { Вычисляет попадание объекта в усечённую пирамиду видимости }
    function CalcTrueInFrustum(
      const frustum: TzeFrustum): TInBoundingVolume; virtual;

    { Применяет матрицу трансформации }
    procedure TransformAt(p: PGDBObjEntity;
      t_matrix: PzeTypedMatrix4d); virtual;

    { Возвращает тип объекта в виде строки }
    function GetObjTypeName: string; virtual;

    { Возвращает числовой идентификатор типа объекта }
    function GetObjType: TObjID; virtual;

    { Создаёт копию объекта }
    function Clone(own: Pointer): PGDBObjEntity; virtual;

    { Добавляет контрольные точки объекта }
    procedure addcontrolpoints(tdesc: Pointer); virtual;

    { Сохраняет состояние для real-time модификации }
    procedure rtsave(refp: Pointer); virtual;

    { Применяет real-time изменение точки }
    procedure rtmodifyonepoint(const rtmod: TRTModifyData); virtual;

    { Проверяет необходимость real-time модификации }
    function IsRTNeedModify(const Point: PControlPointDesc;
      p: Pointer): boolean; virtual;

    { Проецирует контрольную точку на экранные координаты }
    procedure remaponecontrolpoint(pdesc: pcontrolpointdesc;
      ProjectProc: GDBProjectProc); virtual;

    { Вычисляет экранный BBox }
    procedure getoutbound(var DC: TDrawContext); virtual;

    { Создаёт новый инициализированный экземпляр }
    class function CreateInstance: PGDBObjAcdProxy; static;
  end;

{ Выделяет память для нового прокси-объекта }
function AllocAcdProxy: Pointer;

{ Выделяет и инициализирует новый прокси-объект }
function AllocAndInitAcdProxy(
  owner: PGDBObjGenericWithSubordinated): PGDBObjAcdProxy;

implementation

{ --- Вспомогательные функции --- }

{ Конвертирует hex-строку в массив байт.
  Используется при разборе кода 310 из DXF. }
function HexStringToBytes(const HexStr: string): TBytes;
var
  I, Len: Integer;
begin
  Len := Length(HexStr) div 2;
  SetLength(Result, Len);
  for I := 0 to Len - 1 do
    Result[I] := Lo(StrToIntDef('$' + Copy(HexStr, I * 2 + 1, 2), 0));
end;

{ === GDBObjAcdProxy === }

constructor GDBObjAcdProxy.init;
begin
  inherited init(own, layeraddres, LW);
  FBBoxLoaded := False;
  FBBoxMinInOCS := NulVertex;
  FBBoxMaxInOCS := NulVertex;
  FHasContourVertices := False;
  FContourVertices.init(0);
  FHasCenterPoint := False;
end;

constructor GDBObjAcdProxy.initnul;
begin
  inherited initnul(owner);
  FBBoxLoaded := False;
  FBBoxMinInOCS := NulVertex;
  FBBoxMaxInOCS := NulVertex;
  FHasContourVertices := False;
  FContourVertices.init(0);
  FHasCenterPoint := False;
end;

destructor GDBObjAcdProxy.done;
begin
  FContourVertices.done;
  inherited done;
end;

{ Загружает данные объекта из DXF-потока.
  Читает hex-данные кода 310 и накапливает их в FProxyDataBytes. }
procedure GDBObjAcdProxy.LoadFromDXF(var rdr: TZMemReader;
  ptu: PExtensionData;
  var drawing: TDrawingDef; var context: TIODXFLoadContext);
var
  HexAccum: string;
  Code: Integer;
begin
  HexAccum := '';
  Code := rdr.ParseInteger;
  while Code <> 0 do
  begin
    case Code of
      310:
        HexAccum := HexAccum + rdr.ParseString;
    else
      rdr.SkipString;
    end;
    Code := rdr.ParseInteger;
  end;

  if Length(HexAccum) > 0 then
  begin
    FProxyDataBytes := HexStringToBytes(HexAccum);
    programlog.LogOutFormatStr(
      'uzeentacdproxy: LoadFromDXF loaded %d bytes of proxy data',
      [Length(FProxyDataBytes)], LM_Info);
  end
  else
  begin
    SetLength(FProxyDataBytes, 0);
    programlog.LogOutFormatStr(
      'uzeentacdproxy: LoadFromDXF no proxy data (code 310 absent)',
      [], LM_Info);
  end;
end;

{ Сохраняет данные объекта в DXF-поток }
procedure GDBObjAcdProxy.SaveToDXF(var outStream: TZctnrVectorBytes;
  var drawing: TDrawingDef; var IODXFContext: TIODXFSaveContext);
begin
  inherited SaveToDXF(outStream, drawing, IODXFContext);
  { TODO: записать FProxyDataBytes обратно в DXF код 310 }
end;

{ Разбирает FProxyDataBytes через TProxyGraphicParser.
  Заполняет FContourVertices, FBBoxMinInOCS/FBBoxMaxInOCS, FCenterPoint. }
procedure GDBObjAcdProxy.ParseProxyData;
var
  Parser: TProxyGraphicParser;
  ParseResult: TProxyGraphicParseResult;
  ir: itrec;
  pV: PzePoint3d;
begin
  if Length(FProxyDataBytes) = 0 then
    Exit;

  { Сбрасываем предыдущие данные }
  FContourVertices.done;
  FContourVertices.init(0);
  FHasContourVertices := False;
  FBBoxLoaded := False;
  FHasCenterPoint := False;

  Parser := TProxyGraphicParser.Create(FProxyDataBytes);
  try
    ParseResult := Parser.Parse;

    { Обновляем BBox }
    if ParseResult.BBoxLoaded then
    begin
      FBBoxMinInOCS := ParseResult.BBoxMin;
      FBBoxMaxInOCS := ParseResult.BBoxMax;
      FBBoxLoaded := True;

      { Вычисляем центр BBox }
      FCenterPoint.x := (FBBoxMinInOCS.x + FBBoxMaxInOCS.x) / 2;
      FCenterPoint.y := (FBBoxMinInOCS.y + FBBoxMaxInOCS.y) / 2;
      FCenterPoint.z := (FBBoxMinInOCS.z + FBBoxMaxInOCS.z) / 2;
      FHasCenterPoint := True;

      programlog.LogOutFormatStr(
        'uzeentacdproxy: ParseProxyData BBox=(%.3f,%.3f,%.3f)-(%.3f,%.3f,%.3f) primitives=%d',
        [FBBoxMinInOCS.x, FBBoxMinInOCS.y, FBBoxMinInOCS.z,
         FBBoxMaxInOCS.x, FBBoxMaxInOCS.y, FBBoxMaxInOCS.z,
         ParseResult.PrimitiveCount], LM_Info);
    end
    else
    begin
      programlog.LogOutFormatStr(
        'uzeentacdproxy: ParseProxyData no supported primitives found',
        [], LM_Info);
    end;

    { Копируем вершины контуров }
    if ParseResult.HasVertices and (ParseResult.AllVertices.Count > 0) then
    begin
      FContourVertices.init(ParseResult.AllVertices.Count);
      pV := ParseResult.AllVertices.beginiterate(ir);
      while pV <> nil do
      begin
        FContourVertices.PushBackData(pV^);
        pV := ParseResult.AllVertices.iterate(ir);
      end;
      FHasContourVertices := True;
    end;

  finally
    ParseResult.AllVertices.done;
    Parser.Free;
  end;
end;

{ Рассчитывает визуальное представление объекта.
  Аналогично GDBObjCircle:
  - EFCalcEntityCS: устанавливает BBox, вызывает разбор данных
  - EFDraw: заполняет Representation через DrawPolyLineWithLT }
procedure GDBObjAcdProxy.FormatEntity(var drawing: TDrawingDef;
  var DC: TDrawContext; Stage: TEFStages);
begin
  if Assigned(EntExtensions) then
    EntExtensions.RunOnBeforeEntityFormat(@self, drawing, DC);

  { --- Этап расчёта координатной системы и геометрии --- }
  if (Stage = EFAllStages) or (EFCalcEntityCS in Stage) then
  begin
    { Разбираем данные каждый раз при форматировании:
      это обеспечивает корректную работу при первом отображении и
      при изменении трансформации }
    ParseProxyData;

    { Передаём BBox системе видимости }
    if FBBoxLoaded then
    begin
      vp.BoundingBox.LBN := FBBoxMinInOCS;
      vp.BoundingBox.RTF := FBBoxMaxInOCS;
    end
    else
    begin
      vp.BoundingBox.LBN := NulVertex;
      vp.BoundingBox.RTF := NulVertex;
    end;

    CalcActualVisible(DC.DrawingContext.VActuality);
  end;

  { --- Этап формирования визуального представления --- }
  if ((Stage = EFAllStages) or (EFDraw in Stage))
    and not (ESTemp in State)
    and (DCODrawable in DC.Options)
  then
  begin
    Representation.Clear;

    { Рисуем все контуры примитивов через стандартный механизм Representation.
      Это обеспечивает корректную работу с line types и отсечением.
      Аналог: GDBObjCircle.FormatEntity → Representation.DrawPolyLineWithLT }
    if FHasContourVertices and (FContourVertices.Count > 0) then
    begin
      Representation.DrawPolyLineWithLT(DC, FContourVertices, vp, True, True);
      programlog.LogOutFormatStr(
        'uzeentacdproxy: FormatEntity drew %d contour vertices into Representation',
        [FContourVertices.Count], LM_Info);
    end
    else if not FBBoxLoaded then
    begin
      programlog.LogOutFormatStr(
        'uzeentacdproxy: FormatEntity no supported primitives, object skipped in zcUI',
        [], LM_Info);
    end;
  end;

  if Assigned(EntExtensions) then
    EntExtensions.RunOnAfterEntityFormat(@self, drawing, DC);
end;

{ Отрисовывает геометрию через Representation (стандартный путь ZCAD) }
procedure GDBObjAcdProxy.DrawGeometry(lw: integer; var DC: TDrawContext;
  const inFrustumState: TInBoundingVolume);
begin
  Representation.DrawGeometry(DC, vp.BoundingBox, inFrustumState);
  inherited;
end;

{ Вычисляет попадание BBox объекта в усечённую пирамиду видимости }
function GDBObjAcdProxy.CalcTrueInFrustum(
  const frustum: TzeFrustum): TInBoundingVolume;
var
  I: Integer;
  D1, D2: Double;
begin
  if not FBBoxLoaded then
  begin
    Result := IREmpty;
    Exit;
  end;

  { Проверяем оба угла BBox по каждой из 6 плоскостей frustum }
  for I := 0 to 5 do
  begin
    D1 := frustum.v[I].v[0] * FBBoxMinInOCS.x
        + frustum.v[I].v[1] * FBBoxMinInOCS.y
        + frustum.v[I].v[2] * FBBoxMinInOCS.z
        + frustum.v[I].v[3];
    D2 := frustum.v[I].v[0] * FBBoxMaxInOCS.x
        + frustum.v[I].v[1] * FBBoxMaxInOCS.y
        + frustum.v[I].v[2] * FBBoxMaxInOCS.z
        + frustum.v[I].v[3];

    { Если оба угла за плоскостью — объект вне frustum }
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
  { TODO: применить матрицу трансформации к FProxyDataBytes или к вершинам }
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
  { TODO: скопировать FProxyDataBytes и вычисленные данные }
end;

{ Добавляет одну контрольную точку в центре BBox }
procedure GDBObjAcdProxy.addcontrolpoints(tdesc: Pointer);
var
  pdesc: controlpointdesc;
begin
  if not FHasCenterPoint then
    Exit;

  PSelectedObjDesc(tdesc)^.pcontrolpoint^.init(1);

  pdesc.selected := False;
  pdesc.PDrawable := nil;
  pdesc.pointtype := os_begin;
  pdesc.attr := [CPA_Strech];
  pdesc.worldcoord := FCenterPoint;

  PSelectedObjDesc(tdesc)^.pcontrolpoint^.PushBackData(pdesc);
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
  { TODO: спроецировать BBox на экран }
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
  { Регистрируем тип объекта в фабрике DXF-сущностей }
  RegisterDXFEntity(
    GDBAcdProxyID,
    'ACAD_PROXY_ENTITY',
    'ProxyEntity',
    @AllocAcdProxy,
    @AllocAndInitAcdProxy);

  programlog.LogOutFormatStr(
    'uzeentacdproxy: Registered ACAD_PROXY_ENTITY, proxy OpCode handlers: %d',
    [TProxyOpCodeDispatcher.GetRegisteredCount], LM_Info);

end.
