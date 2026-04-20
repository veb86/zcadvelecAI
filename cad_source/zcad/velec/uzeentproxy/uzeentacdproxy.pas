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
  - ProxyEntity наследуется от GDBObjComplex и является контейнером
    подпримитивов (линии, полилинии, окружности, дуги, солиды, текст).
  - Парсинг Proxy Graphic выполняется в FormatEntity через
    TProxyGraphicParser.
  - Каждый тип примитива внутри прокси регистрируется в
    TProxyOpCodeDispatcher в своём модуле.
  - После парсинга контуры преобразуются в подпримитивы и
    добавляются в ConstObjArray.
  - Отрисовка делегируется подпримитивам через механизм
    GDBObjComplex: каждый подпримитив имеет собственную
    Representation, цвет, толщину линии и тип линии.

  Зависимости от модулей-парсеров:
  - uzeentproxyparsercircle    — OpCode=2
  - uzeentproxyparsertext      — OpCode=10, 38
  - uzeentproxyparserarc       — OpCode=4
  - uzeentproxyparserpolyline  — OpCode=6
  - uzeentproxyparserpolylinewithnormals — OpCode=32
  - uzeentproxyparserpolygon   — OpCode=7
  - uzeentproxyparserlwpolyline — OpCode=33
  - uzeentproxyparserellipse   — OpCode=44
  - uzeentproxyparsershell     — OpCode=9
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
  uzeentcomplex,
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
  uzeentproxymanager,
  uzeentproxygraphicparser,
  { Подключаем модули-парсеры примитивов. Каждый регистрирует свой
    обработчик в TProxyOpCodeDispatcher при инициализации модуля.
    Чтобы отключить конкретный примитив — закомментировать строку: }
  uzeentproxyparsercircle,
  uzeentproxyparsertext,
  uzeentproxyparserarc,
  uzeentproxyparserpolyline,
  uzeentproxyparserpolylinewithnormals,
  uzeentproxyparserpolygon,
  uzeentproxyparserlwpolyline,
  uzeentproxyparserellipse,
  uzeentproxyparsershell,
  UGDBSelectedObjArray,
  uzesnap,
  gzctnrVectorTypes,
  gzctnrVector,
  UGDBPoint3DArray,
  UGDBVisibleTreeArray,
  UGDBVisibleOpenArray,
  uzeentgenericsubentry,
  uzeentitiesmanager,
  uzepalette,
  uzestyleslinetypes;

type
  PGDBObjAcdProxy = ^GDBObjAcdProxy;

  { Прокси-объект AutoCAD (ACAD_PROXY_ENTITY).

    Наследуется от GDBObjComplex — составной примитив с контейнером
    подпримитивов. Парсит бинарный Proxy Graphic из DXF, создаёт
    подпримитивы (линии, окружности, дуги, солиды, многострочный текст)
    и добавляет их в ConstObjArray. Отрисовка делегируется подпримитивам. }
  GDBObjAcdProxy = object(GDBObjComplex)
  public
    { Параметры трансформации (по образцу GDBObjBlockInsert).
      Поддерживают перемещение/масштабирование/поворот/зеркализацию
      прокси-объекта. Хранятся отдельно, чтобы пережить пересборку
      матрицы objmatrix в CalcObjMatrix. }
    scale: TzePoint3d;
    rotate: double;
  private
    { Сырые байты Proxy Graphic (код 310 из DXF) }
    FProxyDataBytes: TBytes;

    { Метаданные ACAD_PROXY_ENTITY из DXF }
    FProxyClassID: Integer;
    FAppClassID: Integer;
    FEntityDataSize: Integer;
    FObjectDataSize: Integer;
    FDrawingFormat: Integer;
    FOriginalDataFormat: Integer;

    { Флаг: подпримитивы уже построены }
    FSubEntitiesBuilt: Boolean;

    { BBox, рассчитанный из Proxy Graphic, до пересчёта подпримитивов }
    FProxyBBoxLoaded: Boolean;
    FProxyBBoxMin: TzePoint3d;
    FProxyBBoxMax: TzePoint3d;
    FProxyGripOffset: TzePoint3d;

    { Разбирает FProxyDataBytes и создаёт подпримитивы в ConstObjArray.
      Фактическое создание подпримитивов делегируется зарегистрированным
      в TProxyOpCodeDispatcher построителям (по одному на OpCode) из
      модулей-парсеров uzeentproxyparser*.pas. }
    procedure BuildSubEntities(var drawing: TDrawingDef;
      var DC: TDrawContext);

    { Формирует контекст (владелец, массив подпримитивов, слой/цвет/вес,
      грип-смещение) для передачи в Builder-процедуру каждого OpCode. }
    function MakeBuilderContext(var drawing: TDrawingDef;
      var DC: TDrawContext): TProxySubEntityContext;

  public
    constructor init(own: Pointer; layeraddres: PGDBLayerProp;
      LW: smallint);
    constructor initnul(owner: PGDBObjGenericWithSubordinated);
    destructor done; virtual;

    { Загружает данные объекта из DXF-потока }
    procedure LoadFromDXF(var rdr: TZMemReader;
      ptu: PExtensionData;
      var drawing: TDrawingDef;
      var context: TIODXFLoadContext); virtual;

    { Сохраняет данные объекта в DXF-поток }
    procedure SaveToDXF(var outStream: TZctnrVectorBytes;
      var drawing: TDrawingDef;
      var IODXFContext: TIODXFSaveContext); virtual;

    { Строит подпримитивы и делегирует форматирование GDBObjComplex }
    procedure FormatEntity(var drawing: TDrawingDef;
      var DC: TDrawContext;
      Stage: TEFStages = EFAllStages); virtual;

    { Отрисовка через подпримитивы (наследуется от GDBObjComplex) }
    procedure DrawGeometry(lw: integer; var DC: TDrawContext;
      const inFrustumState: TInBoundingVolume); virtual;

    { Пересчитывает objmatrix с учётом scale и rotate }
    procedure CalcObjMatrix(pdrawing: PTDrawingDef = nil); virtual;

    { Декомпозиция objmatrix обратно в Local.p_insert/scale/rotate,
      чтобы параметры трансформации пережили повторный CalcObjMatrix. }
    procedure ReCalcFromObjMatrix; virtual;

    { Применяет матрицу трансформации (move/scale/rotate/mirror) }
    procedure TransformAt(p: PGDBObjEntity;
      t_matrix: PzeTypedMatrix4d); virtual;

    { Возвращает тип объекта в виде строки }
    function GetObjTypeName: string; virtual;

    { Возвращает числовой идентификатор типа объекта }
    function GetObjType: TObjID; virtual;

    { Создаёт копию объекта }
    function Clone(own: Pointer): PGDBObjEntity; virtual;

    { Геометрический центр proxy graphic для ручки/точки вставки }
    function GetCenterPoint: TzePoint3d; virtual;

    { Устанавливает ручку (grip) в геометрический центр BBox proxy graphic }
    procedure addcontrolpoints(tdesc: Pointer); virtual;

    { Обновляет экранные координаты ручки из геометрического центра BBox }
    procedure remaponecontrolpoint(pdesc: pcontrolpointdesc;
      ProjectProc: GDBProjectProc); virtual;

    { Создаёт новый инициализированный экземпляр }
    class function CreateInstance: PGDBObjAcdProxy; static;
  end;

{ Выделяет память для нового прокси-объекта }
function AllocAcdProxy: Pointer;

{ Выделяет и инициализирует новый прокси-объект }
function AllocAndInitAcdProxy(
  owner: PGDBObjGenericWithSubordinated): PGDBObjAcdProxy;

implementation

uses
  uzeutils;

{ --- Вспомогательные функции --- }

{ Конвертирует hex-строку в массив байт }
function HexStringToBytes(const HexStr: string): TBytes;
var
  I, Len: Integer;
begin
  Len := Length(HexStr) div 2;
  SetLength(Result, Len);
  for I := 0 to Len - 1 do
    Result[I] := Lo(
      StrToIntDef('$' + Copy(HexStr, I * 2 + 1, 2), 0));
end;

{ Конвертирует массив байт в hex-строку }
function BytesToHexString(const Data: TBytes): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to High(Data) do
    Result := Result + IntToHex(Data[I], 2);
end;

{ === GDBObjAcdProxy === }

constructor GDBObjAcdProxy.init;
begin
  inherited init(own, layeraddres, LW);
  FSubEntitiesBuilt := False;
  FProxyBBoxLoaded := False;
  FProxyGripOffset := NulVertex;
  FProxyClassID := 498;
  FAppClassID := 499;
  FEntityDataSize := 0;
  FObjectDataSize := 0;
  FDrawingFormat := 15;
  FOriginalDataFormat := 0;
  scale := ScaleOne;
  rotate := 0;
end;

constructor GDBObjAcdProxy.initnul;
begin
  inherited initnul;
  bp.ListPos.Owner := owner;
  FSubEntitiesBuilt := False;
  FProxyBBoxLoaded := False;
  FProxyGripOffset := NulVertex;
  FProxyClassID := 498;
  FAppClassID := 499;
  FEntityDataSize := 0;
  FObjectDataSize := 0;
  FDrawingFormat := 15;
  FOriginalDataFormat := 0;
  scale := ScaleOne;
  rotate := 0;
end;

destructor GDBObjAcdProxy.done;
begin
  SetLength(FProxyDataBytes, 0);
  inherited done;
end;

{ Загружает данные объекта из DXF-потока }
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
    if not LoadFromDXFObjShared(
      rdr, Code, ptu, drawing, context) then
      case Code of
        90:
          FProxyClassID := StrToIntDef(rdr.ParseString, 498);
        91:
          FAppClassID := StrToIntDef(rdr.ParseString, 499);
        92, 160:
          rdr.SkipString;
        93:
          FEntityDataSize := StrToIntDef(rdr.ParseString, 0);
        94:
          FObjectDataSize := StrToIntDef(rdr.ParseString, 0);
        95:
          FDrawingFormat := StrToIntDef(rdr.ParseString, 15);
        70:
          FOriginalDataFormat := StrToIntDef(
            rdr.ParseString, 0);
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
      'uzeentacdproxy: LoadFromDXF loaded %d bytes',
      [Length(FProxyDataBytes)], LM_Info);
  end
  else
  begin
    SetLength(FProxyDataBytes, 0);
    programlog.LogOutFormatStr(
      'uzeentacdproxy: LoadFromDXF no proxy data',
      [], LM_Info);
  end;

  FSubEntitiesBuilt := False;
  FProxyBBoxLoaded := False;
end;

{ Сохраняет данные объекта в DXF-поток }
procedure GDBObjAcdProxy.SaveToDXF(
  var outStream: TZctnrVectorBytes;
  var drawing: TDrawingDef;
  var IODXFContext: TIODXFSaveContext);
const
  MaxHexCharsPerChunk = 254;
var
  HexStr, Chunk: string;
  GraphicsSize, Offset, ChunkLen: Integer;
begin
  SaveToDXFObjPrefix(outStream, 'ACAD_PROXY_ENTITY',
    'AcDbProxyEntity', IODXFContext);

  dxfIntegerout(outStream, 90, FProxyClassID);
  dxfIntegerout(outStream, 91, FAppClassID);

  GraphicsSize := Length(FProxyDataBytes);
  dxfIntegerout(outStream, 92, GraphicsSize);

  if GraphicsSize > 0 then
  begin
    HexStr := BytesToHexString(FProxyDataBytes);
    Offset := 1;
    while Offset <= Length(HexStr) do
    begin
      ChunkLen := Length(HexStr) - Offset + 1;
      if ChunkLen > MaxHexCharsPerChunk then
        ChunkLen := MaxHexCharsPerChunk;
      Chunk := Copy(HexStr, Offset, ChunkLen);
      dxfStringWithoutEncodeOut(outStream, 310, Chunk);
      Inc(Offset, ChunkLen);
    end;
  end;

  dxfIntegerout(outStream, 93, FEntityDataSize);
  dxfIntegerout(outStream, 94, FObjectDataSize);
  dxfIntegerout(outStream, 95, FDrawingFormat);
  dxfIntegerout(outStream, 70, FOriginalDataFormat);

  programlog.LogOutFormatStr(
    'uzeentacdproxy: SaveToDXF wrote %d bytes',
    [GraphicsSize], LM_Info);
end;

{ Строит контекст для передачи в Builder-процедуру примитива.
  Все данные владельца (слой, цвет, грипп, массив подпримитивов)
  упаковываются в typed Pointer-ы, чтобы исключить циклическую
  зависимость между модулями парсеров и этим модулем. }
function GDBObjAcdProxy.MakeBuilderContext(
  var drawing: TDrawingDef; var DC: TDrawContext): TProxySubEntityContext;
begin
  Result.OwnerEntity      := @Self;
  Result.SubEntitiesArray := @ConstObjArray;
  Result.Drawing          := @drawing;
  Result.DC               := @DC;
  Result.OwnerLayer       := vp.Layer;
  Result.OwnerLineType    := vp.LineType;
  Result.OwnerLineWeight  := vp.LineWeight;
  Result.OwnerColor       := Integer(vp.Color);
  { По умолчанию — вес владельца; реальный вес каждого примитива
    подставляется в BuildSubEntities на каждой итерации цикла. }
  Result.PrimitiveLineWeight := vp.LineWeight;
  Result.GripOffset       := FProxyGripOffset;
end;

{ Разбирает FProxyDataBytes и создаёт подпримитивы.
  Сам модуль не знает, как именно строить GDB-объекты из каждого
  примитива: построением занимаются Builder-процедуры, зарегистрированные
  в TProxyOpCodeDispatcher соответствующими модулями-парсерами
  uzeentproxyparser*.pas. Этот метод лишь организует проход по
  результату парсинга и вызывает диспетчер. }
procedure GDBObjAcdProxy.BuildSubEntities(
  var drawing: TDrawingDef; var DC: TDrawContext);
var
  Parser: TProxyGraphicParser;
  ParseResult: TProxyGraphicParseResult;
  Context: TProxySubEntityContext;
  I: Integer;
  BuiltCount: Integer;
begin
  if Length(FProxyDataBytes) = 0 then
    Exit;

  { Очищаем предыдущие подпримитивы }
  ConstObjArray.Free;
  ConstObjArray.init(8);

  Parser := TProxyGraphicParser.Create(FProxyDataBytes);
  try
    ParseResult := Parser.Parse;

    { Вычисляем BBox и смещение ДО создания подпримитивов,
      чтобы Builder-процедуры корректно переводили координаты
      в локальную систему подпримитивов }
    if ParseResult.BBoxLoaded then
    begin
      FProxyBBoxMin := ParseResult.BBoxMin;
      FProxyBBoxMax := ParseResult.BBoxMax;
      FProxyBBoxLoaded := True;
      FProxyGripOffset := Vertexmorph(
        FProxyBBoxMin, FProxyBBoxMax, 0.5);
      if IsVectorNul(Local.P_insert) then
        Local.P_insert := FProxyGripOffset;
      vp.BoundingBox.LBN := FProxyBBoxMin;
      vp.BoundingBox.RTF := FProxyBBoxMax;

      programlog.LogOutFormatStr(
        'uzeentacdproxy: BuildSubEntities gripOffset='
        + '(%.3f,%.3f,%.3f)',
        [FProxyGripOffset.x, FProxyGripOffset.y,
         FProxyGripOffset.z], LM_Info);
    end;

    { Формируем контекст построителей только один раз — указатели на
      массив подпримитивов, слой/тип линии/вес/цвет владельца и грип
      неизменны в пределах построения. }
    Context := MakeBuilderContext(drawing, DC);

    { Делегируем создание подпримитивов модулям-парсерам.
      Каждый примитив в порядке появления в Proxy Graphic передаётся
      в Builder, зарегистрированный для его OpCode. }
    BuiltCount := 0;
    for I := 0 to High(ParseResult.Primitives) do
    begin
      { Подставляем вес линии, зафиксированный парсером для конкретного
        примитива (SetLineweight OpCode=23 из потока Proxy Graphic).
        Без этого все подпримитивы получали бы одинаковый вес владельца
        прокси-объекта, игнорируя значения, заданные внутри графики. }
      Context.PrimitiveLineWeight := ParseResult.Primitives[I].LineWeight;
      programlog.LogOutFormatStr(
        'uzeentacdproxy: BuildSubEntities[%d] OpCode=%d lineweight=%d',
        [I, ParseResult.Primitives[I].OpCode,
         ParseResult.Primitives[I].LineWeight], LM_Info);
      if TProxyOpCodeDispatcher.BuildSubEntities(
        ParseResult.Primitives[I].OpCode,
        ParseResult.Primitives[I].HandlerResult,
        Context) then
        Inc(BuiltCount);
    end;

    programlog.LogOutFormatStr(
      'uzeentacdproxy: BuildSubEntities built %d/%d primitives',
      [BuiltCount, Length(ParseResult.Primitives)], LM_Info);

  finally
    { Освобождаем вершины всех примитивов результата и сам парсер }
    FreeParseResult(ParseResult);
    Parser.Free;
  end;

  FSubEntitiesBuilt := True;
end;

{ Строит подпримитивы и делегирует форматирование GDBObjComplex.
  Подпримитивы создаются один раз при первом вызове или при
  необходимости перестроения. }
procedure GDBObjAcdProxy.FormatEntity(var drawing: TDrawingDef;
  var DC: TDrawContext; Stage: TEFStages);
begin
  if Assigned(EntExtensions) then
    EntExtensions.RunOnBeforeEntityFormat(@self, drawing, DC);

  if not FSubEntitiesBuilt then
    BuildSubEntities(drawing, DC);

  { Пересчитываем objmatrix с учётом scale/rotate ДО форматирования
    подпримитивов, чтобы подпримитивы унаследовали полную матрицу
    владельца через bp.ListPos.owner.GetMatrix. }
  CalcObjMatrix(@drawing);

  { Форматируем подпримитивы и вычисляем BBox из них: так BBox
    получается в мировых координатах с учётом трансформации. }
  ConstObjArray.FormatEntity(drawing, DC);
  calcbb(DC);
  self.BuildGeometry(drawing);
  CalcActualVisible(DC.DrawingContext.VActuality);

  if FProxyBBoxLoaded then
    { Выводим в лог координаты BBox и ручки (grip) для диагностики }
    programlog.LogOutFormatStr(
      'uzeentacdproxy: FormatEntity bbox min=(%.3f,%.3f,%.3f)'
      + ' max=(%.3f,%.3f,%.3f) grip=(%.3f,%.3f,%.3f)',
      [vp.BoundingBox.LBN.x, vp.BoundingBox.LBN.y, vp.BoundingBox.LBN.z,
       vp.BoundingBox.RTF.x, vp.BoundingBox.RTF.y, vp.BoundingBox.RTF.z,
       GetCenterPoint.x, GetCenterPoint.y, GetCenterPoint.z], LM_Info);

  if Assigned(EntExtensions) then
    EntExtensions.RunOnAfterEntityFormat(@self, drawing, DC);
end;

{ Отрисовка через подпримитивы — делегируется GDBObjComplex }
procedure GDBObjAcdProxy.DrawGeometry(lw: integer;
  var DC: TDrawContext;
  const inFrustumState: TInBoundingVolume);
begin
  inherited DrawGeometry(lw, DC, inFrustumState);
end;

{ Пересчёт objmatrix: базовая матрица (translate(Local.p_insert)*basis) из
  предка, затем поворот вокруг Z на угол rotate и масштабирование scale.
  Та же последовательность, что и в GDBObjBlockInsert.CalcObjMatrix;
  сохраняется параметры трансформации в матрице, чтобы их нельзя было
  "потерять" при повторном форматировании. }
procedure GDBObjAcdProxy.CalcObjMatrix(pdrawing: PTDrawingDef = nil);
var
  m1: TzeTypedMatrix4d;
begin
  inherited CalcObjMatrix(pdrawing);

  if not IsZero(rotate) then
  begin
    m1 := CreateRotationMatrixZ(rotate);
    objMatrix := MatrixMultiply(m1, objMatrix);
  end;

  if (not SameValue(scale.x, 1.0))
    or (not SameValue(scale.y, 1.0))
    or (not SameValue(scale.z, 1.0)) then
  begin
    m1 := CreateScaleMatrix(scale);
    objMatrix := MatrixMultiply(m1, objMatrix);
  end;

  P_insert_in_WCS := VectorTransform3D(NulVertex, objMatrix);
end;

{ Декомпозиция objMatrix в Local.p_insert/basis/scale/rotate, чтобы после
  внешней трансформации (TransformAt) параметры пережили последующий вызов
  CalcObjMatrix. По образцу GDBObjBlockInsert.ReCalcFromObjMatrix. }
procedure GDBObjAcdProxy.ReCalcFromObjMatrix;
var
  ox, tv: TzePoint3d;
begin
  inherited ReCalcFromObjMatrix;
  Local := GetPInsertInOCSBymatrix(objMatrix, scale);

  ox := GetXfFromZ(Local.basis.oz);
  tv := Local.basis.ox;
  if scale.x < -eps then
    tv := VertexMulOnSc(tv, -1);
  rotate := scalardot(tv, ox);
  if rotate > 1.0 then
    rotate := 1.0
  else if rotate < -1.0 then
    rotate := -1.0;
  rotate := arccos(rotate);
  if scalardot(tv, VectorDot(Local.basis.oz,
    GetXfFromZ(Local.basis.oz))) < -eps then
    rotate := 2 * pi - rotate;
end;

procedure GDBObjAcdProxy.TransformAt(p: PGDBObjEntity;
  t_matrix: PzeTypedMatrix4d);
begin
  inherited TransformAt(p, t_matrix);
  ReCalcFromObjMatrix;
end;

function GDBObjAcdProxy.GetObjTypeName: string;
begin
  Result := 'ACAD_PROXY_ENTITY';
end;

function GDBObjAcdProxy.GetObjType: TObjID;
begin
  Result := GDBAcdProxyID;
end;

function GDBObjAcdProxy.GetCenterPoint: TzePoint3d;
begin
  Result := P_insert_in_WCS;
end;

{ Устанавливает ручку управления (grip) в геометрический центр BBox.
  Базовый GDBObjComplex использует P_insert_in_WCS, который для прокси
  всегда равен (0,0,0). Переопределяем, чтобы ручка совпадала с центром
  объекта. }
procedure GDBObjAcdProxy.addcontrolpoints(tdesc: Pointer);
var
  pdesc: controlpointdesc;
  GripCenter: TzePoint3d;
begin
  GripCenter := GetCenterPoint;
  PSelectedObjDesc(tdesc)^.pcontrolpoint^.init(1);
  pdesc.selected := False;
  pdesc.PDrawable := nil;
  pdesc.pointtype := os_point;
  pdesc.worldcoord := GripCenter;
  PSelectedObjDesc(tdesc)^.pcontrolpoint^.PushBackData(pdesc);

  programlog.LogOutFormatStr(
    'uzeentacdproxy: addcontrolpoints grip=(%.3f,%.3f,%.3f)',
    [GripCenter.x, GripCenter.y, GripCenter.z], LM_Info);
end;

{ Пересчитывает экранные координаты ручки из геометрического центра BBox.
  Базовый GDBObjComplex читает P_insert_in_WCS, что для прокси даёт (0,0,0).
  Переопределяем, чтобы ручка всегда следовала за реальным центром объекта. }
procedure GDBObjAcdProxy.remaponecontrolpoint(pdesc: pcontrolpointdesc;
  ProjectProc: GDBProjectProc);
var
  tv: TzePoint3d;
begin
  if pdesc^.pointtype = os_point then
  begin
    pdesc.worldcoord := GetCenterPoint;
    ProjectProc(pdesc.worldcoord, tv);
    pdesc.dispcoord := ToTzePoint2i(tv);
  end;
end;

{ Создаёт копию прокси-объекта }
function GDBObjAcdProxy.Clone(own: Pointer): PGDBObjEntity;
var
  ClonePtr: PGDBObjAcdProxy;
begin
  ClonePtr := CreateInstance;

  { Копируем бинарные данные proxy graphic }
  SetLength(ClonePtr^.FProxyDataBytes, Length(FProxyDataBytes));
  if Length(FProxyDataBytes) > 0 then
    Move(FProxyDataBytes[0], ClonePtr^.FProxyDataBytes[0],
      Length(FProxyDataBytes));

  { Копируем метаданные DXF }
  ClonePtr^.FProxyClassID := FProxyClassID;
  ClonePtr^.FAppClassID := FAppClassID;
  ClonePtr^.FEntityDataSize := FEntityDataSize;
  ClonePtr^.FObjectDataSize := FObjectDataSize;
  ClonePtr^.FDrawingFormat := FDrawingFormat;
  ClonePtr^.FOriginalDataFormat := FOriginalDataFormat;

  { Подпримитивы будут построены при первом FormatEntity }
  ClonePtr^.FSubEntitiesBuilt := False;
  ClonePtr^.FProxyBBoxLoaded := FProxyBBoxLoaded;
  ClonePtr^.FProxyBBoxMin := FProxyBBoxMin;
  ClonePtr^.FProxyBBoxMax := FProxyBBoxMax;
  ClonePtr^.FProxyGripOffset := FProxyGripOffset;

  { Копируем параметры трансформации }
  ClonePtr^.Local := Local;
  ClonePtr^.scale := scale;
  ClonePtr^.rotate := rotate;

  Result := PGDBObjEntity(ClonePtr);
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
  RegisterDXFEntity(
    GDBAcdProxyID,
    'ACAD_PROXY_ENTITY',
    'ProxyEntity',
    @AllocAcdProxy,
    @AllocAndInitAcdProxy);

  programlog.LogOutFormatStr(
    'uzeentacdproxy: Registered ACAD_PROXY_ENTITY, handlers: %d',
    [TProxyOpCodeDispatcher.GetRegisteredCount], LM_Info);

end.
