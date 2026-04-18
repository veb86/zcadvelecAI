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
  uzestylestexts,
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

    { Разбирает FProxyDataBytes и создаёт подпримитивы в ConstObjArray }
    procedure BuildSubEntities(var drawing: TDrawingDef;
      var DC: TDrawContext);

    { Создаёт подпримитив-линию из двух соседних точек контура }
    procedure CreateLineSubEntity(
      const P1, P2: TzePoint3d;
      const Contour: TProxyContour;
      var drawing: TDrawingDef; var DC: TDrawContext);

    { Создаёт подпримитив-солид из заполненного контура }
    procedure CreateSolidSubEntity(
      const Contour: TProxyContour;
      var drawing: TDrawingDef; var DC: TDrawContext);

    { Создаёт подпримитив-текст из TProxyTextItem }
    procedure CreateTextSubEntity(
      const Item: TProxyTextItem;
      var drawing: TDrawingDef; var DC: TDrawContext);

    { Переводит точку proxy graphic в локальные координаты proxy entity }
    function ToLocalPoint(
      const Pt: TzePoint3d): TzePoint3d;

    { Проверяет, объект находится внутри блока }
    function HasOwnerMatrix: Boolean;

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

    { Применяет матрицу трансформации }
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
  uzeentmtext,
  uzeentabstracttext,
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

{ Проверяет, объект находится внутри блока }
function GDBObjAcdProxy.HasOwnerMatrix: Boolean;
begin
  Result := bp.ListPos.owner <> nil;
end;

{ Переводит точку proxy graphic в локальные координаты proxy entity }
function GDBObjAcdProxy.ToLocalPoint(
  const Pt: TzePoint3d): TzePoint3d;
begin
  Result := VertexSub(Pt, FProxyGripOffset);
end;

{ Создаёт подпримитив-линию из двух точек.
  Линия получает индивидуальные визуальные свойства из контура. }
procedure GDBObjAcdProxy.CreateLineSubEntity(
  const P1, P2: TzePoint3d;
  const Contour: TProxyContour;
  var drawing: TDrawingDef; var DC: TDrawContext);
var
  WP1, WP2: TzePoint3d;
  SubEnt: PGDBObjEntity;
  ContourLW: TGDBLineWeight;
begin
  WP1 := ToLocalPoint(P1);
  WP2 := ToLocalPoint(P2);

  ContourLW := Contour.LineWeight;
  if (ContourLW = LnWtByLayer) or (ContourLW = LnWtByBlock)
    or (ContourLW = LnWtByLwDefault) then
    ContourLW := vp.LineWeight;

  SubEnt := ENTF_CreateLine(
    PGDBObjGenericSubEntry(@self),
    @ConstObjArray,
    vp.Layer, vp.LineType, ContourLW, vp.Color,
    WP1, WP2);

  if SubEnt <> nil then
    SubEnt^.FormatEntity(drawing, DC);
end;

{ Создаёт подпримитив-солид из заполненного контура.
  Для контуров с 3 вершинами — один треугольник.
  Для контуров с 4+ вершинами — серия треугольников веером. }
procedure GDBObjAcdProxy.CreateSolidSubEntity(
  const Contour: TProxyContour;
  var drawing: TDrawingDef; var DC: TDrawContext);
var
  ir: itrec;
  pV: PzePoint3d;
  Points: array of TzePoint3d;
  PointCount, I: Integer;
  SubEnt: PGDBObjEntity;
  ContourLW: TGDBLineWeight;
begin
  if Contour.Vertices.Count < 3 then
    Exit;

  PointCount := Contour.Vertices.Count;
  SetLength(Points, PointCount);
  I := 0;
  pV := Contour.Vertices.beginiterate(ir);
  while pV <> nil do
  begin
    Points[I] := ToLocalPoint(pV^);
    Inc(I);
    pV := Contour.Vertices.iterate(ir);
  end;

  ContourLW := Contour.LineWeight;
  if (ContourLW = LnWtByLayer) or (ContourLW = LnWtByBlock)
    or (ContourLW = LnWtByLwDefault) then
    ContourLW := vp.LineWeight;

  { Триангуляция веером: вершина 0 — общая для всех треугольников }
  for I := 1 to PointCount - 2 do
  begin
    if I + 1 < PointCount then
    begin
      SubEnt := ENTF_CreateSolid(
        PGDBObjGenericSubEntry(@self),
        @ConstObjArray,
        vp.Layer, vp.LineType, ContourLW, vp.Color,
        Points[0], Points[I], Points[I + 1]);
      if SubEnt <> nil then
        SubEnt^.FormatEntity(drawing, DC);
    end;
  end;
end;

{ Создаёт подпримитив многострочного текста (MTEXT) из TProxyTextItem.
  Текст внутри таблиц и других proxy-объектов хранится как многострочный
  примитив (GDBObjMText), что позволяет корректно отображать форматирование
  (шрифты, стили, выравнивание) и многострочное содержимое. }
procedure GDBObjAcdProxy.CreateTextSubEntity(
  const Item: TProxyTextItem;
  var drawing: TDrawingDef; var DC: TDrawContext);
var
  pMText: PGDBObjMText;
  TXTStyle: PGDBTextStyle;
  InsertPt: TzePoint3d;
begin
  InsertPt := ToLocalPoint(Item.Insert);

  { Получаем стиль текста.
    Внутри proxy-объекта сохранено имя файла шрифта (например, "times.ttf"
    или "txt.shx"), а не имя стиля. Поэтому сначала пробуем найти стиль,
    чей FontFile совпадает с этим именем файла — тогда разные строки с
    разными шрифтами получат соответствующие стили из таблицы стилей
    чертежа (сравнение с учётом и без учёта расширения).
    Для обратной совместимости и исключения ситуаций, когда в потоке
    случайно оказалось имя стиля, дополнительно проверяем FindStyle
    по имени. В крайнем случае используется стиль "Standard". }
  TXTStyle := nil;
  if Item.FontName <> '' then
  begin
    TXTStyle := drawing.GetTextStyleTable^.FindStyleByFont(
      Item.FontName);
    if TXTStyle = nil then
      TXTStyle := drawing.GetTextStyleTable^.FindStyle(
        Item.FontName, False);
  end;
  if TXTStyle = nil then
    TXTStyle := drawing.GetTextStyleTable^.FindStyle(
      'Standard', False);
  if TXTStyle = nil then
    TXTStyle := PGDBTextStyle(
      drawing.GetTextStyleTable^.getDataMutable(0));

  if TXTStyle = nil then
  begin
    programlog.LogOutFormatStr(
      'uzeentacdproxy: CreateTextSubEntity no font, skip',
      [], LM_Info);
    Exit;
  end;

  { Создаём MTEXT-подпримитив: внутри таблиц и сложных proxy-объектов
    текст хранится как многострочный, что позволяет обрабатывать
    форматирование и переносы строк корректно. }
  pMText := pointer(
    ConstObjArray.CreateInitObj(GDBMtextID, @self));
  if pMText = nil then
    Exit;

  pMText^.vp.Layer := vp.Layer;
  pMText^.vp.LineType := vp.LineType;
  pMText^.vp.LineWeight := vp.LineWeight;
  pMText^.vp.Color := vp.Color;
  { Template — шаблон с форматированием, при пустом Content он будет
    использован как исходный текст в FormatContent. }
  pMText^.Template := Item.Text;
  pMText^.TXTStyle := TXTStyle;
  pMText^.Local.P_insert := InsertPt;
  pMText^.textprop.size := Item.Height;
  pMText^.textprop.wfactor := Item.WidthFactor;
  pMText^.textprop.oblique := 0;
  pMText^.textprop.justify := jsbl;
  { Ширина 0 отключает принудительный перенос строк — MTEXT
    переносит только по явным символам #10. }
  pMText^.Width := 0;
  pMText^.linespacef := 1;
  pMText^.WrapMode := mwmByWord;

  { Устанавливаем поворот через базис OX }
  if Abs(Item.Angle) > 1e-10 then
  begin
    pMText^.Local.basis.ox.x := Cos(Item.Angle);
    pMText^.Local.basis.ox.y := Sin(Item.Angle);
    pMText^.Local.basis.ox.z := 0;
  end;

  pMText^.FormatEntity(drawing, DC);

  programlog.LogOutFormatStr(
    'uzeentacdproxy: CreateTextSubEntity MTEXT "%s" at (%.3f,%.3f)'
    + ' font="%s" style="%s"',
    [Item.Text, InsertPt.x, InsertPt.y,
     Item.FontName, TXTStyle^.Name], LM_Info);
end;

{ Разбирает FProxyDataBytes и создаёт подпримитивы.
  Контуры из парсера преобразуются:
  - незаполненный контур → серия линий (GDBObjLine)
  - заполненный контур → солиды (GDBObjSolid)
  - текстовые элементы → GDBObjMText (многострочный текст) }
procedure GDBObjAcdProxy.BuildSubEntities(
  var drawing: TDrawingDef; var DC: TDrawContext);
var
  Parser: TProxyGraphicParser;
  ParseResult: TProxyGraphicParseResult;
  I: Integer;
  ir: itrec;
  pV, pVNext: PzePoint3d;
  SubEntCount: Integer;
begin
  if Length(FProxyDataBytes) = 0 then
    Exit;

  { Очищаем предыдущие подпримитивы }
  ConstObjArray.Free;
  ConstObjArray.init(8);

  Parser := TProxyGraphicParser.Create(FProxyDataBytes);
  try
    ParseResult := Parser.Parse;
    SubEntCount := 0;

    { Вычисляем BBox и смещение ДО создания подпримитивов,
      чтобы ToLocalPoint корректно пересчитывал координаты }
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

    { Создаём подпримитивы из контуров }
    for I := 0 to ParseResult.ContourCount - 1 do
    begin
      if ParseResult.Contours[I].Vertices.Count = 0 then
        Continue;

      { Заполненные контуры → солиды }
      if ParseResult.Contours[I].Filled then
      begin
        CreateSolidSubEntity(
          ParseResult.Contours[I], drawing, DC);
        Inc(SubEntCount);
      end;

      { Контуры с линиями → серия подпримитивов-линий.
        Каждый отрезок — отдельный GDBObjLine с индивидуальными
        свойствами (цвет, толщина линии, тип линии). }
      if ParseResult.Contours[I].Vertices.Count >= 2 then
      begin
        pV := ParseResult.Contours[I].Vertices.beginiterate(ir);
        pVNext := ParseResult.Contours[I].Vertices.iterate(ir);
        while pVNext <> nil do
        begin
          CreateLineSubEntity(
            pV^, pVNext^,
            ParseResult.Contours[I], drawing, DC);
          Inc(SubEntCount);
          pV := pVNext;
          pVNext := ParseResult.Contours[I].Vertices.iterate(ir);
        end;

        { Замыкающий отрезок для замкнутых контуров }
        if ParseResult.Contours[I].Closed
          and (ParseResult.Contours[I].Vertices.Count > 2)
        then
        begin
          pVNext := ParseResult.Contours[I].Vertices.getDataMutable(0);
          CreateLineSubEntity(
            pV^, pVNext^,
            ParseResult.Contours[I], drawing, DC);
          Inc(SubEntCount);
        end;
      end;
    end;

    { Создаём подпримитивы из текстовых элементов }
    for I := 0 to High(ParseResult.TextItems) do
    begin
      CreateTextSubEntity(
        ParseResult.TextItems[I], drawing, DC);
      Inc(SubEntCount);
    end;

    programlog.LogOutFormatStr(
      'uzeentacdproxy: BuildSubEntities created %d sub-entities '
      + 'from %d contours and %d texts',
      [SubEntCount, ParseResult.ContourCount,
       Length(ParseResult.TextItems)], LM_Info);

  finally
    { Освобождаем контуры результата парсера }
    for I := 0 to Length(ParseResult.Contours) - 1 do
      ParseResult.Contours[I].Vertices.done;
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

  CalcObjMatrix(@drawing);

  { Делегируем форматирование подпримитивов GDBObjComplex }
  inherited FormatEntity(drawing, DC, Stage);

  if FProxyBBoxLoaded then
  begin
    vp.BoundingBox.LBN := FProxyBBoxMin;
    vp.BoundingBox.RTF := FProxyBBoxMax;
    ConstObjArray.ObjTree.BoundingBox := vp.BoundingBox;

    { Выводим в лог координаты BBox и ручки (grip) для диагностики }
    programlog.LogOutFormatStr(
      'uzeentacdproxy: FormatEntity bbox min=(%.3f,%.3f,%.3f)'
      + ' max=(%.3f,%.3f,%.3f)',
      [FProxyBBoxMin.x, FProxyBBoxMin.y, FProxyBBoxMin.z,
       FProxyBBoxMax.x, FProxyBBoxMax.y, FProxyBBoxMax.z], LM_Info);
    programlog.LogOutFormatStr(
      'uzeentacdproxy: FormatEntity grip center=(%.3f,%.3f,%.3f)',
      [GetCenterPoint.x, GetCenterPoint.y, GetCenterPoint.z], LM_Info);
  end;

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

procedure GDBObjAcdProxy.TransformAt(p: PGDBObjEntity;
  t_matrix: PzeTypedMatrix4d);
begin
  inherited TransformAt(p, t_matrix);
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
