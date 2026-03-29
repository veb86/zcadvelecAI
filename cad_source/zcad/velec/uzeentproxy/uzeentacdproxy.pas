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
    в своём модуле (uzeentproxyparsercircle.pas, uzeentproxyparsertext.pas,
    uzeentproxyparserarc.pas, uzeentproxyparserpolyline.pas,
    uzeentproxyparserpolygon.pas, uzeentproxyparserlwpolyline.pas,
    uzeentproxyparserellipse.pas, uzeentproxyparsershell.pas и т.д.)
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
  - uzeentproxyparsercircle    — парсер кругов (OpCode=2)
  - uzeentproxyparsertext      — парсер текста (OpCode=10, 38)
  - uzeentproxyparserarc       — парсер дуг (OpCode=4)
  - uzeentproxyparserpolyline  — парсер полилиний и линий (OpCode=6)
  - uzeentproxyparserpolygon   — парсер полигонов/штриховки (OpCode=7)
  - uzeentproxyparserlwpolyline — парсер 2D полилиний (OpCode=33)
  - uzeentproxyparserellipse   — парсер эллипсов и эллиптических дуг (OpCode=44)
  - uzeentproxyparsershell     — парсер оболочек Shell/PolyFace (OpCode=9)
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
  uzestylestexts,
  uzecamera,
  SysUtils,
  Math,
  uzeentproxymanager,
  uzeentproxygraphicparser,
  uzglgeometry,
  { Подключаем модули-парсеры примитивов. Каждый регистрирует свой обработчик
    в TProxyOpCodeDispatcher при инициализации модуля (секция initialization).
    Чтобы отключить конкретный примитив — закомментировать его строку ниже:
    его initialization не выполнится и OpCode не зарегистрируется. }
  uzeentproxyparsercircle,   { OpCode=2: Circle }
  uzeentproxyparsertext,     { OpCode=10,38: Text, UnicodeText2 }
  uzeentproxyparserarc,      { OpCode=4: CircularArc (ARC) }
  uzeentproxyparserpolyline, { OpCode=6: Polyline/Line }
  uzeentproxyparserpolygon,  { OpCode=7: Polygon/Hatch }
  uzeentproxyparserlwpolyline, { OpCode=33: LwPolyline (LWPOLYLINE) }
  uzeentproxyparserellipse,  { OpCode=44: EllipticArc (ELLIPSE) }
  uzeentproxyparsershell,    { OpCode=9: Shell/PolyFace }
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

    { Метаданные ACAD_PROXY_ENTITY из DXF }
    FProxyClassID: Integer;       { Код 90: ID класса прокси }
    FAppClassID: Integer;         { Код 91: ID класса приложения }
    FEntityDataSize: Integer;     { Код 93: размер данных сущности }
    FObjectDataSize: Integer;     { Код 94: размер данных объекта }
    FDrawingFormat: Integer;      { Код 95: формат чертежа }
    FOriginalDataFormat: Integer; { Код 70: формат исходных данных }

    { Контуры примитивов для раздельной отрисовки }
    FContours: array of TProxyContour;
    { Число заполненных контуров }
    FContourCount: Integer;

    { Текстовые примитивы, собранные при разборе }
    FTextItems: array of TProxyTextItem;

    { Центр BBox — используется как контрольная точка }
    FCenterPoint: TzePoint3d;
    FHasCenterPoint: Boolean;

    { Разбирает FProxyDataBytes и заполняет контуры и BBox }
    procedure ParseProxyData;

    { Применяет матрицу блока-владельца к вершинам и BBox.
      Вызывается после ParseProxyData, если объект находится внутри блока. }
    procedure ApplyOwnerMatrix(const OwnerMatrix: TzeTypedMatrix4d);

    { Отрисовывает текстовые примитивы через Representation.DrawTextContent }
    procedure DrawTextItems(var drawing: TDrawingDef; var DC: TDrawContext);

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

{ Конвертирует массив байт в hex-строку.
  Используется при записи кода 310 в DXF. }
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
  FBBoxLoaded := False;
  FBBoxMinInOCS := NulVertex;
  FBBoxMaxInOCS := NulVertex;
  FContourCount := 0;
  SetLength(FContours, 0);
  FHasCenterPoint := False;
  { Значения по умолчанию для метаданных DXF }
  FProxyClassID := 498;
  FAppClassID := 499;
  FEntityDataSize := 0;
  FObjectDataSize := 0;
  FDrawingFormat := 15;
  FOriginalDataFormat := 0;
end;

constructor GDBObjAcdProxy.initnul;
begin
  inherited initnul(owner);
  FBBoxLoaded := False;
  FBBoxMinInOCS := NulVertex;
  FBBoxMaxInOCS := NulVertex;
  FContourCount := 0;
  SetLength(FContours, 0);
  FHasCenterPoint := False;
  { Значения по умолчанию для метаданных DXF }
  FProxyClassID := 498;
  FAppClassID := 499;
  FEntityDataSize := 0;
  FObjectDataSize := 0;
  FDrawingFormat := 15;
  FOriginalDataFormat := 0;
end;

destructor GDBObjAcdProxy.done;
var
  I: Integer;
begin
  for I := 0 to FContourCount - 1 do
    FContours[I].Vertices.done;
  SetLength(FContours, 0);
  FContourCount := 0;
  inherited done;
end;

{ Загружает данные объекта из DXF-потока.
  Читает hex-данные кода 310, а также метаданные прокси (коды 90,91,93-95,70). }
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
      90:
        FProxyClassID := StrToIntDef(rdr.ParseString, 498);
      91:
        FAppClassID := StrToIntDef(rdr.ParseString, 499);
      92, 160:
        rdr.SkipString; { Размер вычисляется из FProxyDataBytes }
      93:
        FEntityDataSize := StrToIntDef(rdr.ParseString, 0);
      94:
        FObjectDataSize := StrToIntDef(rdr.ParseString, 0);
      95:
        FDrawingFormat := StrToIntDef(rdr.ParseString, 15);
      70:
        FOriginalDataFormat := StrToIntDef(rdr.ParseString, 0);
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

{ Сохраняет данные объекта в DXF-поток.
  Записывает полную структуру ACAD_PROXY_ENTITY:
  заголовок, метаданные, бинарные данные proxy graphic (код 310). }
procedure GDBObjAcdProxy.SaveToDXF(var outStream: TZctnrVectorBytes;
  var drawing: TDrawingDef; var IODXFContext: TIODXFSaveContext);
const
  { Максимум 254 hex-символа на строку (127 байт) }
  MaxHexCharsPerChunk = 254;
var
  HexStr, Chunk: string;
  GraphicsSize, Offset, ChunkLen: Integer;
begin
  SaveToDXFObjPrefix(outStream, 'ACAD_PROXY_ENTITY',
    'AcDbProxyEntity', IODXFContext);

  { Метаданные прокси-объекта }
  dxfIntegerout(outStream, 90, FProxyClassID);
  dxfIntegerout(outStream, 91, FAppClassID);

  { Размер графических данных и сами данные }
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

  { Оставшиеся метаданные }
  dxfIntegerout(outStream, 93, FEntityDataSize);
  dxfIntegerout(outStream, 94, FObjectDataSize);
  dxfIntegerout(outStream, 95, FDrawingFormat);
  dxfIntegerout(outStream, 70, FOriginalDataFormat);

  programlog.LogOutFormatStr(
    'uzeentacdproxy: SaveToDXF wrote %d bytes of proxy data',
    [GraphicsSize], LM_Info);
end;

{ Разбирает FProxyDataBytes через TProxyGraphicParser.
  Заполняет FContours, FBBoxMinInOCS/FBBoxMaxInOCS, FCenterPoint. }
procedure GDBObjAcdProxy.ParseProxyData;
var
  Parser: TProxyGraphicParser;
  ParseResult: TProxyGraphicParseResult;
  I: Integer;
  ir: itrec;
  pV: PzePoint3d;
begin
  if Length(FProxyDataBytes) = 0 then
    Exit;

  { Сбрасываем предыдущие данные }
  for I := 0 to FContourCount - 1 do
    FContours[I].Vertices.done;
  SetLength(FContours, 0);
  FContourCount := 0;
  FBBoxLoaded := False;
  FHasCenterPoint := False;
  SetLength(FTextItems, 0);

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

    { Копируем контуры: каждый примитив хранится отдельно }
    FContourCount := ParseResult.ContourCount;
    if FContourCount > 0 then
    begin
      SetLength(FContours, FContourCount);
      for I := 0 to FContourCount - 1 do
      begin
        FContours[I].Closed := ParseResult.Contours[I].Closed;
        FContours[I].Vertices.init(
          ParseResult.Contours[I].Vertices.Count);
        pV := ParseResult.Contours[I].Vertices.beginiterate(ir);
        while pV <> nil do
        begin
          FContours[I].Vertices.PushBackData(pV^);
          pV := ParseResult.Contours[I].Vertices.iterate(ir);
        end;
      end;
    end;

    { Копируем текстовые примитивы }
    FTextItems := ParseResult.TextItems;

    if Length(FTextItems) > 0 then
      programlog.LogOutFormatStr(
        'uzeentacdproxy: ParseProxyData collected %d text items',
        [Length(FTextItems)], LM_Info);

  finally
    { Освобождаем память контуров результата парсера }
    for I := 0 to Length(ParseResult.Contours) - 1 do
      ParseResult.Contours[I].Vertices.done;
    Parser.Free;
  end;
end;

{ Применяет матрицу блока-владельца к вершинам контуров, BBox и точкам
  вставки текстовых примитивов. Исправляет позиционирование прокси-объектов,
  находящихся внутри блоков: координаты из Proxy Graphic заданы в локальной
  системе координат блока (OCS), а для отображения нужны мировые координаты. }
procedure GDBObjAcdProxy.ApplyOwnerMatrix(const OwnerMatrix: TzeTypedMatrix4d);
var
  ir: itrec;
  pV: PzePoint3d;
  I: Integer;
  TempDouble: Double;
begin
  { Трансформируем вершины каждого контура из OCS блока в WCS }
  for I := 0 to FContourCount - 1 do
  begin
    pV := FContours[I].Vertices.beginiterate(ir);
    while pV <> nil do
    begin
      pV^ := VectorTransform3D(pV^, OwnerMatrix);
      pV := FContours[I].Vertices.iterate(ir);
    end;
  end;

  { Трансформируем углы BBox из OCS блока в WCS }
  if FBBoxLoaded then
  begin
    FBBoxMinInOCS := VectorTransform3D(FBBoxMinInOCS, OwnerMatrix);
    FBBoxMaxInOCS := VectorTransform3D(FBBoxMaxInOCS, OwnerMatrix);
    { Исправляем порядок углов после трансформации: при повороте или
      масштабировании с отражением min и max могут поменяться местами }
    if FBBoxMinInOCS.x > FBBoxMaxInOCS.x then
    begin
      TempDouble := FBBoxMinInOCS.x;
      FBBoxMinInOCS.x := FBBoxMaxInOCS.x;
      FBBoxMaxInOCS.x := TempDouble;
    end;
    if FBBoxMinInOCS.y > FBBoxMaxInOCS.y then
    begin
      TempDouble := FBBoxMinInOCS.y;
      FBBoxMinInOCS.y := FBBoxMaxInOCS.y;
      FBBoxMaxInOCS.y := TempDouble;
    end;
    if FBBoxMinInOCS.z > FBBoxMaxInOCS.z then
    begin
      TempDouble := FBBoxMinInOCS.z;
      FBBoxMinInOCS.z := FBBoxMaxInOCS.z;
      FBBoxMaxInOCS.z := TempDouble;
    end;

    { Пересчитываем центр BBox после трансформации }
    if FHasCenterPoint then
    begin
      FCenterPoint.x := (FBBoxMinInOCS.x + FBBoxMaxInOCS.x) / 2;
      FCenterPoint.y := (FBBoxMinInOCS.y + FBBoxMaxInOCS.y) / 2;
      FCenterPoint.z := (FBBoxMinInOCS.z + FBBoxMaxInOCS.z) / 2;
    end;
  end;

  { Трансформируем точки вставки текстовых примитивов }
  for I := 0 to High(FTextItems) do
    FTextItems[I].Insert := VectorTransform3D(FTextItems[I].Insert, OwnerMatrix);

  programlog.LogOutFormatStr(
    'uzeentacdproxy: ApplyOwnerMatrix applied to %d contours, %d text items',
    [FContourCount, Length(FTextItems)], LM_Info);
end;

{ Отрисовывает все собранные текстовые примитивы.
  Для каждого элемента FTextItems:
  - берёт шрифт из таблицы стилей чертежа (или "Standard" по умолчанию);
  - строит матрицы трансформации аналогично GDBObjText.CalcObjMatrix;
  - вызывает Representation.DrawTextContent. }
procedure GDBObjAcdProxy.DrawTextItems(var drawing: TDrawingDef;
  var DC: TDrawContext);
var
  I: Integer;
  TXTStyle: PGDBTextStyle;
  Item: TProxyTextItem;
  ObjMatrix, DrawMatrix, RotMatrix, ScaleMatrix, TransMatrix: TzeTypedMatrix4d;
  TextOutbound: OutBound4V;
begin
  if Length(FTextItems) = 0 then
    Exit;

  for I := 0 to High(FTextItems) do
  begin
    Item := FTextItems[I];

    { Получаем стиль текста: сначала по имени шрифта, затем "Standard", затем первый }
    TXTStyle := nil;
    if Item.FontName <> '' then
      TXTStyle := drawing.GetTextStyleTable^.FindStyle(Item.FontName, False);
    if TXTStyle = nil then
      TXTStyle := drawing.GetTextStyleTable^.FindStyle('Standard', False);
    if TXTStyle = nil then
      TXTStyle := PGDBTextStyle(drawing.GetTextStyleTable^.getDataMutable(0));

    if (TXTStyle = nil) or (TXTStyle^.pfont = nil) then
    begin
      programlog.LogOutFormatStr(
        'uzeentacdproxy: DrawTextItems[%d] no font found, skip',
        [I], LM_Info);
      Continue;
    end;

    { ObjMatrix: матрица объекта = поворот * перенос в точку вставки.
      Аналогично GDBObjPlainWithOX.CalcObjMatrix + поворот по углу. }
    TransMatrix := CreateTranslationMatrix(Item.Insert);
    RotMatrix := CreateRotationMatrixZ(Item.Angle);
    ObjMatrix := MatrixMultiply(RotMatrix, TransMatrix);

    { DrawMatrix: масштаб ширины и высоты без наклона (ObliqueAngle=0).
      Аналогично GDBObjAbstractText.CalcObjMatrix:
        m3 = scale(wfactor*height, height, height)
        DrawMatrix = m3 * NulTranslation }
    ScaleMatrix := CreateScaleMatrix(
      Item.WidthFactor * Item.Height,
      Item.Height,
      Item.Height);
    DrawMatrix := ScaleMatrix;

    Representation.DrawTextContent(
      DC.drawer,
      Item.Text,
      TXTStyle^.pfont,
      DrawMatrix,
      ObjMatrix,
      Item.Height,
      TextOutbound);

    programlog.LogOutFormatStr(
      'uzeentacdproxy: DrawTextItems[%d] drew text "%s" at (%.3f,%.3f)',
      [I, Item.Text, Item.Insert.x, Item.Insert.y], LM_Info);
  end;
end;

{ Рассчитывает визуальное представление объекта.
  Аналогично GDBObjCircle:
  - EFCalcEntityCS: устанавливает BBox, вызывает разбор данных
  - EFDraw: заполняет Representation через DrawPolyLineWithLT }
procedure GDBObjAcdProxy.FormatEntity(var drawing: TDrawingDef;
  var DC: TDrawContext; Stage: TEFStages);
var
  I: Integer;
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

    { Если объект находится внутри блока — применяем матрицу вставки блока.
      Координаты Proxy Graphic заданы в локальной системе координат блока,
      поэтому для корректного отображения нужно перевести их в WCS.
      Аналогично GDBObjPoint: P_insertInWCS := VectorTransform3D(P_insertInOCS,
      bp.ListPos.owner^.GetMatrix^) }
    if bp.ListPos.owner <> nil then
      ApplyOwnerMatrix(bp.ListPos.owner^.GetMatrix^);

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

    { Рисуем каждый контур отдельно через DrawPolyLineWithLT.
      Это исправляет проблему, когда все примитивы рисовались одной
      непрерывной полилинией, создавая ложные соединения между ними. }
    for I := 0 to FContourCount - 1 do
    begin
      if FContours[I].Vertices.Count > 0 then
        Representation.DrawPolyLineWithLT(
          DC, FContours[I].Vertices, vp,
          FContours[I].Closed, True);
    end;

    if FContourCount > 0 then
      programlog.LogOutFormatStr(
        'uzeentacdproxy: FormatEntity drew %d contours',
        [FContourCount], LM_Info)
    else if not FBBoxLoaded then
      programlog.LogOutFormatStr(
        'uzeentacdproxy: FormatEntity no supported primitives',
        [], LM_Info);

    { Отрисовываем текстовые примитивы }
    DrawTextItems(drawing, DC);
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

{ Создаёт копию прокси-объекта с сохранением всех данных }
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

  { Копируем BBox }
  ClonePtr^.FBBoxMinInOCS := FBBoxMinInOCS;
  ClonePtr^.FBBoxMaxInOCS := FBBoxMaxInOCS;
  ClonePtr^.FBBoxLoaded := FBBoxLoaded;
  ClonePtr^.FCenterPoint := FCenterPoint;
  ClonePtr^.FHasCenterPoint := FHasCenterPoint;

  Result := PGDBObjEntity(ClonePtr);
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
