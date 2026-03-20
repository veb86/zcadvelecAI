# Новая архитектура парсера Proxy Graphic для ZCAD

## Проблема текущей реализации

**Текущее состояние (uzeentacdproxy.pas + uzeentproxyparser.pas):**
1. Привязка к конкретным типам объектов (SPDSPOLYMORPHMARK)
2. Парсинг только BBox и Mesh
3. Нет поддержки командного формата (AcGiWorldDraw)
4. Нет универсального преобразования в примитивы ZCAD

**Цель:**
- Универсальный парсер для ЛЮБЫХ кастомных примитивов
- Преобразование в стандартные сущности ZCAD (круг, линия, полилиния, текст, мтекст)
- Поддержка обоих форматов: AcGiWorldDraw (AutoCAD) и СПДС GraphiCS

---

## Новая архитектура (5 модулей)

```
zcad\velec\uzeentproxy\
├── uzeentproxytypes.pas          # Типы данных (OPCODE, структуры)
├── uzeentproxyparser.pas         # Базовый парсер (ByteStream, команды)
├── uzeentproxyconverter.pas      # Конвертер в сущности ZCAD
├── uzeentproxyrenderer.pas       # Отрисовка через FormatEntity
└── uzeentacdproxy.pas            # Основной класс (обновлённый)
```

---

## Модуль 1: uzeentproxytypes.pas

```pascal
unit uzeentproxytypes;
{$Mode delphi}{$H+}
{$INCLUDE zengineconfig.inc}

interface

uses
  uzeTypes,
  uzeGeometryTypes;

type
  { OPCODE команд Proxy Graphic (AcGiWorldDraw формат) }
  TProxyGraphicCommand = (
    pgcExtents = 1,         // Границы объекта
    pgcCircle = 2,          // Круг
    pgcCircle3P = 3,        // Круг по 3 точкам
    pgcCircularArc = 4,     // Дуга
    pgcCircularArc3P = 5,   // Дуга по 3 точкам
    pgcPolyline = 6,        // Полилиния
    pgcPolygon = 7,         // Полигон
    pgcMesh = 8,            // Меш
    pgcShell = 9,           // Оболочка
    pgcText = 10,           // Текст
    pgcText2 = 11,          // Текст (расширенный)
    pgcXLine = 12,          // Конструкционная линия
    pgcRay = 13,            // Луч
    pgcAttributeColor = 14, // Установить цвет
    pgcAttributeLayer = 16, // Установить слой
    pgcAttributeLinetype = 18, // Установить тип линии
    pgcAttributeMarker = 19,   // Маркер
    pgcAttributeFill = 20,     // Заполнение
    pgcAttributeTrueColor = 22, // True цвет
    pgcAttributeLineWeight = 23, // Вес линии
    pgcAttributeLtScale = 24,    // Масштаб типа линии
    pgcAttributeThickness = 25,  // Толщина
    pgcPushMatrix = 29,     // Начать трансформацию
    pgcPopMatrix = 31,      // Закончить трансформацию
    pgcPolylineWithNormals = 32, // Полилиния с нормалями
    pgcLwPolyline = 33,     // 2D полилиния
    pgcUnicodeText = 36,    // Текст Unicode
    pgcUnicodeText2 = 38,   // Текст Unicode (расширенный)
    pgcEllipticArc = 44     // Эллиптическая дуга
  );

  { Типы примитивов СПДС }
  TSPDSPrimitiveType = (
    sptUnknown,
    sptCircle,      // Окружность
    sptLine,        // Линия
    sptPolyline,    // Полилиния
    sptArc,         // Дуга
    sptText,        // Текст
    sptMText        // Многострочный текст
  );

  { Базовая структура команды }
  TProxyCommand = record
    OpCode: TProxyGraphicCommand;
    Size: Integer;
    Data: TBytes;
  end;

  { Результат парсинга команды }
  TProxyParseResult = record
    PrimitiveType: TSPDSPrimitiveType;
    Entity: PGDBObjEntity;  // Сущность ZCAD (круг, линия, и т.д.)
    Valid: Boolean;
  end;

  { Состояние парсера (атрибуты) }
  TProxyGraphicState = record
    Color: Integer;           // BYLAYER = -1
    Layer: string;            // "0"
    Linetype: string;         // "BYLAYER"
    LineWeight: Integer;      // -2 = BYLAYER
    LtScale: Double;          // 1.0
    Thickness: Double;        // 0.0
    Fill: Boolean;            // false
    TrueColor: Integer;       // 0 = none
    MatrixStack: array of TzeMatrix4d; // Стек трансформаций
  end;

  { Структуры данных для команд }
  TProxyCircleData = record
    Center: TzePoint3d;
    Radius: Double;
    Normal: TzePoint3d;
  end;

  TProxyArcData = record
    Center: TzePoint3d;
    Radius: Double;
    Normal: TzePoint3d;
    StartAngle: Double;  // радианы
    EndAngle: Double;    // радианы
  end;

  TProxyPolylineData = record
    Vertices: array of TzePoint3d;
    Closed: Boolean;
    Bulges: array of Double;  // Для дуг
  end;

  TProxyTextData = record
    Insert: TzePoint3d;
    Text: string;
    Height: Double;
    WidthFactor: Double;
    ObliqueAngle: Double;
    Rotation: Double;  // радианы
    Normal: TzePoint3d;
    Direction: TzePoint3d;
    StyleName: string;
  end;

implementation

end.
```

---

## Модуль 2: uzeentproxyparser.pas

```pascal
unit uzeentproxyparser;
{$Mode delphi}{$H+}
{$INCLUDE zengineconfig.inc}

interface

uses
  SysUtils,
  Classes,
  uzeTypes,
  uzeGeometryTypes,
  uzeentproxytypes;

type
  { Исключение парсера }
  EProxyGraphicError = class(Exception);

  { Поток байтов для чтения (аналог ByteStream из ezdxf) }
  TProxyByteStream = class
  private
    FData: TBytes;
    FIndex: Integer;
    FLength: Integer;
  public
    constructor Create(const Data: TBytes);
    
    { Чтение примитивов }
    function ReadInt32: Integer;
    function ReadUInt32: Cardinal;
    function ReadDouble: Double;
    function ReadFloat: Single;
    function ReadVertex: TzePoint3d;  // 3 doubles
    function ReadVector: TzePoint3d;  // 3 doubles
    
    { Чтение строк }
    function ReadString(Encoding: TEncoding): string;
    function ReadPaddedString(Encoding: TEncoding): string;
    function ReadUnicodeString: string;
    
    { Чтение структур }
    function ReadStruct(const Format: string): TArray<Double>;
    
    { Проверки }
    function EndOfStream: Boolean;
    function RemainingBytes: Integer;
    
    { Свойства }
    property Index: Integer read FIndex;
    property Length: Integer read FLength;
  end;

  { Базовый класс парсера команд }
  TProxyCommandParser = class
  protected
    FStream: TProxyByteStream;
    FState: TProxyGraphicState;
    
    { Преобразование координат }
    function TransformPoint(const Point: TzePoint3d): TzePoint3d;
    function TransformToOCS(const Point: TzePoint3d; const Normal: TzePoint3d): TzePoint3d;
    
  public
    constructor Create(const Stream: TProxyByteStream; const State: TProxyGraphicState);
    destructor Destroy; override;
    
    { Парсинг команды - переопределяется в потомках }
    function Parse: TProxyParseResult; virtual; abstract;
  end;

  { Парсер круга }
  TProxyCircleParser = class(TProxyCommandParser)
  public
    function Parse: TProxyParseResult; override;
  end;

  { Парсер дуги }
  TProxyArcParser = class(TProxyCommandParser)
  public
    function Parse: TProxyParseResult; override;
  end;

  { Парсер полилинии }
  TProxyPolylineParser = class(TProxyCommandParser)
  public
    function Parse: TProxyParseResult; override;
  end;

  { Парсер полигона }
  TProxyPolygonParser = class(TProxyCommandParser)
  public
    function Parse: TProxyParseResult; override;
  end;

  { Парсер текста }
  TProxyTextParser = class(TProxyCommandParser)
  public
    function Parse: TProxyParseResult; override;
  end;

  { Главный парсер Proxy Graphic }
  TProxyGraphicParser = class
  private
    FBuffer: TBytes;
    FStream: TProxyByteStream;
    FState: TProxyGraphicState;
    FEntities: array of PGDBObjEntity;
    FEntityCount: Integer;
    
    { Парсинг заголовка }
    function ParseHeader(out CommandCount: Integer): Boolean;
    
    { Парсинг команды }
    function ParseCommand: Boolean;
    
    { Обработчики команд }
    procedure HandleCircle;
    procedure HandleArc;
    procedure HandlePolyline;
    procedure HandlePolygon;
    procedure HandleText;
    procedure HandleSetColor;
    procedure HandleSetLayer;
    procedure HandleSetLinetype;
    procedure HandlePushMatrix;
    procedure HandlePopMatrix;
    
    { Добавление сущности }
    procedure AddEntity(Entity: PGDBObjEntity);
    
  public
    constructor Create;
    destructor Destroy; override;
    
    { Инициализация из hex-строки DXF }
    function InitFromHex(const HexData: string): Boolean;
    
    { Инициализация из байтов }
    function InitFromBytes(const Data: TBytes): Boolean;
    
    { Главный метод парсинга }
    function Parse: Boolean;
    
    { Получение результатов }
    function GetEntity(Index: Integer): PGDBObjEntity;
    function GetEntityCount: Integer;
    function GetAllEntities: TArray<PGDBObjEntity>;
    
    { Очистка }
    procedure Clear;
    
    { Свойства }
    property EntityCount: Integer read GetEntityCount;
  end;

{ Вспомогательные функции }
function HexToBytes(const HexStr: string): TBytes;
function DefaultEncoding: TEncoding;

implementation

uses
  uzeentity,
  uzeentcircle,
  uzeentarc,
  uzeentpolyline,
  uzeentlwpolyline,
  uzeenttext,
  uzeentmtext,
  uzeconsts,
  Math,
  SysUtils;

{ === TProxyByteStream === }

constructor TProxyByteStream.Create(const Data: TBytes);
begin
  inherited Create;
  FData := Copy(Data, 0, Length(Data));
  FIndex := 0;
  FLength := Length(Data);
end;

function TProxyByteStream.ReadInt32: Integer;
begin
  if FIndex + 4 > FLength then
    raise EProxyGraphicError.Create('ReadInt32: End of stream');
  
  Move(FData[FIndex], Result, 4);
  Inc(FIndex, 4);
end;

function TProxyByteStream.ReadUInt32: Cardinal;
begin
  if FIndex + 4 > FLength then
    raise EProxyGraphicError.Create('ReadUInt32: End of stream');
  
  Move(FData[FIndex], Result, 4);
  Inc(FIndex, 4);
end;

function TProxyByteStream.ReadDouble: Double;
begin
  if FIndex + 8 > FLength then
    raise EProxyGraphicError.Create('ReadDouble: End of stream');
  
  Move(FData[FIndex], Result, 8);
  Inc(FIndex, 8);
end;

function TProxyByteStream.ReadFloat: Single;
begin
  if FIndex + 4 > FLength then
    raise EProxyGraphicError.Create('ReadFloat: End of stream');
  
  Move(FData[FIndex], Result, 4);
  Inc(FIndex, 4);
end;

function TProxyByteStream.ReadVertex: TzePoint3d;
begin
  Result.X := ReadDouble;
  Result.Y := ReadDouble;
  Result.Z := ReadDouble;
end;

function TProxyByteStream.ReadVector: TzePoint3d;
begin
  Result := ReadVertex;
end;

function TProxyByteStream.ReadString(Encoding: TEncoding): string;
var
  I, Len: Integer;
  Bytes: TBytes;
begin
  // Читаем до нулевого байта
  Len := 0;
  while (FIndex + Len < FLength) and (FData[FIndex + Len] <> 0) do
    Inc(Len);
  
  SetLength(Bytes, Len);
  Move(FData[FIndex], Bytes[0], Len);
  Result := Encoding.GetString(Bytes);
  
  Inc(FIndex, Len + 1); // +1 для нулевого терминатора
end;

function TProxyByteStream.ReadPaddedString(Encoding: TEncoding): string;
var
  Len, PaddedLen: Integer;
  Bytes: TBytes;
begin
  // Читаем длину + паддинг
  Len := ReadInt32;
  PaddedLen := ReadInt32;
  
  SetLength(Bytes, Len);
  Move(FData[FIndex], Bytes[0], Len);
  Result := Encoding.GetString(Bytes);
  
  Inc(FIndex, PaddedLen);
end;

function TProxyByteStream.ReadUnicodeString: string;
var
  Len: Integer;
  WBytes: TBytes;
begin
  // Читаем до нулевого слова (UTF-16)
  Len := 0;
  while (FIndex + Len * 2 + 1 < FLength) and 
        ((FData[FIndex + Len * 2] <> 0) or (FData[FIndex + Len * 2 + 1] <> 0)) do
    Inc(Len);
  
  SetLength(WBytes, Len * 2);
  Move(FData[FIndex], WBytes[0], Len * 2);
  Result := TEncoding.Unicode.GetString(WBytes);
  
  Inc(FIndex, Len * 2 + 2); // +2 для нулевого терминатора
end;

function TProxyByteStream.ReadStruct(const Format: string): TArray<Double>;
var
  I: Integer;
  Count: Integer;
begin
  Count := Length(Format);
  SetLength(Result, Count);
  
  for I := 1 to Count do
  begin
    if Format[I] = 'd' then
      Result[I-1] := ReadDouble
    else if Format[I] = 'f' then
      Result[I-1] := ReadFloat
    else if Format[I] = 'i' then
      Result[I-1] := ReadInt32;
  end;
end;

function TProxyByteStream.EndOfStream: Boolean;
begin
  Result := FIndex >= FLength;
end;

function TProxyByteStream.RemainingBytes: Integer;
begin
  Result := FLength - FIndex;
end;

{ === TProxyCommandParser === }

constructor TProxyCommandParser.Create(const Stream: TProxyByteStream; const State: TProxyGraphicState);
begin
  inherited Create;
  FStream := Stream;
  FState := State;
end;

destructor TProxyCommandParser.Destroy;
begin
  inherited Destroy;
end;

function TProxyCommandParser.TransformPoint(const Point: TzePoint3d): TzePoint3d;
var
  Matrix: TzeMatrix4d;
begin
  if Length(FState.MatrixStack) > 0 then
  begin
    Matrix := FState.MatrixStack[High(FState.MatrixStack)];
    Result := Matrix.Transform(Point);
  end
  else
    Result := Point;
end;

function TProxyCommandParser.TransformToOCS(const Point: TzePoint3d; const Normal: TzePoint3d): TzePoint3d;
var
  OCS: TzeOCS;
begin
  if not Normal.IsClose(Z_AXIS, 1e-9) then
  begin
    OCS := TzeOCS.Create(Normal);
    Result := OCS.FromWCS(Point);
  end
  else
    Result := Point;
end;

{ === TProxyCircleParser === }

function TProxyCircleParser.Parse: TProxyParseResult;
var
  CircleData: TProxyCircleData;
  Circle: PGDBObjCircle;
  Center: TzePoint3d;
begin
  Result.Valid := False;
  
  // Читаем данные круга
  CircleData.Center := FStream.ReadVertex;
  CircleData.Radius := FStream.ReadDouble;
  CircleData.Normal := FStream.ReadVector;
  
  // Преобразуем координаты
  Center := TransformToOCS(CircleData.Center, CircleData.Normal);
  
  // Создаём сущность ZCAD
  Circle := AllocAndInitCircle(nil, nil, 0);
  Circle^.Center := Center;
  Circle^.Radius := CircleData.Radius;
  Circle^.SetLayer(FState.Layer);
  Circle^.SetColor(FState.Color);
  
  Result.PrimitiveType := sptCircle;
  Result.Entity := PGDBObjEntity(Circle);
  Result.Valid := True;
end;

{ === TProxyTextParser === }

function TProxyTextParser.Parse: TProxyParseResult;
var
  TextData: TProxyTextData;
  TextEntity: PGDBObjText;
  Insert: TzePoint3d;
begin
  Result.Valid := False;
  
  // Читаем данные текста
  TextData.Insert := FStream.ReadVertex;
  TextData.Normal := FStream.ReadVector;
  TextData.Direction := FStream.ReadVector;
  TextData.Height := FStream.ReadDouble;
  TextData.WidthFactor := FStream.ReadDouble;
  TextData.ObliqueAngle := FStream.ReadDouble;
  TextData.Text := FStream.ReadString(DefaultEncoding);
  
  // Преобразуем координаты
  Insert := TransformToOCS(TextData.Insert, TextData.Normal);
  
  // Создаём сущность ZCAD
  TextEntity := AllocAndInitText(nil, nil, 0);
  TextEntity^.Insert := Insert;
  TextEntity^.Text := TextData.Text;
  TextEntity^.Height := TextData.Height;
  TextEntity^.Rotation := RadToDeg(TextData.Direction.Angle);
  TextEntity^.SetLayer(FState.Layer);
  
  Result.PrimitiveType := sptText;
  Result.Entity := PGDBObjEntity(TextEntity);
  Result.Valid := True;
end;

{ === TProxyGraphicParser === }

constructor TProxyGraphicParser.Create;
begin
  inherited Create;
  FStream := nil;
  FEntityCount := 0;
  
  // Инициализация состояния по умолчанию
  FState.Color := -1;  // BYLAYER
  FState.Layer := '0';
  FState.Linetype := 'BYLAYER';
  FState.LineWeight := -2;  // BYLAYER
  FState.LtScale := 1.0;
  FState.Thickness := 0.0;
  FState.Fill := False;
  FState.TrueColor := 0;
  SetLength(FState.MatrixStack, 0);
end;

destructor TProxyGraphicParser.Destroy;
begin
  Clear;
  FStream.Free;
  inherited Destroy;
end;

function TProxyGraphicParser.InitFromHex(const HexData: string): Boolean;
begin
  Result := InitFromBytes(HexToBytes(HexData));
end;

function TProxyGraphicParser.InitFromBytes(const Data: TBytes): Boolean;
begin
  Clear;
  FBuffer := Copy(Data, 0, Length(Data));
  FStream := TProxyByteStream.Create(FBuffer);
  Result := Length(Data) >= 8;  // Минимум для заголовка
end;

function TProxyGraphicParser.ParseHeader(out CommandCount: Integer): Boolean;
var
  ChunkSize: Integer;
begin
  if FStream = nil then
  begin
    Result := False;
    Exit;
  end;
  
  try
    // Читаем заголовок
    ChunkSize := FStream.ReadInt32;
    CommandCount := FStream.ReadInt32;
    
    Result := (ChunkSize >= 8) and (CommandCount >= 0);
  except
    Result := False;
  end;
end;

function TProxyGraphicParser.ParseCommand: Boolean;
var
  Size, OpCode: Integer;
begin
  Result := False;
  
  try
    // Читаем заголовок команды
    Size := FStream.ReadInt32;
    OpCode := FStream.ReadInt32;
    
    if Size < 8 then
      raise EProxyGraphicError.CreateFmt('Invalid command size: %d', [Size]);
    
    // Обрабатываем команду
    case TProxyGraphicCommand(OpCode) of
      pgcCircle: HandleCircle;
      pgcCircularArc: HandleArc;
      pgcPolyline: HandlePolyline;
      pgcPolygon: HandlePolygon;
      pgcText, pgcText2, pgcUnicodeText, pgcUnicodeText2: HandleText;
      pgcAttributeColor: HandleSetColor;
      pgcAttributeLayer: HandleSetLayer;
      pgcAttributeLinetype: HandleSetLinetype;
      pgcPushMatrix: HandlePushMatrix;
      pgcPopMatrix: HandlePopMatrix;
    else
      // Неизвестная команда - пропускаем
      Inc(FStream.Index, Size - 8);
    end;
    
    Result := True;
  except
    Result := False;
  end;
end;

procedure TProxyGraphicParser.HandleCircle;
var
  Parser: TProxyCircleParser;
  Result: TProxyParseResult;
begin
  Parser := TProxyCircleParser.Create(FStream, FState);
  try
    Result := Parser.Parse;
    if Result.Valid then
      AddEntity(Result.Entity);
  finally
    Parser.Free;
  end;
end;

procedure TProxyGraphicParser.HandleText;
var
  Parser: TProxyTextParser;
  Result: TProxyParseResult;
begin
  Parser := TProxyTextParser.Create(FStream, FState);
  try
    Result := Parser.Parse;
    if Result.Valid then
      AddEntity(Result.Entity);
  finally
    Parser.Free;
  end;
end;

procedure TProxyGraphicParser.HandleSetColor;
begin
  FState.Color := FStream.ReadUInt32;
  if (FState.Color < 0) or (FState.Color > 256) then
    FState.Color := -1;  // BYLAYER
end;

procedure TProxyGraphicParser.HandleSetLayer;
var
  LayerIndex: Integer;
begin
  LayerIndex := FStream.ReadUInt32;
  // TODO: Получить имя слоя из документа по индексу
  FState.Layer := Format('Layer_%d', [LayerIndex]);
end;

procedure TProxyGraphicParser.HandlePushMatrix;
var
  Matrix: TzeMatrix4d;
  I: Integer;
  Values: array[0..15] of Double;
begin
  // Читаем матрицу 4x4
  for I := 0 to 15 do
    Values[I] := FStream.ReadDouble;
  
  Matrix := TzeMatrix4d.Create(Values);
  Matrix.Transpose;  // Транспонируем для правильного порядка
  
  // Добавляем в стек
  SetLength(FState.MatrixStack, Length(FState.MatrixStack) + 1);
  FState.MatrixStack[High(FState.MatrixStack)] := Matrix;
end;

procedure TProxyGraphicParser.HandlePopMatrix;
var
  Len: Integer;
begin
  Len := Length(FState.MatrixStack);
  if Len > 0 then
    SetLength(FState.MatrixStack, Len - 1);
end;

procedure TProxyGraphicParser.AddEntity(Entity: PGDBObjEntity);
var
  Len: Integer;
begin
  Len := FEntityCount;
  SetLength(FEntities, Len + 1);
  FEntities[Len] := Entity;
  Inc(FEntityCount);
end;

function TProxyGraphicParser.Parse: Boolean;
var
  CommandCount, I: Integer;
begin
  Result := False;
  
  try
    // Читаем заголовок
    if not ParseHeader(CommandCount) then
      Exit;
    
    // Читаем команды
    for I := 0 to CommandCount - 1 do
    begin
      if not ParseCommand then
        Break;
    end;
    
    Result := FEntityCount > 0;
  except
    Result := False;
  end;
end;

function TProxyGraphicParser.GetEntity(Index: Integer): PGDBObjEntity;
begin
  if (Index >= 0) and (Index < FEntityCount) then
    Result := FEntities[Index]
  else
    Result := nil;
end;

function TProxyGraphicParser.GetEntityCount: Integer;
begin
  Result := FEntityCount;
end;

function TProxyGraphicParser.GetAllEntities: TArray<PGDBObjEntity>;
begin
  SetLength(Result, FEntityCount);
  Move(FEntities[0], Result[0], FEntityCount * SizeOf(Pointer));
end;

procedure TProxyGraphicParser.Clear;
var
  I: Integer;
begin
  // Освобождаем сущности
  for I := 0 to FEntityCount - 1 do
    FEntities[I]^.done;
  
  SetLength(FEntities, 0);
  FEntityCount := 0;
  FStream.Free;
  FStream := nil;
end;

{ === Вспомогательные функции === }

function HexToBytes(const HexStr: string): TBytes;
var
  I, Val: Integer;
begin
  SetLength(Result, Length(HexStr) div 2);
  for I := 0 to Length(Result) - 1 do
  begin
    if TryStrToInt('$' + Copy(HexStr, I * 2 + 1, 2), Val) then
      Result[I] := Lo(Val)
    else
      Result[I] := 0;
  end;
end;

function DefaultEncoding: TEncoding;
begin
  Result := TEncoding.GetEncoding(1251);  // Windows-1251 для кириллицы
end;

end.
```

---

## Модуль 3: uzeentacdproxy.pas (обновлённый)

```pascal
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
  uzeentproxyconverter;

type
  PGDBObjAcdProxy = ^GDBObjAcdProxy;

  { Прокси-объект AutoCAD (ACAD_PROXY_ENTITY).
    Новая архитектура:
    1. Загружает бинарные данные из DXF
    2. Парсит через TProxyGraphicParser
    3. Создаёт виртуальные сущности ZCAD
    4. Отрисовывает через FormatEntity }
  GDBObjAcdProxy = object(GDBObj3d)
  private
    FBBoxMinInOCS: TzePoint3d;
    FBBoxMaxInOCS: TzePoint3d;
    FBBoxLoaded: Boolean;
    
    { Виртуальные сущности }
    FVirtualEntities: array of PGDBObjEntity;
    FEntityCount: Integer;
    
    { Отрисовка виртуальных сущностей }
    procedure DrawVirtualEntities(var DC: TDrawContext);
    
    { Вычисление BBox из виртуальных сущностей }
    procedure CalcBBoxFromEntities;
    
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
    procedure ExplodeToVirtualEntities;
  end;

{ Выделяет память для нового прокси-объекта }
function AllocAcdProxy: Pointer;

{ Выделяет и инициализирует новый прокси-объект }
function AllocAndInitAcdProxy(owner: PGDBObjGenericWithSubordinated): PGDBObjAcdProxy;

implementation

{ === GDBObjAcdProxy === }

constructor GDBObjAcdProxy.init(own: Pointer; layeraddres: PGDBLayerProp; LW: smallint);
begin
  inherited init(own, layeraddres, LW);
  FBBoxMinInOCS := NulVertex;
  FBBoxMaxInOCS := NulVertex;
  FBBoxLoaded := False;
  FEntityCount := 0;
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
  for I := 0 to FEntityCount - 1 do
    FVirtualEntities[I]^.done;
  
  SetLength(FVirtualEntities, 0);
  FEntityCount := 0;
  
  inherited done;
end;

{ Загружает прокси-объект из DXF }
procedure GDBObjAcdProxy.LoadFromDXF(var rdr: TZMemReader;
  ptu: PExtensionData; var drawing: TDrawingDef;
  var context: TIODXFLoadContext);
var
  HexData: string;
  Parser: TProxyGraphicParser;
  I: Integer;
  Entities: TArray<PGDBObjEntity>;
begin
  inherited LoadFromDXF(rdr, ptu, drawing, context);
  
  programlog.LogOutFormatStr('uzeentacdproxy: LoadFromDXF START', [], LM_Info);
  
  // Читаем бинарные данные (код 310)
  while not rdr.EndOfStream do
  begin
    case rdr.ParseInteger of
      92, 160: // Размер бинарных данных
        rdr.ParseInteger; // Пропускаем
      
      310: // Бинарные данные
        begin
          HexData := rdr.ParseString;
          
          // Парсим через универсальный парсер
          Parser := TProxyGraphicParser.Create;
          try
            if Parser.InitFromHex(HexData) then
            begin
              if Parser.Parse then
              begin
                // Получаем виртуальные сущности
                Entities := Parser.GetAllEntities;
                
                // Сохраняем сущности
                FEntityCount := Length(Entities);
                SetLength(FVirtualEntities, FEntityCount);
                for I := 0 to FEntityCount - 1 do
                  FVirtualEntities[I] := Entities[I];
                
                // Вычисляем BBox
                CalcBBoxFromEntities;
                
                programlog.LogOutFormatStr(
                  'uzeentacdproxy: LoadFromDXF Parsed %d entities',
                  [FEntityCount], LM_Info);
              end;
            end;
          finally
            Parser.Free;
          end;
        end;
      
      0: // Конец объекта
        Break;
      
      else
        rdr.ParseString; // Пропускаем неизвестные коды
    end;
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
  
  // Инициализация
  FBBoxMinInOCS := FVirtualEntities[0]^.vp.BoundingBox.LBN;
  FBBoxMaxInOCS := FVirtualEntities[0]^.vp.BoundingBox.RTF;
  
  // Объединяем BBox всех сущностей
  for I := 1 to FEntityCount - 1 do
  begin
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

{ Отрисовывает виртуальные сущности }
procedure GDBObjAcdProxy.DrawVirtualEntities(var DC: TDrawContext);
var
  I: Integer;
  Entity: PGDBObjEntity;
begin
  for I := 0 to FEntityCount - 1 do
  begin
    Entity := FVirtualEntities[I];
    Entity^.FormatEntity(DC.DrawingContext.DrawingDef, DC, EFDraw);
  end;
end;

{ Рассчитывает визуальное представление }
procedure GDBObjAcdProxy.FormatEntity(var drawing: TDrawingDef;
  var DC: TDrawContext; Stage: TEFStages);
begin
  if assigned(EntExtensions) then
    EntExtensions.RunOnBeforeEntityFormat(@self, drawing, DC);
  
  // Этап расчёта геометрии
  if (Stage = EFAllStages) or (EFCalcEntityCS in Stage) then
  begin
    if FBBoxLoaded then
    begin
      vp.BoundingBox.LBN := FBBoxMinInOCS;
      vp.BoundingBox.RTF := FBBoxMaxInOCS;
    end;
    CalcActualVisible(DC.DrawingContext.VActuality);
  end;
  
  // Этап отрисовки
  if ((Stage = EFAllStages) or (EFDraw in Stage))
    and (not (ESTemp in State))
    and (DCODrawable in DC.Options)
  then
  begin
    Representation.Clear;
    
    // Рисуем виртуальные сущности
    if FEntityCount > 0 then
    begin
      programlog.LogOutFormatStr(
        'uzeentacdproxy: FormatEntity drawing %d virtual entities',
        [FEntityCount], LM_Info);
      
      DrawVirtualEntities(DC);
    end;
  end;
  
  if assigned(EntExtensions) then
    EntExtensions.RunOnAfterEntityFormat(@self, drawing, DC);
end;

{ Отрисовка напрямую }
procedure GDBObjAcdProxy.DrawGeometry(lw: integer; var DC: TDrawContext;
  const inFrustumState: TInBoundingVolume);
begin
  if FEntityCount > 0 then
  begin
    // Рисуем напрямую через drawer
    DrawVirtualEntities(DC);
  end;
end;

{ Взрывает прокси-объект }
procedure GDBObjAcdProxy.ExplodeToVirtualEntities;
var
  I: Integer;
  Layout: PGDBLayout;
begin
  Layout := GetLayout;
  if Layout = nil then
    Exit;
  
  // Добавляем виртуальные сущности в layout
  for I := 0 to FEntityCount - 1 do
  begin
    Layout.AddEntity(FVirtualEntities[I]);
    FVirtualEntities[I] := nil; // Передали владение
  end;
  
  // Удаляем прокси-объект
  Destroy;
end;

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

{ Остальные методы (Clone, TransformAt, и т.д.) - без изменений }

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
```

---

## Алгоритм работы

### 1. Загрузка из DXF

```
DXF файл
└── ACAD_PROXY_ENTITY
    ├── Код 92: 944 (размер)
    ├── Код 310: <hex-данные>
    └── Код 300/301/40: текстовые параметры
         ↓
GDBObjAcdProxy.LoadFromDXF
└── TProxyGraphicParser.InitFromHex
     ↓
TProxyGraphicParser.Parse
├── ParseHeader (ChunkSize, CommandCount)
└── ParseCommand (цикл по командам)
    ├── HandleCircle → TProxyCircleParser.Parse → PGDBObjCircle
    ├── HandleText → TProxyTextParser.Parse → PGDBObjText
    ├── HandlePolygon → TProxyPolygonParser.Parse → PGDBObjPolyline
    └── HandleSetColor → FState.Color := ...
         ↓
FVirtualEntities[] = [Circle, Text, Polyline, ...]
```

### 2. Отрисовка

```
FormatEntity(EFDraw)
└── DrawVirtualEntities
    └── Для каждой сущности:
        Entity^.FormatEntity(DC, EFDraw)
        ├── Circle → Representation.DrawCircle
        ├── Text → Representation.DrawText
        └── Polyline → Representation.DrawPolyline
```

### 3. Взрыв (Explode)

```
ExplodeToVirtualEntities
└── Для каждой сущности:
    Layout.AddEntity(FVirtualEntities[I])
└── Destroy (удалить прокси)
```

---

## Преимущества новой архитектуры

| Характеристика | Старая | Новая |
|---------------|--------|-------|
| **Подход** | Привязка к СПДС | Универсальный |
| **Формат** | Бинарная структура | AcGiWorldDraw команды |
| **Сущности** | Mesh (приближенно) | Круг, текст, полилиния (точно) |
| **Расширяемость** | Сложно | Легко (добавить парсер) |
| **Взрыв** | Нет | Да (в стандартные сущности) |
| **OCS поддержка** | Нет | Да |
| **Трансформации** | Нет | Да (матрицы) |

---

## План внедрения

### Этап 1: Базовая реализация
- [ ] Создать uzeentproxytypes.pas
- [ ] Создать uzeentproxyparser.pas (ByteStream, Circle, Text)
- [ ] Обновить uzeentacdproxy.pas

### Этап 2: Дополнительные парсеры
- [ ] TProxyArcParser (дуга)
- [ ] TProxyPolylineParser (полилиния)
- [ ] TProxyPolygonParser (полигон → Hatch)
- [ ] TProxyLwPolylineParser (2D полилиния)

### Этап 3: Тестирование
- [ ] testspds3entity.dxf (СПДС маркеры)
- [ ] proxy_entities.dxf (AutoCAD прокси)
- [ ] Взрыв прокси-объектов

### Этап 4: Оптимизация
- [ ] Кэширование виртуальных сущностей
- [ ] Пакетная отрисовка
- [ ] LOD для больших мешей

---

*Документ создан на основе анализа ezdxf и СПДС GraphiCS*
