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
  Модуль: uzeentproxyparser
  Назначение: Универсальный парсер Proxy Graphic (AcGiWorldDraw формат)
  
  На основе:
  - Анализа ezdxf (Python)
  - AutoCAD DevBlog - Proxy Graphic Binary Chunk Interpretation
  - ODA DWG Documentation
  
  Подход:
  1. Читаем бинарные данные из DXF (код 310)
  2. Парсим команды AcGiWorldDraw (OPCODE)
  3. Конвертируем в примитивы ZCAD (круг, дуга, текст, и т.д.)
}

unit uzeentproxyparser;
{$Mode delphi}{$H+}
{$Modeswitch typehelpers}
{$INCLUDE zengineconfig.inc}

interface

uses
  SysUtils,
  Classes,
  uzcLog,
  uzeTypes,
  uzeGeometryTypes,
  uzegeometry,
  uzeentproxytypes;

const
  { Базовые оси координат (локальные константы для совместимости) }
  PROXY_X_AXIS: TzePoint3d = (x: 1.0; y: 0.0; z: 0.0);
  PROXY_Y_AXIS: TzePoint3d = (x: 0.0; y: 1.0; z: 0.0);
  PROXY_Z_AXIS: TzePoint3d = (x: 0.0; y: 0.0; z: 1.0);

type
  { Исключение парсера }
  EProxyGraphicError = class(Exception);

  { OCS - Object Coordinate System (локальная реализация для парсера) }
  TzeOCS = class
  private
    FXAxis: TzePoint3d;
    FYAxis: TzePoint3d;
    FZAxis: TzePoint3d;
  public
    constructor Create(const Normal: TzePoint3d);
    destructor Destroy; override;
    function FromWCS(const Point: TzePoint3d): TzePoint3d;
  end;

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
    function ReadInt16: SmallInt;
    function ReadUInt16: Word;
    function ReadDouble: Double;
    function ReadFloat: Single;
    function ReadByte: Byte;
    function ReadBoolean: Boolean;
    
    { Чтение вершин и векторов }
    function ReadVertex: TzePoint3d;  // 3 doubles (24 байта)
    function ReadVector: TzePoint3d;  // 3 doubles (24 байта)
    function ReadPoint2D: TzePoint2d; // 2 doubles (16 байт)
    
    { Чтение строк }
    function ReadString(Encoding: TEncoding): string;
    function ReadPaddedString(Encoding: TEncoding): string;
    function ReadUnicodeString: string;
    function ReadPaddedUnicodeString: string;
    
    { Чтение структур }
    function ReadStruct(const Format: string): TArray<Double>;
    
    { Пропуск байтов }
    procedure Skip(Count: Integer);
    
    { Проверки }
    function EndOfStream: Boolean;
    function RemainingBytes: Integer;
    
    { Свойства }
    property Index: Integer read FIndex;
    property Length: Integer read FLength;
    property Data: TBytes read FData;
  end;

  { Базовый класс парсера команд }
  TProxyCommandParser = class
  protected
    FStream: TProxyByteStream;
    FState: TProxyGraphicState;
    FCommandSize: Integer;
    FEncoding: TEncoding;

    { Преобразование координат }
    function TransformPoint(const Point: TzePoint3d): TzePoint3d;
    function TransformToOCS(const Point: TzePoint3d; const Normal: TzePoint3d): TzePoint3d;

    { Вспомогательные функции }
    function NormalizeAngle(Angle: Double): Double;

  public
    constructor Create(const Stream: TProxyByteStream; const State: TProxyGraphicState; CommandSize: Integer; Encoding: TEncoding);
    destructor Destroy; override;

    { Парсинг команды - переопределяется в потомках }
    function Parse: TProxyCommandResult; virtual; abstract;
  end;

  { Парсер круга (OPCODE=2) }
  TProxyCircleParser = class(TProxyCommandParser)
  public
    function Parse: TProxyCommandResult; override;
  end;

  { Парсер круга по 3 точкам (OPCODE=3) }
  TProxyCircle3PParser = class(TProxyCommandParser)
  public
    function Parse: TProxyCommandResult; override;
  end;

  { Парсер дуги (OPCODE=4) }
  TProxyArcParser = class(TProxyCommandParser)
  public
    function Parse: TProxyCommandResult; override;
  end;

  { Парсер дуги по 3 точкам (OPCODE=5) }
  TProxyArc3PParser = class(TProxyCommandParser)
  public
    function Parse: TProxyCommandResult; override;
  end;

  { Парсер полилинии (OPCODE=6) }
  TProxyPolylineParser = class(TProxyCommandParser)
  public
    function Parse: TProxyCommandResult; override;
  end;

  { Парсер полигона (OPCODE=7) }
  TProxyPolygonParser = class(TProxyCommandParser)
  public
    function Parse: TProxyCommandResult; override;
  end;

  { Парсер текста (OPCODE=10) }
  TProxyTextParser = class(TProxyCommandParser)
  public
    function Parse: TProxyCommandResult; override;
  end;

  { Парсер текста v2 (OPCODE=11) }
  TProxyText2Parser = class(TProxyCommandParser)
  public
    function Parse: TProxyCommandResult; override;
  end;

  { Парсер Unicode текста (OPCODE=36) }
  TProxyUnicodeTextParser = class(TProxyCommandParser)
  public
    function Parse: TProxyCommandResult; override;
  end;

  { Парсер эллиптической дуги (OPCODE=44) }
  TProxyEllipticArcParser = class(TProxyCommandParser)
  public
    function Parse: TProxyCommandResult; override;
  end;

  { Главный парсер Proxy Graphic }
  TProxyGraphicParser = class
  private
    FBuffer: TBytes;
    FStream: TProxyByteStream;
    FState: TProxyGraphicState;
    FCommandSize: Integer;
    FResults: array of TProxyCommandResult;
    FResultCount: Integer;
    FEncoding: TEncoding;
    
    { Парсинг заголовка }
    function ParseHeader(out Header: TProxyGraphicHeader): Boolean;
    
    { Парсинг команды }
    function ParseCommand: Boolean;
    
    { Обработчики команд }
    procedure HandleCircle;
    procedure HandleCircle3P;
    procedure HandleArc;
    procedure HandleArc3P;
    procedure HandlePolyline;
    procedure HandlePolygon;
    procedure HandleText;
    procedure HandleText2;
    procedure HandleUnicodeText;
    procedure HandleUnicodeText2;
    procedure HandleEllipticArc;
    procedure HandleShell;
    procedure HandleSetColor;
    procedure HandleSetLayer;
    procedure HandleSetLinetype;
    procedure HandleSetMarker;
    procedure HandleSetFill;
    procedure HandleSetTrueColor;
    procedure HandleSetLineWeight;
    procedure HandleSetLtScale;
    procedure HandleSetThickness;
    procedure HandlePushMatrix;
    procedure HandlePopMatrix;
    procedure HandleExtents;
    
    { Добавление результата }
    procedure AddResult(const Result: TProxyCommandResult);
    
    { Определение кодировки }
    function GetEncoding: TEncoding;
    
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
    function GetResult(Index: Integer): TProxyCommandResult;
    function GetResultCount: Integer;
    function GetAllResults: TArray<TProxyCommandResult>;
    
    { Проверка результатов }
    function HasValidResults: Boolean;
    function HasPrimitiveType(PrimType: TProxyPrimitiveType): Boolean;
    
    { Очистка }
    procedure Clear;
    
    { Свойства }
    property ResultCount: Integer read GetResultCount;
    property State: TProxyGraphicState read FState;
  end;

{ Вспомогательные функции }
function VectorIsClose(const V1, V2: TzePoint3d; const Epsilon: Double = 1e-14): Boolean; inline;
function VectorNormalize(const V: TzePoint3d): TzePoint3d; inline;
function CrossProduct(const V1, V2: TzePoint3d): TzePoint3d; inline;
function HexToBytes(const HexStr: string): TBytes;
function BytesToHex(const Bytes: TBytes): string;
function DefaultEncoding: TEncoding;
function CP1251Encoding: TEncoding;

implementation

uses
  Math,
  TypInfo,
  uzeentity,
  uzeconsts;

{ === TzeOCS === }

constructor TzeOCS.Create(const Normal: TzePoint3d);
var
  ax: TzePoint3d;
begin
  inherited Create;
  FZAxis := NormalizeVertex(Normal);

  { Вычисляем оси X и Y используя произвольную ось }
  if Abs(FZAxis.x) < 0.9 then
    ax := PROXY_X_AXIS
  else
    ax := PROXY_Y_AXIS;

  FXAxis := NormalizeVertex(ax * FZAxis.z - FZAxis * ax.z);
  FYAxis := NormalizeVertex(FZAxis * FXAxis.x - FXAxis * FZAxis.x);
end;

destructor TzeOCS.Destroy;
begin
  inherited Destroy;
end;

function TzeOCS.FromWCS(const Point: TzePoint3d): TzePoint3d;
begin
  { Преобразование точки из WCS в OCS }
  Result.x := scalardot(Point, FXAxis);
  Result.y := scalardot(Point, FYAxis);
  Result.z := scalardot(Point, FZAxis);
end;

{ === VectorIsClose === }

function VectorIsClose(const V1, V2: TzePoint3d; const Epsilon: Double): Boolean;
begin
  Result := (Abs(V1.x - V2.x) <= Epsilon) and
            (Abs(V1.y - V2.y) <= Epsilon) and
            (Abs(V1.z - V2.z) <= Epsilon);
end;

{ === VectorNormalize === }

function VectorNormalize(const V: TzePoint3d): TzePoint3d;
var
  Len: Double;
begin
  Len := Sqrt(V.x * V.x + V.y * V.y + V.z * V.z);
  if Len > 0 then begin
    Result.x := V.x / Len;
    Result.y := V.y / Len;
    Result.z := V.z / Len;
  end else begin
    Result := V;
  end;
end;

{ === CrossProduct === }

function CrossProduct(const V1, V2: TzePoint3d): TzePoint3d;
begin
  Result.x := V1.y * V2.z - V1.z * V2.y;
  Result.y := V1.z * V2.x - V1.x * V2.z;
  Result.z := V1.x * V2.y - V1.y * V2.x;
end;

{ === Вспомогательные функции === }

function HexToBytes(const HexStr: string): TBytes;
var
  I, Val: Integer;
begin
  SetLength(Result, Length(HexStr) div 2);
  for I := 0 to Length(Result) - 1 do begin
    if TryStrToInt('$' + Copy(HexStr, I * 2 + 1, 2), Val) then
      Result[I] := Lo(Val)
    else
      Result[I] := 0;
  end;
end;

function BytesToHex(const Bytes: TBytes): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Length(Bytes) - 1 do
    Result := Result + IntToHex(Bytes[I], 2);
end;

function DefaultEncoding: TEncoding;
begin
  Result := TEncoding.GetEncoding(1251);  // Windows-1251 для кириллицы
end;

function CP1251Encoding: TEncoding;
begin
  Result := TEncoding.GetEncoding(1251);
end;

{ === TProxyByteStream === }

constructor TProxyByteStream.Create(const Data: TBytes);
begin
  inherited Create;
  FData := Copy(Data, 0, system.Length(Data));
  FIndex := 0;
  FLength := system.Length(Data);
end;

function TProxyByteStream.ReadInt32: Integer;
begin
  if FIndex + 4 > FLength then
    raise EProxyGraphicError.CreateFmt('ReadInt32: End of stream (index=%d, length=%d)', [FIndex, FLength]);
  
  Move(FData[FIndex], Result, 4);
  Inc(FIndex, 4);
end;

function TProxyByteStream.ReadUInt32: Cardinal;
begin
  if FIndex + 4 > FLength then
    raise EProxyGraphicError.CreateFmt('ReadUInt32: End of stream', []);
  
  Move(FData[FIndex], Result, 4);
  Inc(FIndex, 4);
end;

function TProxyByteStream.ReadInt16: SmallInt;
begin
  if FIndex + 2 > FLength then
    raise EProxyGraphicError.CreateFmt('ReadInt16: End of stream', []);
  
  Move(FData[FIndex], Result, 2);
  Inc(FIndex, 2);
end;

function TProxyByteStream.ReadUInt16: Word;
begin
  if FIndex + 2 > FLength then
    raise EProxyGraphicError.CreateFmt('ReadUInt16: End of stream', []);
  
  Move(FData[FIndex], Result, 2);
  Inc(FIndex, 2);
end;

function TProxyByteStream.ReadDouble: Double;
begin
  if FIndex + 8 > FLength then
    raise EProxyGraphicError.CreateFmt('ReadDouble: End of stream', []);
  
  Move(FData[FIndex], Result, 8);
  Inc(FIndex, 8);
end;

function TProxyByteStream.ReadFloat: Single;
begin
  if FIndex + 4 > FLength then
    raise EProxyGraphicError.CreateFmt('ReadFloat: End of stream', []);
  
  Move(FData[FIndex], Result, 4);
  Inc(FIndex, 4);
end;

function TProxyByteStream.ReadByte: Byte;
begin
  if FIndex + 1 > FLength then
    raise EProxyGraphicError.CreateFmt('ReadByte: End of stream', []);
  
  Result := FData[FIndex];
  Inc(FIndex);
end;

function TProxyByteStream.ReadBoolean: Boolean;
begin
  Result := ReadByte <> 0;
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

function TProxyByteStream.ReadPoint2D: TzePoint2d;
begin
  Result.X := ReadDouble;
  Result.Y := ReadDouble;
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
  
  if Len > 0 then begin
    SetLength(Bytes, Len);
    Move(FData[FIndex], Bytes[0], Len);
    Result := Encoding.GetString(Bytes);
  end else
    Result := '';
  
  Inc(FIndex, Len + 1); // +1 для нулевого терминатора
end;

function TProxyByteStream.ReadPaddedString(Encoding: TEncoding): string;
var
  Len, PaddedLen: Integer;
  Bytes: TBytes;
begin
  // Читаем длину + паддинг (как в AutoCAD)
  Len := ReadInt32;
  PaddedLen := ReadInt32;
  
  if Len > 0 then begin
    SetLength(Bytes, Len);
    Move(FData[FIndex], Bytes[0], Len);
    Result := Encoding.GetString(Bytes);
    Inc(FIndex, Len);
  end else
    Result := '';
  
  // Пропускаем паддинг
  if PaddedLen > Len then
    Skip(PaddedLen - Len);
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
  
  if Len > 0 then begin
    SetLength(WBytes, Len * 2);
    Move(FData[FIndex], WBytes[0], Len * 2);
    Result := TEncoding.Unicode.GetString(WBytes);
    Inc(FIndex, Len * 2);
  end else
    Result := '';
  
  Inc(FIndex, 2); // +2 для нулевого терминатора
end;

function TProxyByteStream.ReadPaddedUnicodeString: string;
var
  Len: Integer;
  WBytes: TBytes;
begin
  // Читаем UTF-16 строку с нулевым терминатором (как в AcGiWorldDraw)
  // Формат: последовательность WORD (2 байта) до нулевого WORD
  Len := 0;
  while (FIndex + Len * 2 + 1 < FLength) and
        ((FData[FIndex + Len * 2] <> 0) or (FData[FIndex + Len * 2 + 1] <> 0)) do
    Inc(Len);

  if Len > 0 then begin
    SetLength(WBytes, Len * 2);
    Move(FData[FIndex], WBytes[0], Len * 2);
    try
      Result := TEncoding.Unicode.GetString(WBytes);
    except
      Result := '';
    end;
    Inc(FIndex, Len * 2);
  end else
    Result := '';

  // Пропускаем нулевой терминатор (2 байта)
  if FIndex + 1 < FLength then
    Inc(FIndex, 2);
  
  // Выравниваем по 4 байтам (паддинг до границы DWORD)
  if (FIndex mod 4) <> 0 then
    Skip(4 - (FIndex mod 4));
end;

function TProxyByteStream.ReadStruct(const Format: string): TArray<Double>;
var
  I: Integer;
  Count: Integer;
begin
  Count := system.Length(Format);
  SetLength(Result, Count);
  
  for I := 1 to Count do begin
    case Format[I] of
      'd': Result[I-1] := ReadDouble;
      'f': Result[I-1] := ReadFloat;
      'i': Result[I-1] := ReadInt32;
      'w': Result[I-1] := ReadUInt16;
      'b': Result[I-1] := ReadByte;
    end;
  end;
end;

procedure TProxyByteStream.Skip(Count: Integer);
begin
  if FIndex + Count > FLength then
    Count := FLength - FIndex;
  Inc(FIndex, Count);
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

constructor TProxyCommandParser.Create(const Stream: TProxyByteStream; const State: TProxyGraphicState; CommandSize: Integer; Encoding: TEncoding);
begin
  inherited Create;
  FStream := Stream;
  FState := State;
  FCommandSize := CommandSize;
  FEncoding := Encoding;
end;

destructor TProxyCommandParser.Destroy;
begin
  inherited Destroy;
end;

function TProxyCommandParser.TransformPoint(const Point: TzePoint3d): TzePoint3d;
begin
  // TODO: Применить матрицу трансформации из стека
  // Пока просто возвращаем точку
  Result := Point;
end;

function TProxyCommandParser.TransformToOCS(const Point: TzePoint3d; const Normal: TzePoint3d): TzePoint3d;
var
  OCS: TzeOCS;
begin
  if not VectorIsClose(Normal, PROXY_Z_AXIS, 1e-9) then begin
    OCS := TzeOCS.Create(Normal);
    try
      Result := OCS.FromWCS(Point);
    finally
      OCS.Free;
    end;
  end else
    Result := Point;
end;

function TProxyCommandParser.NormalizeAngle(Angle: Double): Double;
begin
  Result := Angle;
  while Result < 0 do
    Result := Result + 2 * Pi;
  while Result >= 2 * Pi do
    Result := Result - 2 * Pi;
end;

{ === TProxyCircleParser === }

function TProxyCircleParser.Parse: TProxyCommandResult;
var
  Center: TzePoint3d;
  Radius: Double;
  Normal: TzePoint3d;
begin
  InitCommandResult(Result);
  
  try
    // Формат: Center (3d) + Radius (d) + Normal (3d)
    Center := FStream.ReadVertex;
    Radius := FStream.ReadDouble;
    Normal := FStream.ReadVector;
    
    Result.CircleData.Center := TransformToOCS(Center, Normal);
    Result.CircleData.Radius := Radius;
    Result.CircleData.Normal := Normal;
    
    Result.PrimitiveType := pptCircle;
    Result.Valid := True;
  except
    on E: Exception do begin
      Result.Valid := False;
      Result.ErrorMsg := 'Circle parse error: ' + E.Message;
    end;
  end;
end;

{ === TProxyCircle3PParser === }

function TProxyCircle3PParser.Parse: TProxyCommandResult;
begin
  InitCommandResult(Result);
  
  try
    // Формат: Point1 (3d) + Point2 (3d) + Point3 (3d)
    Result.Arc3PData.Point1 := FStream.ReadVertex;
    Result.Arc3PData.Point2 := FStream.ReadVertex;
    Result.Arc3PData.Point3 := FStream.ReadVertex;
    
    Result.PrimitiveType := pptCircle;
    Result.Valid := True;
  except
    on E: Exception do begin
      Result.Valid := False;
      Result.ErrorMsg := 'Circle3P parse error: ' + E.Message;
    end;
  end;
end;

{ === TProxyArcParser === }

function TProxyArcParser.Parse: TProxyCommandResult;
var
  Center: TzePoint3d;
  Radius: Double;
  Normal: TzePoint3d;
  StartVector: TzePoint3d;
  SweepAngle: Double;
  ArcType: Integer;
begin
  InitCommandResult(Result);
  
  try
    // Формат: Center (3d) + Radius (d) + Normal (3d) + StartVector (3d) + SweepAngle (d) + ArcType (i)
    Center := FStream.ReadVertex;
    Radius := FStream.ReadDouble;
    Normal := FStream.ReadVector;
    StartVector := FStream.ReadVector;
    SweepAngle := FStream.ReadDouble;
    ArcType := FStream.ReadInt32;
    
    Result.ArcData.Center := TransformToOCS(Center, Normal);
    Result.ArcData.Radius := Radius;
    Result.ArcData.Normal := Normal;
    Result.ArcData.StartVector := StartVector;
    Result.ArcData.SweepAngle := SweepAngle;
    Result.ArcData.ArcType := ArcType;
    
    Result.PrimitiveType := pptArc;
    Result.Valid := True;
  except
    on E: Exception do begin
      Result.Valid := False;
      Result.ErrorMsg := 'Arc parse error: ' + E.Message;
    end;
  end;
end;

{ === TProxyArc3PParser === }

function TProxyArc3PParser.Parse: TProxyCommandResult;
begin
  InitCommandResult(Result);
  
  try
    // Формат: Point1 (3d) + Point2 (3d) + Point3 (3d) + ArcType (i)
    Result.Arc3PData.Point1 := FStream.ReadVertex;
    Result.Arc3PData.Point2 := FStream.ReadVertex;
    Result.Arc3PData.Point3 := FStream.ReadVertex;
    Result.Arc3PData.ArcType := FStream.ReadInt32;
    
    Result.PrimitiveType := pptArc;
    Result.Valid := True;
  except
    on E: Exception do begin
      Result.Valid := False;
      Result.ErrorMsg := 'Arc3P parse error: ' + E.Message;
    end;
  end;
end;

{ === TProxyPolylineParser === }

function TProxyPolylineParser.Parse: TProxyCommandResult;
var
  Count: Integer;
  I: Integer;
begin
  InitCommandResult(Result);
  
  try
    // Формат: Count (i) + Vertex[] (3d × Count)
    Count := FStream.ReadInt32;
    
    if (Count <= 0) or (Count > 100000) then
      raise EProxyGraphicError.CreateFmt('Invalid polyline vertex count: %d', [Count]);
    
    SetLength(Result.PolylineData.Vertices, Count);
    Result.PolylineData.VertexCount := Count;
    
    for I := 0 to Count - 1 do
      Result.PolylineData.Vertices[I] := FStream.ReadVertex;
    
    Result.PolylineData.HasBulge := False;
    Result.PolylineData.Closed := False;
    
    Result.PrimitiveType := pptPolyline;
    Result.Valid := True;
  except
    on E: Exception do begin
      Result.Valid := False;
      Result.ErrorMsg := 'Polyline parse error: ' + E.Message;
    end;
  end;
end;

{ === TProxyPolygonParser === }

function TProxyPolygonParser.Parse: TProxyCommandResult;
var
  Count: Integer;
  I: Integer;
begin
  InitCommandResult(Result);
  
  try
    // Формат: Count (i) + Vertex[] (3d × Count)
    Count := FStream.ReadInt32;
    
    if (Count <= 0) or (Count > 100000) then
      raise EProxyGraphicError.CreateFmt('Invalid polygon vertex count: %d', [Count]);
    
    SetLength(Result.PolygonData.Vertices, Count);
    Result.PolygonData.VertexCount := Count;
    
    for I := 0 to Count - 1 do
      Result.PolygonData.Vertices[I] := FStream.ReadVertex;
    
    Result.PrimitiveType := pptPolygon;
    Result.Valid := True;
  except
    on E: Exception do begin
      Result.Valid := False;
      Result.ErrorMsg := 'Polygon parse error: ' + E.Message;
    end;
  end;
end;

{ === TProxyTextParser === }

function TProxyTextParser.Parse: TProxyCommandResult;
var
  Insert: TzePoint3d;
  Normal: TzePoint3d;
  Direction: TzePoint3d;
  Height, WidthFactor, ObliqueAngle: Double;
begin
  InitCommandResult(Result);
  
  try
    // Формат: Insert (3d) + Normal (3d) + Direction (3d) + Height (d) + WidthFactor (d) + ObliqueAngle (d) + Text (string)
    Insert := FStream.ReadVertex;
    Normal := FStream.ReadVector;
    Direction := FStream.ReadVector;
    Height := FStream.ReadDouble;
    WidthFactor := FStream.ReadDouble;
    ObliqueAngle := FStream.ReadDouble;
    Result.TextData.Text := FStream.ReadString(FEncoding);
    
    Result.TextData.Insert := TransformToOCS(Insert, Normal);
    Result.TextData.Normal := Normal;
    Result.TextData.Direction := Direction;
    Result.TextData.Height := Height;
    Result.TextData.WidthFactor := WidthFactor;
    Result.TextData.ObliqueAngle := ObliqueAngle;
    
    Result.PrimitiveType := pptText;
    Result.Valid := True;
  except
    on E: Exception do begin
      Result.Valid := False;
      Result.ErrorMsg := 'Text parse error: ' + E.Message;
    end;
  end;
end;

{ === TProxyText2Parser === }

function TProxyText2Parser.Parse: TProxyCommandResult;
var
  Insert: TzePoint3d;
  Normal: TzePoint3d;
  Direction: TzePoint3d;
  IgnoreLength, Raw: Integer;
  Height, WidthFactor, ObliqueAngle, TrackingPercentage: Double;
  IsBackward, IsUpsideDown, IsVertical, IsUnderlined, IsOverlined: Cardinal;
begin
  InitCommandResult(Result);
  
  try
    // Формат: Insert + Normal + Direction + Text + Length + Raw + TextStyle + Fonts + Flags
    Insert := FStream.ReadVertex;
    Normal := FStream.ReadVector;
    Direction := FStream.ReadVector;
    Result.TextData.Text := FStream.ReadString(FEncoding);
    IgnoreLength := FStream.ReadInt32;
    Raw := FStream.ReadInt32;
    Height := FStream.ReadDouble;
    WidthFactor := FStream.ReadDouble;
    ObliqueAngle := FStream.ReadDouble;
    TrackingPercentage := FStream.ReadDouble;
    IsBackward := FStream.ReadUInt32;
    IsUpsideDown := FStream.ReadUInt32;
    IsVertical := FStream.ReadUInt32;
    IsUnderlined := FStream.ReadUInt32;
    IsOverlined := FStream.ReadUInt32;
    Result.TextData.FontName := FStream.ReadString(FEncoding);
    Result.TextData.BigFontName := FStream.ReadString(FEncoding);
    
    Result.TextData.Insert := TransformToOCS(Insert, Normal);
    Result.TextData.Normal := Normal;
    Result.TextData.Direction := Direction;
    Result.TextData.Height := Height;
    Result.TextData.WidthFactor := WidthFactor;
    Result.TextData.ObliqueAngle := ObliqueAngle;
    Result.TextData.IsBackward := IsBackward <> 0;
    Result.TextData.IsUpsideDown := IsUpsideDown <> 0;
    
    Result.PrimitiveType := pptText;
    Result.Valid := True;
  except
    on E: Exception do begin
      Result.Valid := False;
      Result.ErrorMsg := 'Text2 parse error: ' + E.Message;
    end;
  end;
end;

{ === TProxyUnicodeTextParser === }

function TProxyUnicodeTextParser.Parse: TProxyCommandResult;
begin
  InitCommandResult(Result);

  try
    // Формат: как Text, но текст в UTF-16
    Result.TextData.Insert := FStream.ReadVertex;
    Result.TextData.Normal := FStream.ReadVector;
    Result.TextData.Direction := FStream.ReadVector;
    Result.TextData.Height := FStream.ReadDouble;
    Result.TextData.WidthFactor := FStream.ReadDouble;
    Result.TextData.ObliqueAngle := FStream.ReadDouble;
    
    // Читаем текст с обработкой ошибок
    try
      Result.TextData.Text := FStream.ReadPaddedUnicodeString;
    except
      Result.TextData.Text := '[Unicode decode error]';
    end;

    Result.PrimitiveType := pptText;
    Result.Valid := True;
  except
    on E: Exception do begin
      Result.Valid := False;
      Result.ErrorMsg := 'UnicodeText parse error: ' + E.Message;
    end;
  end;
end;

{ === TProxyEllipticArcParser === }

function TProxyEllipticArcParser.Parse: TProxyCommandResult;
begin
  InitCommandResult(Result);

  try
    // Формат: Center + Extrusion + MajorLength + MinorLength + StartParam + EndParam
    Result.EllipticArcData.Center := FStream.ReadVertex;
    Result.EllipticArcData.Extrusion := FStream.ReadVector;
    Result.EllipticArcData.MajorAxisLength := FStream.ReadDouble;
    Result.EllipticArcData.MinorAxisLength := FStream.ReadDouble;
    Result.EllipticArcData.StartParam := FStream.ReadDouble;
    Result.EllipticArcData.EndParam := FStream.ReadDouble;
    // Направление большой оси вычисляем из extrusion
    if VectorIsClose(Result.EllipticArcData.Extrusion, PROXY_Z_AXIS, 1e-9) then
      Result.EllipticArcData.MajorAxisDirection := PROXY_X_AXIS
    else
      Result.EllipticArcData.MajorAxisDirection := VectorNormalize(CrossProduct(Result.EllipticArcData.Extrusion, PROXY_Y_AXIS));

    Result.PrimitiveType := pptEllipse;
    Result.Valid := True;
  except
    on E: Exception do begin
      Result.Valid := False;
      Result.ErrorMsg := 'EllipticArc parse error: ' + E.Message;
    end;
  end;
end;

{ === TProxyGraphicParser === }

constructor TProxyGraphicParser.Create;
begin
  inherited Create;
  FStream := nil;
  FResultCount := 0;
  SetLength(FResults, 0);
  FEncoding := DefaultEncoding;
  InitProxyState(FState);
end;

destructor TProxyGraphicParser.Destroy;
begin
  Clear;
  FStream.Free;
  inherited Destroy;
end;

function TProxyGraphicParser.InitFromHex(const HexData: string): Boolean;
begin
  programlog.LogOutFormatStr('uzeentproxyparser: InitFromHex - HexData length: %d', [Length(HexData)], LM_Info);
  programlog.LogOutFormatStr('uzeentproxyparser: InitFromHex - HexData preview: %s...', [Copy(HexData, 1, 64)], LM_Info);
  Result := InitFromBytes(HexToBytes(HexData));
  if Result then
    programlog.LogOutFormatStr('uzeentproxyparser: InitFromHex - SUCCESS', [], LM_Info)
  else
    programlog.LogOutFormatStr('uzeentproxyparser: InitFromHex - FAILED', [], LM_Info);
end;

function TProxyGraphicParser.InitFromBytes(const Data: TBytes): Boolean;
begin
  programlog.LogOutFormatStr('uzeentproxyparser: InitFromBytes - Data length: %d bytes', [Length(Data)], LM_Info);
  Clear;
  FBuffer := Copy(Data, 0, Length(Data));
  FStream := TProxyByteStream.Create(FBuffer);
  FEncoding := GetEncoding;
  Result := Length(Data) >= PROXY_HEADER_SIZE;
  programlog.LogOutFormatStr('uzeentproxyparser: InitFromBytes - Result: %s', [BoolToStr(Result, True)], LM_Info);
end;

function TProxyGraphicParser.ParseHeader(out Header: TProxyGraphicHeader): Boolean;
begin
  Result := False;
  
  try
    Header.ChunkSize := FStream.ReadInt32;
    Header.CommandCount := FStream.ReadInt32;
    
    Result := (Header.ChunkSize >= PROXY_HEADER_SIZE) and 
              (Header.CommandCount >= 0) and
              (Header.CommandCount < 100000);
  except
    Result := False;
  end;
end;

function TProxyGraphicParser.ParseCommand: Boolean;
var
  Size, OpCodeVal: Integer;
  OpCode: TProxyGraphicCommand;
begin
  Result := False;

  try
    // Проверка на конец потока
    if FStream.EndOfStream then begin
      programlog.LogOutFormatStr('uzeentproxyparser: ParseCommand - End of stream', [], LM_Info);
      Exit;
    end;
    
    // Проверка: осталось ли достаточно байт для заголовка команды
    if FStream.RemainingBytes < PROXY_COMMAND_HEADER_SIZE then begin
      programlog.LogOutFormatStr('uzeentproxyparser: ParseCommand - Not enough bytes for header (remaining=%d)', [FStream.RemainingBytes], LM_Info);
      Exit;
    end;

    // Читаем заголовок команды
    Size := FStream.ReadInt32;
    OpCodeVal := FStream.ReadInt32;

    programlog.LogOutFormatStr('uzeentproxyparser: ParseCommand - Size=%d, OpCode=%d', [Size, OpCodeVal], LM_Info);

    if Size < PROXY_COMMAND_HEADER_SIZE then begin
      programlog.LogOutFormatStr('uzeentproxyparser: ParseCommand - Invalid command size %d, skipping', [Size], LM_Info);
      Exit;
    end;
    
    // Проверка: не выходит ли размер за пределы потока
    if Size > FStream.RemainingBytes + PROXY_COMMAND_HEADER_SIZE then begin
      programlog.LogOutFormatStr('uzeentproxyparser: ParseCommand - Command size %d exceeds stream, truncating', [Size], LM_Info);
      Size := FStream.RemainingBytes + PROXY_COMMAND_HEADER_SIZE;
    end;

    // Сохраняем размер команды для использования в парсерах
    FCommandSize := Size;

    // Преобразуем в enum
    try
      OpCode := TProxyGraphicCommand(OpCodeVal);
      programlog.LogOutFormatStr('uzeentproxyparser: ParseCommand - OpCode enum: %s', [GetEnumName(TypeInfo(TProxyGraphicCommand), Ord(OpCode))], LM_Info);
    except
      programlog.LogOutFormatStr('uzeentproxyparser: ParseCommand - Invalid OpCode %d, skipping', [OpCodeVal], LM_Info);
      // Пропускаем команду с неизвестным OpCode
      FStream.Skip(Size - PROXY_COMMAND_HEADER_SIZE);
      Result := True;
      Exit;
    end;

    // Обрабатываем команду
    programlog.LogOutFormatStr('uzeentproxyparser: ParseCommand - Handling command...', [], LM_Info);
    case OpCode of
      pgcCircle:
        begin
          programlog.LogOutFormatStr('uzeentproxyparser: ParseCommand - CIRCLE', [], LM_Info);
          HandleCircle;
        end;
      pgcCircle3P:
        begin
          programlog.LogOutFormatStr('uzeentproxyparser: ParseCommand - CIRCLE3P', [], LM_Info);
          HandleCircle3P;
        end;
      pgcCircularArc:
        begin
          programlog.LogOutFormatStr('uzeentproxyparser: ParseCommand - ARC', [], LM_Info);
          HandleArc;
        end;
      pgcCircularArc3P:
        begin
          programlog.LogOutFormatStr('uzeentproxyparser: ParseCommand - ARC3P', [], LM_Info);
          HandleArc3P;
        end;
      pgcPolyline:
        begin
          programlog.LogOutFormatStr('uzeentproxyparser: ParseCommand - POLYLINE', [], LM_Info);
          HandlePolyline;
        end;
      pgcPolygon:
        begin
          programlog.LogOutFormatStr('uzeentproxyparser: ParseCommand - POLYGON', [], LM_Info);
          HandlePolygon;
        end;
      pgcText:
        begin
          programlog.LogOutFormatStr('uzeentproxyparser: ParseCommand - TEXT', [], LM_Info);
          HandleText;
        end;
      pgcText2:
        begin
          programlog.LogOutFormatStr('uzeentproxyparser: ParseCommand - TEXT2', [], LM_Info);
          HandleText2;
        end;
      pgcUnicodeText:
        begin
          programlog.LogOutFormatStr('uzeentproxyparser: ParseCommand - UNICODETEXT', [], LM_Info);
          HandleUnicodeText;
        end;
      pgcUnicodeText2:
        begin
          programlog.LogOutFormatStr('uzeentproxyparser: ParseCommand - UNICODETEXT2', [], LM_Info);
          HandleUnicodeText2;
        end;
      pgcEllipticArc:
        begin
          programlog.LogOutFormatStr('uzeentproxyparser: ParseCommand - ELLIPTICARC', [], LM_Info);
          HandleEllipticArc;
        end;
      pgcShell:
        begin
          programlog.LogOutFormatStr('uzeentproxyparser: ParseCommand - SHELL (skipping)', [], LM_Info);
          HandleShell;
        end;
      pgcAttributeColor: HandleSetColor;
      pgcAttributeLayer: HandleSetLayer;
      pgcAttributeLinetype: HandleSetLinetype;
      pgcAttributeMarker: HandleSetMarker;
      pgcAttributeFill: HandleSetFill;
      pgcAttributeTrueColor: HandleSetTrueColor;
      pgcAttributeLineWeight: HandleSetLineWeight;
      pgcAttributeLtScale: HandleSetLtScale;
      pgcAttributeThickness: HandleSetThickness;
      pgcPushMatrix, pgcPushMatrix2: HandlePushMatrix;
      pgcPopMatrix: HandlePopMatrix;
      pgcExtents: HandleExtents;
    else
      begin
        programlog.LogOutFormatStr('uzeentproxyparser: ParseCommand - UNKNOWN OpCode %d, skipping %d bytes', [OpCodeVal, Size - PROXY_COMMAND_HEADER_SIZE], LM_Info);
        // Неизвестная команда - пропускаем
        FStream.Skip(Size - PROXY_COMMAND_HEADER_SIZE);
      end;
    end;

    Result := True;
  except
    on E: Exception do begin
      programlog.LogOutFormatStr('uzeentproxyparser: ParseCommand - EXCEPTION: %s', [E.Message], LM_Info);
      Result := False;
    end;
  end;
end;

procedure TProxyGraphicParser.HandleCircle;
var
  Parser: TProxyCircleParser;
  CmdResult: TProxyCommandResult;
begin
  Parser := TProxyCircleParser.Create(FStream, FState, FCommandSize, FEncoding);
  try
    CmdResult := Parser.Parse;
    AddResult(CmdResult);
  finally
    Parser.Free;
  end;
end;

procedure TProxyGraphicParser.HandleCircle3P;
var
  Parser: TProxyCircle3PParser;
begin
  Parser := TProxyCircle3PParser.Create(FStream, FState, FCommandSize, FEncoding);
  try
    AddResult(Parser.Parse);
  finally
    Parser.Free;
  end;
end;

procedure TProxyGraphicParser.HandleArc;
var
  Parser: TProxyArcParser;
begin
  Parser := TProxyArcParser.Create(FStream, FState, FCommandSize, FEncoding);
  try
    AddResult(Parser.Parse);
  finally
    Parser.Free;
  end;
end;

procedure TProxyGraphicParser.HandleArc3P;
var
  Parser: TProxyArc3PParser;
begin
  Parser := TProxyArc3PParser.Create(FStream, FState, FCommandSize, FEncoding);
  try
    AddResult(Parser.Parse);
  finally
    Parser.Free;
  end;
end;

procedure TProxyGraphicParser.HandlePolyline;
var
  Parser: TProxyPolylineParser;
begin
  Parser := TProxyPolylineParser.Create(FStream, FState, FCommandSize, FEncoding);
  try
    AddResult(Parser.Parse);
  finally
    Parser.Free;
  end;
end;

procedure TProxyGraphicParser.HandlePolygon;
var
  Parser: TProxyPolygonParser;
begin
  Parser := TProxyPolygonParser.Create(FStream, FState, FCommandSize, FEncoding);
  try
    AddResult(Parser.Parse);
  finally
    Parser.Free;
  end;
end;

procedure TProxyGraphicParser.HandleText;
var
  Parser: TProxyTextParser;
begin
  Parser := TProxyTextParser.Create(FStream, FState, FCommandSize, FEncoding);
  try
    AddResult(Parser.Parse);
  finally
    Parser.Free;
  end;
end;

procedure TProxyGraphicParser.HandleText2;
var
  Parser: TProxyText2Parser;
begin
  Parser := TProxyText2Parser.Create(FStream, FState, FCommandSize, FEncoding);
  try
    AddResult(Parser.Parse);
  finally
    Parser.Free;
  end;
end;

procedure TProxyGraphicParser.HandleUnicodeText;
var
  Parser: TProxyUnicodeTextParser;
begin
  Parser := TProxyUnicodeTextParser.Create(FStream, FState, FCommandSize, FEncoding);
  try
    AddResult(Parser.Parse);
  finally
    Parser.Free;
  end;
end;

procedure TProxyGraphicParser.HandleUnicodeText2;
var
  Parser: TProxyUnicodeTextParser;
  StartPos, EndPos: Integer;
begin
  // UnicodeText2 имеет расширенный формат - пока просто пропускаем команду
  // TODO: реализовать полноценный парсер UnicodeText2
  StartPos := FStream.Index;
  // Пропускаем все данные команды
  FStream.Skip(FCommandSize - PROXY_COMMAND_HEADER_SIZE);
  EndPos := FStream.Index;
  programlog.LogOutFormatStr('uzeentproxyparser: HandleUnicodeText2 - SKIPPED %d bytes', [EndPos - StartPos], LM_Info);
end;

procedure TProxyGraphicParser.HandleEllipticArc;
var
  Parser: TProxyEllipticArcParser;
begin
  Parser := TProxyEllipticArcParser.Create(FStream, FState, FCommandSize, FEncoding);
  try
    AddResult(Parser.Parse);
  finally
    Parser.Free;
  end;
end;

procedure TProxyGraphicParser.HandleShell;
var
  StartPos, EndPos: Integer;
begin
  // Shell имеет сложную структуру - пропускаем команду
  // TODO: реализовать полноценный парсер Shell
  StartPos := FStream.Index;
  FStream.Skip(FCommandSize - PROXY_COMMAND_HEADER_SIZE);
  EndPos := FStream.Index;
  programlog.LogOutFormatStr('uzeentproxyparser: HandleShell - SKIPPED %d bytes', [EndPos - StartPos], LM_Info);
end;

procedure TProxyGraphicParser.HandleSetColor;
begin
  FState.Color := FStream.ReadUInt32;
  if (FState.Color < 0) or (FState.Color > 256) then
    FState.Color := PROXY_DEFAULT_COLOR;
end;

procedure TProxyGraphicParser.HandleSetLayer;
var
  LayerIndex: Cardinal;
begin
  LayerIndex := FStream.ReadUInt32;
  // TODO: Получить имя слоя из документа по индексу
  FState.Layer := Format('Layer_%d', [LayerIndex]);
end;

procedure TProxyGraphicParser.HandleSetLinetype;
var
  LTIndex: Cardinal;
begin
  LTIndex := FStream.ReadUInt32;
  // TODO: Получить имя типа линии из документа по индексу
  if LTIndex = 32766 then
    FState.Linetype := 'BYBLOCK'
  else if LTIndex = 32767 then
    FState.Linetype := 'BYLAYER'
  else
    FState.Linetype := Format('Linetype_%d', [LTIndex]);
end;

procedure TProxyGraphicParser.HandleSetMarker;
begin
  // Пропускаем Marker ID (используется для выделения объектов)
  FStream.ReadUInt32;
end;

procedure TProxyGraphicParser.HandleSetFill;
begin
  FState.Fill := FStream.ReadUInt32 <> 0;
end;

procedure TProxyGraphicParser.HandleSetTrueColor;
begin
  FState.TrueColor := FStream.ReadUInt32;
end;

procedure TProxyGraphicParser.HandleSetLineWeight;
begin
  FState.LineWeight := FStream.ReadUInt32;
end;

procedure TProxyGraphicParser.HandleSetLtScale;
begin
  FState.LtScale := FStream.ReadDouble;
end;

procedure TProxyGraphicParser.HandleSetThickness;
begin
  FState.Thickness := FStream.ReadDouble;
end;

procedure TProxyGraphicParser.HandlePushMatrix;
var
  I: Integer;
  Values: array[0..15] of Double;
begin
  // Читаем матрицу 4×4 (16 doubles)
  for I := 0 to 15 do
    Values[I] := FStream.ReadDouble;
  
  // TODO: Сохранить матрицу в стек
  Inc(FState.MatrixCount);
end;

procedure TProxyGraphicParser.HandlePopMatrix;
begin
  if FState.MatrixCount > 0 then
    Dec(FState.MatrixCount);
end;

procedure TProxyGraphicParser.HandleExtents;
begin
  // Читаем BBox (Min + Max)
  FStream.Skip(24); // Min point (3 doubles)
  FStream.Skip(24); // Max point (3 doubles)
end;

procedure TProxyGraphicParser.AddResult(const Result: TProxyCommandResult);
var
  Len: Integer;
begin
  Len := FResultCount;
  SetLength(FResults, Len + 1);
  FResults[Len] := Result;
  Inc(FResultCount);
end;

function TProxyGraphicParser.GetEncoding: TEncoding;
begin
  Result := DefaultEncoding;
end;

function TProxyGraphicParser.Parse: Boolean;
var
  Header: TProxyGraphicHeader;
  I: Integer;
  BytesRemaining: Integer;
begin
  Result := False;

  programlog.LogOutFormatStr('uzeentproxyparser: Parse - START', [], LM_Info);

  try
    // Проверка: есть ли данные для парсинга
    if FStream = nil then begin
      programlog.LogOutFormatStr('uzeentproxyparser: Parse - FStream is nil', [], LM_Info);
      Exit;
    end;
    
    BytesRemaining := FStream.RemainingBytes;
    programlog.LogOutFormatStr('uzeentproxyparser: Parse - Stream bytes remaining: %d', [BytesRemaining], LM_Info);

    // Читаем заголовок
    programlog.LogOutFormatStr('uzeentproxyparser: Parse - Reading header...', [], LM_Info);
    if not ParseHeader(Header) then begin
      programlog.LogOutFormatStr('uzeentproxyparser: Parse - ParseHeader failed', [], LM_Info);
      Exit;
    end;

    programlog.LogOutFormatStr('uzeentproxyparser: Parse - Header parsed, CommandCount=%d', [Header.CommandCount], LM_Info);

    // Читаем команды
    for I := 0 to Header.CommandCount - 1 do begin
      programlog.LogOutFormatStr('uzeentproxyparser: Parse - Processing command %d of %d (bytes remaining: %d)...', [I + 1, Header.CommandCount, FStream.RemainingBytes], LM_Info);

      if not ParseCommand then begin
        programlog.LogOutFormatStr('uzeentproxyparser: Parse - ParseCommand failed at command %d', [I + 1], LM_Info);
        Break;
      end;

      programlog.LogOutFormatStr('uzeentproxyparser: Parse - Command %d processed, results so far: %d', [I + 1, FResultCount], LM_Info);
    end;

    Result := FResultCount > 0;

    if Result then
      programlog.LogOutFormatStr('uzeentproxyparser: Parse - SUCCESS, Results=%d', [FResultCount], LM_Info)
    else
      programlog.LogOutFormatStr('uzeentproxyparser: Parse - No results parsed', [], LM_Info);

  except
    on E: Exception do begin
      programlog.LogOutFormatStr('uzeentproxyparser: Parse - EXCEPTION: %s', [E.Message], LM_Info);
      //programlog.LogOutFormatStr('uzeentproxyparser: Parse - EXCEPTION StackTrace: %s', [GetExceptionBackTrace(E, True)], LM_Info);
      Result := False;
    end;
  end;
end;

function TProxyGraphicParser.GetResult(Index: Integer): TProxyCommandResult;
begin
  if (Index >= 0) and (Index < FResultCount) then
    Result := FResults[Index]
  else
    InitCommandResult(Result);
end;

function TProxyGraphicParser.GetResultCount: Integer;
begin
  Result := FResultCount;
end;

function TProxyGraphicParser.GetAllResults: TArray<TProxyCommandResult>;
begin
  SetLength(Result, FResultCount);
  if FResultCount > 0 then
    Move(FResults[0], Result[0], FResultCount * SizeOf(TProxyCommandResult));
end;

function TProxyGraphicParser.HasValidResults: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to FResultCount - 1 do begin
    if FResults[I].Valid then begin
      Result := True;
      Exit;
    end;
  end;
end;

function TProxyGraphicParser.HasPrimitiveType(PrimType: TProxyPrimitiveType): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to FResultCount - 1 do begin
    if FResults[I].Valid and (FResults[I].PrimitiveType = PrimType) then begin
      Result := True;
      Exit;
    end;
  end;
end;

procedure TProxyGraphicParser.Clear;
var
  I: Integer;
begin
  SetLength(FResults, 0);
  FResultCount := 0;
  FStream.Free;
  FStream := nil;
  InitProxyState(FState);
end;

end.
