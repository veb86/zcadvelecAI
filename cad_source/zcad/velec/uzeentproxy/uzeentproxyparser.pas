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
    procedure HandleSetColor;
    procedure HandleSetLayer;
    procedure HandleSetLinetype;
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
  Len, PaddedLen: Integer;
  WBytes: TBytes;
begin
  Len := ReadInt32;
  PaddedLen := ReadInt32;
  
  if Len > 0 then begin
    SetLength(WBytes, Len * 2);
    Move(FData[FIndex], WBytes[0], Len * 2);
    Result := TEncoding.Unicode.GetString(WBytes);
    Inc(FIndex, Len * 2);
  end else
    Result := '';
  
  // Пропускаем паддинг
  if PaddedLen > Len * 2 then
    Skip(PaddedLen - Len * 2);
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
    Result.TextData.Text := FStream.ReadPaddedUnicodeString;
    
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
  Result := InitFromBytes(HexToBytes(HexData));
end;

function TProxyGraphicParser.InitFromBytes(const Data: TBytes): Boolean;
begin
  Clear;
  FBuffer := Copy(Data, 0, Length(Data));
  FStream := TProxyByteStream.Create(FBuffer);
  FEncoding := GetEncoding;
  Result := Length(Data) >= PROXY_HEADER_SIZE;
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
    // Читаем заголовок команды
    Size := FStream.ReadInt32;
    OpCodeVal := FStream.ReadInt32;
    
    if Size < PROXY_COMMAND_HEADER_SIZE then
      raise EProxyGraphicError.CreateFmt('Invalid command size: %d', [Size]);

    // Сохраняем размер команды для использования в парсерах
    FCommandSize := Size;

    // Преобразуем в enum
    try
      OpCode := TProxyGraphicCommand(OpCodeVal);
    except
      OpCode := pgcExtents;
    end;
    
    // Обрабатываем команду
    case OpCode of
      pgcCircle: HandleCircle;
      pgcCircle3P: HandleCircle3P;
      pgcCircularArc: HandleArc;
      pgcCircularArc3P: HandleArc3P;
      pgcPolyline: HandlePolyline;
      pgcPolygon: HandlePolygon;
      pgcText: HandleText;
      pgcText2: HandleText2;
      pgcUnicodeText, pgcUnicodeText2: HandleUnicodeText;
      pgcEllipticArc: HandleEllipticArc;
      pgcAttributeColor: HandleSetColor;
      pgcAttributeLayer: HandleSetLayer;
      pgcAttributeLinetype: HandleSetLinetype;
      pgcAttributeFill: HandleSetFill;
      pgcAttributeTrueColor: HandleSetTrueColor;
      pgcAttributeLineWeight: HandleSetLineWeight;
      pgcAttributeLtScale: HandleSetLtScale;
      pgcAttributeThickness: HandleSetThickness;
      pgcPushMatrix, pgcPushMatrix2: HandlePushMatrix;
      pgcPopMatrix: HandlePopMatrix;
      pgcExtents: HandleExtents;
    else
      // Неизвестная команда - пропускаем
      FStream.Skip(Size - PROXY_COMMAND_HEADER_SIZE);
    end;
    
    Result := True;
  except
    Result := False;
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
begin
  HandleUnicodeText;
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
begin
  Result := False;
  
  try
    // Читаем заголовок
    if not ParseHeader(Header) then
      Exit;
    
    // Читаем команды
    for I := 0 to Header.CommandCount - 1 do begin
      if not ParseCommand then
        Break;
    end;
    
    Result := FResultCount > 0;
  except
    Result := False;
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
