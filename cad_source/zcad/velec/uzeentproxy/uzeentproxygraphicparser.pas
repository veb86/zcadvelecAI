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
  Модуль: uzeentproxygraphicparser
  Назначение: Полный парсер Proxy Graphic формата AcGiWorldDraw
  
  Структура данных:
  - Заголовок: ChunkSize (4b) + CommandCount (4b)
  - Команды: CommandSize (4b) + OpCode (4b) + Данные
  
  OPCODE:
  - 1 = SetExtents (BBox)
  - 2 = Circle (Center + Radius + Normal)
  - 4 = CircularArc
  - 6 = Polyline
  - 14 = SetColor
  - 16 = SetLayer
  - 29 = PushTransform
  - 31 = PopTransform
  - 44 = EllipticArc
}

unit uzeentproxygraphicparser;
{$Mode delphi}{$H+}
{$INCLUDE zengineconfig.inc}

interface

uses
  SysUtils,
  uzeentproxystream,
  uzeentproxytypes,
  uzeentity,
  uzedrawingdef,
  uzegeometrytypes,
  UGDBPoint3DArray,
  uzgldrawcontext,
  uzeTypes,
  gzctnrVectorTypes,
  gzctnrVector;

type
  { Результат парсинга Proxy Graphic }
  TProxyGraphicParseResult = record
    CircleCount: Integer;
    ArcCount: Integer;
    EntityCount: Integer;
    BBoxMin: TzePoint3d;
    BBoxMax: TzePoint3d;
    BBoxLoaded: Boolean;
    CenterPoint: TzePoint3d;  // Центр первого круга для контрольной точки
    CircleVertices: GDBPoint3DArray;  // Вершины круга для отрисовки
    HasCircleVertices: Boolean;
  end;

  { Парсер Proxy Graphic }
  TProxyGraphicParser = class
  private
    FStream: TProxyByteStream;
    FChunkSize: Integer;
    FCommandCount: Integer;
    FDrawing: TDrawingDef;
    FDC: TDrawContext;
    FStage: TEFStages;
    FResult: TProxyGraphicParseResult;
    
    { Парсинг заголовка }
    function ParseHeader: Boolean;
    
    { Парсинг команды }
    function ParseCommand: Boolean;
    
    { Обработчики команд }
    procedure HandleCircle(CommandSize: Integer);
    procedure HandleArc(CommandSize: Integer);
    procedure HandlePolyline(CommandSize: Integer);
    procedure HandleSetColor(CommandSize: Integer);
    procedure HandleSetLayer(CommandSize: Integer);
    procedure HandlePushMatrix(CommandSize: Integer);
    procedure HandlePopMatrix(CommandSize: Integer);
    procedure HandleExtents(CommandSize: Integer);
    procedure SkipCommand(CommandSize: Integer);
    
  public
    constructor Create(const Data: TBytes; var Drawing: TDrawingDef; var DC: TDrawContext; Stage: TEFStages);
    destructor Destroy; override;
    
    { Главный метод парсинга }
    function Parse: TProxyGraphicParseResult;
  end;

implementation

uses
  uzcLog,
  uzeentcircle,
  uzeentarc,
  uzeentpolyline,
  uzegeometry,
  Math;

const
  PROXY_X_AXIS: TzePoint3d = (x: 1.0; y: 0.0; z: 0.0);
  PROXY_Y_AXIS: TzePoint3d = (x: 0.0; y: 1.0; z: 0.0);
  PROXY_Z_AXIS: TzePoint3d = (x: 0.0; y: 0.0; z: 1.0);

{ === TProxyGraphicParser === }

constructor TProxyGraphicParser.Create(const Data: TBytes; var Drawing: TDrawingDef; var DC: TDrawContext; Stage: TEFStages);
begin
  inherited Create;
  FStream := TProxyByteStream.Create(Data);
  FDrawing := Drawing;
  FDC := DC;
  FStage := Stage;
  FillChar(FResult, SizeOf(FResult), 0);
  FResult.BBoxLoaded := False;
end;

destructor TProxyGraphicParser.Destroy;
begin
  FStream.Free;
  inherited Destroy;
end;

function TProxyGraphicParser.ParseHeader: Boolean;
begin
  Result := False;
  
  try
    FChunkSize := FStream.ReadInt32;
    FCommandCount := FStream.ReadInt32;
    
    programlog.LogOutFormatStr('uzeentproxygraphicparser: Header - ChunkSize=%d, CommandCount=%d', 
      [FChunkSize, FCommandCount], LM_Info);
    
    Result := (FChunkSize > 0) and (FCommandCount > 0) and (FCommandCount < 10000);
  except
    on E: Exception do
    begin
      programlog.LogOutFormatStr('uzeentproxygraphicparser: ParseHeader error: %s', [E.Message], LM_Error);
      Result := False;
    end;
  end;
end;

function TProxyGraphicParser.ParseCommand: Boolean;
var
  CommandSize: Integer;
  OpCode: Integer;
begin
  Result := False;
  
  try
    CommandSize := FStream.ReadInt32;
    OpCode := FStream.ReadInt32;
    
    programlog.LogOutFormatStr('uzeentproxygraphicparser: Command - Size=%d, OpCode=%d', 
      [CommandSize, OpCode], LM_Info);
    
    if CommandSize < 8 then
    begin
      programlog.LogOutFormatStr('uzeentproxygraphicparser: Invalid CommandSize %d, skipping', [CommandSize], LM_Warning);
      Result := True;
      Exit;
    end;
    
    case OpCode of
      2: HandleCircle(CommandSize);       // Круг
      4: HandleArc(CommandSize);          // Дуга
      6: HandlePolyline(CommandSize);     // Полилиния
      14: HandleSetColor(CommandSize);    // Цвет
      16: HandleSetLayer(CommandSize);    // Слой
      29, 30: HandlePushMatrix(CommandSize); // PushTransform
      31: HandlePopMatrix(CommandSize);   // PopTransform
      1: HandleExtents(CommandSize);      // Extents
    else
      SkipCommand(CommandSize);
    end;
    
    Result := True;
  except
    on E: Exception do
    begin
      programlog.LogOutFormatStr('uzeentproxygraphicparser: ParseCommand error: %s', [E.Message], LM_Error);
      Result := False;
    end;
  end;
end;

procedure TProxyGraphicParser.HandleCircle(CommandSize: Integer);
var
  Center: TzePoint3d;
  Radius: Double;
  Normal: TzePoint3d;
  I, SegCount: Integer;
  Angle: Double;
  Pt: TzePoint3d;
  VertexArray: GDBPoint3DArray;
  ir: itrec;
  pV: PzePoint3d;
begin
  try
    // Формат: Center (3d) + Radius (d) + Normal (3d)
    Center := FStream.ReadVertex;
    Radius := FStream.ReadDouble;
    Normal := FStream.ReadVector;
    
    programlog.LogOutFormatStr('uzeentproxygraphicparser: Circle - Center=(%.3f,%.3f,%.3f) Radius=%.3f', 
      [Center.x, Center.y, Center.z, Radius], LM_Info);
    
    if Radius > 0 then
    begin
      // Тесселируем круг в вершины
      SegCount := 64;
      VertexArray.init(SegCount);
      
      for I := 0 to SegCount - 1 do
      begin
        Angle := (I / SegCount) * 2 * Pi;
        Pt := CreateVertex(
          Center.x + Radius * Cos(Angle),
          Center.y + Radius * Sin(Angle),
          Center.z
        );
        VertexArray.PushBackData(Pt);
      end;
      
      // Сохраняем вершины в результат для последующей отрисовки
      if not FResult.HasCircleVertices then
      begin
        FResult.CircleVertices.init(SegCount);
        // Копируем вершины из VertexArray в FResult.CircleVertices через итератор
        pV := VertexArray.beginiterate(ir);
        while pV <> nil do
        begin
          FResult.CircleVertices.PushBackData(pV^);
          pV := VertexArray.iterate(ir);
        end;
        FResult.HasCircleVertices := True;
      end;
      
      // Обновляем BBox
      if not FResult.BBoxLoaded then
      begin
        FResult.BBoxMin := CreateVertex(Center.x - Radius, Center.y - Radius, Center.z);
        FResult.BBoxMax := CreateVertex(Center.x + Radius, Center.y + Radius, Center.z);
        FResult.BBoxLoaded := True;
        FResult.CenterPoint := Center;  // Сохраняем центр первого круга
      end
      else
      begin
        // Расширяем BBox
        if Center.x - Radius < FResult.BBoxMin.x then FResult.BBoxMin.x := Center.x - Radius;
        if Center.y - Radius < FResult.BBoxMin.y then FResult.BBoxMin.y := Center.y - Radius;
        if Center.z - Radius < FResult.BBoxMin.z then FResult.BBoxMin.z := Center.z - Radius;
        if Center.x + Radius > FResult.BBoxMax.x then FResult.BBoxMax.x := Center.x + Radius;
        if Center.y + Radius > FResult.BBoxMax.y then FResult.BBoxMax.y := Center.y + Radius;
        if Center.z + Radius > FResult.BBoxMax.z then FResult.BBoxMax.z := Center.z + Radius;
      end;
      
      VertexArray.done;
      
      Inc(FResult.CircleCount);
      programlog.LogOutFormatStr('uzeentproxygraphicparser: Circle drawn successfully', [], LM_Info);
    end
    else
    begin
      programlog.LogOutFormatStr('uzeentproxygraphicparser: Circle has zero radius, skipping', [], LM_Warning);
    end;
  except
    on E: Exception do
    begin
      programlog.LogOutFormatStr('uzeentproxygraphicparser: HandleCircle error: %s', [E.Message], LM_Error);
      SkipCommand(CommandSize);
    end;
  end;
end;

procedure TProxyGraphicParser.HandleArc(CommandSize: Integer);
begin
  // TODO: Реализовать парсинг дуги
  programlog.LogOutFormatStr('uzeentproxygraphicparser: Arc command not implemented yet', [], LM_Warning);
  SkipCommand(CommandSize);
end;

procedure TProxyGraphicParser.HandlePolyline(CommandSize: Integer);
begin
  // TODO: Реализовать парсинг полилинии
  programlog.LogOutFormatStr('uzeentproxygraphicparser: Polyline command not implemented yet', [], LM_Warning);
  SkipCommand(CommandSize);
end;

procedure TProxyGraphicParser.HandleSetColor(CommandSize: Integer);
var
  Color: Integer;
begin
  try
    Color := FStream.ReadInt32;
    programlog.LogOutFormatStr('uzeentproxygraphicparser: SetColor - Color=%d', [Color], LM_Info);
  except
    on E: Exception do
    begin
      programlog.LogOutFormatStr('uzeentproxygraphicparser: HandleSetColor error: %s', [E.Message], LM_Error);
      SkipCommand(CommandSize);
    end;
  end;
end;

procedure TProxyGraphicParser.HandleSetLayer(CommandSize: Integer);
var
  LayerIndex: Integer;
begin
  try
    LayerIndex := FStream.ReadInt32;
    programlog.LogOutFormatStr('uzeentproxygraphicparser: SetLayer - LayerIndex=%d', [LayerIndex], LM_Info);
  except
    on E: Exception do
    begin
      programlog.LogOutFormatStr('uzeentproxygraphicparser: HandleSetLayer error: %s', [E.Message], LM_Error);
      SkipCommand(CommandSize);
    end;
  end;
end;

procedure TProxyGraphicParser.HandlePushMatrix(CommandSize: Integer);
var
  I: Integer;
  Matrix: array[0..15] of Double;
begin
  try
    for I := 0 to 15 do
      Matrix[I] := FStream.ReadDouble;
    
    programlog.LogOutFormatStr('uzeentproxygraphicparser: PushMatrix - matrix loaded (16 doubles)', [], LM_Info);
  except
    on E: Exception do
    begin
      programlog.LogOutFormatStr('uzeentproxygraphicparser: HandlePushMatrix error: %s', [E.Message], LM_Error);
      SkipCommand(CommandSize);
    end;
  end;
end;

procedure TProxyGraphicParser.HandlePopMatrix(CommandSize: Integer);
begin
  programlog.LogOutFormatStr('uzeentproxygraphicparser: PopMatrix', [], LM_Info);
end;

procedure TProxyGraphicParser.HandleExtents(CommandSize: Integer);
var
  MinPt, MaxPt: TzePoint3d;
begin
  try
    MinPt := FStream.ReadVertex;
    MaxPt := FStream.ReadVertex;
    
    programlog.LogOutFormatStr('uzeentproxygraphicparser: Extents - Min=(%.3f,%.3f,%.3f) Max=(%.3f,%.3f,%.3f)', 
      [MinPt.x, MinPt.y, MinPt.z, MaxPt.x, MaxPt.y, MaxPt.z], LM_Info);
  except
    on E: Exception do
    begin
      programlog.LogOutFormatStr('uzeentproxygraphicparser: HandleExtents error: %s', [E.Message], LM_Error);
      SkipCommand(CommandSize);
    end;
  end;
end;

procedure TProxyGraphicParser.SkipCommand(CommandSize: Integer);
var
  BytesToSkip: Integer;
begin
  BytesToSkip := CommandSize - 8; // Вычитаем размер заголовка команды (Size + OpCode)
  if BytesToSkip > 0 then
  begin
    programlog.LogOutFormatStr('uzeentproxygraphicparser: Skipping %d bytes', [BytesToSkip], LM_Info);
    FStream.Skip(BytesToSkip);
  end;
end;

function TProxyGraphicParser.Parse: TProxyGraphicParseResult;
var
  I: Integer;
begin
  FillChar(Result, SizeOf(Result), 0);
  
  programlog.LogOutFormatStr('uzeentproxygraphicparser: Parse START', [], LM_Info);
  
  try
    // Парсим заголовок
    if not ParseHeader then
    begin
      programlog.LogOutFormatStr('uzeentproxygraphicparser: ParseHeader failed', [], LM_Error);
      Exit;
    end;
    
    // Парсим команды
    for I := 0 to FCommandCount - 1 do
    begin
      programlog.LogOutFormatStr('uzeentproxygraphicparser: Processing command %d of %d', [I + 1, FCommandCount], LM_Info);
      
      if not ParseCommand then
      begin
        programlog.LogOutFormatStr('uzeentproxygraphicparser: ParseCommand failed at command %d', [I + 1], LM_Error);
        Break;
      end;
    end;
    
    FResult.EntityCount := FResult.CircleCount + FResult.ArcCount;
    programlog.LogOutFormatStr('uzeentproxygraphicparser: Parse COMPLETE - Circles=%d, Arcs=%d, Total=%d', 
      [FResult.CircleCount, FResult.ArcCount, FResult.EntityCount], LM_Info);
    
    Result := FResult;
  except
    on E: Exception do
    begin
      programlog.LogOutFormatStr('uzeentproxygraphicparser: Parse EXCEPTION: %s', [E.Message], LM_Error);
      FillChar(Result, SizeOf(Result), 0);
    end;
  end;
end;

end.
