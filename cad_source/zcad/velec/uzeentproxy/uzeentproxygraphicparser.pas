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
  Модуль: uzeentproxygraphicparser
  Назначение: Парсер бинарного блока Proxy Graphic (AcGiWorldDraw формат).

  Структура данных Proxy Graphic:
    [ChunkSize: int32] [CommandCount: int32]
    Повтор CommandCount раз:
      [CommandSize: int32] [OpCode: int32] [Данные...]

  Диспетчеризация:
  - Системные команды (Extents, SetColor, SetLayer, Push/PopMatrix) обрабатываются
    непосредственно в этом модуле.
  - Примитивные команды (Circle, Text, Polyline и т.д.) обрабатываются через
    TProxyOpCodeDispatcher — менеджер, куда каждый модуль-парсер регистрирует
    свой обработчик в секции initialization.
  - Если примитив не зарегистрирован — команда пропускается автоматически.

  Результат парсинга (TProxyGraphicParseResult):
  - BBoxMin, BBoxMax — суммарный BBox всех успешно распаршенных примитивов
  - AllVertices      — вершины всех контуров для отрисовки (полилинии)
  - PrimitiveCount   — количество успешно обработанных примитивов
}

unit uzeentproxygraphicparser;
{$Mode delphi}{$H+}
{$INCLUDE zengineconfig.inc}

interface

uses
  SysUtils,
  uzeentproxystream,
  uzeentproxymanager,
  uzegeometrytypes,
  gzctnrVectorTypes,
  UGDBPoint3DArray;

type
  { Итоговый результат разбора одного Proxy Graphic блока }
  TProxyGraphicParseResult = record
    { Суммарный BBox всех примитивов }
    BBoxMin: TzePoint3d;
    BBoxMax: TzePoint3d;
    { Флаг: BBox вычислен хотя бы одним примитивом }
    BBoxLoaded: Boolean;
    { Все вершины контуров для отрисовки, объединённые в один массив }
    AllVertices: GDBPoint3DArray;
    { Флаг: в AllVertices есть данные }
    HasVertices: Boolean;
    { Число контуров (примитивов), добавленных в AllVertices }
    ContourCount: Integer;
    { Общее число успешно обработанных примитивов (включая без вершин, только BBox) }
    PrimitiveCount: Integer;
  end;

  { Парсер Proxy Graphic.
    Создаётся для каждого прокси-объекта отдельно. }
  TProxyGraphicParser = class
  private
    FStream: TProxyByteStream;
    FResult: TProxyGraphicParseResult;

    { Разбирает заголовок блока; возвращает количество команд }
    function ParseHeader(out CommandCount: Integer): Boolean;

    { Разбирает одну команду; пропускает неизвестные }
    procedure ParseCommand;

    { Системные обработчики — не модульные, так как изменение их поведения
      требует изменения архитектуры всего прокси-объекта }
    procedure HandleExtents;
    procedure HandleSetColor;
    procedure HandleSetLayer;
    procedure HandlePushMatrix;
    procedure HandlePopMatrix;
    procedure SkipDataBytes(const CommandSize: Integer);

    { Добавляет вершины примитива в суммарный массив AllVertices }
    procedure AppendVertices(var Src: GDBPoint3DArray);

    { Расширяет суммарный BBox данными из одного примитива }
    procedure MergeHandlerBBox(const HandlerResult: TProxyHandlerResult);

  public
    constructor Create(const Data: TBytes);
    destructor Destroy; override;

    { Разбирает весь блок Proxy Graphic; возвращает суммарный результат }
    function Parse: TProxyGraphicParseResult;
  end;

implementation

uses
  uzcLog;

const
  { Системные OpCode, обрабатываемые напрямую в этом модуле }
  OPCODE_EXTENTS     = 1;
  OPCODE_SET_COLOR   = 14;
  OPCODE_SET_LAYER   = 16;
  OPCODE_PUSH_MATRIX = 29;
  OPCODE_PUSH_MATRIX2 = 30;
  OPCODE_POP_MATRIX  = 31;

  { Размер заголовка одной команды: [CommandSize: int32] + [OpCode: int32] }
  COMMAND_HEADER_SIZE = 8;

  { Максимально разумное количество команд в одном блоке }
  MAX_COMMAND_COUNT = 100000;

{ === TProxyGraphicParser === }

constructor TProxyGraphicParser.Create(const Data: TBytes);
begin
  inherited Create;
  FStream := TProxyByteStream.Create(Data);
  FillChar(FResult, SizeOf(FResult), 0);
  FResult.AllVertices.init(0);
end;

destructor TProxyGraphicParser.Destroy;
begin
  FStream.Free;
  inherited Destroy;
end;

{ Добавляет все вершины из Src в суммарный массив AllVertices.
  Между контурами вставляется разделитель (NaN-вершина) не нужен —
  используется DrawPolyLine с замкнутым флагом для каждого контура отдельно,
  поэтому контуры хранятся непрерывно, а разделение по контурам
  осуществляется через ContourCount + размеры сегментов (не реализовано здесь:
  текущая реализация просто конкатенирует вершины, рендерер их рисует одним
  DrawPolyLineWithLT). }
procedure TProxyGraphicParser.AppendVertices(var Src: GDBPoint3DArray);
var
  ir: itrec;
  pV: PzePoint3d;
begin
  pV := Src.beginiterate(ir);
  while pV <> nil do
  begin
    FResult.AllVertices.PushBackData(pV^);
    pV := Src.iterate(ir);
  end;
end;

{ Расширяет суммарный BBox данными из результата обработчика }
procedure TProxyGraphicParser.MergeHandlerBBox(
  const HandlerResult: TProxyHandlerResult);
begin
  if not HandlerResult.HasBBox then
    Exit;
  MergeBBox(
    HandlerResult.BBoxMin, HandlerResult.BBoxMax,
    FResult.BBoxMin, FResult.BBoxMax,
    FResult.BBoxLoaded);
end;

{ Разбирает заголовок блока: [ChunkSize][CommandCount] }
function TProxyGraphicParser.ParseHeader(out CommandCount: Integer): Boolean;
var
  ChunkSize: Integer;
begin
  Result := False;
  CommandCount := 0;
  try
    ChunkSize := FStream.ReadInt32;
    CommandCount := FStream.ReadInt32;
    programlog.LogOutFormatStr(
      'uzeentproxygraphicparser: Header ChunkSize=%d CommandCount=%d',
      [ChunkSize, CommandCount], LM_Info);
    Result := (ChunkSize > 0)
      and (CommandCount > 0)
      and (CommandCount < MAX_COMMAND_COUNT);
  except
    on E: Exception do
      programlog.LogOutFormatStr(
        'uzeentproxygraphicparser: ParseHeader error: %s', [E.Message], LM_Info);
  end;
end;

{ Пропускает байты данных команды (всё, что идёт после заголовка) }
procedure TProxyGraphicParser.SkipDataBytes(const CommandSize: Integer);
var
  DataSize: Integer;
begin
  DataSize := CommandSize - COMMAND_HEADER_SIZE;
  if DataSize > 0 then
  begin
    programlog.LogOutFormatStr(
      'uzeentproxygraphicparser: SkipDataBytes %d bytes', [DataSize], LM_Info);
    FStream.Skip(DataSize);
  end;
end;

{ Системный обработчик: ExtentsCommand — BBox объекта из файла }
procedure TProxyGraphicParser.HandleExtents;
var
  MinPt, MaxPt: TzePoint3d;
begin
  try
    MinPt := FStream.ReadVertex;
    MaxPt := FStream.ReadVertex;
    programlog.LogOutFormatStr(
      'uzeentproxygraphicparser: Extents Min=(%.3f,%.3f,%.3f) Max=(%.3f,%.3f,%.3f)',
      [MinPt.x, MinPt.y, MinPt.z, MaxPt.x, MaxPt.y, MaxPt.z], LM_Info);
    { Extents из файла используем только как начальный BBox,
      если реальные примитивы ещё не дали своего }
    if not FResult.BBoxLoaded then
      MergeBBox(MinPt, MaxPt, FResult.BBoxMin, FResult.BBoxMax, FResult.BBoxLoaded);
  except
    on E: Exception do
      programlog.LogOutFormatStr(
        'uzeentproxygraphicparser: HandleExtents error: %s', [E.Message], LM_Info);
  end;
end;

{ Системный обработчик: SetColor — читает и игнорирует значение цвета }
procedure TProxyGraphicParser.HandleSetColor;
begin
  try
    FStream.ReadInt32; { Значение цвета — пока не применяется }
  except
    on E: Exception do
      programlog.LogOutFormatStr(
        'uzeentproxygraphicparser: HandleSetColor error: %s', [E.Message], LM_Info);
  end;
end;

{ Системный обработчик: SetLayer — читает и игнорирует индекс слоя }
procedure TProxyGraphicParser.HandleSetLayer;
begin
  try
    FStream.ReadInt32; { Индекс слоя — пока не применяется }
  except
    on E: Exception do
      programlog.LogOutFormatStr(
        'uzeentproxygraphicparser: HandleSetLayer error: %s', [E.Message], LM_Info);
  end;
end;

{ Системный обработчик: PushMatrix — читает матрицу трансформации (16 double) }
procedure TProxyGraphicParser.HandlePushMatrix;
var
  I: Integer;
begin
  try
    { TODO: применять матрицу к последующим примитивам }
    for I := 0 to 15 do
      FStream.ReadDouble;
    programlog.LogOutFormatStr(
      'uzeentproxygraphicparser: PushMatrix read (matrix transform not yet applied)',
      [], LM_Info);
  except
    on E: Exception do
      programlog.LogOutFormatStr(
        'uzeentproxygraphicparser: HandlePushMatrix error: %s', [E.Message], LM_Info);
  end;
end;

{ Системный обработчик: PopMatrix — конец блока трансформации }
procedure TProxyGraphicParser.HandlePopMatrix;
begin
  { TODO: восстановить предыдущую матрицу }
  programlog.LogOutFormatStr(
    'uzeentproxygraphicparser: PopMatrix', [], LM_Info);
end;

{ Разбирает одну команду.
  Системные OpCode обрабатываются напрямую.
  Остальные передаются в TProxyOpCodeDispatcher. }
procedure TProxyGraphicParser.ParseCommand;
var
  CommandSize: Integer;
  OpCode: Integer;
  HandlerResult: TProxyHandlerResult;
begin
  CommandSize := FStream.ReadInt32;
  OpCode := FStream.ReadInt32;

  programlog.LogOutFormatStr(
    'uzeentproxygraphicparser: Command OpCode=%d Size=%d',
    [OpCode, CommandSize], LM_Info);

  { Слишком маленький размер команды — пропускаем }
  if CommandSize < COMMAND_HEADER_SIZE then
    Exit;

  { Сначала проверяем системные OpCode }
  case OpCode of
    OPCODE_EXTENTS:
      HandleExtents;

    OPCODE_SET_COLOR:
      HandleSetColor;

    OPCODE_SET_LAYER:
      HandleSetLayer;

    OPCODE_PUSH_MATRIX, OPCODE_PUSH_MATRIX2:
      HandlePushMatrix;

    OPCODE_POP_MATRIX:
      HandlePopMatrix;

  else
    { Передаём в диспетчер — каждый зарегистрированный модуль-парсер
      получит вызов своего обработчика }
    if TProxyOpCodeDispatcher.IsRegistered(OpCode) then
    begin
      if TProxyOpCodeDispatcher.HandleOpCode(OpCode, FStream, HandlerResult) then
      begin
        { Обновляем суммарный BBox }
        MergeHandlerBBox(HandlerResult);

        { Сохраняем вершины контура и освобождаем память парсера }
        if HandlerResult.HasVertices and (HandlerResult.Vertices.Count > 0) then
        begin
          AppendVertices(HandlerResult.Vertices);
          Inc(FResult.ContourCount);
          FResult.HasVertices := True;
          HandlerResult.Vertices.done;
        end;

        Inc(FResult.PrimitiveCount);
      end
      else
      begin
        programlog.LogOutFormatStr(
          'uzeentproxygraphicparser: Handler for OpCode=%d returned invalid result',
          [OpCode], LM_Info);
        SkipDataBytes(CommandSize);
      end;
    end
    else
    begin
      programlog.LogOutFormatStr(
        'uzeentproxygraphicparser: OpCode=%d not registered, skipping %d bytes',
        [OpCode, CommandSize - COMMAND_HEADER_SIZE], LM_Info);
      SkipDataBytes(CommandSize);
    end;
  end;
end;

{ Главный метод: разбирает весь блок Proxy Graphic }
function TProxyGraphicParser.Parse: TProxyGraphicParseResult;
var
  CommandCount, I: Integer;
begin
  FResult.AllVertices.init(0);
  FResult.BBoxLoaded := False;
  FResult.HasVertices := False;
  FResult.PrimitiveCount := 0;
  FResult.ContourCount := 0;

  programlog.LogOutFormatStr(
    'uzeentproxygraphicparser: Parse START (registered handlers: %d)',
    [TProxyOpCodeDispatcher.GetRegisteredCount], LM_Info);

  try
    if not ParseHeader(CommandCount) then
    begin
      programlog.LogOutFormatStr(
        'uzeentproxygraphicparser: ParseHeader failed', [], LM_Info);
      Result := FResult;
      Exit;
    end;

    for I := 0 to CommandCount - 1 do
    begin
      if FStream.EndOfStream then
        Break;
      try
        ParseCommand;
      except
        on E: Exception do
        begin
          programlog.LogOutFormatStr(
            'uzeentproxygraphicparser: Command %d exception: %s',
            [I, E.Message], LM_Info);
          Break;
        end;
      end;
    end;

    programlog.LogOutFormatStr(
      'uzeentproxygraphicparser: Parse DONE: primitives=%d contours=%d vertices=%d bbox=%s',
      [FResult.PrimitiveCount, FResult.ContourCount,
       FResult.AllVertices.Count,
       BoolToStr(FResult.BBoxLoaded, True)], LM_Info);

  except
    on E: Exception do
      programlog.LogOutFormatStr(
        'uzeentproxygraphicparser: Parse exception: %s', [E.Message], LM_Info);
  end;

  Result := FResult;
end;

end.
