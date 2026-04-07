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
  uzegeometry,
  gzctnrVectorTypes,
  UGDBPoint3DArray;

type
  { Один контур (набор вершин одного примитива) для отрисовки }
  TProxyContour = record
    { Вершины контура }
    Vertices: GDBPoint3DArray;
    { Флаг: контур замкнут (полигон, круг, эллипс) }
    Closed: Boolean;
    { Флаг: контур заполнен (SOLID заливка, штриховка) }
    Filled: Boolean;
  end;

  { Итоговый результат разбора одного Proxy Graphic блока }
  TProxyGraphicParseResult = record
    { Суммарный BBox всех примитивов }
    BBoxMin: TzePoint3d;
    BBoxMax: TzePoint3d;
    { Флаг: BBox вычислен хотя бы одним примитивом }
    BBoxLoaded: Boolean;
    { Контуры примитивов для отрисовки (каждый контур отдельно) }
    Contours: array of TProxyContour;
    { Число контуров }
    ContourCount: Integer;
    { Общее число успешно обработанных примитивов (включая без вершин) }
    PrimitiveCount: Integer;
    { Текстовые примитивы для отрисовки через DrawTextContent }
    TextItems: array of TProxyTextItem;
  end;

  { Парсер Proxy Graphic.
    Создаётся для каждого прокси-объекта отдельно. }
  TProxyGraphicParser = class
  private
    FStream: TProxyByteStream;
    FResult: TProxyGraphicParseResult;

    { Текущее состояние заливки (SetFill OpCode=20).
      Устанавливается командой SetFill(1), сбрасывается после
      каждого графического примитива (как в ezdxf). }
    FFillActive: Boolean;

    { Стек матриц трансформации для PushMatrix/PopMatrix.
      Когда PushMatrix встречается — матрица читается и помещается
      в стек. Все последующие вершины примитивов трансформируются
      через текущую матрицу (вершина стека). PopMatrix убирает
      матрицу из стека, возвращая предыдущую. }
    FMatrixStack: array of TzeTypedMatrix4d;
    FMatrixStackDepth: Integer;

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
    procedure HandleSetLinetype;
    procedure HandleSetMarker;
    procedure HandleSetFill;
    procedure HandleSetTrueColor;
    procedure HandleSetLineweight;
    procedure HandleSetLtScale;
    procedure HandleSetThickness;
    procedure SkipDataBytes(const CommandSize: Integer);

    { Добавляет контур (набор вершин одного примитива) в массив контуров }
    procedure AppendContour(var Src: GDBPoint3DArray;
      const IsClosed: Boolean; const IsFilled: Boolean);

    { Расширяет суммарный BBox данными из одного примитива }
    procedure MergeHandlerBBox(const HandlerResult: TProxyHandlerResult);

    { Добавляет текстовый элемент в список TextItems результата }
    procedure AppendTextItem(const Item: TProxyTextItem);

    { Применяет текущую матрицу из стека к вершинам результата
      обработчика (если стек не пуст). }
    procedure TransformHandlerVertices(
      var HandlerResult: TProxyHandlerResult);

    { Проверяет, является ли текущая матрица в стеке единичной }
    function HasActiveTransform: Boolean;

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
  OPCODE_EXTENTS          = 1;
  OPCODE_SET_COLOR        = 14;
  OPCODE_SET_LAYER        = 16;
  OPCODE_SET_LINETYPE     = 18;
  OPCODE_SET_MARKER       = 19;
  OPCODE_SET_FILL         = 20;
  OPCODE_SET_TRUE_COLOR   = 22;
  OPCODE_SET_LINEWEIGHT   = 23;
  OPCODE_SET_LTSCALE      = 24;
  OPCODE_SET_THICKNESS    = 25;
  OPCODE_PUSH_MATRIX      = 29;
  OPCODE_PUSH_MATRIX2     = 30;
  OPCODE_POP_MATRIX       = 31;

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
  FFillActive := False;
  FMatrixStackDepth := 0;
  SetLength(FMatrixStack, 0);
end;

destructor TProxyGraphicParser.Destroy;
begin
  FStream.Free;
  inherited Destroy;
end;

{ Добавляет контур (вершины одного примитива) как отдельный элемент.
  Каждый примитив хранится в своём контуре для раздельной отрисовки. }
procedure TProxyGraphicParser.AppendContour(var Src: GDBPoint3DArray;
  const IsClosed: Boolean; const IsFilled: Boolean);
var
  Idx: Integer;
  ir: itrec;
  pV: PzePoint3d;
begin
  Idx := Length(FResult.Contours);
  SetLength(FResult.Contours, Idx + 1);
  FResult.Contours[Idx].Vertices.init(Src.Count);
  FResult.Contours[Idx].Closed := IsClosed;
  FResult.Contours[Idx].Filled := IsFilled;
  pV := Src.beginiterate(ir);
  while pV <> nil do
  begin
    FResult.Contours[Idx].Vertices.PushBackData(pV^);
    pV := Src.iterate(ir);
  end;
end;

{ Добавляет текстовый элемент в список TextItems результата }
procedure TProxyGraphicParser.AppendTextItem(const Item: TProxyTextItem);
var
  NewLen: Integer;
begin
  NewLen := Length(FResult.TextItems) + 1;
  SetLength(FResult.TextItems, NewLen);
  FResult.TextItems[NewLen - 1] := Item;
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

{ Системный обработчик: PushMatrix — читает матрицу трансформации (16 double)
  и помещает её в стек. Все последующие примитивы будут трансформированы
  через эту матрицу до вызова PopMatrix. }
procedure TProxyGraphicParser.HandlePushMatrix;
var
  I, Row, Col: Integer;
  MatrixData: array[0..15] of Double;
  NewMatrix: TzeTypedMatrix4d;
begin
  try
    { Читаем 16 чисел double из потока (4×4 матрица, построчно) }
    for I := 0 to 15 do
      MatrixData[I] := FStream.ReadDouble;

    { Заполняем матрицу ZCAD.
      Формат proxy: построчно data[row*4 + col], translation в col 3.
      Формат ZCAD: .mtr.v[row].v[col], translation в row 3.
      Транспонируем: mtr.v[Row].v[Col] := data[Col * 4 + Row]. }
    for Row := 0 to 3 do
      for Col := 0 to 3 do
        NewMatrix.mtr.v[Row].v[Col] := MatrixData[Col * 4 + Row];

    { Явно устанавливаем тип матрицы «общая трансформация».
      Без этого поле t содержит мусор из стека, и если он совпадает
      с CMTIdentity, то VectorTransform3D пропускает умножение. }
    NewMatrix.t := CMTTransform;

    { Добавляем матрицу в стек }
    Inc(FMatrixStackDepth);
    if FMatrixStackDepth > Length(FMatrixStack) then
      SetLength(FMatrixStack, FMatrixStackDepth);
    FMatrixStack[FMatrixStackDepth - 1] := NewMatrix;

    programlog.LogOutFormatStr(
      'uzeentproxygraphicparser: PushMatrix depth=%d ' +
      'translation=(%.3f, %.3f, %.3f)',
      [FMatrixStackDepth,
       NewMatrix.mtr.v[3].v[0],
       NewMatrix.mtr.v[3].v[1],
       NewMatrix.mtr.v[3].v[2]],
      LM_Info);
  except
    on E: Exception do
      programlog.LogOutFormatStr(
        'uzeentproxygraphicparser: HandlePushMatrix error: %s',
        [E.Message], LM_Info);
  end;
end;

{ Системный обработчик: PopMatrix — убирает матрицу из стека }
procedure TProxyGraphicParser.HandlePopMatrix;
begin
  if FMatrixStackDepth > 0 then
  begin
    Dec(FMatrixStackDepth);
    programlog.LogOutFormatStr(
      'uzeentproxygraphicparser: PopMatrix depth=%d',
      [FMatrixStackDepth], LM_Info);
  end
  else
    programlog.LogOutFormatStr(
      'uzeentproxygraphicparser: PopMatrix on empty stack',
      [], LM_Info);
end;

{ Проверяет, есть ли активная (не единичная) матрица в стеке }
function TProxyGraphicParser.HasActiveTransform: Boolean;
begin
  Result := FMatrixStackDepth > 0;
end;

{ Применяет текущую матрицу из стека к вершинам и BBox результата.
  Если стек пуст — ничего не делает.
  Используется после каждого успешного вызова обработчика примитива. }
procedure TProxyGraphicParser.TransformHandlerVertices(
  var HandlerResult: TProxyHandlerResult);
var
  ir: itrec;
  pV: PzePoint3d;
  CurrentMatrix: TzeTypedMatrix4d;
begin
  if not HasActiveTransform then
    Exit;

  { Берём верхнюю матрицу из стека }
  CurrentMatrix := FMatrixStack[FMatrixStackDepth - 1];

  { Трансформируем вершины контура }
  if HandlerResult.HasVertices and (HandlerResult.Vertices.Count > 0) then
  begin
    pV := HandlerResult.Vertices.beginiterate(ir);
    while pV <> nil do
    begin
      pV^ := VectorTransform3D(pV^, CurrentMatrix);
      pV := HandlerResult.Vertices.iterate(ir);
    end;
  end;

  { Трансформируем BBox }
  if HandlerResult.HasBBox then
  begin
    HandlerResult.BBoxMin :=
      VectorTransform3D(HandlerResult.BBoxMin, CurrentMatrix);
    HandlerResult.BBoxMax :=
      VectorTransform3D(HandlerResult.BBoxMax, CurrentMatrix);
  end;

  { Трансформируем точку вставки текста }
  if HandlerResult.HasTextItem then
    HandlerResult.TextItem.Insert :=
      VectorTransform3D(HandlerResult.TextItem.Insert, CurrentMatrix);
end;

{ Системный обработчик: SetLinetype — читает индекс типа линии }
procedure TProxyGraphicParser.HandleSetLinetype;
begin
  try
    FStream.ReadInt32;
  except
    on E: Exception do
      programlog.LogOutFormatStr(
        'uzeentproxygraphicparser: HandleSetLinetype error: %s',
        [E.Message], LM_Info);
  end;
end;

{ Системный обработчик: SetMarker — читает индекс маркера выбора }
procedure TProxyGraphicParser.HandleSetMarker;
begin
  try
    FStream.ReadInt32;
  except
    on E: Exception do
      programlog.LogOutFormatStr(
        'uzeentproxygraphicparser: HandleSetMarker error: %s',
        [E.Message], LM_Info);
  end;
end;

{ Системный обработчик: SetFill — читает и сохраняет флаг заливки.
  Значение 1 = заливка включена, 0 = выключена.
  Флаг сбрасывается после каждого графического примитива. }
procedure TProxyGraphicParser.HandleSetFill;
var
  FillValue: Integer;
begin
  try
    FillValue := FStream.ReadInt32;
    FFillActive := (FillValue = 1);
    programlog.LogOutFormatStr(
      'uzeentproxygraphicparser: SetFill value=%d active=%s',
      [FillValue, BoolToStr(FFillActive, True)], LM_Info);
  except
    on E: Exception do
      programlog.LogOutFormatStr(
        'uzeentproxygraphicparser: HandleSetFill error: %s',
        [E.Message], LM_Info);
  end;
end;

{ Системный обработчик: SetTrueColor — читает значение RGB цвета }
procedure TProxyGraphicParser.HandleSetTrueColor;
begin
  try
    FStream.ReadInt32;
  except
    on E: Exception do
      programlog.LogOutFormatStr(
        'uzeentproxygraphicparser: HandleSetTrueColor error: %s',
        [E.Message], LM_Info);
  end;
end;

{ Системный обработчик: SetLineweight — читает вес линии }
procedure TProxyGraphicParser.HandleSetLineweight;
begin
  try
    FStream.ReadInt32;
  except
    on E: Exception do
      programlog.LogOutFormatStr(
        'uzeentproxygraphicparser: HandleSetLineweight error: %s',
        [E.Message], LM_Info);
  end;
end;

{ Системный обработчик: SetLtScale — читает масштаб типа линии }
procedure TProxyGraphicParser.HandleSetLtScale;
begin
  try
    FStream.ReadDouble;
  except
    on E: Exception do
      programlog.LogOutFormatStr(
        'uzeentproxygraphicparser: HandleSetLtScale error: %s',
        [E.Message], LM_Info);
  end;
end;

{ Системный обработчик: SetThickness — читает толщину }
procedure TProxyGraphicParser.HandleSetThickness;
begin
  try
    FStream.ReadDouble;
  except
    on E: Exception do
      programlog.LogOutFormatStr(
        'uzeentproxygraphicparser: HandleSetThickness error: %s',
        [E.Message], LM_Info);
  end;
end;

{ Разбирает одну команду.
  Системные OpCode обрабатываются напрямую.
  Остальные передаются в TProxyOpCodeDispatcher.
  После обработки команды указатель потока устанавливается на начало
  следующей команды (по CommandSize), даже если обработчик не прочитал
  все данные. Это необходимо для корректного разбора команд с
  дополнительными данными (traits), которые обработчик может пропустить. }
procedure TProxyGraphicParser.ParseCommand;
var
  CommandSize: Integer;
  OpCode: Integer;
  HandlerResult: TProxyHandlerResult;
  StartIndex: Integer;
  ExpectedEnd: Integer;
  BytesRemaining: Integer;
begin
  { Запоминаем позицию начала команды (до чтения заголовка) }
  StartIndex := FStream.Index;

  CommandSize := FStream.ReadInt32;
  OpCode := FStream.ReadInt32;

  { Позиция, на которой должна закончиться обработка команды }
  ExpectedEnd := StartIndex + CommandSize;

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

    OPCODE_SET_LINETYPE:
      HandleSetLinetype;

    OPCODE_SET_MARKER:
      HandleSetMarker;

    OPCODE_SET_FILL:
      HandleSetFill;

    OPCODE_SET_TRUE_COLOR:
      HandleSetTrueColor;

    OPCODE_SET_LINEWEIGHT:
      HandleSetLineweight;

    OPCODE_SET_LTSCALE:
      HandleSetLtScale;

    OPCODE_SET_THICKNESS:
      HandleSetThickness;

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
        { Применяем матрицу трансформации из стека (если есть) }
        TransformHandlerVertices(HandlerResult);

        { Обновляем суммарный BBox }
        MergeHandlerBBox(HandlerResult);

        { Определяем заливку: флаг FFillActive применяется
          к замкнутым контурам (Polygon, Shell и др.).
          Аналогично ezdxf: SetFill(1) → Polygon/Shell = SOLID. }
        if HandlerResult.HasVertices and (HandlerResult.Vertices.Count > 0) then
        begin
          if FFillActive and HandlerResult.Closed then
            HandlerResult.Filled := True;
          AppendContour(HandlerResult.Vertices,
            HandlerResult.Closed, HandlerResult.Filled);
          Inc(FResult.ContourCount);
          HandlerResult.Vertices.done;
        end;

        { Сохраняем текстовый примитив для последующей отрисовки }
        if HandlerResult.HasTextItem then
          AppendTextItem(HandlerResult.TextItem);

        Inc(FResult.PrimitiveCount);

        { Сбрасываем флаг заливки после каждого примитива
          (аналогично ezdxf: fill auto-reset после entity) }
        FFillActive := False;
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

  { Корректируем позицию потока: если обработчик не прочитал все
    данные команды (например, traits у Shell), пропускаем остаток.
    Это гарантирует, что следующая команда будет прочитана с верной позиции. }
  BytesRemaining := ExpectedEnd - FStream.Index;
  if BytesRemaining > 0 then
  begin
    programlog.LogOutFormatStr(
      'uzeentproxygraphicparser: Skipping %d trailing bytes for OpCode=%d',
      [BytesRemaining, OpCode], LM_Info);
    FStream.Skip(BytesRemaining);
  end;
end;

{ Главный метод: разбирает весь блок Proxy Graphic }
function TProxyGraphicParser.Parse: TProxyGraphicParseResult;
var
  CommandCount, I: Integer;
begin
  SetLength(FResult.Contours, 0);
  FResult.BBoxLoaded := False;
  FResult.PrimitiveCount := 0;
  FResult.ContourCount := 0;
  SetLength(FResult.TextItems, 0);

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
      'uzeentproxygraphicparser: Parse DONE: primitives=%d contours=%d bbox=%s',
      [FResult.PrimitiveCount, FResult.ContourCount,
       BoolToStr(FResult.BBoxLoaded, True)], LM_Info);

  except
    on E: Exception do
      programlog.LogOutFormatStr(
        'uzeentproxygraphicparser: Parse exception: %s', [E.Message], LM_Info);
  end;

  Result := FResult;
end;

end.
