{
*****************************************************************************
*                                                                           *
*  This file is part of the ZCAD                                            *
*                                                                           *
*****************************************************************************

@author(HTTP IPC dispatcher for ZCAD)
@author(Vladimir Bobrov)

Описание:

  Универсальный HTTP IPC диспетчер.

  ВАЖНО:

    Этот unit НЕ зависит от:

      uzvipcserver
      TIPCCommandType
      PIPCCommand
      IPCCommandQueue

  HTTP IPC состоит из трёх частей:

      1. Реестр команд
      2. Очередь команд
      3. HTTP -> Command -> Response

  Команды регистрируются другими unit-ами:

      uzvhttpcmdgeometry
      uzvhttpcmdtext
      uzvhttpcmdbatch
      uzvhttpcmddevice
      uzvhttpcmdfile

  Пример регистрации:

      RegisterHTTPIPCCommand(
        'LINE',
        @HTTPCommandLine
      );

  HTTP поток НЕ выполняет ZCAD-команду.

  Он:

      JSON
        |
        v
      Command
        |
        v
      HTTPIPCCommandQueue
        |
        v
      главный поток ZCAD
        |
        v
      Handler
        |
        v
      Response
}

{$mode objfpc}{$H+}

unit uzvhttpipc;

{$INCLUDE zengineconfig.inc}

interface

uses
  Classes,
  SysUtils,
  SyncObjs,
  fpjson,
  jsonparser,
  uzclog,
  uzbLogTypes;

type

  {=========================================================================}
  {                                                                         }
  {  Обработчик HTTP IPC команды                                            }
  {                                                                         }
  {=========================================================================}

  THTTPIPCCommandHandler = function(
    AArgs: TJSONArray;
    out AResult: string;
    out AError: string
  ): Boolean;


  {=========================================================================}
  {                                                                         }
  {  Команда HTTP IPC                                                      }
  {                                                                         }
  {=========================================================================}

  THTTPIPCCommand = class
  private
    FRefCount: LongInt;

    procedure DestroyCommand;

  public
    ID: string;
    CmdName: string;
    Token: string;

    Args: TJSONArray;

    Handler: THTTPIPCCommandHandler;

    ResultText: string;
    ErrorText: string;

    Success: Boolean;

    Completed: TEvent;

    constructor Create;
    destructor Destroy; override;

    procedure AddRef;
    procedure Release;

    function WaitFor(ATimeout: Cardinal): TWaitResult;
  end;


  {=========================================================================}
  {                                                                         }
  {  Очередь HTTP IPC                                                       }
  {                                                                         }
  {=========================================================================}

  THTTPIPCCommandQueue = class
  private
    FLock: TCriticalSection;
    FItems: TList;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Enqueue(ACommand: THTTPIPCCommand);

    function TryDequeue(
      out ACommand: THTTPIPCCommand
    ): Boolean;

    function IsEmpty: Boolean;
    function Count: Integer;
  end;


{=============================================================================
  РЕЕСТР КОМАНД
=============================================================================}

{**
  Зарегистрировать HTTP IPC команду.

  Пример:

    RegisterHTTPIPCCommand(
      'LINE',
      @HTTPCommandLine
    );
}
procedure RegisterHTTPIPCCommand(
  const AName: string;
  AHandler: THTTPIPCCommandHandler
);


{**
  Проверить наличие команды в реестре.
}
function HTTPIPCCommandRegistered(
  const AName: string
): Boolean;


{**
  Получить обработчик команды.

  Result = True:
    команда найдена.

  Result = False:
    команда отсутствует.
}
function HTTPIPCGetCommandHandler(
  const AName: string;
  out AHandler: THTTPIPCCommandHandler
): Boolean;


{=============================================================================
  ОЧЕРЕДЬ
=============================================================================}

{**
  Глобальная очередь HTTP IPC.

  Обрабатывается главным потоком ZCAD.
}
var
  HTTPIPCCommandQueue: THTTPIPCCommandQueue;


{=============================================================================
  HTTP EXECUTION
=============================================================================}

{**
  Выполнить HTTP IPC JSON запрос.

  Формат:

    {
      "id": "123",
      "cmd": "LINE",
      "args": [0, 0, 100, 100],
      "token": ""
    }

  HTTP поток:

    1. Парсит JSON.
    2. Находит Handler.
    3. Создаёт THTTPIPCCommand.
    4. Помещает его в очередь.
    5. Ждёт завершения.
    6. Возвращает JSON.

  ВАЖНО:

    Реальное выполнение Handler происходит не здесь,
    а в главном потоке ZCAD через HTTPIPCCommandQueue.
}
function HTTPIPCExecuteJSON(
  const AJSON: string;
  out AResponse: string
): Boolean;


{=============================================================================
  ГЛАВНЫЙ ПОТОК
=============================================================================}

{**
  Обработать одну команду из очереди.

  Эту функцию должен вызывать главный поток ZCAD.

  Например:

      while HTTPIPCProcessNextCommand do;

  или из TTimer:

      HTTPIPCProcessPendingCommands;
}
function HTTPIPCProcessNextCommand: Boolean;


{**
  Обработать все ожидающие команды.

  Вызывать только из главного потока ZCAD.
}
procedure HTTPIPCProcessPendingCommands;


{**
  Время ожидания HTTP команды.

  30 секунд.
}
const
  HTTP_IPC_COMMAND_TIMEOUT = 30000;


implementation

type

  {=========================================================================}
  {                                                                         }
  {  Элемент реестра                                                        }
  {                                                                         }
  {=========================================================================}

  THTTPIPCCommandRegistration = record
    Name: string;
    Handler: THTTPIPCCommandHandler;
  end;


var

  { Реестр команд }
  HTTPIPCCommandRegistry:
    array of THTTPIPCCommandRegistration;

  { Защита реестра }
  HTTPIPCRegistryLock:
    TCriticalSection;


{=============================================================================
  LOG
=============================================================================}

procedure HTTPIPCLog(
  const AMessage: string;
  ALogLevel: TLogLevel
);
begin

  ProgramLog.LogOutFormatStr(
    '[HTTP-IPC] %s',
    [AMessage],
    ALogLevel,
    0
  );

end;


{=============================================================================
  НОРМАЛИЗАЦИЯ ИМЕНИ
=============================================================================}

function NormalizeCommandName(
  const AName: string
): string;
begin

  Result :=
    UpperCase(
      Trim(AName)
    );

end;


{=============================================================================
  THTTPIPCCommand
=============================================================================}

constructor THTTPIPCCommand.Create;
begin

  inherited Create;

  FRefCount := 1;

  ID := '';
  CmdName := '';
  Token := '';

  Args := nil;

  Handler := nil;

  ResultText := '';
  ErrorText := '';

  Success := False;

  Completed :=
    TEvent.Create(
      nil,
      True,
      False,
      ''
    );

end;


destructor THTTPIPCCommand.Destroy;
begin

  if Args <> nil then
  begin
    Args.Free;
    Args := nil;
  end;


  if Completed <> nil then
  begin
    Completed.Free;
    Completed := nil;
  end;


  inherited Destroy;

end;


procedure THTTPIPCCommand.DestroyCommand;
begin

  Destroy;

end;


procedure THTTPIPCCommand.AddRef;
begin

  InterlockedIncrement(FRefCount);

end;


procedure THTTPIPCCommand.Release;
begin

  if InterlockedDecrement(FRefCount) = 0 then
    DestroyCommand;

end;


function THTTPIPCCommand.WaitFor(
  ATimeout: Cardinal
): TWaitResult;
begin

  if Completed = nil then
    Exit(wrError);

  Result :=
    Completed.WaitFor(
      ATimeout
    );

end;


{=============================================================================
  THTTPIPCCommandQueue
=============================================================================}

constructor THTTPIPCCommandQueue.Create;
begin

  inherited Create;

  FLock :=
    TCriticalSection.Create;

  FItems :=
    TList.Create;

end;


destructor THTTPIPCCommandQueue.Destroy;
var
  I: Integer;
  Cmd: THTTPIPCCommand;
begin

  if FLock <> nil then
    FLock.Acquire;

  try

    if FItems <> nil then
    begin

      for I := 0 to FItems.Count - 1 do
      begin

        Cmd :=
          THTTPIPCCommand(
            FItems[I]
          );

        if Cmd <> nil then
          Cmd.Release;

      end;

      FItems.Clear;

    end;

  finally

    if FLock <> nil then
      FLock.Release;

  end;


  FItems.Free;
  FItems := nil;

  FLock.Free;
  FLock := nil;


  inherited Destroy;

end;


procedure THTTPIPCCommandQueue.Enqueue(
  ACommand: THTTPIPCCommand
);
begin

  if ACommand = nil then
    raise Exception.Create(
      'Cannot enqueue nil HTTP IPC command'
    );

  { Очередь получает собственную ссылку }
  ACommand.AddRef;

  FLock.Acquire;
  try

    try

      FItems.Add(
        ACommand
      );

    except

      { Если Add завершился ошибкой,
        возвращаем ссылку очереди. }

      ACommand.Release;

      raise;

    end;

  finally

    FLock.Release;

  end;

end;


function THTTPIPCCommandQueue.TryDequeue(
  out ACommand: THTTPIPCCommand
): Boolean;
begin

  ACommand := nil;

  FLock.Acquire;

  try

    if FItems.Count = 0 then
      Exit(False);


    ACommand :=
      THTTPIPCCommand(
        FItems[0]
      );


    FItems.Delete(0);


    { ВАЖНО:

      Ссылка очереди передаётся вызывающему.

      Поэтому здесь НЕ вызываем Release.
    }

    Result := True;

  finally

    FLock.Release;

  end;

end;


function THTTPIPCCommandQueue.IsEmpty: Boolean;
begin

  FLock.Acquire;

  try

    Result :=
      FItems.Count = 0;

  finally

    FLock.Release;

  end;

end;


function THTTPIPCCommandQueue.Count: Integer;
begin

  FLock.Acquire;

  try

    Result :=
      FItems.Count;

  finally

    FLock.Release;

  end;

end;


{=============================================================================
  REPOSITORY SEARCH
=============================================================================}

function FindHTTPIPCCommandIndex(
  const AName: string
): Integer;
var
  I: Integer;
  Name: string;
begin

  Result := -1;

  Name :=
    NormalizeCommandName(
      AName
    );


  for I := 0 to High(HTTPIPCCommandRegistry) do
  begin

    if HTTPIPCCommandRegistry[I].Name = Name then
    begin

      Result := I;
      Exit;

    end;

  end;

end;


{=============================================================================
  REGISTER
=============================================================================}

procedure RegisterHTTPIPCCommand(
  const AName: string;
  AHandler: THTTPIPCCommandHandler
);
var
  Name: string;
  Index: Integer;
  Count: Integer;
begin

  Name :=
    NormalizeCommandName(
      AName
    );


  if Name = '' then
  begin

    HTTPIPCLog(
      'Cannot register command with empty name',
      LM_Error
    );

    Exit;

  end;


  if not Assigned(AHandler) then
  begin

    HTTPIPCLog(
      Format(
        'Cannot register command "%s": handler is nil',
        [Name]
      ),
      LM_Error
    );

    Exit;

  end;


  HTTPIPCRegistryLock.Acquire;

  try

    Index :=
      FindHTTPIPCCommandIndex(
        Name
      );


    if Index >= 0 then
    begin

      HTTPIPCLog(
        Format(
          'HTTP IPC command already registered: %s',
          [Name]
        ),
        LM_Error
      );

      Exit;

    end;


    Count :=
      Length(
        HTTPIPCCommandRegistry
      );


    SetLength(
      HTTPIPCCommandRegistry,
      Count + 1
    );


    HTTPIPCCommandRegistry[Count].Name :=
      Name;

    HTTPIPCCommandRegistry[Count].Handler :=
      AHandler;


    HTTPIPCLog(
      Format(
        'HTTP IPC command registered: %s',
        [Name]
      ),
      LM_Debug
    );

  finally

    HTTPIPCRegistryLock.Release;

  end;

end;


{=============================================================================
  REGISTERED?
=============================================================================}

function HTTPIPCCommandRegistered(
  const AName: string
): Boolean;
begin

  HTTPIPCRegistryLock.Acquire;

  try

    Result :=
      FindHTTPIPCCommandIndex(
        AName
      ) >= 0;

  finally

    HTTPIPCRegistryLock.Release;

  end;

end;


{=============================================================================
  GET HANDLER
=============================================================================}

function HTTPIPCGetCommandHandler(
  const AName: string;
  out AHandler: THTTPIPCCommandHandler
): Boolean;
var
  Index: Integer;
begin

  AHandler := nil;

  HTTPIPCRegistryLock.Acquire;

  try

    Index :=
      FindHTTPIPCCommandIndex(
        AName
      );


    if Index < 0 then
      Exit(False);


    AHandler :=
      HTTPIPCCommandRegistry[Index].Handler;


    Result :=
      Assigned(AHandler);

  finally

    HTTPIPCRegistryLock.Release;

  end;

end;


{=============================================================================
  ERROR RESPONSE
=============================================================================}

function HTTPIPCCreateErrorResponse(
  const AID: string;
  const AError: string
): string;
var
  Response: TJSONObject;
begin

  Response :=
    TJSONObject.Create;

  try

    Response.Add(
      'id',
      AID
    );

    Response.Add(
      'status',
      'error'
    );

    Response.Add(
      'error',
      AError
    );


    Result :=
      Response.AsJSON;

  finally

    Response.Free;

  end;

end;


{=============================================================================
  SUCCESS RESPONSE
=============================================================================}

function HTTPIPCCreateCommandResponse(
  ACommand: THTTPIPCCommand
): string;
var
  Response: TJSONObject;
begin

  Response :=
    TJSONObject.Create;

  try

    Response.Add(
      'id',
      ACommand.ID
    );


    if ACommand.Success then
    begin

      Response.Add(
        'status',
        'ok'
      );


      if ACommand.ResultText <> '' then
        Response.Add(
          'result',
          ACommand.ResultText
        );

    end
    else
    begin

      Response.Add(
        'status',
        'error'
      );


      if ACommand.ErrorText <> '' then
        Response.Add(
          'error',
          ACommand.ErrorText
        )
      else
        Response.Add(
          'error',
          'Command execution failed'
        );

    end;


    Result :=
      Response.AsJSON;

  finally

    Response.Free;

  end;

end;


{=============================================================================
  JSON EXECUTION
=============================================================================}

function HTTPIPCExecuteJSON(
  const AJSON: string;
  out AResponse: string
): Boolean;
var
  Parser: TJSONParser;
  Root: TJSONObject;

  Cmd: THTTPIPCCommand;

  CmdName: string;
  CommandID: string;
  Token: string;

  ArgsData: TJSONData;

  Handler: THTTPIPCCommandHandler;

  WaitResult: TWaitResult;

  Enqueued: Boolean;

begin

  Result := False;
  AResponse := '';

  Parser := nil;
  Root := nil;

  Cmd := nil;

  Enqueued := False;


  {=========================================================================}
  { Проверка JSON }
  {=========================================================================}

  if Trim(AJSON) = '' then
  begin

    AResponse :=
      HTTPIPCCreateErrorResponse(
        '',
        'Empty request body'
      );

    Exit;

  end;


  {=========================================================================}
  { Проверка очереди }
  {=========================================================================}

  if HTTPIPCCommandQueue = nil then
  begin

    HTTPIPCLog(
      'HTTP IPC command queue is not initialized',
      LM_Error
    );


    AResponse :=
      HTTPIPCCreateErrorResponse(
        '',
        'HTTP IPC command queue is not initialized'
      );

    Exit;

  end;


  {=========================================================================}
  { JSON PARSE }
  {=========================================================================}

  try

    Parser :=
      TJSONParser.Create(
        AJSON
      );

    try

      try

        Root :=
          Parser.Parse as TJSONObject;

      except

        on E: Exception do
        begin

          HTTPIPCLog(
            Format(
              'JSON parse error: %s',
              [E.Message]
            ),
            LM_Error
          );


          AResponse :=
            HTTPIPCCreateErrorResponse(
              '',
              Format(
                'Invalid JSON: %s',
                [E.Message]
              )
            );


          Exit;

        end;

      end;


      if Root = nil then
      begin

        AResponse :=
          HTTPIPCCreateErrorResponse(
            '',
            'JSON root object expected'
          );

        Exit;

      end;


      {=====================================================================}
      { ID }
      {=====================================================================}

      CommandID :=
        Root.Get(
          'id',
          ''
        );


      if CommandID = '' then
      begin

        AResponse :=
          HTTPIPCCreateErrorResponse(
            '',
            'Missing command ID'
          );

        Exit;

      end;


      {=====================================================================}
      { COMMAND }
      {=====================================================================}

      CmdName :=
        Root.Get(
          'cmd',
          ''
        );


      if CmdName = '' then
      begin

        AResponse :=
          HTTPIPCCreateErrorResponse(
            CommandID,
            'Missing command name'
          );

        Exit;

      end;


      CmdName :=
        NormalizeCommandName(
          CmdName
        );


      {=====================================================================}
      { FIND HANDLER }
      {=====================================================================}

      if not HTTPIPCGetCommandHandler(
        CmdName,
        Handler
      ) then
      begin

        HTTPIPCLog(
          Format(
            'Unknown HTTP IPC command: %s',
            [CmdName]
          ),
          LM_Error
        );


        AResponse :=
          HTTPIPCCreateErrorResponse(
            CommandID,
            Format(
              'Unknown command: %s',
              [CmdName]
            )
          );


        Exit;

      end;


      {=====================================================================}
      { TOKEN }
      {=====================================================================}

      Token :=
        Root.Get(
          'token',
          ''
        );


      {=====================================================================}
      { CREATE COMMAND }
      {=====================================================================}

      Cmd :=
        THTTPIPCCommand.Create;


      Cmd.ID :=
        CommandID;

      Cmd.CmdName :=
        CmdName;

      Cmd.Token :=
        Token;

      Cmd.Handler :=
        Handler;


      {=====================================================================}
      { ARGS }
      {=====================================================================}

      ArgsData :=
        Root.Find(
          'args'
        );


      if ArgsData <> nil then
      begin

        if not (
          ArgsData is TJSONArray
        ) then
        begin

          AResponse :=
            HTTPIPCCreateErrorResponse(
              CommandID,
              'Command args must be an array'
            );


          Exit;

        end;


        Cmd.Args :=
          TJSONArray(
            ArgsData
          ).Clone as TJSONArray;

      end
      else
      begin

        Cmd.Args :=
          TJSONArray.Create;

      end;


      HTTPIPCLog(
        Format(
          'Command parsed: %s (ID: %s)',
          [
            CmdName,
            CommandID
          ]
        ),
        LM_Debug
      );


    finally

      Root.Free;

    end;

  finally

    Parser.Free;

  end;


  {=========================================================================}
  { ENQUEUE }
  {=========================================================================}

  try

    HTTPIPCCommandQueue.Enqueue(
      Cmd
    );


    Enqueued := True;


    HTTPIPCLog(
      Format(
        'Command enqueued: %s (%s)',
        [
          Cmd.ID,
          Cmd.CmdName
        ]
      ),
      LM_Debug
    );


    {=======================================================================}
    { WAIT }
    {=======================================================================}

    WaitResult :=
      Cmd.WaitFor(
        HTTP_IPC_COMMAND_TIMEOUT
      );


    {=======================================================================}
    { COMPLETED }
    {=======================================================================}

    if WaitResult = wrSignaled then
    begin

      AResponse :=
        HTTPIPCCreateCommandResponse(
          Cmd
        );


      HTTPIPCLog(
        Format(
          'Command completed: %s (%s)',
          [
            Cmd.ID,
            Cmd.CmdName
          ]
        ),
        LM_Debug
      );


      Result := True;

    end


    {=======================================================================}
    { TIMEOUT }
    {=======================================================================}

    else if WaitResult = wrTimeout then
    begin

      HTTPIPCLog(
        Format(
          'Command timeout: %s (%s)',
          [
            Cmd.ID,
            Cmd.CmdName
          ]
        ),
        LM_Error
      );


      AResponse :=
        HTTPIPCCreateErrorResponse(
          Cmd.ID,
          'Command timeout'
        );


      Result := False;

    end


    {=======================================================================}
    { WAIT ERROR }
    {=======================================================================}

    else
    begin

      HTTPIPCLog(
        Format(
          'Command wait error: %s (%s)',
          [
            Cmd.ID,
            Cmd.CmdName
          ]
        ),
        LM_Error
      );


      AResponse :=
        HTTPIPCCreateErrorResponse(
          Cmd.ID,
          'Error waiting for command completion'
        );


      Result := False;

    end;


  except

    on E: Exception do
    begin

      HTTPIPCLog(
        Format(
          'HTTP IPC queue error: %s',
          [E.Message]
        ),
        LM_Error
      );


      AResponse :=
        HTTPIPCCreateErrorResponse(
          CommandID,
          Format(
            'HTTP IPC queue error: %s',
            [E.Message]
          )
        );


      Result := False;

    end;

  end;


  {=========================================================================}
  { RELEASE HTTP OWNER }
  {=========================================================================}

  { ВАЖНО:

    После Enqueue:

      HTTP владеет одной ссылкой.
      Queue владеет второй ссылкой.

    Если HTTP timeout:

      HTTP Release
          |
          +---- Queue продолжает владеть командой.
          |
          +---- главный поток позже выполнит её.

    Поэтому timeout больше НЕ вызывает утечку
    и НЕ требует Cmd := nil.
  }

  if Cmd <> nil then
  begin

    Cmd.Release;
    Cmd := nil;

  end;


end;


{=============================================================================
  PROCESS ONE COMMAND
=============================================================================}

function HTTPIPCProcessNextCommand: Boolean;
var
  Cmd: THTTPIPCCommand;
  StatusText: string;
begin

  Result := False;

  if HTTPIPCCommandQueue = nil then
    Exit;


  Cmd := nil;


  if not HTTPIPCCommandQueue.TryDequeue(
    Cmd
  ) then
    Exit;


  try

    HTTPIPCLog(
      Format(
        'Executing command in main thread: %s (%s)',
        [
          Cmd.ID,
          Cmd.CmdName
        ]
      ),
      LM_Debug
    );


    {=======================================================================}
    { EXECUTE HANDLER }
    {=======================================================================}

    try

      Cmd.ResultText := '';
      Cmd.ErrorText := '';
      Cmd.Success := False;


      if not Assigned(Cmd.Handler) then
      begin

        Cmd.ErrorText :=
          'Command handler is not assigned';

      end
      else
      begin

        Cmd.Success :=
          Cmd.Handler(
            Cmd.Args,
            Cmd.ResultText,
            Cmd.ErrorText
          );

      end;


    except

      on E: Exception do
      begin

        Cmd.Success := False;

        Cmd.ResultText := '';

        Cmd.ErrorText :=
          Format(
            'Exception: %s',
            [E.Message]
          );


        HTTPIPCLog(
          Format(
            'Command exception [%s]: %s',
            [
              Cmd.CmdName,
              E.Message
            ]
          ),
          LM_Error
        );

      end;

    end;

   {=======================================================================}
{ SIGNAL HTTP THREAD }
{=======================================================================}

if Cmd.Completed <> nil then
  Cmd.Completed.SetEvent;


if Cmd.Success then
  StatusText := 'ok'
else
  StatusText := 'error';


HTTPIPCLog(
  Format(
    'Command finished: %s (%s), status=%s',
    [
      Cmd.ID,
      Cmd.CmdName,
      StatusText
    ]
  ),
  LM_Debug
);


    Result := True;


  finally

    {=======================================================================}
    { RELEASE QUEUE OWNER }
    {=======================================================================}

    Cmd.Release;

  end;

end;


{=============================================================================
  PROCESS ALL
=============================================================================}

procedure HTTPIPCProcessPendingCommands;
begin

  while HTTPIPCProcessNextCommand do
  begin
    { Обрабатываем следующую команду }
  end;

end;


{=============================================================================
  INITIALIZATION
=============================================================================}

initialization

  HTTPIPCCommandRegistry :=
    nil;


  HTTPIPCRegistryLock :=
    TCriticalSection.Create;


  HTTPIPCCommandQueue :=
    THTTPIPCCommandQueue.Create;


  ProgramLog.LogOutFormatStr(
    'HTTP IPC dispatcher initialized',
    [],
    LM_Info,
    0
  );


{=============================================================================
  FINALIZATION
=============================================================================}

finalization

  {===========================================================================
    ВАЖНО:

    Очередь должна уничтожаться ДО lock реестра.

    HTTP server к этому моменту должен быть остановлен,
    чтобы новые команды больше не могли поступать.
  ===========================================================================}

  if HTTPIPCCommandQueue <> nil then
  begin

    HTTPIPCCommandQueue.Free;
    HTTPIPCCommandQueue := nil;

  end;


  SetLength(
    HTTPIPCCommandRegistry,
    0
  );


  if HTTPIPCRegistryLock <> nil then
  begin

    HTTPIPCRegistryLock.Free;
    HTTPIPCRegistryLock := nil;

  end;


  ProgramLog.LogOutFormatStr(
    'HTTP IPC dispatcher finalized',
    [],
    LM_Info,
    0
  );

end.

