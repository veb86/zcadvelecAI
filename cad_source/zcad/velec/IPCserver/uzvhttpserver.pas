{
*****************************************************************************
*                                                                           *
*  This file is part of the ZCAD                                            *
*                                                                           *
*****************************************************************************

@author(HTTP Server for ZCAD)
@author(Vladimir Bobrov)
}

{$mode objfpc}{$H+}

unit uzvhttpserver;

{$INCLUDE zengineconfig.inc}

interface

uses
  Classes,
  SysUtils,
  fphttpserver,
  httpdefs,
  fpjson,
  jsonparser,
  uzclog,
  uzccommandsabstract,
  uzccommandsimpl,
  uzbLogTypes,
  uzvhttpipc,
  uzcinterface;

const
  {** Адрес HTTP-сервера по умолчанию }
  HTTP_DEFAULT_HOST = '127.0.0.1';

  {** Порт HTTP-сервера по умолчанию }
  HTTP_DEFAULT_PORT = 5000;

type

    {**
    Поток, в котором работает основной цикл TFPHttpServer.
    }
    TZCADHTTPServerThread = class(TThread)
    private
      FServer: TFPHttpServer;
      FFinished: Boolean;
    protected
      procedure Execute; override;
    public
      constructor Create(AServer: TFPHttpServer);
      property Finished: Boolean read FFinished;
    end;

  {**
    Менеджер HTTP-сервера ZCAD.

    Сервер не знает структуру JSON.
    Он только принимает HTTP POST и передает тело запроса
    зарегистрированному обработчику.
  }
  TZCADHTTPServerManager = class
  private
    FServer: TFPHttpServer;
    FServerThread: TZCADHTTPServerThread;

    FRunning: Boolean;
    FHost: string;
    FPort: Integer;

    procedure CleanupStoppedServer;

    procedure HandleRequest(
      Sender: TObject;
      var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse
    );

    procedure Log(
      const AMessage: string;
      ALogLevel: TLogLevel
    );

  public
    constructor Create;
    destructor Destroy; override;

    function Start(
      const AHost: string = HTTP_DEFAULT_HOST;
      APort: Integer = HTTP_DEFAULT_PORT
    ): Boolean;

    procedure Stop;

    function IsRunning: Boolean;

    property Host: string read FHost;
    property Port: Integer read FPort;
    property Running: Boolean read FRunning;
  end;


var
  {** Глобальный менеджер HTTP-сервера }
  ZCADHTTPServerManager: TZCADHTTPServerManager;


{** Инициализация HTTP-модуля }
procedure ZCADHTTPServerInit;

{** Завершение HTTP-модуля }
procedure ZCADHTTPServerDone;

{** Запуск HTTP-сервера }
function ZCADHTTPServerStart(
  const AHost: string = HTTP_DEFAULT_HOST;
  APort: Integer = HTTP_DEFAULT_PORT
): Boolean;

{** Остановка HTTP-сервера }
procedure ZCADHTTPServerStop;

{** Проверка состояния }
function ZCADHTTPServerIsRunning: Boolean;


{** Команда ZCAD HTTPStart }
function HTTPStart_com(
  const Context: TZCADCommandContext;
  Operands: TCommandOperands
): TCommandResult;


{** Команда ZCAD HTTPStop }
function HTTPStop_com(
  const Context: TZCADCommandContext;
  Operands: TCommandOperands
): TCommandResult;


{** Команда ZCAD HTTPStatus }
function HTTPStatus_com(
  const Context: TZCADCommandContext;
  Operands: TCommandOperands
): TCommandResult;


implementation


{=============================================================================
  TZCADHTTPServerThread
=============================================================================}

constructor TZCADHTTPServerThread.Create(AServer: TFPHttpServer);
begin

  inherited Create(True);

  FreeOnTerminate := false;

  FServer := AServer;
  FFinished := False;
  Start;

end;


procedure TZCADHTTPServerThread.Execute;
var
  Server: TFPHttpServer;
begin

  Server := FServer;


  ProgramLog.LogOutFormatStr(
    '[HTTP] Server thread started',
    [],
    LM_Info,
    0
  );


  try

    ProgramLog.LogOutFormatStr(
      '[HTTP] Entering FServer.Active := True',
      [],
      LM_Info,
      0
    );


    Server.Active := True;


  except

    on E: Exception do
    begin

      ProgramLog.LogOutFormatStr(
        '[HTTP] Server thread exception: %s',
        [E.Message],
        LM_Error,
        0
      );

    end;

  end;


  ProgramLog.LogOutFormatStr(
    '[HTTP] Server thread finished',
    [],
    LM_Info,
    0
  );


  {---------------------------------------------------------------------------
    HTTP-сервер больше не нужен.

    FServer.Active := True уже завершился,
    поэтому теперь сервер можно освобождать.
  ---------------------------------------------------------------------------}
  FFinished := True;

end;



{=============================================================================
  TZCADHTTPServerManager
=============================================================================}

constructor TZCADHTTPServerManager.Create;
begin
  inherited Create;

  FServer := nil;
  FServerThread := nil;
  FRunning := False;

  FHost := HTTP_DEFAULT_HOST;
  FPort := HTTP_DEFAULT_PORT;
end;


destructor TZCADHTTPServerManager.Destroy;
begin
  Stop;

  inherited Destroy;
end;


procedure TZCADHTTPServerManager.Log(
  const AMessage: string;
  ALogLevel: TLogLevel
);
begin
  ProgramLog.LogOutFormatStr(
    '[HTTP] %s',
    [AMessage],
    ALogLevel,
    0
  );
end;

procedure TZCADHTTPServerManager.CleanupStoppedServer;
begin

  if FServerThread = nil then
    Exit;


  if not FServerThread.Finished then
    Exit;


  Log(
    'HTTP server thread finished, cleaning up',
    LM_Debug
  );


  { Поток уже полностью вышел из Execute }

  FServerThread.Free;
  FServerThread := nil;


  { Теперь можно освобождать TFPHttpServer }

  if FServer <> nil then
  begin

    FServer.Free;
    FServer := nil;

  end;

end;


{=============================================================================
  HTTP REQUEST
=============================================================================}

procedure TZCADHTTPServerManager.HandleRequest(
  Sender: TObject;
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse
);
var
  JSONText: string;
  ResponseText: string;
begin

  Log(
    Format(
      'HTTP request: %s %s',
      [ARequest.Method, ARequest.URI]
    ),
    LM_Debug
  );

  {---------------------------------------------------------------------------
    Проверяем endpoint
  ---------------------------------------------------------------------------}

  if ARequest.URI <> '/ipc' then
  begin
    AResponse.Code := 404;
    AResponse.ContentType := 'application/json';
    AResponse.Content :=
      '{"status":"error","error":"Endpoint not found"}';
    Exit;
  end;

  {---------------------------------------------------------------------------
    Разрешаем только POST
  ---------------------------------------------------------------------------}

  if UpperCase(ARequest.Method) <> 'POST' then
  begin
    AResponse.Code := 405;
    AResponse.ContentType := 'application/json';
    AResponse.Content :=
      '{"status":"error","error":"Method not allowed"}';
    Exit;
  end;

  {---------------------------------------------------------------------------
    Получаем JSON из тела HTTP-запроса
  ---------------------------------------------------------------------------}

  JSONText := ARequest.Content;

  if Trim(JSONText) = '' then
  begin
    AResponse.Code := 400;
    AResponse.ContentType := 'application/json';
    AResponse.Content :=
      '{"status":"error","error":"Empty request body"}';
    Exit;
  end;

  {---------------------------------------------------------------------------
    Передаём JSON в независимый IPC-модуль
  ---------------------------------------------------------------------------}

  ResponseText := '';

  try
    HTTPIPCExecuteJSON(JSONText, ResponseText);

    AResponse.Code := 200;
    AResponse.ContentType := 'application/json';
    AResponse.Content := ResponseText;

  except
    on E: Exception do
    begin
      Log(
        Format(
          'HTTP IPC error: %s',
          [E.Message]
        ),
        LM_Error
      );

      AResponse.Code := 500;
      AResponse.ContentType := 'application/json';
      AResponse.Content :=
        '{"status":"error","error":"Internal server error"}';
    end;
  end;

end;


{=============================================================================
  START
=============================================================================}

function TZCADHTTPServerManager.Start(
  const AHost: string;
  APort: Integer
): Boolean;
begin

  Result := False;


  CleanupStoppedServer;


  if FServerThread <> nil then
  begin

    if not FServerThread.Finished then
    begin

      Log(
        'Previous HTTP server is still stopping',
        LM_Info
      );

      Exit(False);

    end;

  end;


  if FRunning then
  begin

    Log(
      'HTTP server already running',
      LM_Info
    );

    Exit(True);
  end;


  FHost := AHost;
  FPort := APort;


  try

    {-----------------------------------------------------------------------
      Создаём сервер
    -----------------------------------------------------------------------}

    FServer := TFPHttpServer.Create(nil);


    {-----------------------------------------------------------------------
      Настройки
    -----------------------------------------------------------------------}

    FServer.Address := FHost;
    FServer.Port := FPort;

    FServer.Threaded := True;

    FServer.OnRequest := @HandleRequest;


    {-----------------------------------------------------------------------
      Запускаем
    -----------------------------------------------------------------------}

    FServerThread := TZCADHTTPServerThread.Create(FServer);

    FRunning := True;


    Log(
      Format(
        'HTTP server started on %s:%d',
        [FHost, FPort]
      ),
      LM_Info
    );


    Result := True;


  except

    on E: Exception do
    begin

      Log(
        Format(
          'Failed to start HTTP server: %s',
          [E.Message]
        ),
        LM_Error
      );


      if FServer <> nil then
      begin

        try
          FServer.Active := False;
        except
        end;

      end;


      FRunning := False;
      Result := False;

    end;

  end;

end;


{=============================================================================
  STOP
=============================================================================}

procedure TZCADHTTPServerManager.Stop;
begin

  if not FRunning then
    Exit;


  Log(
    'Stopping HTTP server',
    LM_Info
  );


  {---------------------------------------------------------------------------
    Запрашиваем остановку сервера.

    ВАЖНО:
    FServer и FServerThread здесь НЕ обнуляем.
    Они нужны менеджеру до полного завершения потока.
  ---------------------------------------------------------------------------}

  try

    if FServer <> nil then
      FServer.Active := False;

  except

    on E: Exception do
      Log(
        Format(
          'HTTP server stop error: %s',
          [E.Message]
        ),
        LM_Error
      );

  end;


  { Сервер больше считается запущенным }

  FRunning := False;


  Log(
    'HTTP server stop requested',
    LM_Info
  );

end;


{=============================================================================
  STATUS
=============================================================================}

function TZCADHTTPServerManager.IsRunning: Boolean;
begin
  Result := FRunning;
end;


{=============================================================================
  GLOBAL FUNCTIONS
=============================================================================}

procedure ZCADHTTPServerInit;
begin

  if ZCADHTTPServerManager = nil then
  begin

    ZCADHTTPServerManager :=
      TZCADHTTPServerManager.Create;

    ProgramLog.LogOutFormatStr(
      'HTTP Server module initialized',
      [],
      LM_Info,
      0
    );

  end;

end;


procedure ZCADHTTPServerDone;
begin

  if ZCADHTTPServerManager <> nil then
  begin

    ZCADHTTPServerManager.Free;
    ZCADHTTPServerManager := nil;

    ProgramLog.LogOutFormatStr(
      'HTTP Server module finalized',
      [],
      LM_Info,
      0
    );

  end;

end;


function ZCADHTTPServerStart(
  const AHost: string;
  APort: Integer
): Boolean;
begin

  if ZCADHTTPServerManager = nil then
    ZCADHTTPServerInit;

  Result :=
    ZCADHTTPServerManager.Start(
      AHost,
      APort
    );

end;


procedure ZCADHTTPServerStop;
begin

  if ZCADHTTPServerManager <> nil then
    ZCADHTTPServerManager.Stop;

end;


function ZCADHTTPServerIsRunning: Boolean;
begin

  Result :=
    (ZCADHTTPServerManager <> nil) and
    ZCADHTTPServerManager.IsRunning;

end;


{=============================================================================
  ZCAD COMMAND: HTTPStart
=============================================================================}

function HTTPStart_com(
  const Context: TZCADCommandContext;
  Operands: TCommandOperands
): TCommandResult;
var
  Host: string;
  Port: Integer;
  Params: TStringList;
begin

  Result := cmd_error;


  Host := HTTP_DEFAULT_HOST;
  Port := HTTP_DEFAULT_PORT;


  {---------------------------------------------------------------------------
    Разбор:

      HTTPStart
      HTTPStart 127.0.0.1
      HTTPStart 127.0.0.1 8888
  ---------------------------------------------------------------------------}

  if Operands <> '' then
  begin

    Params := TStringList.Create;

    try

      Params.Delimiter := ' ';
      Params.DelimitedText := Operands;


      if Params.Count >= 1 then
        Host := Params[0];


      if Params.Count >= 2 then
        Port :=
          StrToIntDef(
            Params[1],
            HTTP_DEFAULT_PORT
          );

    finally

      Params.Free;

    end;

  end;


  if ZCADHTTPServerStart(Host, Port) then
  begin

    zcUI.TextMessage(
      Format(
        'HTTP server started on %s:%d',
        [Host, Port]
      ),
      TMWOHistoryOut
    );

    Result := cmd_ok;

  end
  else
  begin

    zcUI.TextMessage(
      'Failed to start HTTP server',
      TMWOShowError
    );

  end;

end;


{=============================================================================
  ZCAD COMMAND: HTTPStop
=============================================================================}

function HTTPStop_com(
  const Context: TZCADCommandContext;
  Operands: TCommandOperands
): TCommandResult;
begin

  if ZCADHTTPServerIsRunning then
  begin

    ZCADHTTPServerStop;


    zcUI.TextMessage(
  'HTTP server stop requested',
  TMWOHistoryOut
);


    Result := cmd_ok;

  end
  else
  begin

    zcUI.TextMessage(
      'HTTP server is not running',
      TMWOHistoryOut
    );


    Result := cmd_error;

  end;

end;


{=============================================================================
  ZCAD COMMAND: HTTPStatus
=============================================================================}

function HTTPStatus_com(
  const Context: TZCADCommandContext;
  Operands: TCommandOperands
): TCommandResult;
begin

  if ZCADHTTPServerIsRunning then
  begin

    zcUI.TextMessage(
      Format(
        'HTTP server is running on %s:%d',
        [
          ZCADHTTPServerManager.Host,
          ZCADHTTPServerManager.Port
        ]
      ),
      TMWOHistoryOut
    );
  end
  else
  begin

    zcUI.TextMessage(
      'HTTP server is not running',
      TMWOHistoryOut
    );
  end;


  Result := cmd_ok;

end;


{=============================================================================
  INITIALIZATION
=============================================================================}

initialization

  ProgramLog.LogOutFormatStr(
    'Unit "%s" initialization',
    [{$INCLUDE %FILE%}],
    LM_Info,
    UnitsInitializeLMId
  );


  {---------------------------------------------------------------------------
    Регистрация команд ZCAD

    Сервер при этом НЕ запускается!
  ---------------------------------------------------------------------------}

  CreateZCADCommand(
    @HTTPStart_com,
    'HTTPStart',
    CADWG,
    0
  );


  CreateZCADCommand(
    @HTTPStop_com,
    'HTTPStop',
    CADWG,
    0
  );


  CreateZCADCommand(
    @HTTPStatus_com,
    'HTTPStatus',
    CADWG,
    0
  );


  {---------------------------------------------------------------------------
    Только создаём менеджер.
    HTTP-сокет НЕ открывается.
  ---------------------------------------------------------------------------}

  ZCADHTTPServerInit;


finalization

  ZCADHTTPServerDone;


  ProgramLog.LogOutFormatStr(
    'Unit "%s" finalization',
    [{$INCLUDE %FILE%}],
    LM_Info,
    UnitsFinalizeLMId
  );

end.
