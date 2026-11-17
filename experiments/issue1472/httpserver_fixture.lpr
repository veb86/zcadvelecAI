{
  HTTP-регрессия #1472: реальные обработчики uzvhttpserver и сервер FPC.
  Автор: OpenAI Codex, 2026-11-17. Зависимости: FCL-Web, сгенерированные include.
  GUI, IPC, widgets и Grist заменены заглушками; транспорт и заголовки настоящие.
}
program httpserver_fixture;
{$mode objfpc}{$H+}

uses
  cthreads, Classes, SysUtils, fphttpserver, httpdefs;

const
  LM_Info = 0;
  LM_Error = 1;
  TMWOHistoryOut = 0;

type
  { В FPC 3.2.2 Address доступен только наследникам сервера. }
  TLocalHTTPServer = class(TFPHttpServer)
  public
    property Address;
  end;

  TTestUI = class
    procedure TextMessage(const AMessage: string; AMode: Integer);
  end;

  TZCADHTTPServerManager = class
    procedure Log(const AMessage: string; ALevel: Integer);
    procedure SetCORSHeaders(var AResponse: TFPHTTPConnectionResponse);
    procedure HandleRequest(Sender: TObject; var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
  end;

var
  zcUI: TTestUI;
  Server: TLocalHTTPServer;
  Manager: TZCADHTTPServerManager;

{ Не требует GUI при передаче тела запроса в HTTP-обработчик. }
procedure TTestUI.TextMessage(const AMessage: string; AMode: Integer);
begin
end;

{ Не требует настройки лога ZCAD для проверки HTTP-транспорта. }
procedure TZCADHTTPServerManager.Log(const AMessage: string; ALevel: Integer);
begin
end;

{ Проверяет передачу JSON без изменений и ветку обработки исключения. }
procedure HTTPIPCExecuteJSON(const ARequest: string; out AResponse: string);
begin
  if ARequest = '{"raise":true}' then
    raise Exception.Create('Test IPC failure');
  AResponse := ARequest;
end;

{ Позволяет проверить CORS перед делегированием обработки widgets. }
procedure HTTPWidgetsHandleRequest(var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
begin
  AResponse.Content := '{"handler":"widgets"}';
end;

{ Позволяет проверить CORS перед делегированием обработки Grist. }
procedure HandleGristRequest(var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
begin
  AResponse.Content := '{"handler":"grist"}';
end;

{ Включаются текущие тела методов из исходного файла, без копии реализации. }
{$I cors.inc}
{$I request.inc}

begin
  zcUI := TTestUI.Create;
  Manager := TZCADHTTPServerManager.Create;
  Server := TLocalHTTPServer.Create(nil);
  try
    Server.Address := '127.0.0.1';
    Server.Port := StrToInt(ParamStr(1));
    Server.Threaded := True;
    Server.OnRequest := @Manager.HandleRequest;
    WriteLn('Starting HTTP fixture');
    Flush(Output);
    Server.Active := True;
  finally
    Server.Free;
    Manager.Free;
    zcUI.Free;
  end;
end.
