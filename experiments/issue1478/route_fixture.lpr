{
  HTTP-регрессия #1478: настоящий маршрут /api/export_selection.

  Используются реальные HandleRequest и SetCORSHeaders из uzvhttpserver
  (вставляются тестом как include), реальная очередь uzvhttpipc,
  реальные модули uzvhttpexportroute и uzvhttpexportselection.
  Заменены только журнал (stubs/uzclog.pas) и сама запись DXF: вместо
  чертежа ZCAD обработчик пишет небольшой файл по тем же правилам пути.

  HTTP-сервер работает в отдельном потоке, главный поток крутит очередь
  так же, как TzcMainForm.idle в ZCAD.
}
program route_fixture;

{$mode objfpc}{$H+}

uses
  cthreads, Classes, SysUtils, fpjson, fphttpserver, httpdefs,
  uzclog, uzvhttpipc, uzvhttpexportselection, uzvhttpexportroute;

const
  TMWOHistoryOut = 0;
  { Пауза главного цикла между проходами очереди, мс }
  PUMP_INTERVAL_MS = 5;
  { Длительность «медленной» выгрузки для проверки TIMEOUT, мс }
  SLOW_EXPORT_MS = 3000;
  { Маркеры в имени файла, включающие тестовые ветки обработчика }
  SLOW_MARKER = 'slow';
  PLAIN_ERROR_MARKER = 'plain-error';
  { Содержимое тестового DXF: одна LINE на слое WALLS }
  FAKE_DXF = '0'#10'SECTION'#10'2'#10'ENTITIES'#10'0'#10'LINE'#10'8'#10'WALLS'#10 +
    '10'#10'0.0'#10'20'#10'0.0'#10'11'#10'10.0'#10'21'#10'5.0'#10 +
    '0'#10'ENDSEC'#10'0'#10'EOF'#10;

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

  { Поток HTTP-сервера: Active блокирует поток до остановки }
  TServerThread = class(TThread)
  protected
    procedure Execute; override;
  end;

var
  zcUI: TTestUI;
  Server: TLocalHTTPServer;
  Manager: TZCADHTTPServerManager;
  ServerThread: TServerThread;

{ Не требует GUI при передаче тела запроса в HTTP-обработчик. }
procedure TTestUI.TextMessage(const AMessage: string; AMode: Integer);
begin
end;

{ Не требует настройки лога ZCAD для проверки HTTP-транспорта. }
procedure TZCADHTTPServerManager.Log(const AMessage: string; ALevel: Integer);
begin
end;

{ Запускает HTTP-сервер в фоновом потоке }
procedure TServerThread.Execute;
begin
  Server.Active := True;
end;

{ Заглушка маршрутов widgets, не относящихся к проверке. }
procedure HTTPWidgetsHandleRequest(var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
begin
  AResponse.Content := '{"handler":"widgets"}';
end;

{ Заглушка маршрутов Grist, не относящихся к проверке. }
procedure HandleGristRequest(var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
begin
  AResponse.Content := '{"handler":"grist"}';
end;

{ Читает переменные окружения так же, как модуль команды ZCAD }
function ReadEnv(const AName: string): string;
begin
  Result := GetEnvironmentVariable(AName);
end;

{ Поставщик таймаута, как в uzvhttpcmdexportselection }
function FakeExportTimeout: Cardinal;
begin
  Result := ExportTimeoutMs(ExportConfigLoad(@ReadEnv, GetTempDir(False)));
end;

{ Записывает тестовый DXF и проверяет правила overwrite, как ZCAD }
procedure WriteFakeDXF(const APath: string; AOverwrite: Boolean);
var
  Stream: TFileStream;
begin
  if FileExists(APath) and not AOverwrite then
    raise EExportSelection.CreateCode(ERR_FILE_EXISTS, 'File already exists: ' + APath);
  if not ForceDirectories(ExtractFileDir(APath)) then
    raise EExportSelection.CreateCode(ERR_PATH_NOT_WRITABLE, 'Cannot create ' + APath);
  Stream := TFileStream.Create(APath, fmCreate);
  try
    Stream.WriteBuffer(FAKE_DXF[1], Length(FAKE_DXF));
  finally
    Stream.Free;
  end;
end;

{ Формирует итог выгрузки одной тестовой линии }
function FakeResult(const APath: string; const ARequest: TExportRequest): TExportResult;
begin
  Result := Default(TExportResult);
  Result.Path := APath;
  Result.Scope := ARequest.Scope;
  Result.Units := ExportUnitsName(4);
  Result.EntityCount := 1;
  Result.HasBBox := True;
  Result.BBox.MaxX := 10;
  Result.BBox.MaxY := 5;
  Result.IncludeXData := ARequest.IncludeXData;
  Result.FileSize := Length(FAKE_DXF);
end;

{ Выполняет выгрузку без чертежа: путь, ошибки и ответ как у ZCAD }
function RunFakeExport(AArgs: TJSONArray; out AResult, AError: string): Boolean;
var
  Request: TExportRequest;
  Path: string;
begin
  if GetCurrentThreadId <> MainThreadID then
    raise EExportSelection.CreateCode(ERR_INTERNAL_ERROR, 'Export left the main thread');
  Request := ExportRequestParse(AArgs);
  Path := ExportResolvePath(Request, ExportConfigLoad(@ReadEnv, GetTempDir(False)));
  if Pos(SLOW_MARKER, ExtractFileName(Path)) > 0 then
    Sleep(SLOW_EXPORT_MS);
  if Pos(PLAIN_ERROR_MARKER, ExtractFileName(Path)) > 0 then
  begin
    AError := 'plain text failure';
    Exit(False);
  end;
  WriteFakeDXF(Path, Request.Overwrite);
  AResult := ExportSuccessJSON(FakeResult(Path, Request));
  Result := True;
end;

{ Обработчик EXPORT_SELECTION с тем же контрактом ошибок, что у ZCAD }
function FakeExportCmd(AArgs: TJSONArray; out AResult, AError: string): Boolean;
begin
  AResult := '';
  AError := '';
  try
    Result := RunFakeExport(AArgs, AResult, AError);
  except
    on E: EExportSelection do
    begin
      AError := ExportErrorJSON(E.Code, E.Message);
      Result := False;
    end;
  end;
end;

{ Включаются текущие тела методов из исходного файла, без копии реализации. }
{$I cors.inc}
{$I request.inc}

begin
  { ZCAD работает с UTF-8 RTL (LazUTF8): повторяем это без LCL }
  DefaultSystemCodePage := CP_UTF8;
  SetMultiByteConversionCodePage(CP_UTF8);
  RegisterHTTPIPCCommand(EXPORT_IPC_COMMAND, @FakeExportCmd);
  RegisterHTTPIPCCommandTimeout(EXPORT_IPC_COMMAND, @FakeExportTimeout);
  zcUI := TTestUI.Create;
  Manager := TZCADHTTPServerManager.Create;
  Server := TLocalHTTPServer.Create(nil);
  Server.Address := '127.0.0.1';
  Server.Port := StrToInt(ParamStr(1));
  Server.Threaded := True;
  Server.OnRequest := @Manager.HandleRequest;
  ServerThread := TServerThread.Create(False);
  WriteLn('Starting HTTP fixture');
  Flush(Output);
  { Главный поток обслуживает очередь, пока тест не завершит процесс }
  while True do
  begin
    HTTPIPCProcessPendingCommands;
    Sleep(PUMP_INTERVAL_MS);
  end;
end.
