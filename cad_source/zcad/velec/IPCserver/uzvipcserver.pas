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
@author(IPC Server for ZCAD)
@author(Vladimir Bobrov)
}
{$mode objfpc}{$H+}

{**IPC-сервер для удаленного управления ZCAD через TCP + JSON}
unit uzvipcserver;

{$INCLUDE zengineconfig.inc}

interface

uses
  Classes, SysUtils, Sockets, ssockets, fpjson, jsonparser, syncobjs,
  uzegeometrytypes, uzegeometry, uzeentline, uzeentity, uzeentityfactory,
  uzcdrawings, uzccommandsmanager, uzccommandsimpl, uzccommandsabstract,
  uzcinterface, uzclog, uzcutils, uzcsysvars, uzeconsts, uzcstrconsts,
  uzeentdevice, uzeentblockinsert, uzeentlwpolyline, uzeentpolyline,
  uzeentcircle, uzeentarc, uzeenttext, uzeentmtext,
  uzeffdxf, uzeffdxfsupport, uzbpaths, uzcFileStructure, uzbLogTypes,
  uzeTypes;

const
  {** Порт по умолчанию для IPC-сервера }
  IPC_DEFAULT_PORT = 7777;
  {** Хост по умолчанию (только localhost) }
  IPC_DEFAULT_HOST = '127.0.0.1';
  {** Таймаут выполнения команды (мс) }
  IPC_COMMAND_TIMEOUT = 30000;
  {** Максимальный размер JSON-запроса }
  IPC_MAX_REQUEST_SIZE = 65536;

type
  {** Статус команды }
  TIPCCommandStatus = (csIdle, csBusy);

  {** Результат выполнения команды }
  TIPCCommandResult = record
    Status: string;
    Result: string;
    Error: string;
  end;

  {** Тип команды IPC }
  TIPCCommandType = (ictPing, ictSave, ictExport, ictLine, ictCircle,
                     ictArc, ictPolyline, ictText, ictMText, ictBlockInsert,
                     ictBeginBatch, ictEndBatch, ictUnknown);

  {** Запись команды в очереди }
  PIPCCommand = ^TIPCCommand;
  TIPCCommand = record
    ID: string;
    CmdType: TIPCCommandType;
    Token: string;
    Args: TJSONArray;
    Response: TJSONObject;
    Completed: TEvent;
  end;

  {** Очередь команд }
  TIPCCommandQueue = class
  private
    FQueue: TList;
    FStatus: TIPCCommandStatus;
    FCriticalSection: TCriticalSection;
    FCurrentCommand: PIPCCommand;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Enqueue(ACmd: PIPCCommand);
    function Dequeue: PIPCCommand;
    function IsEmpty: Boolean;
    procedure SetStatus(AStatus: TIPCCommandStatus);
    function GetStatus: TIPCCommandStatus;
    procedure Clear;
  end;

  {** Поток IPC-сервера }
  TIPCServerThread = class(TThread)
  private
    FHost: string;
    FPort: Integer;
    FToken: string;
    FServerSocket: TSocket;
    FRunning: Boolean;
    FDebugMode: Boolean;
    procedure Log(const AMessage: string; ALogLevel: TLogLevel);
    procedure ProcessClient(ASocket: TSocket);
    function ParseCommand(const AJSON: string; out ACmd: PIPCCommand): Boolean;
    function ExecuteCommand(ACmd: PIPCCommand): TIPCCommandResult;
    function GetCommandType(const ACmdName: string): TIPCCommandType;
    function ValidateToken(const AToken: string): Boolean;
    function SendResponse(ASocket: TSocket; AResponse: TJSONObject): Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(const AHost: string; APort: Integer; const AToken: string);
    destructor Destroy; override;
    procedure Stop;
    property DebugMode: Boolean read FDebugMode write FDebugMode;
  end;

  {** Менеджер IPC-сервера }
  TIPCServerManager = class
  private
    FServerThread: TIPCServerThread;
    FEnabled: Boolean;
    FHost: string;
    FPort: Integer;
    FToken: string;
    FDebugMode: Boolean;
    FWhiteList: TStringList;
    procedure Log(const AMessage: string; ALogLevel: TLogLevel);
  public
    constructor Create;
    destructor Destroy; override;
    function Start(const AHost: string = IPC_DEFAULT_HOST;
                   APort: Integer = IPC_DEFAULT_PORT;
                   const AToken: string = ''): Boolean;
    procedure Stop;
    function IsRunning: Boolean;
    procedure SetDebugMode(AEnabled: Boolean);
    procedure AddToWhiteList(const ACommand: string);
    procedure RemoveFromWhiteList(const ACommand: string);
    function IsCommandAllowed(const ACommand: string): Boolean;
    property Enabled: Boolean read FEnabled;
    property Host: string read FHost;
    property Port: Integer read FPort;
  end;

var
  {** Глобальный экземпляр менеджера IPC-сервера }
  IPCServerManager: TIPCServerManager;
  {** Очередь команд для выполнения в главном потоке }
  IPCCommandQueue: TIPCCommandQueue;

{** Инициализация IPC-сервера }
procedure IPCServerInit;
{** Завершение работы IPC-сервера }
procedure IPCServerDone;
{** Запуск IPC-сервера }
function IPCServerStart(const AHost: string = IPC_DEFAULT_HOST;
                        APort: Integer = IPC_DEFAULT_PORT;
                        const AToken: string = ''): Boolean;
{** Остановка IPC-сервера }
procedure IPCServerStop;
{** Проверка статуса IPC-сервера }
function IPCServerIsRunning: Boolean;

{** Команда запуска IPC-сервера }
function IPCStart_com(const Context: TZCADCommandContext;
  Operands: TCommandOperands): TCommandResult;
{** Команда остановки IPC-сервера }
function IPCStop_com(const Context: TZCADCommandContext;
  Operands: TCommandOperands): TCommandResult;
{** Команда статуса IPC-сервера }
function IPCStatus_com(const Context: TZCADCommandContext;
  Operands: TCommandOperands): TCommandResult;

implementation

{=== Вспомогательные функции ===}

function JSONToString(AJSON: TJSONObject): string;
begin
  Result := AJSON.AsJSON;
end;

function CreateResponse(const AID, AStatus, AResult, AError: string): TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('id', AID);
  Result.Add('status', AStatus);
  if AResult <> '' then
    Result.Add('result', AResult);
  if AError <> '' then
    Result.Add('error', AError);
end;

{=== TIPCCommandQueue ===}

constructor TIPCCommandQueue.Create;
begin
  inherited;
  FQueue := TList.Create;
  FCriticalSection := TCriticalSection.Create;
  FStatus := csIdle;
  FCurrentCommand := nil;
end;

destructor TIPCCommandQueue.Destroy;
begin
  Clear;
  FQueue.Free;
  FCriticalSection.Free;
  inherited;
end;

procedure TIPCCommandQueue.Enqueue(ACmd: PIPCCommand);
begin
  FCriticalSection.Enter;
  try
    FQueue.Add(ACmd);
  finally
    FCriticalSection.Leave;
  end;
end;

function TIPCCommandQueue.Dequeue: PIPCCommand;
begin
  Result := nil;
  FCriticalSection.Enter;
  try
    if FQueue.Count > 0 then
    begin
      Result := PIPCCommand(FQueue[0]);
      FQueue.Delete(0);
      FCurrentCommand := Result;
    end;
  finally
    FCriticalSection.Leave;
  end;
end;

function TIPCCommandQueue.IsEmpty: Boolean;
begin
  FCriticalSection.Enter;
  try
    Result := FQueue.Count = 0;
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TIPCCommandQueue.SetStatus(AStatus: TIPCCommandStatus);
begin
  FCriticalSection.Enter;
  try
    FStatus := AStatus;
  finally
    FCriticalSection.Leave;
  end;
end;

function TIPCCommandQueue.GetStatus: TIPCCommandStatus;
begin
  FCriticalSection.Enter;
  try
    Result := FStatus;
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TIPCCommandQueue.Clear;
var
  I: Integer;
  Cmd: PIPCCommand;
begin
  FCriticalSection.Enter;
  try
    for I := 0 to FQueue.Count - 1 do
    begin
      Cmd := PIPCCommand(FQueue[I]);
      if Cmd^.Response <> nil then
        Cmd^.Response.Free;
      if Cmd^.Args <> nil then
        Cmd^.Args.Free;
      Cmd^.Completed.Free;
      Dispose(Cmd);
    end;
    FQueue.Clear;
  finally
    FCriticalSection.Leave;
  end;
end;

{=== TIPCServerThread ===}

constructor TIPCServerThread.Create(const AHost: string; APort: Integer;
  const AToken: string);
begin
  inherited Create(True);
  FHost := AHost;
  FPort := APort;
  FToken := AToken;
  FDebugMode := False;
  FRunning := False;
  FServerSocket := 0;
  FreeOnTerminate := False;
end;

destructor TIPCServerThread.Destroy;
begin
  Stop;
  inherited;
end;

procedure TIPCServerThread.Log(const AMessage: string; ALogLevel: TLogLevel);
begin
  if FDebugMode or (ALogLevel <> LM_Debug) then
  begin
    zcUI.TextMessage('[IPC] ' + AMessage, TMWOHistoryOut);
    ProgramLog.LogOutFormatStr('[IPC] %s', [AMessage], ALogLevel, 0);
  end;
end;

function TIPCServerThread.ValidateToken(const AToken: string): Boolean;
begin
  {** Если токен не задан - проверка отключена }
  if FToken = '' then
    Exit(True);
  Result := AToken = FToken;
end;

function TIPCServerThread.GetCommandType(const ACmdName: string): TIPCCommandType;
begin
  if SameText(ACmdName, 'PING') then
    Result := ictPing
  else if SameText(ACmdName, 'SAVE') then
    Result := ictSave
  else if SameText(ACmdName, 'EXPORT') then
    Result := ictExport
  else if SameText(ACmdName, 'LINE') then
    Result := ictLine
  else if SameText(ACmdName, 'CIRCLE') then
    Result := ictCircle
  else if SameText(ACmdName, 'ARC') then
    Result := ictArc
  else if SameText(ACmdName, 'POLYLINE') then
    Result := ictPolyline
  else if SameText(ACmdName, 'TEXT') then
    Result := ictText
  else if SameText(ACmdName, 'MTEXT') then
    Result := ictMText
  else if SameText(ACmdName, 'BLOCKINSERT') then
    Result := ictBlockInsert
  else if SameText(ACmdName, 'BEGIN_BATCH') then
    Result := ictBeginBatch
  else if SameText(ACmdName, 'END_BATCH') then
    Result := ictEndBatch
  else
    Result := ictUnknown;
end;

function TIPCServerThread.ParseCommand(const AJSON: string; out ACmd: PIPCCommand): Boolean;
var
  Parser: TJSONParser;
  Root: TJSONObject;
  CmdName: string;
  Token: string;
  ID: string;
begin
  Result := False;
  ACmd := nil;

  try
    Parser := TJSONParser.Create(AJSON);
    try
      Root := Parser.Parse as TJSONObject;
      if Root = nil then
      begin
        Log('Failed to parse JSON', LM_Info);
        Exit;
      end;

      try
        {** Проверка токена }
        Token := Root.Get('token', '');
        if not ValidateToken(Token) then
        begin
          Log('Invalid token', LM_Info);
          Exit;
        end;

        {** Получение ID команды }
        ID := Root.Get('id', '');
        if ID = '' then
        begin
          Log('Missing command ID', LM_Info);
          Exit;
        end;

        {** Получение имени команды }
        CmdName := Root.Get('cmd', '');
        if CmdName = '' then
        begin
          Log('Missing command name', LM_Info);
          Exit;
        end;

        {** Создание записи команды }
        New(ACmd);
        ACmd^.ID := ID;
        ACmd^.CmdType := GetCommandType(CmdName);
        ACmd^.Token := Token;
        ACmd^.Completed := TEvent.Create(nil, True, False, '');

        {** Получение аргументов }
        if Root.Find('args') <> nil then
          ACmd^.Args := Root.Arrays['args'].Clone as TJSONArray
        else
          ACmd^.Args := TJSONArray.Create;

        ACmd^.Response := nil;
        Result := True;

        Log(Format('Parsed command: %s (ID: %s)', [CmdName, ID]), LM_Debug);
      finally
        Root.Free;
      end;
    finally
      Parser.Free;
    end;
  except
    on E: Exception do
    begin
      Log(Format('Error parsing command: %s', [E.Message]), LM_Error);
      if ACmd <> nil then
      begin
        if ACmd^.Args <> nil then
          ACmd^.Args.Free;
        ACmd^.Completed.Free;
        Dispose(ACmd);
        ACmd := nil;
      end;
    end;
  end;
end;

function TIPCServerThread.ExecuteCommand(ACmd: PIPCCommand): TIPCCommandResult;

  function GetStringArg(AIndex: Integer; const ADefault: string = ''): string;
  begin
    if (ACmd^.Args <> nil) and (AIndex < ACmd^.Args.Count) then
      Result := ACmd^.Args.Items[AIndex].AsString
    else
      Result := ADefault;
  end;

  function GetFloatArg(AIndex: Integer; ADefault: Double = 0): Double;
  begin
    if (ACmd^.Args <> nil) and (AIndex < ACmd^.Args.Count) then
      Result := ACmd^.Args.Items[AIndex].AsFloat
    else
      Result := ADefault;
  end;

  function GetIntArg(AIndex: Integer; ADefault: Integer = 0): Integer;
  begin
    if (ACmd^.Args <> nil) and (AIndex < ACmd^.Args.Count) then
      Result := ACmd^.Args.Items[AIndex].AsInteger
    else
      Result := ADefault;
  end;

  procedure ExecuteSave;
  var
    FileName: string;
  begin
    FileName := GetStringArg(0);
    if FileName = '' then
      FileName := drawings.GetCurrentDWG^.GetFileName;

    if FileName = '' then
    begin
      Result.Status := 'error';
      Result.Error := 'No filename specified';
      Exit;
    end;

    try
      {** Используем команду QSave для сохранения }
      commandmanager.ExecuteCommandSilent('QSave', drawings.GetCurrentDWG, drawings.GetCurrentOGLWParam);
      Result.Status := 'ok';
      Result.Result := Format('File saved: %s', [FileName]);
    except
      on E: Exception do
      begin
        Result.Status := 'error';
        Result.Error := Format('Save error: %s', [E.Message]);
      end;
    end;
  end;

  procedure ExecuteExport;
  var
    FileName: string;
    FileExt: string;
  begin
    FileName := GetStringArg(0);
    if FileName = '' then
    begin
      Result.Status := 'error';
      Result.Error := 'No filename specified';
      Exit;
    end;

    FileExt := UpperCase(ExtractFileExt(FileName));

    if FileExt = '.DXF' then
    begin
      try
        {** Используем команду SaveAs для экспорта }
        commandmanager.ExecuteCommandSilent('SaveAs(' + FileName + ')',
          drawings.GetCurrentDWG, drawings.GetCurrentOGLWParam);
        Result.Status := 'ok';
        Result.Result := Format('Exported to: %s', [FileName]);
      except
        on E: Exception do
        begin
          Result.Status := 'error';
          Result.Error := Format('Export error: %s', [E.Message]);
        end;
      end;
    end
    else
    begin
      Result.Status := 'error';
      Result.Error := Format('Unsupported export format: %s', [FileExt]);
    end;
  end;

  procedure ExecuteLine;
  var
    PLine: PGDBObjLine;
    P1, P2: TzePoint3d;
  begin
    if (ACmd^.Args = nil) or (ACmd^.Args.Count < 4) then
    begin
      Result.Status := 'error';
      Result.Error := 'LINE requires 4 arguments: x1 y1 x2 y2';
      Exit;
    end;

    P1.x := GetFloatArg(0);
    P1.y := GetFloatArg(1);
    P1.z := 0;

    P2.x := GetFloatArg(2);
    P2.y := GetFloatArg(3);
    P2.z := 0;

    PLine := AllocEnt(GDBLineID);
    PLine^.init(nil, nil, LnWtByLayer, P1, P2);
    zcSetEntPropFromCurrentDrawingProp(PLine);
    zcAddEntToCurrentDrawingWithUndo(PLine);
    zcRedrawCurrentDrawing;

    Result.Status := 'ok';
    Result.Result := Format('Line created: (%.2f,%.2f)-(%.2f,%.2f)', [P1.x, P1.y, P2.x, P2.y]);
  end;

  procedure ExecuteCircle;
  var
    PCircle: PGDBObjCircle;
    Center: TzePoint3d;
    Radius: Double;
  begin
    if (ACmd^.Args = nil) or (ACmd^.Args.Count < 3) then
    begin
      Result.Status := 'error';
      Result.Error := 'CIRCLE requires 3 arguments: x y radius';
      Exit;
    end;

    Center.x := GetFloatArg(0);
    Center.y := GetFloatArg(1);
    Center.z := 0;
    Radius := GetFloatArg(2);

    PCircle := AllocEnt(GDBCircleID);
    PCircle^.init(nil, nil, LnWtByLayer, Center, Radius);
    zcSetEntPropFromCurrentDrawingProp(PCircle);
    zcAddEntToCurrentDrawingWithUndo(PCircle);
    zcRedrawCurrentDrawing;

    Result.Status := 'ok';
    Result.Result := Format('Circle created: center (%.2f,%.2f), radius %.2f', [Center.x, Center.y, Radius]);
  end;

  procedure ExecuteText;
  var
    PText: PGDBObjText;
    InsertPoint: TzePoint3d;
    TextContent: string;
    Height: Double;
  begin
    if (ACmd^.Args = nil) or (ACmd^.Args.Count < 3) then
    begin
      Result.Status := 'error';
      Result.Error := 'TEXT requires at least 3 arguments: x y text [height]';
      Exit;
    end;

    InsertPoint.x := GetFloatArg(0);
    InsertPoint.y := GetFloatArg(1);
    InsertPoint.z := 0;
    TextContent := GetStringArg(2);
    Height := GetFloatArg(3, 2.5);

    PText := GDBObjText.CreateInstance;
    zcSetEntPropFromCurrentDrawingProp(PText);
    PText^.TXTStyle := drawings.GetCurrentDWG^.GetCurrentTextStyle;
    PText^.Local.P_insert := InsertPoint;
    PText^.Template := TDXFEntsInternalStringType(TextContent);
    PText^.obj_height := Height;
    zcAddEntToCurrentDrawingWithUndo(PText);
    zcRedrawCurrentDrawing;

    Result.Status := 'ok';
    Result.Result := Format('Text created: "%s" at (%.2f,%.2f)', [TextContent, InsertPoint.x, InsertPoint.y]);
  end;

begin
  Result.Status := 'error';
  Result.Result := '';
  Result.Error := '';

  try
    case ACmd^.CmdType of
      ictPing:
        begin
          Result.Status := 'ok';
          Result.Result := 'pong';
        end;

      ictSave:
        ExecuteSave;

      ictExport:
        ExecuteExport;

      ictLine:
        ExecuteLine;

      ictCircle:
        ExecuteCircle;

      ictText:
        ExecuteText;

      ictUnknown:
        begin
          Result.Status := 'error';
          Result.Error := 'Unknown command';
        end;

    else
      begin
        Result.Status := 'error';
        Result.Error := 'Command not implemented';
      end;
    end;
  except
    on E: Exception do
    begin
      Result.Status := 'error';
      Result.Error := Format('Exception: %s', [E.Message]);
      Log(Format('Command execution error: %s', [E.Message]), LM_Error);
    end;
  end;
end;

function TIPCServerThread.SendResponse(ASocket: TSocket; AResponse: TJSONObject): Boolean;
var
  ResponseStr: string;
  BytesSent: Integer;
  ResponseBytes: TBytes;
begin
  Result := False;
  if AResponse = nil then
    Exit;

  try
    ResponseStr := AResponse.AsJSON + #10;
    ResponseBytes := TEncoding.UTF8.GetBytes(ResponseStr);

    BytesSent := fpSend(ASocket, @ResponseBytes[0], Length(ResponseBytes), 0);
    Result := BytesSent = Length(ResponseBytes);

    Log(Format('Response sent: %s', [ResponseStr]), LM_Debug);
  except
    on E: Exception do
      Log(Format('Error sending response: %s', [E.Message]), LM_Error);
  end;
end;

procedure TIPCServerThread.ProcessClient(ASocket: TSocket);
var
  Buffer: array[0..IPC_MAX_REQUEST_SIZE - 1] of Byte;
  BytesRead: Integer;
  RequestStr: string;
  Cmd: PIPCCommand;
  CmdResult: TIPCCommandResult;
  Response: TJSONObject;
  RequestCount: Integer;
begin
  Log('Client connected', LM_Info);
  RequestCount := 0;

  try
    {** Цикл обработки нескольких запросов в одном соединении }
    while FRunning do
    begin
      FillChar(Buffer, SizeOf(Buffer), 0);
      BytesRead := fpRecv(ASocket, @Buffer[0], SizeOf(Buffer) - 1, 0);

      if BytesRead <= 0 then
      begin
        Log('Client disconnected or error reading', LM_Info);
        Exit;
      end;

      SetString(RequestStr, PAnsiChar(@Buffer[0]), BytesRead);
      RequestStr := Trim(RequestStr);

      Log(Format('Received [%d]: %s', [RequestCount, RequestStr]), LM_Debug);

      {** Парсинг команды }
      if not ParseCommand(RequestStr, Cmd) then
      begin
        Response := CreateResponse('', 'error', '', 'Invalid request or token');
        SendResponse(ASocket, Response);
        Response.Free;
        Exit;
      end;

      try
        {** Выполнение команды в главном потоке через очередь }
        IPCCommandQueue.SetStatus(csBusy);
        try
          {** Добавляем команду в очередь }
          IPCCommandQueue.Enqueue(Cmd);

          {** Ждем завершения выполнения команды в главном потоке }
          Cmd^.Completed.WaitFor(IPC_COMMAND_TIMEOUT);

          {** Получаем результат }
          if Cmd^.Response <> nil then
          begin
            SendResponse(ASocket, Cmd^.Response);
          end
          else
          begin
            {** Таймаут или ошибка }
            CmdResult.Status := 'error';
            CmdResult.Error := 'Command timeout or execution error';
            Response := CreateResponse(Cmd^.ID, CmdResult.Status, CmdResult.Result, CmdResult.Error);
            SendResponse(ASocket, Response);
            Response.Free;
          end;
        finally
          IPCCommandQueue.SetStatus(csIdle);
        end;
      finally
        {** Очистка }
        if Cmd^.Response <> nil then
          Cmd^.Response.Free;
        if Cmd^.Args <> nil then
          Cmd^.Args.Free;
        Cmd^.Completed.Free;
        Dispose(Cmd);
      end;

      Inc(RequestCount);
      Log(Format('Request %d processed', [RequestCount]), LM_Debug);
    end;

  except
    on E: Exception do
    begin
      Log(Format('Error processing client: %s', [E.Message]), LM_Error);
      try
        Response := CreateResponse('', 'error', '', E.Message);
        SendResponse(ASocket, Response);
        Response.Free;
      except
      end;
    end;
  end;
  
  {** Закрытие соединения с клиентом }
  CloseSocket(ASocket);
  Log('Client connection closed', LM_Info);
end;

procedure TIPCServerThread.Execute;
var
  Addr: TInetSockAddr;
  ClientSocket: TSocket;
  ClientAddr: TInetSockAddr;
  ClientAddrLen: TSockLen;
begin
  Log(Format('Starting IPC server on %s:%d', [FHost, FPort]), LM_Info);

  {** Создание сокета }
  FServerSocket := fpSocket(AF_INET, SOCK_STREAM, 0);
  if FServerSocket < 0 then
  begin
    Log('Failed to create socket', LM_Info);
    Exit;
  end;

  try
    {** Настройка адреса }
    Addr.sin_family := AF_INET;
    Addr.sin_port := htons(FPort);
    Addr.sin_addr.s_addr := StrToNetAddr(FHost).s_addr;

    {** Привязка сокета }
    if fpBind(FServerSocket, @Addr, SizeOf(Addr)) <> 0 then
    begin
      Log(Format('Failed to bind socket: %s', [SysErrorMessage(SocketError)]), LM_Info);
      Exit;
    end;

    {** Прослушивание }
    if fpListen(FServerSocket, 5) <> 0 then
    begin
      Log(Format('Failed to listen on socket: %s', [SysErrorMessage(SocketError)]), LM_Info);
      Exit;
    end;

    FRunning := True;
    Log('IPC server started successfully', LM_Info);

    {** Цикл приема соединений }
    while not Terminated and FRunning do
    begin
      ClientAddrLen := SizeOf(ClientAddr);
      ClientSocket := fpAccept(FServerSocket, @ClientAddr, @ClientAddrLen);

      if ClientSocket < 0 then
      begin
        if SocketError <> EsockEINTR then
          Log(Format('Accept error: %s', [SysErrorMessage(SocketError)]), LM_Info);
        Continue;
      end;

      {** Обработка клиента }
      ProcessClient(ClientSocket);

      {** Закрытие соединения с клиентом }
      CloseSocket(ClientSocket);
    end;

  finally
    if FServerSocket > 0 then
    begin
      CloseSocket(FServerSocket);
      FServerSocket := 0;
    end;
    FRunning := False;
    Log('IPC server stopped',LM_Info);
  end;
end;

procedure TIPCServerThread.Stop;
begin
  FRunning := False;
  if FServerSocket > 0 then
  begin
    CloseSocket(FServerSocket);
    FServerSocket := 0;
  end;
  Terminate;
  WaitFor;
end;

{=== TIPCServerManager ===}

constructor TIPCServerManager.Create;
begin
  inherited;
  FServerThread := nil;
  FEnabled := False;
  FHost := IPC_DEFAULT_HOST;
  FPort := IPC_DEFAULT_PORT;
  FToken := '';
  FDebugMode := False;
  FWhiteList := TStringList.Create;
  FWhiteList.CaseSensitive := False;
end;

destructor TIPCServerManager.Destroy;
begin
  Stop;
  FWhiteList.Free;
  inherited;
end;

procedure TIPCServerManager.Log(const AMessage: string; ALogLevel: TLogLevel);
begin
  if FDebugMode or (ALogLevel <> LM_Debug) then
  begin
    zcUI.TextMessage('[IPC-Mgr] ' + AMessage, TMWOHistoryOut);
    ProgramLog.LogOutFormatStr('[IPC-Mgr] %s', [AMessage], ALogLevel, 0);
  end;
end;

function TIPCServerManager.Start(const AHost: string; APort: Integer;
  const AToken: string): Boolean;
begin
  Result := False;

  if FEnabled then
  begin
    Log('IPC server already running', LM_Info);
    Exit;
  end;

  FHost := AHost;
  FPort := APort;
  FToken := AToken;

  try
    FServerThread := TIPCServerThread.Create(FHost, FPort, FToken);
    FServerThread.DebugMode := FDebugMode;
    FServerThread.Start;

    FEnabled := True;
    Log(Format('IPC server started on %s:%d', [FHost, FPort]),LM_Info);
    Result := True;
  except
    on E: Exception do
    begin
      Log(Format('Failed to start IPC server: %s', [E.Message]), LM_Info);
      FServerThread := nil;
    end;
  end;
end;

procedure TIPCServerManager.Stop;
begin
  if not FEnabled then
    Exit;

  if FServerThread <> nil then
  begin
    FServerThread.Stop;
    FServerThread.Free;
    FServerThread := nil;
  end;

  FEnabled := False;
  Log('IPC server stopped',LM_Info);
end;

function TIPCServerManager.IsRunning: Boolean;
begin
  Result := FEnabled and (FServerThread <> nil) and not FServerThread.Terminated;
end;

procedure TIPCServerManager.SetDebugMode(AEnabled: Boolean);
begin
  FDebugMode := AEnabled;
  if FServerThread <> nil then
    FServerThread.DebugMode := AEnabled;
end;

procedure TIPCServerManager.AddToWhiteList(const ACommand: string);
begin
  if FWhiteList.IndexOf(ACommand) < 0 then
    FWhiteList.Add(ACommand);
end;

procedure TIPCServerManager.RemoveFromWhiteList(const ACommand: string);
var
  Index: Integer;
begin
  Index := FWhiteList.IndexOf(ACommand);
  if Index >= 0 then
    FWhiteList.Delete(Index);
end;

function TIPCServerManager.IsCommandAllowed(const ACommand: string): Boolean;
begin
  {** Если белый список пуст - разрешены все команды }
  if FWhiteList.Count = 0 then
    Exit(True);
  Result := FWhiteList.IndexOf(ACommand) >= 0;
end;

{=== Глобальные функции ===}

procedure IPCServerInit;
begin
  IPCCommandQueue := TIPCCommandQueue.Create;
  IPCServerManager := TIPCServerManager.Create;
  ProgramLog.LogOutFormatStr('IPC Server module initialized', [], LM_Info, 0);
end;

procedure IPCServerDone;
begin
  if IPCServerManager <> nil then
  begin
    IPCServerManager.Free;
    IPCServerManager := nil;
  end;
  if IPCCommandQueue <> nil then
  begin
    IPCCommandQueue.Free;
    IPCCommandQueue := nil;
  end;
  ProgramLog.LogOutFormatStr('IPC Server module finalized', [], LM_Info, 0);
end;

function IPCServerStart(const AHost: string; APort: Integer;
  const AToken: string): Boolean;
begin
  if IPCServerManager = nil then
    IPCServerInit;
  Result := IPCServerManager.Start(AHost, APort, AToken);
end;

procedure IPCServerStop;
begin
  if IPCServerManager <> nil then
    IPCServerManager.Stop;
end;

function IPCServerIsRunning: Boolean;
begin
  Result := (IPCServerManager <> nil) and IPCServerManager.IsRunning;
end;

{=== Команды ZCAD ===}

function IPCStart_com(const Context: TZCADCommandContext;
  Operands: TCommandOperands): TCommandResult;
var
  Host: string;
  Port: Integer;
  Token: string;
  Params: TStringList;
begin
  Result := cmd_error;

  Host := IPC_DEFAULT_HOST;
  Port := IPC_DEFAULT_PORT;
  Token := '';

  {** Парсинг параметров: Host Port Token }
  if Operands <> '' then
  begin
    Params := TStringList.Create;
    try
      Params.Delimiter := ' ';
      Params.DelimitedText := Operands;

      if Params.Count >= 1 then
        Host := Params[0];
      if Params.Count >= 2 then
        Port := StrToIntDef(Params[1], IPC_DEFAULT_PORT);
      if Params.Count >= 3 then
        Token := Params[2];
    finally
      Params.Free;
    end;
  end;

  if IPCServerStart(Host, Port, Token) then
  begin
    zcUI.TextMessage(Format('IPC server started on %s:%d', [Host, Port]), TMWOHistoryOut);
    Result := cmd_ok;
  end
  else
    zcUI.TextMessage('Failed to start IPC server', TMWOShowError);
end;

function IPCStop_com(const Context: TZCADCommandContext;
  Operands: TCommandOperands): TCommandResult;
begin
  if IPCServerIsRunning then
  begin
    IPCServerStop;
    zcUI.TextMessage('IPC server stopped', TMWOHistoryOut);
    Result := cmd_ok;
  end
  else
  begin
    zcUI.TextMessage('IPC server is not running', TMWOHistoryOut);
    Result := cmd_error;
  end;
end;

function IPCStatus_com(const Context: TZCADCommandContext;
  Operands: TCommandOperands): TCommandResult;
begin
  if IPCServerIsRunning then
    zcUI.TextMessage(Format('IPC server is running on %s:%d',
      [IPCServerManager.Host, IPCServerManager.Port]), TMWOHistoryOut)
  else
    zcUI.TextMessage('IPC server is not running', TMWOHistoryOut);
  Result := cmd_ok;
end;

{** Обработчик команд из очереди - вызывается из главного потока }
procedure IPCProcessCommands;
var
  Cmd: PIPCCommand;
  CmdResult: TIPCCommandResult;
  Response: TJSONObject;
begin
  if (IPCCommandQueue = nil) or IPCCommandQueue.IsEmpty then
    Exit;

  {** Получаем команду из очереди }
  Cmd := IPCCommandQueue.Dequeue;
  if Cmd = nil then
    Exit;

  try
    {** Выполняем команду }
    if IPCServerManager <> nil then
    begin
      {** Создаем временный поток для выполнения команды }
      {** На самом деле мы уже в главном потоке, поэтому выполняем напрямую }
      with TIPCServerThread.Create('', 0, '') do
      try
        CmdResult := ExecuteCommand(Cmd);
      finally
        Free;
      end;
    end
    else
    begin
      CmdResult.Status := 'error';
      CmdResult.Error := 'IPC server manager not available';
    end;

    {** Формируем ответ }
    Response := CreateResponse(Cmd^.ID, CmdResult.Status, CmdResult.Result, CmdResult.Error);
    Cmd^.Response := Response;

    {** Сигнализируем о завершении }
    Cmd^.Completed.SetEvent;

  except
    on E: Exception do
    begin
      CmdResult.Status := 'error';
      CmdResult.Error := Format('Exception: %s', [E.Message]);
      Response := CreateResponse(Cmd^.ID, CmdResult.Status, '', CmdResult.Error);
      Cmd^.Response := Response;
      Cmd^.Completed.SetEvent;
    end;
  end;
end;

initialization
  programlog.LogOutFormatStr('Unit "%s" initialization', [{$INCLUDE %FILE%}],
    LM_Info, UnitsInitializeLMId);

  {** Регистрация команд ZCAD }
  CreateZCADCommand(@IPCStart_com, 'IPCStart', CADWG, 0);
  CreateZCADCommand(@IPCStop_com, 'IPCStop', CADWG, 0);
  CreateZCADCommand(@IPCStatus_com, 'IPCStatus', CADWG, 0);

  {** Инициализация IPC-сервера }
  IPCServerInit;

finalization
  {** Завершение работы IPC-сервера }
  IPCServerDone;

  ProgramLog.LogOutFormatStr('Unit "%s" finalization', [{$INCLUDE %FILE%}],
    LM_Info, UnitsFinalizeLMId);

end.
