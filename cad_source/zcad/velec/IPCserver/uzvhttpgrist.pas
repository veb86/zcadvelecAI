{
*****************************************************************************
*                                                                           *
*  This file is part of the ZCAD                                            *
*                                                                           *
*****************************************************************************

@author(HTTP Grist Queue Server for ZCAD)
@author(Vladimir Bobrov)

Описание:

  Модуль для поддержки двустороннего обмена с Grist Widget managerGRIST.

  Реализует механизм GRIST_POLL / GRIST_ACK для передачи команд:
  
    ZCAD → Grist

  Через endpoints:
  
    POST /grist/poll  - получение команд из очереди
    POST /grist/ack   - подтверждение выполнения команды

  ВАЖНО:
  
    Этот модуль НЕ зависит от:
    
      uzvipcserver
      TIPCCommandType
      PIPCCommand
      IPCCommandQueue
    
  Это независимая очередь для направления ZCAD → Grist.
  
  Существующий канал Grist → ZCAD через /ipc не меняется.

  Архитектура:
  
    1. Очередь команд TGristCommandQueue
    2. Команда TGristCommand с уникальным ID
    3. HTTP endpoint /grist/poll
    4. HTTP endpoint /grist/ack

  Пример использования из кода ZCAD:
  
    var
      Args: TJSONObject;
      CmdID: Int64;
    begin
      Args := TJSONObject.Create;
      Args.Add('table', 'Devices');
      Args.Add('recordId', 25);
      Args.Add('field', 'Name');
      Args.Add('value', 'Светильник');
      
      CmdID := QueueGristCommand('SET_GRIST_VALUE', Args);
      
//      { Args больше не нужен - очередь владеет копией }
//    end;
//
//  managerGRIST делает:
//  
//    POST /grist/poll
//    Body: {}
//    
//    Ответ:
//    {
//      "ok": true,
//      "commands": [
//        {
//          "id": 1001,
//          "command": "SET_GRIST_VALUE",
//          "args": {...}
//        }
//      ]
//    }
//    
//  После выполнения:
//  
//    POST /grist/ack
//    Body: {"id": 1001}
//    
//    Ответ:
//    {
//      "ok": true,
//      "id": 1001
//    }
//}

{$mode objfpc}{$H+}

unit uzvhttpgrist;

{$INCLUDE zengineconfig.inc}

interface

uses
  Classes,
  SysUtils,
  SyncObjs,
  fpjson,
  jsonparser,
  uzclog,
  uzbLogTypes,
  fphttpserver,
  httpdefs;

type

  {=========================================================================}
  {                                                                         }
  {  Состояние команды в очереди                                            }
  {                                                                         }
  {=========================================================================}

  TGristCommandState = (
    gcsPending,    { Команда ожидает выдачи }
    gcsInFlight,   { Команда выдана, ждёт ACK }
    gcsAcked       { Команда подтверждена (для логирования) }
  );


  {=========================================================================}
  {                                                                         }
  {  Команда ZCAD → Grist                                                  }
  {                                                                         }
  {=========================================================================}

  TGristCommand = class
  private
    FState: TGristCommandState;
    FLastPollTime: TDateTime;
    FRefCount: LongInt;

    procedure DestroyCommand;

  public
    ID: Int64;
    CommandName: string;
    Args: TJSONObject;

    constructor Create(
      AID: Int64;
      const ACommandName: string;
      AArgs: TJSONObject
    );
    
    destructor Destroy; override;

    procedure AddRef;
    procedure Release;

    property State: TGristCommandState read FState write FState;
    property LastPollTime: TDateTime read FLastPollTime write FLastPollTime;
  end;


  {=========================================================================}
  {                                                                         }
  {  Очередь команд ZCAD → Grist                                           }
  {                                                                         }
  {=========================================================================}

  TGristCommandQueue = class
  private
    FLock: TCriticalSection;
    FItems: TList;
    FNextID: Int64;
    FCommandTimeout: Cardinal;
    FPollBatchSize: Integer;

    function GetCount: Integer;
    function GetIsEmpty: Boolean;

  public
    constructor Create;
    destructor Destroy; override;

    {-----------------------------------------------------------------------}
    { Добавить команду в очередь                                            }
    {-----------------------------------------------------------------------}
    
    function Enqueue(
      const ACommandName: string;
      AArgs: TJSONObject
    ): Int64;

    {-----------------------------------------------------------------------}
    { Получить команды для POLL                                             }
    {-----------------------------------------------------------------------}
    
    function PollCommands(
      out ACommands: TJSONArray
    ): Integer;

    {-----------------------------------------------------------------------}
    { Подтвердить выполнение команды (ACK)                                  }
    {-----------------------------------------------------------------------}
    
    function AckCommand(
      AID: Int64
    ): Boolean;

    {-----------------------------------------------------------------------}
    { Проверить наличие команды по ID                                       }
    {-----------------------------------------------------------------------}
    
    function HasCommand(
      AID: Int64
    ): Boolean;

    {-----------------------------------------------------------------------}
    { Обработать timeout для IN_FLIGHT команд                               }
    {-----------------------------------------------------------------------}
    
    procedure ProcessTimeouts;

    {-----------------------------------------------------------------------}
    { Свойства                                                              }
    {-----------------------------------------------------------------------}
    
    property Count: Integer read GetCount;
    property IsEmpty: Boolean read GetIsEmpty;
    
    property CommandTimeout: Cardinal 
      read FCommandTimeout write FCommandTimeout;
    
    property PollBatchSize: Integer 
      read FPollBatchSize write FPollBatchSize;
  end;


{=============================================================================}
{  РЕЕСТР КОМАНД                                                             }
{=============================================================================}

{**
  Проверить наличие команды в реестре.
  
  Примечание: В текущей реализации реестр команд не используется.
  Все команды передаются напрямую через очередь.
}
function GristCommandRegistered(
  const AName: string
): Boolean;


{**
  Зарегистрировать обработчик команды Grist.
  
  Примечание: В текущей реализации реестр команд не используется.
  Все команды передаются напрямую через очередь.
}
procedure RegisterGristCommandHandler(
  const AName: string;
  AHandler: Pointer
);


{=============================================================================}
{  ОЧЕРЕДЬ                                                                   }
{=============================================================================}

{**
  Глобальная очередь команд ZCAD → Grist.
  
  Обрабатывается HTTP-потоками через /grist/poll и /grist/ack.
}
var
  GristCommandQueue: TGristCommandQueue;


{=============================================================================}
{  HTTP ENDPOINTS                                                            }
{=============================================================================}

{**
  Обработать запрос POST /grist/poll
  
  managerGRIST отправляет:
  
    POST /grist/poll
    Content-Type: application/json
    Body: {}
  
  Ответ:
  
    {
      "ok": true,
      "commands": [...]
    }
}
procedure HandleGristPoll(
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse
);


{**
  Обработать запрос POST /grist/ack
  
  managerGRIST отправляет:
  
    POST /grist/ack
    Content-Type: application/json
    Body: {"id": 1001}
  
  Ответ:
  
    {
      "ok": true,
      "id": 1001
    }
}
procedure HandleGristAck(
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse
);


{**
  Обработать любой запрос к /grist/*
  
  Вызывается из uzvhttpserver.pas
}
procedure HandleGristRequest(
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse
);


{=============================================================================}
{  ПУБЛИЧНЫЕ ФУНКЦИИ ДЛЯ ZCAD                                                }
{=============================================================================}

{**
  Добавить команду в очередь ZCAD → Grist.
  
  Параметры:
    ACommandName - имя команды (например, 'SET_GRIST_VALUE')
    AArgs - параметры команды (TJSONObject)
              Очередь принимает владение объектом!
  
  Возвращает:
    Уникальный ID команды
  
  Пример:
  
    var
      Args: TJSONObject;
      CmdID: Int64;
    begin
      Args := TJSONObject.Create;
      Args.Add('table', 'Devices');
      Args.Add('recordId', 25);
      Args.Add('field', 'Name');
      Args.Add('value', 'Светильник');
      
      CmdID := QueueGristCommand('SET_GRIST_VALUE', Args);
      
      { После вызова очередь владеет Args }
    end;
}
function QueueGristCommand(
  const ACommandName: string;
  AArgs: TJSONObject
): Int64;


{=============================================================================}
{  КОНСТАНТЫ                                                                 }
{=============================================================================}

const
  {** Timeout для неподтверждённых команд (мс) }
  DEFAULT_GRIST_COMMAND_TIMEOUT = 5000;

  {** Максимальное количество команд в одном POLL }
  DEFAULT_GRIST_POLL_BATCH_SIZE = 10;


implementation


procedure RegisterGristCommandHandler(
  const AName: string;
  AHandler: Pointer
);
begin
  { Пустая реализация для совместимости }
end;

function GristCommandRegistered(
  const AName: string
): Boolean;
begin
  Result := True;
end;


{=============================================================================}
{  ЛОГИРОВАНИЕ                                                               }
{=============================================================================}

procedure GristLog(
  const AMessage: string;
  ALogLevel: TLogLevel
);
begin
  ProgramLog.LogOutFormatStr(
    '[HTTP-GRIST] %s',
    [AMessage],
    ALogLevel,
    0
  );
end;


{=============================================================================}
{  TGristCommand                                                             }
{=============================================================================}

constructor TGristCommand.Create(
  AID: Int64;
  const ACommandName: string;
  AArgs: TJSONObject
);
begin
  inherited Create;

  FRefCount := 1;
  
  ID := AID;
  CommandName := ACommandName;
  
  { Очередь принимает владение AArgs }
  Args := AArgs;
  
  FState := gcsPending;
  FLastPollTime := 0;
end;


destructor TGristCommand.Destroy;
begin
  if Args <> nil then
  begin
    Args.Free;
    Args := nil;
  end;

  inherited Destroy;
end;


procedure TGristCommand.DestroyCommand;
begin
  Destroy;
end;


procedure TGristCommand.AddRef;
begin
  InterlockedIncrement(FRefCount);
end;


procedure TGristCommand.Release;
begin
  if InterlockedDecrement(FRefCount) = 0 then
    DestroyCommand;
end;


{=============================================================================}
{  TGristCommandQueue                                                        }
{=============================================================================}

constructor TGristCommandQueue.Create;
begin
  inherited Create;

  FLock := TCriticalSection.Create;
  FItems := TList.Create;
  
  FNextID := 1;
  
  FCommandTimeout := DEFAULT_GRIST_COMMAND_TIMEOUT;
  FPollBatchSize := DEFAULT_GRIST_POLL_BATCH_SIZE;
end;


destructor TGristCommandQueue.Destroy;
var
  I: Integer;
  Cmd: TGristCommand;
begin
  if FLock <> nil then
    FLock.Acquire;

  try
    if FItems <> nil then
    begin
      for I := 0 to FItems.Count - 1 do
      begin
        Cmd := TGristCommand(FItems[I]);
        
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


function TGristCommandQueue.GetCount: Integer;
begin
  FLock.Acquire;
  try
    Result := FItems.Count;
  finally
    FLock.Release;
  end;
end;


function TGristCommandQueue.GetIsEmpty: Boolean;
begin
  FLock.Acquire;
  try
    Result := FItems.Count = 0;
  finally
    FLock.Release;
  end;
end;


function TGristCommandQueue.Enqueue(
  const ACommandName: string;
  AArgs: TJSONObject
): Int64;
var
  Cmd: TGristCommand;
begin
  if AArgs = nil then
  begin
    GristLog(
      'Cannot enqueue command with nil args',
      LM_Error
    );
    
    Result := -1;
    Exit;
  end;

  FLock.Acquire;
  try
    { Создаём команду с уникальным ID }
    Result := FNextID;
    Inc(FNextID);
    
    { Передаём владение AArgs команде }
    Cmd := TGristCommand.Create(Result, ACommandName, AArgs);
    
    { Очередь получает собственную ссылку }
    Cmd.AddRef;
    
    try
      FItems.Add(Cmd);
      
      GristLog(
        Format(
          'Grist command queued: ID=%d Command=%s',
          [Result, ACommandName]
        ),
        LM_Info
      );
      
    except
      { Если Add завершился ошибкой, возвращаем ссылку }
      Cmd.Release;
      raise;
    end;
    
  finally
    FLock.Release;
  end;
end;


function TGristCommandQueue.PollCommands(
  out ACommands: TJSONArray
): Integer;
var
  I: Integer;
  Cmd: TGristCommand;
  CmdObj: TJSONObject;
  NowTime: TDateTime;
  TimeoutSec: Double;
  BatchCount: Integer;
begin
  Result := 0;
  ACommands := TJSONArray.Create;
  
  NowTime := Now;
  TimeoutSec := FCommandTimeout / 1000.0 / 24 / 60 / 60; { конвертация в дни }
  BatchCount := 0;
  
  FLock.Acquire;
  try
    { Сначала проверяем timeout для IN_FLIGHT команд }
    for I := 0 to FItems.Count - 1 do
    begin
      Cmd := TGristCommand(FItems[I]);
      
      if Cmd = nil then
        Continue;
      
      if Cmd.State = gcsInFlight then
      begin
        if (NowTime - Cmd.LastPollTime) > TimeoutSec then
        begin
          GristLog(
            Format(
              'Grist command timeout: ID=%d',
              [Cmd.ID]
            ),
            LM_Info
          );
          
          { Возвращаем команду в PENDING }
          Cmd.State := gcsPending;
          Cmd.LastPollTime := 0;
        end;
      end;
    end;
    
    { Собираем команды для выдачи }
    for I := 0 to FItems.Count - 1 do
    begin
      if BatchCount >= FPollBatchSize then
        Break;
      
      Cmd := TGristCommand(FItems[I]);
      
      if Cmd = nil then
        Continue;
      
      { Выдаём только PENDING команды }
      if Cmd.State <> gcsPending then
        Continue;
      
      { Помечаем как IN_FLIGHT }
      Cmd.State := gcsInFlight;
      Cmd.LastPollTime := NowTime;
      
      { Формируем JSON команды }
      CmdObj := TJSONObject.Create;
      CmdObj.Add('id', Cmd.ID);
      CmdObj.Add('command', Cmd.CommandName);
      CmdObj.Add('args', Cmd.Args.Clone);
      
      ACommands.Add(CmdObj);
      
      Inc(BatchCount);
      Inc(Result);
    end;
    
    if Result > 0 then
    begin
      GristLog(
        Format(
          'Grist poll: returned %d command(s)',
          [Result]
        ),
        LM_Info
      );
    end;
    
  finally
    FLock.Release;
  end;
end;


function TGristCommandQueue.AckCommand(
  AID: Int64
): Boolean;
var
  I: Integer;
  Cmd: TGristCommand;
begin
  Result := False;
  
  FLock.Acquire;
  try
    for I := 0 to FItems.Count - 1 do
    begin
      Cmd := TGristCommand(FItems[I]);
      
      if Cmd = nil then
        Continue;
      
      if Cmd.ID = AID then
      begin
        { Находим команду - удаляем её из очереди }
        FItems.Delete(I);
        
        GristLog(
          Format(
            'Grist ACK: ID=%d',
            [AID]
          ),
          LM_Info
        );
        
        { Освобождаем ссылку очереди }
        Cmd.Release;
        
        Result := True;
        Exit;
      end;
    end;
    
    { Команда не найдена }
    GristLog(
      Format(
        'Grist ACK: unknown command ID=%d',
        [AID]
      ),
      LM_Warning
    );
    
  finally
    FLock.Release;
  end;
end;


function TGristCommandQueue.HasCommand(
  AID: Int64
): Boolean;
var
  I: Integer;
  Cmd: TGristCommand;
begin
  Result := False;
  
  FLock.Acquire;
  try
    for I := 0 to FItems.Count - 1 do
    begin
      Cmd := TGristCommand(FItems[I]);
      
      if Cmd = nil then
        Continue;
      
      if Cmd.ID = AID then
      begin
        Result := True;
        Exit;
      end;
    end;
  finally
    FLock.Release;
  end;
end;


procedure TGristCommandQueue.ProcessTimeouts;
var
  I: Integer;
  Cmd: TGristCommand;
  NowTime: TDateTime;
  TimeoutSec: Double;
begin
  NowTime := Now;
  TimeoutSec := FCommandTimeout / 1000.0 / 24 / 60 / 60;
  
  FLock.Acquire;
  try
    for I := 0 to FItems.Count - 1 do
    begin
      Cmd := TGristCommand(FItems[I]);
      
      if Cmd = nil then
        Continue;
      
      if Cmd.State = gcsInFlight then
      begin
        if (NowTime - Cmd.LastPollTime) > TimeoutSec then
        begin
          GristLog(
            Format(
              'Grist command timeout: ID=%d',
              [Cmd.ID]
            ),
            LM_Info
          );
          
          Cmd.State := gcsPending;
          Cmd.LastPollTime := 0;
        end;
      end;
    end;
  finally
    FLock.Release;
  end;
end;


{=============================================================================}
{  HTTP RESPONSE HELPERS                                                     }
{=============================================================================}

function CreateGristSuccessResponse(
  ACommands: TJSONArray = nil
): string;
var
  Response: TJSONObject;
begin
  Response := TJSONObject.Create;
  try
    Response.Add('ok', True);
    
    if ACommands <> nil then
      Response.Add('commands', ACommands.Clone)
    else
      Response.Add('commands', TJSONArray.Create);
    
    Result := Response.AsJSON;
  finally
    Response.Free;
  end;
end;


function CreateGristErrorResponse(
  const AError: string
): string;
var
  Response: TJSONObject;
begin
  Response := TJSONObject.Create;
  try
    Response.Add('ok', False);
    Response.Add('error', AError);
    
    Result := Response.AsJSON;
  finally
    Response.Free;
  end;
end;


function CreateGristAckResponse(
  AID: Int64
): string;
var
  Response: TJSONObject;
begin
  Response := TJSONObject.Create;
  try
    Response.Add('ok', True);
    Response.Add('id', AID);
    
    Result := Response.AsJSON;
  finally
    Response.Free;
  end;
end;


{=============================================================================}
{  HTTP ENDPOINT: POST /grist/poll                                           }
{=============================================================================}

procedure HandleGristPoll(
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse
);
var
  JSONText: string;
  Commands: TJSONArray;
  ResponseText: string;
  Parser: TJSONParser;
  Root: TJSONData;
begin
  { Проверяем метод }
  if UpperCase(ARequest.Method) <> 'POST' then
  begin
    AResponse.Code := 405;
    AResponse.ContentType := 'application/json; charset=utf-8';
    AResponse.Content := CreateGristErrorResponse('Method not allowed');
    Exit;
  end;
  
  { Читаем тело запроса }
  JSONText := ARequest.Content;
  
  { Парсим JSON (допускаем пустой объект {}) }
  Parser := nil;
  Root := nil;
  
  try
    if Trim(JSONText) <> '' then
    begin
      Parser := TJSONParser.Create(JSONText, False);
      Root := Parser.Parse;
      
      if Root = nil then
      begin
        AResponse.Code := 400;
        AResponse.ContentType := 'application/json; charset=utf-8';
        AResponse.Content := CreateGristErrorResponse('Invalid JSON');
        Exit;
      end;
      
      if not (Root is TJSONObject) then
      begin
        AResponse.Code := 400;
        AResponse.ContentType := 'application/json; charset=utf-8';
        AResponse.Content := CreateGristErrorResponse('JSON root must be object');
        Exit;
      end;
    end;
    
    { Получаем команды из очереди }
    Commands := nil;
    try
      GristCommandQueue.PollCommands(Commands);
      
      { Формируем ответ }
      ResponseText := CreateGristSuccessResponse(Commands);
      
      AResponse.Code := 200;
      AResponse.ContentType := 'application/json; charset=utf-8';
      AResponse.Content := ResponseText;
      
    finally
      if Commands <> nil then
        Commands.Free;
    end;
    
  except
    on E: Exception do
    begin
      GristLog(
        Format(
          'HTTP GRIST poll error: %s',
          [E.Message]
        ),
        LM_Error
      );
      
      AResponse.Code := 500;
      AResponse.ContentType := 'application/json; charset=utf-8';
      AResponse.Content := CreateGristErrorResponse('Internal server error');
    end;
  finally
    if Root <> nil then
      Root.Free;
    
    if Parser <> nil then
      Parser.Free;
  end;
end;


{=============================================================================}
{  HTTP ENDPOINT: POST /grist/ack                                            }
{=============================================================================}

procedure HandleGristAck(
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse
);
var
  JSONText: string;
  Parser: TJSONParser;
  Root: TJSONObject;
  IDData: TJSONData;
  CommandID: Int64;
  ResponseText: string;
begin
  { Проверяем метод }
  if UpperCase(ARequest.Method) <> 'POST' then
  begin
    AResponse.Code := 405;
    AResponse.ContentType := 'application/json; charset=utf-8';
    AResponse.Content := CreateGristErrorResponse('Method not allowed');
    Exit;
  end;
  
  { Читаем тело запроса }
  JSONText := ARequest.Content;
  
  if Trim(JSONText) = '' then
  begin
    AResponse.Code := 400;
    AResponse.ContentType := 'application/json; charset=utf-8';
    AResponse.Content := CreateGristErrorResponse('Empty request body');
    Exit;
  end;
  
  { Парсим JSON }
  Parser := nil;
  Root := nil;
  
  try
    Parser := TJSONParser.Create(JSONText, False);
    Root := Parser.Parse as TJSONObject;
    
    if Root = nil then
    begin
      AResponse.Code := 400;
      AResponse.ContentType := 'application/json; charset=utf-8';
      AResponse.Content := CreateGristErrorResponse('Invalid JSON');
      Exit;
    end;
    
    { Проверяем наличие поля id }
    IDData := Root.Find('id');
    
    if IDData = nil then
    begin
      AResponse.Code := 400;
      AResponse.ContentType := 'application/json; charset=utf-8';
      AResponse.Content := CreateGristErrorResponse('Missing "id" field');
      Exit;
    end;
    
    { Получаем ID }
    try
      CommandID := IDData.AsInt64;
    except
      on E: Exception do
      begin
        AResponse.Code := 400;
        AResponse.ContentType := 'application/json; charset=utf-8';
        AResponse.Content := CreateGristErrorResponse('"id" must be a number');
        Exit;
      end;
    end;
    
    { Подтверждаем команду }
    if GristCommandQueue.AckCommand(CommandID) then
    begin
      ResponseText := CreateGristAckResponse(CommandID);
      
      AResponse.Code := 200;
      AResponse.ContentType := 'application/json; charset=utf-8';
      AResponse.Content := ResponseText;
    end
    else
    begin
      AResponse.Code := 404;
      AResponse.ContentType := 'application/json; charset=utf-8';
      AResponse.Content := CreateGristErrorResponse('Unknown command id');
    end;
    
  except
    on E: Exception do
    begin
      GristLog(
        Format(
          'HTTP GRIST ack error: %s',
          [E.Message]
        ),
        LM_Error
      );
      
      AResponse.Code := 500;
      AResponse.ContentType := 'application/json; charset=utf-8';
      AResponse.Content := CreateGristErrorResponse('Internal server error');
    end;
  finally
    if Root <> nil then
      Root.Free;
    
    if Parser <> nil then
      Parser.Free;
  end;
end;


{=============================================================================}
{  HTTP ENDPOINT: /grist/*                                                   }
{=============================================================================}

procedure HandleGristRequest(
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse
);
var
  URI: string;
begin
  URI := ARequest.URI;
  
  { Маршрутизация внутри /grist/* }
  if URI = '/grist/poll' then
  begin
    HandleGristPoll(ARequest, AResponse);
    Exit;
  end;
  
  if URI = '/grist/ack' then
  begin
    HandleGristAck(ARequest, AResponse);
    Exit;
  end;
  
  { Неизвестный endpoint }
  AResponse.Code := 404;
  AResponse.ContentType := 'application/json; charset=utf-8';
  AResponse.Content := CreateGristErrorResponse('Endpoint not found');
end;


{=============================================================================}
{  PUBLIC FUNCTION: QueueGristCommand                                        }
{=============================================================================}

function QueueGristCommand(
  const ACommandName: string;
  AArgs: TJSONObject
): Int64;
begin
  if GristCommandQueue = nil then
  begin
    GristLog(
      'GristCommandQueue is not initialized',
      LM_Error
    );
    
    Result := -1;
    Exit;
  end;
  
  Result := GristCommandQueue.Enqueue(ACommandName, AArgs);
end;


{=============================================================================}
{  ИНИЦИАЛИЗАЦИЯ МОДУЛЯ                                                      }
{=============================================================================}

initialization
  GristCommandQueue := TGristCommandQueue.Create;
  
  GristLog(
    'Grist HTTP module initialized',
    LM_Info
  );

finalization
  if GristCommandQueue <> nil then
  begin
    GristCommandQueue.Free;
    GristCommandQueue := nil;
  end;
  
  GristLog(
    'Grist HTTP module finalized',
    LM_Info
  );

end.
