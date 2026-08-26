{
*****************************************************************************
*                                                                           *
*  This file is part of the ZCAD                                            *
*                                                                           *
*****************************************************************************

@author(HTTP IPC bridge for ZCAD)
@author(Vladimir Bobrov)
}

{$mode objfpc}{$H+}

unit uzvhttpipc;

{$INCLUDE zengineconfig.inc}

interface

uses
  Classes,
  SysUtils,
  fpjson,
  jsonparser,
  syncobjs,
  uzclog,
  uzbLogTypes,
  uzvipcserver;

{**
  Передаёт JSON-команду из HTTP-транспорта
  в существующую IPCCommandQueue.

  Входной JSON:

    {
      "id": "cmd-0001",
      "cmd": "PING",
      "args": []
    }

  Выходной JSON формируется существующим
  uzvipcintegration.pas.

  Result = True  - команда была поставлена в очередь
                   и получила результат выполнения.

  Result = False - ошибка формирования/передачи команды.
}
function HTTPIPCExecuteJSON(
  const AJSON: string;
  out AResponse: string
): Boolean;


implementation


{=============================================================================
  ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ
=============================================================================}

function HTTPIPCGetCommandType(
  const ACmdName: string
): TIPCCommandType;
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

  else if SameText(ACmdName, 'BATCH_LINES') then
    Result := ictBatchLines

  else if SameText(ACmdName, 'INSERT_DEVICE') then
    Result := ictInsertDevice

  else
    Result := ictUnknown;

end;


function HTTPIPCCreateErrorResponse(
  const AID: string;
  const AError: string
): string;
var
  Response: TJSONObject;
begin

  Response := TJSONObject.Create;

  try

    Response.Add('id', AID);
    Response.Add('status', 'error');
    Response.Add('error', AError);

    Result := Response.AsJSON;

  finally

    Response.Free;

  end;

end;


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
  HTTPIPCExecuteJSON
=============================================================================}

function HTTPIPCExecuteJSON(
  const AJSON: string;
  out AResponse: string
): Boolean;
var
  Parser: TJSONParser;
  Root: TJSONObject;

  Cmd: PIPCCommand;

  CmdName: string;
  CommandID: string;
  Token: string;

  ArgsData: TJSONData;

  WaitResult: TWaitResult;

  Response: TJSONObject;

  CommandEnqueued: Boolean;

begin

  Result := False;
  AResponse := '';

  Cmd := nil;
  Parser := nil;
  Root := nil;
  CommandEnqueued := False;


  {===========================================================================
    Проверка входных данных
  ===========================================================================}

  if Trim(AJSON) = '' then
  begin

    AResponse :=
      HTTPIPCCreateErrorResponse(
        '',
        'Empty request body'
      );

    Exit;

  end;


  {===========================================================================
    Проверка существования IPC-очереди
  ===========================================================================}

  if IPCCommandQueue = nil then
  begin

    HTTPIPCLog(
      'IPCCommandQueue is not initialized',
      LM_Error
    );

    AResponse :=
      HTTPIPCCreateErrorResponse(
        '',
        'IPC command queue is not initialized'
      );

    Exit;

  end;


  {===========================================================================
    Парсинг JSON
  ===========================================================================}

  try

    Parser := TJSONParser.Create(AJSON);

    try

      try

        Root := Parser.Parse as TJSONObject;

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


      {=======================================================================
        Получаем ID
      =======================================================================}

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


      {=======================================================================
        Получаем имя команды
      =======================================================================}

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


      {=======================================================================
        Получаем token.

        На этом уровне token не проверяется.

        Причина:
        HTTP-сервер работает отдельно от TIPCServerThread,
        а FToken у TIPCServerThread является private.

        При необходимости авторизацию HTTP можно добавить
        отдельным механизмом позже.
      =======================================================================}

      Token :=
        Root.Get(
          'token',
          ''
        );


      {=======================================================================
        Создаём команду
      =======================================================================}

      New(Cmd);

      Cmd^.ID := CommandID;

      Cmd^.CmdType :=
        HTTPIPCGetCommandType(
          CmdName
        );

      Cmd^.Token := Token;

      Cmd^.Response := nil;

      Cmd^.Completed :=
        TEvent.Create(
          nil,
          True,
          False,
          ''
        );


      {=======================================================================
        Получаем args
      =======================================================================}

      ArgsData :=
        Root.Find('args');

      if ArgsData <> nil then
      begin

        if not (ArgsData is TJSONArray) then
        begin

          AResponse :=
            HTTPIPCCreateErrorResponse(
              CommandID,
              'Command args must be an array'
            );

          Exit;

        end;


        {---------------------------------------------------------------------
          ВАЖНО:

          Делаем Clone.

          Root будет уничтожен после выхода из блока парсинга,
          поэтому нельзя передавать Root.Arrays['args'] напрямую.

          Кроме того, существующий uzvipcintegration.pas
          освобождает Cmd^.Args после выполнения команды.
        ---------------------------------------------------------------------}

        Cmd^.Args :=
          TJSONArray(
            ArgsData
          ).Clone as TJSONArray;

      end
      else
      begin

        Cmd^.Args :=
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


  {===========================================================================
    Если команда неизвестна

    Мы всё равно передаём её существующему IPC executor.

    Это важно для сохранения поведения:
    uzvipcintegration.pas сам вернёт:

      Unknown command

    Таким образом HTTP-транспорт не дублирует
    логику обработки команд.
  ===========================================================================}


  {===========================================================================
    ПОМЕЩЕНИЕ КОМАНДЫ В ОЧЕРЕДЬ
  ===========================================================================}

  try

    IPCCommandQueue.Enqueue(Cmd);

    CommandEnqueued := True;


    HTTPIPCLog(
      Format(
        'Command enqueued: %s',
        [Cmd^.ID]
      ),
      LM_Debug
    );


    {=======================================================================
      ОЖИДАНИЕ ВЫПОЛНЕНИЯ

      Команду выполняет НЕ HTTP-поток.

      Её заберёт:

        uzvipcintegration.pas
          ↓
        IPCCommandHandler.ProcessQueue
          ↓
        ExecutePing / ExecuteLine / ...
          ↓
        Cmd^.Completed.SetEvent

      Поэтому HTTP-поток здесь только ждёт.
    =======================================================================}

    WaitResult :=
      Cmd^.Completed.WaitFor(
        IPC_COMMAND_TIMEOUT
      );


    {=======================================================================
      Команда выполнена
    =======================================================================}

    if WaitResult = wrSignaled then
    begin

      if Cmd^.Response <> nil then
      begin

        {-------------------------------------------------------------------
          Копируем JSON в строку.

          После этого TJSONObject можно освободить.
        -------------------------------------------------------------------}

        Response := Cmd^.Response;

        AResponse :=
          Response.AsJSON;

        Cmd^.Response := nil;

        Response.Free;


        HTTPIPCLog(
          Format(
            'Command completed: %s',
            [Cmd^.ID]
          ),
          LM_Debug
        );


        Result := True;

      end
      else
      begin

        AResponse :=
          HTTPIPCCreateErrorResponse(
            Cmd^.ID,
            'Command completed without response'
          );

        HTTPIPCLog(
          Format(
            'Command completed without response: %s',
            [Cmd^.ID]
          ),
          LM_Error
        );

        Result := False;

      end;

    end

    {=======================================================================
      Таймаут
    =======================================================================}

    else if WaitResult = wrTimeout then
    begin

      HTTPIPCLog(
        Format(
          'Command timeout: %s',
          [Cmd^.ID]
        ),
        LM_Error
      );


      AResponse :=
        HTTPIPCCreateErrorResponse(
          Cmd^.ID,
          'Command timeout'
        );


      {---------------------------------------------------------------------
        ВАЖНО!

        Здесь мы НЕ освобождаем Cmd.

        Команда уже помещена в IPCCommandQueue и может всё ещё
        находиться в очереди или выполняться главным потоком.

        Если сейчас сделать:

          Dispose(Cmd);

        главный поток может получить dangling pointer.

        Поэтому при timeout право владения командой остаётся
        у IPC-системы.

        Это безопаснее, чем возможный Access Violation.

        Для первого этапа это сознательное решение.
      ---------------------------------------------------------------------}

      Cmd := nil;

      Result := False;

    end

    {=======================================================================
      Другой результат WaitFor
    =======================================================================}

    else
    begin

      HTTPIPCLog(
        Format(
          'Command wait error: %s',
          [Cmd^.ID]
        ),
        LM_Error
      );


      AResponse :=
        HTTPIPCCreateErrorResponse(
          Cmd^.ID,
          'Error waiting for command completion'
        );


      {---------------------------------------------------------------------
        Аналогично timeout:

        команда уже передана в очередь, поэтому освобождать её
        здесь нельзя.
      ---------------------------------------------------------------------}

      Cmd := nil;

      Result := False;

    end;


  except

    on E: Exception do
    begin

      HTTPIPCLog(
        Format(
          'IPC queue error: %s',
          [E.Message]
        ),
        LM_Error
      );


      AResponse :=
        HTTPIPCCreateErrorResponse(
          CommandID,
          Format(
            'IPC queue error: %s',
            [E.Message]
          )
        );


      {---------------------------------------------------------------------
        Если команда уже была помещена в очередь,
        не пытаемся её освобождать.
      ---------------------------------------------------------------------}

      if CommandEnqueued then
        Cmd := nil;


      Result := False;

    end;

  end;


  {===========================================================================
    ОЧИСТКА

    До этого места мы доходим только если команда НЕ была передана
    очереди либо уже полностью получила результат.

    Если Cmd = nil:
      либо команда передана очереди,
      либо команда была освобождена ниже.

    Если Cmd <> nil:
      команда существует и не была передана очереди.
  ===========================================================================}

  if Cmd <> nil then
  begin

    if Cmd^.Response <> nil then
    begin

      Cmd^.Response.Free;
      Cmd^.Response := nil;

    end;


    if Cmd^.Args <> nil then
    begin

      Cmd^.Args.Free;
      Cmd^.Args := nil;

    end;


    if Cmd^.Completed <> nil then
    begin

      Cmd^.Completed.Free;
      Cmd^.Completed := nil;

    end;


    Dispose(Cmd);

    Cmd := nil;

  end;


end;


end.
