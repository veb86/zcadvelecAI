{
*****************************************************************************
*  Test Grist Command - Testing ZCAD -> Grist command queue
*****************************************************************************
}

{$mode objfpc}{$H+}

unit uzvtestgrist;

{$INCLUDE zengineconfig.inc}

interface

uses
  SysUtils,
  Classes,
  fpjson,
  jsonparser,
  uzclog,
  uzvhttpgrist,
  uzccommandsmanager,
  uzccommandsabstract,
  uzccommandsimpl,
  uzcinterface;

function TestGristCommand_com(const Context: TZCADCommandContext;
  operands: TCommandOperands): TCommandResult;

implementation

procedure LogMessage(const Msg: string);
begin
  zcUI.TextMessage(Msg, TMWOHistoryOut);
end;

function TestGristCommand_com(const Context: TZCADCommandContext;
  operands: TCommandOperands): TCommandResult;
var
  Args: TJSONObject;
  CmdID: Int64;
  LogMsg: string;
begin
  Result := cmd_ok;

  LogMessage('========================================');
  LogMessage('TEST GRIST COMMAND');
  LogMessage('========================================');

  Args := TJSONObject.Create;

    {----------------------------------------------------------
      Формируем JSON-аргументы команды SET_GRIST_VALUE
    ----------------------------------------------------------}

    Args.Add('table', 'Devices');
    Args.Add('recordId', 25);
    Args.Add('field', 'Name');
    Args.Add('value', 'Светильник');

    LogMessage('Добавление команды в очередь ZCAD -> Grist...');
    LogMessage('  command = SET_GRIST_VALUE');
    LogMessage('  table = Devices');
    LogMessage('  recordId = 25');
    LogMessage('  field = Name');
    LogMessage('  value = Светильник');

    {----------------------------------------------------------
      Передаём команду в очередь ZCAD -> Grist
    ----------------------------------------------------------}

    CmdID := QueueGristCommand('SET_GRIST_VALUE', Args);

    {----------------------------------------------------------
      Формируем сообщение о результате
    ----------------------------------------------------------}

    LogMsg := Format(
      'testGrist: команда добавлена в очередь. ID=%d',
      [CmdID]
    );


    { Сообщение пользователю ZCAD }
    LogMessage('');
    LogMessage(LogMsg);
    LogMessage(
      'Команда ожидает обработки через POST /grist/poll от managerGRIST'
    );

    LogMessage('========================================');
    LogMessage('TEST GRIST COMMAND - completed');
    LogMessage('========================================');

    {----------------------------------------------------------
      QueueGristCommand передаёт владение Args очереди.
      Поэтому не освобождаем Args здесь.
    ----------------------------------------------------------}

end;

initialization
  programlog.LogOutFormatStr(
    'Unit "%s" initialization',
    [{$INCLUDE %FILE%}],
    LM_Info,
    UnitsInitializeLMId
  );

  { Регистрация команды ZCAD }
  CreateZCADCommand(
    @TestGristCommand_com,
    'testGrist',
    CADWG,
    0
  );

finalization
  ProgramLog.LogOutFormatStr(
    'Unit "%s" finalization',
    [{$INCLUDE %FILE%}],
    LM_Info,
    UnitsFinalizeLMId
  );

end.
