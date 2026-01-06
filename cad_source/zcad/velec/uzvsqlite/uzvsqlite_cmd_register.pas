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
{$mode objfpc}{$H+}

unit uzvsqlite_cmd_register;

{$INCLUDE zengineconfig.inc}

interface

uses
  SysUtils,
  uzccommandsmanager, uzccommandsabstract, uzccommandsimpl,
  uzvsqlite_cmd_export, uzvsqlite_cmd_test, uzclog;

// Регистрация всех команд модуля uzvsqlite
procedure RegisterUzvSQLiteCommands;

implementation

type
  // Обёртка для команды экспорта
  TSQLiteExportCommand = class(TCommand)
  public
    function Execute(
      pCommandParam: Pointer;
      operationResult: TCommandResult
    ): TCommandResult; override;
  end;

  // Обёртка для тестовой команды
  TSQLiteTestCommand = class(TCommand)
  public
    function Execute(
      pCommandParam: Pointer;
      operationResult: TCommandResult
    ): TCommandResult; override;
  end;

{ TSQLiteExportCommand }

function TSQLiteExportCommand.Execute(
  pCommandParam: Pointer;
  operationResult: TCommandResult
): TCommandResult;
begin
  CmdUzvSQLiteExport(pCommandParam, Result);
end;

{ TSQLiteTestCommand }

function TSQLiteTestCommand.Execute(
  pCommandParam: Pointer;
  operationResult: TCommandResult
): TCommandResult;
begin
  CmdUzvSQLiteTest(pCommandParam, Result);
end;

// Регистрация команд в системе ZCAD
procedure RegisterUzvSQLiteCommands;
var
  exportCmd: TSQLiteExportCommand;
  testCmd: TSQLiteTestCommand;
begin
  programlog.LogOutFormatStr(
    'uzvsqlite: Начало регистрации команд',
    [],
    LM_Info
  );

  // Создаём и регистрируем команду экспорта
  exportCmd := TSQLiteExportCommand.Create('SQLiteExport', 0);
  CommandManager.RegisterCommand(exportCmd);

  // Создаём и регистрируем тестовую команду
  testCmd := TSQLiteTestCommand.Create('SQLiteTest', 0);
  CommandManager.RegisterCommand(testCmd);

  programlog.LogOutFormatStr(
    'uzvsqlite: Команды зарегистрированы: SQLiteExport, SQLiteTest',
    [],
    LM_Info
  );
end;

end.
