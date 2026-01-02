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

unit uzvxlsxexport_cmd_register;

{$INCLUDE zengineconfig.inc}

interface

uses
  SysUtils,
  uzccommandsmanager, uzccommandsabstract, uzccommandsimpl,
  uzvxlsxexport_command, uzclog;

// Регистрация команды UzvXLSXexport в системе ZCAD
procedure RegisterUzvXLSXexportCommands;

implementation

type
  // Обёртка для команды экспорта XLSX
  TXLSXExportCommand = class(TCommand)
  public
    function Execute(
      pCommandParam: Pointer;
      operationResult: TCommandResult
    ): TCommandResult; override;
  end;

{ TXLSXExportCommand }

function TXLSXExportCommand.Execute(
  pCommandParam: Pointer;
  operationResult: TCommandResult
): TCommandResult;
begin
  CmdUzvXLSXexport(pCommandParam, Result);
end;

// Регистрация команды в системе ZCAD
procedure RegisterUzvXLSXexportCommands;
var
  xlsxExportCmd: TXLSXExportCommand;
begin
  programlog.LogOutFormatStr(
    'uzvxlsxexport: Начало регистрации команд',
    [],
    LM_Info
  );

  // Создаём и регистрируем команду экспорта XLSX
  xlsxExportCmd := TXLSXExportCommand.Create('UzvXLSXexport', 0);
  CommandManager.RegisterCommand(xlsxExportCmd);

  programlog.LogOutFormatStr(
    'uzvxlsxexport: Команда UzvXLSXexport зарегистрирована',
    [],
    LM_Info
  );
end;

end.
