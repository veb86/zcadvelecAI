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

{**Модуль реализации и регистрации команды экспорта в MS SQLite}
unit uzvsqlite_command;

{$INCLUDE zengineconfig.inc}

interface

uses
  SysUtils,
  Classes,
  Dialogs,
  uzccommandsmanager,
  uzccommandsabstract,
  uzccommandsimpl,
  uzclog,
  uzcinterface,
  uzcdrawings,
  uzbtypes,
  uzvsqlite_types,
  uzvsqlite_config,
  uzvsqlite_exporter;

{**Функция команды экспорта данных в MS SQLite}
function SQLiteExport_com(
  const Context: TZCADCommandContext;
  operands: TCommandOperands
): TCommandResult;

implementation

{**Получить путь к файлу базы данных из пользовательского диалога}
function GetDatabasePath: string;
var
  openDialog: TOpenDialog;
begin
  Result := '';

  openDialog := TOpenDialog.Create(nil);
  try
    openDialog.Title := 'Выберите файл базы данных MS SQLite';
    openDialog.Filter := 'MS SQLite Database|*.mdb;*.accdb|All Files|*.*';
    openDialog.Options := [ofFileMustExist, ofEnableSizing];

    if openDialog.Execute then
      Result := openDialog.FileName;
  finally
    openDialog.Free;
  end;
end;

{**Вывести результаты экспорта в интерфейс}
procedure DisplayExportResults(const exportResult: TExportResult);
begin
  zcUI.TextMessage(
    StringOfChar('=', 70),
    TMWOHistoryOut
  );

  zcUI.TextMessage(
    'РЕЗУЛЬТАТЫ ЭКСПОРТА:',
    TMWOHistoryOut
  );

  zcUI.TextMessage(
    exportResult.GetSummary,
    TMWOHistoryOut
  );

  zcUI.TextMessage(
    StringOfChar('=', 70),
    TMWOHistoryOut
  );
end;

{**Определить код результата на основе количества ошибок}
function DetermineCommandResult(totalErrors: Integer): TCommandResult;
begin
  if totalErrors > 0 then
  begin
    zcUI.TextMessage(
      'Экспорт завершён с ошибками',
      TMWOHistoryOut
    );
    Result := cmd_error;
  end
  else
  begin
    zcUI.TextMessage(
      'Экспорт успешно завершён',
      TMWOHistoryOut
    );
    Result := cmd_OK;
  end;
end;

{**Выполнить процесс экспорта данных}
function PerformExport(const databasePath: string): TCommandResult;
var
  config: TExportConfig;
  exporter: TSQLiteExporter;
  exportResult: TExportResult;
begin
  Result := cmd_Error;

  config := TExportConfig.Create;
  try
    config.DatabasePath := databasePath;

    zcUI.TextMessage(
      'Файл базы данных: ' + databasePath,
      TMWOHistoryOut
    );

    exporter := TSQLiteExporter.Create(config);
    try
      exportResult := exporter.Execute;
      try
        DisplayExportResults(exportResult);
        Result := DetermineCommandResult(exportResult.TotalErrors);
      finally
        exportResult.Free;
      end;
    finally
      exporter.Free;
    end;
  finally
    config.Free;
  end;
end;

{**Функция команды экспорта данных в MS SQLite}
function SQLiteExport_com(
  const Context: TZCADCommandContext;
  operands: TCommandOperands
): TCommandResult;
var
  databasePath: string;
begin
  zcUI.TextMessage(
    'Запуск экспорта данных в MS SQLite...',
    TMWOHistoryOut
  );

  programlog.LogOutFormatStr(
    'uzvsqlite: Запуск команды экспорта в MS SQLite',
    [],
    LM_Info
  );

  try
    databasePath := GetDatabasePath;

    if databasePath = '' then
    begin
      zcUI.TextMessage(
        'Экспорт отменён: файл не выбран',
        TMWOHistoryOut
      );
      Result := cmd_OK;
      Exit;
    end;

    Result := PerformExport(databasePath);

  except
    on E: Exception do
    begin
      zcUI.TextMessage(
        'ОШИБКА: ' + E.Message,
        TMWOHistoryOut
      );

      programlog.LogOutFormatStr(
        'uzvsqlite: Ошибка выполнения команды экспорта: %s',
        [E.Message],
        LM_Info
      );

      Result := cmd_Error;
    end;
  end;
end;

initialization
  CreateZCADCommand(
    @SQLiteExport_com,
    'SQLiteExport',
    CADWG,
    0
  );

end.
