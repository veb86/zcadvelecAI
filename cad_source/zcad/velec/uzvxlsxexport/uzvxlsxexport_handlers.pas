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

unit uzvxlsxexport_handlers;

{$INCLUDE zengineconfig.inc}

interface

uses
  SysUtils, Classes, Dialogs,
  uzvxlsxexport_types, uzvxlsxexport_config,
  uzvaccess_cmd_export, uzclog, uzcinterface;

type
  // Класс с обработчиками команд
  TXlsxExportHandlers = class
  private
    FConfig: TXlsxExportConfig;
    FAccessDbPath: String;

  public
    constructor Create(AConfig: TXlsxExportConfig);

    // Обработчик команды getaccessfile
    function HandleGetAccessFile(const AParams: TStringList): Boolean;

    // Обработчик команды query
    function HandleQuery(const AParams: TStringList): Boolean;

    property Config: TXlsxExportConfig read FConfig;
    property AccessDbPath: String read FAccessDbPath;
  end;

implementation

{ TXlsxExportHandlers }

constructor TXlsxExportHandlers.Create(AConfig: TXlsxExportConfig);
begin
  FConfig := AConfig;
  FAccessDbPath := '';
end;

function TXlsxExportHandlers.HandleGetAccessFile(
  const AParams: TStringList
): Boolean;
var
  operationResult: TCommandResult;
begin
  Result := False;

  programlog.LogOutFormatStr(
    'uzvxlsxexport: Выполнение команды getaccessfile',
    [],
    LM_Info
  );

  try
    // Вызываем существующую команду экспорта Access
    CmdUzvAccessExport(nil, operationResult);

    // Проверяем результат
    if operationResult = cmd_OK then
    begin
      Result := True;

      // Сохраняем путь к БД для последующих запросов
      if FConfig.AccessDbPath <> '' then
        FAccessDbPath := FConfig.AccessDbPath;

      programlog.LogOutFormatStr(
        'uzvxlsxexport: Команда getaccessfile выполнена успешно',
        [],
        LM_Info
      );

      zcUI.TextMessage(
        'Экспорт в Access выполнен успешно',
        TMWOHistoryOut
      );
    end
    else
    begin
      programlog.LogOutFormatStr(
        'uzvxlsxexport: Ошибка выполнения команды getaccessfile',
        [],
        LM_Info
      );

      zcUI.TextMessage(
        'Ошибка экспорта в Access',
        TMWOHistoryOut
      );
    end;

  except
    on E: Exception do
    begin
      programlog.LogOutFormatStr(
        'uzvxlsxexport: Исключение при выполнении getaccessfile: %s',
        [E.Message],
        LM_Info
      );

      zcUI.TextMessage(
        'ОШИБКА: ' + E.Message,
        TMWOHistoryOut
      );
    end;
  end;
end;

function TXlsxExportHandlers.HandleQuery(
  const AParams: TStringList
): Boolean;
var
  tableName: String;
  queryText: String;
begin
  Result := False;

  programlog.LogOutFormatStr(
    'uzvxlsxexport: Выполнение команды query',
    [],
    LM_Info
  );

  // Проверяем параметры
  if AParams.Count < 2 then
  begin
    programlog.LogOutFormatStr(
      'uzvxlsxexport: Недостаточно параметров для команды query',
      [],
      LM_Info
    );

    zcUI.TextMessage(
      'Ошибка: команда query требует 2 параметра (таблица, запрос)',
      TMWOHistoryOut
    );
    Exit;
  end;

  tableName := AParams[0];
  queryText := AParams[1];

  programlog.LogOutFormatStr(
    'uzvxlsxexport: Запрос к таблице "%s": %s',
    [tableName, queryText],
    LM_Info
  );

  try
    // TODO: Реализовать выполнение запроса к БД
    // Здесь должна быть логика:
    // 1. Подключение к БД Access (используя FAccessDbPath)
    // 2. Выполнение запроса
    // 3. Сохранение результатов для использования в ячейках

    zcUI.TextMessage(
      Format('Выполнение запроса к таблице "%s"', [tableName]),
      TMWOHistoryOut
    );

    // Заглушка - возвращаем успех
    Result := True;

    programlog.LogOutFormatStr(
      'uzvxlsxexport: Команда query выполнена',
      [],
      LM_Info
    );

  except
    on E: Exception do
    begin
      programlog.LogOutFormatStr(
        'uzvxlsxexport: Ошибка выполнения query: %s',
        [E.Message],
        LM_Info
      );

      zcUI.TextMessage(
        'ОШИБКА: ' + E.Message,
        TMWOHistoryOut
      );
    end;
  end;
end;

end.
