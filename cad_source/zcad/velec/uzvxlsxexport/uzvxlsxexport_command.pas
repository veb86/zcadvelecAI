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

unit uzvxlsxexport_command;

{$INCLUDE zengineconfig.inc}

interface

uses
  SysUtils, Classes, Dialogs,
  uzccommandsabstract, uzccommandsimpl, uzcinterface,
  uzvxlsxexport_types, uzvxlsxexport_config, uzvxlsxexport_parser,
  uzvxlsxexport_executor, uzvxlsxexport_registry,
  uzvxlsxexport_handlers, uzvzcadxlsxfps, uzclog;

// Основная команда UzvXLSXexport
procedure CmdUzvXLSXexport(
  pCommandParam: Pointer;
  var operationResult: TCommandResult
);

implementation

procedure CmdUzvXLSXexport(
  pCommandParam: Pointer;
  var operationResult: TCommandResult
);
var
  config: TXlsxExportConfig;
  registry: TXlsxCommandRegistry;
  handlers: TXlsxExportHandlers;
  parser: TXlsxExportParser;
  executor: TXlsxExportExecutor;
  instructions: TXlsxExportInstructionList;
  exportResult: TXlsxExportResult;
  templateFile: String;
  openDialog: TOpenDialog;
  i: Integer;
begin
  zcUI.TextMessage(
    'Запуск генератора XLSX файлов...',
    TMWOHistoryOut
  );

  programlog.LogOutFormatStr(
    'uzvxlsxexport: Начало выполнения команды UzvXLSXexport',
    [],
    LM_Info
  );

  try
    // Создаём конфигурацию
    config := TXlsxExportConfig.Create;
    try
      // Получаем путь к шаблону
      templateFile := '';

      // TODO: Проверить операнд команды для быстрого запуска
      // Если операнд не задан, открываем диалог

      openDialog := TOpenDialog.Create(nil);
      try
        openDialog.Title := 'Выберите шаблонный файл XLSX';
        openDialog.Filter := 'Excel Files|*.xlsx|All Files|*.*';
        openDialog.Options := [ofFileMustExist, ofEnableSizing];

        if openDialog.Execute then
          templateFile := openDialog.FileName
        else
        begin
          zcUI.TextMessage(
            'Экспорт отменён: файл не выбран',
            TMWOHistoryOut
          );
          operationResult := cmd_OK;
          Exit;
        end;

      finally
        openDialog.Free;
      end;

      config.TemplatePath := templateFile;

      zcUI.TextMessage(
        'Файл шаблона: ' + templateFile,
        TMWOHistoryOut
      );

      programlog.LogOutFormatStr(
        'uzvxlsxexport: Выбран шаблон "%s"',
        [templateFile],
        LM_Info
      );

      // Создаём реестр команд
      registry := TXlsxCommandRegistry.Create;
      try
        // Создаём обработчики команд
        handlers := TXlsxExportHandlers.Create(config);
        try
          // Регистрируем обработчики команд
          registry.RegisterExportCommand(
            'getaccessfile',
            @handlers.HandleGetAccessFile,
            'Генерация файла Access БД'
          );

          registry.RegisterExportCommand(
            'query',
            @handlers.HandleQuery,
            'Выполнение запроса к БД'
          );

          // Создаём парсер
          parser := TXlsxExportParser.Create(templateFile);
          try
            // Парсим все листы EXPORT
            instructions := parser.ParseAllExportSheets;
            try
              if instructions.Size = 0 then
              begin
                zcUI.TextMessage(
                  'В шаблоне не найдено листов EXPORT',
                  TMWOHistoryOut
                );

                programlog.LogOutFormatStr(
                  'uzvxlsxexport: Листы EXPORT не найдены',
                  [],
                  LM_Info
                );

                operationResult := cmd_OK;
                Exit;
              end;

              zcUI.TextMessage(
                Format('Найдено листов EXPORT: %d', [instructions.Size]),
                TMWOHistoryOut
              );

              // Создаём исполнитель
              executor := TXlsxExportExecutor.Create(config, registry);
              try
                // Выполняем все инструкции
                exportResult := executor.ExecuteAll(instructions);
                try
                  // Выводим результаты
                  zcUI.TextMessage(
                    StringOfChar('=', 70),
                    TMWOHistoryOut
                  );

                  zcUI.TextMessage(
                    'РЕЗУЛЬТАТЫ ГЕНЕРАЦИИ XLSX:',
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

                  // Сохраняем результат
                  if exportResult.Success then
                  begin
                    zcUI.TextMessage(
                      'Генерация XLSX успешно завершена',
                      TMWOHistoryOut
                    );
                    operationResult := cmd_OK;
                  end
                  else
                  begin
                    zcUI.TextMessage(
                      'Генерация XLSX завершена с ошибками',
                      TMWOHistoryOut
                    );
                    operationResult := cmd_OK_WithErrors;
                  end;

                finally
                  exportResult.Free;
                end;

              finally
                executor.Free;
              end;

            finally
              // Освобождаем инструкции
              for i := 0 to instructions.Size - 1 do
                instructions[i].Free;
              instructions.Free;
            end;

          finally
            parser.Free;
          end;

        finally
          handlers.Free;
        end;

      finally
        registry.Free;
      end;

    finally
      config.Free;
    end;

  except
    on E: Exception do
    begin
      zcUI.TextMessage(
        'ОШИБКА: ' + E.Message,
        TMWOHistoryOut
      );

      programlog.LogOutFormatStr(
        'uzvxlsxexport: Ошибка выполнения команды: %s',
        [E.Message],
        LM_Info
      );

      operationResult := cmd_Error;
    end;
  end;

  // Закрываем XLSX файл
  try
    destroyWorkbook();
  except
    // Игнорируем ошибки закрытия
  end;
end;

end.
