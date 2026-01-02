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

unit uzvxlsxexport_executor;

{$INCLUDE zengineconfig.inc}

interface

uses
  SysUtils, Classes,
  uzvxlsxexport_types, uzvxlsxexport_config,
  uzvxlsxexport_registry, uzclog;

type
  // Исполнитель команд экспорта
  TXlsxExportExecutor = class
  private
    FConfig: TXlsxExportConfig;
    FRegistry: TXlsxCommandRegistry;
    FContext: TObject;

    // Выполнить одну команду
    function ExecuteCommand(
      const ACommandName: String;
      const AParams: TStringList
    ): Boolean;

  public
    constructor Create(
      AConfig: TXlsxExportConfig;
      ARegistry: TXlsxCommandRegistry
    );

    // Выполнить инструкцию (набор команд из одного листа EXPORT)
    function ExecuteInstruction(
      AInstruction: TXlsxExportInstruction
    ): TXlsxSheetResult;

    // Выполнить все инструкции
    function ExecuteAll(
      AInstructions: TXlsxExportInstructionList
    ): TXlsxExportResult;

    property Config: TXlsxExportConfig read FConfig;
    property Registry: TXlsxCommandRegistry read FRegistry;
  end;

implementation

{ TXlsxExportExecutor }

constructor TXlsxExportExecutor.Create(
  AConfig: TXlsxExportConfig;
  ARegistry: TXlsxCommandRegistry
);
begin
  FConfig := AConfig;
  FRegistry := ARegistry;
  FContext := nil;

  programlog.LogOutFormatStr(
    'uzvxlsxexport: Исполнитель команд создан',
    [],
    LM_Info
  );
end;

function TXlsxExportExecutor.ExecuteCommand(
  const ACommandName: String;
  const AParams: TStringList
): Boolean;
var
  handler: TXlsxExportCommandHandler;
begin
  Result := False;

  // Ищем обработчик команды
  handler := FRegistry.FindExportCommand(ACommandName);

  if not Assigned(handler) then
  begin
    programlog.LogOutFormatStr(
      'uzvxlsxexport: Команда "%s" не найдена в реестре',
      [ACommandName],
      LM_Info
    );
    Exit;
  end;

  // Выполняем команду
  try
    Result := handler(AParams);

    programlog.LogOutFormatStr(
      'uzvxlsxexport: Команда "%s" выполнена, результат: %s',
      [ACommandName, BoolToStr(Result, True)],
      LM_Info
    );

  except
    on E: Exception do
    begin
      programlog.LogOutFormatStr(
        'uzvxlsxexport: Ошибка выполнения команды "%s": %s',
        [ACommandName, E.Message],
        LM_Info
      );
      Result := False;
    end;
  end;
end;

function TXlsxExportExecutor.ExecuteInstruction(
  AInstruction: TXlsxExportInstruction
): TXlsxSheetResult;
var
  result: TXlsxSheetResult;
  i: Integer;
  commandLine: String;
  params: TStringList;
  commandName: String;
  success: Boolean;
begin
  // Инициализация результата
  result.SheetName := AInstruction.SheetName;
  result.Success := True;
  result.RowsProcessed := 0;
  result.ErrorCount := 0;
  result.ErrorMessages := TStringList.Create;

  programlog.LogOutFormatStr(
    'uzvxlsxexport: Начало выполнения инструкций листа "%s"',
    [AInstruction.SheetName],
    LM_Info
  );

  // Выполняем команды
  params := TStringList.Create;
  try
    for i := 0 to AInstruction.Commands.Count - 1 do
    begin
      commandLine := AInstruction.Commands[i];

      // Разбираем строку команды
      params.Clear;
      params.Delimiter := '|';
      params.StrictDelimiter := True;
      params.DelimitedText := commandLine;

      if params.Count = 0 then
        Continue;

      commandName := params[0];
      params.Delete(0);

      // Выполняем команду
      success := ExecuteCommand(commandName, params);

      if not success then
      begin
        Inc(result.ErrorCount);
        result.ErrorMessages.Add(
          Format('Ошибка выполнения команды "%s"', [commandName])
        );

        if FConfig.StopOnError then
        begin
          result.Success := False;
          Break;
        end;
      end;

      Inc(result.RowsProcessed);
    end;

  finally
    params.Free;
  end;

  if result.ErrorCount > 0 then
    result.Success := False;

  programlog.LogOutFormatStr(
    'uzvxlsxexport: Завершено выполнение листа "%s", ошибок: %d',
    [AInstruction.SheetName, result.ErrorCount],
    LM_Info
  );

  Result := result;
end;

function TXlsxExportExecutor.ExecuteAll(
  AInstructions: TXlsxExportInstructionList
): TXlsxExportResult;
var
  exportResult: TXlsxExportResult;
  i: Integer;
  sheetResult: TXlsxSheetResult;
begin
  exportResult := TXlsxExportResult.Create;
  exportResult.StartTime := Now;

  programlog.LogOutFormatStr(
    'uzvxlsxexport: Начало выполнения всех инструкций, листов: %d',
    [AInstructions.Size],
    LM_Info
  );

  for i := 0 to AInstructions.Size - 1 do
  begin
    sheetResult := ExecuteInstruction(AInstructions[i]);
    exportResult.AddSheetResult(sheetResult);

    if not sheetResult.Success and FConfig.StopOnError then
    begin
      programlog.LogOutFormatStr(
        'uzvxlsxexport: Прерывание выполнения из-за ошибки на листе "%s"',
        [AInstructions[i].SheetName],
        LM_Info
      );
      Break;
    end;
  end;

  exportResult.EndTime := Now;

  programlog.LogOutFormatStr(
    'uzvxlsxexport: Завершено выполнение всех инструкций',
    [],
    LM_Info
  );

  Result := exportResult;
end;

end.
