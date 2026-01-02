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

unit uzvxlsxexport_types;

{$INCLUDE zengineconfig.inc}

interface

uses
  SysUtils, Classes, Variants, gvector;

type
  // Тип команды экспорта в листе EXPORT
  TXlsxExportCommandType = (
    xctUnknown,        // Неизвестная команда
    xctGetAccessFile,  // getaccessfile - генерация Access БД
    xctQuery           // query - запрос к БД
  );

  // Тип команды в ячейках таблицы
  TXlsxCellCommandType = (
    cctUnknown,   // Неизвестная команда
    cctValue,     // Обычное значение
    cctFormula,   // Формула
    cctVariable,  // Переменная из результата запроса
    cctFunction   // Функция обработки
  );

  // Регистратор команд экспорта
  TXlsxExportCommandHandler = function(
    const AParams: TStringList
  ): Boolean of object;

  // Регистратор команд ячеек
  TXlsxCellCommandHandler = function(
    const AValue: String;
    const AContext: TObject
  ): String of object;

  // Информация о команде экспорта
  TXlsxExportCommandInfo = class
  public
    CommandName: String;
    Handler: TXlsxExportCommandHandler;
    Description: String;

    constructor Create(
      const AName: String;
      AHandler: TXlsxExportCommandHandler;
      const ADescription: String
    );
  end;

  // Информация о команде ячейки
  TXlsxCellCommandInfo = class
  public
    CommandName: String;
    Handler: TXlsxCellCommandHandler;
    Description: String;

    constructor Create(
      const AName: String;
      AHandler: TXlsxCellCommandHandler;
      const ADescription: String
    );
  end;

  // Списки команд
  TXlsxExportCommandList = specialize TVector<TXlsxExportCommandInfo>;
  TXlsxCellCommandList = specialize TVector<TXlsxCellCommandInfo>;

  // Инструкция экспорта из одного листа EXPORT
  TXlsxExportInstruction = class
  private
    FSheetName: String;
    FCommands: TStringList;
  public
    constructor Create(const ASheetName: String);
    destructor Destroy; override;

    // Добавить команду
    procedure AddCommand(
      const ACommandName: String;
      const AParam1: String = '';
      const AParam2: String = ''
    );

    property SheetName: String read FSheetName;
    property Commands: TStringList read FCommands;
  end;

  TXlsxExportInstructionList = specialize TVector<TXlsxExportInstruction>;

  // Результат обработки одного листа
  TXlsxSheetResult = record
    SheetName: String;
    Success: Boolean;
    RowsProcessed: Integer;
    ErrorCount: Integer;
    ErrorMessages: TStringList;
  end;

  // Общий результат экспорта
  TXlsxExportResult = class
  private
    FSheetResults: TList;
    FTotalRowsProcessed: Integer;
    FTotalErrors: Integer;
    FStartTime: TDateTime;
    FEndTime: TDateTime;
    FSuccess: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    // Добавить результат обработки листа
    procedure AddSheetResult(const AResult: TXlsxSheetResult);

    // Получить длительность выполнения
    function GetDuration: Double;

    // Получить сводку как текст
    function GetSummary: String;

    property SheetResults: TList read FSheetResults;
    property TotalRowsProcessed: Integer read FTotalRowsProcessed;
    property TotalErrors: Integer read FTotalErrors;
    property StartTime: TDateTime read FStartTime write FStartTime;
    property EndTime: TDateTime read FEndTime write FEndTime;
    property Success: Boolean read FSuccess write FSuccess;
  end;

// Вспомогательные функции

// Преобразование строки в тип команды экспорта
function StringToExportCommandType(const AValue: String): TXlsxExportCommandType;

// Преобразование типа команды экспорта в строку
function ExportCommandTypeToString(AType: TXlsxExportCommandType): String;

implementation

{ TXlsxExportCommandInfo }

constructor TXlsxExportCommandInfo.Create(
  const AName: String;
  AHandler: TXlsxExportCommandHandler;
  const ADescription: String
);
begin
  CommandName := AName;
  Handler := AHandler;
  Description := ADescription;
end;

{ TXlsxCellCommandInfo }

constructor TXlsxCellCommandInfo.Create(
  const AName: String;
  AHandler: TXlsxCellCommandHandler;
  const ADescription: String
);
begin
  CommandName := AName;
  Handler := AHandler;
  Description := ADescription;
end;

{ TXlsxExportInstruction }

constructor TXlsxExportInstruction.Create(const ASheetName: String);
begin
  FSheetName := ASheetName;
  FCommands := TStringList.Create;
end;

destructor TXlsxExportInstruction.Destroy;
begin
  FCommands.Free;
  inherited Destroy;
end;

procedure TXlsxExportInstruction.AddCommand(
  const ACommandName: String;
  const AParam1: String;
  const AParam2: String
);
var
  commandLine: String;
begin
  commandLine := ACommandName;

  if AParam1 <> '' then
    commandLine := commandLine + '|' + AParam1;

  if AParam2 <> '' then
    commandLine := commandLine + '|' + AParam2;

  FCommands.Add(commandLine);
end;

{ TXlsxExportResult }

constructor TXlsxExportResult.Create;
begin
  FSheetResults := TList.Create;
  FTotalRowsProcessed := 0;
  FTotalErrors := 0;
  FStartTime := Now;
  FEndTime := Now;
  FSuccess := True;
end;

destructor TXlsxExportResult.Destroy;
var
  i: Integer;
  pResult: ^TXlsxSheetResult;
begin
  // Освобождаем результаты листов
  for i := 0 to FSheetResults.Count - 1 do
  begin
    pResult := FSheetResults[i];
    if Assigned(pResult^.ErrorMessages) then
      pResult^.ErrorMessages.Free;
    Dispose(pResult);
  end;

  FSheetResults.Free;
  inherited Destroy;
end;

procedure TXlsxExportResult.AddSheetResult(const AResult: TXlsxSheetResult);
var
  pResult: ^TXlsxSheetResult;
begin
  New(pResult);
  pResult^ := AResult;
  FSheetResults.Add(pResult);

  // Обновляем общую статистику
  Inc(FTotalRowsProcessed, AResult.RowsProcessed);
  Inc(FTotalErrors, AResult.ErrorCount);

  if not AResult.Success then
    FSuccess := False;
end;

function TXlsxExportResult.GetDuration: Double;
begin
  Result := (FEndTime - FStartTime) * 24 * 60 * 60;
end;

function TXlsxExportResult.GetSummary: String;
var
  i: Integer;
  pResult: ^TXlsxSheetResult;
begin
  Result := Format('Экспорт XLSX завершен за %.2f сек.' + LineEnding, [GetDuration]);
  Result := Result + Format('Обработано листов: %d' + LineEnding, [FSheetResults.Count]);
  Result := Result + Format('Всего строк обработано: %d' + LineEnding, [FTotalRowsProcessed]);
  Result := Result + Format('Ошибок: %d' + LineEnding, [FTotalErrors]);

  // Детали по каждому листу
  if FSheetResults.Count > 0 then
  begin
    Result := Result + LineEnding + 'Детали по листам:' + LineEnding;
    for i := 0 to FSheetResults.Count - 1 do
    begin
      pResult := FSheetResults[i];
      Result := Result + Format(
        '  %s: обработано %d строк, ошибок %d',
        [pResult^.SheetName, pResult^.RowsProcessed, pResult^.ErrorCount]
      ) + LineEnding;
    end;
  end;
end;

{ Вспомогательные функции }

function StringToExportCommandType(const AValue: String): TXlsxExportCommandType;
var
  lowerValue: String;
begin
  lowerValue := LowerCase(Trim(AValue));

  if lowerValue = 'getaccessfile' then
    Result := xctGetAccessFile
  else if lowerValue = 'query' then
    Result := xctQuery
  else
    Result := xctUnknown;
end;

function ExportCommandTypeToString(AType: TXlsxExportCommandType): String;
begin
  case AType of
    xctGetAccessFile: Result := 'getaccessfile';
    xctQuery: Result := 'query';
  else
    Result := 'unknown';
  end;
end;

end.
