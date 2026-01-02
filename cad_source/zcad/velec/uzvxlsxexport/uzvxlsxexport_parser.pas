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

unit uzvxlsxexport_parser;

{$INCLUDE zengineconfig.inc}

interface

uses
  SysUtils, Classes,
  uzvxlsxexport_types, uzvzcadxlsxfps, uzclog;

type
  // Парсер инструкций из листов EXPORT
  TXlsxExportParser = class
  private
    FTemplatePath: String;

    // Проверить, является ли имя листа листом экспорта
    function IsExportSheet(const ASheetName: String): Boolean;

    // Парсинг одного листа EXPORT
    function ParseExportSheet(
      const ASheetName: String
    ): TXlsxExportInstruction;

  public
    constructor Create(const ATemplatePath: String);

    // Парсить все листы EXPORT из шаблона
    function ParseAllExportSheets: TXlsxExportInstructionList;

    property TemplatePath: String read FTemplatePath;
  end;

implementation

{ TXlsxExportParser }

constructor TXlsxExportParser.Create(const ATemplatePath: String);
begin
  FTemplatePath := ATemplatePath;

  programlog.LogOutFormatStr(
    'uzvxlsxexport: Парсер создан для шаблона "%s"',
    [ATemplatePath],
    LM_Info
  );
end;

function TXlsxExportParser.IsExportSheet(const ASheetName: String): Boolean;
var
  upperName: String;
begin
  upperName := UpperCase(Trim(ASheetName));
  Result := (Length(upperName) >= 6) and (Copy(upperName, 1, 6) = 'EXPORT');
end;

function TXlsxExportParser.ParseExportSheet(
  const ASheetName: String
): TXlsxExportInstruction;
var
  instruction: TXlsxExportInstruction;
  rowIndex: Cardinal;
  cellA, cellB, cellC: String;
  commandName: String;
begin
  instruction := TXlsxExportInstruction.Create(ASheetName);

  programlog.LogOutFormatStr(
    'uzvxlsxexport: Парсинг листа "%s"',
    [ASheetName],
    LM_Info
  );

  // Читаем команды из ячеек A1, A2, A3, A4...
  rowIndex := 0;

  while rowIndex < 100 do
  begin
    // Читаем значение из столбца A
    cellA := Trim(getCellValue(ASheetName, rowIndex, 0));

    // Если ячейка пустая, останавливаемся
    if cellA = '' then
      Break;

    // Читаем параметры из столбцов B и C
    cellB := Trim(getCellValue(ASheetName, rowIndex, 1));
    cellC := Trim(getCellValue(ASheetName, rowIndex, 2));

    // Добавляем команду
    instruction.AddCommand(cellA, cellB, cellC);

    programlog.LogOutFormatStr(
      'uzvxlsxexport: Найдена команда "%s" с параметрами "%s", "%s"',
      [cellA, cellB, cellC],
      LM_Info
    );

    Inc(rowIndex);
  end;

  Result := instruction;
end;

function TXlsxExportParser.ParseAllExportSheets: TXlsxExportInstructionList;
var
  instructions: TXlsxExportInstructionList;
  sheetNum: Integer;
  sheetName: String;
  instruction: TXlsxExportInstruction;
begin
  instructions := TXlsxExportInstructionList.Create;

  programlog.LogOutFormatStr(
    'uzvxlsxexport: Начало парсинга всех листов EXPORT',
    [],
    LM_Info
  );

  // Открываем файл шаблона
  if not openXLSXFile(FTemplatePath) then
  begin
    programlog.LogOutFormatStr(
      'uzvxlsxexport: Ошибка открытия файла шаблона "%s"',
      [FTemplatePath],
      LM_Info
    );
    Result := instructions;
    Exit;
  end;

  // Перебираем возможные имена листов: EXPORT1, EXPORT2, ...
  sheetNum := 1;
  while sheetNum <= 99 do
  begin
    sheetName := 'EXPORT' + IntToStr(sheetNum);

    // Проверяем, существует ли такой лист
    if getNumWorkSheetName(sheetName) >= 0 then
    begin
      programlog.LogOutFormatStr(
        'uzvxlsxexport: Найден лист "%s"',
        [sheetName],
        LM_Info
      );

      // Парсим лист
      instruction := ParseExportSheet(sheetName);
      instructions.PushBack(instruction);
    end
    else
    begin
      // Лист не найден, прекращаем поиск
      Break;
    end;

    Inc(sheetNum);
  end;

  programlog.LogOutFormatStr(
    'uzvxlsxexport: Парсинг завершен, найдено листов: %d',
    [instructions.Size],
    LM_Info
  );

  Result := instructions;
end;

end.
