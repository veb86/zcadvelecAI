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

{** Модуль команды объединения ячеек таблицы }
unit uzvspreadsheet_cmdmergecells;

{$INCLUDE zengineconfig.inc}

interface

uses
  Classes,
  SysUtils,
  Grids,
  fpspreadsheet,
  fpspreadsheetctrls,
  fpspreadsheetgrid;

{ Объединяет выделенные ячейки }
procedure ExecuteMergeCells(aWorkbookSource: TsWorkbookSource;
  aWorksheetGrid: TsWorksheetGrid);

implementation

uses
  uzclog,
  uzcinterface;

{ Объединяет выделенные ячейки }
procedure ExecuteMergeCells(aWorkbookSource: TsWorkbookSource;
  aWorksheetGrid: TsWorksheetGrid);
var
  workbook: TsWorkbook;
  worksheet: TsWorksheet;
  selection: TGridRect;
  startRow, endRow, startCol, endCol: Integer;
  temp: Integer;
begin
  if (aWorkbookSource = nil) or (aWorksheetGrid = nil) then
  begin
    zcUI.TextMessage('Ошибка: источник данных не инициализирован',
      TMWOHistoryOut);
    Exit;
  end;

  workbook := aWorkbookSource.Workbook;
  if workbook = nil then
  begin
    zcUI.TextMessage('Ошибка: книга не загружена', TMWOHistoryOut);
    Exit;
  end;

  worksheet := workbook.ActiveWorksheet;
  if worksheet = nil then
  begin
    zcUI.TextMessage('Ошибка: активный лист не выбран', TMWOHistoryOut);
    Exit;
  end;

  try
    selection := aWorksheetGrid.Selection;

    startRow := selection.Top - aWorksheetGrid.FixedRows;
    endRow := selection.Bottom - aWorksheetGrid.FixedRows;
    startCol := selection.Left - aWorksheetGrid.FixedCols;
    endCol := selection.Right - aWorksheetGrid.FixedCols;

    if startRow > endRow then
    begin
      temp := startRow;
      startRow := endRow;
      endRow := temp;
    end;

    if startCol > endCol then
    begin
      temp := startCol;
      startCol := endCol;
      endCol := temp;
    end;

    if (startRow < 0) or (startCol < 0) then
    begin
      zcUI.TextMessage('Ошибка: выделенный диапазон выходит за пределы листа',
        TMWOHistoryOut);
      Exit;
    end;

    if (startRow = endRow) and (startCol = endCol) then
    begin
      zcUI.TextMessage('Для объединения выделите несколько ячеек',
        TMWOHistoryOut);
      Exit;
    end;

    aWorksheetGrid.MergeCells(selection);

    programlog.LogOutFormatStr(
      'Объединены ячейки [%d, %d] - [%d, %d]',
      [startRow, startCol, endRow, endCol],
      LM_Info
    );
    zcUI.TextMessage('Ячейки объединены', TMWOHistoryOut);
  except
    on E: Exception do
    begin
      programlog.LogOutFormatStr(
        'Ошибка объединения ячеек: %s',
        [E.Message],
        LM_Error
      );
      zcUI.TextMessage('Ошибка объединения ячеек: ' + E.Message,
        TMWOHistoryOut);
    end;
  end;
end;

end.
