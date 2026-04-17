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

{
  Модуль: uzeacadtable_edit
  Назначение: Операции редактирования содержимого таблицы —
  изменение текста ячеек, типа данных, формул и блоков.
  Зависимости: uzeacadtable_types, uzeacadtable_styles, uzclog
}

unit uzeacadtable_edit;
{$Mode delphi}{$H+}
{$INCLUDE zengineconfig.inc}

interface

uses
  uzeacadtable_types, uzeacadtable_styles,
  uzclog, uzbLogIntf;

// Устанавливает текст ячейки по координатам строки и столбца
procedure SetCellText(
  var ACells: array of array of TTableCell;
  ARowIdx, AColIdx: Integer;
  const AText: String);

// Инициализирует ячейку таблицы значениями по умолчанию
procedure InitTableCell(var ACell: TTableCell);

// Инициализирует двумерный массив ячеек из загруженных данных
procedure InitCellsFromLoaded(
  var ACells: array of array of TTableCell;
  const ACellTexts: array of String;
  const ACellAlignments: array of Integer;
  const ACellColSpans: array of Integer;
  const ACellRowSpans: array of Integer;
  ARowCount, AColCount: Integer);

implementation

procedure InitTableCell(var ACell: TTableCell);
begin
  ACell.DataType := cdtText;
  ACell.Text := '';
  ACell.Value := 0;
  ACell.Formula := '';
  ACell.BlockName := '';
  ACell.CellAlignment := 0;
  ACell.ColSpan := 1;
  ACell.RowSpan := 1;
  InitCellStyle(ACell.Style);
end;

procedure SetCellText(
  var ACells: array of array of TTableCell;
  ARowIdx, AColIdx: Integer;
  const AText: String);
begin
  if (ARowIdx < 0) or (ARowIdx > High(ACells)) then
    Exit;
  if (AColIdx < 0) or (AColIdx > High(ACells[ARowIdx])) then
    Exit;
  ACells[ARowIdx][AColIdx].Text := AText;
  programlog.LogOutFormatStr(
    'AcadTable: edit: SetCellText[%d,%d] = "%s"',
    [ARowIdx, AColIdx, AText], LM_Info);
end;

procedure InitCellsFromLoaded(
  var ACells: array of array of TTableCell;
  const ACellTexts: array of String;
  const ACellAlignments: array of Integer;
  const ACellColSpans: array of Integer;
  const ACellRowSpans: array of Integer;
  ARowCount, AColCount: Integer);
var
  RowIdx, ColIdx, CellIndex: Integer;
begin
  for RowIdx := 0 to ARowCount - 1 do
    for ColIdx := 0 to AColCount - 1 do
    begin
      InitTableCell(ACells[RowIdx][ColIdx]);
      CellIndex := RowIdx * AColCount + ColIdx;
      // Текст
      if (CellIndex >= 0) and
         (CellIndex <= High(ACellTexts)) then
        ACells[RowIdx][ColIdx].Text :=
          ACellTexts[CellIndex];
      // Выравнивание
      if CellIndex < Length(ACellAlignments) then
        ACells[RowIdx][ColIdx].CellAlignment :=
          ACellAlignments[CellIndex];
      // Объединение столбцов
      if CellIndex < Length(ACellColSpans) then
        ACells[RowIdx][ColIdx].ColSpan :=
          ACellColSpans[CellIndex];
      // Объединение строк
      if CellIndex < Length(ACellRowSpans) then
        ACells[RowIdx][ColIdx].RowSpan :=
          ACellRowSpans[CellIndex];
    end;
end;

end.
