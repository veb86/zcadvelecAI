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
  Модуль: uzeacadtable_cell
  Назначение: Логика работы с отдельной ячейкой таблицы —
  чтение/запись текста, разрешение стиля с учётом иерархии
  (CellAlignment > Cell > Row > Column > TableStyle > Defaults).
  Зависимости: uzeacadtable_types, uzeacadtable_styles
}

unit uzeacadtable_cell;
{$Mode delphi}{$H+}
{$INCLUDE zengineconfig.inc}

interface

uses
  uzeacadtable_types, uzeacadtable_styles,
  uzclog, uzbLogIntf;

// Возвращает текст ячейки из плоского массива текстов
function GetCellText(
  const ACellTexts: array of String;
  AColCount, ARowIdx, AColIdx: Integer): String;

// Записывает текст ячейки в плоский массив по линейному индексу
procedure SetCellTextByIndex(
  var ACellTexts: array of String;
  var ACellTextsLen: Integer;
  ACellIdx: Integer; const AText: String);

// Резолвит итоговый стиль ячейки с учётом иерархии
function ResolveCellStyle(
  ARowIdx, AColIdx: Integer;
  const ATableStyle: TTableStyle;
  const ARows: array of TTableRow;
  const ACols: array of TTableColumn;
  const ACells: array of array of TTableCell;
  ARowCount, AColCount: Integer;
  ATableFlags: Integer): TCellStyle;

// Определяет базовый стиль строки по позиции в таблице.
// RowIdx=0 → TitleCell, RowIdx=1 → HeaderCell, RowIdx>=2 → DataCell.
function GetBaseRowStyle(
  ARowIdx: Integer;
  const ATableStyle: TTableStyle): TCellStyle;

implementation

function GetCellText(
  const ACellTexts: array of String;
  AColCount, ARowIdx, AColIdx: Integer): String;
var
  CellIndex: Integer;
begin
  Result := '';
  if AColCount <= 0 then
    Exit;
  CellIndex := ARowIdx * AColCount + AColIdx;
  if (CellIndex >= 0) and (CellIndex <= High(ACellTexts)) then
    Result := ACellTexts[CellIndex];
end;

procedure SetCellTextByIndex(
  var ACellTexts: array of String;
  var ACellTextsLen: Integer;
  ACellIdx: Integer; const AText: String);
begin
  if ACellIdx < 0 then
    Exit;
  if ACellIdx >= CAcadTableMaxCells then
  begin
    programlog.LogOutFormatStr(
      'AcadTable: cell: SetCellTextByIndex — индекс %d превышает лимит %d',
      [ACellIdx, CAcadTableMaxCells], LM_Info);
    Exit;
  end;
  if ACellIdx > ACellTextsLen - 1 then
    ACellTextsLen := ACellIdx + 1;
  ACellTexts[ACellIdx] := AText;
end;

function GetBaseRowStyle(
  ARowIdx: Integer;
  const ATableStyle: TTableStyle): TCellStyle;
begin
  // Стиль назначается по индексу строки:
  //   0 → Title (Название), 1 → Header (Заголовок), >=2 → Data (Данные)
  case ARowIdx of
    0: Result := ATableStyle.TitleCell;
    1: Result := ATableStyle.HeaderCell;
  else
    Result := ATableStyle.DataCell;
  end;
end;

// Применяет переопределения из ASource к ATarget для заданных флагов
procedure ApplyStyleOverrides(
  var ATarget: TCellStyle; const ASource: TCellStyle);
begin
  if soTextStyle in ASource.Overrides then
    ATarget.TextStyle := ASource.TextStyle;
  if soTextHeight in ASource.Overrides then
    ATarget.TextHeight := ASource.TextHeight;
  if soTextColor in ASource.Overrides then
    ATarget.TextColor := ASource.TextColor;
  if soHAlign in ASource.Overrides then
    ATarget.HorzAlign := ASource.HorzAlign;
  if soVAlign in ASource.Overrides then
    ATarget.VertAlign := ASource.VertAlign;
  if soBackground in ASource.Overrides then
  begin
    ATarget.HasBackground := ASource.HasBackground;
    ATarget.BackgroundColor := ASource.BackgroundColor;
  end;
  if soBorders in ASource.Overrides then
  begin
    ATarget.Borders := ASource.Borders;
    ATarget.BorderColor := ASource.BorderColor;
  end;
end;

function ResolveCellStyle(
  ARowIdx, AColIdx: Integer;
  const ATableStyle: TTableStyle;
  const ARows: array of TTableRow;
  const ACols: array of TTableColumn;
  const ACells: array of array of TTableCell;
  ARowCount, AColCount: Integer;
  ATableFlags: Integer): TCellStyle;
var
  TableCell: TTableCell;
  RowStyle, ColStyle, BaseStyle: TCellStyle;
begin
  // Базовый стиль по позиции строки
  BaseStyle := GetBaseRowStyle(ARowIdx, ATableStyle);
  if BaseStyle.Overrides = [] then
    BaseStyle := ATableStyle.DefaultCell;

  // Стиль строки
  if (ARowIdx >= 0) and (ARowIdx <= High(ARows)) then
    RowStyle := ARows[ARowIdx].Style
  else
    InitCellStyle(RowStyle);

  // Стиль столбца
  if (AColIdx >= 0) and (AColIdx <= High(ACols)) then
    ColStyle := ACols[AColIdx].Style
  else
    InitCellStyle(ColStyle);

  // Стиль ячейки
  if (ARowIdx >= 0) and (ARowIdx < ARowCount) and
     (AColIdx >= 0) and (AColIdx < AColCount) and
     (Length(ACells) > ARowIdx) and
     (Length(ACells[ARowIdx]) > AColIdx) then
    TableCell := ACells[ARowIdx][AColIdx]
  else
    InitCellStyle(TableCell.Style);

  // Применяем от низшего приоритета к высшему
  Result := BaseStyle;
  ApplyStyleOverrides(Result, RowStyle);
  ApplyStyleOverrides(Result, ColStyle);
  ApplyStyleOverrides(Result, TableCell.Style);

  // CellAlignment из group 170 — наивысший приоритет
  if TableCell.CellAlignment > 0 then
  begin
    Result.HorzAlign := DXFAlignToHorz(TableCell.CellAlignment);
    Result.VertAlign := DXFAlignToVert(TableCell.CellAlignment);
    programlog.LogOutFormatStr(
      'AcadTable: cell: ResolveCellStyle Cell[%d,%d] ' +
      'CellAlignment=%d HorzAlign=%d VertAlign=%d TextHeight=%.2f',
      [ARowIdx, AColIdx, TableCell.CellAlignment,
       Ord(Result.HorzAlign), Ord(Result.VertAlign),
       Result.TextHeight], LM_Info);
  end;
end;

end.
