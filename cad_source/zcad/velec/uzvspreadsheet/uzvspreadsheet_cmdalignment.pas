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

{** Модуль выравнивания текста внутри ячеек электронной таблицы.

    Содержит логику преобразования между индексом комбобокса выравнивания
    (9 вариантов: левое/центр/правое × верх/центр/низ) и парой выравниваний
    fpspreadsheet (TsHorAlignment, TsVertAlignment), а также применение
    выравнивания к выделенному диапазону и чтение выравнивания активной ячейки.

    Используется модулем формы (uzvspreadsheet_gui) для комбобокса на панели
    инструментов. Логика преобразования индексов вынесена в отдельные функции,
    чтобы её можно было покрыть автоматическими тестами. }
unit uzvspreadsheet_cmdalignment;

{$INCLUDE zengineconfig.inc}

interface

uses
  Classes,
  SysUtils,
  Grids,
  fpspreadsheet,
  fpsTypes,
  fpspreadsheetctrls,
  fpspreadsheetgrid;

const
  // Количество вариантов выравнивания в комбобоксе
  ALIGNMENT_OPTION_COUNT = 9;

  // Подписи вариантов выравнивания в порядке индексов комбобокса.
  // Порядок: слева направо (левое/центр/правое), сверху вниз (верх/центр/низ).
  ALIGNMENT_CAPTIONS: array[0..ALIGNMENT_OPTION_COUNT - 1] of string = (
    'По левому краю вверху',
    'По центру вверху',
    'По правому краю вверху',
    'По левому краю по центру',
    'По центру по центру',
    'По правому краю по центру',
    'По левому краю внизу',
    'По центру внизу',
    'По правому краю внизу'
  );

{ Преобразует индекс комбобокса (0..8) в пару выравниваний fpspreadsheet.
  Возвращает False, если индекс выходит за допустимые границы. }
function AlignmentIndexToAlignments(aIndex: Integer;
  out aHor: TsHorAlignment; out aVert: TsVertAlignment): Boolean;

{ Преобразует пару выравниваний fpspreadsheet в индекс комбобокса (0..8).
  Значения "по умолчанию" (haDefault/vaDefault) трактуются как левое/верхнее. }
function AlignmentsToIndex(aHor: TsHorAlignment;
  aVert: TsVertAlignment): Integer;

{ Заполняет список подписями вариантов выравнивания (для Items комбобокса) }
procedure FillAlignmentItems(aItems: TStrings);

{ Применяет выбранное выравнивание (по индексу комбобокса) ко всем ячейкам
  выделенного диапазона таблицы }
procedure ApplyAlignmentToSelection(aWorkbookSource: TsWorkbookSource;
  aWorksheetGrid: TsWorksheetGrid; aIndex: Integer);

{ Возвращает индекс комбобокса, соответствующий выравниванию активной ячейки.
  Если данные недоступны, возвращает 0 (левое/верхнее). }
function GetActiveCellAlignmentIndex(aWorkbookSource: TsWorkbookSource;
  aWorksheetGrid: TsWorksheetGrid): Integer;

implementation

uses
  uzclog,
  uzcinterface;

{ Группа горизонтального выравнивания: 0 - левое, 1 - центр, 2 - правое }
function HorToGroup(aHor: TsHorAlignment): Integer;
begin
  case aHor of
    haCenter: Result := 1;
    haRight:  Result := 2;
    else      Result := 0;  // haLeft, haDefault
  end;
end;

{ Группа вертикального выравнивания: 0 - верх, 1 - центр, 2 - низ }
function VertToGroup(aVert: TsVertAlignment): Integer;
begin
  case aVert of
    vaCenter: Result := 1;
    vaBottom: Result := 2;
    else      Result := 0;  // vaTop, vaDefault
  end;
end;

function AlignmentIndexToAlignments(aIndex: Integer;
  out aHor: TsHorAlignment; out aVert: TsVertAlignment): Boolean;
const
  HorByGroup: array[0..2] of TsHorAlignment = (haLeft, haCenter, haRight);
  VertByGroup: array[0..2] of TsVertAlignment = (vaTop, vaCenter, vaBottom);
begin
  aHor := haLeft;
  aVert := vaTop;
  Result := (aIndex >= 0) and (aIndex < ALIGNMENT_OPTION_COUNT);
  if not Result then
    Exit;

  aHor := HorByGroup[aIndex mod 3];
  aVert := VertByGroup[aIndex div 3];
end;

function AlignmentsToIndex(aHor: TsHorAlignment;
  aVert: TsVertAlignment): Integer;
begin
  Result := VertToGroup(aVert) * 3 + HorToGroup(aHor);
end;

procedure FillAlignmentItems(aItems: TStrings);
var
  i: Integer;
begin
  if aItems = nil then
    Exit;

  aItems.BeginUpdate;
  try
    aItems.Clear;
    for i := 0 to ALIGNMENT_OPTION_COUNT - 1 do
      aItems.Add(ALIGNMENT_CAPTIONS[i]);
  finally
    aItems.EndUpdate;
  end;
end;

{ Возвращает выделенный диапазон в координатах листа (без фиксированных
  заголовков) с упорядоченными границами. False, если диапазон некорректен. }
function GetNormalizedSelection(aWorksheetGrid: TsWorksheetGrid;
  out aStartRow, aEndRow, aStartCol, aEndCol: Integer): Boolean;
var
  selection: TGridRect;
  temp: Integer;
begin
  Result := False;
  aStartRow := 0;
  aEndRow := 0;
  aStartCol := 0;
  aEndCol := 0;

  if aWorksheetGrid = nil then
    Exit;

  selection := aWorksheetGrid.Selection;

  aStartRow := selection.Top - aWorksheetGrid.FixedRows;
  aEndRow := selection.Bottom - aWorksheetGrid.FixedRows;
  aStartCol := selection.Left - aWorksheetGrid.FixedCols;
  aEndCol := selection.Right - aWorksheetGrid.FixedCols;

  if aStartRow > aEndRow then
  begin
    temp := aStartRow;
    aStartRow := aEndRow;
    aEndRow := temp;
  end;

  if aStartCol > aEndCol then
  begin
    temp := aStartCol;
    aStartCol := aEndCol;
    aEndCol := temp;
  end;

  Result := (aStartRow >= 0) and (aStartCol >= 0) and
            (aEndRow >= aStartRow) and (aEndCol >= aStartCol);
end;

procedure ApplyAlignmentToSelection(aWorkbookSource: TsWorkbookSource;
  aWorksheetGrid: TsWorksheetGrid; aIndex: Integer);
var
  worksheet: TsWorksheet;
  hor: TsHorAlignment;
  vert: TsVertAlignment;
  startRow, endRow, startCol, endCol: Integer;
  row, col: Integer;
begin
  if (aWorkbookSource = nil) or (aWorksheetGrid = nil) then
    Exit;

  if aWorkbookSource.Workbook = nil then
    Exit;

  worksheet := aWorkbookSource.Workbook.ActiveWorksheet;
  if worksheet = nil then
    Exit;

  if not AlignmentIndexToAlignments(aIndex, hor, vert) then
    Exit;

  if not GetNormalizedSelection(aWorksheetGrid, startRow, endRow,
    startCol, endCol) then
    Exit;

  try
    for row := startRow to endRow do
      for col := startCol to endCol do
      begin
        worksheet.WriteHorAlignment(Cardinal(row), Cardinal(col), hor);
        worksheet.WriteVertAlignment(Cardinal(row), Cardinal(col), vert);
      end;

    programlog.LogOutFormatStr(
      'Выравнивание ячеек [%d,%d]-[%d,%d] изменено на "%s"',
      [startRow, startCol, endRow, endCol, ALIGNMENT_CAPTIONS[aIndex]],
      LM_Info
    );
  except
    on E: Exception do
    begin
      programlog.LogOutFormatStr(
        'Ошибка изменения выравнивания: %s',
        [E.Message],
        LM_Error
      );
      zcUI.TextMessage('Ошибка изменения выравнивания: ' + E.Message,
        TMWOHistoryOut);
    end;
  end;
end;

function GetActiveCellAlignmentIndex(aWorkbookSource: TsWorkbookSource;
  aWorksheetGrid: TsWorksheetGrid): Integer;
var
  worksheet: TsWorksheet;
  cell: PCell;
  row, col: Integer;
begin
  Result := 0;

  if (aWorkbookSource = nil) or (aWorksheetGrid = nil) then
    Exit;

  if aWorkbookSource.Workbook = nil then
    Exit;

  worksheet := aWorkbookSource.Workbook.ActiveWorksheet;
  if worksheet = nil then
    Exit;

  row := aWorksheetGrid.Row - aWorksheetGrid.FixedRows;
  col := aWorksheetGrid.Col - aWorksheetGrid.FixedCols;
  if (row < 0) or (col < 0) then
    Exit;

  cell := worksheet.FindCell(Cardinal(row), Cardinal(col));
  if cell = nil then
    Exit;

  Result := AlignmentsToIndex(worksheet.ReadHorAlignment(cell),
    worksheet.ReadVertAlignment(cell));
end;

end.
