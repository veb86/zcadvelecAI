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

{** Модуль команд назначения типа ячеек электронной таблицы (issue #1368).

    Добавляет три команды на панель инструментов редактора: «Заголовок
    таблицы» (Title), «Шапка» (Header) и «Данные» (Data). Каждая команда
    закрашивает фон выделенных ячеек определённым цветом, чтобы пользователь
    визуально видел, к какому типу относится ячейка:

      Title  — #D9E2F3 (голубой);
      Header — #E7E6E6 (серый);
      Data   — #FFFFFF (белый).

    При экспорте в GDBObjAcadTable цвет классифицируется отдельно для каждой
    ячейки и сохраняется как её собственный тип Title/Header/Data (issue
    #1409). Совместимая классификация строк сохранена для старых потребителей.
    Менять высоту текста и прочее форматирование команды не должны —
    изменяется только цвет заливки. }
unit uzvspreadsheet_cmdcellstyle;

{$INCLUDE zengineconfig.inc}

interface

uses
  Classes,
  SysUtils,
  Types,
  Grids,
  fpspreadsheet,
  fpsTypes,
  fpspreadsheetctrls,
  fpspreadsheetgrid;

type
  { Тип ячейки/строки таблицы для классификации по цвету заливки }
  TZVCellStyleKind = (
    zvskData,    // Данные (#FFFFFF)
    zvskHeader,  // Шапка (#E7E6E6)
    zvskTitle    // Заголовок таблицы (#D9E2F3)
  );

const
  // Цвета заливки ячеек в формате TsColor ($00BBGGRR — обратный порядок
  // байтов относительно HTML #RRGGBB).
  // Title  = #D9E2F3 -> R=D9 G=E2 B=F3 -> $00F3E2D9 (голубой)
  CZVTitleBgColor:  TsColor = $00F3E2D9;
  // Header = #E7E6E6 -> R=E7 G=E6 B=E6 -> $00E6E6E7 (серый)
  CZVHeaderBgColor: TsColor = $00E6E6E7;
  // Data   = #FFFFFF -> $00FFFFFF (белый)
  CZVDataBgColor:   TsColor = $00FFFFFF;

{ Возвращает цвет заливки, соответствующий типу ячейки }
function CellStyleKindColor(AKind: TZVCellStyleKind): TsColor;

{ Возвращает индекс базового стиля строки GDBObjAcadTable для типа ячейки:
  Title -> 0, Header -> 1, Data -> 2. }
function CellStyleKindToRowStyleIndex(AKind: TZVCellStyleKind): Integer;

{ Классифицирует цвет заливки ячейки. Цвет Title -> zvskTitle, цвет
  Header -> zvskHeader; любой другой цвет (в т.ч. белый, прозрачный,
  неустановленный) трактуется как zvskData. }
function ClassifyCellBackgroundColor(AColor: TsColor): TZVCellStyleKind;

{ Определяет тип строки ARow по цвету заливки её ячеек (столбцы 0..AColCount-1).
  Возвращает zvskTitle/zvskHeader только если ВСЕ ячейки строки относятся к
  соответствующему типу (правило единогласия), иначе zvskData. }
function DetectRowStyleKind(aWorksheet: TsWorksheet;
  ARow, AColCount: Integer): TZVCellStyleKind;

{ Собирает индексы базового стиля строк (0=Title, 1=Header, 2=Data) для всех
  строк 0..ARowCount-1 по цвету заливки ячеек. Результат предназначен для
  передачи в GDBObjAcadTable.SetRowStyleTypes при экспорте. }
function CollectRowStyleTypes(aWorksheet: TsWorksheet;
  ARowCount, AColCount: Integer): TIntegerDynArray;

{ Собирает тип каждой ячейки в порядке row-major. В отличие от
  CollectRowStyleTypes смешанные стили одной строки не схлопываются в Data. }
function CollectCellStyleTypes(aWorksheet: TsWorksheet;
  ARowCount, AColCount: Integer): TIntegerDynArray;

{ Применяет тип ячейки (цвет заливки) ко всем ячейкам выделенного диапазона }
procedure ApplyCellStyleToSelection(aWorkbookSource: TsWorkbookSource;
  aWorksheetGrid: TsWorksheetGrid; AKind: TZVCellStyleKind);

implementation

uses
  uzclog,
  uzcinterface,
  uzvspreadsheet_cmdregistry;

function CellStyleKindColor(AKind: TZVCellStyleKind): TsColor;
begin
  case AKind of
    zvskTitle:  Result := CZVTitleBgColor;
    zvskHeader: Result := CZVHeaderBgColor;
    else        Result := CZVDataBgColor;  // zvskData
  end;
end;

function CellStyleKindToRowStyleIndex(AKind: TZVCellStyleKind): Integer;
begin
  case AKind of
    zvskTitle:  Result := 0;
    zvskHeader: Result := 1;
    else        Result := 2;  // zvskData
  end;
end;

function ClassifyCellBackgroundColor(AColor: TsColor): TZVCellStyleKind;
begin
  if AColor = CZVTitleBgColor then
    Result := zvskTitle
  else if AColor = CZVHeaderBgColor then
    Result := zvskHeader
  else
    Result := zvskData;
end;

function DetectRowStyleKind(aWorksheet: TsWorksheet;
  ARow, AColCount: Integer): TZVCellStyleKind;
var
  col: Integer;
  cell: PCell;
  kind, firstKind: TZVCellStyleKind;
begin
  Result := zvskData;
  if (aWorksheet = nil) or (ARow < 0) or (AColCount <= 0) then
    Exit;

  firstKind := zvskData;
  for col := 0 to AColCount - 1 do
  begin
    cell := aWorksheet.FindCell(Cardinal(ARow), Cardinal(col));
    kind := ClassifyCellBackgroundColor(aWorksheet.ReadBackgroundColor(cell));
    if col = 0 then
      firstKind := kind
    else if kind <> firstKind then
    begin
      // Цвета ячеек строки различаются — единогласия нет, строка считается
      // строкой данных.
      Result := zvskData;
      Exit;
    end;
  end;

  Result := firstKind;
end;

function CollectRowStyleTypes(aWorksheet: TsWorksheet;
  ARowCount, AColCount: Integer): TIntegerDynArray;
var
  row: Integer;
begin
  SetLength(Result, 0);
  if (aWorksheet = nil) or (ARowCount <= 0) or (AColCount <= 0) then
    Exit;

  SetLength(Result, ARowCount);
  for row := 0 to ARowCount - 1 do
    Result[row] := CellStyleKindToRowStyleIndex(
      DetectRowStyleKind(aWorksheet, row, AColCount));
end;

function CollectCellStyleTypes(aWorksheet: TsWorksheet;
  ARowCount, AColCount: Integer): TIntegerDynArray;
var
  row, col: Integer;
  cell: PCell;
begin
  SetLength(Result, 0);
  if (aWorksheet = nil) or (ARowCount <= 0) or (AColCount <= 0) then
    Exit;

  SetLength(Result, ARowCount * AColCount);
  for row := 0 to ARowCount - 1 do
    for col := 0 to AColCount - 1 do
    begin
      cell := aWorksheet.FindCell(Cardinal(row), Cardinal(col));
      Result[row * AColCount + col] := CellStyleKindToRowStyleIndex(
        ClassifyCellBackgroundColor(aWorksheet.ReadBackgroundColor(cell)));
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

procedure ApplyCellStyleToSelection(aWorkbookSource: TsWorkbookSource;
  aWorksheetGrid: TsWorksheetGrid; AKind: TZVCellStyleKind);
var
  worksheet: TsWorksheet;
  startRow, endRow, startCol, endCol: Integer;
  row, col: Integer;
  color: TsColor;
const
  KindCaptions: array[TZVCellStyleKind] of string =
    ('Данные', 'Шапка', 'Заголовок');
begin
  if (aWorkbookSource = nil) or (aWorksheetGrid = nil) then
    Exit;

  if aWorkbookSource.Workbook = nil then
    Exit;

  worksheet := aWorkbookSource.Workbook.ActiveWorksheet;
  if worksheet = nil then
    Exit;

  if not GetNormalizedSelection(aWorksheetGrid, startRow, endRow,
    startCol, endCol) then
    Exit;

  color := CellStyleKindColor(AKind);

  try
    for row := startRow to endRow do
      for col := startCol to endCol do
        worksheet.WriteBackgroundColor(Cardinal(row), Cardinal(col), color);

    programlog.LogOutFormatStr(
      'Тип ячеек [%d,%d]-[%d,%d] изменён на "%s"',
      [startRow, startCol, endRow, endCol, KindCaptions[AKind]],
      LM_Info
    );
  except
    on E: Exception do
    begin
      programlog.LogOutFormatStr(
        'Ошибка изменения типа ячеек: %s',
        [E.Message],
        LM_Error
      );
      zcUI.TextMessage('Ошибка изменения типа ячеек: ' + E.Message,
        TMWOHistoryOut);
    end;
  end;
end;

{ Обработчики команд для реестра }
procedure CommandCellStyleTitle(const Context: TSpreadsheetCommandContext);
begin
  ApplyCellStyleToSelection(Context.WorkbookSource, Context.WorksheetGrid,
    zvskTitle);
end;

procedure CommandCellStyleHeader(const Context: TSpreadsheetCommandContext);
begin
  ApplyCellStyleToSelection(Context.WorkbookSource, Context.WorksheetGrid,
    zvskHeader);
end;

procedure CommandCellStyleData(const Context: TSpreadsheetCommandContext);
begin
  ApplyCellStyleToSelection(Context.WorkbookSource, Context.WorksheetGrid,
    zvskData);
end;

initialization
  RegisterSpreadsheetCommand(
    'CellStyleTitle',
    'Заголовок таблицы (Title)',
    'Закрасить выделенные ячейки как заголовок таблицы (#D9E2F3)',
    'velec/sheet_cell_title',
    @CommandCellStyleTitle,
    142,
    4
  );

  RegisterSpreadsheetCommand(
    'CellStyleHeader',
    'Шапка (Header)',
    'Закрасить выделенные ячейки как шапку таблицы (#E7E6E6)',
    'velec/sheet_cell_header',
    @CommandCellStyleHeader,
    143,
    4
  );

  RegisterSpreadsheetCommand(
    'CellStyleData',
    'Данные (Data)',
    'Закрасить выделенные ячейки как данные (#FFFFFF)',
    'velec/sheet_cell_data',
    @CommandCellStyleData,
    144,
    4
  );

end.
