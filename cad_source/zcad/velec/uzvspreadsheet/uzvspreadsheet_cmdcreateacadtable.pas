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

{** Модуль команды создания таблицы ACAD_TABLE (GDBObjAcadTable) из редактора
    электронных таблиц на чертёж (issue #1357).

    Команда анализирует заполненную часть активного листа книги, строит по её
    содержимому AutoCAD-совместимую таблицу ACAD_TABLE со стилем "Standard" и
    помещает её на чертёж. В этой задаче все ячейки оформляются как данные
    (Data); отдельное отображение строк заголовка/названия (Title/Header)
    добавят будущие задачи. }
unit uzvspreadsheet_cmdcreateacadtable;

{$INCLUDE zengineconfig.inc}

interface

uses
  Classes,
  SysUtils,
  fpspreadsheet,
  fpsTypes,
  fpspreadsheetctrls,
  fpspreadsheetgrid,
  uzccommandsabstract,
  uzccommandsimpl,
  uzccommandsmanager,
  uzcdrawings,
  uzcinterface,
  uzcutils,
  uzcstrconsts,
  uzeacadtable_types,
  uzeacadtable_model,
  uzestylestablesdxf,
  uzeentsubordinated,
  uzeconsts,
  uzeTypes,
  uzegeometry,
  uzegeometrytypes,
  uzgldrawcontext,
  uzvspreadsheet_dimensions,
  uzvspreadsheet_gui;

const
  // Имя стиля таблицы по умолчанию для создаваемой ACAD_TABLE (issue #1357)
  ACADTABLE_DEFAULT_STYLE = 'Standard';
  // Максимальное количество строк/столбцов для проверки заполненности
  MAX_CHECK_ROWS = 500;
  MAX_CHECK_COLS = 50;

// Команда создания таблицы ACAD_TABLE из редактора на чертёж
// @param Context - контекст выполнения команды
// @param operands - операнды команды
// @return результат выполнения команды
function CreateAcadTableFromEditor_com(
  const Context: TZCADCommandContext;
  operands: TCommandOperands
): TCommandResult;

implementation

uses
  uzclog,
  uzvspreadsheet_cmdregistry;

// Находит последнюю заполненную строку в листе
// @param worksheet - рабочий лист
// @return индекс последней заполненной строки (0-based) или 0 если пусто
function FindLastFilledRow(worksheet: TsWorksheet): Cardinal;
var
  row, col: Cardinal;
  cell: PCell;
  maxRow: Cardinal;
  content: string;
begin
  Result := 0;
  maxRow := 0;

  if worksheet = nil then
    Exit;

  for row := 0 to MAX_CHECK_ROWS do
    for col := 0 to MAX_CHECK_COLS do
    begin
      cell := worksheet.FindCell(row, col);
      if cell <> nil then
      begin
        content := worksheet.ReadAsText(cell);
        if Trim(content) <> '' then
          if row > maxRow then
            maxRow := row;
      end;
    end;

  Result := maxRow;
end;

// Находит последний заполненный столбец в листе
// @param worksheet - рабочий лист
// @return индекс последнего заполненного столбца (0-based) или 0 если пусто
function FindLastFilledCol(worksheet: TsWorksheet): Cardinal;
var
  row, col: Cardinal;
  cell: PCell;
  maxCol: Cardinal;
  content: string;
begin
  Result := 0;
  maxCol := 0;

  if worksheet = nil then
    Exit;

  for col := 0 to MAX_CHECK_COLS do
    for row := 0 to MAX_CHECK_ROWS do
    begin
      cell := worksheet.FindCell(row, col);
      if cell <> nil then
      begin
        content := worksheet.ReadAsText(cell);
        if Trim(content) <> '' then
          if col > maxCol then
            maxCol := col;
      end;
    end;

  Result := maxCol;
end;

// Гарантирует наличие стиля таблицы DXF с заданным именем в текущем чертеже.
// Если стиль отсутствует, создаёт его (со значениями по умолчанию).
procedure EnsureDXFTableStyle(const AStyleName: string);
var
  styleTable: PGDBDXFTableStyleArray;
begin
  styleTable := @drawings.GetCurrentDWG^.DXFTableStyleTable;
  styleTable^.AddStyle(AStyleName);
end;

// Создаёт таблицу ACAD_TABLE (GDBObjAcadTable) из данных редактора в
// конструктивной области чертежа.
// @param worksheet - рабочий лист с данными
// @param lastRow - последняя заполненная строка (0-based)
// @param lastCol - последний заполненный столбец (0-based)
// @return указатель на созданный объект таблицы или nil при ошибке
function CreateAcadTableFromWorksheet(
  worksheet: TsWorksheet;
  lastRow: Cardinal;
  lastCol: Cardinal
): PGDBObjAcadTable;
var
  pt: PGDBObjAcadTable;
  row, col: Cardinal;
  cell: PCell;
  texts: TTableTextArray;
  colWidths, rowHeights: TTableSizeArray;
  rowCount, colCount: Integer;
  insPt: TzePoint3d;
  dc: TDrawContext;
begin
  Result := nil;

  if worksheet = nil then
  begin
    programlog.LogOutFormatStr(
      'CreateAcadTableFromWorksheet: worksheet is nil', [], LM_Info);
    Exit;
  end;

  // Проверяем, есть ли данные для построения
  if (lastRow = 0) and (lastCol = 0) then
  begin
    cell := worksheet.FindCell(0, 0);
    if (cell = nil) or (Trim(worksheet.ReadAsText(cell)) = '') then
    begin
      programlog.LogOutFormatStr(
        'CreateAcadTableFromWorksheet: no data to build', [], LM_Info);
      Exit;
    end;
  end;

  rowCount := Integer(lastRow) + 1;
  colCount := Integer(lastCol) + 1;

  // Собираем тексты ячеек: индекс = строка*colCount + столбец
  SetLength(texts, rowCount * colCount);
  for row := 0 to lastRow do
    for col := 0 to lastCol do
    begin
      cell := worksheet.FindCell(row, col);
      if cell <> nil then
        texts[Integer(row) * colCount + Integer(col)] :=
          worksheet.ReadAsText(cell)
      else
        texts[Integer(row) * colCount + Integer(col)] := '';
    end;

  // Собираем ширины столбцов и высоты строк листа в единицах чертежа,
  // чтобы перенести их в таблицу ACAD_TABLE (issue #1359)
  colWidths := CollectColWidths(worksheet, colCount);
  rowHeights := CollectRowHeights(worksheet, rowCount);

  // Создаём сущность и помещаем её в конструктивный root чертежа
  pt := AllocAndInitAcadTable(
    PGDBObjGenericWithSubordinated(@drawings.CurrentDWG^.ConstructObjRoot));
  drawings.CurrentDWG^.ConstructObjRoot.ObjArray.AddPEntity(pt^);

  // Строим таблицу из текстов ячеек с переносом размеров столбцов/строк
  // (точка вставки временная, нулевая — окончательное положение
  // пользователь укажет при перемещении из root)
  insPt := NulVertex;
  if not pt^.BuildFromCellTextsWithSizes(rowCount, colCount, texts,
       colWidths, rowHeights, insPt) then
  begin
    programlog.LogOutFormatStr(
      'CreateAcadTableFromWorksheet: BuildFromCellTexts failed', [], LM_Info);
    Exit;
  end;

  // Гарантируем наличие и применяем стиль "Standard" (issue #1357).
  // Если применение не удалось, остаётся стиль по умолчанию из
  // InitTableStyle, который также даёт корректную равномерную геометрию.
  EnsureDXFTableStyle(ACADTABLE_DEFAULT_STYLE);
  if not pt^.SetTableStyleName(ACADTABLE_DEFAULT_STYLE,
       drawings.GetCurrentDWG^) then
    programlog.LogOutFormatStr(
      'CreateAcadTableFromWorksheet: style "%s" not applied, using default',
      [ACADTABLE_DEFAULT_STYLE], LM_Info);

  // Строим геометрию таблицы в конструктивном root
  dc := drawings.GetCurrentDWG^.CreateDrawingRC;
  pt^.FormatEntity(drawings.GetCurrentDWG^, dc);

  programlog.LogOutFormatStr(
    'CreateAcadTableFromWorksheet: created ACAD_TABLE %d rows, %d cols',
    [rowCount, colCount], LM_Info);

  Result := pt;
end;

// Перемещает таблицу из конструктивного root в чертёж с запросом точки вставки
// @return True если пользователь указал точку и таблица успешно вставлена
function MoveAcadTableToDrawingInteractive: Boolean;
begin
  Result := False;

  if commandmanager.MoveConstructRootTo(rscmSpecifyFirstPoint) = IRNormal then
  begin
    zcMoveEntsFromConstructRootToCurrentDrawingWithUndo('CreateAcadTable');
    programlog.LogOutFormatStr(
      'MoveAcadTableToDrawingInteractive: table inserted successfully',
      [], LM_Info);
    Result := True;
  end
  else
    programlog.LogOutFormatStr(
      'MoveAcadTableToDrawingInteractive: user cancelled insertion',
      [], LM_Info);
end;

// Создаёт таблицу из редактора и помещает её в конструктивную область чертежа.
// Возвращает количество строк и столбцов через параметры.
// @return True если таблица успешно создана в конструктивной области
function BuildAcadTableInConstructRoot(out lastRow, lastCol: Cardinal): Boolean;
var
  workbookSource: TsWorkbookSource;
  worksheet: TsWorksheet;
begin
  Result := False;
  lastRow := 0;
  lastCol := 0;

  // Проверяем, открыта ли форма редактора
  if uzvSpreadsheetForm = nil then
  begin
    zcUI.TextMessage('Ошибка: редактор таблиц не открыт.', TMWOHistoryOut);
    Exit;
  end;

  // Получаем источник данных книги
  workbookSource := uzvSpreadsheetForm.FWorkbookSource;
  if workbookSource = nil then
  begin
    zcUI.TextMessage(
      'Ошибка: источник данных книги не инициализирован', TMWOHistoryOut);
    Exit;
  end;

  // Получаем активный рабочий лист
  worksheet := workbookSource.Workbook.ActiveWorksheet;
  if worksheet = nil then
  begin
    zcUI.TextMessage('Ошибка: активный лист не найден', TMWOHistoryOut);
    Exit;
  end;

  // Определяем заполненный диапазон
  lastRow := FindLastFilledRow(worksheet);
  lastCol := FindLastFilledCol(worksheet);

  programlog.LogOutFormatStr(
    'BuildAcadTableInConstructRoot: filled range rows 0..%d, cols 0..%d',
    [lastRow, lastCol], LM_Info);

  // Проверяем, есть ли данные
  if (lastRow = 0) and (lastCol = 0) then
  begin
    if worksheet.FindCell(0, 0) = nil then
    begin
      zcUI.TextMessage(
        'Таблица пуста. Нет данных для создания.', TMWOHistoryOut);
      Exit;
    end;
  end;

  // Создаём таблицу ACAD_TABLE из данных редактора в конструктивном root
  if CreateAcadTableFromWorksheet(worksheet, lastRow, lastCol) = nil then
  begin
    zcUI.TextMessage(
      'Ошибка: не удалось создать таблицу из данных редактора',
      TMWOHistoryOut);
    Exit;
  end;

  Result := True;
end;

// Основная функция команды создания таблицы ACAD_TABLE
function CreateAcadTableFromEditor_com(
  const Context: TZCADCommandContext;
  operands: TCommandOperands
): TCommandResult;
var
  lastRow, lastCol: Cardinal;
begin
  Result := cmd_ok;

  programlog.LogOutFormatStr(
    'CreateAcadTableFromEditor_com: command started', [], LM_Info);

  // Создаём таблицу в конструктивной области
  if not BuildAcadTableInConstructRoot(lastRow, lastCol) then
  begin
    Result := cmd_cancel;
    Exit;
  end;

  // Предлагаем пользователю указать точку вставки и перемещаем таблицу
  if MoveAcadTableToDrawingInteractive then
  begin
    zcUI.TextMessage(
      Format('Таблица ACAD_TABLE создана (%d строк, %d столбцов)',
        [lastRow + 1, lastCol + 1]),
      TMWOHistoryOut);
    zcRedrawCurrentDrawing;
  end
  else
    Result := cmd_cancel;

  programlog.LogOutFormatStr(
    'CreateAcadTableFromEditor_com: command finished', [], LM_Info);
end;

{ Обработчик команды "Создать таблицу ACAD_TABLE" для реестра команд.
  Выполняет зарегистрированную в ZCAD команду. }
procedure CommandCreateAcadTable(const Context: TSpreadsheetCommandContext);
begin
  commandmanager.executecommand(
    'CreateAcadTableFromEditor',
    drawings.GetCurrentDWG,
    drawings.GetCurrentOGLWParam
  );
end;

initialization
  // Регистрируем команду в системе ZCAD
  CreateZCADCommand(
    @CreateAcadTableFromEditor_com,
    'CreateAcadTableFromEditor',
    CADWG,
    0
  );

  programlog.LogOutFormatStr(
    'Команда CreateAcadTableFromEditor зарегистрирована', [], LM_Info);

  // Регистрируем кнопку команды в редакторе электронных таблиц
  RegisterSpreadsheetCommand(
    'CreateAcadTable',
    'Создать таблицу ACAD_TABLE',
    'Создать таблицу ACAD_TABLE из редактора на чертёж',
    'velec/table_insert',
    @CommandCreateAcadTable,
    161,
    6
  );

end.
