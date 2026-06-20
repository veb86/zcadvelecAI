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

{** Модуль команды вставки таблицы из редактора на чертёж
    Содержит логику получения данных из fpspreadsheet и создания GDBObjTable }
unit uzvspreadsheet_cmdinserttable;

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
  uzeenttable,
  uzeconsts,
  uzeTypes,
  uzgldrawcontext,
  uzctnrvectorstrings,
  uzvspreadsheet_gui;

const
  // Значения по умолчанию для таблицы
  DEFAULT_TABLE_STYLE = 'spreadsheet';
  // Максимальное количество строк/столбцов для проверки
  MAX_CHECK_ROWS = 500;
  MAX_CHECK_COLS = 50;

// Команда вставки таблицы из редактора на чертёж
// @param Context - контекст выполнения команды
// @param operands - операнды команды
// @return результат выполнения команды
function InsertTableFromEditor_com(
  const Context: TZCADCommandContext;
  operands: TCommandOperands
): TCommandResult;

// Вставляет таблицу из редактора в текущий чертёж (вызывается из GUI)
procedure InsertTableFromEditor_GUI;

implementation

uses
  uzclog,
  uzvspreadsheet_cmdregistry;

// Находит последнюю заполненную строку в таблице
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

  // Перебираем строки и столбцы для поиска заполненных ячеек
  for row := 0 to MAX_CHECK_ROWS do
  begin
    for col := 0 to MAX_CHECK_COLS do
    begin
      cell := worksheet.FindCell(row, col);
      if cell <> nil then
      begin
        content := worksheet.ReadAsText(cell);
        if Trim(content) <> '' then
        begin
          if row > maxRow then
            maxRow := row;
        end;
      end;
    end;
  end;

  Result := maxRow;
end;

// Находит последний заполненный столбец в таблице
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

  // Перебираем столбцы и строки для поиска заполненных ячеек
  for col := 0 to MAX_CHECK_COLS do
  begin
    for row := 0 to MAX_CHECK_ROWS do
    begin
      cell := worksheet.FindCell(row, col);
      if cell <> nil then
      begin
        content := worksheet.ReadAsText(cell);
        if Trim(content) <> '' then
        begin
          if col > maxCol then
            maxCol := col;
        end;
      end;
    end;
  end;

  Result := maxCol;
end;

// Создаёт таблицу GDBObjTable из данных редактора
// @param worksheet - рабочий лист с данными
// @param lastRow - последняя заполненная строка
// @param lastCol - последний заполненный столбец
// @return указатель на созданный объект таблицы
function CreateTableFromWorksheet(
  worksheet: TsWorksheet;
  lastRow: Cardinal;
  lastCol: Cardinal
): PGDBObjTable;
var
  pt: PGDBObjTable;
  pvs: PTZctnrVectorStrings;
  row, col: Cardinal;
  cell: PCell;
  cellValue: string;
  dc: TDrawContext;
  tableStyle: Pointer;
begin
  Result := nil;

  if worksheet = nil then
  begin
    programlog.LogOutFormatStr(
      'CreateTableFromWorksheet: worksheet is nil',
      [],
      LM_Info
    );
    Exit;
  end;

  // Проверяем, есть ли данные для вставки
  if (lastRow = 0) and (lastCol = 0) then
  begin
    cell := worksheet.FindCell(0, 0);
    if (cell = nil) or (Trim(worksheet.ReadAsText(cell)) = '') then
    begin
      programlog.LogOutFormatStr(
        'CreateTableFromWorksheet: no data to insert',
        [],
        LM_Info
      );
      Exit;
    end;
  end;

  // Создаём объект таблицы
  Getmem(Pointer(pt), SizeOf(GDBObjTable));
  pt^.initnul;

  // Устанавливаем владельца и добавляем в конструктивный root
  pt^.bp.ListPos.Owner := @drawings.CurrentDWG^.ConstructObjRoot;
  drawings.CurrentDWG^.ConstructObjRoot.ObjArray.AddPEntity(pt^);

  // Получаем стиль таблицы - используем 'ShRaspr' или первый доступный
  tableStyle := drawings.GetCurrentDWG^.TableStyleTable.getAddres(DEFAULT_TABLE_STYLE);
  if tableStyle = nil then
    tableStyle := drawings.GetCurrentDWG^.TableStyleTable.getDataMutable(0);
  pt^.PTableStyle := tableStyle;

  // Очищаем существующие данные таблицы
  pt^.tbl.free;

  // Заполняем таблицу данными из редактора
  for row := 0 to lastRow do
  begin
    // Создаём новую строку в таблице
    pvs := pt^.tbl.CreateObject;
    pvs^.init(lastCol + 1);

    for col := 0 to lastCol do
    begin
      cell := worksheet.FindCell(row, col);
      if cell <> nil then
        cellValue := worksheet.ReadAsText(cell)
      else
        cellValue := '';

      // Добавляем значение ячейки в строку таблицы
      pvs^.PushBackData(cellValue);
    end;
  end;

  // Строим геометрию таблицы в конструктивном root
  dc := drawings.GetCurrentDWG^.CreateDrawingRC;
  pt^.Build(drawings.GetCurrentDWG^);
  pt^.FormatEntity(drawings.GetCurrentDWG^, dc);

  programlog.LogOutFormatStr(
    'CreateTableFromWorksheet: created table with %d rows, %d cols',
    [lastRow + 1, lastCol + 1],
    LM_Info
  );

  Result := pt;
end;

// Перемещает таблицу из конструктивного root в чертёж с запросом точки вставки
// @return True если пользователь указал точку и таблица успешно вставлена
function MoveTableToDrawingInteractive: Boolean;
begin
  Result := False;

  if commandmanager.MoveConstructRootTo(rscmSpecifyFirstPoint) = IRNormal then
  begin
    zcMoveEntsFromConstructRootToCurrentDrawingWithUndo('InsertTable');

    programlog.LogOutFormatStr(
      'MoveTableToDrawingInteractive: table inserted successfully',
      [],
      LM_Info
    );

    Result := True;
  end
  else
  begin
    programlog.LogOutFormatStr(
      'MoveTableToDrawingInteractive: user cancelled insertion',
      [],
      LM_Info
    );
  end;
end;

// Создаёт таблицу из редактора и помещает её в конструктивную область чертежа.
// Возвращает количество строк и столбцов через параметры.
// @return True если таблица успешно создана в конструктивной области
function BuildTableInConstructRoot(out lastRow, lastCol: Cardinal): Boolean;
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
    zcUI.TextMessage(
      'Ошибка: редактор таблиц не открыт.',
      TMWOHistoryOut
    );
    Exit;
  end;

  // Получаем источник данных книги
  workbookSource := uzvSpreadsheetForm.FWorkbookSource;
  if workbookSource = nil then
  begin
    zcUI.TextMessage(
      'Ошибка: источник данных книги не инициализирован',
      TMWOHistoryOut
    );
    Exit;
  end;

  // Получаем активный рабочий лист
  worksheet := workbookSource.Workbook.ActiveWorksheet;
  if worksheet = nil then
  begin
    zcUI.TextMessage(
      'Ошибка: активный лист не найден',
      TMWOHistoryOut
    );
    Exit;
  end;

  // Определяем заполненный диапазон
  lastRow := FindLastFilledRow(worksheet);
  lastCol := FindLastFilledCol(worksheet);

  programlog.LogOutFormatStr(
    'BuildTableInConstructRoot: filled range is rows 0..%d, cols 0..%d',
    [lastRow, lastCol],
    LM_Info
  );

  // Проверяем, есть ли данные
  if (lastRow = 0) and (lastCol = 0) then
  begin
    if worksheet.FindCell(0, 0) = nil then
    begin
      zcUI.TextMessage(
        'Таблица пуста. Нет данных для вставки.',
        TMWOHistoryOut
      );
      Exit;
    end;
  end;

  // Создаём таблицу из данных редактора в конструктивном root
  if CreateTableFromWorksheet(worksheet, lastRow, lastCol) = nil then
  begin
    zcUI.TextMessage(
      'Ошибка: не удалось создать таблицу из данных редактора',
      TMWOHistoryOut
    );
    Exit;
  end;

  Result := True;
end;

// Вставляет таблицу из редактора в текущий чертёж (вызывается из GUI)
procedure InsertTableFromEditor_GUI;
begin
  // Данная процедура оставлена для обратной совместимости.
  // Реальная вставка с выбором точки происходит в InsertTableFromEditor_com.
  zcUI.TextMessage(
    'Используйте команду InsertTableFromEditor для вставки таблицы.',
    TMWOHistoryOut
  );
end;

// Основная функция команды вставки таблицы
function InsertTableFromEditor_com(
  const Context: TZCADCommandContext;
  operands: TCommandOperands
): TCommandResult;
var
  lastRow, lastCol: Cardinal;
begin
  Result := cmd_ok;

  programlog.LogOutFormatStr(
    'InsertTableFromEditor_com: command started',
    [],
    LM_Info
  );

  // Создаём таблицу в конструктивной области
  if not BuildTableInConstructRoot(lastRow, lastCol) then
  begin
    Result := cmd_cancel;
    Exit;
  end;

  // Предлагаем пользователю указать точку вставки и перемещаем таблицу в чертёж
  if MoveTableToDrawingInteractive then
  begin
    zcUI.TextMessage(
      Format('Таблица успешно вставлена (%d строк, %d столбцов)',
        [lastRow + 1, lastCol + 1]),
      TMWOHistoryOut
    );
    zcRedrawCurrentDrawing;
  end
  else
    Result := cmd_cancel;

  programlog.LogOutFormatStr(
    'InsertTableFromEditor_com: command finished',
    [],
    LM_Info
  );
end;

{ Обработчик команды "Вставить таблицу в чертёж" для реестра команд.
  Выполняет зарегистрированную в ZCAD команду вставки таблицы. }
procedure CommandInsertTable(const Context: TSpreadsheetCommandContext);
begin
  commandmanager.executecommand(
    'InsertTableFromEditor',
    drawings.GetCurrentDWG,
    drawings.GetCurrentOGLWParam
  );
end;

initialization
  // Регистрируем команду в системе ZCAD
  CreateZCADCommand(
    @InsertTableFromEditor_com,
    'InsertTableFromEditor',
    CADWG,
    0
  );

  programlog.LogOutFormatStr(
    'Команда InsertTableFromEditor зарегистрирована',
    [],
    LM_Info
  );

  // Регистрируем кнопку команды в редакторе электронных таблиц
  RegisterSpreadsheetCommand(
    'InsertTable',
    'Вставить таблицу',
    'Вставить таблицу из редактора в чертёж',
    'velec/table_insert',
    @CommandInsertTable,
    160,
    6
  );

end.
