{
*****************************************************************************
* *
* This file is part of the ZCAD *
* *
* See the file COPYING.txt, included in this distribution, *
* for details about the copyright. *
* *
* This program is distributed in the hope that it will be useful, *
* but WITHOUT ANY WARRANTY; without even the implied warranty of *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. *
* *
*****************************************************************************
}
{
@author(Vladimir Bobrov)
}
{$mode objfpc}{$H+}

{
 Модуль: uzvrtcmdinserttable
 Назначение: Команда вставки таблицы из редактора на чертёж
 Описание: Модуль содержит команду InsertTableFromEditor, которая:
1. Получает данные из редактора электронных таблиц
2. Определяет заполненный диапазон ячеек
3. Создаёт примитив GDBObjTable
4. Вставляет таблицу в текущий чертёж
 Зависимости: uzvspreadsheet_gui, uzeenttable, uzccommandsabstract
}
unit uzvrtcmdinserttable;

{$INCLUDE zengineconfig.inc}

interface

uses
 SysUtils,
 Classes,
 fpspreadsheet,
 fpsTypes,
 uzccommandsabstract,
 uzccommandsimpl,
 uzccommandsmanager,
 uzcdrawings,
 uzcinterface,
 uzeenttable,
 uzeconsts,
 uzeTypes,
 uzgldrawcontext,
 uzctnrvectorstrings,
 uzvspreadsheet_gui;

// Команда вставки таблицы из редактора на чертёж
// @param Context - контекст выполнения команды
// @param operands - операнды команды
// @return результат выполнения команды
function InsertTableFromEditor_com(
 const Context: TZCADCommandContext;
 operands: TCommandOperands
): TCommandResult;

implementation

uses
 uzclog;

const
 // Значения по умолчанию для таблицы
 DEFAULT_TABLE_STYLE = 'ShRaspr';
 DEFAULT_SCALE =1.0;
 // Максимальное количество строк/столбцов для проверки
 MAX_CHECK_ROWS =1000;
 MAX_CHECK_COLS =100;

// Находит последнюю заполненную строку в таблице
// @param worksheet - рабочий лист
// @return индекс последней заполненной строки (0-based) или0 если пусто
function FindLastFilledRow(worksheet: TsWorksheet): Cardinal;
var
 row, col: Cardinal;
 cell: PCell;
 maxRow: Cardinal;
 content: string;
begin
 Result :=0;
 maxRow :=0;

 if worksheet = nil then
 Exit;

 // Перебираем строки и столбцы для поиска заполненных ячеек
 for row :=0 to MAX_CHECK_ROWS do
 begin
 for col :=0 to MAX_CHECK_COLS do
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
// @return индекс последнего заполненного столбца (0-based) или0 если пусто
function FindLastFilledCol(worksheet: TsWorksheet): Cardinal;
var
 row, col: Cardinal;
 cell: PCell;
 maxCol: Cardinal;
 content: string;
begin
 Result :=0;
 maxCol :=0;

 if worksheet = nil then
 Exit;

 // Перебираем столбцы и строки для поиска заполненных ячеек
 for col :=0 to MAX_CHECK_COLS do
 begin
 for row :=0 to MAX_CHECK_ROWS do
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
 if (lastRow =0) and (lastCol =0) then
 begin
 cell := worksheet.FindCell(0,0);
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

 // Получаем стиль таблицы
 tableStyle := drawings.GetCurrentDWG^.TableStyleTable.getAddres(DEFAULT_TABLE_STYLE);
 if tableStyle <> nil then
 pt^.PTableStyle := tableStyle
 else
 begin
 // Если стиль не найден, используем первый доступный
 pt^.PTableStyle := drawings.GetCurrentDWG^.TableStyleTable.getDataMutable(0);
 end;

 // Очищаем существующие данные таблицы
 pt^.tbl.free;

 // Заполняем таблицу данными из редактора
 for row :=0 to lastRow do
 begin
 // Создаём новую строку в таблице
 pvs := pointer(pt^.tbl.CreateObject);
 pvs^.init(lastCol +1);

 for col :=0 to lastCol do
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

 // Строим геометрию таблицы
 dc := drawings.GetCurrentDWG^.CreateDrawingRC;
 pt^.Build(drawings.GetCurrentDWG^);
 pt^.FormatEntity(drawings.GetCurrentDWG^, dc);

 programlog.LogOutFormatStr(
 'CreateTableFromWorksheet: created table with %d rows, %d cols',
 [lastRow +1, lastCol +1],
 LM_Info
 );

 Result := pt;
end;

// Вставляет таблицу в основной root чертежа
// @param pt - указатель на таблицу для вставки
// @return True если успешно
function InsertTableToDrawing(pt: PGDBObjTable): Boolean;
var
 dc: TDrawContext;
begin
 Result := False;

 if pt = nil then
 begin
 programlog.LogOutFormatStr(
 'InsertTableToDrawing: table is nil',
 [],
 LM_Info
 );
 Exit;
 end;

 try
 // Меняем владельца на основной root чертежа
 pt^.bp.ListPos.Owner := drawings.GetCurrentROOT;
 drawings.GetCurrentROOT^.ObjArray.AddPEntity(pt^);

 // Форматируем таблицу
 dc := drawings.GetCurrentDWG^.CreateDrawingRC;
 pt^.FormatEntity(drawings.GetCurrentDWG^, dc);

 programlog.LogOutFormatStr(
 'InsertTableToDrawing: table inserted successfully',
 [],
 LM_Info
 );

 Result := True;
 except
 on E: Exception do
 begin
 programlog.LogOutFormatStr(
 'InsertTableToDrawing: error inserting table: %s',
 [E.Message],
 LM_Info
 );
 end;
 end;
end;

// Основная функция команды вставки таблицы
function InsertTableFromEditor_com(
 const Context: TZCADCommandContext;
 operands: TCommandOperands
): TCommandResult;
var
 workbookSource: TsWorkbookSource;
 worksheet: TsWorksheet;
 lastRow: Cardinal;
 lastCol: Cardinal;
 pt: PGDBObjTable;
begin
 Result := cmd_ok;

 programlog.LogOutFormatStr(
 'InsertTableFromEditor_com: command started',
 [],
 LM_Info
 );

 // Проверяем, открыта ли форма редактора
 if uzvSpreadsheetForm = nil then
 begin
 zcUI.TextMessage(
 'Ошибка: редактор таблиц не открыт. Откройте редактор перед выполнением команды.',
 TMWOHistoryOut
 );
 programlog.LogOutFormatStr(
 'InsertTableFromEditor_com: spreadsheet form not open',
 [],
 LM_Info
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
 programlog.LogOutFormatStr(
 'InsertTableFromEditor_com: workbook source is nil',
 [],
 LM_Info
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
 programlog.LogOutFormatStr(
 'InsertTableFromEditor_com: active worksheet is nil',
 [],
 LM_Info
 );
 Exit;
 end;

 // Определяем заполненный диапазон
 lastRow := FindLastFilledRow(worksheet);
 lastCol := FindLastFilledCol(worksheet);

 programlog.LogOutFormatStr(
 'InsertTableFromEditor_com: filled range is rows0..%d, cols0..%d',
 [lastRow, lastCol],
 LM_Info
 );

 // Проверяем, есть ли данные
 if (lastRow =0) and (lastCol =0) then
 begin
 // Проверяем первую ячейку
 if worksheet.FindCell(0,0) = nil then
 begin
 zcUI.TextMessage(
 'Таблица пуста. Нет данных для вставки.',
 TMWOHistoryOut
 );
 Exit;
 end;
 end;

 // Создаём таблицу из данных редактора
 pt := CreateTableFromWorksheet(worksheet, lastRow, lastCol);
 if pt = nil then
 begin
 zcUI.TextMessage(
 'Ошибка: не удалось создать таблицу из данных редактора',
 TMWOHistoryOut
 );
 Exit;
 end;

 // Вставляем таблицу в чертёж
 if InsertTableToDrawing(pt) then
 begin
 zcUI.TextMessage(
 Format('Таблица успешно вставлена (%d строк, %d столбцов)', 
 [lastRow +1, lastCol +1]),
 TMWOHistoryOut
 );

 // Перерисовываем чертёж
 zcRedrawCurrentDrawing;
 end
 else
 begin
 zcUI.TextMessage(
 'Ошибка: не удалось вставить таблицу в чертёж',
 TMWOHistoryOut
 );
 end;

 programlog.LogOutFormatStr(
 'InsertTableFromEditor_com: command finished',
 [],
 LM_Info
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

end.
