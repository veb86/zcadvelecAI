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
 Модуль: uzvrtcmdloadtable
 Назначение: Команда загрузки выделенной таблицы в редактор
 Описание: Модуль содержит команду LoadSelectedTableToEditor, которая:
1. Проверяет выделение объектов в чертеже
2. Находит выделенную таблицу GDBObjTable
3. Загружает данные таблицы в редактор электронных таблиц
4. Обрабатывает несохранённые изменения в редакторе
 Зависимости: uzvspreadsheet_gui, uzeenttable, uzccommandsabstract
}
unit uzvrtcmdloadtable;

{$INCLUDE zengineconfig.inc}

interface

uses
 SysUtils,
 Classes,
 //LCLDialogs,
 fpspreadsheet,
 fpsTypes,
 fpspreadsheetctrls,
 uzccommandsabstract,
 uzccommandsimpl,
 uzccommandsmanager,
 uzcdrawings,
 uzcinterface,
 uzcutils,
 uzeenttable,
 uzeconsts,
 uzeTypes,
 uzeentity,
 uzgldrawcontext,
 uzctnrvectorstrings,
 uzvspreadsheet_gui,
 gzctnrVectorTypes,
 UGDBVisibleTreeArray;

// Команда загрузки выделенной таблицы в редактор
// @param Context - контекст выполнения команды
// @param operands - операнды команды
// @return результат выполнения команды
function LoadSelectedTableToEditor_com(
 const Context: TZCADCommandContext;
 operands: TCommandOperands
): TCommandResult;

implementation

uses
 //Dialogs,
 uzclog,
 uzvrtcmdinserttable;

type
// Тип результата проверки выделения
TSelectionCheckResult = (
 scrNoSelection, // Ничего не выделено
 scrMultipleTables, // Выделено несколько таблиц
 scrMixedSelection, // Выделены разные типы объектов (включая таблицу)
 scrNotATable, // Выделен один объект, но не таблица
 scrValidTable // Выделена одна таблица - корректно
 );

// Результат диалога подтверждения
TConfirmDialogResult = (
 cdrSave, // Сохранить текущую таблицу
 cdrDontSave, // Не сохранять, закрыть без сохранения
 cdrCancel // Отменить операцию
 );

// Проверяет выделение объектов и возвращает найденную таблицу
// @param outTable - выходной параметр: найденная таблица
// @param outTableCount - выходной параметр: количество найденных таблиц
// @param outTotalCount - выходной параметр: общее количество выделенных объектов
// @return результат проверки выделения
function CheckSelection(
 out outTable: PGDBObjTable;
 out outTableCount: Integer;
 out outTotalCount: Integer
 ): TSelectionCheckResult;
var
 pobj: PGDBObjEntity;
 ir: itrec;
 selCount: Integer;
 tableCount: Integer;
 foundTable: PGDBObjTable;
 poa: PGDBObjEntityTreeArray;
begin
 Result := scrNoSelection;
 outTable := nil;
 outTableCount :=0;
 outTotalCount :=0;
 foundTable := nil;

 // Получаем количество выделенных объектов
 selCount := drawings.GetCurrentDWG^.wa.param.seldesc.Selectedobjcount;
 outTotalCount := selCount;

 // Проверяем, есть ли выделенные объекты
 if selCount =0 then
 begin
 Result := scrNoSelection;
 Exit;
 end;

 // Перебираем выделенные объекты
 tableCount :=0;
 poa := @drawings.GetCurrentROOT^.ObjArray;
 pobj := poa^.beginiterate(ir);

 if pobj <> nil then
 repeat
 if pobj^.Selected then
 begin
 // Проверяем тип объекта
 if pobj^.GetObjType = GDBTableID then
 begin
 Inc(tableCount);
 foundTable := PGDBObjTable(pobj);
 end;
 end;
 pobj := poa^.iterate(ir);
 until pobj = nil;

 outTableCount := tableCount;

 // Определяем результат проверки
 if tableCount =0 then
 begin
 if selCount =1 then
 Result := scrNotATable
 else
 Result := scrMixedSelection;
 end
 else if tableCount >1 then
 begin
 Result := scrMultipleTables;
 end
 else if selCount >1 then
 begin
 // Одна таблица, но есть другие выделенные объекты
 Result := scrMixedSelection;
 end
 else
 begin
 // Ровно одна таблица выделена
 outTable := foundTable;
 Result := scrValidTable;
 end;
end;

// Проверяет, есть ли несохранённые изменения в редакторе
// @return True если есть несохранённые изменения
function HasUnsavedChanges: Boolean;
var
 workbookSource: TsWorkbookSource;
 worksheet: TsWorksheet;
 row, col: Cardinal;
 cell: PCell;
begin
 Result := False;

 // Если форма не создана или не видна - считаем, что изменений нет
 if uzvSpreadsheetForm = nil then
 Exit;

 workbookSource := uzvSpreadsheetForm.FWorkbookSource;
 if workbookSource = nil then
 Exit;

 if workbookSource.Workbook = nil then
 Exit;

 worksheet := workbookSource.Workbook.ActiveWorksheet;
 if worksheet = nil then
 Exit;

 // Проверяем наличие данных в редакторе
 // Если есть хотя бы одна заполненная ячейка - считаем, что есть данные
 for row := 0 to 10 do
 begin
 for col := 0 to 10 do
 begin
 cell := worksheet.FindCell(row, col);
 if cell <> nil then
 begin
 if Trim(worksheet.ReadAsText(cell)) <> '' then
 begin
 Result := True;
 Exit;
 end;
 end;
 end;
 end;
end;

// Показывает диалог подтверждения сохранения
// @return результат выбора пользователя
// NOTE: Диалог отключён из-за проблем с LCL Dialogs, всегда возвращаем cdrDontSave
function ShowConfirmDialog: TConfirmDialogResult;
begin
  // Всегда продолжаем без сохранения
  Result := cdrDontSave;
end;

// Загружает данные из таблицы GDBObjTable в рабочий лист
// @param table - указатель на таблицу
// @param worksheet - рабочий лист для загрузки данных
// @return True если успешно
function LoadTableDataToWorksheet(
 table: PGDBObjTable;
 worksheet: TsWorksheet
 ): Boolean;
var
 pvs: PTZctnrVectorStrings;
 pstr: PString;
 irRow, irCol: itrec;
 rowIndex: Integer;
 colIndex: Integer;
begin
 Result := False;

 if table = nil then
 begin
 programlog.LogOutFormatStr(
 'LoadTableDataToWorksheet: table is nil',
 [],
 LM_Info
 );
 Exit;
 end;

 if worksheet = nil then
 begin
 programlog.LogOutFormatStr(
 'LoadTableDataToWorksheet: worksheet is nil',
 [],
 LM_Info
 );
 Exit;
 end;

 try
 rowIndex :=0;

 // Перебираем строки таблицы
 pvs := table^.tbl.beginiterate(irRow);
 if pvs <> nil then
 repeat
 colIndex :=0;

 // Перебираем ячейки в строке
 pstr := pvs^.beginiterate(irCol);
 if pstr <> nil then
 repeat
 // Записываем значение в ячейку рабочего листа
 worksheet.WriteText(rowIndex, colIndex, pstr^);

 Inc(colIndex);
 pstr := pvs^.iterate(irCol);
 until pstr = nil;

 Inc(rowIndex);
 pvs := table^.tbl.iterate(irRow);
 until pvs = nil;

 programlog.LogOutFormatStr(
 'LoadTableDataToWorksheet: loaded %d rows to worksheet',
 [rowIndex],
 LM_Info
 );

 Result := True;
 except
 on E: Exception do
 begin
 programlog.LogOutFormatStr(
 'LoadTableDataToWorksheet: error loading data: %s',
 [E.Message],
 LM_Info
 );
 end;
 end;
end;

// Основная функция команды загрузки таблицы
function LoadSelectedTableToEditor_com(
 const Context: TZCADCommandContext;
 operands: TCommandOperands
 ): TCommandResult;
var
 selectedTable: PGDBObjTable;
 tableCount: Integer;
 totalCount: Integer;
 checkResult: TSelectionCheckResult;
 confirmResult: TConfirmDialogResult;
 workbookSource: TsWorkbookSource;
 worksheet: TsWorksheet;
 needsConfirm: Boolean;
begin
 Result := cmd_ok;

 programlog.LogOutFormatStr(
 'LoadSelectedTableToEditor_com: command started',
 [],
 LM_Info
 );

 // Шаг1: Проверяем выделение
 checkResult := CheckSelection(selectedTable, tableCount, totalCount);

 // Обрабатываем результаты проверки
 case checkResult of
 scrNoSelection:
 begin
 zcUI.TextMessage(
 'Ошибка: не выделено ни одного объекта.',
 TMWOHistoryOut
 );
 programlog.LogOutFormatStr(
 'LoadSelectedTableToEditor_com: no objects selected',
 [],
 LM_Info
 );
 Exit;
 end;

 scrMultipleTables:
 begin
 zcUI.TextMessage(
 'Ошибка: выделено более одной таблицы.',
 TMWOHistoryOut
 );
 programlog.LogOutFormatStr(
 'LoadSelectedTableToEditor_com: multiple tables selected (count=%d)',
 [tableCount],
 LM_Info
 );
 Exit;
 end;

 scrMixedSelection:
 begin
 zcUI.TextMessage(
 'Ошибка: выделено несколько объектов (таблица + другие элементы).',
 TMWOHistoryOut
 );
 programlog.LogOutFormatStr(
 'LoadSelectedTableToEditor_com: mixed selection (total=%d, tables=%d)',
 [totalCount, tableCount],
 LM_Info
 );
 Exit;
 end;

 scrNotATable:
 begin
 zcUI.TextMessage(
 'Ошибка: выделенный объект не является таблицей.',
 TMWOHistoryOut
 );
 programlog.LogOutFormatStr(
 'LoadSelectedTableToEditor_com: selected object is not a table',
 [],
 LM_Info
 );
 Exit;
 end;
 end;

 // Если дошли сюда - выделена ровно одна таблица
 programlog.LogOutFormatStr(
 'LoadSelectedTableToEditor_com: found valid table at %p',
 [Pointer(selectedTable)],
 LM_Info
 );

 // Шаг2: Проверяем наличие несохранённых изменений
 needsConfirm := HasUnsavedChanges;

 if needsConfirm then
 begin
 confirmResult := ShowConfirmDialog;

 case confirmResult of
 cdrSave:
 begin
 // Сохраняем текущую таблицу на чертёж
 // Вызываем команду вставки таблицы
 InsertTableFromEditor_com(Context, operands);
 // Продолжаем загрузку новой таблицы
 end;

 cdrCancel:
 begin
 // Отменяем операцию
 zcUI.TextMessage(
 'Операция отменена пользователем.',
 TMWOHistoryOut
 );
 programlog.LogOutFormatStr(
 'LoadSelectedTableToEditor_com: cancelled by user',
 [],
 LM_Info
 );
 Exit;
 end;

 cdrDontSave:
 begin
 // Продолжаем без сохранения
 programlog.LogOutFormatStr(
 'LoadSelectedTableToEditor_com: continuing without saving',
 [],
 LM_Info
 );
 end;
 end;
 end;

 // Шаг3: Открываем редактор таблиц
 zcUI.ShowForm('uzvspreadsheet_gui');

 // Получаем источник данных
 if uzvSpreadsheetForm = nil then
 begin
 zcUI.TextMessage(
 'Ошибка: не удалось открыть редактор таблиц.',
 TMWOHistoryOut
 );
 programlog.LogOutFormatStr(
 'LoadSelectedTableToEditor_com: failed to open spreadsheet form',
 [],
 LM_Info
 );
 Exit;
 end;

 workbookSource := uzvSpreadsheetForm.FWorkbookSource;
 if workbookSource = nil then
 begin
 zcUI.TextMessage(
 'Ошибка: источник данных редактора не инициализирован.',
 TMWOHistoryOut
 );
 Exit;
 end;

 // Создаём новую книгу
 workbookSource.CreateNewWorkbook;
 worksheet := workbookSource.Workbook.ActiveWorksheet;

 if worksheet = nil then
 begin
 zcUI.TextMessage(
 'Ошибка: не удалось создать рабочий лист.',
 TMWOHistoryOut
 );
 Exit;
 end;

 // Шаг4: Загружаем данные из таблицы в редактор
 if LoadTableDataToWorksheet(selectedTable, worksheet) then
 begin
 zcUI.TextMessage(
 'Таблица успешно загружена в редактор.',
 TMWOHistoryOut
 );

 // Перерисовываем форму
 uzvSpreadsheetForm.WorksheetGrid.Invalidate;
 end
 else
 begin
 zcUI.TextMessage(
 'Ошибка: не удалось загрузить данные таблицы в редактор.',
 TMWOHistoryOut
 );
 end;

 programlog.LogOutFormatStr(
 'LoadSelectedTableToEditor_com: command finished',
 [],
 LM_Info
 );
end;

initialization
 // Регистрируем команду в системе ZCAD
 CreateZCADCommand(
 @LoadSelectedTableToEditor_com,
 'LoadSelectedTableToEditor',
 CADWG,
0
 );

 programlog.LogOutFormatStr(
 'Команда LoadSelectedTableToEditor зарегистрирована',
 [],
 LM_Info
 );

end.
