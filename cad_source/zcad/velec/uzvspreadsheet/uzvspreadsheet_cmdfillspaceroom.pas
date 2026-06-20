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

{** Модуль для заполнения параметров пространств из данных таблицы
    Содержит процедуры для обработки и обновления расширенных атрибутов }
unit uzvspreadsheet_cmdfillspaceroom;

{$INCLUDE zengineconfig.inc}

interface

uses
  SysUtils,
  Classes,
  Generics.Collections,
  uzclog,
  Dialogs,
  uzcinterface,
  uzcdrawings,
  uzeentity,
  uzeentpolyline,
  gzctnrVectorTypes,
  uzeconsts,
  uzcenitiesvariablesextender,
  uzsbVarmanDef;

type
  { Структура данных одного помещения из таблицы }
  TRoomInfo = record
    RoomPos: String;
    RoomName: String;
    RoomArea: String;
    RoomCategory: String;
  end;

  { Список помещений }
  TRoomInfoList = specialize TList<TRoomInfo>;

{ Основная процедура заполнения пространств из таблицы }
procedure FillSpacesFromTable(RoomList: TRoomInfoList);

implementation

uses
  Grids,
  fpspreadsheet,
  fpsTypes,
  fpspreadsheetctrls,
  fpspreadsheetgrid,
  uzvspreadsheet_cmdregistry;

{ Проверяет наличие переменных пространства у примитива }
function HasSpaceVariables(pEntity: PGDBObjEntity): Boolean;
var
  VarExt: TVariablesExtender;
  pvd: pvardesk;
begin
  Result := False;

  VarExt := pEntity^.specialize GetExtension<TVariablesExtender>;
  if VarExt = nil then
    Exit;

  pvd := VarExt.entityunit.FindVariable('Space_RoomPos');
  if pvd <> nil then
    Result := True;
end;

{ Получает значение переменной пространства }
function GetSpaceVariable(pEntity: PGDBObjEntity;
  const VarName: String): String;
var
  VarExt: TVariablesExtender;
  pvd: pvardesk;
begin
  Result := '';

  VarExt := pEntity^.specialize GetExtension<TVariablesExtender>;
  if VarExt = nil then
    Exit;

  pvd := VarExt.entityunit.FindVariable(VarName);
  if pvd <> nil then
    Result := pvd^.data.PTD^.GetValueAsString(pvd^.data.Addr.Instance);
end;

{ Устанавливает значение переменной пространства }
procedure SetSpaceVariable(pEntity: PGDBObjEntity;
  const VarName, Value: String);
var
  VarExt: TVariablesExtender;
  pvd: pvardesk;
begin
  VarExt := pEntity^.specialize GetExtension<TVariablesExtender>;
  if VarExt = nil then
    Exit;

  pvd := VarExt.entityunit.FindVariable(VarName);
  if pvd <> nil then
     pstring(pvd^.data.Addr.Instance)^ := Value;
    //pvd^.data.PTD^.SetValueFromString(Value, pvd^.data.Addr.Instance);
end;

//VarDesc := VarExt.entityunit.FindVariable(VarName);
//if VarDesc <> nil then
//begin
//  try
//    pstring(VarDesc^.data.Addr.Instance)^ := Value;


{ Основная процедура заполнения пространств из таблицы }
procedure FillSpacesFromTable(RoomList: TRoomInfoList);
var
  pEntity: PGDBObjEntity;
  ir: itrec;
  Room: TRoomInfo;
  SpacePos: String;
  MatchedCount: Integer;
  ProcessedCount: Integer;
  i: Integer;
begin
  programlog.LogOutFormatStr('Начало заполнения пространств помещений',
    [], LM_Info);

  // Проверка на пустой список
  if (RoomList = nil) or (RoomList.Count = 0) then
  begin
    ShowMessage('Нет данных для заполнения пространств');
    programlog.LogOutFormatStr('Список помещений пуст', [], LM_Info);
    Exit;
  end;

  programlog.LogOutFormatStr('Получено %d записей для обработки',
    [RoomList.Count], LM_Info);

  MatchedCount := 0;
  ProcessedCount := 0;

  // Перебираем все примитивы в чертеже
  pEntity := drawings.GetCurrentROOT^.ObjArray.beginiterate(ir);
  if pEntity <> nil then
    repeat
      Inc(ProcessedCount);

      // Проверяем является ли примитив полилинией с переменными пространства
      if pEntity^.GetObjType = GDBPolyLineID then
      begin
        if HasSpaceVariables(pEntity) then
        begin
          SpacePos := GetSpaceVariable(pEntity, 'Space_RoomPos');

          // Ищем совпадение с данными из таблицы
          for i := 0 to RoomList.Count - 1 do
          begin
            Room := RoomList[i];

            if SpacePos = Room.RoomPos then
            begin
              programlog.LogOutFormatStr(
                'Найдено совпадение: RoomPos "%s"',
                [Room.RoomPos], LM_Info);

              // Обновляем переменные пространства
              SetSpaceVariable(pEntity, 'Space_RoomName', Room.RoomName);
              SetSpaceVariable(pEntity, 'Space_RoomArea', Room.RoomArea);
              SetSpaceVariable(pEntity, 'Space_RoomCategory',
                Room.RoomCategory);

              Inc(MatchedCount);
              Break;
            end;
          end;
        end;
      end;

      pEntity := drawings.GetCurrentROOT^.ObjArray.iterate(ir);
    until pEntity = nil;

  programlog.LogOutFormatStr('Обработано примитивов: %d',
    [ProcessedCount], LM_Info);
  programlog.LogOutFormatStr('Обновлено пространств: %d',
    [MatchedCount], LM_Info);

  if MatchedCount = 0 then
    ShowMessage('Не найдено совпадений с позициями помещений')
  else
  begin
    ShowMessage(Format('Обновлено %d помещений', [MatchedCount]));

    // Перерисовываем чертеж для отображения изменений
    zcUI.Do_GUIaction(nil, zcMsgUIActionRedraw);
  end;
end;

{ Обработчик команды "Заполнить пространства помещений" для реестра команд.
  Считывает выделенный диапазон таблицы и заполняет параметры пространств. }
procedure CommandFillSpaceRoom(const Context: TSpreadsheetCommandContext);
var
  StartRow, EndRow, StartCol, EndCol: Integer;
  RoomList: TRoomInfoList;
  Room: TRoomInfo;
  worksheet: TsWorksheet;
  row, col: Integer;
  ColCount: Integer;
  cell: PCell;
  selection: TGridRect;
begin
  // Проверка наличия рабочей книги
  if (Context.WorkbookSource = nil) or (Context.WorkbookSource.Workbook = nil) then
  begin
    ShowMessage('Нет открытой книги');
    Exit;
  end;

  worksheet := Context.WorkbookSource.Workbook.ActiveWorksheet;
  if worksheet = nil then
  begin
    ShowMessage('Нет активного листа');
    Exit;
  end;

  if Context.WorksheetGrid = nil then
  begin
    ShowMessage('Не удалось определить выделенный диапазон');
    Exit;
  end;

  // Получаем выделенный диапазон с учётом фиксированных строк и колонок
  selection := Context.WorksheetGrid.Selection;
  StartRow := selection.Top - Context.WorksheetGrid.FixedRows;
  EndRow := selection.Bottom - Context.WorksheetGrid.FixedRows;
  StartCol := selection.Left - Context.WorksheetGrid.FixedCols;
  EndCol := selection.Right - Context.WorksheetGrid.FixedCols;

  if not ((StartRow >= 0) and (EndRow >= StartRow) and
          (StartCol >= 0) and (EndCol >= StartCol)) then
  begin
    ShowMessage('Не удалось определить выделенный диапазон');
    Exit;
  end;

  // Проверка минимального количества колонок
  ColCount := EndCol - StartCol + 1;
  if ColCount < 2 then
  begin
    ShowMessage('Минимум должно быть выделено 2-е колонки');
    Exit;
  end;

  programlog.LogOutFormatStr(
    'Заполнение пространств: выделено строк %d, колонок %d',
    [EndRow - StartRow + 1, ColCount], LM_Info);

  // Создаем список помещений
  RoomList := TRoomInfoList.Create;
  try
    // Обрабатываем каждую строку выделенного диапазона
    for row := StartRow to EndRow do
    begin
      Room.RoomPos := '';
      Room.RoomName := '';
      Room.RoomArea := '';
      Room.RoomCategory := '';

      // Читаем данные из колонок согласно таблице из ТЗ
      for col := StartCol to EndCol do
      begin
        cell := worksheet.FindCell(row, col);
        case col - StartCol of
          0: if cell <> nil then
               Room.RoomPos := worksheet.ReadAsText(cell);
          1: if cell <> nil then
               Room.RoomName := worksheet.ReadAsText(cell);
          2: if cell <> nil then
               Room.RoomArea := worksheet.ReadAsText(cell);
          3: if cell <> nil then
               Room.RoomCategory := worksheet.ReadAsText(cell);
        end;
      end;

      // Добавляем в список только если RoomPos не пустой
      if Room.RoomPos <> '' then
        RoomList.Add(Room);
    end;

    // Проверка на пустой список
    if RoomList.Count = 0 then
    begin
      ShowMessage('Нет данных для заполнения пространств');
      Exit;
    end;

    programlog.LogOutFormatStr('Подготовлено %d записей для обработки',
      [RoomList.Count], LM_Info);

    // Вызываем процедуру заполнения пространств
    FillSpacesFromTable(RoomList);

  finally
    RoomList.Free;
  end;
end;

initialization
  RegisterSpreadsheetCommand(
    'FillSpaceRoom',
    'Заполнить пространства',
    'Заполнить пространства помещений из таблицы',
    'velec/space_room',
    @CommandFillSpaceRoom,
    150,
    5
  );

end.
