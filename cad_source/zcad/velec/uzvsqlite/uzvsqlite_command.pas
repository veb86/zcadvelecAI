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

{**
  Модуль реализации и регистрации команды экспорта в SQLite

  Команда SQLiteExport поддерживает два режима работы:
    Mode=0 - экспорт всего чертежа (устройства, кабели, суперлинии)
    Mode=1 - экспорт только выделенных объектов

  Формат вызова: SQLiteExport(Mode, DBFileName)
    Mode      - режим экспорта (0 или 1), по умолчанию 0
    DBFileName - путь к файлу БД (необязательный)

  Зависимости: uzvsqlite_config, uzvsqlite_exporter, uzcdrawings
**}
unit uzvsqlite_command;

{$INCLUDE zengineconfig.inc}

interface

uses
  SysUtils,
  Classes,
  Dialogs,
  uzccommandsmanager,
  uzccommandsabstract,
  uzccommandsimpl,
  uzclog,
  uzcinterface,
  uzcdrawings,
  uzeentity,
  uzeconsts,
  uzetypes,
  gzctnrVectorTypes,
  uzvsqlite_types,
  uzvsqlite_config,
  uzvsqlite_exporter;

const
  // Режим экспорта всего чертежа
  EXPORT_MODE_ALL = 0;
  // Режим экспорта только выделенных объектов
  EXPORT_MODE_SELECTED = 1;
  // Фильтр для диалога выбора файла БД
  SQLITE_FILE_FILTER =
    'SQLite DB|*.db;*.sqlite;*.grist|All Files|*.*';
  // Заголовок диалога выбора файла БД
  SQLITE_DIALOG_TITLE =
    'Выберите файл базы данных SQLite';

{**Функция команды экспорта данных в SQLite}
function SQLiteExport_com(
  const Context: TZCADCommandContext;
  operands: TCommandOperands
): TCommandResult;

implementation

{**
  Разбор строки операндов команды на Mode и DBFileName

  Операнды передаются через запятую: "Mode,DBFileName"
  Если операнды пустые, Mode=0, DBFileName=''
**}
procedure ParseOperands(
  const operands: string;
  out exportMode: Integer;
  out dbFileName: string
);
var
  commaPos: Integer;
  modeStr: string;
  trimmedOps: string;
begin
  exportMode := EXPORT_MODE_ALL;
  dbFileName := '';
  trimmedOps := Trim(operands);

  if trimmedOps = '' then
    Exit;

  // Ищем разделитель (запятую) между параметрами
  commaPos := Pos(',', trimmedOps);

  if commaPos > 0 then
  begin
    modeStr := Trim(Copy(trimmedOps, 1, commaPos - 1));
    dbFileName := Trim(Copy(
      trimmedOps, commaPos + 1, Length(trimmedOps)
    ));
  end
  else
    modeStr := trimmedOps;

  // Преобразуем Mode в число
  try
    exportMode := StrToInt(modeStr);
  except
    on E: EConvertError do
    begin
      programlog.LogOutFormatStr(
        'uzvsqlite: Некорректное значение Mode="%s",'
        + ' используется 0',
        [modeStr],
        LM_Info
      );
      exportMode := EXPORT_MODE_ALL;
    end;
  end;

  // Проверяем допустимость значения Mode
  if not (exportMode in [EXPORT_MODE_ALL, EXPORT_MODE_SELECTED])
  then
  begin
    programlog.LogOutFormatStr(
      'uzvsqlite: Недопустимое значение Mode=%d,'
      + ' используется 0',
      [exportMode],
      LM_Info
    );
    exportMode := EXPORT_MODE_ALL;
  end;

  programlog.LogOutFormatStr(
    'uzvsqlite: Параметры команды: Mode=%d, DBFileName="%s"',
    [exportMode, dbFileName],
    LM_Info
  );
end;

{**
  Показать диалог выбора файла базы данных

  @return Путь к выбранному файлу или пустая строка
**}
function ShowDatabaseDialog: string;
var
  openDialog: TOpenDialog;
begin
  Result := '';

  openDialog := TOpenDialog.Create(nil);
  try
    openDialog.Title := SQLITE_DIALOG_TITLE;
    openDialog.Filter := SQLITE_FILE_FILTER;
    openDialog.Options := [ofFileMustExist, ofEnableSizing];

    if openDialog.Execute then
      Result := openDialog.FileName;
  finally
    openDialog.Free;
  end;
end;

{**
  Получить путь к папке текущего чертежа

  @return Путь к папке чертежа или пустая строка
**}
function GetDrawingDirectory: string;
var
  drawingFile: string;
begin
  Result := '';
  drawingFile := drawings.GetCurrentDWG^.GetFileName;

  if drawingFile = '' then
  begin
    programlog.LogOutFormatStr(
      'uzvsqlite: Чертёж не сохранён, путь не определён',
      [],
      LM_Info
    );
    Exit;
  end;

  Result := ExtractFilePath(drawingFile);
end;

{**
  Скопировать файл БД в папку чертежа

  Защищает оригинал от изменений — экспорт идёт в копию.
  Если файл с таким именем уже есть в папке чертежа — перезаписать.

  @param sourceFile - путь к исходному файлу БД
  @param drawingDir - путь к папке чертежа
  @return Путь к копии файла или пустая строка при ошибке
**}
function CopyDatabaseToDrawingDir(
  const sourceFile: string;
  const drawingDir: string
): string;
var
  destFile: string;
  sourceStream, destStream: TFileStream;
begin
  Result := '';
  destFile := IncludeTrailingPathDelimiter(drawingDir)
    + ExtractFileName(sourceFile);

  programlog.LogOutFormatStr(
    'uzvsqlite: Копирование БД: "%s" -> "%s"',
    [sourceFile, destFile],
    LM_Info
  );

  try
    sourceStream := TFileStream.Create(
      sourceFile, fmOpenRead or fmShareDenyWrite
    );
    try
      destStream := TFileStream.Create(
        destFile, fmCreate
      );
      try
        destStream.CopyFrom(sourceStream, 0);
      finally
        destStream.Free;
      end;
    finally
      sourceStream.Free;
    end;

    Result := destFile;

    programlog.LogOutFormatStr(
      'uzvsqlite: Файл БД скопирован: "%s"',
      [destFile],
      LM_Info
    );
  except
    on E: Exception do
    begin
      zcUI.TextMessage(
        'Ошибка копирования файла БД: ' + E.Message,
        TMWOHistoryOut
      );
      programlog.LogOutFormatStr(
        'uzvsqlite: Ошибка копирования БД: %s',
        [E.Message],
        LM_Info
      );
    end;
  end;
end;

{**
  Определить рабочий файл базы данных

  Логика:
  - Если DBFileName не передан -> диалог выбора файла
  - Если файл не найден -> диалог выбора файла
  - Если файл найден -> копировать в папку чертежа

  @param dbFileName - путь к файлу БД из параметров команды
  @return Путь к рабочему файлу БД или пустая строка
**}
function ResolveDatabasePath(
  const dbFileName: string
): string;
var
  drawingDir: string;
begin
  Result := '';

  // Если параметр не передан — открываем диалог
  if dbFileName = '' then
  begin
    programlog.LogOutFormatStr(
      'uzvsqlite: DBFileName не задан, открываем диалог',
      [],
      LM_Info
    );
    Result := ShowDatabaseDialog;
    Exit;
  end;

  // Если указанный файл не существует — открываем диалог
  if not FileExists(dbFileName) then
  begin
    programlog.LogOutFormatStr(
      'uzvsqlite: Файл не найден: "%s", открываем диалог',
      [dbFileName],
      LM_Info
    );
    zcUI.TextMessage(
      'Файл не найден: ' + dbFileName
      + '. Выберите файл вручную.',
      TMWOHistoryOut
    );
    Result := ShowDatabaseDialog;
    Exit;
  end;

  // Файл существует — копируем в папку чертежа
  drawingDir := GetDrawingDirectory;

  if drawingDir = '' then
  begin
    zcUI.TextMessage(
      'Ошибка: чертёж не сохранён.'
      + ' Сохраните чертёж перед экспортом.',
      TMWOHistoryOut
    );
    Exit;
  end;

  Result := CopyDatabaseToDrawingDir(dbFileName, drawingDir);
end;

{**
  Проверить наличие выделенных объектов на чертеже

  Проверяет, есть ли выделенные объекты допустимых типов
  (device, cable, superline)

  @return True если есть выделенные объекты нужных типов
**}
function HasSelectedEntities: Boolean;
var
  pObj: PGDBObjEntity;
  ir: itrec;
  objType: TObjID;
begin
  Result := False;

  pObj := drawings.GetCurrentROOT^.ObjArray.beginiterate(ir);

  if pObj = nil then
    Exit;

  repeat
    if pObj^.selected then
    begin
      objType := pObj^.GetObjType;
      if (objType = GDBDeviceID) or
         (objType = GDBSuperLineID) or
         (objType = GDBCableID) then
      begin
        Result := True;
        Exit;
      end;
    end;
    pObj := drawings.GetCurrentROOT^.ObjArray.iterate(ir);
  until pObj = nil;
end;

{**Вывести результаты экспорта в интерфейс}
procedure DisplayExportResults(const exportResult: TExportResult);
begin
  zcUI.TextMessage(
    StringOfChar('=', 70),
    TMWOHistoryOut
  );

  zcUI.TextMessage(
    'РЕЗУЛЬТАТЫ ЭКСПОРТА:',
    TMWOHistoryOut
  );

  zcUI.TextMessage(
    exportResult.GetSummary,
    TMWOHistoryOut
  );

  zcUI.TextMessage(
    StringOfChar('=', 70),
    TMWOHistoryOut
  );
end;

{**
  Определить код результата и вывести сообщение о завершении

  @param totalErrors - количество ошибок при экспорте
  @param exportMode - режим экспорта (0 или 1)
  @return Код результата выполнения команды
**}
function DetermineCommandResult(
  totalErrors: Integer;
  exportMode: Integer
): TCommandResult;
var
  successMsg: string;
begin
  if totalErrors > 0 then
  begin
    zcUI.TextMessage(
      'Экспорт завершён с ошибками',
      TMWOHistoryOut
    );
    Result := cmd_error;
    Exit;
  end;

  // Сообщение зависит от режима экспорта
  if exportMode = EXPORT_MODE_SELECTED then
    successMsg := 'Экспорт выделенных объектов успешно завершён'
  else
    successMsg := 'Экспорт всего чертежа успешно завершён';

  zcUI.TextMessage(successMsg, TMWOHistoryOut);
  Result := cmd_OK;
end;

{**
  Выполнить процесс экспорта данных

  @param databasePath - путь к рабочему файлу БД
  @param exportMode - режим экспорта (0=все, 1=выделенные)
  @return Код результата выполнения
**}
function PerformExport(
  const databasePath: string;
  exportMode: Integer
): TCommandResult;
var
  config: TExportConfig;
  exporter: TSQLiteExporter;
  exportResult: TExportResult;
begin
  Result := cmd_Error;

  config := TExportConfig.Create;
  try
    config.DatabasePath := databasePath;
    config.EntityMode := exportMode;

    zcUI.TextMessage(
      'Файл базы данных: ' + databasePath,
      TMWOHistoryOut
    );

    exporter := TSQLiteExporter.Create(config);
    try
      exportResult := exporter.Execute;
      try
        DisplayExportResults(exportResult);
        Result := DetermineCommandResult(
          exportResult.TotalErrors, exportMode
        );
      finally
        exportResult.Free;
      end;
    finally
      exporter.Free;
    end;
  finally
    config.Free;
  end;
end;

{**
  Функция команды экспорта данных в SQLite

  Формат вызова: SQLiteExport(Mode, DBFileName)
    Mode=0 - экспорт всего чертежа
    Mode=1 - экспорт выделенных объектов
    DBFileName - путь к файлу БД (необязательный)
**}
function SQLiteExport_com(
  const Context: TZCADCommandContext;
  operands: TCommandOperands
): TCommandResult;
var
  exportMode: Integer;
  dbFileName: string;
  databasePath: string;
begin
  zcUI.TextMessage(
    'Запуск экспорта данных в SQLite...',
    TMWOHistoryOut
  );

  programlog.LogOutFormatStr(
    'uzvsqlite: Запуск команды экспорта в SQLite',
    [],
    LM_Info
  );

  try
    // Разбор параметров команды
    ParseOperands(operands, exportMode, dbFileName);

    // При Mode=1 проверяем наличие выделенных объектов
    if exportMode = EXPORT_MODE_SELECTED then
    begin
      if not HasSelectedEntities then
      begin
        zcUI.TextMessage(
          'Не выбраны объекты для экспорта',
          TMWOHistoryOut
        );
        programlog.LogOutFormatStr(
          'uzvsqlite: Нет выделенных объектов, отмена',
          [],
          LM_Info
        );
        Result := cmd_OK;
        Exit;
      end;
    end;

    // Определяем рабочий файл БД
    databasePath := ResolveDatabasePath(dbFileName);

    if databasePath = '' then
    begin
      zcUI.TextMessage(
        'Экспорт отменён: файл не выбран',
        TMWOHistoryOut
      );
      Result := cmd_OK;
      Exit;
    end;

    // Выполняем экспорт
    Result := PerformExport(databasePath, exportMode);

  except
    on E: Exception do
    begin
      zcUI.TextMessage(
        'ОШИБКА: ' + E.Message,
        TMWOHistoryOut
      );

      programlog.LogOutFormatStr(
        'uzvsqlite: Ошибка выполнения команды: %s',
        [E.Message],
        LM_Info
      );

      Result := cmd_Error;
    end;
  end;
end;

initialization
  CreateZCADCommand(
    @SQLiteExport_com,
    'SQLiteExport',
    CADWG,
    0
  );

  programlog.LogOutFormatStr(
    'Unit "%s" initialization',
    [{$INCLUDE %FILE%}],
    LM_Info
  );

end.
