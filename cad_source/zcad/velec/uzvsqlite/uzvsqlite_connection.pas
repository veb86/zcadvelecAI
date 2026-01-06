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

unit uzvsqlite_connection;

{$INCLUDE zengineconfig.inc}

interface

uses
  SysUtils, Classes, DB,
  SQLDB, SQLite3Conn, sqlite3dyn,
  uzvsqlite_types, uzclog, uzvsqlite_config;

type
  {**
    Класс для управления подключением к базе данных SQLite

    Обеспечивает подключение, отключение, управление транзакциями
    и выполнение запросов к базе данных SQLite через sqlite3.dll
  **}
  TSQLiteConnection = class
  private
    FConnection: TSQLite3Connection;
    FTransaction: TSQLTransaction;
    FQuery: TSQLQuery;
    FConfig: TExportConfig;
    FConnected: Boolean;

    // Загрузить библиотеку SQLite
    function LoadSQLiteLibrary: Boolean;

    // Попытка подключения с повторами
    function TryConnect: Boolean;

    // Задержка перед повторной попыткой
    procedure DelayRetry(AAttempt: Integer);

  public
    constructor Create(AConfig: TExportConfig);
    destructor Destroy; override;

    // Подключиться к базе данных
    function Connect: Boolean;

    // Отключиться от базы данных
    procedure Disconnect;

    // Начать транзакцию
    procedure BeginTransaction;

    // Зафиксировать транзакцию
    procedure CommitTransaction;

    // Откатить транзакцию
    procedure RollbackTransaction;

    // Получить список таблиц EXPORT
    function ListExportTables: TStringList;

    // Открыть таблицу для чтения
    function OpenTable(const ATableName: String): TDataSet;

    // Выполнить SQL-запрос
    procedure ExecuteSQL(const ASQL: String);

    // Получить запрос для параметризованных вставок
    function GetQuery: TSQLQuery;

    property Connected: Boolean read FConnected;
  end;

implementation

uses
  StrUtils, RegExpr;

{ TSQLiteConnection }

constructor TSQLiteConnection.Create(
  AConfig: TExportConfig
);
begin
  FConfig := AConfig;
  FConnected := False;

  // Загрузка библиотеки SQLite
  if not LoadSQLiteLibrary then
    raise Exception.Create('Не удалось загрузить sqlite3.dll');

  // Создание компонентов подключения
  FConnection := TSQLite3Connection.Create(nil);
  FTransaction := TSQLTransaction.Create(nil);
  FQuery := TSQLQuery.Create(nil);

  // Настройка подключения
  FConnection.DatabaseName := FConfig.DatabasePath;

  // Привязка компонентов
  FTransaction.DataBase := FConnection;
  FConnection.Transaction := FTransaction;
  FQuery.DataBase := FConnection;
  FQuery.Transaction := FTransaction;
end;

destructor TSQLiteConnection.Destroy;
begin
  Disconnect;
  FQuery.Free;
  FTransaction.Free;
  FConnection.Free;
  inherited Destroy;
end;

function TSQLiteConnection.LoadSQLiteLibrary: Boolean;
var
  libPath: String;
begin
  // Попытка найти sqlite3.dll в различных местах
  libPath := 'sqlite3.dll';

  if not FileExists(libPath) then
  begin
    libPath := ExtractFilePath(ParamStr(0)) + 'sqlite3.dll';
    programlog.LogOutFormatStr(
      'uzvsqlite: Поиск sqlite3.dll по пути: %s',
      [libPath],
      LM_Info
    );
  end;

  if not FileExists(libPath) then
    libPath := 'C:\zcad\zcad\sqlite3.dll';

  sqlite3dyn.SQLiteDefaultLibrary := libPath;
  Result := FileExists(libPath);

  if Result then
    programlog.LogOutFormatStr(
      'uzvsqlite: Библиотека SQLite загружена: %s',
      [libPath],
      LM_Info
    )
  else
    programlog.LogOutFormatStr(
      'uzvsqlite: Не удалось найти sqlite3.dll по пути: %s',
      [libPath],
      LM_Info
    );
end;

procedure TSQLiteConnection.DelayRetry(AAttempt: Integer);
var
  delay: Integer;
begin
  // Экспоненциальная задержка: delay * 2^(attempt-1)
  delay := FConfig.RetryDelay * (1 shl (AAttempt - 1));

  if delay > 10000 then
    delay := 10000; // Максимум 10 секунд

  Sleep(delay);
end;

function TSQLiteConnection.TryConnect: Boolean;
begin
  Result := False;

  try
    // Устанавливаем путь к базе данных
    FConnection.DatabaseName := FConfig.DatabasePath;

    // Подключаемся
    if not FConnection.Connected then
      FConnection.Open;

    // Активируем транзакцию
    if not FTransaction.Active then
      FTransaction.Active := True;

    FConnected := True;
    Result := True;

    programlog.LogOutFormatStr(
      'uzvsqlite: Подключение к базе данных установлено: %s',
      [FConfig.DatabasePath],
      LM_Info
    );

  except
    on E: Exception do
    begin
      programlog.LogOutFormatStr(
        'uzvsqlite: Ошибка подключения: %s',
        [E.Message],
        LM_Info
      );
      FConnected := False;
      Result := False;
    end;
  end;
end;

function TSQLiteConnection.Connect: Boolean;
var
  attempt: Integer;
  maxAttempts: Integer;
begin
  Result := False;

  if FConnected then
  begin
    Result := True;
    Exit;
  end;

  maxAttempts := FConfig.RetryAttempts + 1;

  for attempt := 1 to maxAttempts do
  begin
    programlog.LogOutFormatStr(
      'uzvsqlite: Попытка подключения %d из %d',
      [attempt, maxAttempts],
      LM_Info
    );

    if TryConnect then
    begin
      Result := True;
      Exit;
    end;

    // Если это не последняя попытка, делаем задержку
    if attempt < maxAttempts then
      DelayRetry(attempt);
  end;

  programlog.LogOutFormatStr(
    'uzvsqlite: Не удалось подключиться после всех попыток',
    [],
    LM_Info
  );
end;

procedure TSQLiteConnection.Disconnect;
begin
  if not FConnected then
    Exit;

  try
    // Закрываем транзакцию
    if FTransaction.Active then
      FTransaction.Active := False;

    // Закрываем подключение
    if FConnection.Connected then
      FConnection.Close;

    FConnected := False;
    programlog.LogOutFormatStr(
      'uzvsqlite: Отключение от базы данных выполнено',
      [],
      LM_Info
    );

  except
    on E: Exception do
      programlog.LogOutFormatStr(
        'uzvsqlite: Ошибка отключения: %s',
        [E.Message],
        LM_Info
      );
  end;
end;

procedure TSQLiteConnection.BeginTransaction;
begin
  if not FConnected then
    raise Exception.Create('Нет подключения к базе данных');

  try
    if not FTransaction.Active then
      FTransaction.StartTransaction;


  except
    on E: Exception do
    begin
      programlog.LogOutFormatStr(
        'uzvsqlite: Ошибка начала транзакции: %s',
        [E.Message],
        LM_Info
      );
      raise;
    end;
  end;
end;

procedure TSQLiteConnection.CommitTransaction;
begin
  if not FConnected then
    raise Exception.Create('Нет подключения к базе данных');

  try
    if FTransaction.Active then
      FTransaction.Commit;


  except
    on E: Exception do
    begin
      programlog.LogOutFormatStr(
        'uzvsqlite: Ошибка фиксации транзакции: %s',
        [E.Message],
        LM_Info
      );
      raise;
    end;
  end;
end;

procedure TSQLiteConnection.RollbackTransaction;
begin
  if not FConnected then
    Exit;

  try
    if FTransaction.Active then
      FTransaction.Rollback;


  except
    on E: Exception do
      programlog.LogOutFormatStr(
        'uzvsqlite: Ошибка отката транзакции: %s',
        [E.Message],
        LM_Info
      );
  end;
end;

// Функция сравнения для сортировки таблиц EXPORT по номеру
function CompareExportTableNames(List: TStringList; Index1, Index2: Integer): Integer;
var
  num1, num2: Integer;
  name1, name2: String;
begin
  name1 := List[Index1];
  name2 := List[Index2];

  // Извлекаем номера
  num1 := StrToIntDef(Copy(name1, 7, Length(name1) - 6), 0);
  num2 := StrToIntDef(Copy(name2, 7, Length(name2) - 6), 0);

  Result := num1 - num2;
end;

function TSQLiteConnection.ListExportTables: TStringList;
var
  tables: TStringList;
  tableName: String;
  regex: TRegExpr;
  i: Integer;
begin
  Result := TStringList.Create;

  if not FConnected then
  begin
    programlog.LogOutFormatStr(
      'uzvsqlite: Нет подключения к базе данных',
      [],
      LM_Info
    );
    Exit;
  end;

  // Получаем список всех таблиц
  tables := TStringList.Create;
  try
    FConnection.GetTableNames(tables, False);

    programlog.LogOutFormatStr(
      'uzvsqlite: Получено таблиц из БД: %d',
      [tables.Count],
      LM_Info
    );

    // Фильтруем таблицы по шаблону EXPORT\d+
    regex := TRegExpr.Create('^EXPORT(\d+)$');
    try
      regex.ModifierI := True; // Регистронезависимый поиск

      for i := 0 to tables.Count - 1 do
      begin
        tableName := tables[i];
        if regex.Exec(tableName) then
        begin
          Result.Add(tableName);
          programlog.LogOutFormatStr(
            'uzvsqlite: Найдена таблица экспорта: %s',
            [tableName],
            LM_Info
          );
        end;
      end;

    finally
      regex.Free;
    end;

  finally
    tables.Free;
  end;

  // Сортируем таблицы по номеру (EXPORT1, EXPORT2, ...)
  Result.CustomSort(@CompareExportTableNames);

  programlog.LogOutFormatStr(
    'uzvsqlite: Всего найдено таблиц экспорта: %d',
    [Result.Count],
    LM_Info
  );
end;


function TSQLiteConnection.OpenTable(const ATableName: String): TDataSet;
var
  tempQuery: TSQLQuery;
begin
  if not FConnected then
    raise Exception.Create('Нет подключения к базе данных');

  tempQuery := TSQLQuery.Create(nil);
  tempQuery.DataBase := FConnection;
  tempQuery.Transaction := FTransaction;

  // Отключаем автоматическое получение первичных ключей
  // для обхода ошибки SQLPrimaryKeys с SQLite ODBC драйвером
  tempQuery.UsePrimaryKeyAsKey := False;

  try
    tempQuery.SQL.Text := Format('SELECT * FROM [%s]', [ATableName]);

    programlog.LogOutFormatStr(
      'uzvsqlite: Попытка открытия таблицы %s',
      [ATableName],
      LM_Info
    );

    tempQuery.Open;

    programlog.LogOutFormatStr(
      'uzvsqlite: Таблица %s открыта, строк: %d',
      [ATableName, tempQuery.RecordCount],
      LM_Info
    );

    Result := tempQuery;

  except
    on E: Exception do
    begin
      programlog.LogOutFormatStr(
        'uzvsqlite: Ошибка открытия таблицы %s: %s',
        [ATableName, E.Message],
        LM_Info
      );
      tempQuery.Free;
      raise;
    end;
  end;
end;

procedure TSQLiteConnection.ExecuteSQL(const ASQL: String);
begin
  if not FConnected then
    raise Exception.Create('Нет подключения к базе данных');

  try
    FQuery.SQL.Text := ASQL;
    FQuery.ExecSQL;


  except
    on E: Exception do
    begin
      programlog.LogOutFormatStr(
       'Ошибка выполнения SQL: ' + E.Message,[],
        LM_Info
      );
      raise;
    end;
  end;
end;

function TSQLiteConnection.GetQuery: TSQLQuery;
begin
  Result := FQuery;
end;

end.
