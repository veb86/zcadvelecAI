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

unit uzvsqlite_executor;

{$INCLUDE zengineconfig.inc}

interface

uses
  SysUtils, Classes, Variants, DB, SQLDB,
  uzvsqlite_types, uzclog, uzvsqlite_config,
  uzvsqlite_connection, uzvsqlite_validator,
  uzvsqlite_source_provider, uzvsqlite_sqlbuilder, uzcinterface,
  uzvsqlite_template_parser, uzvsqlite_functions, uzeentity;

type
  // Тип для хранения массива Variant в TList
  TVariantArray = array of Variant;
  PVariantArray = ^TVariantArray;


  {**
    Класс для выполнения экспорта данных в SQLite

    Обрабатывает инструкции экспорта, получает данные из источника,
    выполняет валидацию и вставляет/обновляет данные в целевой таблице
  **}
  TExportExecutor = class
  private
    FConnection: TSQLiteConnection;
    FValidator: TTypeValidator;
    FConfig: TExportConfig;
    FSqlBuilder: TSqlBuilder;
    FBatchSize: Integer;

    // Подготовить параметры запроса
    procedure PrepareQueryParams(
      AQuery: TSQLQuery;
      AInstructions: TExportInstructions;
      const AValues: array of Variant
    );

    // Вставить пакет данных
    function InsertBatch(
      const ATableName: String;
      AInstructions: TExportInstructions;
      ABatchData: TList
    ): Integer;

    // Обновить или вставить запись (UPSERT)
    function UpsertRecord(
      const ATableName: String;
      AInstructions: TExportInstructions;
      const AValues: array of Variant
    ): Boolean;

    // Проверить существование записи по ключам
    function RecordExists(
      const ATableName: String;
      AInstructions: TExportInstructions;
      const AValues: array of Variant
    ): Boolean;

    // Извлечь значения из примитива
    function ExtractValues(
      AEntity: Pointer;
      AInstructions: TExportInstructions;
      ASourceProvider: TEntitySourceProvider;
      out AValues: array of Variant
    ): Boolean;

    // Проверить, содержит ли набор инструкций плейсхолдеры цикла
    function HasLoopPlaceholders(AInstructions: TExportInstructions): Boolean;

    // Заменить плейсхолдер <lnum> на конкретное число в строке
    function ReplacePlaceholder(const ASource: String; ALoopNum: Integer): String;

    // Извлечь значения для конкретной итерации цикла
    function ExtractLoopValues(
      AEntity: Pointer;
      AInstructions: TExportInstructions;
      ASourceProvider: TEntitySourceProvider;
      ALoopNum: Integer;
      out AValues: array of Variant
    ): Boolean;

    // Извлечь все итерации цикла для одного примитива
    function ExtractAllLoopIterations(
      AEntity: Pointer;
      AInstructions: TExportInstructions;
      ASourceProvider: TEntitySourceProvider;
      out ARowsList: TList
    ): Integer;

  public
    constructor Create(
      AConnection: TSQLiteConnection;
      AValidator: TTypeValidator;
      AConfig: TExportConfig
    );
    destructor Destroy; override;

    // Выполнить экспорт для одной EXPORT-таблицы
    function ExecuteExport(
      const AExportTableName: String;
      AInstructions: TExportInstructions;
      ASourceProvider: TEntitySourceProvider
    ): TExportTableResult;
  end;

implementation

{ TExportExecutor }

constructor TExportExecutor.Create(
  AConnection: TSQLiteConnection;
  AValidator: TTypeValidator;
  AConfig: TExportConfig
);
begin
  FConnection := AConnection;
  FValidator := AValidator;
  FConfig := AConfig;
  FSqlBuilder := TSqlBuilder.Create;
  FBatchSize := AConfig.BatchSize;
end;

destructor TExportExecutor.Destroy;
begin
  FSqlBuilder.Free;
  inherited Destroy;
end;


procedure TExportExecutor.PrepareQueryParams(
  AQuery: TSQLQuery;
  AInstructions: TExportInstructions;
  const AValues: array of Variant
);
var
  i: Integer;
  mapping: TColumnMapping;
  paramName: String;
  value: Variant;
begin
  // Очищаем существующие параметры
  AQuery.Params.Clear;

  // Добавляем параметры для каждой колонки
  for i := 0 to AInstructions.ColumnMappings.Size - 1 do
  begin
    if i > High(AValues) then
      Break;

    mapping := AInstructions.ColumnMappings[i];
    value := AValues[i];
    paramName := mapping.ColumnName;

    // Создаём параметр
    if VarIsNull(value) then
    begin
      AQuery.Params.CreateParam(ftString, paramName, ptInput).Clear;
    end
    else
    begin
      case mapping.DataType of
        cdtString: //begin
          AQuery.Params.CreateParam(ftString, paramName, ptInput).AsString :=
            VarToStr(value);
          //zcUI.TextMessage('cdtString: ' + VarToStr(value), TMWOHistoryOut);
        //end;

        cdtInteger:
          AQuery.Params.CreateParam(ftInteger, paramName, ptInput).AsInteger :=
            Integer(value);

        cdtFloat:
          AQuery.Params.CreateParam(ftFloat, paramName, ptInput).AsFloat :=
            Double(value);

        cdtBoolean:
          AQuery.Params.CreateParam(ftBoolean, paramName, ptInput).AsBoolean :=
            Boolean(value);
      end;
    end;
  end;
end;

function TExportExecutor.RecordExists(
  const ATableName: String;
  AInstructions: TExportInstructions;
  const AValues: array of Variant
): Boolean;
var
  query: TSQLQuery;
  sql: String;
  i, idx: Integer;
  keyCol: String;
  value: Variant;
  count: Integer;
begin
  Result := False;

  // Если нет ключевых колонок, считаем что записи не существует
  if AInstructions.KeyColumns.Count = 0 then
    Exit;

  query := TSQLQuery.Create(nil);
  try
    query.DataBase := FConnection.GetQuery.DataBase;
    query.Transaction := FConnection.GetQuery.Transaction;

    sql := FSqlBuilder.BuildExistsSQL(ATableName, AInstructions);
    query.SQL.Text := sql;

    // Устанавливаем параметры только для ключевых колонок
    for i := 0 to AInstructions.KeyColumns.Count - 1 do
    begin
      keyCol := AInstructions.KeyColumns[i];

      // Находим индекс этой колонки в маппингах
      idx := -1;
      for idx := 0 to AInstructions.ColumnMappings.Size - 1 do
      begin
        if AInstructions.ColumnMappings[idx].ColumnName = keyCol then
          Break;
      end;

      if (idx >= 0) and (idx <= High(AValues)) then
      begin
        value := AValues[idx];
        query.Params.CreateParam(ftString, keyCol, ptInput).AsString :=
          VarToStr(value);
      end;
    end;

    // Выполняем запрос
    query.Open;
    try
      count := query.FieldByName('RecCount').AsInteger;
      Result := (count > 0);
    finally
      query.Close;
    end;

  finally
    query.Free;
  end;
end;

function TExportExecutor.UpsertRecord(
  const ATableName: String;
  AInstructions: TExportInstructions;
  const AValues: array of Variant
): Boolean;
var
  query: TSQLQuery;
  sql: String;
  exists: Boolean;
begin
  Result := False;

  query := FConnection.GetQuery;

  try
    // Проверяем существование записи
    exists := RecordExists(ATableName, AInstructions, AValues);

    if exists then
    begin
      // UPDATE
      sql := FSqlBuilder.BuildUpdateSQL(ATableName, AInstructions);
    end
    else
    begin
      // INSERT
      sql := FSqlBuilder.BuildInsertSQL(ATableName, AInstructions);
    end;

    query.SQL.Text := sql;
    PrepareQueryParams(query, AInstructions, AValues);
    query.ExecSQL;

    Result := True;

  except
    on E: Exception do
    begin
      programlog.LogOutFormatStr(
        'uzvsqlite: Ошибка при выполнении UPSERT: %s',
        [E.Message],
        LM_Info
      );
      Result := False;
    end;
  end;
end;

function TExportExecutor.InsertBatch(
  const ATableName: String;
  AInstructions: TExportInstructions;
  ABatchData: TList
): Integer;
var
  i: Integer;
  query: TSQLQuery;
  sql: String;
  values: array of Variant;
begin
  Result := 0;

  if ABatchData.Count = 0 then
    Exit;

  query := FConnection.GetQuery;
  sql := FSqlBuilder.BuildInsertSQL(ATableName, AInstructions);

  query.SQL.Text := sql;

  // Вставляем записи по одной
  for i := 0 to ABatchData.Count - 1 do
  begin
    // Извлекаем указатель на массив значений и разыменовываем его
    values := PVariantArray(ABatchData[i])^;

    try
      PrepareQueryParams(query, AInstructions, values);
      query.ExecSQL;
      Inc(Result);

    except
      on E: Exception do
      begin
        programlog.LogOutFormatStr(
          'uzvsqlite: Ошибка вставки записи %d: %s',
          [i + 1, E.Message],
          LM_Info
        );

        if FConfig.ErrorMode = emStop then
          raise;
      end;
    end;
  end;
end;

function TExportExecutor.ExtractValues(
  AEntity: Pointer;
  AInstructions: TExportInstructions;
  ASourceProvider: TEntitySourceProvider;
  out AValues: array of Variant
): Boolean;
var
  i: Integer;
  mapping: TColumnMapping;
  rawValue, convertedValue: Variant;
  parsedValue: String;
  lnum: Integer;
  context: TExportContext;
  funcName: String;
begin
  Result := True;

  // TODO: получить реальное значение lnum из контекста цикла
  // Пока используем значение по умолчанию 1
  lnum := 1;

  // Извлекаем значения для каждой колонки
  for i := 0 to AInstructions.ColumnMappings.Size - 1 do
  begin
    mapping := AInstructions.ColumnMappings[i];

    // Обработка константных значений
    if mapping.IsConstant then
    begin
      AValues[i] := mapping.DefaultValue;
      Continue;
    end;

    // Проверяем, является ли SourceParam вызовом функции
    if IsFunctionCall(mapping.SourceParam) then
    begin
      // Извлекаем имя функции
      funcName := ExtractFunctionName(mapping.SourceParam);

      // Подготавливаем контекст для вызова функции
      context.Entity := PGDBObjEntity(AEntity);
      context.LoopIndex := lnum;

      // Вызываем функцию и получаем результат
      rawValue := ProcessFunctionCall(funcName, context);

      programlog.LogOutFormatStr(
        'uzvsqlite: Функция "%s" → "%s"',
        [mapping.SourceParam, VarToStr(rawValue)],
        LM_Info
      );
    end
    // Проверяем, является ли SourceParam шаблоном
    // Шаблоны содержат @@[ или <
    else if (Pos('@@[', mapping.SourceParam) > 0) or
       (Pos('<', mapping.SourceParam) > 0) then
    begin
      // Парсим шаблон
      parsedValue := ParseTemplate(mapping.SourceParam, AEntity, lnum);
      rawValue := parsedValue;

      programlog.LogOutFormatStr(
        'uzvsqlite: Шаблон "%s" → "%s"',
        [mapping.SourceParam, parsedValue],
        LM_Info
      );
    end
    else
    begin
      // Обычный параметр - получаем через старый механизм
      rawValue := ASourceProvider.GetPropertyValue(AEntity, mapping.SourceParam);
    end;

    // Валидируем и преобразуем тип
    try
      if not FValidator.ValidateAndConvert(
        rawValue,
        mapping.DataType,
        convertedValue
      ) then
      begin
        programlog.LogOutFormatStr(
          'uzvsqlite: Ошибка валидации для колонки "%s"',
          [mapping.ColumnName],
          LM_Info
        );

        if FConfig.ErrorMode = emStop then
        begin
          Result := False;
          Exit;
        end;

        // В мягком режиме используем значение по умолчанию
        convertedValue := Null;
      end;
    except
      on E: Exception do
      begin
        programlog.LogOutFormatStr(
          'uzvsqlite: Исключение при валидации '
          + 'колонки "%s": %s',
          [mapping.ColumnName, E.Message],
          LM_Info
        );

        if FConfig.ErrorMode = emStop then
        begin
          Result := False;
          Exit;
        end;

        convertedValue := Null;
      end;
    end;

    AValues[i] := convertedValue;
  end;
end;

// Проверить, содержит ли набор инструкций плейсхолдеры цикла
function TExportExecutor.HasLoopPlaceholders(
  AInstructions: TExportInstructions
): Boolean;
var
  i: Integer;
begin
  Result := False;

  // Проверяем все маппинги на наличие плейсхолдера
  for i := 0 to AInstructions.ColumnMappings.Size - 1 do
  begin
    if AInstructions.ColumnMappings[i].HasLoopPlaceholder then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

// Заменить плейсхолдер <lnum> на конкретное число в строке
function TExportExecutor.ReplacePlaceholder(
  const ASource: String;
  ALoopNum: Integer
): String;
begin
  Result := StringReplace(
    ASource,
    LOOP_NUMBER_PLACEHOLDER,
    IntToStr(ALoopNum),
    [rfReplaceAll]
  );
end;

// Извлечь значения для конкретной итерации цикла
function TExportExecutor.ExtractLoopValues(
  AEntity: Pointer;
  AInstructions: TExportInstructions;
  ASourceProvider: TEntitySourceProvider;
  ALoopNum: Integer;
  out AValues: array of Variant
): Boolean;
var
  i: Integer;
  mapping: TColumnMapping;
  rawValue, convertedValue: Variant;
  paramName: String;
  hasAnyValue: Boolean;
  context: TExportContext;
  funcName: String;
begin
  Result := False;
  hasAnyValue := False;

  // Извлекаем значения для каждой колонки с заменой плейсхолдера
  for i := 0 to AInstructions.ColumnMappings.Size - 1 do
  begin
    mapping := AInstructions.ColumnMappings[i];

    // Обработка константных значений
    if mapping.IsConstant then
    begin
      // Для константы также заменяем плейсхолдер, если он есть
      if mapping.HasLoopPlaceholder then
        AValues[i] := ReplacePlaceholder(VarToStr(mapping.DefaultValue), ALoopNum)
      else
        AValues[i] := mapping.DefaultValue;
      Continue;
    end;

    // Специальная обработка: если SourceParam - это сам плейсхолдер или экранированный плейсхолдер
    // (например \<lnum> или <lnum>), то используем номер итерации как значение
    if (Trim(mapping.SourceParam) = LOOP_NUMBER_PLACEHOLDER) or
       (Trim(mapping.SourceParam) = '\' + LOOP_NUMBER_PLACEHOLDER) then
    begin
      AValues[i] := ALoopNum;
      hasAnyValue := True;
      Continue;
    end;

    // Проверяем, является ли SourceParam вызовом функции
    if IsFunctionCall(mapping.SourceParam) then
    begin
      // Извлекаем имя функции
      funcName := ExtractFunctionName(mapping.SourceParam);

      // Подготавливаем контекст для вызова функции
      context.Entity := PGDBObjEntity(AEntity);
      context.LoopIndex := ALoopNum;

      // Вызываем функцию и получаем результат
      rawValue := ProcessFunctionCall(funcName, context);
      hasAnyValue := True;

      programlog.LogOutFormatStr(
        'uzvsqlite: Функция "%s" (итерация %d) → "%s"',
        [mapping.SourceParam, ALoopNum, VarToStr(rawValue)],
        LM_Info
      );
    end
    else
    begin
      // Для параметров с плейсхолдером заменяем номер
      if mapping.HasLoopPlaceholder then
        paramName := ReplacePlaceholder(mapping.SourceParam, ALoopNum)
      else
        paramName := mapping.SourceParam;

      // Получаем значение из примитива
      rawValue := ASourceProvider.GetPropertyValue(AEntity, paramName);
    end;
    //zcUI.TextMessage('rawValue ' + rawValue, TMWOHistoryOut);
    // Если хотя бы один параметр с плейсхолдером вернул nil - прекращаем цикл
    if mapping.HasLoopPlaceholder and VarIsNull(rawValue) then
    begin
      programlog.LogOutFormatStr(
        'uzvsqlite: Параметр "%s" не найден, завершение цикла на итерации %d',
        [paramName, ALoopNum],
        LM_Info
      );
      Exit; // Result = False
    end;

    // Если параметр с плейсхолдером найден, отмечаем что есть данные
    if mapping.HasLoopPlaceholder and not VarIsNull(rawValue) then
      hasAnyValue := True;

    // Валидируем и преобразуем тип
    try
      if not FValidator.ValidateAndConvert(
        rawValue,
        mapping.DataType,
        convertedValue
      ) then
      begin
        programlog.LogOutFormatStr(
          'uzvsqlite: Ошибка валидации для колонки "%s" '
          + '(итерация %d)',
          [mapping.ColumnName, ALoopNum],
          LM_Info
        );

        if FConfig.ErrorMode = emStop then
          Exit; // Result = False

        // В мягком режиме используем значение по умолчанию
        convertedValue := Null;
      end;
    except
      on E: Exception do
      begin
        programlog.LogOutFormatStr(
          'uzvsqlite: Исключение при валидации колонки '
          + '"%s" (итерация %d): %s',
          [mapping.ColumnName, ALoopNum, E.Message],
          LM_Info
        );

        if FConfig.ErrorMode = emStop then
          Exit; // Result = False

        convertedValue := Null;
      end;
    end;

    AValues[i] := convertedValue;
  end;

  // Успех только если хотя бы один параметр с плейсхолдером имел значение
  Result := hasAnyValue;
end;

// Извлечь все итерации цикла для одного примитива
function TExportExecutor.ExtractAllLoopIterations(
  AEntity: Pointer;
  AInstructions: TExportInstructions;
  ASourceProvider: TEntitySourceProvider;
  out ARowsList: TList
): Integer;
var
  loopNum: Integer;
  values: TVariantArray;
  pValues: PVariantArray;
  success: Boolean;
  i:integer;
begin
  Result := 0;
  ARowsList := TList.Create;

  // Начинаем цикл с 1
  loopNum := 1;

  while True do
  begin
    // Подготавливаем массив для значений
    SetLength(values, AInstructions.ColumnMappings.Size);

    // Пытаемся извлечь значения для текущей итерации
    success := ExtractLoopValues(
      AEntity,
      AInstructions,
      ASourceProvider,
      loopNum,
      values
    );

    // Если не удалось извлечь - завершаем цикл
    if not success then
    begin
      programlog.LogOutFormatStr(
        'uzvsqlite: Цикл завершён на итерации %d (всего итераций: %d)',
        [loopNum, loopNum - 1],
        LM_Info
      );
      Break;
    end;

    // Добавляем строку данных в список
    //New(pValues);
    //pValues^ := Copy(values, 0, Length(values));
    //ARowsList.Add(pValues);
    //Inc(Result);
    New(pValues);
    SetLength(pValues^, Length(values));

    for i := 0 to High(values) do
      pValues^[i] := values[i];  // ВАЖНО: отдельное присваивание Variant

    ARowsList.Add(pValues);
    Inc(Result);



    Inc(loopNum);

    // Защита от бесконечного цикла (максимум 1000 итераций)
    if loopNum > 1000 then
    begin
      programlog.LogOutFormatStr(
        'uzvsqlite: Превышен лимит итераций цикла (1000), принудительное завершение',
        [],
        LM_Info
      );
      Break;
    end;
  end;
end;

function TExportExecutor.ExecuteExport(
  const AExportTableName: String;
  AInstructions: TExportInstructions;
  ASourceProvider: TEntitySourceProvider
): TExportTableResult;
var
  entities: TList;
  i, j, k: Integer;
  entity: Pointer;
  values: array of Variant;
  batchData: TList;
  inserted, updated: Integer;
  hasKeys: Boolean;
  hasLoopMode: Boolean;
  loopRows: TList;
  loopRowCount: Integer;
  pValues: PVariantArray;
begin
  // Инициализация результата
  Result.TableName := AExportTableName;
  Result.TargetTable := AInstructions.TargetTable;
  Result.RowsProcessed := 0;
  Result.RowsInserted := 0;
  Result.RowsUpdated := 0;
  Result.ErrorCount := 0;
  Result.ErrorMessages := TStringList.Create;
  Result.Success := True;

  programlog.LogOutFormatStr(
    'uzvsqlite: Начало экспорта из "%s" в "%s"',
    [AExportTableName, AInstructions.TargetTable],
    LM_Info
  );

  try
    // Получаем список примитивов
    entities := ASourceProvider.GetEntities(AInstructions.TypeData);
    try
      if entities.Count = 0 then
      begin
        programlog.LogOutFormatStr(
          'uzvsqlite: Нет данных для экспорта',
          [],
          LM_Info
        );
        Exit;
      end;

      programlog.LogOutFormatStr(
        'uzvsqlite: Найдено объектов для экспорта: %d',
        [entities.Count],
        LM_Info
      );

      // Подготовка массива значений
      SetLength(values, AInstructions.ColumnMappings.Size);

      // Проверяем наличие ключевых колонок
      hasKeys := AInstructions.KeyColumns.Count > 0;

      // Проверяем наличие плейсхолдеров цикла
      hasLoopMode := HasLoopPlaceholders(AInstructions);

      if hasLoopMode then
        programlog.LogOutFormatStr(
          'uzvsqlite: Обнаружен режим цикла (плейсхолдер %s)',
          [LOOP_NUMBER_PLACEHOLDER],
          LM_Info
        );

      // Начинаем транзакцию
      if not FConfig.DryRun then
        FConnection.BeginTransaction;

      try
        // ===== РЕЖИМ С ЦИКЛОМ (плейсхолдеры) =====
        if hasLoopMode then
        begin
          programlog.LogOutFormatStr(
            'uzvsqlite: Режим цикла - каждый объект может генерировать несколько строк',
            [],
            LM_Info
          );

          batchData := TList.Create;
          try
            // Обрабатываем каждый примитив
            for i := 0 to entities.Count - 1 do
            begin
              entity := entities[i];

              // Извлекаем все итерации цикла для данного примитива
              loopRowCount := ExtractAllLoopIterations(
                entity,
                AInstructions,
                ASourceProvider,
                loopRows
              );

              if loopRowCount = 0 then
              begin
                programlog.LogOutFormatStr(
                  'uzvsqlite: Объект %d: нет данных для экспорта',
                  [i + 1],
                  LM_Info
                );
                Continue;
              end;

              programlog.LogOutFormatStr(
                'uzvsqlite: Объект %d: извлечено строк: %d',
                [i + 1, loopRowCount],
                LM_Info
              );

              // Добавляем все строки в пакет
              for j := 0 to loopRows.Count - 1 do
              begin
                Inc(Result.RowsProcessed);
                batchData.Add(loopRows[j]);

                // Вставляем пакет при достижении размера батча
                if (batchData.Count >= FBatchSize) or
                   ((i = entities.Count - 1) and (j = loopRows.Count - 1)) then
                begin
                  if not FConfig.DryRun then
                  begin
                    inserted := InsertBatch(
                      AInstructions.TargetTable,
                      AInstructions,
                      batchData
                    );
                    Inc(Result.RowsInserted, inserted);
                  end;

                  // Освобождаем память для вставленных строк
                  for k := 0 to batchData.Count - 1 do
                  begin
                    pValues := batchData[k];
                    Dispose(pValues);
                  end;

                  batchData.Clear;
                end;
              end;

              // Освобождаем список loopRows (память для данных уже была
              // либо освобождена после вставки, либо будет освобождена позже)
              loopRows.Free;

              // Прогресс
              if (i + 1) mod 100 = 0 then
                programlog.LogOutFormatStr(
                  'uzvsqlite: Обработано объектов: %d / %d',
                  [i + 1, entities.Count],
                  LM_Info
                );
            end;
          finally
            // Освобождаем память для оставшихся массивов, если есть
            for j := 0 to batchData.Count - 1 do
            begin
              pValues := batchData[j];
              Dispose(pValues);
            end;

            batchData.Free;
          end;
        end
        // ===== ОБЫЧНЫЙ РЕЖИМ (без плейсхолдеров) =====
        else if hasKeys then
        begin
          // Режим UPSERT (с ключевыми колонками)
          programlog.LogOutFormatStr(
            'uzvsqlite: Режим UPSERT (обновление/вставка)',
            [],
            LM_Info
          );

          for i := 0 to entities.Count - 1 do
          begin
            entity := entities[i];

            // Извлекаем значения
            if not ExtractValues(entity, AInstructions, ASourceProvider, values) then
            begin
              Inc(Result.ErrorCount);
              Continue;
            end;

            Inc(Result.RowsProcessed);

            // Выполняем UPSERT
            if not FConfig.DryRun then
            begin
              if UpsertRecord(AInstructions.TargetTable, AInstructions, values) then
              begin
                // Не различаем вставку и обновление в этом режиме
                Inc(Result.RowsInserted);
              end
              else
              begin
                Inc(Result.ErrorCount);
              end;
            end;

            // Прогресс
            if (i + 1) mod 100 = 0 then
              programlog.LogOutFormatStr(
                'uzvsqlite: Обработано: %d / %d',
                [i + 1, entities.Count],
                LM_Info
              );
          end;
        end
        else
        begin
          // Режим только INSERT (пакетная вставка)
          programlog.LogOutFormatStr(
            'uzvsqlite: Режим INSERT (пакетная вставка)',
            [],
            LM_Info
          );

          batchData := TList.Create;
          try
            for i := 0 to entities.Count - 1 do
            begin
              entity := entities[i];

              // Извлекаем значения
              if not ExtractValues(entity, AInstructions, ASourceProvider, values) then
              begin
                Inc(Result.ErrorCount);
                Continue;
              end;

              Inc(Result.RowsProcessed);

              // Создаём новый массив для каждого устройства
              // чтобы избежать перезаписи данных при добавлении в пакет
              New(pValues);
              SetLength(pValues^, Length(values));

              // Копируем значения в новый массив
              for j := 0 to High(values) do
                pValues^[j] := values[j];

              // Добавляем указатель на новый массив в пакет
              batchData.Add(pValues);

              // Вставляем пакет при достижении размера батча
              if (batchData.Count >= FBatchSize) or (i = entities.Count - 1) then
              begin
                if not FConfig.DryRun then
                begin
                  inserted := InsertBatch(AInstructions.TargetTable,
                    AInstructions, batchData);
                  Inc(Result.RowsInserted, inserted);
                end;

                // Освобождаем память выделенную для массивов
                for j := 0 to batchData.Count - 1 do
                begin
                  SetLength(PVariantArray(batchData[j])^, 0);
                  Dispose(PVariantArray(batchData[j]));
                end;

                batchData.Clear;
              end;

              // Прогресс
              if (i + 1) mod 100 = 0 then
                programlog.LogOutFormatStr(
                  'uzvsqlite: Обработано: %d / %d',
                  [i + 1, entities.Count],
                  LM_Info
                );
            end;
          finally
            // Освобождаем память для оставшихся массивов, если есть
            for j := 0 to batchData.Count - 1 do
            begin
              SetLength(PVariantArray(batchData[j])^, 0);
              Dispose(PVariantArray(batchData[j]));
            end;

            batchData.Free;
          end;
        end;

        // Фиксируем транзакцию
        if not FConfig.DryRun then
          FConnection.CommitTransaction;

        programlog.LogOutFormatStr(
          'uzvsqlite: Экспорт завершён: обработано %d, вставлено %d, ошибок %d',
          [Result.RowsProcessed, Result.RowsInserted, Result.ErrorCount],
          LM_Info
        );

      except
        on E: Exception do
        begin
          // Откатываем транзакцию
          if not FConfig.DryRun then
            FConnection.RollbackTransaction;

          programlog.LogOutFormatStr(
            'uzvsqlite: Ошибка экспорта: %s',
            [E.Message],
            LM_Info
          );
          Result.ErrorMessages.Add(E.Message);
          Result.Success := False;

          if FConfig.ErrorMode = emStop then
            raise;
        end;
      end;

    finally
      entities.Free;
    end;

  except
    on E: Exception do
    begin
      programlog.LogOutFormatStr(
        'uzvsqlite: Критическая ошибка экспорта: %s',
        [E.Message],
        LM_Info
      );
      Result.ErrorMessages.Add(E.Message);
      Result.Success := False;
    end;
  end;
end;

end.
