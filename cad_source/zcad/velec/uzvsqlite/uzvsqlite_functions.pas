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

unit uzvsqlite_functions;

{$INCLUDE zengineconfig.inc}

interface

uses
  SysUtils, Classes, Variants,
  uzeentity, uzclog, uzvsqlite_types, uzcvariablesutils;

const
  // Префикс вызова функции (открывающая фигурная скобка)
  FUNC_PREFIX = '{';

  // Суффикс вызова функции (закрывающая фигурная скобка)
  FUNC_SUFFIX = '}';

type
  {**
    Контекст экспортируемого объекта

    Содержит указатель на примитив и вспомогательную информацию,
    используемую функциями экспорта для вычисления значений
  **}
  TExportContext = record
    Entity: PGDBObjEntity;      // Указатель на экспортируемый примитив
    LoopIndex: Integer;         // Номер итерации в цикле (если применимо)
  end;

// Проверяет, является ли строка вызовом функции
function IsFunctionCall(const AValue: String): Boolean;

// Извлекает имя функции из строки вида {FunctionName}
function ExtractFunctionName(const AValue: String): String;

// Обрабатывает вызов функции и возвращает вычисленное значение
function ProcessFunctionCall(
  const AFunctionName: String;
  const AContext: TExportContext
): Variant;

implementation

// Проверяет, является ли строка вызовом функции
function IsFunctionCall(const AValue: String): Boolean;
var
  len: Integer;
begin
  Result := False;
  len := Length(AValue);

  // Строка должна быть не короче 2 символов и начинаться с { и заканчиваться на }
  if len < 2 then
    Exit;

  if (AValue[1] = FUNC_PREFIX) and (AValue[len] = FUNC_SUFFIX) then
    Result := True;
end;

// Извлекает имя функции из строки вида {FunctionName}
function ExtractFunctionName(const AValue: String): String;
var
  len: Integer;
begin
  len := Length(AValue);

  // Убираем фигурные скобки и получаем имя функции
  if len > 2 then
    Result := LowerCase(Trim(Copy(AValue, 2, len - 2)))
  else
    Result := '';
end;

{**
  Тестовая функция 1: возвращает Boolean

  Проверяет, задано ли у объекта имя слоя.
  Возвращает True, если имя слоя непустое, иначе False.
**}
function Func_Test1(const AContext: TExportContext): Boolean;
var
  layerName: Variant;
begin
  Result := False;

  if AContext.Entity = nil then
    Exit;

  try
    // Получаем имя слоя через механизм переменных
    layerName := GetVariableValueFromSelectedEntity(
      AContext.Entity,
      'NMU_Layer'
    );

    // Проверяем, что значение не пустое
    if not VarIsNull(layerName) then
      Result := (Trim(VarToStr(layerName)) <> '');

    programlog.LogOutFormatStr(
      'uzvsqlite: Func_Test1 вызвана, результат: %s',
      [BoolToStr(Result, True)],
      LM_Info
    );

  except
    on E: Exception do
    begin
      programlog.LogOutFormatStr(
        'uzvsqlite: Ошибка в Func_Test1: %s',
        [E.Message],
        LM_Info
      );
      Result := False;
    end;
  end;
end;

{**
  Тестовая функция 2: возвращает Integer

  Возвращает количество вложенных примитивов объекта.
  Для простых примитивов возвращает 0.
**}
function Func_Test2(const AContext: TExportContext): Integer;
var
  subEntCount: Variant;
begin
  Result := 0;

  if AContext.Entity = nil then
    Exit;

  try
    // Пытаемся получить количество подобъектов
    subEntCount := GetVariableValueFromSelectedEntity(
      AContext.Entity,
      'ObjArray.Size'
    );

    if not VarIsNull(subEntCount) then
      Result := StrToIntDef(VarToStr(subEntCount), 0);

    programlog.LogOutFormatStr(
      'uzvsqlite: Func_Test2 вызвана, результат: %d',
      [Result],
      LM_Info
    );

  except
    on E: Exception do
    begin
      programlog.LogOutFormatStr(
        'uzvsqlite: Ошибка в Func_Test2: %s',
        [E.Message],
        LM_Info
      );
      Result := 0;
    end;
  end;
end;

{**
  Тестовая функция 3: возвращает Float

  Возвращает длину объекта.
  Для объектов без длины возвращает 0.0.
**}
function Func_Test3(const AContext: TExportContext): Double;
var
  length: Variant;
begin
  Result := 0.0;

  if AContext.Entity = nil then
    Exit;

  try
    // Пытаемся получить длину объекта
    length := GetVariableValueFromSelectedEntity(
      AContext.Entity,
      'Length'
    );

    if not VarIsNull(length) then
    begin
      // Пытаемся преобразовать к числу
      try
        Result := StrToFloatDef(VarToStr(length), 0.0);
      except
        Result := 0.0;
      end;
    end;

    programlog.LogOutFormatStr(
      'uzvsqlite: Func_Test3 вызвана, результат: %.2f',
      [Result],
      LM_Info
    );

  except
    on E: Exception do
    begin
      programlog.LogOutFormatStr(
        'uzvsqlite: Ошибка в Func_Test3: %s',
        [E.Message],
        LM_Info
      );
      Result := 0.0;
    end;
  end;
end;

{**
  Тестовая функция 4: возвращает String

  Возвращает строку вида 'OBJ_' + имя объекта.
  Если имя не задано, возвращает 'OBJ_NONAME'.
**}
function Func_Test4(const AContext: TExportContext): String;
var
  objName: Variant;
begin
  Result := 'OBJ_NONAME';

  if AContext.Entity = nil then
    Exit;

  try
    // Получаем имя объекта
    objName := GetVariableValueFromSelectedEntity(
      AContext.Entity,
      'Name'
    );

    if not VarIsNull(objName) then
    begin
      Result := 'OBJ_' + VarToStr(objName);
    end;

    programlog.LogOutFormatStr(
      'uzvsqlite: Func_Test4 вызвана, результат: %s',
      [Result],
      LM_Info
    );

  except
    on E: Exception do
    begin
      programlog.LogOutFormatStr(
        'uzvsqlite: Ошибка в Func_Test4: %s',
        [E.Message],
        LM_Info
      );
      Result := 'OBJ_ERROR';
    end;
  end;
end;

// Обрабатывает вызов функции и возвращает вычисленное значение
function ProcessFunctionCall(
  const AFunctionName: String;
  const AContext: TExportContext
): Variant;
var
  funcName: String;
begin
  // Приводим имя функции к нижнему регистру для сравнения
  funcName := LowerCase(Trim(AFunctionName));

  programlog.LogOutFormatStr(
    'uzvsqlite: Вызов функции экспорта: %s',
    [funcName],
    LM_Info
  );

  try
    // Диспетчеризация вызова функции
    if funcName = 'test1' then
      Result := Func_Test1(AContext)
    else if funcName = 'test2' then
      Result := Func_Test2(AContext)
    else if funcName = 'test3' then
      Result := Func_Test3(AContext)
    else if funcName = 'test4' then
      Result := Func_Test4(AContext)
    else
    begin
      // Неизвестная функция
      programlog.LogOutFormatStr(
        'uzvsqlite: Предупреждение - неизвестная функция экспорта: %s',
        [funcName],
        LM_Info
      );
      Result := Null;
    end;

  except
    on E: Exception do
    begin
      programlog.LogOutFormatStr(
        'uzvsqlite: Критическая ошибка при вызове функции %s: %s',
        [funcName, E.Message],
        LM_Info
      );
      Result := Null;
    end;
  end;
end;

end.
