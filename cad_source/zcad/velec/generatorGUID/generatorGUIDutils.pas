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

{**Модуль вспомогательных функций для генератора GUID.
   Содержит утилиты для генерации, валидации и форматирования GUID.}
unit generatorGUIDutils;

{$INCLUDE zengineconfig.inc}

interface

uses
  SysUtils;

{**Создать новый GUID с использованием системного генератора.
   @return GUID в стандартном формате xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx}
function CreateNewGUID: String;

{**Проверить соответствие строки формату GUID.
   @param AGUID - строка для проверки
   @return True если строка соответствует формату GUID}
function ValidateGUIDFormat(const AGUID: String): Boolean;

{**Преобразовать GUID в верхний регистр.
   @param AGUID - исходный GUID
   @return GUID в верхнем регистре}
function NormalizeGUID(const AGUID: String): String;

{**Удалить фигурные скобки из GUID если они присутствуют.
   @param AGUID - исходный GUID
   @return GUID без фигурных скобок}
function StripGUIDBraces(const AGUID: String): String;

implementation

const
  {**Длина GUID без скобок (8-4-4-4-12 + 4 дефиса)}
  GUID_LENGTH_NO_BRACES = 36;

  {**Длина GUID со скобками}
  GUID_LENGTH_WITH_BRACES = 38;

  {**Допустимые символы в GUID (шестнадцатеричные цифры)}
  HEX_CHARS = ['0'..'9', 'A'..'F', 'a'..'f'];

{**Проверить символ на принадлежность к шестнадцатеричным цифрам}
function IsHexChar(C: Char): Boolean;
begin
  Result := C in HEX_CHARS;
end;

{**Создать новый GUID с использованием системного генератора}
function CreateNewGUID: String;
var
  NewGUID: TGUID;
begin
  // Генерируем GUID с использованием системной функции
  if CreateGUID(NewGUID) = 0 then
  begin
    // Преобразуем в строку и удаляем фигурные скобки
    Result := GUIDToString(NewGUID);
    Result := StripGUIDBraces(Result);
  end
  else
    Result := '';
end;

{**Проверить соответствие строки формату GUID}
function ValidateGUIDFormat(const AGUID: String): Boolean;
var
  WorkGUID: String;
  I: Integer;
begin
  Result := False;

  // Удаляем скобки если есть
  WorkGUID := StripGUIDBraces(AGUID);

  // Проверяем длину
  if Length(WorkGUID) <> GUID_LENGTH_NO_BRACES then
    Exit;

  // Проверяем позиции дефисов: 9, 14, 19, 24
  if (WorkGUID[9] <> '-') or
     (WorkGUID[14] <> '-') or
     (WorkGUID[19] <> '-') or
     (WorkGUID[24] <> '-') then
    Exit;

  // Проверяем остальные символы на шестнадцатеричность
  for I := 1 to Length(WorkGUID) do
  begin
    if I in [9, 14, 19, 24] then
      Continue;

    if not IsHexChar(WorkGUID[I]) then
      Exit;
  end;

  Result := True;
end;

{**Преобразовать GUID в верхний регистр}
function NormalizeGUID(const AGUID: String): String;
begin
  Result := UpperCase(StripGUIDBraces(AGUID));
end;

{**Удалить фигурные скобки из GUID}
function StripGUIDBraces(const AGUID: String): String;
begin
  Result := AGUID;

  if Length(Result) = 0 then
    Exit;

  // Удаляем открывающую скобку
  if Result[1] = '{' then
    Delete(Result, 1, 1);

  // Удаляем закрывающую скобку
  if (Length(Result) > 0) and (Result[Length(Result)] = '}') then
    Delete(Result, Length(Result), 1);
end;

end.
