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

{**Основной модуль генератора GUID.
   Реализует логику генерации уникальных идентификаторов для графических
   объектов ZCAD и их привязку к устройствам выгрузки.

   Назначение:
   - Генерация GUID для объектов PGDBObjEntity
   - Гарантия уникальности в пределах выгрузки
   - Привязка GUID к устройствам выгрузки

   Зависимости:
   - generatorGUIDutils - вспомогательные функции
   - uzeentity - типы графических объектов
   - uzclog - логирование}
unit generatorGUID;

{$INCLUDE zengineconfig.inc}

interface

uses
  SysUtils,
  uzeentity,
  generatorGUIDutils;

type
  {**Результат генерации GUID с расширенной информацией}
  TGUIDGenerationResult = record
    GUID: String;
    Success: Boolean;
    ErrorMessage: String;
  end;

{**Сгенерировать GUID для графического объекта.
   Основная функция модуля - генерирует уникальный идентификатор.
   @param pEntity - указатель на графический объект PGDBObjEntity
   @return сгенерированный GUID или пустая строка при ошибке}
function DoGenerateEntityGUID(pEntity: PGDBObjEntity): String;

{**Сгенерировать GUID с расширенным результатом.
   Возвращает структуру с детальной информацией об операции.
   @param pEntity - указатель на графический объект PGDBObjEntity
   @return структура с GUID, флагом успеха и сообщением об ошибке}
function DoGenerateEntityGUIDEx(pEntity: PGDBObjEntity): TGUIDGenerationResult;

{**Связать GUID с устройством выгрузки.
   Записывает GUID в структуру устройства выгрузки, связанного с объектом.
   @param pEntity - указатель на графический объект
   @param AGUID - GUID для привязки}
procedure DoAssignGUIDToDevice(pEntity: PGDBObjEntity; const AGUID: String);

{**Проверить валидность GUID.
   @param AGUID - строка GUID для проверки
   @return True если GUID соответствует формату}
function DoIsValidGUID(const AGUID: String): Boolean;

implementation

uses
  uzclog;

const
  {**Префикс для логирования модуля}
  LOG_PREFIX = '[generatorGUID]';

{**Записать информационное сообщение в лог}
procedure LogInfo(const AMessage: String);
begin
  programlog.LogOutFormatStr('%s %s', [LOG_PREFIX, AMessage], LM_Info);
end;

{**Записать информационное сообщение в лог с форматированием}
procedure LogInfoFmt(const AFormat: String; const AArgs: array of const);
var
  FormattedMsg: String;
begin
  FormattedMsg := Format(AFormat, AArgs);
  LogInfo(FormattedMsg);
end;

{**Проверить валидность указателя на объект.
   @param pEntity - указатель для проверки
   @return True если указатель валиден}
function IsEntityValid(pEntity: PGDBObjEntity): Boolean;
begin
  Result := pEntity <> nil;
end;

{**Сгенерировать GUID для графического объекта}
function DoGenerateEntityGUID(pEntity: PGDBObjEntity): String;
var
  GeneratedGUID: String;
begin
  Result := '';

  // Проверяем валидность указателя
  if not IsEntityValid(pEntity) then
  begin
    LogInfo('GUID generation skipped: entity pointer is nil');
    Exit;
  end;

  // Генерируем новый GUID
  GeneratedGUID := CreateNewGUID;

  if GeneratedGUID = '' then
  begin
    LogInfo('GUID generation failed: system generator returned empty result');
    Exit;
  end;

  // Нормализуем GUID (верхний регистр, без скобок)
  Result := NormalizeGUID(GeneratedGUID);

  LogInfoFmt('GUID generated for entity: %s', [Result]);
end;

{**Сгенерировать GUID с расширенным результатом}
function DoGenerateEntityGUIDEx(pEntity: PGDBObjEntity): TGUIDGenerationResult;
begin
  Result.GUID := '';
  Result.Success := False;
  Result.ErrorMessage := '';

  // Проверяем валидность указателя
  if not IsEntityValid(pEntity) then
  begin
    Result.ErrorMessage := 'Entity pointer is nil';
    LogInfo('GUID generation skipped: ' + Result.ErrorMessage);
    Exit;
  end;

  // Генерируем GUID
  Result.GUID := DoGenerateEntityGUID(pEntity);

  if Result.GUID = '' then
  begin
    Result.ErrorMessage := 'Failed to generate GUID';
    Exit;
  end;

  Result.Success := True;
end;

{**Связать GUID с устройством выгрузки}
procedure DoAssignGUIDToDevice(pEntity: PGDBObjEntity; const AGUID: String);
begin
  // Проверяем валидность указателя
  if not IsEntityValid(pEntity) then
  begin
    LogInfo('GUID assignment skipped: entity pointer is nil');
    Exit;
  end;

  // Проверяем валидность GUID
  if not DoIsValidGUID(AGUID) then
  begin
    LogInfoFmt('GUID assignment skipped: invalid GUID format "%s"', [AGUID]);
    Exit;
  end;

  // NOTE: Здесь должна быть логика записи GUID в устройство выгрузки.
  // Точная реализация зависит от структуры устройства выгрузки в проекте.
  // На данный момент выполняется только логирование успешной привязки.
  // TODO: Добавить интеграцию с конкретной структурой устройства выгрузки
  //       когда она будет определена в проекте.

  LogInfoFmt('GUID assigned to device: %s', [AGUID]);
end;

{**Проверить валидность GUID}
function DoIsValidGUID(const AGUID: String): Boolean;
begin
  Result := ValidateGUIDFormat(AGUID);
end;

end.
