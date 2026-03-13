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

{**Модуль публичного интерфейса для генератора GUID.
   Содержит объявления типов и функций, доступных внешним модулям.
   Модуль предназначен для генерации уникальных идентификаторов GUID
   для объектов выгрузки на основе переданного графического объекта.}
unit generatorGUIDinterface;

{$INCLUDE zengineconfig.inc}

interface

uses
  uzeentity,
  generatorGUID;

{**Сгенерировать GUID для графического объекта.
   @param pEntity - указатель на графический объект PGDBObjEntity
   @return сгенерированный GUID в строковом формате или пустая строка при ошибке}
function GenerateEntityGUID(pEntity: PGDBObjEntity): String;

{**Сгенерировать GUID с расширенным результатом.
   @param pEntity - указатель на графический объект PGDBObjEntity
   @return структура с GUID, флагом успеха и сообщением об ошибке}
function GenerateEntityGUIDEx(
  pEntity: PGDBObjEntity
): generatorGUID.TGUIDGenerationResult;

{**Связать GUID с устройством выгрузки.
   @param pEntity - указатель на графический объект
   @param AGUID - GUID для привязки
   @return True если привязка успешна}
procedure AssignGUIDToDevice(pEntity: PGDBObjEntity; const AGUID: String);

{**Проверить валидность GUID.
   @param AGUID - строка GUID для проверки
   @return True если GUID соответствует формату}
function IsValidGUID(const AGUID: String): Boolean;

implementation

{**Сгенерировать GUID для графического объекта}
function GenerateEntityGUID(pEntity: PGDBObjEntity): String;
begin
  Result := generatorGUID.DoGenerateEntityGUID(pEntity);
end;

{**Сгенерировать GUID с расширенным результатом}
function GenerateEntityGUIDEx(
  pEntity: PGDBObjEntity
): generatorGUID.TGUIDGenerationResult;
begin
  Result := generatorGUID.DoGenerateEntityGUIDEx(pEntity);
end;

{**Связать GUID с устройством выгрузки}
procedure AssignGUIDToDevice(pEntity: PGDBObjEntity; const AGUID: String);
begin
  generatorGUID.DoAssignGUIDToDevice(pEntity, AGUID);
end;

{**Проверить валидность GUID}
function IsValidGUID(const AGUID: String): Boolean;
begin
  Result := generatorGUID.DoIsValidGUID(AGUID);
end;

end.
