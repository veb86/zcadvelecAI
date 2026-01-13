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

unit uzvsqlite_template_parser;

{$INCLUDE zengineconfig.inc}

interface

uses
  SysUtils, uzeentity, uzsbvarmandef, uzcvariablesutils, uzclog;

{**
  Парсер шаблонов для выгрузки данных

  Поддерживает следующие типы плейсхолдеров:
  - @@[param_name] - динамический параметр через FindVariableInEnt
  - <AttributeName> - системный атрибут объекта (Name, ID и т.д.)
  - <lnum> внутри @@[] - заменяется на номер цикла
  - Обычный текст - копируется как есть
**}

// Парсинг шаблона и получение результата
function ParseTemplate(
  const ATemplate: String;
  AEntity: Pointer;
  ALnum: Integer
): String;

// Получить значение системного атрибута объекта
function GetSystemAttribute(
  AEntity: Pointer;
  const AAttrName: String
): String;

implementation

uses
  uzeconsts;

// Получить значение системного атрибута объекта
function GetSystemAttribute(
  AEntity: Pointer;
  const AAttrName: String
): String;
var
  pEntity: PGDBObjEntity;
  lowerAttr: String;
begin
  Result := '';

  if AEntity = nil then
  begin
    programlog.LogOutFormatStr(
      'uzvsqlite: GetSystemAttribute - entity is nil',
      [],
      LM_Info
    );
    Exit;
  end;

  pEntity := PGDBObjEntity(AEntity);
  lowerAttr := LowerCase(Trim(AAttrName));

  // Обработка системных атрибутов
  //if lowerAttr = 'name' then
  //begin
  //  // Получаем имя объекта
  //  Result := pEntity^.Name;
  //end
  //else if lowerAttr = 'id' then
  //begin
  //  // Получаем ID объекта как строку
  //  Result := IntToStr(pEntity^.vp.ID);
  //end
  //else if lowerAttr = 'type' then
  //begin
  //  // Получаем тип объекта
  //  Result := IntToStr(pEntity^.GetObjType);
  //end
  //else if lowerAttr = 'layer' then
  //begin
  //  // Получаем имя слоя
  //  Result := pEntity^.vp.Layer^.Name;
  //end
  //else
  //begin
  //  // Неизвестный атрибут
  //  programlog.LogOutFormatStr(
  //    'uzvsqlite: Неизвестный системный атрибут: %s',
  //    [AAttrName],
  //    LM_Info
  //  );
  //end;

  programlog.LogOutFormatStr(
    'uzvsqlite: GetSystemAttribute(%s) = %s',
    [AAttrName, Result],
    LM_Info
  );
end;

// Получить значение динамического параметра
function GetDynamicParameter(
  AEntity: Pointer;
  const AParamName: String
): String;
var
  pEntity: PGDBObjEntity;
  pvd: pvardesk;
begin
  Result := '';

  if AEntity = nil then
  begin
    programlog.LogOutFormatStr(
      'uzvsqlite: GetDynamicParameter - entity is nil',
      [],
      LM_Info
    );
    Exit;
  end;

  pEntity := PGDBObjEntity(AEntity);

  // Ищем переменную по имени через FindVariableInEnt
  pvd := FindVariableInEnt(pEntity, AParamName);

  if pvd = nil then
  begin
    programlog.LogOutFormatStr(
      'uzvsqlite: Параметр не найден: %s',
      [AParamName],
      LM_Info
    );
    Exit;
  end;

  // Получаем значение как строку
  Result := pvd^.Data.ptd^.GetValueAsString(pvd^.Data.Addr.Instance);

  programlog.LogOutFormatStr(
    'uzvsqlite: GetDynamicParameter(%s) = %s',
    [AParamName, Result],
    LM_Info
  );
end;

// Найти следующую позицию любого из маркеров
function FindNextMarker(
  const AText: String;
  AStartPos: Integer;
  out AMarkerType: Integer;
  out AMarkerPos: Integer
): Boolean;
var
  pos1, pos2: Integer;
begin
  Result := False;
  AMarkerType := 0;
  AMarkerPos := 0;

  // Ищем @@[
  pos1 := Pos('@@[', Copy(AText, AStartPos, Length(AText)));
  if pos1 > 0 then
    pos1 := pos1 + AStartPos - 1;

  // Ищем <
  pos2 := Pos('<', Copy(AText, AStartPos, Length(AText)));
  if pos2 > 0 then
    pos2 := pos2 + AStartPos - 1;

  // Определяем ближайший маркер
  if (pos1 > 0) and ((pos2 = 0) or (pos1 < pos2)) then
  begin
    AMarkerType := 1; // @@[
    AMarkerPos := pos1;
    Result := True;
  end
  else if pos2 > 0 then
  begin
    AMarkerType := 2; // <
    AMarkerPos := pos2;
    Result := True;
  end;
end;

// Парсинг шаблона и получение результата
function ParseTemplate(
  const ATemplate: String;
  AEntity: Pointer;
  ALnum: Integer
): String;
var
  position: Integer;
  markerType, markerPos: Integer;
  endPos: Integer;
  token: String;
  processedToken: String;
  paramValue: String;
begin
  Result := '';
  position := 1;

  programlog.LogOutFormatStr(
    'uzvsqlite: ParseTemplate - template: %s, lnum: %d',
    [ATemplate, ALnum],
    LM_Info
  );

  // Парсим строку шаблона
  while position <= Length(ATemplate) do
  begin
    // Ищем следующий маркер
    if FindNextMarker(ATemplate, position, markerType, markerPos) then
    begin
      // Добавляем текст до маркера
      if markerPos > position then
      begin
        Result := Result + Copy(ATemplate, position, markerPos - position);
      end;

      // Обрабатываем маркер
      if markerType = 1 then
      begin
        // Обработка @@[...]
        endPos := Pos(']', Copy(ATemplate, markerPos + 3, Length(ATemplate)));

        if endPos > 0 then
        begin
          endPos := endPos + markerPos + 2;

          // Извлекаем токен между @@[ и ]
          token := Copy(ATemplate, markerPos + 3, endPos - markerPos - 3);

          // Обрабатываем <lnum> внутри токена
          processedToken := StringReplace(
            token,
            '<lnum>',
            IntToStr(ALnum),
            [rfReplaceAll, rfIgnoreCase]
          );

          // Получаем значение параметра
          paramValue := GetDynamicParameter(AEntity, processedToken);
          Result := Result + paramValue;

          position := endPos + 1;
        end
        else
        begin
          // Некорректный синтаксис - копируем как есть
          programlog.LogOutFormatStr(
            'uzvsqlite: Некорректный синтаксис @@[ без ]',
            [],
            LM_Info
          );
          Result := Result + '@@[';
          position := markerPos + 3;
        end;
      end
      else if markerType = 2 then
      begin
        // Обработка <...>
        endPos := Pos('>', Copy(ATemplate, markerPos + 1, Length(ATemplate)));

        if endPos > 0 then
        begin
          endPos := endPos + markerPos;

          // Извлекаем токен между < и >
          token := Copy(ATemplate, markerPos + 1, endPos - markerPos - 1);

          // Проверяем, не является ли это <lnum> вне @@[]
          if LowerCase(token) = 'lnum' then
          begin
            // <lnum> вне @@[] заменяется на номер цикла
            Result := Result + IntToStr(ALnum);
          end
          else
          begin
            // Получаем значение системного атрибута
            paramValue := GetSystemAttribute(AEntity, token);
            Result := Result + paramValue;
          end;

          position := endPos + 1;
        end
        else
        begin
          // Некорректный синтаксис - копируем как есть
          programlog.LogOutFormatStr(
            'uzvsqlite: Некорректный синтаксис < без >',
            [],
            LM_Info
          );
          Result := Result + '<';
          position := markerPos + 1;
        end;
      end;
    end
    else
    begin
      // Нет больше маркеров - копируем остаток
      Result := Result + Copy(ATemplate, position, Length(ATemplate));
      Break;
    end;
  end;

  programlog.LogOutFormatStr(
    'uzvsqlite: ParseTemplate - result: %s',
    [Result],
    LM_Info
  );
end;

end.
