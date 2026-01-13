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

unit uzvaccess_template_parser;

{$INCLUDE zengineconfig.inc}

interface

uses
  SysUtils, Classes, Variants,
  uzeentity, uzsbvarmandef, uzcvariablesutils,
  uzvaccess_types, uzclog;

const
  // Маркеры для динамических параметров @@[param]
  DYNAMIC_PARAM_START = '@@[';
  DYNAMIC_PARAM_END = ']';

  // Маркеры для системных атрибутов <AttributeName>
  SYSTEM_ATTR_START = '<';
  SYSTEM_ATTR_END = '>';

type
  {**
    Парсер составных шаблонов для выгрузки

    Обрабатывает шаблоны, содержащие:
    - Динамические параметры: @@[param_name]
    - Системные атрибуты: <AttributeName>
    - Статический текст
  **}
  TTemplateParser = class
  private
    // Получить значение динамического параметра из сущности
    function GetDynamicParamValue(
      AEntity: Pointer;
      const AParamName: String
    ): String;

    // Получить значение системного атрибута из сущности
    function GetSystemAttributeValue(
      AEntity: Pointer;
      const AAttributeName: String
    ): String;

    // Проверить, является ли позиция началом маркера
    function IsMarkerAt(
      const ATemplate: String;
      APosition: Integer;
      const AMarker: String
    ): Boolean;

    // Найти конец маркера, начиная с позиции
    function FindMarkerEnd(
      const ATemplate: String;
      AStartPos: Integer;
      const AEndMarker: String
    ): Integer;

    // Извлечь следующий токен из шаблона
    function ExtractNextToken(
      const ATemplate: String;
      var APosition: Integer;
      out ATokenType: String;
      out ATokenValue: String
    ): Boolean;

  public
    {**
      Разобрать шаблон и заменить плейсхолдеры на значения

      @param ATemplate - строка шаблона
      @param AEntity - указатель на сущность (устройство/кабель/суперлиния)
      @param ALnumValue - значение для замены <lnum> (0 если не используется)
      @return результирующая строка с замененными значениями
    **}
    function ParseTemplate(
      const ATemplate: String;
      AEntity: Pointer;
      ALnumValue: Integer = 0
    ): String;
  end;

implementation

uses
  uzeconsts;

{ TTemplateParser }

function TTemplateParser.IsMarkerAt(
  const ATemplate: String;
  APosition: Integer;
  const AMarker: String
): Boolean;
var
  i: Integer;
begin
  Result := False;

  // Проверяем, не выходим ли за границы
  if (APosition < 1) or (APosition + Length(AMarker) - 1 > Length(ATemplate)) then
    Exit;

  // Сравниваем символы
  for i := 1 to Length(AMarker) do
  begin
    if ATemplate[APosition + i - 1] <> AMarker[i] then
      Exit;
  end;

  Result := True;
end;

function TTemplateParser.FindMarkerEnd(
  const ATemplate: String;
  AStartPos: Integer;
  const AEndMarker: String
): Integer;
var
  i: Integer;
begin
  Result := 0;

  // Ищем конечный маркер
  for i := AStartPos to Length(ATemplate) - Length(AEndMarker) + 1 do
  begin
    if IsMarkerAt(ATemplate, i, AEndMarker) then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

function TTemplateParser.ExtractNextToken(
  const ATemplate: String;
  var APosition: Integer;
  out ATokenType: String;
  out ATokenValue: String
): Boolean;
var
  markerStart, markerEnd: Integer;
  nextMarkerPos: Integer;
  textEnd: Integer;
begin
  Result := False;
  ATokenType := '';
  ATokenValue := '';

  // Если достигли конца строки
  if APosition > Length(ATemplate) then
    Exit;

  // Проверяем динамический параметр @@[...]
  if IsMarkerAt(ATemplate, APosition, DYNAMIC_PARAM_START) then
  begin
    markerStart := APosition + Length(DYNAMIC_PARAM_START);
    markerEnd := FindMarkerEnd(ATemplate, markerStart, DYNAMIC_PARAM_END);

    if markerEnd > 0 then
    begin
      ATokenType := 'dynamic';
      ATokenValue := Copy(
        ATemplate,
        markerStart,
        markerEnd - markerStart
      );
      APosition := markerEnd + Length(DYNAMIC_PARAM_END);
      Result := True;
      Exit;
    end;
  end;

  // Проверяем системный атрибут <...>
  if IsMarkerAt(ATemplate, APosition, SYSTEM_ATTR_START) then
  begin
    markerStart := APosition + Length(SYSTEM_ATTR_START);
    markerEnd := FindMarkerEnd(ATemplate, markerStart, SYSTEM_ATTR_END);

    if markerEnd > 0 then
    begin
      ATokenType := 'system';
      ATokenValue := Copy(
        ATemplate,
        markerStart,
        markerEnd - markerStart
      );
      APosition := markerEnd + Length(SYSTEM_ATTR_END);
      Result := True;
      Exit;
    end;
  end;

  // Обычный текст до следующего маркера
  textEnd := Length(ATemplate) + 1;

  // Ищем ближайший маркер
  nextMarkerPos := APosition + 1;
  while nextMarkerPos <= Length(ATemplate) do
  begin
    if IsMarkerAt(ATemplate, nextMarkerPos, DYNAMIC_PARAM_START) or
       IsMarkerAt(ATemplate, nextMarkerPos, SYSTEM_ATTR_START) then
    begin
      textEnd := nextMarkerPos;
      Break;
    end;
    Inc(nextMarkerPos);
  end;

  // Извлекаем текст
  ATokenType := 'text';
  ATokenValue := Copy(ATemplate, APosition, textEnd - APosition);
  APosition := textEnd;
  Result := (ATokenValue <> '');
end;

function TTemplateParser.GetDynamicParamValue(
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
      'uzvaccess_template_parser: Попытка получить параметр "%s" из nil-объекта',
      [AParamName],
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
      'uzvaccess_template_parser: Параметр "%s" не найден',
      [AParamName],
      LM_Info
    );
    Exit;
  end;

  // Получаем строковое представление значения
  Result := pvd^.Data.ptd^.GetValueAsString(pvd^.Data.Addr.Instance);

  programlog.LogOutFormatStr(
    'uzvaccess_template_parser: Параметр "%s" = "%s"',
    [AParamName, Result],
    LM_Info
  );
end;

function TTemplateParser.GetSystemAttributeValue(
  AEntity: Pointer;
  const AAttributeName: String
): String;
var
  pEntity: PGDBObjEntity;
  attrLower: String;
begin
  Result := '';

  if AEntity = nil then
  begin
    programlog.LogOutFormatStr(
      'uzvaccess_template_parser: Попытка получить атрибут "%s" из nil-объекта',
      [AAttributeName],
      LM_Info
    );
    Exit;
  end;

  pEntity := PGDBObjEntity(AEntity);
  attrLower := LowerCase(Trim(AAttributeName));

  // Обработка известных системных атрибутов
  //if attrLower = 'name' then
  //begin
  //  // Получаем имя через GetVariableAsString
  //  Result := pEntity^.GetVariableAsString('NMO_Name');
  //end
  //else if attrLower = 'basename' then
  //begin
  //  Result := pEntity^.GetVariableAsString('NMO_BaseName');
  //end
  //else if attrLower = 'id' then
  //begin
  //  // ID - это адрес объекта в памяти
  //  Result := IntToStr(PtrUInt(pEntity));
  //end
  //else if attrLower = 'type' then
  //begin
  //  // Тип объекта
  //  Result := IntToStr(pEntity^.GetObjType);
  //end
  //else
  //begin
  //  // Для неизвестных атрибутов пытаемся получить как обычную переменную
  //  programlog.LogOutFormatStr(
  //    'uzvaccess_template_parser: Неизвестный системный атрибут "%s", попытка получить как переменную',
  //    [AAttributeName],
  //    LM_Info
  //  );
  //  Result := GetDynamicParamValue(AEntity, AAttributeName);
  //end;

  programlog.LogOutFormatStr(
    'uzvaccess_template_parser: Системный атрибут "%s" = "%s"',
    [AAttributeName, Result],
    LM_Info
  );
end;

function TTemplateParser.ParseTemplate(
  const ATemplate: String;
  AEntity: Pointer;
  ALnumValue: Integer = 0
): String;
var
  position: Integer;
  tokenType, tokenValue: String;
  processedValue: String;
  resultBuilder: String;
begin
  Result := '';
  resultBuilder := '';
  position := 1;

  programlog.LogOutFormatStr(
    'uzvaccess_template_parser: Начало парсинга шаблона: "%s"',
    [ATemplate],
    LM_Info
  );

  // Обрабатываем шаблон токен за токеном
  while ExtractNextToken(ATemplate, position, tokenType, tokenValue) do
  begin
    if tokenType = 'dynamic' then
    begin
      // Обработка динамического параметра @@[...]
      // Сначала заменяем <lnum> если есть
      if ALnumValue > 0 then
      begin
        processedValue := StringReplace(
          tokenValue,
          LNUM_PLACEHOLDER,
          IntToStr(ALnumValue),
          [rfReplaceAll]
        );
      end
      else
      begin
        processedValue := tokenValue;
      end;

      // Получаем значение параметра
      resultBuilder := resultBuilder + GetDynamicParamValue(AEntity, processedValue);
    end
    else if tokenType = 'system' then
    begin
      // Обработка системного атрибута <...>
      resultBuilder := resultBuilder + GetSystemAttributeValue(AEntity, tokenValue);
    end
    else if tokenType = 'text' then
    begin
      // Обычный текст добавляем как есть
      resultBuilder := resultBuilder + tokenValue;
    end;
  end;

  Result := resultBuilder;

  programlog.LogOutFormatStr(
    'uzvaccess_template_parser: Результат парсинга: "%s"',
    [Result],
    LM_Info
  );
end;

end.
