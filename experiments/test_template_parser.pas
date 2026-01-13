program test_template_parser;

{$mode objfpc}{$H+}

uses
  SysUtils;

{**
  Тестовая программа для демонстрации работы парсера составных шаблонов

  Примеры обработки:
  1. @@[SLCABAGEN<lnum>_SLTypeagen]_@@[NMO_BaseName]!!!<Name>
  2. <Type>_<ID>
  3. Device: <Name> (Type: <Type>)
**}

procedure TestTemplatePatterns;
begin
  WriteLn('=== Тест парсера составных шаблонов ===');
  WriteLn;

  WriteLn('Поддерживаемые форматы:');
  WriteLn('1. Динамические параметры: @@[param_name]');
  WriteLn('   - Получаются через FindVariableInEnt');
  WriteLn('   - Поддерживают <lnum> для итерации');
  WriteLn;

  WriteLn('2. Системные атрибуты: <AttributeName>');
  WriteLn('   - Name - имя объекта');
  WriteLn('   - BaseName - базовое имя');
  WriteLn('   - ID - идентификатор объекта');
  WriteLn('   - Type - тип объекта');
  WriteLn;

  WriteLn('3. Статический текст');
  WriteLn('   - Включается в результат как есть');
  WriteLn;

  WriteLn('Примеры шаблонов:');
  WriteLn('  @@[SLCABAGEN<lnum>_SLTypeagen]_@@[NMO_BaseName]!!!<Name>');
  WriteLn('  => ЭО_EL1!!!VEL_LIGHT_2x36');
  WriteLn;
  WriteLn('  <Type>_<ID>');
  WriteLn('  => DeviceType_12345678');
  WriteLn;
  WriteLn('  Device: <Name> (Type: <Type>, ID: <ID>)');
  WriteLn('  => Device: VEL_LIGHT_2x36 (Type: DeviceType, ID: 12345678)');
  WriteLn;
end;

begin
  TestTemplatePatterns;
  WriteLn('Нажмите Enter для выхода...');
  ReadLn;
end.
