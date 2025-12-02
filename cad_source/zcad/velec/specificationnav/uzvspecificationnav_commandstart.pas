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

{**Модуль команды запуска навигатора спецификаций}
unit uzvspecificationnav_commandstart;

{$INCLUDE zengineconfig.inc}

interface

uses
  SysUtils,
  uzccommandsmanager,
  uzccommandsabstract,
  uzccommandsimpl,
  uzclog,
  uzcinterface;

// Команда запуска навигатора спецификаций
function SpecNavigatorStart_com(
  const Context: TZCADCommandContext;
  operands: TCommandOperands
): TCommandResult;

implementation

uses
  uzvspecificationnav;

var
  GlobalSpecNavFrame: TSpecificationNavFrame = nil;

// Команда запуска навигатора спецификаций
function SpecNavigatorStart_com(
  const Context: TZCADCommandContext;
  operands: TCommandOperands
): TCommandResult;
begin
  Result := cmd_ok;

  try
    programlog.LogOutFormatStr(
      'Запуск навигатора спецификаций',
      [],
      LM_Info
    );

    zcUI.TextMessage('==============================================', TMWOHistoryOut);
    zcUI.TextMessage('Навигатор спецификационных данных', TMWOHistoryOut);
    zcUI.TextMessage('==============================================', TMWOHistoryOut);

    // Создаем форму, если она еще не создана
    if GlobalSpecNavFrame = nil then
    begin
      GlobalSpecNavFrame := TSpecificationNavFrame.Create(nil);

      programlog.LogOutFormatStr(
        'Форма навигатора спецификаций создана',
        [],
        LM_Info
      );

      zcUI.TextMessage('Форма навигатора создана', TMWOHistoryOut);
    end
    else
    begin
      programlog.LogOutFormatStr(
        'Форма навигатора спецификаций уже существует',
        [],
        LM_Info
      );

      zcUI.TextMessage('Форма навигатора уже открыта', TMWOHistoryOut);
    end;

    // Показываем форму (здесь должна быть интеграция с основным окном)
    zcUI.TextMessage('', TMWOHistoryOut);
    zcUI.TextMessage('Для работы с навигатором используйте команды:', TMWOHistoryOut);
    zcUI.TextMessage('  SpecRead     - считать спецификации из выделенных устройств', TMWOHistoryOut);
    zcUI.TextMessage('  SpecDisplay  - отобразить загруженные спецификации', TMWOHistoryOut);
    zcUI.TextMessage('==============================================', TMWOHistoryOut);

  except
    on E: Exception do
    begin
      programlog.LogOutFormatStr(
        'Ошибка при запуске навигатора спецификаций: %s',
        [E.Message],
        LM_Info
      );

      zcUI.TextMessage('Ошибка: ' + E.Message, TMWOHistoryOut);
      Result := cmd_error;
    end;
  end;
end;

// Функция получения глобального экземпляра навигатора
function GetSpecNavigator: TSpecificationNavFrame;
begin
  if GlobalSpecNavFrame = nil then
    GlobalSpecNavFrame := TSpecificationNavFrame.Create(nil);

  Result := GlobalSpecNavFrame;
end;

initialization
  // Регистрация команды в системе ZCAD
  CreateZCADCommand(
    @SpecNavigatorStart_com,
    'SpecNavigator',
    CADWG,
    0
  );

  programlog.LogOutFormatStr(
    'Команда SpecNavigator зарегистрирована',
    [],
    LM_Info
  );

finalization
  // Освобождение ресурсов при завершении
  if GlobalSpecNavFrame <> nil then
  begin
    GlobalSpecNavFrame.Free;
    GlobalSpecNavFrame := nil;
  end;

end.
