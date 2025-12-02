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

{**Модуль команды отображения загруженных спецификационных данных}
unit uzvspecificationnav_cmddisplay;

{$INCLUDE zengineconfig.inc}

interface

uses
  SysUtils,
  uzccommandsmanager,
  uzccommandsabstract,
  uzccommandsimpl,
  uzclog,
  uzcinterface;

// Команда отображения спецификационных данных
function SpecDisplay_com(
  const Context: TZCADCommandContext;
  operands: TCommandOperands
): TCommandResult;

implementation

uses
  uzvspecificationnav_commandstart;

// Команда отображения спецификационных данных
function SpecDisplay_com(
  const Context: TZCADCommandContext;
  operands: TCommandOperands
): TCommandResult;
var
  SpecNav: TSpecificationNavFrame;
  recordCount: Integer;
begin
  Result := cmd_ok;

  try
    programlog.LogOutFormatStr(
      'Выполнение команды SpecDisplay',
      [],
      LM_Info
    );

    zcUI.TextMessage('==============================================', TMWOHistoryOut);
    zcUI.TextMessage('Команда: Отображение спецификаций', TMWOHistoryOut);
    zcUI.TextMessage('==============================================', TMWOHistoryOut);

    // Получаем экземпляр навигатора
    SpecNav := GetSpecNavigator;

    if SpecNav = nil then
    begin
      zcUI.TextMessage('Ошибка: не удалось получить экземпляр навигатора', TMWOHistoryOut);
      zcUI.TextMessage('Выполните команду SpecNavigator для инициализации', TMWOHistoryOut);
      zcUI.TextMessage('==============================================', TMWOHistoryOut);
      Exit;
    end;

    // Проверяем наличие данных
    recordCount := SpecNav.SpecificationList.Count;

    if recordCount = 0 then
    begin
      zcUI.TextMessage('Нет загруженных спецификационных данных', TMWOHistoryOut);
      zcUI.TextMessage('', TMWOHistoryOut);
      zcUI.TextMessage('Используйте команду SpecRead для загрузки данных', TMWOHistoryOut);
      zcUI.TextMessage('==============================================', TMWOHistoryOut);
      Exit;
    end;

    zcUI.TextMessage(
      Format('Загружено записей: %d', [recordCount]),
      TMWOHistoryOut
    );

    // Обновляем отображение
    SpecNav.RefreshDisplay;

    zcUI.TextMessage('Данные отображены в навигаторе', TMWOHistoryOut);
    zcUI.TextMessage('==============================================', TMWOHistoryOut);

  except
    on E: Exception do
    begin
      programlog.LogOutFormatStr(
        'Ошибка при выполнении команды SpecDisplay: %s',
        [E.Message],
        LM_Info
      );

      zcUI.TextMessage('', TMWOHistoryOut);
      zcUI.TextMessage('Критическая ошибка: ' + E.Message, TMWOHistoryOut);
      zcUI.TextMessage('==============================================', TMWOHistoryOut);
      Result := cmd_error;
    end;
  end;
end;

initialization
  // Регистрация команды в системе ZCAD
  CreateZCADCommand(
    @SpecDisplay_com,
    'SpecDisplay',
    CADWG,
    0
  );

  programlog.LogOutFormatStr(
    'Команда SpecDisplay зарегистрирована',
    [],
    LM_Info
  );

end.
