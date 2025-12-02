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

{**Модуль команды считывания спецификационных данных из выделенных устройств}
unit uzvspecificationnav_cmdread;

{$INCLUDE zengineconfig.inc}

interface

uses
  SysUtils,
  uzccommandsmanager,
  uzccommandsabstract,
  uzccommandsimpl,
  uzclog,
  uzcinterface,
  uzcdrawings;

// Команда считывания спецификационных данных
function SpecRead_com(
  const Context: TZCADCommandContext;
  operands: TCommandOperands
): TCommandResult;

implementation

uses
  uzvspecificationnav_commandstart;

// Команда считывания спецификационных данных
function SpecRead_com(
  const Context: TZCADCommandContext;
  operands: TCommandOperands
): TCommandResult;
var
  SpecNav: TSpecificationNavFrame;
  selectedCount: Integer;
begin
  Result := cmd_ok;

  try
    programlog.LogOutFormatStr(
      'Выполнение команды SpecRead',
      [],
      LM_Info
    );

    zcUI.TextMessage('==============================================', TMWOHistoryOut);
    zcUI.TextMessage('Команда: Считывание спецификаций', TMWOHistoryOut);
    zcUI.TextMessage('==============================================', TMWOHistoryOut);

    // Проверяем наличие активного чертежа
    if drawings.GetCurrentDWG = nil then
    begin
      zcUI.TextMessage('Ошибка: нет активного чертежа', TMWOHistoryOut);
      zcUI.TextMessage('==============================================', TMWOHistoryOut);
      Exit;
    end;

    // Проверяем наличие выделенных объектов
    selectedCount := drawings.GetCurrentDWG^.wa.param.seldesc.Selectedobjcount;

    if selectedCount = 0 then
    begin
      zcUI.TextMessage('Ошибка: не выделено ни одного объекта', TMWOHistoryOut);
      zcUI.TextMessage('', TMWOHistoryOut);
      zcUI.TextMessage('Выделите устройства на чертеже и повторите команду', TMWOHistoryOut);
      zcUI.TextMessage('==============================================', TMWOHistoryOut);
      Exit;
    end;

    zcUI.TextMessage(
      Format('Выделено объектов: %d', [selectedCount]),
      TMWOHistoryOut
    );

    // Получаем экземпляр навигатора
    SpecNav := GetSpecNavigator;

    if SpecNav = nil then
    begin
      zcUI.TextMessage('Ошибка: не удалось получить экземпляр навигатора', TMWOHistoryOut);
      zcUI.TextMessage('Выполните команду SpecNavigator для инициализации', TMWOHistoryOut);
      zcUI.TextMessage('==============================================', TMWOHistoryOut);
      Exit;
    end;

    // Выполняем загрузку данных
    SpecNav.LoadDataFromSelection;

    zcUI.TextMessage('==============================================', TMWOHistoryOut);

  except
    on E: Exception do
    begin
      programlog.LogOutFormatStr(
        'Ошибка при выполнении команды SpecRead: %s',
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
    @SpecRead_com,
    'SpecRead',
    CADWG,
    0
  );

  programlog.LogOutFormatStr(
    'Команда SpecRead зарегистрирована',
    [],
    LM_Info
  );

end.
