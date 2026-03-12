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

{**Главный модуль управления вращением объектов}
unit uzvrotate_main;

{$INCLUDE zengineconfig.inc}

interface
uses
  SysUtils,
  Forms,
  uzccommandsmanager,
  uzccommandsabstract,
  uzccommandsimpl,
  uzclog,
  uzcdrawings,
  uzcinterface,
  uzvrotate_struct,
  uzvrotate_logic,
  uzvrotate_form;

{**Функция команды управления вращением объектов}
function RotateControl_com(
  const Context: TZCADCommandContext;
  operands: TCommandOperands
): TCommandResult;

implementation

{**Вывести сообщение в историю ZCAD}
procedure PrintMessage(const Msg: String);
begin
  zcUI.TextMessage(Msg, TMWOHistoryOut);
end;

{**Вывести форматированное сообщение в историю ZCAD}
procedure PrintFormatMessage(const Fmt: String; const Args: array of const);
begin
  PrintMessage(Format(Fmt, Args));
end;

{**Проверить наличие выделенных объектов}
function HasSelectedObjects: Boolean;
begin
  Result := drawings.GetCurrentDWG^.SelObjArray.Count > 0;
end;

{**Функция команды управления вращением объектов}
function RotateControl_com(
  const Context: TZCADCommandContext;
  operands: TCommandOperands
): TCommandResult;
var
  selectedCount: Integer;
begin
  Result := cmd_ok;

  // Логирование старта команды
  programlog.LogOutFormatStr(
    'Запущена команда управления вращением объектов',
    [],
    LM_Info
  );

  PrintMessage('[RotateControl] Запуск модуля вращения...');

  // Проверка наличия выделенных объектов
  if not HasSelectedObjects then
  begin
    PrintMessage('[RotateControl] Нет выделенных объектов.');
    PrintMessage('[RotateControl] Выделите объекты на чертеже и повторите команду.');
    programlog.LogOutFormatStr(
      'Команда завершена: нет выделенных объектов',
      [],
      LM_Warning
    );
    Exit;
  end;

  // Получаем количество выделенных объектов
  selectedCount := GetSelectedObjectsCount;

  PrintFormatMessage(
    '[RotateControl] Выделено объектов: %d',
    [selectedCount]
  );

  // Открытие формы вращения
  programlog.LogOutFormatStr(
    'Открытие формы управления вращением',
    [],
    LM_Info
  );

  try
    with TfrmRotateControl.Create(Application) do
    try
      // Инициализация данных вращения
      if not InitializeForSelectedObjects then
      begin
        PrintMessage('[RotateControl] Ошибка инициализации данных вращения.');
        Result := cmd_error;
        Free;
        Exit;
      end;

      // Показ формы модально
      ShowModal;

      programlog.LogOutFormatStr(
        'Форма управления вращением закрыта пользователем',
        [],
        LM_Info
      );
    except
      on E: Exception do
      begin
        PrintFormatMessage(
          '[RotateControl] Ошибка: %s',
          [E.Message]
        );
        programlog.LogOutFormatStr(
          'Ошибка при работе с формой вращения: %s',
          [E.Message],
          LM_Error
        );
        Result := cmd_error;
        Free;
      end;
    end;

  except
    on E: Exception do
    begin
      PrintFormatMessage(
        '[RotateControl] Ошибка при инициализации: %s',
        [E.Message]
      );
      programlog.LogOutFormatStr(
        'Ошибка при инициализации: %s',
        [E.Message],
        LM_Error
      );
      Result := cmd_error;
    end;
  end;

  programlog.LogOutFormatStr(
    'Команда управления вращением объектов завершена',
    [],
    LM_Info
  );
end;

initialization
  // Регистрация команды rotatecontrol
  CreateZCADCommand(@RotateControl_com, 'rotatecontrol', CADWG, 0);

end.
