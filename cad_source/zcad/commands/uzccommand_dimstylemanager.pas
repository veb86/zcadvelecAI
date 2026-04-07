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
{
  Модуль: uzccommand_dimstylemanager
  Назначение: регистрирует команду DimStyleManager для открытия диалога
  управления размерными стилями. Команда требует открытого чертежа (флаг CADWG).
  Зависимости: uzclog, uzcui_dimstylemanager, uzcinterface, uzcsysvars,
               uzccommandsabstract, uzccommandsimpl
}
{$mode delphi}
unit uzccommand_dimstylemanager;

{$INCLUDE zengineconfig.inc}

interface

uses
  SysUtils,
  uzcLog,
  uzcui_dimstylemanager,
  uzcsysvars,
  uzcinterface,
  uzctreenode,
  uzccommandsabstract,
  uzccommandsimpl;

implementation

{ Открывает диалог управления размерными стилями в модальном режиме }
function DimStyleManager_cmd(
  const Context: TZCADCommandContext;
  operands: TCommandOperands): TCommandResult;
begin
  programlog.LogOutFormatStr(
    'uzccommand_dimstylemanager: открытие менеджера размерных стилей',
    [], LM_Info);

  DimStyleManagerForm := TDimStyleManagerForm.Create(nil);
  SetHeightControl(DimStyleManagerForm,
    sysvar.INTF.INTF_DefaultControlHeight^);
  zcUI.DOShowModal(DimStyleManagerForm);
  FreeAndNil(DimStyleManagerForm);

  Result := cmd_ok;
end;

initialization
  programlog.LogOutFormatStr('Unit "%s" initialization', [{$INCLUDE %FILE%}],
    LM_Info, UnitsInitializeLMId);
  CreateZCADCommand(@DimStyleManager_cmd, 'DimStyleManager', CADWG, 0);

finalization
  ProgramLog.LogOutFormatStr('Unit "%s" finalization', [{$INCLUDE %FILE%}],
    LM_Info, UnitsFinalizeLMId);
end.
