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
// Команда SpellUserDict открывает форму управления пользовательским словарём
// орфографии (issue #1361): просмотр, поиск и удаление добавленных слов.
{$mode delphi}
unit uzccommand_spelluserdict;

{$INCLUDE zengineconfig.inc}

interface

uses
  SysUtils,
  uzcLog,
  uzcinterface,
  uzccommandsabstract,uzccommandsimpl;

implementation

const
  // Имя зарегистрированной формы пользовательского словаря (см.
  // uzcregspellchecker.RegisterZCADFormInfo).
  CUserDictFormName = 'uzvfuserdict';

function SpellUserDict_com(const Context:TZCADCommandContext;
  operands:TCommandOperands):TCommandResult;
begin
  zcUI.ShowForm(CUserDictFormName);
  Result:=cmd_ok;
end;

initialization
  programlog.LogOutFormatStr('Unit "%s" initialization',[{$INCLUDE %FILE%}],
    LM_Info,UnitsInitializeLMId);
  CreateZCADCommand(@SpellUserDict_com,'SpellUserDict',0,0);

finalization
  ProgramLog.LogOutFormatStr('Unit "%s" finalization',[{$INCLUDE %FILE%}],
    LM_Info,UnitsFinalizeLMId);
end.
