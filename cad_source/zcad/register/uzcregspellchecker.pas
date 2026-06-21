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

unit uzcregspellchecker;
{$INCLUDE zengineconfig.inc}
interface

uses
  Types, Controls,
  uzcguimanager,
  uzvfspellform,
  uzvfuserdictform;

implementation

uses
  uzclog;

procedure uzvfspellcheckerSetupProc(Form: TControl);
begin
  programlog.LogOutFormatStr('uzvfspellcheckerSetupProc: setup started',
    [], LM_Info);

  if Form = nil then begin
    programlog.LogOutFormatStr('uzvfspellcheckerSetupProc: nil form',
      [], LM_Info);
    Exit;
  end;

  if Form is TSpellCheckerForm then
    TSpellCheckerForm(Form).CheckCurrentDrawing
  else
    programlog.LogOutFormatStr(
      'uzvfspellcheckerSetupProc: unexpected form class "%s"',
      [Form.ClassName], LM_Info);
end;

// При показе формы пользовательского словаря перечитываем список слов с диска,
// чтобы он отражал актуальное содержимое словаря (issue #1361).
procedure uzvfuserdictSetupProc(Form: TControl);
begin
  programlog.LogOutFormatStr('uzvfuserdictSetupProc: setup started',
    [], LM_Info);

  if Form = nil then begin
    programlog.LogOutFormatStr('uzvfuserdictSetupProc: nil form', [], LM_Info);
    Exit;
  end;

  if Form is TUserDictForm then
    TUserDictForm(Form).ReloadWords
  else
    programlog.LogOutFormatStr(
      'uzvfuserdictSetupProc: unexpected form class "%s"',
      [Form.ClassName], LM_Info);
end;

initialization
  ZCADGUIManager.RegisterZCADFormInfo('uzvfspellchecker','uzvfspellchecker',TSpellCheckerForm,Rect(0,100,900,600),@uzvfspellcheckerSetupProc,nil,@SpellCheckerForm,true);
  ZCADGUIManager.RegisterZCADFormInfo('uzvfuserdict','uzvfuserdict',TUserDictForm,Rect(0,100,500,600),@uzvfuserdictSetupProc,nil,@UserDictForm,true);
finalization
end.
