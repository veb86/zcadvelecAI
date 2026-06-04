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
  uzvfspellform;

implementation

procedure uzvfspellcheckerSetupProc({%H-}Form: TControl);
begin
  // Reserved for future spellchecker form setup.
end;

initialization
  ZCADGUIManager.RegisterZCADFormInfo('uzvfspellchecker','uzvfspellchecker',TSpellCheckerForm,Rect(0,100,900,600),@uzvfspellcheckerSetupProc,nil,@SpellCheckerForm,true);
finalization
end.
