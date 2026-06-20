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
@author(Andrey Zubarev <zamtmn@yandex.ru>) 
}

unit uzcSpeller;
{$INCLUDE zengineconfig.inc}
{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}
{$Codepage UTF8}

interface

uses
  SysUtils,FileUtil,
  uHunspell,uSpeller,
  uzbPaths,
  uzcLog,uzbLog,uzbLogTypes,
  uzcSysParams,uzcFileStructure,
  uzelongprocesssupport;

const
  // Имя пользовательского словаря, в который добавляются новые слова.
  // Хранится в той же папке (writable config), что и сохранённые панели.
  CUserDictionaryFile='userwords.dic';

var
  SpellChecker:TSpeller;

procedure CreateSpellChecker;
procedure DestroySpellChecker;
procedure ReloadSpellChecker;

// Путь к пользовательскому словарю в writable config (см. SaveLayout).
function GetUserDictionaryPath:string;

implementation

var
  LMDSpeller:TModuleDesk;
  //lph:TLPSHandle;

procedure SpellLogCallBack(MsgType:TMsgType;Msg:TMsg);
begin
  case MsgType of
    MsgInfo:
      ProgramLog.LogOutStr(Msg,LM_Info,LMDSpeller);
    MsgCriticalInfo:
      ProgramLog.LogOutStr(Msg,LM_Necessarily,LMDSpeller);
    MsgWarning:
      ProgramLog.LogOutStr(Msg,LM_Warning,LMDSpeller);
    MsgError:
      ProgramLog.LogOutStr(Msg,LM_Error,LMDSpeller);
  end;
end;

function GetUserDictionaryPath:string;
begin
  Result:=GetWritableFilePath(CFSdictionariesDir,CUserDictionaryFile);
end;

procedure CreateSpellChecker;
var
  lph:TLPSHandle;
  UserDict:string;
begin
  lph:=LPS.StartLongProcess('SpellChecker.Create and LoadDictionaries',nil);
  SpellChecker.CreateRec(@SpellLogCallBack);
  SpellChecker.LoadDictionaries(ExpandPath(ZCSysParams.saved.DictionariesPath));
  // Подключить пользовательский словарь, если он уже создан
  UserDict:=GetUserDictionaryPath;
  if FileExists(UserDict) then
    SpellChecker.LoadDictionary(UserDict);
  LPS.EndLongProcess(lph);
end;

procedure DestroySpellChecker;
begin
  SpellChecker.DestroyRec;
end;

// Пересоздать SpellChecker, чтобы вновь добавленные в пользовательский
// словарь слова сразу учитывались в текущей сессии (см. issue #1296).
procedure ReloadSpellChecker;
begin
  DestroySpellChecker;
  CreateSpellChecker;
end;

initialization
  LMDSpeller:=ProgramLog.RegisterModule('Speller/Hunspell');
  CreateSpellChecker;
  {t:=SpellChecker.SpellTextSimple('претворяя в');
  t:=SpellChecker.SpellTextSimple('претворяя the');
  t:=SpellChecker.SpellTextSimple('претворяя ##');
  t:=SpellChecker.SpellTextSimple('##');
  t:=SpellChecker.SpellWord('притворяя');
  t:=SpellChecker.SpellWord('the');
  t:=SpellChecker.SpellWord('притваряя');}
finalization
  DestroySpellChecker;
end.

