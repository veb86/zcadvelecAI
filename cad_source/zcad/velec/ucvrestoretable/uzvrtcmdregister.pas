{
*****************************************************************************
* *
* This file is part of the ZCAD *
* *
* See the file COPYING.txt, included in this distribution, *
* for details about the copyright. *
* *
* This program is distributed in the hope that it will be useful, *
* but WITHOUT ANY WARRANTY; without even the implied warranty of *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. *
* *
*****************************************************************************
}
{
@author(Vladimir Bobrov)
} 
{$mode objfpc}{$H+}

{
 Модуль: uzvrtcmdregister
 Назначение: Регистрация команд редактора таблиц
 Описание: Модуль регистрирует команды:
 - InsertTableFromEditor: вставка таблицы из редактора на чертёж
 - LoadSelectedTableToEditor: загрузка выделенной таблицы в редактор
 Зависимости: uzvrtcmdinserttable, uzvrtcmdloadtable
}
unit uzvrtcmdregister;

{$INCLUDE zengineconfig.inc}

interface

uses
 uzvrtcmdinserttable,
 uzvrtcmdloadtable;

implementation

uses
 uzclog;

initialization
 programlog.LogOutFormatStr(
 'Модуль uzvrtcmdregister инициализирован: команды таблиц зарегистрированы',
 [],
 LM_Info
 );

finalization
 programlog.LogOutFormatStr(
 'Модуль uzvrtcmdregister завершён',
 [],
 LM_Info
 );

end.
