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

{** Сборочный модуль команд редактора электронных таблиц.

    Единственная задача модуля — подключить в сборку все модули команд,
    чтобы их секции initialization выполнились и команды зарегистрировались
    в реестре (uzvspreadsheet_cmdregistry).

    Чтобы добавить новую кнопку/команду в редактор таблиц, создайте отдельный
    модуль команды (по образцу существующих uzvspreadsheet_cmd*), который в
    своей секции initialization вызывает RegisterSpreadsheetCommand, и добавьте
    его имя в секцию uses ниже. Изменять модуль формы uzvspreadsheet_gui при
    этом не требуется. }
unit uzvspreadsheet_commands;

{$INCLUDE zengineconfig.inc}

interface

uses
  // Базовый реестр команд
  uzvspreadsheet_cmdregistry,
  // Модули команд редактора электронных таблиц (саморегистрация в initialization)
  uzvspreadsheet_cmdnewbook,
  uzvspreadsheet_cmdopenbook,
  uzvspreadsheet_cmdsavebook,
  uzvspreadsheet_cmdcalc,
  uzvspreadsheet_cmdundoredo,
  uzvspreadsheet_cmdrowcolumns,
  uzvspreadsheet_cmdmergecells,
  uzvspreadsheet_cmdfillspaceroom,
  uzvspreadsheet_cmdinserttable,
  uzvspreadsheet_cmdcreateacadtable;

implementation

end.
