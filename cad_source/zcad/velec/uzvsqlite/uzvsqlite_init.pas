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

unit uzvsqlite_init;

{$INCLUDE zengineconfig.inc}

interface

uses
  uzvsqlite_cmd_register, uzclog;

implementation

// Инициализация модуля при загрузке
initialization
  programlog.LogOutFormatStr(
    'uzvsqlite: Инициализация модуля uzvsqlite',
    [],
    LM_Info
  );

  // Регистрируем команды
  RegisterUzvSQLiteCommands;

  programlog.LogOutFormatStr(
    'uzvsqlite: Модуль uzvsqlite успешно инициализирован',
    [],
    LM_Info
  );

end.
