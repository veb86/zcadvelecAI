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

unit uzvxlsxexport_init;

{$INCLUDE zengineconfig.inc}

interface

uses
  uzvxlsxexport_cmd_register, uzclog;

implementation

// Инициализация модуля при загрузке
initialization
  programlog.LogOutFormatStr(
    'uzvxlsxexport: Инициализация модуля uzvxlsxexport',
    [],
    LM_Info
  );

  // Регистрируем команды
  RegisterUzvXLSXexportCommands;

  programlog.LogOutFormatStr(
    'uzvxlsxexport: Модуль uzvxlsxexport успешно инициализирован',
    [],
    LM_Info
  );

end.
