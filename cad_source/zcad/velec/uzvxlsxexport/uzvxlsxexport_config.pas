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

unit uzvxlsxexport_config;

{$INCLUDE zengineconfig.inc}

interface

uses
  SysUtils, Classes;

type
  // Конфигурация экспорта XLSX
  TXlsxExportConfig = class
  private
    FTemplatePath: String;
    FOutputPath: String;
    FAccessDbPath: String;
    FStopOnError: Boolean;
  public
    constructor Create;

    // Путь к шаблону XLSX
    property TemplatePath: String read FTemplatePath write FTemplatePath;

    // Путь к выходному файлу XLSX
    property OutputPath: String read FOutputPath write FOutputPath;

    // Путь к БД Access (для getaccessfile)
    property AccessDbPath: String read FAccessDbPath write FAccessDbPath;

    // Остановить при ошибке
    property StopOnError: Boolean read FStopOnError write FStopOnError;
  end;

implementation

{ TXlsxExportConfig }

constructor TXlsxExportConfig.Create;
begin
  FTemplatePath := '';
  FOutputPath := '';
  FAccessDbPath := '';
  FStopOnError := False;
end;

end.
