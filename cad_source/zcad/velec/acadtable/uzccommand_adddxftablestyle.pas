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
  Модуль: uzccommand_adddxftablestyle
  Назначение: добавляет новый стиль таблицы DXF с заданными параметрами.
  Создаёт стиль "Новый" по образцу стиля "Standard", но с изменёнными
  высотами текста: title=111, header=222, data=333.
  Зависимости: uzclog, uzcdrawings, uzestylestablesdxf
}
unit uzccommand_adddxftablestyle;
{$Mode delphi}{$H+}
{$INCLUDE zengineconfig.inc}

interface

uses
  uzclog,
  uzcdrawings,
  uzestylestablesdxf,
  uzccommandsabstract,
  uzccommandsimpl;

implementation

const
  CNormalTitleHeight = 111.0;
  CNormalHeaderHeight = 222.0;
  CNormalDataHeight = 333.0;
  CNormalAlignment = 5;
  CNormalTextColor = 0;
  CNormalBackgroundColor = 7;
  CNormalBackgroundEnabled = False;

function AddDXFTableStyle_cmd(const Context: TZCADCommandContext;
  operands: TCommandOperands): TCommandResult;
var
  TableStyleTable: PGDBDXFTableStyleArray;
  NewStyle: PTGDBDXFTableStyle;
  CellStyle: TGDBDXFTableCellStyle;
begin
  programlog.LogOutFormatStr(
    'uzccommand_adddxftablestyle: добавление стиля таблицы "Новый"',
    [], LM_Info);

  TableStyleTable := @drawings.GetCurrentDWG^.DXFTableStyleTable;
  NewStyle := TableStyleTable^.AddStyle('Новый');

  if NewStyle = nil then
  begin
    programlog.LogOutFormatStr(
      'uzccommand_adddxftablestyle: ошибка создания стиля',
      [], LM_Info);
    Result := cmd_error;
    Exit;
  end;

  FillChar(CellStyle, SizeOf(CellStyle), 0);

  CellStyle.TextHeight := CNormalTitleHeight;
  CellStyle.Alignment := CNormalAlignment;
  CellStyle.TextColor := CNormalTextColor;
  CellStyle.BackgroundColor := CNormalBackgroundColor;
  CellStyle.BackgroundColorEnabled := CNormalBackgroundEnabled;
  NewStyle^.CellFormats.PushBackData(CellStyle);
  NewStyle^.CellTextStyleName[0] := 'Standard';

  FillChar(CellStyle, SizeOf(CellStyle), 0);
  CellStyle.TextHeight := CNormalHeaderHeight;
  CellStyle.Alignment := CNormalAlignment;
  CellStyle.TextColor := CNormalTextColor;
  CellStyle.BackgroundColor := CNormalBackgroundColor;
  CellStyle.BackgroundColorEnabled := CNormalBackgroundEnabled;
  NewStyle^.CellFormats.PushBackData(CellStyle);
  NewStyle^.CellTextStyleName[1] := 'Standard';

  FillChar(CellStyle, SizeOf(CellStyle), 0);
  CellStyle.TextHeight := CNormalDataHeight;
  CellStyle.Alignment := CNormalAlignment;
  CellStyle.TextColor := CNormalTextColor;
  CellStyle.BackgroundColor := CNormalBackgroundColor;
  CellStyle.BackgroundColorEnabled := CNormalBackgroundEnabled;
  NewStyle^.CellFormats.PushBackData(CellStyle);
  NewStyle^.CellTextStyleName[2] := 'Standard';

  programlog.LogOutFormatStr(
    'uzccommand_adddxftablestyle: стиль "Новый" добавлен успешно',
    [], LM_Info);

  Result := cmd_ok;
end;

initialization
  programlog.LogOutFormatStr('Unit "%s" initialization', [{$INCLUDE %FILE%}],
    LM_Info, UnitsInitializeLMId);
  CreateZCADCommand(@AddDXFTableStyle_cmd, 'AddDXFTableStyle', CADWG, 0);

finalization
  ProgramLog.LogOutFormatStr('Unit "%s" finalization', [{$INCLUDE %FILE%}],
    LM_Info, UnitsFinalizeLMId);
end.