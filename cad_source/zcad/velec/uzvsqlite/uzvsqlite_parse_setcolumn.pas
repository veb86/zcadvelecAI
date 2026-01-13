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

unit uzvsqlite_parse_setcolumn;

{$INCLUDE zengineconfig.inc}

interface

uses
  SysUtils, uzvsqlite_types, uzclog;

// Парсинг инструкции setcolumn
// Col2 - имя колонки, Col3 - тип данных, Col4 - параметр источника
procedure ParseSetColumnInstruction(
  AInstruction: TExportInstructions;
  const ACol2, ACol3, ACol4: String
);

implementation

procedure ParseSetColumnInstruction(
  AInstruction: TExportInstructions;
  const ACol2, ACol3, ACol4: String
);
var
  mapping: TColumnMapping;
begin
  // Проверка обязательных параметров
  if (ACol2 = '') or (ACol3 = '') or (ACol4 = '') then
  begin
    programlog.LogOutFormatStr(
      'uzvsqlite: Инструкция setcolumn с неполными параметрами - пропускается',
      [],
      LM_Info
    );
    Exit;
  end;

  // Создание маппинга колонки
  mapping := TColumnMapping.Create;
  mapping.ColumnName := ACol2;
  mapping.DataType := StringToColumnDataType(ACol3);
  mapping.SourceParam := ACol4;

  // Проверка наличия плейсхолдера цикла в имени параметра или имени колонки
  mapping.HasLoopPlaceholder := (Pos(LOOP_NUMBER_PLACEHOLDER, ACol4) > 0) or
                                (Pos(LOOP_NUMBER_PLACEHOLDER, ACol2) > 0);

  AInstruction.AddColumnMapping(mapping);

  if mapping.HasLoopPlaceholder then
    programlog.LogOutFormatStr(
      'uzvsqlite: setcolumn - колонка: %s, тип: %s, источник: %s (с плейсхолдером цикла)',
      [ACol2, ACol3, ACol4],
      LM_Info
    )
  else
    programlog.LogOutFormatStr(
      'uzvsqlite: setcolumn - колонка: %s, тип: %s, источник: %s',
      [ACol2, ACol3, ACol4],
      LM_Info
    );
end;

end.
