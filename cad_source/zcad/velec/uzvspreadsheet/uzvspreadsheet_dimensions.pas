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

{** Модуль связывания размеров ячеек электронной таблицы (TsWorksheet)
    с размерами ячеек таблицы ACAD_TABLE (GDBObjAcadTable), issue #1359.

    Содержит:
    - константу масштаба перевода размеров листа в единицы чертежа;
    - функции преобразования "лист <-> ACAD" для одного размера;
    - чтение/запись ширины столбца и высоты строки активной ячейки листа
      в миллиметрах;
    - сбор массивов ширин столбцов и высот строк в единицах чертежа для
      передачи в GDBObjAcadTable.BuildFromCellTextsWithSizes.

    Логика вынесена отдельно от формы (uzvspreadsheet_gui) и команды
    (uzvspreadsheet_cmdcreateacadtable), чтобы её можно было покрыть
    автоматическими тестами. Размеры листа читаются в миллиметрах
    (suMillimeters); единицы по умолчанию у книги fpspreadsheet —
    миллиметры, поэтому перенос в чертёж выполняется один к одному. }
unit uzvspreadsheet_dimensions;

{$INCLUDE zengineconfig.inc}

interface

uses
  SysUtils,
  fpspreadsheet,
  fpsTypes,
  uzeacadtable_types;

const
  // Масштаб перевода размера ячейки листа (мм) в единицы чертежа ACAD.
  // По умолчанию 1:1 — чертёж ведётся в миллиметрах. Значение вынесено
  // в именованную константу, чтобы при необходимости поменять связь.
  CWorksheetToAcadScale = 1.0;

{ Переводит размер ячейки листа (в миллиметрах) в размер ячейки таблицы
  ACAD (в единицах чертежа). }
function WorksheetToAcadSize(AWorksheetMM: Double): Double;

{ Переводит размер ячейки таблицы ACAD (единицы чертежа) обратно в размер
  ячейки листа (в миллиметрах). }
function AcadToWorksheetSize(AAcadSize: Double): Double;

{ Возвращает ширину столбца листа в миллиметрах. Если данные недоступны,
  возвращает 0. }
function GetWorksheetColWidthMM(AWorksheet: TsWorksheet; ACol: Integer): Double;

{ Возвращает высоту строки листа в миллиметрах. Если данные недоступны,
  возвращает 0. }
function GetWorksheetRowHeightMM(AWorksheet: TsWorksheet; ARow: Integer): Double;

{ Задаёт ширину столбца листа в миллиметрах. Возвращает False, если данные
  недоступны или значение не положительно. }
function SetWorksheetColWidthMM(AWorksheet: TsWorksheet;
  ACol: Integer; AWidthMM: Double): Boolean;

{ Задаёт высоту строки листа в миллиметрах. Возвращает False, если данные
  недоступны или значение не положительно. }
function SetWorksheetRowHeightMM(AWorksheet: TsWorksheet;
  ARow: Integer; AHeightMM: Double): Boolean;

{ Собирает ширины столбцов листа в единицах чертежа (для передачи в
  GDBObjAcadTable.BuildFromCellTextsWithSizes). }
function CollectColWidths(AWorksheet: TsWorksheet;
  AColCount: Integer): TTableSizeArray;

{ Собирает высоты строк листа в единицах чертежа. }
function CollectRowHeights(AWorksheet: TsWorksheet;
  ARowCount: Integer): TTableSizeArray;

implementation

function WorksheetToAcadSize(AWorksheetMM: Double): Double;
begin
  Result := AWorksheetMM * CWorksheetToAcadScale;
end;

function AcadToWorksheetSize(AAcadSize: Double): Double;
begin
  if CWorksheetToAcadScale = 0 then
    Result := AAcadSize
  else
    Result := AAcadSize / CWorksheetToAcadScale;
end;

function GetWorksheetColWidthMM(AWorksheet: TsWorksheet; ACol: Integer): Double;
begin
  Result := 0;
  if (AWorksheet = nil) or (ACol < 0) then
    Exit;
  Result := AWorksheet.GetColWidth(Cardinal(ACol), suMillimeters);
end;

function GetWorksheetRowHeightMM(AWorksheet: TsWorksheet; ARow: Integer): Double;
begin
  Result := 0;
  if (AWorksheet = nil) or (ARow < 0) then
    Exit;
  Result := AWorksheet.GetRowHeight(Cardinal(ARow), suMillimeters);
end;

function SetWorksheetColWidthMM(AWorksheet: TsWorksheet;
  ACol: Integer; AWidthMM: Double): Boolean;
begin
  Result := False;
  if (AWorksheet = nil) or (ACol < 0) or (AWidthMM <= 0) then
    Exit;
  AWorksheet.WriteColWidth(Cardinal(ACol), AWidthMM, suMillimeters);
  Result := True;
end;

function SetWorksheetRowHeightMM(AWorksheet: TsWorksheet;
  ARow: Integer; AHeightMM: Double): Boolean;
begin
  Result := False;
  if (AWorksheet = nil) or (ARow < 0) or (AHeightMM <= 0) then
    Exit;
  AWorksheet.WriteRowHeight(Cardinal(ARow), AHeightMM, suMillimeters);
  Result := True;
end;

function CollectColWidths(AWorksheet: TsWorksheet;
  AColCount: Integer): TTableSizeArray;
var
  ColIdx: Integer;
begin
  System.SetLength(Result, 0);
  if (AWorksheet = nil) or (AColCount <= 0) then
    Exit;
  System.SetLength(Result, AColCount);
  for ColIdx := 0 to AColCount - 1 do
    Result[ColIdx] :=
      WorksheetToAcadSize(GetWorksheetColWidthMM(AWorksheet, ColIdx));
end;

function CollectRowHeights(AWorksheet: TsWorksheet;
  ARowCount: Integer): TTableSizeArray;
var
  RowIdx: Integer;
begin
  System.SetLength(Result, 0);
  if (AWorksheet = nil) or (ARowCount <= 0) then
    Exit;
  System.SetLength(Result, ARowCount);
  for RowIdx := 0 to ARowCount - 1 do
    Result[RowIdx] :=
      WorksheetToAcadSize(GetWorksheetRowHeightMM(AWorksheet, RowIdx));
end;

end.
