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

{ Задаёт одинаковую ширину для диапазона столбцов листа [AStartCol..AEndCol].
  Применяется к нескольким выделенным столбцам (issue #1359). Возвращает
  количество фактически изменённых столбцов. }
function SetWorksheetColWidthsMM(AWorksheet: TsWorksheet;
  AStartCol, AEndCol: Integer; AWidthMM: Double): Integer;

{ Задаёт одинаковую высоту для диапазона строк листа [AStartRow..AEndRow].
  Применяется к нескольким выделенным строкам (issue #1359). Возвращает
  количество фактически изменённых строк. }
function SetWorksheetRowHeightsMM(AWorksheet: TsWorksheet;
  AStartRow, AEndRow: Integer; AHeightMM: Double): Integer;

{ Собирает ширины столбцов листа в единицах чертежа (для передачи в
  GDBObjAcadTable.BuildFromCellTextsWithSizes). }
function CollectColWidths(AWorksheet: TsWorksheet;
  AColCount: Integer): TTableSizeArray;

{ Собирает высоты строк листа в единицах чертежа. }
function CollectRowHeights(AWorksheet: TsWorksheet;
  ARowCount: Integer): TTableSizeArray;

{ Преобразует пару выравниваний fpspreadsheet (горизонтальное и вертикальное)
  в код выравнивания AutoCAD (group 170, значения 1..9). Значения "по
  умолчанию" (haDefault/vaDefault) трактуются как левое/верхнее — так же, как
  в комбобоксе выравнивания редактора (issue #1352). }
function WorksheetAlignmentToAcad(AHor: TsHorAlignment;
  AVert: TsVertAlignment): Integer;

{ Преобразует код выравнивания AutoCAD (1..9) обратно в пару выравниваний
  fpspreadsheet. Некорректный код трактуется как левое/верхнее. }
procedure AcadAlignmentToWorksheet(AAlignment: Integer;
  out AHor: TsHorAlignment; out AVert: TsVertAlignment);

{ Собирает выравнивания ячеек листа в кодировке AutoCAD для передачи в
  GDBObjAcadTable.BuildFromCellTextsWithSizesAndAlignments (issue #1363).
  Значения "по умолчанию" преобразуются в фактическое выравнивание листа
  слева/сверху, а не в наследование от стиля таблицы (issue #1399). }
function CollectCellAlignments(AWorksheet: TsWorksheet;
  ARowCount, AColCount: Integer): TTableAlignmentArray;

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

function SetWorksheetColWidthsMM(AWorksheet: TsWorksheet;
  AStartCol, AEndCol: Integer; AWidthMM: Double): Integer;
var
  ColIdx: Integer;
begin
  Result := 0;
  for ColIdx := AStartCol to AEndCol do
    if SetWorksheetColWidthMM(AWorksheet, ColIdx, AWidthMM) then
      Inc(Result);
end;

function SetWorksheetRowHeightsMM(AWorksheet: TsWorksheet;
  AStartRow, AEndRow: Integer; AHeightMM: Double): Integer;
var
  RowIdx: Integer;
begin
  Result := 0;
  for RowIdx := AStartRow to AEndRow do
    if SetWorksheetRowHeightMM(AWorksheet, RowIdx, AHeightMM) then
      Inc(Result);
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

{ Группа горизонтального выравнивания: 0 - левое, 1 - центр, 2 - правое.
  Совпадает с логикой комбобокса выравнивания редактора (issue #1352). }
function HorAlignToGroup(AHor: TsHorAlignment): Integer;
begin
  // Константы выравнивания квалифицируем именем модуля fpsTypes: модуль
  // uzeacadtable_types объявляет одноимённые THorzAlign (haLeft/haCenter/
  // haRight), которые перекрывают значения TsHorAlignment и без квалификации
  // ломают сборку ("Constant and CASE types do not match").
  case AHor of
    fpsTypes.haCenter: Result := 1;
    fpsTypes.haRight:  Result := 2;
    else               Result := 0;  // haLeft, haDefault
  end;
end;

{ Группа вертикального выравнивания: 0 - верх, 1 - центр, 2 - низ. }
function VertAlignToGroup(AVert: TsVertAlignment): Integer;
begin
  // Аналогично горизонтали: uzeacadtable_types объявляет TVertAlign
  // (vaTop/vaMiddle/vaBottom), перекрывающий TsVertAlignment, поэтому
  // квалифицируем константы именем модуля fpsTypes.
  case AVert of
    fpsTypes.vaCenter: Result := 1;
    fpsTypes.vaBottom: Result := 2;
    else               Result := 0;  // vaTop, vaDefault
  end;
end;

function WorksheetAlignmentToAcad(AHor: TsHorAlignment;
  AVert: TsVertAlignment): Integer;
begin
  // Код AutoCAD (group 170): 1..9 = вертикаль*3 + горизонталь + 1,
  // где 1=TopLeft .. 9=BottomRight.
  Result := VertAlignToGroup(AVert) * 3 + HorAlignToGroup(AHor) + 1;
end;

procedure AcadAlignmentToWorksheet(AAlignment: Integer;
  out AHor: TsHorAlignment; out AVert: TsVertAlignment);
const
  HorByGroup: array[0..2] of TsHorAlignment = (
    fpsTypes.haLeft, fpsTypes.haCenter, fpsTypes.haRight);
  VertByGroup: array[0..2] of TsVertAlignment = (
    fpsTypes.vaTop, fpsTypes.vaCenter, fpsTypes.vaBottom);
var
  AlignmentIndex: Integer;
begin
  AHor := fpsTypes.haLeft;
  AVert := fpsTypes.vaTop;
  if (AAlignment < 1) or (AAlignment > 9) then
    Exit;
  AlignmentIndex := AAlignment - 1;
  AHor := HorByGroup[AlignmentIndex mod 3];
  AVert := VertByGroup[AlignmentIndex div 3];
end;

function CollectCellAlignments(AWorksheet: TsWorksheet;
  ARowCount, AColCount: Integer): TTableAlignmentArray;
var
  RowIdx, ColIdx: Integer;
  Cell: PCell;
  Hor: TsHorAlignment;
  Vert: TsVertAlignment;
begin
  System.SetLength(Result, 0);
  if (AWorksheet = nil) or (ARowCount <= 0) or (AColCount <= 0) then
    Exit;
  System.SetLength(Result, ARowCount * AColCount);
  for RowIdx := 0 to ARowCount - 1 do
    for ColIdx := 0 to AColCount - 1 do
    begin
      // Пустая ячейка не имеет записи fpspreadsheet и получает фактическое
      // выравнивание листа по умолчанию: слева/сверху (код AutoCAD 1).
      Result[RowIdx * AColCount + ColIdx] :=
        WorksheetAlignmentToAcad(fpsTypes.haDefault, fpsTypes.vaDefault);
      Cell := AWorksheet.FindCell(Cardinal(RowIdx), Cardinal(ColIdx));
      if Cell = nil then
        Continue;
      Hor := AWorksheet.ReadHorAlignment(Cell);
      Vert := AWorksheet.ReadVertAlignment(Cell);
      // Даже полностью стандартное выравнивание надо записывать явно.
      // Иначе ACAD_TABLE применяет MiddleCenter из стиля таблицы.
      Result[RowIdx * AColCount + ColIdx] :=
        WorksheetAlignmentToAcad(Hor, Vert);
    end;
end;

end.
