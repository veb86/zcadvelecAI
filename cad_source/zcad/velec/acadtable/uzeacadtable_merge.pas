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

{
  Модуль: uzeacadtable_merge
  Назначение: Логика объединения и разъединения ячеек таблицы.
  Проверка принадлежности ячейки к объединённому диапазону,
  определение главной ячейки, расчёт размеров объединённых областей,
  проверка видимости границ.
  Зависимости: uzeacadtable_types, uzclog
}

unit uzeacadtable_merge;
{$Mode delphi}{$H+}
{$INCLUDE zengineconfig.inc}

interface

uses
  Types,
  uzeacadtable_types, uzclog, uzbLogIntf;

type
  TAcadTableSizeGetter = function(Idx: Integer): Double of object;

// Проверяет, находится ли ячейка в каком-либо объединённом диапазоне
function IsCellMerged(
  ARowIdx, AColIdx: Integer;
  const AMerges: array of TMergeRange): Boolean;

// Возвращает координаты главной ячейки объединённого диапазона.
// Если ячейка не объединена — возвращает её собственные координаты.
function GetMergeRoot(
  ARowIdx, AColIdx: Integer;
  const AMerges: array of TMergeRange): TPoint;

// Возвращает суммарную ширину объединённой ячейки.
// Для необъединённой — ширина одного столбца.
function GetMergedCellWidth(
  ARowIdx, AColIdx: Integer;
  const AMerges: array of TMergeRange;
  AGetColWidth: TAcadTableSizeGetter): Double;

// Возвращает суммарную высоту объединённой ячейки.
// Для необъединённой — высота одной строки.
function GetMergedCellHeight(
  ARowIdx, AColIdx: Integer;
  const AMerges: array of TMergeRange;
  AGetRowHeight: TAcadTableSizeGetter): Double;

// Проверяет видимость вертикальной границы между ColIdx и ColIdx+1
// на строке RowIdx. Граница скрыта внутри объединённого диапазона.
function IsColBorderVisible(
  ARowIdx, AColIdx: Integer;
  const AMerges: array of TMergeRange): Boolean;

// Проверяет видимость горизонтальной границы между RowIdx и RowIdx+1
// на столбце ColIdx. Граница скрыта внутри объединённого диапазона.
function IsRowBorderVisible(
  ARowIdx, AColIdx: Integer;
  const AMerges: array of TMergeRange): Boolean;

// Строит массив объединений из ColSpan и RowSpan ячеек.
// Каждая невиртуальная ячейка с ColSpan>1 или RowSpan>1 формирует
// диапазон. Виртуальные ячейки не создают диапазонов.
procedure BuildMergeRanges(
  const ACells: array of array of TTableCell;
  const ACellVirtualFlags: array of Boolean;
  ARowCount, AColCount: Integer;
  var AMerges: array of TMergeRange;
  var AMergeCount: Integer);

implementation

function IsCellMerged(
  ARowIdx, AColIdx: Integer;
  const AMerges: array of TMergeRange): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to High(AMerges) do
    if (ARowIdx >= AMerges[i].Row1) and
       (ARowIdx <= AMerges[i].Row2) and
       (AColIdx >= AMerges[i].Col1) and
       (AColIdx <= AMerges[i].Col2) then
    begin
      Result := True;
      Exit;
    end;
end;

function GetMergeRoot(
  ARowIdx, AColIdx: Integer;
  const AMerges: array of TMergeRange): TPoint;
var
  i: Integer;
begin
  Result.X := AColIdx;
  Result.Y := ARowIdx;
  for i := 0 to High(AMerges) do
    if (ARowIdx >= AMerges[i].Row1) and
       (ARowIdx <= AMerges[i].Row2) and
       (AColIdx >= AMerges[i].Col1) and
       (AColIdx <= AMerges[i].Col2) then
    begin
      Result.X := AMerges[i].Col1;
      Result.Y := AMerges[i].Row1;
      Exit;
    end;
end;

function GetMergedCellWidth(
  ARowIdx, AColIdx: Integer;
  const AMerges: array of TMergeRange;
  AGetColWidth: TAcadTableSizeGetter): Double;
var
  i, Col2: Integer;
begin
  Col2 := AColIdx;
  for i := 0 to High(AMerges) do
    if (ARowIdx >= AMerges[i].Row1) and
       (ARowIdx <= AMerges[i].Row2) and
       (AColIdx = AMerges[i].Col1) then
    begin
      Col2 := AMerges[i].Col2;
      Break;
    end;
  Result := 0;
  for i := AColIdx to Col2 do
    Result := Result + AGetColWidth(i);
end;

function GetMergedCellHeight(
  ARowIdx, AColIdx: Integer;
  const AMerges: array of TMergeRange;
  AGetRowHeight: TAcadTableSizeGetter): Double;
var
  i, Row2: Integer;
begin
  Row2 := ARowIdx;
  for i := 0 to High(AMerges) do
    if (AColIdx >= AMerges[i].Col1) and
       (AColIdx <= AMerges[i].Col2) and
       (ARowIdx = AMerges[i].Row1) then
    begin
      Row2 := AMerges[i].Row2;
      Break;
    end;
  Result := 0;
  for i := ARowIdx to Row2 do
    Result := Result + AGetRowHeight(i);
end;

function IsRowBorderVisible(
  ARowIdx, AColIdx: Integer;
  const AMerges: array of TMergeRange): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to High(AMerges) do
    if (AColIdx >= AMerges[i].Col1) and
       (AColIdx <= AMerges[i].Col2) and
       (ARowIdx >= AMerges[i].Row1) and
       (ARowIdx < AMerges[i].Row2) then
    begin
      Result := False;
      Exit;
    end;
end;

function IsColBorderVisible(
  ARowIdx, AColIdx: Integer;
  const AMerges: array of TMergeRange): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to High(AMerges) do
    if (ARowIdx >= AMerges[i].Row1) and
       (ARowIdx <= AMerges[i].Row2) and
       (AColIdx >= AMerges[i].Col1) and
       (AColIdx < AMerges[i].Col2) then
    begin
      Result := False;
      Exit;
    end;
end;

procedure BuildMergeRanges(
  const ACells: array of array of TTableCell;
  const ACellVirtualFlags: array of Boolean;
  ARowCount, AColCount: Integer;
  var AMerges: array of TMergeRange;
  var AMergeCount: Integer);
var
  RowIdx, ColIdx, CellIndex: Integer;
  MergeRange: TMergeRange;
begin
  AMergeCount := 0;
  for RowIdx := 0 to ARowCount - 1 do
    for ColIdx := 0 to AColCount - 1 do
    begin
      CellIndex := RowIdx * AColCount + ColIdx;
      // Пропускаем виртуальные ячейки
      if (CellIndex < Length(ACellVirtualFlags)) and
         ACellVirtualFlags[CellIndex] then
        Continue;
      // Проверяем наличие объединения
      if (ACells[RowIdx][ColIdx].ColSpan > 1) or
         (ACells[RowIdx][ColIdx].RowSpan > 1) then
      begin
        MergeRange.Row1 := RowIdx;
        MergeRange.Col1 := ColIdx;
        MergeRange.Row2 :=
          RowIdx + ACells[RowIdx][ColIdx].RowSpan - 1;
        MergeRange.Col2 :=
          ColIdx + ACells[RowIdx][ColIdx].ColSpan - 1;
        // Ограничиваем рамками таблицы
        if MergeRange.Row2 >= ARowCount then
          MergeRange.Row2 := ARowCount - 1;
        if MergeRange.Col2 >= AColCount then
          MergeRange.Col2 := AColCount - 1;
        if AMergeCount < Length(AMerges) then
        begin
          AMerges[AMergeCount] := MergeRange;
          programlog.LogOutFormatStr(
            'AcadTable: merge: BuildMergeRanges[%d] ' +
            '(%d,%d)-(%d,%d) colSpan=%d rowSpan=%d',
            [AMergeCount, MergeRange.Row1, MergeRange.Col1,
             MergeRange.Row2, MergeRange.Col2,
             ACells[RowIdx][ColIdx].ColSpan,
             ACells[RowIdx][ColIdx].RowSpan], LM_Info);
        end;
        Inc(AMergeCount);
      end;
    end;
end;

end.
