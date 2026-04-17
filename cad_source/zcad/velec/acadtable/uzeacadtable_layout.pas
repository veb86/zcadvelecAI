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
  Модуль: uzeacadtable_layout
  Назначение: Расчёт размеров и геометрии таблицы —
  суммарная ширина/высота, сегменты визуализации (разрывы),
  bounding box.
  Зависимости: uzeacadtable_types, uzctnrvectordouble
}

unit uzeacadtable_layout;
{$Mode delphi}{$H+}
{$INCLUDE zengineconfig.inc}

interface

uses
  uzeacadtable_types, uzctnrvectordouble;

// Возвращает высоту строки по индексу.
// При выходе за границы — значение по умолчанию.
function GetRowHeight(
  RowIndex: Integer;
  const ARowHeights: TZctnrVectorDouble): Double;

// Возвращает ширину столбца по индексу.
// При выходе за границы — значение по умолчанию.
function GetColWidth(
  ColIndex: Integer;
  const AColWidths: TZctnrVectorDouble): Double;

// Возвращает суммарную высоту всех строк таблицы
function GetTotalHeight(
  ARowCount: Integer;
  const ARowHeights: TZctnrVectorDouble): Double;

// Возвращает суммарную ширину всех столбцов таблицы
function GetTotalWidth(
  AColCount: Integer;
  const AColWidths: TZctnrVectorDouble): Double;

// Вычисляет сегменты визуализации для разорванной таблицы
procedure BuildRenderSegments(
  ARowCount: Integer;
  const ARowHeights: TZctnrVectorDouble;
  const AColWidths: TZctnrVectorDouble;
  AColCount: Integer;
  ATableFlags: Integer;
  ABreakEnabled: Boolean;
  ABreakDirection: TAcadTableBreakDirection;
  ABreakRepeatTopLabels: Boolean;
  ABreakManualHeight: Boolean;
  ABreakSpacing: Double;
  out ASegments: array of TAcadTableRenderSegment;
  out ASegmentCount: Integer);

implementation

function GetRowHeight(
  RowIndex: Integer;
  const ARowHeights: TZctnrVectorDouble): Double;
begin
  if (RowIndex >= 0) and (RowIndex < ARowHeights.Count) then
    Result := ARowHeights.parray^[RowIndex]
  else
    Result := CAcadTableDefaultRowHeight;
end;

function GetColWidth(
  ColIndex: Integer;
  const AColWidths: TZctnrVectorDouble): Double;
begin
  if (ColIndex >= 0) and (ColIndex < AColWidths.Count) then
    Result := AColWidths.parray^[ColIndex]
  else
    Result := CAcadTableDefaultColWidth;
end;

function GetTotalHeight(
  ARowCount: Integer;
  const ARowHeights: TZctnrVectorDouble): Double;
var
  RowIdx: Integer;
begin
  Result := 0;
  for RowIdx := 0 to ARowCount - 1 do
    Result := Result + GetRowHeight(RowIdx, ARowHeights);
end;

function GetTotalWidth(
  AColCount: Integer;
  const AColWidths: TZctnrVectorDouble): Double;
var
  ColIdx: Integer;
begin
  Result := 0;
  for ColIdx := 0 to AColCount - 1 do
    Result := Result + GetColWidth(ColIdx, AColWidths);
end;

procedure BuildRenderSegments(
  ARowCount: Integer;
  const ARowHeights: TZctnrVectorDouble;
  const AColWidths: TZctnrVectorDouble;
  AColCount: Integer;
  ATableFlags: Integer;
  ABreakEnabled: Boolean;
  ABreakDirection: TAcadTableBreakDirection;
  ABreakRepeatTopLabels: Boolean;
  ABreakManualHeight: Boolean;
  ABreakSpacing: Double;
  out ASegments: array of TAcadTableRenderSegment;
  out ASegmentCount: Integer);
var
  TitleSuppressed, HeaderSuppressed: Boolean;
  RepeatRowCount: Integer;
  BaseSegmentHeight: Double;
  RowIdx, SegmentIdx, SegmentStart, SegmentEnd: Integer;
  CurrentSegmentHeight, RemainingHeight, HeaderHeight: Double;
  SegmentHeightWithRepeats, SegmentWidth, DirectionSign: Double;
begin
  ASegmentCount := 0;
  if (ARowCount <= 0) or (Length(ASegments) = 0) then
    Exit;

  TitleSuppressed := (ATableFlags and 2) <> 0;
  HeaderSuppressed := (ATableFlags and 4) <> 0;

  RepeatRowCount := 0;
  if ABreakRepeatTopLabels then
  begin
    if (not TitleSuppressed) and (ARowCount > 0) then
      Inc(RepeatRowCount);
    if (not HeaderSuppressed) and (ARowCount > 1) then
      Inc(RepeatRowCount);
  end;

  BaseSegmentHeight := GetTotalHeight(ARowCount, ARowHeights);
  if ABreakEnabled and (not ABreakManualHeight) and
     (RepeatRowCount > 0) then
  begin
    HeaderHeight := 0;
    for RowIdx := 0 to RepeatRowCount - 1 do
      HeaderHeight :=
        HeaderHeight + GetRowHeight(RowIdx, ARowHeights);
    BaseSegmentHeight := BaseSegmentHeight - HeaderHeight;
    if BaseSegmentHeight <= 0 then
      BaseSegmentHeight :=
        GetTotalHeight(ARowCount, ARowHeights);
  end;

  SegmentIdx := 0;
  SegmentStart := 0;
  while (SegmentStart < ARowCount) and
        (SegmentIdx < Length(ASegments)) do
  begin
    SegmentEnd := SegmentStart;
    CurrentSegmentHeight := 0;
    while SegmentEnd < ARowCount do
    begin
      RemainingHeight :=
        CurrentSegmentHeight +
        GetRowHeight(SegmentEnd, ARowHeights);
      if SegmentEnd > SegmentStart then
      begin
        SegmentHeightWithRepeats := RemainingHeight;
        if SegmentStart > 0 then
          SegmentHeightWithRepeats :=
            SegmentHeightWithRepeats +
            (GetTotalHeight(ARowCount, ARowHeights) -
             BaseSegmentHeight);
        if ABreakEnabled and (not ABreakManualHeight) and
           (SegmentHeightWithRepeats >
            BaseSegmentHeight + 1e-9) then
          Break;
      end;
      CurrentSegmentHeight := RemainingHeight;
      Inc(SegmentEnd);
    end;

    if SegmentEnd = SegmentStart then
      Inc(SegmentEnd);

    ASegments[SegmentIdx].StartRow := SegmentStart;
    ASegments[SegmentIdx].EndRow := SegmentEnd - 1;
    ASegments[SegmentIdx].OffsetX := 0;
    ASegments[SegmentIdx].OffsetY := 0;

    if SegmentIdx > 0 then
    begin
      case ABreakDirection of
        atbdDown:
          ASegments[SegmentIdx].OffsetY :=
            -SegmentIdx *
            (BaseSegmentHeight + ABreakSpacing);
        atbdLeft:
          begin
            SegmentWidth :=
              GetTotalWidth(AColCount, AColWidths);
            DirectionSign := -1;
            ASegments[SegmentIdx].OffsetX :=
              DirectionSign * SegmentIdx *
              (SegmentWidth + ABreakSpacing);
          end;
      else
        begin
          SegmentWidth :=
            GetTotalWidth(AColCount, AColWidths);
          DirectionSign := 1;
          ASegments[SegmentIdx].OffsetX :=
            DirectionSign * SegmentIdx *
            (SegmentWidth + ABreakSpacing);
        end;
      end;
    end;

    Inc(SegmentIdx);
    SegmentStart := SegmentEnd;
  end;

  ASegmentCount := SegmentIdx;
end;

end.
