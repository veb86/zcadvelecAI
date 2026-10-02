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
  Модуль: uzeacadtable_dxf_split
  Назначение: Преобразование данных разрыва ACAD_TABLE из индекса NOD
  (TZAcadTableSplitInfo) в параметры частей объекта таблицы ZCAD и обратно
  (issue #1465). Чистые функции над записями, без доступа к объекту таблицы:
  высоты разбиения частей, флаги ручного положения/высоты, сборка записи
  разрыва для записи DXF по частям таблицы.
  Зависимости: uzeffdxfnodacadtable, uzegeometrytypes
}

unit uzeacadtable_dxf_split;
{$Mode delphi}{$H+}
{$INCLUDE zengineconfig.inc}

interface

uses
  uzegeometrytypes, uzeTypes, uzeffdxfnodacadtable;

type
  // Данные одной части таблицы для сборки записи разрыва при записи DXF.
  // Номер части 0 — главная часть, далее продолжения по порядку.
  TAcadTableSplitWritePart = record
    EntityHandle: TDWGHandle;
    InsertPoint: TzePoint3d;
    // Число строк части вместе с повторёнными метками
    RowCount: Integer;
    // Число повторённых в начале части строк-меток главной части
    RepeatRows: Integer;
    BreakHeight: Double;
  end;
  TAcadTableSplitWriteParts = array of TAcadTableSplitWritePart;

  // Общие параметры разрыва таблицы для записи DXF
  TAcadTableSplitWriteOptions = record
    // Разрыв включён. Без него AutoCAD всё равно пишет запись layout 1 с
    // промежутком и высотой, но без строк-меток (одна часть, issue #1465).
    Enabled: Boolean;
    RepeatTop: Boolean;
    RepeatBottom: Boolean;
    ManualPositions: Boolean;
    ManualHeights: Boolean;
    // Направление разрыва в нотации записи AutoCAD
    // (CAcadTableBreakDirectionRight и т. д.)
    Direction: Integer;
    Spacing: Double;
    TopLabelRows: Integer;
  end;

// True, если для части APartNumber (0 — главная) в записи высот задана
// высота разбиения; AHeight получает её значение.
function AcadTableSplitPartHeight(const AInfo: TZAcadTableSplitInfo;
  APartNumber: Integer; out AHeight: Double): Boolean;

// Признаки ручного положения и ручной высоты частей из флагов разрыва
function AcadTableSplitManualPositions(
  const AInfo: TZAcadTableSplitInfo): Boolean;
function AcadTableSplitManualHeights(
  const AInfo: TZAcadTableSplitInfo): Boolean;

// True, если запись описывает разорванную таблицу с данными разрыва
// (layout 1 или старая запись ZCAD)
function AcadTableSplitHasBreakData(const AInfo: TZAcadTableSplitInfo): Boolean;

// Собирает запись разрыва AutoCAD (layout 1) по частям таблицы. Диапазоны
// строк — логические строки полной таблицы без повторённых меток.
procedure BuildAcadTableSplitWriteInfo(
  const AOptions: TAcadTableSplitWriteOptions;
  const AParts: TAcadTableSplitWriteParts;
  out AInfo: TZAcadTableSplitInfo);

implementation

function AcadTableSplitPartHeight(const AInfo: TZAcadTableSplitInfo;
  APartNumber: Integer; out AHeight: Double): Boolean;
begin
  AHeight := 0;
  Result := (APartNumber >= 0) and (APartNumber <= High(AInfo.Heights)) and
    ((AInfo.Heights[APartNumber].Flags and CAcadTableHeightHasHeight) <> 0);
  if Result then
    AHeight := AInfo.Heights[APartNumber].Height;
end;

function AcadTableSplitManualPositions(
  const AInfo: TZAcadTableSplitInfo): Boolean;
begin
  Result := (AInfo.BreakFlags and CAcadTableBreakManualPositions) <> 0;
end;

function AcadTableSplitManualHeights(
  const AInfo: TZAcadTableSplitInfo): Boolean;
begin
  Result := (AInfo.BreakFlags and CAcadTableBreakManualHeights) <> 0;
end;

function AcadTableSplitHasBreakData(const AInfo: TZAcadTableSplitInfo): Boolean;
begin
  Result := AInfo.Legacy or (AInfo.Layout = CAcadTableLayoutSplit);
end;

// Флаги разрыва по общим параметрам таблицы
function SplitWriteFlags(const AOptions: TAcadTableSplitWriteOptions): Integer;
begin
  Result := 0;
  if AOptions.Enabled then
    Result := CAcadTableBreakEnable;
  if AOptions.RepeatTop then
    Result := Result or CAcadTableBreakRepeatTop;
  if AOptions.RepeatBottom then
    Result := Result or CAcadTableBreakRepeatBottom;
  if AOptions.ManualPositions then
    Result := Result or CAcadTableBreakManualPositions;
  if AOptions.ManualHeights then
    Result := Result or CAcadTableBreakManualHeights;
end;

// Запись высот: у главной части — только высота; у продолжения положение
// пишется при ручном положении, высота — при ручной высоте (как AutoCAD).
procedure FillSplitWriteHeights(const AOptions: TAcadTableSplitWriteOptions;
  const AParts: TAcadTableSplitWriteParts; var AInfo: TZAcadTableSplitInfo);
var
  I: Integer;
begin
  SetLength(AInfo.Heights, Length(AParts));
  for I := 0 to High(AParts) do begin
    AInfo.Heights[I].X := 0;
    AInfo.Heights[I].Y := 0;
    AInfo.Heights[I].Z := 0;
    AInfo.Heights[I].Height := AParts[I].BreakHeight;
    AInfo.Heights[I].Flags := CAcadTableHeightHasHeight;
    if I = 0 then
      Continue;
    AInfo.Heights[I].Flags := 0;
    if AOptions.ManualPositions then begin
      AInfo.Heights[I].X := AParts[I].InsertPoint.x - AParts[0].InsertPoint.x;
      AInfo.Heights[I].Y := AParts[I].InsertPoint.y - AParts[0].InsertPoint.y;
      AInfo.Heights[I].Z := AParts[I].InsertPoint.z - AParts[0].InsertPoint.z;
      AInfo.Heights[I].Flags := CAcadTableHeightHasPosition;
    end;
    if AOptions.ManualHeights then
      AInfo.Heights[I].Flags :=
        AInfo.Heights[I].Flags or CAcadTableHeightHasHeight;
  end;
end;

// Число строк-меток в начале главной части: только у включённого разрыва
// с повтором верхних меток (иначе AutoCAD пишет 0 и диапазон с нуля).
function SplitWriteTopLabelRows(
  const AOptions: TAcadTableSplitWriteOptions): Integer;
begin
  Result := 0;
  if AOptions.Enabled and AOptions.RepeatTop and
     (AOptions.TopLabelRows > 0) then
    Result := AOptions.TopLabelRows;
end;

// Диапазоны логических строк частей; первая часть начинается после меток,
// смещение — от точки вставки главной части.
procedure FillSplitWriteRanges(const AOptions: TAcadTableSplitWriteOptions;
  const AParts: TAcadTableSplitWriteParts; var AInfo: TZAcadTableSplitInfo);
var
  I, NextRow, OwnRows: Integer;
begin
  SetLength(AInfo.RowRanges, Length(AParts));
  NextRow := SplitWriteTopLabelRows(AOptions);
  for I := 0 to High(AParts) do begin
    AInfo.RowRanges[I].OffsetX := AParts[I].InsertPoint.x - AParts[0].InsertPoint.x;
    AInfo.RowRanges[I].OffsetY := AParts[I].InsertPoint.y - AParts[0].InsertPoint.y;
    AInfo.RowRanges[I].OffsetZ := AParts[I].InsertPoint.z - AParts[0].InsertPoint.z;
    if I = 0 then
      OwnRows := AParts[I].RowCount - SplitWriteTopLabelRows(AOptions)
    else
      OwnRows := AParts[I].RowCount - AParts[I].RepeatRows;
    if OwnRows < 0 then
      OwnRows := 0;
    AInfo.RowRanges[I].StartRow := NextRow;
    AInfo.RowRanges[I].EndRow := NextRow + OwnRows - 1;
    Inc(NextRow, OwnRows);
  end;
end;

procedure BuildAcadTableSplitWriteInfo(
  const AOptions: TAcadTableSplitWriteOptions;
  const AParts: TAcadTableSplitWriteParts;
  out AInfo: TZAcadTableSplitInfo);
var
  I: Integer;
begin
  AInfo := Default(TZAcadTableSplitInfo);
  AInfo.Layout := CAcadTableLayoutSplit;
  AInfo.BreakFlags := SplitWriteFlags(AOptions);
  AInfo.BreakDirection := AOptions.Direction;
  AInfo.BreakSpacing := AOptions.Spacing;
  AInfo.TopLabelRows := SplitWriteTopLabelRows(AOptions);
  if Length(AParts) = 0 then
    Exit;
  AInfo.EntityHandle := AParts[0].EntityHandle;
  FillSplitWriteHeights(AOptions, AParts, AInfo);
  FillSplitWriteRanges(AOptions, AParts, AInfo);
  SetLength(AInfo.Continuations, Length(AParts) - 1);
  for I := 1 to High(AParts) do
    AInfo.Continuations[I - 1] := AParts[I].EntityHandle;
end;

end.
