{ Standalone test for the spreadsheet cell-size linkage logic (issue #1359).

  fpspreadsheet is not exercised here; this program replicates the pure
  conversion and fallback logic implemented in:
    * cad_source/zcad/velec/uzvspreadsheet/uzvspreadsheet_dimensions.pas
      (WorksheetToAcadSize / AcadToWorksheetSize)
    * cad_source/zcad/velec/acadtable/uzeacadtable_model.pas
      (SizeOrDefault, used by BuildFromCellTextsWithSizes)

  It verifies:
    * worksheet (mm) -> ACAD (drawing units) conversion at the 1:1 scale,
    * the conversion round-trips,
    * SizeOrDefault picks provided positive sizes and falls back to the
      default for missing / zero / negative entries,
    * collecting an array of widths sums to the expected total.

  Build & run:
    fpc -Mobjfpc experiments/test_dimension_mapping.pas \
      && ./experiments/test_dimension_mapping
}
program test_dimension_mapping;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  TTableSizeArray = array of Double;

const
  // Mirror of constants from the production units
  CWorksheetToAcadScale = 1.0;
  CAcadTableDefaultColWidth = 30.0;
  CAcadTableDefaultRowHeight = 10.0;

{ ---- logic copied verbatim from uzvspreadsheet_dimensions.pas ---- }

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

{ ---- logic copied verbatim from uzeacadtable_model.pas ---- }

function SizeOrDefault(const ASizes: TTableSizeArray;
  AIndex: Integer; ADefault: Double): Double;
begin
  if (AIndex >= 0) and (AIndex <= High(ASizes)) and (ASizes[AIndex] > 0) then
    Result := ASizes[AIndex]
  else
    Result := ADefault;
end;

{ ---- test harness ---- }

var
  Failures: Integer = 0;

procedure Check(aCondition: Boolean; const aMsg: string);
begin
  if aCondition then
    WriteLn('  ok  : ', aMsg)
  else
  begin
    WriteLn('  FAIL: ', aMsg);
    Inc(Failures);
  end;
end;

function NearlyEqual(a, b: Double): Boolean;
begin
  Result := Abs(a - b) < 1e-9;
end;

var
  widths: TTableSizeArray;
  total: Double;
  i: Integer;
begin
  WriteLn('Testing cell-size linkage (issue #1359)');

  // Conversion at 1:1 scale
  Check(NearlyEqual(WorksheetToAcadSize(25.4), 25.4),
    'worksheet 25.4 mm -> acad 25.4 units');
  Check(NearlyEqual(AcadToWorksheetSize(WorksheetToAcadSize(42.0)), 42.0),
    'conversion round-trips');

  // SizeOrDefault: provided positive value is used
  SetLength(widths, 3);
  widths[0] := 40; widths[1] := 0; widths[2] := -5;
  Check(NearlyEqual(SizeOrDefault(widths, 0, CAcadTableDefaultColWidth), 40),
    'positive width used as-is');
  Check(NearlyEqual(SizeOrDefault(widths, 1, CAcadTableDefaultColWidth),
    CAcadTableDefaultColWidth), 'zero width -> default');
  Check(NearlyEqual(SizeOrDefault(widths, 2, CAcadTableDefaultColWidth),
    CAcadTableDefaultColWidth), 'negative width -> default');
  Check(NearlyEqual(SizeOrDefault(widths, 5, CAcadTableDefaultColWidth),
    CAcadTableDefaultColWidth), 'out-of-range index -> default');

  // Empty array always yields the default (default-row-height case)
  SetLength(widths, 0);
  Check(NearlyEqual(SizeOrDefault(widths, 0, CAcadTableDefaultRowHeight),
    CAcadTableDefaultRowHeight), 'empty array -> default');

  // Collecting widths and summing them (model builds total this way)
  SetLength(widths, 3);
  widths[0] := 40; widths[1] := 50; widths[2] := 60;
  total := 0;
  for i := 0 to High(widths) do
    total := total + SizeOrDefault(widths, i, CAcadTableDefaultColWidth);
  Check(NearlyEqual(total, 150), 'sum of widths 40+50+60 = 150');

  WriteLn;
  if Failures = 0 then
    WriteLn('ALL TESTS PASSED')
  else
  begin
    WriteLn(Failures, ' TEST(S) FAILED');
    Halt(1);
  end;
end.
