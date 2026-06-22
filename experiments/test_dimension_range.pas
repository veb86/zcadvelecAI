{ Standalone test for the multi-column / multi-row size logic (issue #1359).

  fpspreadsheet is not exercised here; this program replicates the pure
  range-setting logic implemented in
    cad_source/zcad/velec/uzvspreadsheet/uzvspreadsheet_dimensions.pas
      (SetWorksheetColWidthsMM / SetWorksheetRowHeightsMM)
  over a plain array standing in for the worksheet columns/rows.

  It verifies:
    * applying a width across a span updates every column in the span,
    * the function returns the count of changed entries,
    * non-positive values are rejected (count stays 0, no entry changed),
    * a single-cell span (start = end) updates exactly one entry.

  Build & run:
    fpc -Mobjfpc experiments/test_dimension_range.pas \
      && ./experiments/test_dimension_range
}
program test_dimension_range;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  TSizes = array of Double;

var
  Sheet: TSizes;

{ Mirrors SetWorksheetColWidthMM/SetWorksheetRowHeightMM: rejects
  non-positive sizes and out-of-range indices. }
function SetOne(AIndex: Integer; AValue: Double): Boolean;
begin
  Result := False;
  if (AIndex < 0) or (AIndex > High(Sheet)) or (AValue <= 0) then
    Exit;
  Sheet[AIndex] := AValue;
  Result := True;
end;

{ Mirrors SetWorksheetColWidthsMM/SetWorksheetRowHeightsMM. }
function SetRange(AStart, AEnd: Integer; AValue: Double): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := AStart to AEnd do
    if SetOne(i, AValue) then
      Inc(Result);
end;

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

begin
  WriteLn('Testing multi-column/row size logic (issue #1359)');

  // Span across several columns updates each one and reports the count.
  SetLength(Sheet, 5);
  Check(SetRange(1, 3, 42.0) = 3, 'span [1..3] reports 3 changed');
  Check(NearlyEqual(Sheet[1], 42.0) and NearlyEqual(Sheet[2], 42.0)
    and NearlyEqual(Sheet[3], 42.0), 'columns 1..3 set to 42');
  Check(NearlyEqual(Sheet[0], 0.0) and NearlyEqual(Sheet[4], 0.0),
    'columns outside span untouched');

  // Single-cell span updates exactly one entry.
  Check(SetRange(0, 0, 15.0) = 1, 'single-cell span reports 1 changed');
  Check(NearlyEqual(Sheet[0], 15.0), 'column 0 set to 15');

  // Non-positive value rejected: nothing changed.
  Check(SetRange(1, 3, 0.0) = 0, 'zero value -> 0 changed');
  Check(SetRange(1, 3, -7.0) = 0, 'negative value -> 0 changed');
  Check(NearlyEqual(Sheet[1], 42.0), 'rejected value leaves column intact');

  WriteLn;
  if Failures = 0 then
    WriteLn('ALL TESTS PASSED')
  else
  begin
    WriteLn(Failures, ' TEST(S) FAILED');
    Halt(1);
  end;
end.
