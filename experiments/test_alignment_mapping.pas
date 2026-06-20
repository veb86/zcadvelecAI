{ Standalone test for the spreadsheet cell alignment combobox mapping logic
  (issue #1352).

  fpspreadsheet is not installed in this environment, so this program
  replicates the enum order from fpsTypes:
      TsHorAlignment  = (haDefault, haLeft, haCenter, haRight);
      TsVertAlignment = (vaDefault, vaTop, vaCenter, vaBottom);
  and the exact mapping logic implemented in
  cad_source/zcad/velec/uzvspreadsheet/uzvspreadsheet_cmdalignment.pas.

  It verifies:
    * each of the 9 combobox indices maps to the expected (hor, vert) pair,
    * AlignmentsToIndex is the inverse of AlignmentIndexToAlignments,
    * "default" alignments are treated as left/top,
    * out-of-range indices are rejected.

  Build & run:
    fpc -Mobjfpc experiments/test_alignment_mapping.pas && ./experiments/test_alignment_mapping
}
program test_alignment_mapping;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  // Mirror of fpsTypes enums (same declaration order!)
  TsHorAlignment = (haDefault, haLeft, haCenter, haRight);
  TsVertAlignment = (vaDefault, vaTop, vaCenter, vaBottom);

const
  ALIGNMENT_OPTION_COUNT = 9;

{ ---- logic copied verbatim from uzvspreadsheet_cmdalignment.pas ---- }

function HorToGroup(aHor: TsHorAlignment): Integer;
begin
  case aHor of
    haCenter: Result := 1;
    haRight:  Result := 2;
    else      Result := 0;  // haLeft, haDefault
  end;
end;

function VertToGroup(aVert: TsVertAlignment): Integer;
begin
  case aVert of
    vaCenter: Result := 1;
    vaBottom: Result := 2;
    else      Result := 0;  // vaTop, vaDefault
  end;
end;

function AlignmentIndexToAlignments(aIndex: Integer;
  out aHor: TsHorAlignment; out aVert: TsVertAlignment): Boolean;
const
  HorByGroup: array[0..2] of TsHorAlignment = (haLeft, haCenter, haRight);
  VertByGroup: array[0..2] of TsVertAlignment = (vaTop, vaCenter, vaBottom);
begin
  aHor := haLeft;
  aVert := vaTop;
  Result := (aIndex >= 0) and (aIndex < ALIGNMENT_OPTION_COUNT);
  if not Result then
    Exit;
  aHor := HorByGroup[aIndex mod 3];
  aVert := VertByGroup[aIndex div 3];
end;

function AlignmentsToIndex(aHor: TsHorAlignment;
  aVert: TsVertAlignment): Integer;
begin
  Result := VertToGroup(aVert) * 3 + HorToGroup(aHor);
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

procedure ExpectMapping(aIndex: Integer; aHor: TsHorAlignment;
  aVert: TsVertAlignment; const aName: string);
var
  hor: TsHorAlignment;
  vert: TsVertAlignment;
begin
  Check(AlignmentIndexToAlignments(aIndex, hor, vert), aName + ': valid index');
  Check((hor = aHor) and (vert = aVert), aName + ': index -> alignments');
  Check(AlignmentsToIndex(aHor, aVert) = aIndex, aName + ': alignments -> index');
end;

var
  i: Integer;
  hor: TsHorAlignment;
  vert: TsVertAlignment;
begin
  WriteLn('Testing alignment mapping (issue #1352)');

  // Order required by the issue: left/center/right x top/center/bottom
  ExpectMapping(0, haLeft,   vaTop,    'left-top');
  ExpectMapping(1, haCenter, vaTop,    'center-top');
  ExpectMapping(2, haRight,  vaTop,    'right-top');
  ExpectMapping(3, haLeft,   vaCenter, 'left-center');
  ExpectMapping(4, haCenter, vaCenter, 'center-center');
  ExpectMapping(5, haRight,  vaCenter, 'right-center');
  ExpectMapping(6, haLeft,   vaBottom, 'left-bottom');
  ExpectMapping(7, haCenter, vaBottom, 'center-bottom');
  ExpectMapping(8, haRight,  vaBottom, 'right-bottom');

  // Round-trip for every valid index
  for i := 0 to ALIGNMENT_OPTION_COUNT - 1 do
  begin
    AlignmentIndexToAlignments(i, hor, vert);
    Check(AlignmentsToIndex(hor, vert) = i,
      Format('round-trip index %d', [i]));
  end;

  // Defaults are treated as left/top -> index 0
  Check(AlignmentsToIndex(haDefault, vaDefault) = 0,
    'haDefault/vaDefault -> index 0');
  Check(AlignmentsToIndex(haDefault, vaCenter) = 3,
    'haDefault/vaCenter -> index 3 (left-center)');
  Check(AlignmentsToIndex(haRight, vaDefault) = 2,
    'haRight/vaDefault -> index 2 (right-top)');

  // Out-of-range indices are rejected
  Check(not AlignmentIndexToAlignments(-1, hor, vert), 'reject index -1');
  Check(not AlignmentIndexToAlignments(ALIGNMENT_OPTION_COUNT, hor, vert),
    'reject index 9');

  WriteLn;
  if Failures = 0 then
    WriteLn('ALL TESTS PASSED')
  else
  begin
    WriteLn(Failures, ' TEST(S) FAILED');
    Halt(1);
  end;
end.
