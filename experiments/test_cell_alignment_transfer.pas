{ Standalone test for the spreadsheet -> ACAD_TABLE cell-alignment linkage
  logic (issue #1363).

  fpspreadsheet is not installed in this environment, so this program
  replicates the enum order from fpsTypes:
      TsHorAlignment  = (haDefault, haLeft, haCenter, haRight);
      TsVertAlignment = (vaDefault, vaTop, vaCenter, vaBottom);
  and the pure conversion / collection logic implemented in
  cad_source/zcad/velec/uzvspreadsheet/uzvspreadsheet_dimensions.pas
      (WorksheetAlignmentToAcad / CollectCellAlignments)
  together with the AutoCAD-code -> hor/vert decoding in
  cad_source/zcad/velec/acadtable/uzeacadtable_styles.pas
      (DXFAlignToHorz / DXFAlignToVert).

  It verifies:
    * each fpspreadsheet (hor, vert) pair maps to the expected AutoCAD code
      (group 170, 1..9),
    * the AutoCAD code decodes back to the same hor/vert groups (round-trip),
    * "default" alignments are treated as left/top, matching the editor
      combobox convention (issue #1352),
    * cells without an explicit alignment collect to 0 (inherit from style).

  Build & run:
    fpc -Mobjfpc experiments/test_cell_alignment_transfer.pas \
      && ./experiments/test_cell_alignment_transfer
}
program test_cell_alignment_transfer;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  // Mirror of fpsTypes enums (same declaration order!)
  TsHorAlignment = (haDefault, haLeft, haCenter, haRight);
  TsVertAlignment = (vaDefault, vaTop, vaCenter, vaBottom);
  // Mirror of uzeacadtable_types
  THorzAlign = (zhaLeft, zhaCenter, zhaRight);
  TVertAlign = (zvaTop, zvaMiddle, zvaBottom);

{ ---- logic copied verbatim from uzvspreadsheet_dimensions.pas ---- }

function HorAlignToGroup(AHor: TsHorAlignment): Integer;
begin
  case AHor of
    haCenter: Result := 1;
    haRight:  Result := 2;
    else      Result := 0;  // haLeft, haDefault
  end;
end;

function VertAlignToGroup(AVert: TsVertAlignment): Integer;
begin
  case AVert of
    vaCenter: Result := 1;
    vaBottom: Result := 2;
    else      Result := 0;  // vaTop, vaDefault
  end;
end;

function WorksheetAlignmentToAcad(AHor: TsHorAlignment;
  AVert: TsVertAlignment): Integer;
begin
  Result := VertAlignToGroup(AVert) * 3 + HorAlignToGroup(AHor) + 1;
end;

{ Collects the AutoCAD code for one cell the way CollectCellAlignments does:
  0 (inherit) when both alignments are default, otherwise the encoded code. }
function CollectOne(AHor: TsHorAlignment; AVert: TsVertAlignment): Integer;
begin
  if (AHor <> haDefault) or (AVert <> vaDefault) then
    Result := WorksheetAlignmentToAcad(AHor, AVert)
  else
    Result := 0;
end;

{ ---- logic copied verbatim from uzeacadtable_styles.pas ---- }

function DXFAlignToHorz(Alignment: Integer): THorzAlign;
var
  HorzPart: Integer;
begin
  HorzPart := (Alignment - 1) mod 3;
  case HorzPart of
    1: Result := zhaCenter;
    2: Result := zhaRight;
  else
    Result := zhaLeft;
  end;
end;

function DXFAlignToVert(Alignment: Integer): TVertAlign;
var
  VertPart: Integer;
begin
  VertPart := (Alignment - 1) div 3;
  case VertPart of
    1: Result := zvaMiddle;
    2: Result := zvaBottom;
  else
    Result := zvaTop;
  end;
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

procedure ExpectCode(aHor: TsHorAlignment; aVert: TsVertAlignment;
  aCode: Integer; aHorz: THorzAlign; aVert2: TVertAlign; const aName: string);
begin
  Check(WorksheetAlignmentToAcad(aHor, aVert) = aCode,
    aName + ': hor/vert -> AutoCAD code ' + IntToStr(aCode));
  Check(DXFAlignToHorz(aCode) = aHorz, aName + ': code -> horizontal group');
  Check(DXFAlignToVert(aCode) = aVert2, aName + ': code -> vertical group');
end;

begin
  WriteLn('Testing cell alignment transfer (issue #1363)');

  // The 9 combinations, AutoCAD codes 1..9 (1=TopLeft .. 9=BottomRight)
  ExpectCode(haLeft,   vaTop,    1, zhaLeft,   zvaTop,    'left-top');
  ExpectCode(haCenter, vaTop,    2, zhaCenter, zvaTop,    'center-top');
  ExpectCode(haRight,  vaTop,    3, zhaRight,  zvaTop,    'right-top');
  ExpectCode(haLeft,   vaCenter, 4, zhaLeft,   zvaMiddle, 'left-center');
  ExpectCode(haCenter, vaCenter, 5, zhaCenter, zvaMiddle, 'center-center');
  ExpectCode(haRight,  vaCenter, 6, zhaRight,  zvaMiddle, 'right-center');
  ExpectCode(haLeft,   vaBottom, 7, zhaLeft,   zvaBottom, 'left-bottom');
  ExpectCode(haCenter, vaBottom, 8, zhaCenter, zvaBottom, 'center-bottom');
  ExpectCode(haRight,  vaBottom, 9, zhaRight,  zvaBottom, 'right-bottom');

  // Defaults are treated as left/top (matching editor combobox, issue #1352)
  Check(WorksheetAlignmentToAcad(haDefault, vaDefault) = 1,
    'haDefault/vaDefault -> AutoCAD code 1 (TopLeft)');
  Check(WorksheetAlignmentToAcad(haDefault, vaCenter) = 4,
    'haDefault/vaCenter -> AutoCAD code 4 (MiddleLeft)');
  Check(WorksheetAlignmentToAcad(haRight, vaDefault) = 3,
    'haRight/vaDefault -> AutoCAD code 3 (TopRight)');

  // Collection emits 0 (inherit) only when BOTH alignments are default
  Check(CollectOne(haDefault, vaDefault) = 0,
    'fully default cell -> 0 (inherit from style)');
  Check(CollectOne(haCenter, vaDefault) = 2,
    'explicit horizontal only -> code 2 (TopCenter)');
  Check(CollectOne(haDefault, vaBottom) = 7,
    'explicit vertical only -> code 7 (BottomLeft)');
  Check(CollectOne(haCenter, vaCenter) = 5,
    'explicit hor+vert -> code 5 (MiddleCenter)');

  WriteLn;
  if Failures = 0 then
    WriteLn('ALL TESTS PASSED')
  else
  begin
    WriteLn(Failures, ' TEST(S) FAILED');
    Halt(1);
  end;
end.
