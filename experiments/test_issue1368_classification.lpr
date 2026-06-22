program test_issue1368_classification;
{$mode objfpc}{$H+}

{ Self-contained verification of the issue #1368 cell-colour logic. Replicates
  the exact TsColor constants and unanimity rule from uzvspreadsheet_cmdcellstyle
  WITHOUT any external dependency (fpspreadsheet/LCL/lazutils are unavailable in
  this headless environment). Proves two things:
    1) the HTML #RRGGBB colours map to the right TsColor ($00BBGGRR) values;
    2) the row classification follows the "all cells unanimous" rule. }

uses
  SysUtils;

type
  TsColor = LongWord;            // matches fpsTypes.TsColor encoding $00BBGGRR
  TZVCellStyleKind = (zvskData, zvskHeader, zvskTitle);

const
  scTransparent = $20000000;     // fpsTypes: no fill
  CZVTitleBgColor:  TsColor = $00F3E2D9;  // #D9E2F3
  CZVHeaderBgColor: TsColor = $00E6E6E7;  // #E7E6E6
  CZVDataBgColor:   TsColor = $00FFFFFF;  // #FFFFFF

var
  failures: Integer = 0;

{ Converts an HTML #RRGGBB triple to TsColor ($00BBGGRR, little-endian RGB). }
function HtmlToTsColor(r, g, b: Byte): TsColor;
begin
  Result := TsColor(r) or (TsColor(g) shl 8) or (TsColor(b) shl 16);
end;

function KindName(k: TZVCellStyleKind): string;
begin
  case k of
    zvskTitle: Result := 'Title';
    zvskHeader: Result := 'Header';
    else Result := 'Data';
  end;
end;

function ClassifyCellBackgroundColor(AColor: TsColor): TZVCellStyleKind;
begin
  if AColor = CZVTitleBgColor then Result := zvskTitle
  else if AColor = CZVHeaderBgColor then Result := zvskHeader
  else Result := zvskData;
end;

{ Unanimity rule over a row of colours. }
function DetectRowStyleKind(const row: array of TsColor): TZVCellStyleKind;
var
  i: Integer;
  kind, firstKind: TZVCellStyleKind;
begin
  Result := zvskData;
  if Length(row) = 0 then Exit;
  firstKind := zvskData;
  for i := 0 to High(row) do
  begin
    kind := ClassifyCellBackgroundColor(row[i]);
    if i = 0 then firstKind := kind
    else if kind <> firstKind then begin Result := zvskData; Exit; end;
  end;
  Result := firstKind;
end;

procedure CheckColor(const aName: string; got, expected: TsColor);
begin
  if got = expected then
    WriteLn('  PASS ', aName, ' = $', IntToHex(got, 8))
  else
  begin
    WriteLn('  FAIL ', aName, ' got $', IntToHex(got, 8), ' expected $', IntToHex(expected, 8));
    Inc(failures);
  end;
end;

procedure Check(const aName: string; got, expected: TZVCellStyleKind);
begin
  if got = expected then
    WriteLn('  PASS ', aName, ' = ', KindName(got))
  else
  begin
    WriteLn('  FAIL ', aName, ' got ', KindName(got), ' expected ', KindName(expected));
    Inc(failures);
  end;
end;

begin
  WriteLn('HTML -> TsColor conversion:');
  CheckColor('Title  #D9E2F3', HtmlToTsColor($D9, $E2, $F3), CZVTitleBgColor);
  CheckColor('Header #E7E6E6', HtmlToTsColor($E7, $E6, $E6), CZVHeaderBgColor);
  CheckColor('Data   #FFFFFF', HtmlToTsColor($FF, $FF, $FF), CZVDataBgColor);

  WriteLn('Cell classification:');
  Check('title colour', ClassifyCellBackgroundColor(CZVTitleBgColor), zvskTitle);
  Check('header colour', ClassifyCellBackgroundColor(CZVHeaderBgColor), zvskHeader);
  Check('white colour', ClassifyCellBackgroundColor(CZVDataBgColor), zvskData);
  Check('transparent', ClassifyCellBackgroundColor(scTransparent), zvskData);

  WriteLn('Row classification (unanimity rule):');
  Check('all title', DetectRowStyleKind([CZVTitleBgColor, CZVTitleBgColor, CZVTitleBgColor]), zvskTitle);
  Check('all header', DetectRowStyleKind([CZVHeaderBgColor, CZVHeaderBgColor]), zvskHeader);
  Check('all data', DetectRowStyleKind([CZVDataBgColor, CZVDataBgColor]), zvskData);
  Check('mixed t+h', DetectRowStyleKind([CZVTitleBgColor, CZVHeaderBgColor]), zvskData);
  Check('title+transparent', DetectRowStyleKind([CZVTitleBgColor, scTransparent]), zvskData);
  Check('empty row', DetectRowStyleKind([]), zvskData);

  WriteLn;
  if failures = 0 then begin WriteLn('ALL TESTS PASSED'); Halt(0); end
  else begin WriteLn(failures, ' TEST(S) FAILED'); Halt(1); end;
end.
