unit uzctmtextwrap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, uzeentmtext, uzefontbase, uzefont;

type
  TMTextWrapTest = class(TTestCase)
  published
    procedure KeepsLongTokenOnOneLineOutsideTables;
    procedure SplitsLongTokenInsideTables;
  end;

implementation

procedure RunWrapTest(
  const AContent: string;
  AWrapMode: TMTextWrapMode;
  out ALineCount: Integer;
  out AFirstLine, ASecondLine: string);
var
  Font: GDBFont;
  Text: XYZWStringArray;
begin
  FillChar(Font, SizeOf(Font), 0);
  Font.font.IsUnicode := False;
  Text.init(4);
  try
    FormatMtext(@Font, 3, 1, 1, AContent, Text, AWrapMode);
    ALineCount := Text.Count;
    if ALineCount > 0 then
      AFirstLine := Text.getDataMutable(0)^.str
    else
      AFirstLine := '';
    if ALineCount > 1 then
      ASecondLine := Text.getDataMutable(1)^.str
    else
      ASecondLine := '';
  finally
    Text.done;
  end;
end;

procedure TMTextWrapTest.KeepsLongTokenOnOneLineOutsideTables;
var
  LineCount: Integer;
  FirstLine, SecondLine: string;
begin
  RunWrapTest('12345', mwmByWord, LineCount, FirstLine, SecondLine);
  AssertEquals('plain MTEXT should stay on one line without spaces', 1, LineCount);
  AssertEquals('12345', FirstLine);
  AssertEquals('', SecondLine);
end;

procedure TMTextWrapTest.SplitsLongTokenInsideTables;
var
  LineCount: Integer;
  FirstLine, SecondLine: string;
begin
  RunWrapTest('12345', mwmByWordThenChar, LineCount, FirstLine, SecondLine);
  AssertEquals('table MTEXT should split long token by chars', 2, LineCount);
  AssertEquals('123', FirstLine);
  AssertEquals('45', SecondLine);
end;

initialization
  RegisterTest(TMTextWrapTest);

end.
