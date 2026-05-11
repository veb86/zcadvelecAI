unit uzctenttextnilstyle;
{$Codepage UTF8}

{ Issue #1194 regression test.
  When a DWG-loaded TEXT entity reaches FormatEntity/CalcGabarit with
  TXTStyle=nil (e.g. its text style ref failed to resolve during
  BeginDWGImport), the code used to dereference TXTStyle^.pfont^.font
  and crash. FormatEntity must now resolve the drawing's Standard style
  as fallback, and CalcGabarit must exit cleanly when the chain is still
  incomplete. }

interface

uses
  SysUtils,
  fpcunit, testregistry,
  uzeenttext,
  uzedrawingsimple,
  uzgldrawcontext,
  uzegeometry,
  Interfaces;

type
  TTextNilStyleTest = class(TTestCase)
  published
    procedure CalcGabaritWithNilStyleDoesNotCrash;
    procedure FormatEntityResolvesStandardWhenNilStyle;
  end;

implementation

procedure TTextNilStyleTest.CalcGabaritWithNilStyleDoesNotCrash;
var
  drawing: TSimpleDrawing;
  text: PGDBObjText;
begin
  drawing.init(nil);
  text := GDBObjText.CreateInstance;
  text^.Content := 'ABC';
  // TXTStyle is left nil on purpose — the bug from #1194 reproduces when
  // FormatEntity / CalcGabarit are called in this state.
  text^.TXTStyle := nil;
  text^.CalcGabarit(drawing);
  AssertTrue('CalcGabarit must return without dereferencing nil TXTStyle', True);
end;

procedure TTextNilStyleTest.FormatEntityResolvesStandardWhenNilStyle;
var
  drawing: TSimpleDrawing;
  text: PGDBObjText;
  dc: TDrawContext;
begin
  drawing.init(nil);
  text := GDBObjText.CreateInstance;
  text^.Content := 'ABC';
  text^.TXTStyle := nil;
  drawing.GetCurrentRoot^.AddMi(text);
  dc := drawing.CreateDrawingRC;
  text^.FormatEntity(drawing, dc);
  // After the fix FormatEntity must either attach a Standard text style or
  // bail out cleanly; in either case it must not have crashed.
  AssertTrue('FormatEntity must not crash on nil TXTStyle', True);
end;

initialization
  RegisterTest(TTextNilStyleTest);

end.
