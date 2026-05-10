unit uzctenttextjustify;

{$mode objfpc}{$H+}

interface

uses
  fpcunit,
  testregistry;

type
  TTextJustifyMappingTest = class(TTestCase)
  published
    procedure BaselineCenterMapsToCenterJustify;
  end;

implementation

uses
  uzeentabstracttext,
  uzeenttext;

procedure TTextJustifyMappingTest.BaselineCenterMapsToCenterJustify;
begin
  AssertEquals('TEXT horizontal Center with baseline vertical must import as Center',
    Ord(jsbtc), Ord(jt[0, 1]));
end;

initialization
  RegisterTest(TTextJustifyMappingTest);

end.
