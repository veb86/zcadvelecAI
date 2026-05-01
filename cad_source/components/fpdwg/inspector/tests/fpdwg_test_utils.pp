unit fpdwg_test_utils;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  fpcunit,
  testregistry;

type
  TFPDWGUtilsTest = class(TTestCase)
  published
    procedure SafeDecodeTextReturnsEmptyForNil;
    procedure SafeDecodeTextKeepsValidUTF8;
    procedure SafeDecodeTextConvertsCP1251ToUTF8;
    procedure SafeDecodeTextFallsBackToHexAndWarns;
    procedure VersionConversionFallsBackToFromVersion;
  end;

implementation

uses
  dwg,
  fpdwg_logger,
  fpdwg_libredwg_utils,
  fpdwg_types;

procedure TFPDWGUtilsTest.SafeDecodeTextReturnsEmptyForNil;
begin
  AssertEquals('', SafeDecodeText(nil, 65001, nil));
end;

procedure TFPDWGUtilsTest.SafeDecodeTextKeepsValidUTF8;
var
  Raw: AnsiString;
begin
  Raw := 'Walls';

  AssertEquals('Walls', SafeDecodeText(PAnsiChar(Raw), 65001, nil));
end;

procedure TFPDWGUtilsTest.SafeDecodeTextConvertsCP1251ToUTF8;
var
  Raw: AnsiString;
  Expected: string;
begin
  Raw := #$CF#$F0#$E8#$E2#$E5#$F2;
  Expected := #$D0#$9F#$D1#$80#$D0#$B8#$D0#$B2#$D0#$B5#$D1#$82;

  AssertEquals(Expected, SafeDecodeText(PAnsiChar(Raw), 1251, nil));
end;

procedure TFPDWGUtilsTest.SafeDecodeTextFallsBackToHexAndWarns;
var
  Raw: AnsiString;
  Logger: IDWGLogger;
  MemoryLogger: TDWGMemoryLogger;
begin
  Raw := #$C3#$28;
  MemoryLogger := TDWGMemoryLogger.Create;
  Logger := MemoryLogger;
  try
    AssertEquals('<hex:C3 28>', SafeDecodeText(PAnsiChar(Raw), 65001,
      Logger));
    AssertEquals(1, MemoryLogger.ErrorCount);
    AssertEquals(1400, MemoryLogger.GetError(0).Code);
  finally
    Logger := nil;
  end;
end;

procedure TFPDWGUtilsTest.VersionConversionFallsBackToFromVersion;
begin
  AssertEquals(Ord(dvR2007), Ord(DWGVersionFromLibre(R_INVALID, R_2007)));
end;

begin
  RegisterTests([TFPDWGUtilsTest]);
end.
