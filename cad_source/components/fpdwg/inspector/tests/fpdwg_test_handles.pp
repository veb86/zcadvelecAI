unit fpdwg_test_handles;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  fpcunit,
  testregistry;

type
  TFPDWGHandleRefTest = class(TTestCase)
  published
    procedure NilBitCodeRefReturnsNull;
    procedure EmptyBitCodeRefReturnsNull;
    procedure AbsoluteRefWinsOverHandleref;
    procedure HandlerefIsFallbackWhenAbsoluteRefIsZero;
    procedure DwgHandleValueReturnsRawHandleValue;
  end;

implementation

uses
  dwg,
  fpdwg_types,
  fpdwg_libredwg_utils;

procedure TFPDWGHandleRefTest.NilBitCodeRefReturnsNull;
var
  Ref: TDWGHandleRef;
begin
  Ref := HandleRefFromBitCode(nil);

  AssertEquals(Int64(0), Int64(Ref.Value));
  AssertEquals(Ord(hsNull), Ord(Ref.Source));
  AssertTrue(Ref.IsNull);
end;

procedure TFPDWGHandleRefTest.EmptyBitCodeRefReturnsNull;
var
  RawRef: Dwg_Object_Ref;
  Ref: TDWGHandleRef;
begin
  FillChar(RawRef, SizeOf(RawRef), 0);

  Ref := HandleRefFromBitCode(@RawRef);

  AssertEquals(Int64(0), Int64(Ref.Value));
  AssertEquals(Ord(hsNull), Ord(Ref.Source));
  AssertTrue(Ref.IsNull);
end;

procedure TFPDWGHandleRefTest.AbsoluteRefWinsOverHandleref;
var
  RawRef: Dwg_Object_Ref;
  Ref: TDWGHandleRef;
begin
  FillChar(RawRef, SizeOf(RawRef), 0);
  RawRef.absolute_ref := $2A;
  RawRef.handleref.value := $33;

  Ref := HandleRefFromBitCode(@RawRef);

  AssertEquals(Int64($2A), Int64(Ref.Value));
  AssertEquals(Ord(hsAbsoluteRef), Ord(Ref.Source));
  AssertEquals('2A', Ref.ToString);
end;

procedure TFPDWGHandleRefTest.HandlerefIsFallbackWhenAbsoluteRefIsZero;
var
  RawRef: Dwg_Object_Ref;
  Ref: TDWGHandleRef;
begin
  FillChar(RawRef, SizeOf(RawRef), 0);
  RawRef.absolute_ref := 0;
  RawRef.handleref.value := $33;

  Ref := HandleRefFromBitCode(@RawRef);

  AssertEquals(Int64($33), Int64(Ref.Value));
  AssertEquals(Ord(hsHandleref), Ord(Ref.Source));
  AssertEquals('33', Ref.ToString);
end;

procedure TFPDWGHandleRefTest.DwgHandleValueReturnsRawHandleValue;
var
  Handle: Dwg_Handle;
begin
  FillChar(Handle, SizeOf(Handle), 0);
  Handle.value := $ABCDEF;

  AssertEquals(Int64($ABCDEF), Int64(DWGHandleValue(Handle)));
end;

begin
  RegisterTests([TFPDWGHandleRefTest]);
end.
