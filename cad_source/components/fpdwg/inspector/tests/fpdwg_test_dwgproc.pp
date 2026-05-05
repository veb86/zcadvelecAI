unit fpdwg_test_dwgproc;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  fpcunit,
  testregistry;

type
  TFPDWGProcHandleTest = class(TTestCase)
  published
    procedure NilRefReturnsFalse;
    procedure EmptyRefReturnsFalse;
    procedure AbsoluteRefWinsOverHandleref;
    procedure HandlerefIsFallbackWhenAbsoluteRefIsZero;
    procedure ObjectHandleValueReadsRawHandle;
    procedure ObjectOwnerHandleEntityReadsOwnerRef;
    procedure ObjectOwnerHandleObjectReadsOwnerRef;
    procedure ObjectOwnerHandleNilTioReturnsFalse;
  end;

  TFPDWGProcLineTest = class(TTestCase)
  published
    procedure CopyLineEndpointsCopiesAllAxes;
    procedure CopyLineEndpointsKeepsStartZSeparateFromStartX;
  end;

implementation

uses
  dwg,
  dwgproc;

procedure TFPDWGProcHandleTest.NilRefReturnsFalse;
var
  Value: QWord;
begin
  AssertFalse(DWGRefHandleValue(nil, Value));
  AssertEquals(Int64(0), Int64(Value));
end;

procedure TFPDWGProcHandleTest.EmptyRefReturnsFalse;
var
  RawRef: Dwg_Object_Ref;
  Value: QWord;
begin
  FillChar(RawRef, SizeOf(RawRef), 0);

  AssertFalse(DWGRefHandleValue(@RawRef, Value));
  AssertEquals(Int64(0), Int64(Value));
end;

procedure TFPDWGProcHandleTest.AbsoluteRefWinsOverHandleref;
var
  RawRef: Dwg_Object_Ref;
  Value: QWord;
begin
  FillChar(RawRef, SizeOf(RawRef), 0);
  RawRef.absolute_ref := $2A;
  RawRef.handleref.value := $33;

  AssertTrue(DWGRefHandleValue(@RawRef, Value));
  AssertEquals(Int64($2A), Int64(Value));
end;

procedure TFPDWGProcHandleTest.HandlerefIsFallbackWhenAbsoluteRefIsZero;
var
  RawRef: Dwg_Object_Ref;
  Value: QWord;
begin
  FillChar(RawRef, SizeOf(RawRef), 0);
  RawRef.absolute_ref := 0;
  RawRef.handleref.value := $33;

  AssertTrue(DWGRefHandleValue(@RawRef, Value));
  AssertEquals(Int64($33), Int64(Value));
end;

procedure TFPDWGProcHandleTest.ObjectHandleValueReadsRawHandle;
var
  Obj: Dwg_Object;
begin
  FillChar(Obj, SizeOf(Obj), 0);
  Obj.handle.value := $ABCDEF;

  AssertEquals(Int64($ABCDEF), Int64(DWGObjectHandleValue(Obj)));
end;

procedure TFPDWGProcHandleTest.ObjectOwnerHandleEntityReadsOwnerRef;
var
  Obj: Dwg_Object;
  Ent: Dwg_Object_Entity;
  OwnerRef: Dwg_Object_Ref;
  Value: QWord;
begin
  FillChar(Obj, SizeOf(Obj), 0);
  FillChar(Ent, SizeOf(Ent), 0);
  FillChar(OwnerRef, SizeOf(OwnerRef), 0);
  OwnerRef.absolute_ref := $40;
  Ent.ownerhandle := @OwnerRef;
  Obj.supertype := DWG_SUPERTYPE_ENTITY;
  Obj.tio.entity := @Ent;

  AssertTrue(DWGObjectOwnerHandleValue(Obj, Value));
  AssertEquals(Int64($40), Int64(Value));
end;

procedure TFPDWGProcHandleTest.ObjectOwnerHandleObjectReadsOwnerRef;
var
  Obj: Dwg_Object;
  Inner: Dwg_Object_Object;
  OwnerRef: Dwg_Object_Ref;
  Value: QWord;
begin
  FillChar(Obj, SizeOf(Obj), 0);
  FillChar(Inner, SizeOf(Inner), 0);
  FillChar(OwnerRef, SizeOf(OwnerRef), 0);
  OwnerRef.absolute_ref := 0;
  OwnerRef.handleref.value := $51;
  Inner.ownerhandle := @OwnerRef;
  Obj.supertype := DWG_SUPERTYPE_OBJECT;
  Obj.tio.&object := @Inner;

  AssertTrue(DWGObjectOwnerHandleValue(Obj, Value));
  AssertEquals(Int64($51), Int64(Value));
end;

procedure TFPDWGProcHandleTest.ObjectOwnerHandleNilTioReturnsFalse;
var
  Obj: Dwg_Object;
  Value: QWord;
begin
  FillChar(Obj, SizeOf(Obj), 0);
  Obj.supertype := DWG_SUPERTYPE_ENTITY;
  Obj.tio.entity := nil;

  AssertFalse(DWGObjectOwnerHandleValue(Obj, Value));
  AssertEquals(Int64(0), Int64(Value));
end;

procedure TFPDWGProcLineTest.CopyLineEndpointsCopiesAllAxes;
var
  Line: Dwg_Entity_LINE;
  Endpoints: TDWGLineEndpoints;
begin
  FillChar(Line, SizeOf(Line), 0);
  Line.start.x := 1.0;
  Line.start.y := 2.0;
  Line.start.z := 3.0;
  Line.end_.x := 4.0;
  Line.end_.y := 5.0;
  Line.end_.z := 6.0;

  DWGCopyLineEndpoints(Line, Endpoints);

  AssertEquals('start.x', 1.0, Endpoints.StartX, 0.0);
  AssertEquals('start.y', 2.0, Endpoints.StartY, 0.0);
  AssertEquals('start.z', 3.0, Endpoints.StartZ, 0.0);
  AssertEquals('end.x', 4.0, Endpoints.EndX, 0.0);
  AssertEquals('end.y', 5.0, Endpoints.EndY, 0.0);
  AssertEquals('end.z', 6.0, Endpoints.EndZ, 0.0);
end;

procedure TFPDWGProcLineTest.CopyLineEndpointsKeepsStartZSeparateFromStartX;
var
  Line: Dwg_Entity_LINE;
  Endpoints: TDWGLineEndpoints;
begin
  // Regression: previously start.z and end_.z were copied from start.x/end_.x,
  // collapsing the line onto an angled plane. Verify Z is independent of X.
  FillChar(Line, SizeOf(Line), 0);
  Line.start.x := 10.0;
  Line.start.y := 0.0;
  Line.start.z := 99.0;
  Line.end_.x := 20.0;
  Line.end_.y := 0.0;
  Line.end_.z := 77.0;

  DWGCopyLineEndpoints(Line, Endpoints);

  AssertEquals('start.x preserved', 10.0, Endpoints.StartX, 0.0);
  AssertEquals('start.z must come from raw .z, not .x', 99.0, Endpoints.StartZ, 0.0);
  AssertEquals('end.x preserved', 20.0, Endpoints.EndX, 0.0);
  AssertEquals('end.z must come from raw .z, not .x', 77.0, Endpoints.EndZ, 0.0);
end;

begin
  RegisterTests([TFPDWGProcHandleTest, TFPDWGProcLineTest]);
end.
