unit fpdwg_test_validator;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  fpcunit,
  testregistry;

type
  TFPDWGValidatorTest = class(TTestCase)
  published
    procedure ValidateCollectsBrokenRefsAndOrphans;
    procedure ValidateDetectsOwnerCycles;
  end;

implementation

uses
  SysUtils,
  fpdwg_types,
  fpdwg_resolver,
  fpdwg_validator,
  fpdwg_document,
  fpdwg_model_base,
  fpdwg_model_entities;

function Ref(AHandle: TDWGHandle): TDWGHandleRef;
begin
  Result.Value := AHandle;
  Result.Source := hsAbsoluteRef;
end;

procedure TFPDWGValidatorTest.ValidateCollectsBrokenRefsAndOrphans;
var
  Doc: TDWGDocument;
  Resolver: TDWGResolver;
  Validator: TDWGValidator;
  ResultInfo: TDWGValidationResult;
  Line: TDWGLine;
  BrokenRef: TDWGBrokenReference;
begin
  Doc := TDWGDocument.Create(lmTolerant, nil);
  try
    Line := TDWGLine.Create;
    Line.Handle := $40;
    Line.LayerHandle := Ref($99);
    Doc.AddObject(Line);

    Resolver := TDWGResolver.Create(Doc.Registry, nil);
    try
      Resolver.ResolveAll;
    finally
      Resolver.Free;
    end;

    Validator := TDWGValidator.Create(Doc, nil);
    try
      ResultInfo := Validator.Validate;
      try
        AssertEquals(1, ResultInfo.BrokenRefCount);
        AssertEquals(1, ResultInfo.OrphanCount);
        AssertEquals(0, ResultInfo.CycleCount);

        BrokenRef := ResultInfo.BrokenRefAt(0);
        AssertEquals(Int64($40), Int64(BrokenRef.SourceHandle));
        AssertEquals(Int64($99), Int64(BrokenRef.TargetHandle));
        AssertEquals('layer', BrokenRef.RefName);
        AssertTrue(BrokenRef.Required);
        AssertTrue(ResultInfo.OrphanAt(0).Obj = Line);
      finally
        ResultInfo.Free;
      end;
    finally
      Validator.Free;
    end;
  finally
    Doc.Free;
  end;
end;

procedure TFPDWGValidatorTest.ValidateDetectsOwnerCycles;
var
  Doc: TDWGDocument;
  Resolver: TDWGResolver;
  Validator: TDWGValidator;
  ResultInfo: TDWGValidationResult;
  Obj: TDWGObject;
  Cycle: TDWGOwnerCycle;
begin
  Doc := TDWGDocument.Create(lmTolerant, nil);
  try
    Obj := TDWGObject.Create;
    Obj.Handle := $50;
    Obj.OwnerHandle := Ref($50);
    Doc.AddObject(Obj);

    Resolver := TDWGResolver.Create(Doc.Registry, nil);
    try
      Resolver.ResolveAll;
    finally
      Resolver.Free;
    end;

    Validator := TDWGValidator.Create(Doc, nil);
    try
      ResultInfo := Validator.Validate;
      try
        AssertEquals(0, ResultInfo.BrokenRefCount);
        AssertEquals(0, ResultInfo.OrphanCount);
        AssertEquals(1, ResultInfo.CycleCount);

        Cycle := ResultInfo.CycleAt(0);
        AssertEquals(Int64($50), Int64(Cycle.StartHandle));
        AssertEquals(Int64($50), Int64(Cycle.RepeatedHandle));
      finally
        ResultInfo.Free;
      end;
    finally
      Validator.Free;
    end;
  finally
    Doc.Free;
  end;
end;

begin
  RegisterTests([TFPDWGValidatorTest]);
end.
