program test_handle_normalize_fix;
{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

{ Standalone reproduction for issue #1213. Covers two pieces:
  - DWGNormalizeObjectHandlesFromRefs is widen-only and does not overwrite
    a full Dwg_Object handle with a narrower absolute_ref.
  - DWGRefHandleCandidatesValue puts Ref^.obj^.handle.value first, matching
    fpdwginspect's HandleRefFromBitCode (issue #1213 follow-up — was the
    actual root cause of the wraparound when ref-driven lookups dropped the
    canonical full handle in favour of a 16-bit absolute_ref). Compiles
    without libredwg.so. }

uses
  SysUtils,
  dwg,
  uzedwghandle;

var
  Failures: Integer = 0;

procedure Check(const Msg: string; Expected, Actual: QWord);
begin
  if Expected = Actual then
    WriteLn('PASS: ', Msg, ' = $', IntToHex(Actual, 1))
  else begin
    WriteLn('FAIL: ', Msg, ' expected $', IntToHex(Expected, 1),
      ' but got $', IntToHex(Actual, 1));
    Inc(Failures);
  end;
end;

procedure RunExistingTest;
var
  Raw: Dwg_Data;
  Objects: array[0..1] of Dwg_Object;
  Refs: array[0..1] of Dwg_Object_Ref;
  RefPtrs: array[0..1] of PDwg_Object_Ref;
begin
  FillChar(Raw, SizeOf(Raw), 0);
  FillChar(Objects, SizeOf(Objects), 0);
  FillChar(Refs, SizeOf(Refs), 0);

  Raw.num_objects := Length(Objects);
  Raw.&object := @Objects[0];
  Raw.num_object_refs := Length(RefPtrs);
  Raw.object_ref := @RefPtrs[0];

  Objects[0].index := 0;
  Objects[0].parent := @Raw;
  Objects[0].handle.value := $325E;
  Objects[1].index := 1;
  Objects[1].parent := @Raw;
  Objects[1].handle.value := $325E;

  Refs[0].obj := @Objects[0];
  Refs[0].absolute_ref := $325E;
  Refs[1].obj := @Objects[1];
  Refs[1].absolute_ref := $A325E;
  RefPtrs[0] := @Refs[0];
  RefPtrs[1] := @Refs[1];

  DWGNormalizeObjectHandles(Raw);

  WriteLn('-- existing test: ObjectHandleNormalizeUsesObjectRefAbsoluteRefs');
  Check('obj0 stays at $325E', $325E, Objects[0].handle.value);
  Check('obj1 widens to $A325E from absolute_ref', $A325E, Objects[1].handle.value);
end;

procedure RunRegressionTest;
var
  Raw: Dwg_Data;
  Objects: array[0..1] of Dwg_Object;
  Refs: array[0..1] of Dwg_Object_Ref;
  RefPtrs: array[0..1] of PDwg_Object_Ref;
begin
  FillChar(Raw, SizeOf(Raw), 0);
  FillChar(Objects, SizeOf(Objects), 0);
  FillChar(Refs, SizeOf(Refs), 0);

  Raw.num_objects := Length(Objects);
  Raw.&object := @Objects[0];
  Raw.num_object_refs := Length(RefPtrs);
  Raw.object_ref := @RefPtrs[0];

  Objects[0].index := 0;
  Objects[0].parent := @Raw;
  Objects[0].handle.value := $A325E;
  Objects[1].index := 1;
  Objects[1].parent := @Raw;
  Objects[1].handle.value := $1FFFF;

  Refs[0].obj := @Objects[0];
  Refs[0].absolute_ref := $325E;
  Refs[1].obj := @Objects[1];
  Refs[1].absolute_ref := $FFFF;
  RefPtrs[0] := @Refs[0];
  RefPtrs[1] := @Refs[1];

  DWGNormalizeObjectHandles(Raw);

  WriteLn('-- regression: ObjectHandleNormalizePreservesFullHandleAgainstTruncatedRef');
  Check('full handle survives narrower absolute_ref', $A325E, Objects[0].handle.value);
  Check('full handle above 16 bits survives FFFF absolute_ref', $1FFFF, Objects[1].handle.value);
end;

procedure RunRefCandidatePriorityTest;
var
  Target: Dwg_Object;
  Ref: Dwg_Object_Ref;
  Candidates: TDWGRefHandleCandidates;
  Value: QWord;
begin
  FillChar(Target, SizeOf(Target), 0);
  FillChar(Ref, SizeOf(Ref), 0);

  // Reproduce the canonical-vs-truncated mismatch: the resolved object
  // already carries the full 64-bit handle while absolute_ref has been
  // squeezed into the low 16 bits.
  Target.handle.value := $A325E;
  Ref.obj := @Target;
  Ref.absolute_ref := $325E;
  Ref.handleref.value := $325E;

  WriteLn('-- regression: RefHandleCandidatesPreferResolvedObjectHandle');
  if not DWGRefHandleCandidatesValue(@Ref, Candidates) then begin
    WriteLn('FAIL: candidates not produced');
    Inc(Failures);
  end else begin
    Check('candidates[0] is the full handle from obj.handle.value',
      $A325E, Candidates.Values[0]);
    if Candidates.Count >= 2 then
      Check('candidates[1] keeps absolute_ref as fallback',
        $325E, Candidates.Values[1]);
  end;
  if DWGRefHandleValue(@Ref, Value) then
    Check('DWGRefHandleValue returns the full handle',
      $A325E, Value)
  else begin
    WriteLn('FAIL: DWGRefHandleValue returned False');
    Inc(Failures);
  end;
end;

begin
  RunExistingTest;
  RunRegressionTest;
  RunRefCandidatePriorityTest;
  WriteLn;
  if Failures = 0 then
    WriteLn('All handle normalization checks pass.')
  else begin
    WriteLn('FAILURES: ', Failures);
    Halt(1);
  end;
end.
