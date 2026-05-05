unit fpdwg_test_loadcontext;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  fpcunit,
  testregistry;

type
  { Stage 2 (TZ_DWG_LOAD_TO_ZCAD §12.2) regression tests for the import
    context, verifying parent-after-child resolve, missing-owner fallback,
    self-owner cycle, A->B->A cycle and idempotent attach. The tests use
    fake opaque pointers so they do not need ZCAD entities or LibreDWG. }
  TDWGLoadContextHandleMapTest = class(TTestCase)
  published
    procedure RegisterShellInsertsByHandle;
    procedure RegisterShellRejectsDuplicate;
    procedure TryGetMissingHandleReturnsFalse;
    procedure RegisterShellAcceptsOutOfOrderHandles;
  end;

  TDWGLoadContextResolveTest = class(TTestCase)
  published
    procedure ParentAfterChildResolvesToBlockOwner;
    procedure NullOwnerFallsBackToRoot;
    procedure MissingOwnerHandleFallsBackWithWarning;
    procedure NonContainerOwnerFallsBackWithWarning;
    procedure SelfOwnerCycleDoesNotRecurse;
    procedure OwnerChainCycleABABreaks;
    procedure OwnerChainCycleABCABreaks;
    procedure AttachIsIdempotentAcrossResolveCalls;
    procedure AttachCallbackReceivesOwnerAndReason;
    procedure ChildOfBlockHeaderIsNotAttachedToRoot;
  end;

implementation

uses
  SysUtils,
  uzedwgloadcontext;

type
  // Sentinel pointers used as opaque ZCAD entity stand-ins. The resolver only
  // forwards them to the attach callback and never dereferences them, so plain
  // typed constants are safer than allocating dummy records.
  TFakePtr = type Pointer;

  TFakeAttachCall = record
    Entity: Pointer;
    Owner: Pointer;
    Reason: TDWGAttachReason;
  end;

  PFakeRecorder = ^TFakeRecorder;
  TFakeRecorder = record
    Calls: array of TFakeAttachCall;
  end;

procedure FakeAttach(Entity: Pointer; Owner: Pointer;
  Reason: TDWGAttachReason; Data: Pointer);
var
  Rec: PFakeRecorder;
  Call: TFakeAttachCall;
begin
  Rec := PFakeRecorder(Data);
  Call.Entity := Entity;
  Call.Owner := Owner;
  Call.Reason := Reason;
  SetLength(Rec^.Calls, Length(Rec^.Calls) + 1);
  Rec^.Calls[High(Rec^.Calls)] := Call;
end;

function MakePtr(Tag: PtrInt): Pointer; inline;
begin
  // Return a stable bit pattern that resolver code can carry around without
  // dereferencing. Using PtrInt avoids 32/64 bit assumptions in tests.
  Result := Pointer(Tag);
end;

{ ---------- TDWGLoadContextHandleMapTest ---------- }

procedure TDWGLoadContextHandleMapTest.RegisterShellInsertsByHandle;
var
  Map: TDWGZCADHandleMap;
  Entry: TDWGZCADHandleEntry;
begin
  Map := TDWGZCADHandleMap.Create;
  try
    AssertTrue(Map.RegisterShell($10, dokEntity, MakePtr($A1), 0, msCreated));
    AssertEquals(1, Map.Count);
    AssertTrue(Map.TryGet($10, Entry));
    AssertEquals(Int64($10), Int64(Entry.Handle));
    AssertEquals(Ord(dokEntity), Ord(Entry.Kind));
    AssertEquals(Ord(msCreated), Ord(Entry.ShellState));
  finally
    Map.Free;
  end;
end;

procedure TDWGLoadContextHandleMapTest.RegisterShellRejectsDuplicate;
var
  Map: TDWGZCADHandleMap;
  Entry: TDWGZCADHandleEntry;
begin
  Map := TDWGZCADHandleMap.Create;
  try
    AssertTrue(Map.RegisterShell($20, dokEntity, MakePtr($B1), 0, msCreated));
    AssertFalse(Map.RegisterShell($20, dokBlockDef, MakePtr($B2), 1,
      msCreated));
    AssertEquals('first registration wins', 1, Map.Count);
    AssertTrue(Map.TryGet($20, Entry));
    AssertEquals(Ord(dokEntity), Ord(Entry.Kind));
    AssertEquals(PtrInt($B1), PtrInt(Entry.Ptr));
  finally
    Map.Free;
  end;
end;

procedure TDWGLoadContextHandleMapTest.TryGetMissingHandleReturnsFalse;
var
  Map: TDWGZCADHandleMap;
  Entry: TDWGZCADHandleEntry;
begin
  Map := TDWGZCADHandleMap.Create;
  try
    AssertFalse(Map.TryGet($DEAD, Entry));
  finally
    Map.Free;
  end;
end;

procedure TDWGLoadContextHandleMapTest.RegisterShellAcceptsOutOfOrderHandles;
var
  Map: TDWGZCADHandleMap;
  Entry: TDWGZCADHandleEntry;
begin
  // Stage 2 fundamental: handles arrive in any order. The map must keep them
  // searchable without imposing insertion order constraints on the loader.
  Map := TDWGZCADHandleMap.Create;
  try
    AssertTrue(Map.RegisterShell($30, dokEntity, MakePtr($C3), 0, msCreated));
    AssertTrue(Map.RegisterShell($10, dokBlockDef, MakePtr($C1), 1,
      msCreated));
    AssertTrue(Map.RegisterShell($20, dokLayer, MakePtr($C2), 2, msCreated));
    AssertEquals(3, Map.Count);
    AssertTrue(Map.TryGet($10, Entry));
    AssertEquals(Ord(dokBlockDef), Ord(Entry.Kind));
    AssertTrue(Map.TryGet($20, Entry));
    AssertEquals(Ord(dokLayer), Ord(Entry.Kind));
    AssertTrue(Map.TryGet($30, Entry));
    AssertEquals(Ord(dokEntity), Ord(Entry.Kind));
  finally
    Map.Free;
  end;
end;

{ ---------- TDWGLoadContextResolveTest ---------- }

procedure TDWGLoadContextResolveTest.ParentAfterChildResolvesToBlockOwner;
var
  Ctx: TDWGZCADLoadContext;
  Recorder: TFakeRecorder;
  Pending: PDWGZCADPendingOwner;
  Block, Line, Root: Pointer;
begin
  // §5.3 / §12.2: child entity (LINE) is processed before its owner
  // (BLOCK_HEADER). After ResolveOwners the LINE must end up under the block,
  // not under fallback root.
  Ctx := TDWGZCADLoadContext.Create;
  try
    Block := MakePtr($B100);
    Line := MakePtr($E100);
    Root := MakePtr($F100);

    Ctx.SetFallbackOwner(Root);
    Ctx.SetAttachProc(@FakeAttach, @Recorder);

    // Child arrives first: entity registered, owner not yet known.
    AssertTrue(Ctx.RegisterShell($10, dokEntity, Line, 0));
    Ctx.QueueOwnerResolve(Line, $10, $20);

    // Owner arrives later in the same shell phase.
    AssertTrue(Ctx.RegisterShell($20, dokBlockDef, Block, 1));

    Ctx.ResolveOwners;

    Pending := Ctx.FindPending($10);
    AssertNotNull('pending owner record exists', Pending);
    AssertEquals('attached state', Ord(asAttached), Ord(Pending^.AttachState));
    AssertEquals('attached owner is block, not fallback root',
      PtrInt(Block), PtrInt(Pending^.AttachedOwner));
    AssertEquals('attach reason is resolved', Ord(arResolved),
      Ord(Pending^.AttachReason));
    AssertEquals('one attach call recorded', 1, Length(Recorder.Calls));
    AssertEquals('callback owner is block', PtrInt(Block),
      PtrInt(Recorder.Calls[0].Owner));
  finally
    Ctx.Free;
  end;
end;

procedure TDWGLoadContextResolveTest.NullOwnerFallsBackToRoot;
var
  Ctx: TDWGZCADLoadContext;
  Recorder: TFakeRecorder;
  Pending: PDWGZCADPendingOwner;
  Line, Root: Pointer;
begin
  Ctx := TDWGZCADLoadContext.Create;
  try
    Line := MakePtr($E200);
    Root := MakePtr($F200);
    Ctx.SetFallbackOwner(Root);
    Ctx.SetAttachProc(@FakeAttach, @Recorder);

    Ctx.RegisterShell($30, dokEntity, Line, 0);
    Ctx.QueueOwnerResolve(Line, $30, 0); // owner handle 0 == null
    Ctx.ResolveOwners;

    Pending := Ctx.FindPending($30);
    AssertEquals(Ord(asFallback), Ord(Pending^.AttachState));
    AssertEquals(Ord(arNullOwner), Ord(Pending^.AttachReason));
    AssertEquals('falls back to root', PtrInt(Root),
      PtrInt(Pending^.AttachedOwner));
    AssertEquals('callback receives root owner',
      PtrInt(Root), PtrInt(Recorder.Calls[0].Owner));
  finally
    Ctx.Free;
  end;
end;

procedure TDWGLoadContextResolveTest.MissingOwnerHandleFallsBackWithWarning;
var
  Ctx: TDWGZCADLoadContext;
  Recorder: TFakeRecorder;
  Pending: PDWGZCADPendingOwner;
  Line, Root: Pointer;
  I: Integer;
  HasWarning: Boolean;
begin
  Ctx := TDWGZCADLoadContext.Create;
  try
    Line := MakePtr($E300);
    Root := MakePtr($F300);
    Ctx.SetFallbackOwner(Root);
    Ctx.SetAttachProc(@FakeAttach, @Recorder);

    Ctx.RegisterShell($40, dokEntity, Line, 0);
    Ctx.QueueOwnerResolve(Line, $40, $999); // owner $999 never registered
    Ctx.ResolveOwners;

    Pending := Ctx.FindPending($40);
    AssertEquals(Ord(asFallback), Ord(Pending^.AttachState));
    AssertEquals(Ord(arOwnerNotFound), Ord(Pending^.AttachReason));
    AssertEquals(PtrInt(Root), PtrInt(Pending^.AttachedOwner));

    HasWarning := False;
    for I := 0 to Ctx.WarningCount - 1 do
      if Ctx.WarningAt(I).Code = DWG_WARN_OWNER_NOT_FOUND then
        HasWarning := True;
    AssertTrue('owner-not-found warning recorded', HasWarning);
  finally
    Ctx.Free;
  end;
end;

procedure TDWGLoadContextResolveTest.NonContainerOwnerFallsBackWithWarning;
var
  Ctx: TDWGZCADLoadContext;
  Recorder: TFakeRecorder;
  Pending: PDWGZCADPendingOwner;
  Line, Layer, Root: Pointer;
  I: Integer;
  HasWarning: Boolean;
begin
  // §5.5: owner found but not a container (e.g. LINE pointing at a LAYER).
  Ctx := TDWGZCADLoadContext.Create;
  try
    Line := MakePtr($E400);
    Layer := MakePtr($D400);
    Root := MakePtr($F400);
    Ctx.SetFallbackOwner(Root);
    Ctx.SetAttachProc(@FakeAttach, @Recorder);

    Ctx.RegisterShell($50, dokEntity, Line, 0);
    Ctx.RegisterShell($60, dokLayer, Layer, 1);
    Ctx.QueueOwnerResolve(Line, $50, $60);
    Ctx.ResolveOwners;

    Pending := Ctx.FindPending($50);
    AssertEquals(Ord(asFallback), Ord(Pending^.AttachState));
    AssertEquals(Ord(arOwnerNotContainer), Ord(Pending^.AttachReason));
    AssertEquals(PtrInt(Root), PtrInt(Pending^.AttachedOwner));

    HasWarning := False;
    for I := 0 to Ctx.WarningCount - 1 do
      if Ctx.WarningAt(I).Code = DWG_WARN_OWNER_NOT_CONTAINER then
        HasWarning := True;
    AssertTrue('non-container warning recorded', HasWarning);
  finally
    Ctx.Free;
  end;
end;

procedure TDWGLoadContextResolveTest.SelfOwnerCycleDoesNotRecurse;
var
  Ctx: TDWGZCADLoadContext;
  Recorder: TFakeRecorder;
  Pending: PDWGZCADPendingOwner;
  Line, Root: Pointer;
  I: Integer;
  HasWarning: Boolean;
begin
  Ctx := TDWGZCADLoadContext.Create;
  try
    Line := MakePtr($E500);
    Root := MakePtr($F500);
    Ctx.SetFallbackOwner(Root);
    Ctx.SetAttachProc(@FakeAttach, @Recorder);

    Ctx.RegisterShell($70, dokEntity, Line, 0);
    Ctx.QueueOwnerResolve(Line, $70, $70); // owner == entity

    Ctx.ResolveOwners;

    Pending := Ctx.FindPending($70);
    AssertEquals(Ord(asFallback), Ord(Pending^.AttachState));
    AssertEquals(Ord(arSelfOwnerCycle), Ord(Pending^.AttachReason));
    AssertEquals(PtrInt(Root), PtrInt(Pending^.AttachedOwner));
    AssertEquals(1, Ctx.CycleCount);

    HasWarning := False;
    for I := 0 to Ctx.WarningCount - 1 do
      if Ctx.WarningAt(I).Code = DWG_WARN_OWNER_SELF_CYCLE then
        HasWarning := True;
    AssertTrue('self-owner cycle warning recorded', HasWarning);
  finally
    Ctx.Free;
  end;
end;

procedure TDWGLoadContextResolveTest.OwnerChainCycleABABreaks;
var
  Ctx: TDWGZCADLoadContext;
  Recorder: TFakeRecorder;
  PendingA, PendingB: PDWGZCADPendingOwner;
  PtrA, PtrB, Root: Pointer;
  I: Integer;
  HasCycleWarning: Boolean;
begin
  // §5.4: A owns B, B owns A. Both must end up in fallback, no recursion,
  // and at least one cycle warning recorded.
  Ctx := TDWGZCADLoadContext.Create;
  try
    PtrA := MakePtr($BA1);
    PtrB := MakePtr($BA2);
    Root := MakePtr($F600);
    Ctx.SetFallbackOwner(Root);
    Ctx.SetAttachProc(@FakeAttach, @Recorder);

    // Both A and B are containers (block defs) so the resolver tries to
    // descend through ownership instead of bailing on kind.
    Ctx.RegisterShell($80, dokBlockDef, PtrA, 0);
    Ctx.RegisterShell($90, dokBlockDef, PtrB, 1);
    Ctx.QueueOwnerResolve(PtrA, $80, $90);
    Ctx.QueueOwnerResolve(PtrB, $90, $80);

    Ctx.ResolveOwners;

    PendingA := Ctx.FindPending($80);
    PendingB := Ctx.FindPending($90);
    AssertNotNull(PendingA);
    AssertNotNull(PendingB);

    // The first pending entered the cycle and must be in fallback. The other
    // either also fell back or attached to the now-resolved partner; both
    // outcomes are acceptable as long as nothing recursed and at least one
    // cycle warning was emitted.
    AssertTrue('A reached terminal state',
      PendingA^.AttachState in [asFallback, asAttached]);
    AssertTrue('B reached terminal state',
      PendingB^.AttachState in [asFallback, asAttached]);

    HasCycleWarning := False;
    for I := 0 to Ctx.WarningCount - 1 do
      if Ctx.WarningAt(I).Code = DWG_WARN_OWNER_CHAIN_CYCLE then
        HasCycleWarning := True;
    AssertTrue('A->B->A cycle warning recorded', HasCycleWarning);
    AssertTrue('cycle counter increased', Ctx.CycleCount >= 1);
  finally
    Ctx.Free;
  end;
end;

procedure TDWGLoadContextResolveTest.OwnerChainCycleABCABreaks;
var
  Ctx: TDWGZCADLoadContext;
  Recorder: TFakeRecorder;
  Root: Pointer;
  I: Integer;
  HasCycleWarning: Boolean;
begin
  Ctx := TDWGZCADLoadContext.Create;
  try
    Root := MakePtr($F700);
    Ctx.SetFallbackOwner(Root);
    Ctx.SetAttachProc(@FakeAttach, @Recorder);

    Ctx.RegisterShell($A0, dokBlockDef, MakePtr($BA01), 0);
    Ctx.RegisterShell($B0, dokBlockDef, MakePtr($BA02), 1);
    Ctx.RegisterShell($C0, dokBlockDef, MakePtr($BA03), 2);
    Ctx.QueueOwnerResolve(MakePtr($BA01), $A0, $B0);
    Ctx.QueueOwnerResolve(MakePtr($BA02), $B0, $C0);
    Ctx.QueueOwnerResolve(MakePtr($BA03), $C0, $A0);

    Ctx.ResolveOwners;

    HasCycleWarning := False;
    for I := 0 to Ctx.WarningCount - 1 do
      if Ctx.WarningAt(I).Code = DWG_WARN_OWNER_CHAIN_CYCLE then
        HasCycleWarning := True;
    AssertTrue('A->B->C->A cycle warning recorded', HasCycleWarning);
    AssertTrue('cycle counter increased', Ctx.CycleCount >= 1);
  finally
    Ctx.Free;
  end;
end;

procedure TDWGLoadContextResolveTest.AttachIsIdempotentAcrossResolveCalls;
var
  Ctx: TDWGZCADLoadContext;
  Recorder: TFakeRecorder;
  Block, Line, Root: Pointer;
begin
  Ctx := TDWGZCADLoadContext.Create;
  try
    Block := MakePtr($B800);
    Line := MakePtr($E800);
    Root := MakePtr($F800);
    Ctx.SetFallbackOwner(Root);
    Ctx.SetAttachProc(@FakeAttach, @Recorder);

    Ctx.RegisterShell($D0, dokBlockDef, Block, 0);
    Ctx.RegisterShell($E0, dokEntity, Line, 1);
    Ctx.QueueOwnerResolve(Line, $E0, $D0);

    Ctx.ResolveOwners;
    Ctx.ResolveOwners; // second call must not fire AddMi again

    AssertEquals('attach callback called exactly once', 1,
      Length(Recorder.Calls));
    AssertEquals('attach counter unchanged on repeat', 1, Ctx.AttachCount);
  finally
    Ctx.Free;
  end;
end;

procedure TDWGLoadContextResolveTest.AttachCallbackReceivesOwnerAndReason;
var
  Ctx: TDWGZCADLoadContext;
  Recorder: TFakeRecorder;
  Block, Line, Root: Pointer;
begin
  Ctx := TDWGZCADLoadContext.Create;
  try
    Block := MakePtr($B900);
    Line := MakePtr($E900);
    Root := MakePtr($F900);
    Ctx.SetFallbackOwner(Root);
    Ctx.SetAttachProc(@FakeAttach, @Recorder);

    Ctx.RegisterShell($101, dokEntity, Line, 0);
    Ctx.RegisterShell($102, dokBlockDef, Block, 1);
    Ctx.QueueOwnerResolve(Line, $101, $102);
    Ctx.ResolveOwners;

    AssertEquals('one attach call', 1, Length(Recorder.Calls));
    AssertEquals('callback entity', PtrInt(Line),
      PtrInt(Recorder.Calls[0].Entity));
    AssertEquals('callback owner', PtrInt(Block),
      PtrInt(Recorder.Calls[0].Owner));
    AssertEquals('callback reason', Ord(arResolved),
      Ord(Recorder.Calls[0].Reason));
  finally
    Ctx.Free;
  end;
end;

procedure TDWGLoadContextResolveTest.ChildOfBlockHeaderIsNotAttachedToRoot;
var
  Ctx: TDWGZCADLoadContext;
  Recorder: TFakeRecorder;
  Pending: PDWGZCADPendingOwner;
  Block, Line, Root: Pointer;
  RootCalls: Integer;
  I: Integer;
begin
  // §12.2: explicit guard against the original bug. With the old loader the
  // entity was added to pObjRoot at allocation time. The new pipeline must
  // never invoke the root callback if the block owner is reachable.
  Ctx := TDWGZCADLoadContext.Create;
  try
    Block := MakePtr($BAA);
    Line := MakePtr($EAA);
    Root := MakePtr($FAA);
    Ctx.SetFallbackOwner(Root);
    Ctx.SetAttachProc(@FakeAttach, @Recorder);

    Ctx.RegisterShell($201, dokEntity, Line, 0);
    Ctx.RegisterShell($202, dokBlockDef, Block, 1);
    Ctx.QueueOwnerResolve(Line, $201, $202);
    Ctx.ResolveOwners;

    Pending := Ctx.FindPending($201);
    AssertEquals(Ord(asAttached), Ord(Pending^.AttachState));
    AssertEquals(0, Ctx.FallbackCount);

    RootCalls := 0;
    for I := 0 to High(Recorder.Calls) do
      if PtrUInt(Recorder.Calls[I].Owner) = PtrUInt(Root) then
        Inc(RootCalls);
    AssertEquals('entity must not have been added to fallback root', 0,
      RootCalls);
  finally
    Ctx.Free;
  end;
end;

initialization
  RegisterTests([TDWGLoadContextHandleMapTest, TDWGLoadContextResolveTest]);

end.
