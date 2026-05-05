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

  { Stage 3 (TZ §12.3) regression tests for the visual-property reference
    queue. These cover the four failure modes the resolver must recognise:
    null ref, missing handle, kind mismatch, and successful resolution
    (including the layer-declared-after-entity case the spec calls out by
    name). Tests use opaque pointers so they exercise the resolver
    independently of any ZCAD entity. }
  TDWGLoadContextRefTest = class(TTestCase)
  published
    procedure LayerDeclaredAfterEntityResolvesAtEnd;
    procedure NullLayerHandleFallsBackToSystemLayer;
    procedure MissingLayerHandleFallsBackToSystemLayer;
    procedure LineTypeKindMismatchFallsBackToByLayer;
    procedure ResolveRefsIsIdempotent;
    procedure SecondQueueForSameSlotReplacesFirst;
    procedure RefAttachCallbackReceivesSlotAndReason;
    procedure NilPtrShellIsTreatedAsNotFound;
  end;

  { Stage 4 (TZ §12.4) regression tests for block / model-space / paper-space
    ownership routing at the load-context layer. The high-level production
    behaviour (BlockDefArray.create, model/paper recognition by Dwg_Data
    pointers) lives in uzefflibredwg2ents.pas and depends on ZCAD entity
    units, so these tests focus on the resolver contract that the production
    code relies on: dokModelSpace / dokPaperSpace / dokBlockDef are valid
    container kinds for ownership, child entities of each route to the right
    pointer (block content does NOT land in the model root), and a block
    handle registered after its child still resolves correctly. }
  TDWGLoadContextBlockTest = class(TTestCase)
  published
    procedure EntityInsideBlockDoesNotReachModelRoot;
    procedure ModelSpaceShellAcceptsChildEntities;
    procedure PaperSpaceShellAcceptsChildEntities;
    procedure DuplicateBlockHandleKeepsFirstShell;
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

{ ---------- TDWGLoadContextRefTest (Stage 3) ---------- }

type
  TFakeRefAttachCall = record
    Entity: Pointer;
    Ref: Pointer;
    Slot: TDWGZCADRefSlot;
    Reason: TDWGAttachReason;
  end;

  PFakeRefRecorder = ^TFakeRefRecorder;
  TFakeRefRecorder = record
    Calls: array of TFakeRefAttachCall;
  end;

procedure FakeRefAttach(Entity: Pointer; Ref: Pointer;
  Slot: TDWGZCADRefSlot; Reason: TDWGAttachReason; Data: Pointer);
var
  Rec: PFakeRefRecorder;
  Call: TFakeRefAttachCall;
begin
  Rec := PFakeRefRecorder(Data);
  Call.Entity := Entity;
  Call.Ref := Ref;
  Call.Slot := Slot;
  Call.Reason := Reason;
  SetLength(Rec^.Calls, Length(Rec^.Calls) + 1);
  Rec^.Calls[High(Rec^.Calls)] := Call;
end;

procedure TDWGLoadContextRefTest.LayerDeclaredAfterEntityResolvesAtEnd;
var
  Ctx: TDWGZCADLoadContext;
  Pending: PDWGZCADPendingRef;
  Entity, Layer, SysLayer: Pointer;
begin
  // TZ §13: "layer_declared_later" — entity references a layer handle whose
  // shell is registered AFTER QueueRefResolve runs. ResolveRefs at end of
  // load must still attach to the real layer, not the fallback.
  Ctx := TDWGZCADLoadContext.Create;
  try
    Entity := MakePtr($E1);
    Layer := MakePtr($A1);
    SysLayer := MakePtr($5A);
    Ctx.SetFallbackLayer(SysLayer);
    Ctx.RegisterShell($101, dokEntity, Entity, 0);
    Ctx.QueueRefResolve(Entity, $101, $202, dokLayer, rsLayer, nil);
    // Layer arrives later in the file — register after the entity has queued.
    Ctx.RegisterShell($202, dokLayer, Layer, 1);
    Ctx.ResolveRefs;
    Pending := Ctx.FindPendingRef($101, rsLayer);
    AssertNotNull('pending ref recorded', Pending);
    AssertEquals(Ord(asAttached), Ord(Pending^.AttachState));
    AssertEquals(Ord(arResolved), Ord(Pending^.AttachReason));
    AssertEquals(PtrInt(Layer), PtrInt(Pending^.AttachedRef));
    AssertEquals(1, Ctx.RefAttachCount);
    AssertEquals(0, Ctx.RefFallbackCount);
  finally
    Ctx.Free;
  end;
end;

procedure TDWGLoadContextRefTest.NullLayerHandleFallsBackToSystemLayer;
var
  Ctx: TDWGZCADLoadContext;
  Pending: PDWGZCADPendingRef;
  Entity, SysLayer: Pointer;
begin
  // §12.3 fallback policy: ref handle 0 (the DWG entity has no layer ref) must
  // route to the registered system-layer fallback, not nil.
  Ctx := TDWGZCADLoadContext.Create;
  try
    Entity := MakePtr($E2);
    SysLayer := MakePtr($5A);
    Ctx.SetFallbackLayer(SysLayer);
    Ctx.RegisterShell($102, dokEntity, Entity, 0);
    Ctx.QueueRefResolve(Entity, $102, 0, dokLayer, rsLayer, nil);
    Ctx.ResolveRefs;
    Pending := Ctx.FindPendingRef($102, rsLayer);
    AssertNotNull(Pending);
    AssertEquals(Ord(asFallback), Ord(Pending^.AttachState));
    AssertEquals(Ord(arRefNull), Ord(Pending^.AttachReason));
    AssertEquals(PtrInt(SysLayer), PtrInt(Pending^.AttachedRef));
    AssertEquals(1, Ctx.RefFallbackCount);
  finally
    Ctx.Free;
  end;
end;

procedure TDWGLoadContextRefTest.MissingLayerHandleFallsBackToSystemLayer;
var
  Ctx: TDWGZCADLoadContext;
  Pending: PDWGZCADPendingRef;
  Entity, SysLayer: Pointer;
begin
  // §12.3: a non-zero handle that never gets registered (broken file) must
  // fall back the same way as a null handle, but with a different reason
  // code so the diagnostic logger can flag the corruption.
  Ctx := TDWGZCADLoadContext.Create;
  try
    Entity := MakePtr($E3);
    SysLayer := MakePtr($5A);
    Ctx.SetFallbackLayer(SysLayer);
    Ctx.RegisterShell($103, dokEntity, Entity, 0);
    Ctx.QueueRefResolve(Entity, $103, $DEAD, dokLayer, rsLayer, nil);
    Ctx.ResolveRefs;
    Pending := Ctx.FindPendingRef($103, rsLayer);
    AssertNotNull(Pending);
    AssertEquals(Ord(asFallback), Ord(Pending^.AttachState));
    AssertEquals(Ord(arRefNotFound), Ord(Pending^.AttachReason));
    AssertEquals(PtrInt(SysLayer), PtrInt(Pending^.AttachedRef));
    AssertTrue('warning recorded for missing ref',
      Ctx.WarningCount >= 1);
  finally
    Ctx.Free;
  end;
end;

procedure TDWGLoadContextRefTest.LineTypeKindMismatchFallsBackToByLayer;
var
  Ctx: TDWGZCADLoadContext;
  Pending: PDWGZCADPendingRef;
  Entity, Layer, ByLayer: Pointer;
begin
  // §12.3: if the resolver finds the handle but it points to the wrong
  // ZCAD table (e.g. a layer pointer where a linetype was expected) the
  // entity must NOT receive the wrong-typed pointer; it falls back to
  // the slot-specific default and a kind-mismatch warning is emitted.
  Ctx := TDWGZCADLoadContext.Create;
  try
    Entity := MakePtr($E4);
    Layer := MakePtr($A1);
    ByLayer := MakePtr($BB);
    Ctx.SetFallbackLineType(ByLayer);
    Ctx.RegisterShell($104, dokEntity, Entity, 0);
    Ctx.RegisterShell($204, dokLayer, Layer, 1);
    Ctx.QueueRefResolve(Entity, $104, $204, dokLineType, rsLineType, nil);
    Ctx.ResolveRefs;
    Pending := Ctx.FindPendingRef($104, rsLineType);
    AssertNotNull(Pending);
    AssertEquals(Ord(asFallback), Ord(Pending^.AttachState));
    AssertEquals(Ord(arRefKindMismatch), Ord(Pending^.AttachReason));
    AssertEquals(PtrInt(ByLayer), PtrInt(Pending^.AttachedRef));
  finally
    Ctx.Free;
  end;
end;

procedure TDWGLoadContextRefTest.ResolveRefsIsIdempotent;
var
  Ctx: TDWGZCADLoadContext;
  Pending: PDWGZCADPendingRef;
  Entity, Layer: Pointer;
  AttachCountBefore: Integer;
begin
  // §5.4 idempotency: calling ResolveRefs twice must not double-attach a
  // pending ref. The state is checked at top of ResolveRef and returned
  // immediately when already attached.
  Ctx := TDWGZCADLoadContext.Create;
  try
    Entity := MakePtr($E5);
    Layer := MakePtr($A2);
    Ctx.RegisterShell($105, dokEntity, Entity, 0);
    Ctx.RegisterShell($205, dokLayer, Layer, 1);
    Ctx.QueueRefResolve(Entity, $105, $205, dokLayer, rsLayer, nil);
    Ctx.ResolveRefs;
    AttachCountBefore := Ctx.RefAttachCount;
    Ctx.ResolveRefs;
    AssertEquals('second ResolveRefs must be a no-op',
      AttachCountBefore, Ctx.RefAttachCount);
    Pending := Ctx.FindPendingRef($105, rsLayer);
    AssertEquals(Ord(asAttached), Ord(Pending^.AttachState));
  finally
    Ctx.Free;
  end;
end;

procedure TDWGLoadContextRefTest.SecondQueueForSameSlotReplacesFirst;
var
  Ctx: TDWGZCADLoadContext;
  Pending: PDWGZCADPendingRef;
  Entity, LayerA, LayerB: Pointer;
begin
  // §5.4: a mapper called twice for the same entity should not produce two
  // attached layers. AppendOrReplace overwrites the previous queue entry
  // for the same (entity, slot) key so the resolver only fires once and
  // the latest mapper call wins.
  Ctx := TDWGZCADLoadContext.Create;
  try
    Entity := MakePtr($E6);
    LayerA := MakePtr($AA);
    LayerB := MakePtr($BB);
    Ctx.RegisterShell($106, dokEntity, Entity, 0);
    Ctx.RegisterShell($206, dokLayer, LayerA, 1);
    Ctx.RegisterShell($207, dokLayer, LayerB, 2);
    Ctx.QueueRefResolve(Entity, $106, $206, dokLayer, rsLayer, nil);
    Ctx.QueueRefResolve(Entity, $106, $207, dokLayer, rsLayer, nil);
    AssertEquals('exactly one queue entry per (entity, slot)',
      1, Ctx.PendingRefs.Count);
    Ctx.ResolveRefs;
    Pending := Ctx.FindPendingRef($106, rsLayer);
    AssertEquals(PtrInt(LayerB), PtrInt(Pending^.AttachedRef));
  finally
    Ctx.Free;
  end;
end;

procedure TDWGLoadContextRefTest.RefAttachCallbackReceivesSlotAndReason;
var
  Ctx: TDWGZCADLoadContext;
  Recorder: TFakeRefRecorder;
  Entity, Layer: Pointer;
begin
  // §12.3: the ref-attach callback must be invoked once per pending ref
  // with both the resolved Slot and the Reason so production code can
  // route the pointer into the right vp field and emit the right log.
  Ctx := TDWGZCADLoadContext.Create;
  try
    Entity := MakePtr($E7);
    Layer := MakePtr($A3);
    SetLength(Recorder.Calls, 0);
    Ctx.SetRefAttachProc(@FakeRefAttach, @Recorder);
    Ctx.RegisterShell($107, dokEntity, Entity, 0);
    Ctx.RegisterShell($208, dokLayer, Layer, 1);
    Ctx.QueueRefResolve(Entity, $107, $208, dokLayer, rsLayer, nil);
    Ctx.ResolveRefs;
    AssertEquals(1, Length(Recorder.Calls));
    AssertEquals(Ord(rsLayer), Ord(Recorder.Calls[0].Slot));
    AssertEquals(Ord(arResolved), Ord(Recorder.Calls[0].Reason));
    AssertEquals(PtrInt(Layer), PtrInt(Recorder.Calls[0].Ref));
    AssertEquals(PtrInt(Entity), PtrInt(Recorder.Calls[0].Entity));
  finally
    Ctx.Free;
  end;
end;

procedure TDWGLoadContextRefTest.NilPtrShellIsTreatedAsNotFound;
var
  Ctx: TDWGZCADLoadContext;
  Pending: PDWGZCADPendingRef;
  Entity, ByLayer: Pointer;
begin
  // The LTYPE mapper may register a shell with a nil pointer when the table
  // is full and the entry could not be created. The resolver must treat
  // that as "not found" instead of attaching nil to vp.LineType.
  Ctx := TDWGZCADLoadContext.Create;
  try
    Entity := MakePtr($E8);
    ByLayer := MakePtr($BC);
    Ctx.SetFallbackLineType(ByLayer);
    Ctx.RegisterShell($108, dokEntity, Entity, 0);
    Ctx.RegisterShell($209, dokLineType, nil, 1);
    Ctx.QueueRefResolve(Entity, $108, $209, dokLineType, rsLineType, nil);
    Ctx.ResolveRefs;
    Pending := Ctx.FindPendingRef($108, rsLineType);
    AssertNotNull(Pending);
    AssertEquals(Ord(asFallback), Ord(Pending^.AttachState));
    AssertEquals(Ord(arRefNotFound), Ord(Pending^.AttachReason));
    AssertEquals(PtrInt(ByLayer), PtrInt(Pending^.AttachedRef));
  finally
    Ctx.Free;
  end;
end;

{ ---------- TDWGLoadContextBlockTest (Stage 4) ---------- }

procedure TDWGLoadContextBlockTest.EntityInsideBlockDoesNotReachModelRoot;
var
  Ctx: TDWGZCADLoadContext;
  Recorder: TFakeRecorder;
  Pending: PDWGZCADPendingOwner;
  Block, Line, Root: Pointer;
  RootCalls, I: Integer;
begin
  // §12.4 acceptance: "entity внутри block не попадает в model root". The
  // resolver must route the child to the registered block-def pointer even
  // though the model-space root is also a valid fallback.
  Ctx := TDWGZCADLoadContext.Create;
  try
    Block := MakePtr($BB04);
    Line := MakePtr($EE04);
    Root := MakePtr($FF04);
    SetLength(Recorder.Calls, 0);
    Ctx.SetFallbackOwner(Root);
    Ctx.SetAttachProc(@FakeAttach, @Recorder);

    // Model-space registered under handle 0 mirrors BeginDWGImport.
    Ctx.RegisterShell(0, dokModelSpace, Root, -1);
    // BLOCK_HEADER -> dokBlockDef pointer (real production path).
    Ctx.RegisterShell($300, dokBlockDef, Block, 0);
    // Entity owner-handle points to the block, not the model root.
    Ctx.RegisterShell($301, dokEntity, Line, 1);
    Ctx.QueueOwnerResolve(Line, $301, $300);

    Ctx.ResolveOwners;

    Pending := Ctx.FindPending($301);
    AssertNotNull(Pending);
    AssertEquals('child attaches to block, not fallback', Ord(asAttached),
      Ord(Pending^.AttachState));
    AssertEquals('owner pointer is the block-def',
      PtrInt(Block), PtrInt(Pending^.AttachedOwner));
    AssertEquals('no fallback occurred', 0, Ctx.FallbackCount);

    RootCalls := 0;
    for I := 0 to High(Recorder.Calls) do
      if PtrUInt(Recorder.Calls[I].Owner) = PtrUInt(Root) then
        Inc(RootCalls);
    AssertEquals('block content must not be added to model root', 0,
      RootCalls);
  finally
    Ctx.Free;
  end;
end;

procedure TDWGLoadContextBlockTest.ModelSpaceShellAcceptsChildEntities;
var
  Ctx: TDWGZCADLoadContext;
  Recorder: TFakeRecorder;
  Pending: PDWGZCADPendingOwner;
  ModelRoot, Line: Pointer;
begin
  // §12.4: a BLOCK_HEADER recognised as model space registers under
  // dokModelSpace pointing at the drawing's pObjRoot. A child entity whose
  // owner-handle points at that handle must resolve to the same pointer.
  Ctx := TDWGZCADLoadContext.Create;
  try
    ModelRoot := MakePtr($F501);
    Line := MakePtr($E501);
    Ctx.SetFallbackOwner(ModelRoot);
    Ctx.SetAttachProc(@FakeAttach, @Recorder);

    // Real DWG model-space handle is non-zero (header-resolved); register it
    // alongside the handle-0 fallback that BeginDWGImport sets up.
    Ctx.RegisterShell(0, dokModelSpace, ModelRoot, -1);
    Ctx.RegisterShell($1F, dokModelSpace, ModelRoot, 0);
    Ctx.RegisterShell($401, dokEntity, Line, 1);
    Ctx.QueueOwnerResolve(Line, $401, $1F);

    Ctx.ResolveOwners;

    Pending := Ctx.FindPending($401);
    AssertNotNull(Pending);
    AssertEquals(Ord(asAttached), Ord(Pending^.AttachState));
    AssertEquals('owner is the model-space root', PtrInt(ModelRoot),
      PtrInt(Pending^.AttachedOwner));
  finally
    Ctx.Free;
  end;
end;

procedure TDWGLoadContextBlockTest.PaperSpaceShellAcceptsChildEntities;
var
  Ctx: TDWGZCADLoadContext;
  Recorder: TFakeRecorder;
  Pending: PDWGZCADPendingOwner;
  PaperRoot, Line: Pointer;
begin
  // §12.4: paper space is recognised as a container kind (dokPaperSpace) so
  // entities owned by it resolve cleanly even though Stage 4 still routes
  // their pointer to the same drawing root.
  Ctx := TDWGZCADLoadContext.Create;
  try
    PaperRoot := MakePtr($F502);
    Line := MakePtr($E502);
    Ctx.SetFallbackOwner(PaperRoot);
    Ctx.SetAttachProc(@FakeAttach, @Recorder);

    Ctx.RegisterShell($2F, dokPaperSpace, PaperRoot, 0);
    Ctx.RegisterShell($402, dokEntity, Line, 1);
    Ctx.QueueOwnerResolve(Line, $402, $2F);

    Ctx.ResolveOwners;

    Pending := Ctx.FindPending($402);
    AssertNotNull(Pending);
    AssertEquals('paper-space owner accepted as container',
      Ord(asAttached), Ord(Pending^.AttachState));
    AssertEquals(PtrInt(PaperRoot), PtrInt(Pending^.AttachedOwner));
  finally
    Ctx.Free;
  end;
end;

procedure TDWGLoadContextBlockTest.DuplicateBlockHandleKeepsFirstShell;
var
  Ctx: TDWGZCADLoadContext;
  Entry: TDWGZCADHandleEntry;
  BlockA, BlockB: Pointer;
  I: Integer;
  HasDupWarning: Boolean;
begin
  // §12.4: "повторный block name обрабатывается как merge/duplicate по
  // LoadMode". The actual MergeItem semantics live in the ZCAD-side
  // BlockDefArray check; here we verify the load-context contract that
  // backs it: a second RegisterShell on the same handle keeps the first
  // pointer and emits a duplicate-handle warning, so the loader can
  // detect the merge case and keep child resolves pointing at the
  // already-loaded block-def.
  Ctx := TDWGZCADLoadContext.Create;
  try
    BlockA := MakePtr($BB10);
    BlockB := MakePtr($BB11);

    AssertTrue('first registration succeeds',
      Ctx.RegisterShell($500, dokBlockDef, BlockA, 0));
    AssertFalse('duplicate handle is rejected',
      Ctx.RegisterShell($500, dokBlockDef, BlockB, 1));

    AssertTrue(Ctx.TryGetEntry($500, Entry));
    AssertEquals('first block-def pointer wins',
      PtrInt(BlockA), PtrInt(Entry.Ptr));

    HasDupWarning := False;
    for I := 0 to Ctx.WarningCount - 1 do
      if Ctx.WarningAt(I).Code = DWG_WARN_DUPLICATE_HANDLE then
        HasDupWarning := True;
    AssertTrue('duplicate-handle warning recorded', HasDupWarning);
  finally
    Ctx.Free;
  end;
end;

initialization
  RegisterTests([TDWGLoadContextHandleMapTest, TDWGLoadContextResolveTest,
    TDWGLoadContextRefTest, TDWGLoadContextBlockTest]);

end.
