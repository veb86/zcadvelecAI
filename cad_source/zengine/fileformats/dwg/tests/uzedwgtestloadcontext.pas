unit uzedwgtestloadcontext;

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
    // R4 (TZ §3.4): the raw scan registers (dokUnknown, nil) placeholders;
    // mappers running later must be allowed to upgrade them in place to
    // their real kind/ptr without tripping the duplicate-rejection branch.
    procedure RegisterShellUpgradesRawScanPlaceholder;
    procedure RegisterShellRejectsRealDuplicateAfterUpgrade;
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
    procedure AlternateOwnerHandleResolvesAfterPrimaryNonContainer;
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
    procedure AlternateLayerHandleResolvesAfterPrimaryKindMismatch;
    procedure ResolveRefsIsIdempotent;
    procedure SecondQueueForSameSlotReplacesFirst;
    procedure LayerLineTypeUsesLayerSlot;
    procedure NullLayerLineTypeFallsBackToContinuous;
    procedure InlineLineTypeAttachesWithoutFallbackWarning;
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

  { Stage 5 (TZ §12.5) regression tests for the textstyle-ref slot used by
    TEXT and MTEXT entities. The contract mirrors the rsLayer / rsLineType
    ref slots covered in Stage 3: a real handle resolves to its registered
    style; a null/missing handle drops onto the SetFallbackTextStyle pointer;
    a kind mismatch (e.g. handle pointing at a layer) likewise falls back.
    Tests use opaque pointers so they exercise the resolver alone, with no
    ZCAD entity dependency. }
  TDWGLoadContextTextStyleRefTest = class(TTestCase)
  published
    procedure StyleDeclaredAfterTextResolvesAtEnd;
    procedure NullStyleHandleFallsBackToStandard;
    procedure MissingStyleHandleFallsBackToStandard;
    procedure StyleKindMismatchFallsBackToStandard;
  end;

  { Stage 6 (issue #1091) resolver contracts for INSERT/ATTRIB/DIMENSION.
    Production mappers build on these queues: INSERT uses rsBlockDef, ATTRIB
    is owned by dokBlockInsert, and dimensions use rsDimStyle plus generated
    block refs. }
  TDWGLoadContextStage6Test = class(TTestCase)
  published
    procedure BlockDefDeclaredAfterInsertResolvesAtEnd;
    procedure BrokenBlockRefFallsBackWithWarning;
    procedure AttribOwnerInsertResolvesToInsertContainer;
    procedure DimStyleDeclaredAfterDimensionResolvesAtEnd;
    procedure NullDimStyleFallsBackToDefault;
    procedure BlockDefRefAcceptsModelSpaceForInsert;
  end;

  { Issue #1189 regression: per-entity fallback log lines emitted by
    DWGAttachEntity / DWGAttachRef must use the silent "{WH}" marker (history
    only), not "{WHM}" (history + modal message box). The user explicitly asked
    for fallback diagnostics to remain as silent log entries because the loader
    recovers correctly and a per-entity modal dialog interrupts file loading
    on drawings with many unresolved owner/ref handles. }
  TDWGLoadContextSilentFallbackTest = class(TTestCase)
  private
    function LocateImportSource: string;
  published
    procedure DWGAttachFallbackLogsUseSilentMarker;
    procedure DWGAttachReasonTextCoversAllReasons;
  end;

  { Issue #1198 P4 (per АНАЛИЗ_ЗАГРУЗЧИКА_DWG.md §P4) regression: warning
    list must aggregate per-code totals + first-sample + distinct-handle
    counts, and ShouldEmitDetail must throttle subsequent occurrences of
    the same (Code, Handle) so the main log only shows the first one. }
  TDWGLoadContextWarningAggregateTest = class(TTestCase)
  published
    procedure AddTracksTotalAndDistinctHandles;
    procedure FirstSampleIsCapturedAndPreserved;
    procedure ShouldEmitDetailFiresOnceThenSuppresses;
    procedure ShouldEmitDetailKeysOnCodeAndHandle;
    procedure DistinctHandlesIncrementsOnNewHandleOnly;
    procedure CodeForAttachReasonCoversFallbackReasons;
  end;

implementation

uses
  Classes,
  SysUtils,
  uzedwgtypes,
  uzedwgdiagnostics,
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

procedure TDWGLoadContextHandleMapTest.RegisterShellUpgradesRawScanPlaceholder;
var
  Map: TDWGZCADHandleMap;
  Entry: TDWGZCADHandleEntry;
begin
  // R4 (TZ §3.4): the raw scan seeds the map with (dokUnknown, nil, raw_index)
  // placeholders. When the entity mapper runs later it should upgrade the
  // entry in place to its real kind/ptr while keeping the captured raw_index.
  Map := TDWGZCADHandleMap.Create;
  try
    AssertTrue('placeholder accepted',
      Map.RegisterShell($40, dokUnknown, nil, 7, msCreated));
    AssertTrue('mapper upgrade accepted',
      Map.RegisterShell($40, dokEntity, MakePtr($D1), -1, msCreated));
    AssertEquals('upgrade does not duplicate the entry', 1, Map.Count);
    AssertTrue(Map.TryGet($40, Entry));
    AssertEquals('kind upgraded to dokEntity',
      Ord(dokEntity), Ord(Entry.Kind));
    AssertEquals('ptr upgraded to mapper-supplied value',
      PtrInt($D1), PtrInt(Entry.Ptr));
    AssertEquals('raw index preserved from the raw scan', 7, Entry.RawIndex);
  finally
    Map.Free;
  end;
end;

procedure TDWGLoadContextHandleMapTest.RegisterShellRejectsRealDuplicateAfterUpgrade;
var
  Map: TDWGZCADHandleMap;
  Entry: TDWGZCADHandleEntry;
begin
  // After the placeholder has been upgraded by a mapper, a second mapper
  // hitting the same handle is a real duplicate and must be rejected.
  Map := TDWGZCADHandleMap.Create;
  try
    Map.RegisterShell($50, dokUnknown, nil, 3, msCreated);
    AssertTrue(Map.RegisterShell($50, dokEntity, MakePtr($E1), -1, msCreated));
    AssertFalse('second mapper-side registration rejected',
      Map.RegisterShell($50, dokBlockDef, MakePtr($E2), -1, msCreated));
    AssertEquals(1, Map.Count);
    AssertTrue(Map.TryGet($50, Entry));
    AssertEquals('first mapper kind wins',
      Ord(dokEntity), Ord(Entry.Kind));
    AssertEquals('first mapper ptr wins',
      PtrInt($E1), PtrInt(Entry.Ptr));
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

procedure TDWGLoadContextResolveTest.AlternateOwnerHandleResolvesAfterPrimaryNonContainer;
var
  Ctx: TDWGZCADLoadContext;
  Pending: PDWGZCADPendingOwner;
  Line, Layer, Block, Root: Pointer;
  Handles: array[0..1] of TDWGZCADHandle;
begin
  // Issue #1189: the first decoded owner handle can point at a non-container
  // shell while a scalar fallback handle names the real block/container.
  Ctx := TDWGZCADLoadContext.Create;
  try
    Line := MakePtr($E401);
    Layer := MakePtr($D401);
    Block := MakePtr($B401);
    Root := MakePtr($F401);
    Handles[0] := $61;
    Handles[1] := $62;
    Ctx.SetFallbackOwner(Root);
    Ctx.RegisterShell($51, dokEntity, Line, 0);
    Ctx.RegisterShell($61, dokLayer, Layer, 1);
    Ctx.RegisterShell($62, dokBlockDef, Block, 2);
    Ctx.QueueOwnerResolveCandidates(Line, $51, Handles, 2);
    Ctx.ResolveOwners;

    Pending := Ctx.FindPending($51);
    AssertNotNull(Pending);
    AssertEquals(Ord(asAttached), Ord(Pending^.AttachState));
    AssertEquals(Ord(arResolved), Ord(Pending^.AttachReason));
    AssertEquals(Int64($62), Int64(Pending^.OwnerHandle));
    AssertEquals(PtrInt(Block), PtrInt(Pending^.AttachedOwner));
    AssertEquals(1, Ctx.AttachCount);
    AssertEquals(0, Ctx.FallbackCount);
    AssertEquals(0, Ctx.WarningCount);
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

procedure TDWGLoadContextRefTest.AlternateLayerHandleResolvesAfterPrimaryKindMismatch;
var
  Ctx: TDWGZCADLoadContext;
  Pending: PDWGZCADPendingRef;
  Entity, WrongLineType, Layer, SysLayer: Pointer;
  Handles: array[0..1] of TDWGZCADHandle;
begin
  // Issue #1189: recovered DWGs can expose both a resolved object handle and
  // scalar handles for one BITCODE_H. The first decoded handle may point at a
  // shell of the wrong ZCAD kind; the resolver must try the scalar fallback
  // before warning and assigning the system fallback.
  Ctx := TDWGZCADLoadContext.Create;
  try
    Entity := MakePtr($E40);
    WrongLineType := MakePtr($A40);
    Layer := MakePtr($A41);
    SysLayer := MakePtr($5A);
    Handles[0] := $100B;
    Handles[1] := $9D67;
    Ctx.SetFallbackLayer(SysLayer);
    Ctx.RegisterShell($1040, dokEntity, Entity, 0);
    Ctx.RegisterShell($100B, dokLineType, WrongLineType, 1);
    Ctx.RegisterShell($9D67, dokLayer, Layer, 2);
    Ctx.QueueRefResolveCandidates(Entity, $1040, Handles, 2,
      dokLayer, rsLayer, nil);
    Ctx.ResolveRefs;
    Pending := Ctx.FindPendingRef($1040, rsLayer);
    AssertNotNull(Pending);
    AssertEquals(Ord(asAttached), Ord(Pending^.AttachState));
    AssertEquals(Ord(arResolved), Ord(Pending^.AttachReason));
    AssertEquals(Int64($9D67), Int64(Pending^.RefHandle));
    AssertEquals(PtrInt(Layer), PtrInt(Pending^.AttachedRef));
    AssertEquals(1, Ctx.RefAttachCount);
    AssertEquals(0, Ctx.RefFallbackCount);
    AssertEquals(0, Ctx.WarningCount);
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

procedure TDWGLoadContextRefTest.LayerLineTypeUsesLayerSlot;
var
  Ctx: TDWGZCADLoadContext;
  Pending: PDWGZCADPendingRef;
  Layer, Continuous: Pointer;
begin
  // Issue #1122: a LAYER table entry has its own linetype ref. It targets the
  // PGDBLayerProp.LT field and must not reuse the entity rsLineType slot, whose
  // production callback writes through PGDBObjEntity.vp.LineType.
  Ctx := TDWGZCADLoadContext.Create;
  try
    Layer := MakePtr($AA70);
    Continuous := MakePtr($CC70);
    Ctx.RegisterShell($170, dokLayer, Layer, 0);
    Ctx.RegisterShell($270, dokLineType, Continuous, 1);
    Ctx.QueueRefResolve(Layer, $170, $270, dokLineType, rsLayerLineType, nil);
    Ctx.ResolveRefs;

    Pending := Ctx.FindPendingRef($170, rsLayerLineType);
    AssertNotNull('layer linetype ref recorded under layer slot', Pending);
    AssertEquals(Ord(asAttached), Ord(Pending^.AttachState));
    AssertEquals(Ord(arResolved), Ord(Pending^.AttachReason));
    AssertEquals(PtrInt(Continuous), PtrInt(Pending^.AttachedRef));
    AssertTrue('entity linetype slot remains unused for the layer handle',
      Ctx.FindPendingRef($170, rsLineType) = nil);
  finally
    Ctx.Free;
  end;
end;

procedure TDWGLoadContextRefTest.NullLayerLineTypeFallsBackToContinuous;
var
  Ctx: TDWGZCADLoadContext;
  Pending: PDWGZCADPendingRef;
  Layer, Continuous: Pointer;
begin
  // A broken/missing layer linetype should leave the layer drawable. The DWG
  // mapper passes Continuous explicitly for this slot because ByLayer is an
  // entity-level fallback and is not a useful layer.LT value.
  Ctx := TDWGZCADLoadContext.Create;
  try
    Layer := MakePtr($AA71);
    Continuous := MakePtr($CC71);
    Ctx.RegisterShell($171, dokLayer, Layer, 0);
    Ctx.QueueRefResolve(Layer, $171, 0, dokLineType, rsLayerLineType,
      Continuous);
    Ctx.ResolveRefs;

    Pending := Ctx.FindPendingRef($171, rsLayerLineType);
    AssertNotNull(Pending);
    AssertEquals(Ord(asFallback), Ord(Pending^.AttachState));
    AssertEquals(Ord(arRefNull), Ord(Pending^.AttachReason));
    AssertEquals(PtrInt(Continuous), PtrInt(Pending^.AttachedRef));
  finally
    Ctx.Free;
  end;
end;

procedure TDWGLoadContextRefTest.InlineLineTypeAttachesWithoutFallbackWarning;
var
  Ctx: TDWGZCADLoadContext;
  Pending: PDWGZCADPendingRef;
  Recorder: TFakeRefRecorder;
  Entity, ByLayer: Pointer;
begin
  // Issue #1124: LibreDWG common entity ltype_flags can encode ByLayer,
  // ByBlock or Continuous inline, with no handle to resolve. That is a valid
  // linetype, not a broken null ref, so it must not increment fallback or
  // warning counters.
  Ctx := TDWGZCADLoadContext.Create;
  try
    Entity := MakePtr($E9);
    ByLayer := MakePtr($BD);
    SetLength(Recorder.Calls, 0);
    Ctx.SetRefAttachProc(@FakeRefAttach, @Recorder);
    Ctx.RegisterShell($109, dokEntity, Entity, 0);
    Ctx.QueueRefResolve(Entity, $109, 0, dokLineType, rsLineType, ByLayer,
      True);
    Ctx.ResolveRefs;

    Pending := Ctx.FindPendingRef($109, rsLineType);
    AssertNotNull(Pending);
    AssertEquals(Ord(asAttached), Ord(Pending^.AttachState));
    AssertEquals(Ord(arResolved), Ord(Pending^.AttachReason));
    AssertEquals(PtrInt(ByLayer), PtrInt(Pending^.AttachedRef));
    AssertEquals(1, Ctx.RefAttachCount);
    AssertEquals(0, Ctx.RefFallbackCount);
    AssertEquals(0, Ctx.WarningCount);
    AssertEquals(1, Length(Recorder.Calls));
    AssertEquals(Ord(arResolved), Ord(Recorder.Calls[0].Reason));
    AssertEquals(PtrInt(ByLayer), PtrInt(Recorder.Calls[0].Ref));
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

{ ---------- TDWGLoadContextTextStyleRefTest (Stage 5) ---------- }

procedure TDWGLoadContextTextStyleRefTest.StyleDeclaredAfterTextResolvesAtEnd;
var
  Ctx: TDWGZCADLoadContext;
  Pending: PDWGZCADPendingRef;
  TextEnt, Style, StdStyle: Pointer;
begin
  // Same out-of-order pattern as LayerDeclaredAfterEntityResolvesAtEnd:
  // TEXT may queue its style ref before the STYLE shell is registered.
  // ResolveRefs at end of load must still attach the real style.
  Ctx := TDWGZCADLoadContext.Create;
  try
    TextEnt := MakePtr($E1A);
    Style := MakePtr($A1A);
    StdStyle := MakePtr($5A1);
    Ctx.SetFallbackTextStyle(StdStyle);
    Ctx.RegisterShell($201, dokEntity, TextEnt, 0);
    Ctx.QueueRefResolve(TextEnt, $201, $202, dokTextStyle, rsTextStyle, nil);
    // STYLE arrives later in the file.
    Ctx.RegisterShell($202, dokTextStyle, Style, 1);
    Ctx.ResolveRefs;
    Pending := Ctx.FindPendingRef($201, rsTextStyle);
    AssertNotNull('pending ref recorded', Pending);
    AssertEquals(Ord(asAttached), Ord(Pending^.AttachState));
    AssertEquals(Ord(arResolved), Ord(Pending^.AttachReason));
    AssertEquals(PtrInt(Style), PtrInt(Pending^.AttachedRef));
  finally
    Ctx.Free;
  end;
end;

procedure TDWGLoadContextTextStyleRefTest.NullStyleHandleFallsBackToStandard;
var
  Ctx: TDWGZCADLoadContext;
  Pending: PDWGZCADPendingRef;
  TextEnt, StdStyle: Pointer;
begin
  // §12.5 fallback: a TEXT/MTEXT with no style handle (handle 0) routes to
  // the registered Standard fallback so subsequent FormatEntity has a usable
  // style pointer instead of nil.
  Ctx := TDWGZCADLoadContext.Create;
  try
    TextEnt := MakePtr($E2A);
    StdStyle := MakePtr($5A2);
    Ctx.SetFallbackTextStyle(StdStyle);
    Ctx.RegisterShell($203, dokEntity, TextEnt, 0);
    Ctx.QueueRefResolve(TextEnt, $203, 0, dokTextStyle, rsTextStyle, nil);
    Ctx.ResolveRefs;
    Pending := Ctx.FindPendingRef($203, rsTextStyle);
    AssertNotNull(Pending);
    AssertEquals(Ord(asFallback), Ord(Pending^.AttachState));
    AssertEquals(Ord(arRefNull), Ord(Pending^.AttachReason));
    AssertEquals(PtrInt(StdStyle), PtrInt(Pending^.AttachedRef));
  finally
    Ctx.Free;
  end;
end;

procedure TDWGLoadContextTextStyleRefTest.MissingStyleHandleFallsBackToStandard;
var
  Ctx: TDWGZCADLoadContext;
  Pending: PDWGZCADPendingRef;
  TextEnt, StdStyle: Pointer;
begin
  // §12.5: a non-zero style handle that is never registered (broken file)
  // drops onto Standard with an arRefNotFound reason — the same fallback
  // behaviour as the layer slot, so callers don't have to special-case
  // text styles.
  Ctx := TDWGZCADLoadContext.Create;
  try
    TextEnt := MakePtr($E3A);
    StdStyle := MakePtr($5A3);
    Ctx.SetFallbackTextStyle(StdStyle);
    Ctx.RegisterShell($204, dokEntity, TextEnt, 0);
    Ctx.QueueRefResolve(TextEnt, $204, $DEAD2, dokTextStyle, rsTextStyle, nil);
    Ctx.ResolveRefs;
    Pending := Ctx.FindPendingRef($204, rsTextStyle);
    AssertNotNull(Pending);
    AssertEquals(Ord(asFallback), Ord(Pending^.AttachState));
    AssertEquals(Ord(arRefNotFound), Ord(Pending^.AttachReason));
    AssertEquals(PtrInt(StdStyle), PtrInt(Pending^.AttachedRef));
  finally
    Ctx.Free;
  end;
end;

procedure TDWGLoadContextTextStyleRefTest.StyleKindMismatchFallsBackToStandard;
var
  Ctx: TDWGZCADLoadContext;
  Pending: PDWGZCADPendingRef;
  TextEnt, Layer, StdStyle: Pointer;
begin
  // §12.5: handle exists but points at the wrong table (e.g. a LAYER instead
  // of a STYLE). The entity must NOT receive the wrong-typed pointer; it
  // falls back to Standard with arRefKindMismatch so the load diagnostic
  // can flag the corruption.
  Ctx := TDWGZCADLoadContext.Create;
  try
    TextEnt := MakePtr($E4A);
    Layer := MakePtr($A4A);
    StdStyle := MakePtr($5A4);
    Ctx.SetFallbackTextStyle(StdStyle);
    Ctx.RegisterShell($205, dokEntity, TextEnt, 0);
    Ctx.RegisterShell($305, dokLayer, Layer, 1);
    Ctx.QueueRefResolve(TextEnt, $205, $305, dokTextStyle, rsTextStyle, nil);
    Ctx.ResolveRefs;
    Pending := Ctx.FindPendingRef($205, rsTextStyle);
    AssertNotNull(Pending);
    AssertEquals(Ord(asFallback), Ord(Pending^.AttachState));
    AssertEquals(Ord(arRefKindMismatch), Ord(Pending^.AttachReason));
    AssertEquals(PtrInt(StdStyle), PtrInt(Pending^.AttachedRef));
  finally
    Ctx.Free;
  end;
end;

{ ---------- TDWGLoadContextStage6Test ---------- }

procedure TDWGLoadContextStage6Test.BlockDefDeclaredAfterInsertResolvesAtEnd;
var
  Ctx: TDWGZCADLoadContext;
  Pending: PDWGZCADPendingRef;
  InsertEnt, BlockDef: Pointer;
begin
  Ctx := TDWGZCADLoadContext.Create;
  try
    InsertEnt := MakePtr($610);
    BlockDef := MakePtr($611);
    Ctx.RegisterShell($610, dokBlockInsert, InsertEnt, 0);
    Ctx.QueueRefResolve(InsertEnt, $610, $620, dokBlockDef, rsBlockDef, nil);
    Ctx.RegisterShell($620, dokBlockDef, BlockDef, 1);

    Ctx.ResolveRefs;

    Pending := Ctx.FindPendingRef($610, rsBlockDef);
    AssertNotNull(Pending);
    AssertEquals(Ord(asAttached), Ord(Pending^.AttachState));
    AssertEquals(Ord(arResolved), Ord(Pending^.AttachReason));
    AssertEquals(PtrInt(BlockDef), PtrInt(Pending^.AttachedRef));
  finally
    Ctx.Free;
  end;
end;

procedure TDWGLoadContextStage6Test.BrokenBlockRefFallsBackWithWarning;
var
  Ctx: TDWGZCADLoadContext;
  Pending: PDWGZCADPendingRef;
  InsertEnt, FallbackBlock: Pointer;
begin
  Ctx := TDWGZCADLoadContext.Create;
  try
    InsertEnt := MakePtr($612);
    FallbackBlock := MakePtr($6FF);
    Ctx.RegisterShell($612, dokBlockInsert, InsertEnt, 0);
    Ctx.QueueRefResolve(InsertEnt, $612, $DEAD6, dokBlockDef, rsBlockDef,
      FallbackBlock);

    Ctx.ResolveRefs;

    Pending := Ctx.FindPendingRef($612, rsBlockDef);
    AssertNotNull(Pending);
    AssertEquals(Ord(asFallback), Ord(Pending^.AttachState));
    AssertEquals(Ord(arRefNotFound), Ord(Pending^.AttachReason));
    AssertEquals(PtrInt(FallbackBlock), PtrInt(Pending^.AttachedRef));
    AssertTrue('missing block ref warning recorded', Ctx.WarningCount >= 1);
  finally
    Ctx.Free;
  end;
end;

procedure TDWGLoadContextStage6Test.AttribOwnerInsertResolvesToInsertContainer;
var
  Ctx: TDWGZCADLoadContext;
  Pending: PDWGZCADPendingOwner;
  InsertEnt, AttribEnt, Root: Pointer;
begin
  Ctx := TDWGZCADLoadContext.Create;
  try
    InsertEnt := MakePtr($613);
    AttribEnt := MakePtr($614);
    Root := MakePtr($6F0);
    Ctx.SetFallbackOwner(Root);
    Ctx.RegisterShell(0, dokModelSpace, Root, -1);
    Ctx.RegisterShell($613, dokBlockInsert, InsertEnt, 0);
    Ctx.RegisterShell($614, dokEntity, AttribEnt, 1);
    Ctx.QueueOwnerResolve(AttribEnt, $614, $613);

    Ctx.ResolveOwners;

    Pending := Ctx.FindPending($614);
    AssertNotNull(Pending);
    AssertEquals(Ord(asAttached), Ord(Pending^.AttachState));
    AssertEquals(Ord(arResolved), Ord(Pending^.AttachReason));
    AssertEquals(PtrInt(InsertEnt), PtrInt(Pending^.AttachedOwner));
    AssertEquals(0, Ctx.FallbackCount);
  finally
    Ctx.Free;
  end;
end;

procedure TDWGLoadContextStage6Test.DimStyleDeclaredAfterDimensionResolvesAtEnd;
var
  Ctx: TDWGZCADLoadContext;
  Pending: PDWGZCADPendingRef;
  DimEnt, DimStyle, StdStyle: Pointer;
begin
  Ctx := TDWGZCADLoadContext.Create;
  try
    DimEnt := MakePtr($615);
    DimStyle := MakePtr($616);
    StdStyle := MakePtr($6F1);
    Ctx.SetFallbackDimStyle(StdStyle);
    Ctx.RegisterShell($615, dokEntity, DimEnt, 0);
    Ctx.QueueRefResolve(DimEnt, $615, $617, dokDimStyle, rsDimStyle, nil);
    Ctx.RegisterShell($617, dokDimStyle, DimStyle, 1);

    Ctx.ResolveRefs;

    Pending := Ctx.FindPendingRef($615, rsDimStyle);
    AssertNotNull(Pending);
    AssertEquals(Ord(asAttached), Ord(Pending^.AttachState));
    AssertEquals(Ord(arResolved), Ord(Pending^.AttachReason));
    AssertEquals(PtrInt(DimStyle), PtrInt(Pending^.AttachedRef));
  finally
    Ctx.Free;
  end;
end;

procedure TDWGLoadContextStage6Test.NullDimStyleFallsBackToDefault;
var
  Ctx: TDWGZCADLoadContext;
  Pending: PDWGZCADPendingRef;
  DimEnt, StdStyle: Pointer;
begin
  Ctx := TDWGZCADLoadContext.Create;
  try
    DimEnt := MakePtr($618);
    StdStyle := MakePtr($6F2);
    Ctx.SetFallbackDimStyle(StdStyle);
    Ctx.RegisterShell($618, dokEntity, DimEnt, 0);
    Ctx.QueueRefResolve(DimEnt, $618, 0, dokDimStyle, rsDimStyle, nil);

    Ctx.ResolveRefs;

    Pending := Ctx.FindPendingRef($618, rsDimStyle);
    AssertNotNull(Pending);
    AssertEquals(Ord(asFallback), Ord(Pending^.AttachState));
    AssertEquals(Ord(arRefNull), Ord(Pending^.AttachReason));
    AssertEquals(PtrInt(StdStyle), PtrInt(Pending^.AttachedRef));
  finally
    Ctx.Free;
  end;
end;

procedure TDWGLoadContextStage6Test.BlockDefRefAcceptsModelSpaceForInsert;
var
  Ctx: TDWGZCADLoadContext;
  Pending: PDWGZCADPendingRef;
  InsertEnt, ModelRoot: Pointer;
begin
  Ctx := TDWGZCADLoadContext.Create;
  try
    InsertEnt := MakePtr($619);
    ModelRoot := MakePtr($6F3);
    Ctx.RegisterShell($619, dokBlockInsert, InsertEnt, 0);
    Ctx.RegisterShell($621, dokModelSpace, ModelRoot, 1);
    Ctx.QueueRefResolve(InsertEnt, $619, $621, dokBlockDef, rsBlockDef, nil);

    Ctx.ResolveRefs;

    Pending := Ctx.FindPendingRef($619, rsBlockDef);
    AssertNotNull(Pending);
    AssertEquals(Ord(asAttached), Ord(Pending^.AttachState));
    AssertEquals(Ord(arResolved), Ord(Pending^.AttachReason));
    AssertEquals(PtrInt(ModelRoot), PtrInt(Pending^.AttachedRef));
    AssertEquals('model-space block refs are not hard errors', 0,
      Ctx.RefFallbackCount);
  finally
    Ctx.Free;
  end;
end;

{ ---------- TDWGLoadContextSilentFallbackTest ---------- }

function TDWGLoadContextSilentFallbackTest.LocateImportSource: string;
const
  Candidates: array[0..3] of string = (
    'cad_source/zengine/fileformats/dwg/uzedwgimport.pas',
    '../uzedwgimport.pas',
    '../../uzedwgimport.pas',
    '../../../uzedwgimport.pas'
  );
var
  I: Integer;
  Base, Candidate: string;
begin
  Base := ExtractFilePath(ParamStr(0));
  if Base = '' then
    Base := IncludeTrailingPathDelimiter(GetCurrentDir);
  for I := Low(Candidates) to High(Candidates) do begin
    Candidate := Base + Candidates[I];
    if FileExists(Candidate) then
      Exit(Candidate);
    Candidate := Candidates[I];
    if FileExists(Candidate) then
      Exit(Candidate);
  end;
  Result := '';
end;

procedure TDWGLoadContextSilentFallbackTest.DWGAttachFallbackLogsUseSilentMarker;
var
  Source: TStringList;
  SourcePath, Line: string;
  I: Integer;
  Offenders: TStringList;
begin
  SourcePath := LocateImportSource;
  if SourcePath = '' then begin
    { Out-of-tree test run: source file is not reachable from CWD. The check
      is enforced wherever the project tree is available, so skip silently
      instead of failing a packaged test. }
    Exit;
  end;
  Source := TStringList.Create;
  Offenders := TStringList.Create;
  try
    Source.LoadFromFile(SourcePath);
    for I := 0 to Source.Count - 1 do begin
      Line := Source[I];
      if (Pos('{WHM}', Line) > 0) and (Pos('fallback', Line) > 0) then
        Offenders.Add(Format('%s:%d: %s',
          [ExtractFileName(SourcePath), I + 1, Trim(Line)]));
    end;
    AssertEquals(
      'Issue #1189: per-entity fallback log lines must use {WH}, not {WHM}.' +
      ' Offending lines: ' + Offenders.Text,
      0, Offenders.Count);
  finally
    Offenders.Free;
    Source.Free;
  end;
end;

procedure TDWGLoadContextSilentFallbackTest.DWGAttachReasonTextCoversAllReasons;
var
  Reason: TDWGAttachReason;
begin
  { Defensive: every TDWGAttachReason value must produce a non-empty label so
    silent log entries stay parseable when post-processed. }
  for Reason := Low(TDWGAttachReason) to High(TDWGAttachReason) do
    AssertTrue('reason ' + IntToStr(Ord(Reason)) + ' has empty text',
      DWGAttachReasonToText(Reason) <> '');
end;

{ ---------- TDWGLoadContextWarningAggregateTest (Issue #1198 P4) ---------- }

procedure TDWGLoadContextWarningAggregateTest.AddTracksTotalAndDistinctHandles;
var
  Ctx: TDWGZCADLoadContext;
  Agg: TDWGImportCodeAggregate;
  I, Found: Integer;
begin
  Ctx := TDWGZCADLoadContext.Create;
  try
    { Three occurrences of code 1410 against two distinct handles, plus one
      occurrence of code 1402 against a third handle: aggregate should have
      two rows, with the right totals on each. }
    Ctx.RaiseWarning(wsWarning, DWG_WARN_REF_KIND_MISMATCH, $100, 'a');
    Ctx.RaiseWarning(wsWarning, DWG_WARN_REF_KIND_MISMATCH, $100, 'b');
    Ctx.RaiseWarning(wsWarning, DWG_WARN_REF_KIND_MISMATCH, $200, 'c');
    Ctx.RaiseWarning(wsWarning, DWG_WARN_OWNER_NOT_FOUND, $300, 'd');

    AssertEquals('aggregate rows', 2, Ctx.WarningAggregateCount);
    Found := 0;
    for I := 0 to Ctx.WarningAggregateCount - 1 do begin
      Agg := Ctx.WarningAggregateAt(I);
      if Agg.Code = DWG_WARN_REF_KIND_MISMATCH then begin
        AssertEquals('total for 1410', 3, Agg.TotalCount);
        AssertEquals('distinct handles for 1410', 2, Agg.DistinctHandles);
        Inc(Found);
      end
      else if Agg.Code = DWG_WARN_OWNER_NOT_FOUND then begin
        AssertEquals('total for 1402', 1, Agg.TotalCount);
        AssertEquals('distinct handles for 1402', 1, Agg.DistinctHandles);
        Inc(Found);
      end;
    end;
    AssertEquals('both codes reported', 2, Found);
  finally
    Ctx.Free;
  end;
end;

procedure TDWGLoadContextWarningAggregateTest.FirstSampleIsCapturedAndPreserved;
var
  Ctx: TDWGZCADLoadContext;
  Agg: TDWGImportCodeAggregate;
  I: Integer;
begin
  Ctx := TDWGZCADLoadContext.Create;
  try
    Ctx.RaiseWarning(wsWarning, DWG_WARN_REF_NOT_FOUND, $42, 'first sample');
    Ctx.RaiseWarning(wsWarning, DWG_WARN_REF_NOT_FOUND, $43, 'second');
    Ctx.RaiseWarning(wsWarning, DWG_WARN_REF_NOT_FOUND, $44, 'third');

    for I := 0 to Ctx.WarningAggregateCount - 1 do begin
      Agg := Ctx.WarningAggregateAt(I);
      if Agg.Code = DWG_WARN_REF_NOT_FOUND then begin
        AssertTrue('first sample captured', Agg.HasFirstSample);
        AssertEquals('first sample handle preserved',
          Int64($42), Int64(Agg.FirstSample.Handle));
        AssertEquals('first sample text preserved',
          'first sample', Agg.FirstSample.Text);
        Exit;
      end;
    end;
    Fail('no aggregate row for DWG_WARN_REF_NOT_FOUND');
  finally
    Ctx.Free;
  end;
end;

procedure TDWGLoadContextWarningAggregateTest.ShouldEmitDetailFiresOnceThenSuppresses;
var
  Ctx: TDWGZCADLoadContext;
begin
  Ctx := TDWGZCADLoadContext.Create;
  try
    { First occurrence wins the slot, the second is throttled. }
    AssertTrue('first call emits',
      Ctx.ShouldEmitDetail(DWG_WARN_REF_KIND_MISMATCH, $7000));
    AssertFalse('second call suppressed',
      Ctx.ShouldEmitDetail(DWG_WARN_REF_KIND_MISMATCH, $7000));
    AssertFalse('third call still suppressed',
      Ctx.ShouldEmitDetail(DWG_WARN_REF_KIND_MISMATCH, $7000));
  finally
    Ctx.Free;
  end;
end;

procedure TDWGLoadContextWarningAggregateTest.ShouldEmitDetailKeysOnCodeAndHandle;
var
  Ctx: TDWGZCADLoadContext;
begin
  Ctx := TDWGZCADLoadContext.Create;
  try
    { Different handle => fresh slot, not coalesced with the previous one. }
    AssertTrue('code 1410 handle $7001',
      Ctx.ShouldEmitDetail(DWG_WARN_REF_KIND_MISMATCH, $7001));
    AssertTrue('code 1410 handle $7002 distinct from $7001',
      Ctx.ShouldEmitDetail(DWG_WARN_REF_KIND_MISMATCH, $7002));
    { Same handle, different code => fresh slot. }
    AssertTrue('code 1402 handle $7001 distinct from code 1410',
      Ctx.ShouldEmitDetail(DWG_WARN_OWNER_NOT_FOUND, $7001));
    { Repeat of any one of them is suppressed. }
    AssertFalse('code 1410 handle $7001 second time suppressed',
      Ctx.ShouldEmitDetail(DWG_WARN_REF_KIND_MISMATCH, $7001));
  finally
    Ctx.Free;
  end;
end;

procedure TDWGLoadContextWarningAggregateTest.DistinctHandlesIncrementsOnNewHandleOnly;
var
  Ctx: TDWGZCADLoadContext;
  Agg: TDWGImportCodeAggregate;
  I: Integer;
begin
  Ctx := TDWGZCADLoadContext.Create;
  try
    { Five occurrences split across two handles: distinct=2, total=5. }
    Ctx.RaiseWarning(wsWarning, DWG_WARN_REF_NULL, $A0, '');
    Ctx.RaiseWarning(wsWarning, DWG_WARN_REF_NULL, $A0, '');
    Ctx.RaiseWarning(wsWarning, DWG_WARN_REF_NULL, $A0, '');
    Ctx.RaiseWarning(wsWarning, DWG_WARN_REF_NULL, $B0, '');
    Ctx.RaiseWarning(wsWarning, DWG_WARN_REF_NULL, $B0, '');

    for I := 0 to Ctx.WarningAggregateCount - 1 do begin
      Agg := Ctx.WarningAggregateAt(I);
      if Agg.Code = DWG_WARN_REF_NULL then begin
        AssertEquals('total', 5, Agg.TotalCount);
        AssertEquals('distinct handles', 2, Agg.DistinctHandles);
        Exit;
      end;
    end;
    Fail('no aggregate row for DWG_WARN_REF_NULL');
  finally
    Ctx.Free;
  end;
end;

procedure TDWGLoadContextWarningAggregateTest.CodeForAttachReasonCoversFallbackReasons;
begin
  { Every fallback reason must map to a real diagnostic code so the import
    side gate uses the same dedup key as the resolver side aggregate. }
  AssertEquals(DWG_WARN_OWNER_NULL,
    DWGCodeForAttachReason(arNullOwner));
  AssertEquals(DWG_WARN_OWNER_NOT_FOUND,
    DWGCodeForAttachReason(arOwnerNotFound));
  AssertEquals(DWG_WARN_OWNER_NOT_CONTAINER,
    DWGCodeForAttachReason(arOwnerNotContainer));
  AssertEquals(DWG_WARN_OWNER_SELF_CYCLE,
    DWGCodeForAttachReason(arSelfOwnerCycle));
  AssertEquals(DWG_WARN_OWNER_CHAIN_CYCLE,
    DWGCodeForAttachReason(arOwnerChainCycle));
  AssertEquals(DWG_WARN_OWNER_SKIPPED,
    DWGCodeForAttachReason(arOwnerSkipped));
  AssertEquals(DWG_WARN_REF_NULL,
    DWGCodeForAttachReason(arRefNull));
  AssertEquals(DWG_WARN_REF_NOT_FOUND,
    DWGCodeForAttachReason(arRefNotFound));
  AssertEquals(DWG_WARN_REF_KIND_MISMATCH,
    DWGCodeForAttachReason(arRefKindMismatch));
  { Non-fallback reasons return 0 so the caller can guard. }
  AssertEquals('arResolved has no code', 0,
    DWGCodeForAttachReason(arResolved));
  AssertEquals('arPending has no code', 0,
    DWGCodeForAttachReason(arPending));
end;

initialization
  RegisterTests([TDWGLoadContextHandleMapTest, TDWGLoadContextResolveTest,
    TDWGLoadContextRefTest, TDWGLoadContextBlockTest,
    TDWGLoadContextTextStyleRefTest, TDWGLoadContextStage6Test,
    TDWGLoadContextSilentFallbackTest,
    TDWGLoadContextWarningAggregateTest]);

end.
