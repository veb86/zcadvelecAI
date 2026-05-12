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

  { Issue #1198 P3 (per АНАЛИЗ_ЗАГРУЗЧИКА_DWG.md §P3 / §6.1) regression:
    diagnostic side-files. Tests use a tempdir-scoped path and assert that
    each mode produces the expected files and that CSV headers and rows
    match the documented columns. }
  TDWGLoadContextSideFilesTest = class(TTestCase)
  private
    FTempDir: String;
    function TempPath(const Suffix: String): String;
    function ReadAllText(const Path: String): String;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure ModeFromStringIsCaseInsensitive;
    procedure ModeOffWritesNothing;
    procedure ModeSummaryWritesTxtAndJson;
    procedure ModeFullAddsThreeCsvFiles;
    procedure HandlesCsvCapturesEveryRegisteredHandle;
    procedure RefsCsvCapturesEveryPendingRef;
    procedure OwnersCsvCapturesEveryPendingOwner;
    procedure SummaryTxtIncludesWarningsByCode;
    procedure SummaryJsonIsParseableShape;
    procedure SourcePathEmptyFallsBackToCwd;
  end;

  { Issue #1198 P2 (per АНАЛИЗ_ЗАГРУЗЧИКА_DWG.md §P2 / §5) regression:
    fixedtype histogram and handler-registry introspection. Tests live in the
    load-context suite because they exercise the FixedType field stored on
    TDWGZCADHandleEntry plus the small helpers added to uzedwgsidefiles. }
  TDWGLoadContextFixedTypeTest = class(TTestCase)
  published
    procedure RegisterShellInitializesFixedTypeToUnused;
    procedure FixedTypeIsMutableAfterRegisterShell;
    procedure FixedTypeToTextReturnsSymbolicNameForKnownEnum;
    procedure FixedTypeToTextFallsBackToHexForUndeclaredValue;
    procedure CountByFixedTypeBucketsByDistinctFixedType;
    procedure CountByFixedTypeOrdersByDescendingCount;
    procedure CountByFixedTypeIsEmptyForEmptyContext;
    procedure HasHandlerForReturnsFalseForUnregisteredType;
    procedure HasHandlerForReturnsTrueForRegisteredControlObject;
    procedure HandlesCsvIncludesFixedTypeColumn;
    procedure SummaryTxtIncludesFixedTypeSection;
    procedure SummaryJsonIncludesFixedTypesField;
  end;

  { Issue #1198 P5 (per АНАЛИЗ_ЗАГРУЗЧИКА_DWG.md §P5 / §4.5) regression:
    pending-list lookups must be O(log N) and preserve legacy semantics.
    The pending-owner list keys on EntityHandle alone and is expected to
    return the first matching item (legacy contract). The pending-ref list
    keys on (EntityHandle, Slot) and replaces the existing item when the
    same key is queued again. Tests exercise the indexes through the public
    surface (Append, AppendOrReplace, ItemByEntityHandle / ItemByEntityAndSlot,
    Clear) plus the integrating context (QueueOwnerResolve / QueueRefResolve). }
  TDWGLoadContextPendingIndexTest = class(TTestCase)
  published
    procedure OwnerLookupFindsAppendedItem;
    procedure OwnerLookupReturnsNilForMissingHandle;
    procedure OwnerLookupReturnsFirstItemForRepeatedHandle;
    procedure OwnerLookupHandlesOutOfOrderInsertion;
    procedure OwnerClearResetsBothItemsAndIndex;
    procedure RefLookupFindsAppendedItem;
    procedure RefLookupReturnsNilForMissingKey;
    procedure RefLookupKeysSeparatelyOnSlot;
    procedure RefAppendOrReplaceReusesExistingItemIndex;
    procedure RefAppendOrReplaceDoesNotGrowIndexForReplace;
    procedure RefClearResetsBothItemsAndIndex;
    procedure RefLookupHandlesOutOfOrderInsertion;
    procedure QueueOwnerResolveIntegrationKeepsItemReachable;
    procedure QueueRefResolveIntegrationKeepsItemReachable;
  end;

  { Issue #1198 P6 (per АНАЛИЗ_ЗАГРУЗЧИКА_DWG.md §4.4/§P6) regression:
    AddTextStyle must not collapse distinct DWG handles onto a single ZCAD
    pstyle by renaming everything to 'Standard'. The pure helpers
    DWGTextStyleBaseName / DWGTextStyleUniquifyName / DWGTextStylePtrOwned-
    ByAnotherHandle encode the new rules; these tests pin them so future
    refactors cannot silently regress to the old aliasing behaviour. The
    tests use opaque pointers and a real TDWGZCADLoadContext (no ZCAD
    drawing dependency) so they run in the same isolated harness as the
    other Stage-5 regressions. }
  TDWGLoadContextTextStyleNameTest = class(TTestCase)
  published
    procedure BaseNameUsesFontFileForShapeStyles;
    procedure BaseNameUsesDecodedNameWhenPresent;
    procedure BaseNameFallsBackToHandleHexWhenNameEmpty;
    procedure BaseNameFallsBackToStandardWhenHandleZero;
    procedure UniquifyAppendsHandleHexSuffix;
    procedure UniquifyHandlesZeroHandleDefensively;
    procedure PtrOwnedReturnsFalseForUnregisteredPointer;
    procedure PtrOwnedReturnsFalseForSameHandleReregistration;
    procedure PtrOwnedReturnsTrueWhenAnotherHandleClaimsPointer;
    procedure PtrOwnedIgnoresEntriesOfOtherKinds;
    procedure PtrOwnedReturnsFalseForNilContextOrPtr;
  end;

implementation

uses
  Classes,
  SysUtils,
  dwg,
  uzedwgtypes,
  uzedwgdiagnostics,
  uzedwgloadcontext,
  uzedwgsidefiles,
  uzedwgentityregistry,
  uzedwgcontrolobjects,
  uzedwgstylename;

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

{ ---------- TDWGLoadContextSideFilesTest (Issue #1198 P3) ---------- }

procedure TDWGLoadContextSideFilesTest.SetUp;
begin
  inherited;
  FTempDir := IncludeTrailingPathDelimiter(GetTempDir) +
    'dwgsidefiles_' + IntToStr(Random(MaxInt)) + '_' +
    IntToStr(Random(MaxInt));
  if not ForceDirectories(FTempDir) then
    Fail('cannot create temp dir ' + FTempDir);
end;

procedure TDWGLoadContextSideFilesTest.TearDown;
var
  Search: TSearchRec;
begin
  if DirectoryExists(FTempDir) then begin
    if FindFirst(IncludeTrailingPathDelimiter(FTempDir) + '*', faAnyFile, Search) = 0 then
    try
      repeat
        if (Search.Name <> '.') and (Search.Name <> '..') then
          DeleteFile(IncludeTrailingPathDelimiter(FTempDir) + Search.Name);
      until FindNext(Search) <> 0;
    finally
      FindClose(Search);
    end;
    RemoveDir(FTempDir);
  end;
  inherited;
end;

function TDWGLoadContextSideFilesTest.TempPath(const Suffix: String): String;
begin
  Result := IncludeTrailingPathDelimiter(FTempDir) + 'unit' + Suffix;
end;

function TDWGLoadContextSideFilesTest.ReadAllText(const Path: String): String;
var
  Stream: TFileStream;
  Bytes: TBytes;
begin
  Result := '';
  Stream := TFileStream.Create(Path, fmOpenRead or fmShareDenyNone);
  try
    SetLength(Bytes, Stream.Size);
    if Stream.Size > 0 then
      Stream.ReadBuffer(Bytes[0], Stream.Size);
    SetLength(Result, Length(Bytes));
    if Length(Bytes) > 0 then
      Move(Bytes[0], Result[1], Length(Bytes));
  finally
    Stream.Free;
  end;
end;

procedure TDWGLoadContextSideFilesTest.ModeFromStringIsCaseInsensitive;
begin
  AssertTrue('off',     DWGDiagModeFromString('off')     = dmOff);
  AssertTrue('OFF',     DWGDiagModeFromString('OFF')     = dmOff);
  AssertTrue('summary', DWGDiagModeFromString('summary') = dmSummary);
  AssertTrue('Summary', DWGDiagModeFromString('Summary') = dmSummary);
  AssertTrue('full',    DWGDiagModeFromString('full')    = dmFull);
  AssertTrue('trace',   DWGDiagModeFromString('trace')   = dmTrace);
  AssertTrue('unknown', DWGDiagModeFromString('garble')  = dmOff);
  AssertTrue('empty',   DWGDiagModeFromString('')        = dmOff);
end;

procedure TDWGLoadContextSideFilesTest.ModeOffWritesNothing;
var
  Ctx: TDWGZCADLoadContext;
  Res: TDWGSideFileResult;
begin
  Ctx := TDWGZCADLoadContext.Create;
  try
    Ctx.RegisterShell($10, dokEntity, MakePtr($AA01), 0);
    Res := DWGWriteSideFiles(Ctx, TempPath('.dwg'), dmOff);
    AssertEquals('no files', 0, Length(Res.FilesWritten));
    AssertFalse(FileExists(TempPath('.dwg.summary.txt')));
    AssertFalse(FileExists(TempPath('.dwg.summary.json')));
    AssertFalse(FileExists(TempPath('.dwg.handles.csv')));
  finally
    Ctx.Free;
  end;
end;

procedure TDWGLoadContextSideFilesTest.ModeSummaryWritesTxtAndJson;
var
  Ctx: TDWGZCADLoadContext;
  Res: TDWGSideFileResult;
begin
  Ctx := TDWGZCADLoadContext.Create;
  try
    Ctx.RegisterShell($10, dokEntity, MakePtr($AA01), 0);
    Res := DWGWriteSideFiles(Ctx, TempPath('.dwg'), dmSummary);
    AssertEquals('two files', 2, Length(Res.FilesWritten));
    AssertTrue('summary.txt exists', FileExists(TempPath('.dwg.summary.txt')));
    AssertTrue('summary.json exists', FileExists(TempPath('.dwg.summary.json')));
    AssertFalse('handles.csv absent', FileExists(TempPath('.dwg.handles.csv')));
    AssertFalse('refs.csv absent', FileExists(TempPath('.dwg.refs.csv')));
    AssertFalse('owners.csv absent', FileExists(TempPath('.dwg.owners.csv')));
  finally
    Ctx.Free;
  end;
end;

procedure TDWGLoadContextSideFilesTest.ModeFullAddsThreeCsvFiles;
var
  Ctx: TDWGZCADLoadContext;
  Res: TDWGSideFileResult;
begin
  Ctx := TDWGZCADLoadContext.Create;
  try
    Ctx.RegisterShell($10, dokEntity, MakePtr($AA01), 0);
    Res := DWGWriteSideFiles(Ctx, TempPath('.dwg'), dmFull);
    AssertEquals('five files', 5, Length(Res.FilesWritten));
    AssertTrue(FileExists(TempPath('.dwg.summary.txt')));
    AssertTrue(FileExists(TempPath('.dwg.summary.json')));
    AssertTrue(FileExists(TempPath('.dwg.handles.csv')));
    AssertTrue(FileExists(TempPath('.dwg.refs.csv')));
    AssertTrue(FileExists(TempPath('.dwg.owners.csv')));
  finally
    Ctx.Free;
  end;
end;

procedure TDWGLoadContextSideFilesTest.HandlesCsvCapturesEveryRegisteredHandle;
var
  Ctx: TDWGZCADLoadContext;
  Path, Text: String;
begin
  Ctx := TDWGZCADLoadContext.Create;
  try
    Ctx.RegisterShell($10, dokEntity, MakePtr($AA01), 7);
    Ctx.RegisterShell($20, dokLayer, MakePtr($AA02), 8);
    Path := TempPath('.dwg.handles.csv');
    DWGWriteHandlesCsv(Ctx, Path);
    Text := ReadAllText(Path);
    AssertTrue('header present', Pos('RawIndex;HandleHex;ResolvedKind', Text) > 0);
    AssertTrue('hex 10 row',  Pos('7;10;dokEntity;', Text) > 0);
    AssertTrue('hex 20 row',  Pos('8;20;dokLayer;',  Text) > 0);
  finally
    Ctx.Free;
  end;
end;

procedure TDWGLoadContextSideFilesTest.RefsCsvCapturesEveryPendingRef;
var
  Ctx: TDWGZCADLoadContext;
  Path, Text: String;
  Ent, Layer: Pointer;
begin
  Ctx := TDWGZCADLoadContext.Create;
  try
    Ent := MakePtr($AA01);
    Layer := MakePtr($AA02);
    Ctx.RegisterShell($10, dokEntity, Ent, 0);
    Ctx.RegisterShell($20, dokLayer, Layer, 1);
    Ctx.QueueRefResolve(Ent, $10, $20, dokLayer, rsLayer, nil);
    Path := TempPath('.dwg.refs.csv');
    DWGWriteRefsCsv(Ctx, Path);
    Text := ReadAllText(Path);
    AssertTrue('header present',
      Pos('EntityHandle;Slot;RefHandle', Text) > 0);
    AssertTrue('rsLayer row', Pos('10;rsLayer;20;', Text) > 0);
  finally
    Ctx.Free;
  end;
end;

procedure TDWGLoadContextSideFilesTest.OwnersCsvCapturesEveryPendingOwner;
var
  Ctx: TDWGZCADLoadContext;
  Path, Text: String;
  Ent, Block: Pointer;
begin
  Ctx := TDWGZCADLoadContext.Create;
  try
    Ent := MakePtr($AA01);
    Block := MakePtr($AA02);
    Ctx.RegisterShell($10, dokEntity, Ent, 0);
    Ctx.RegisterShell($20, dokBlockDef, Block, 1);
    Ctx.QueueOwnerResolve(Ent, $10, $20);
    Path := TempPath('.dwg.owners.csv');
    DWGWriteOwnersCsv(Ctx, Path);
    Text := ReadAllText(Path);
    AssertTrue('header present',
      Pos('EntityHandle;OwnerHandle;Candidates', Text) > 0);
    AssertTrue('owner row', Pos('10;20;', Text) > 0);
  finally
    Ctx.Free;
  end;
end;

procedure TDWGLoadContextSideFilesTest.SummaryTxtIncludesWarningsByCode;
var
  Ctx: TDWGZCADLoadContext;
  Path, Text: String;
begin
  Ctx := TDWGZCADLoadContext.Create;
  try
    Ctx.RegisterShell($10, dokEntity, MakePtr($AA01), 0);
    Ctx.RaiseWarning(wsWarning, DWG_WARN_REF_KIND_MISMATCH, $10, 'kindmiss-a');
    Ctx.RaiseWarning(wsWarning, DWG_WARN_REF_KIND_MISMATCH, $11, 'kindmiss-b');
    Ctx.RaiseWarning(wsWarning, DWG_WARN_OWNER_NOT_FOUND, $10, 'ownermissing');
    Path := TempPath('.dwg.summary.txt');
    DWGWriteSummaryTxt(Ctx, TempPath('.dwg'), Path);
    Text := ReadAllText(Path);
    AssertTrue('handles_total line', Pos('handles_total: 1', Text) > 0);
    AssertTrue('1410 reported', Pos('1410 (ref kind mismatch)', Text) > 0);
    AssertTrue('1402 reported', Pos('1402 (owner not found)', Text) > 0);
    AssertTrue('kind histogram', Pos('dokEntity: 1', Text) > 0);
  finally
    Ctx.Free;
  end;
end;

procedure TDWGLoadContextSideFilesTest.SummaryJsonIsParseableShape;
var
  Ctx: TDWGZCADLoadContext;
  Path, Text: String;
begin
  Ctx := TDWGZCADLoadContext.Create;
  try
    Ctx.RegisterShell($10, dokEntity, MakePtr($AA01), 0);
    Ctx.RaiseWarning(wsWarning, DWG_WARN_REF_KIND_MISMATCH, $10, 'kindmiss');
    Path := TempPath('.dwg.summary.json');
    DWGWriteSummaryJson(Ctx, TempPath('.dwg'), Path);
    Text := ReadAllText(Path);
    { Shape only; the writer is not a JSON-spec-compliant emitter and we
      do not want to hard-code field order. Just verify the obvious
      hooks exist so a downstream script can do its own structured read. }
    AssertTrue('top brace',  Pos('{', Text) > 0);
    AssertTrue('file key',   Pos('"file":', Text) > 0);
    AssertTrue('kinds key',  Pos('"kinds":', Text) > 0);
    AssertTrue('warnings',   Pos('"warnings":', Text) > 0);
    AssertTrue('1410 entry', Pos('"1410": 1', Text) > 0);
    AssertTrue('end brace',  Pos('}', Text) > 0);
  finally
    Ctx.Free;
  end;
end;

procedure TDWGLoadContextSideFilesTest.SourcePathEmptyFallsBackToCwd;
begin
  { Helper-level test: an empty source path must not crash; the writer
    falls back to a generic "dwg.<suffix>" name. We don't write the file
    here (cwd is not under our control) but the path constructor result
    is well-defined and tested directly. }
  AssertEquals('dwg.summary.txt', DWGSideFilePath('', '.summary.txt'));
  AssertEquals('/x/foo.dwg.summary.txt',
    DWGSideFilePath('/x/foo.dwg', '.summary.txt'));
end;

{ ---------- TDWGLoadContextFixedTypeTest ---------- }

function ReadAllTextFileP2(const Path: String): String;
var
  Stream: TFileStream;
  Bytes: TBytes;
begin
  Result := '';
  Stream := TFileStream.Create(Path, fmOpenRead or fmShareDenyNone);
  try
    SetLength(Bytes, Stream.Size);
    if Stream.Size > 0 then
      Stream.ReadBuffer(Bytes[0], Stream.Size);
    SetLength(Result, Length(Bytes));
    if Length(Bytes) > 0 then
      Move(Bytes[0], Result[1], Length(Bytes));
  finally
    Stream.Free;
  end;
end;

procedure TDWGLoadContextFixedTypeTest.RegisterShellInitializesFixedTypeToUnused;
var
  Map: TDWGZCADHandleMap;
  Entry: TDWGZCADHandleEntry;
begin
  // Mapper-side RegisterShell calls (pre-Phase 1, tests) must not leave the
  // FixedType field uninitialized — DWG_TYPE_UNUSED (0) is the sentinel that
  // means "raw scan did not see this handle".
  Map := TDWGZCADHandleMap.Create;
  try
    AssertTrue(Map.RegisterShell($10, dokEntity, MakePtr($A1), 0, msCreated));
    AssertTrue(Map.TryGet($10, Entry));
    AssertEquals('fresh shell starts at DWG_TYPE_UNUSED',
      Ord(DWG_TYPE_UNUSED), Ord(Entry.FixedType));
  finally
    Map.Free;
  end;
end;

procedure TDWGLoadContextFixedTypeTest.FixedTypeIsMutableAfterRegisterShell;
var
  Map: TDWGZCADHandleMap;
  Mut: PDWGZCADHandleEntry;
  Entry: TDWGZCADHandleEntry;
begin
  // ScanRawObjects writes FixedType through TryGetMutable — the test mirrors
  // exactly that sequence so a future refactor that loses the mutator surface
  // breaks here instead of silently leaving FixedType at DWG_TYPE_UNUSED.
  Map := TDWGZCADHandleMap.Create;
  try
    AssertTrue(Map.RegisterShell($20, dokUnknown, nil, 5, msCreated));
    AssertTrue(Map.TryGetMutable($20, Mut));
    AssertNotNull(Mut);
    Mut^.FixedType := DWG_TYPE_LINE;
    AssertTrue(Map.TryGet($20, Entry));
    AssertEquals(Ord(DWG_TYPE_LINE), Ord(Entry.FixedType));
  finally
    Map.Free;
  end;
end;

procedure TDWGLoadContextFixedTypeTest.FixedTypeToTextReturnsSymbolicNameForKnownEnum;
begin
  // RTTI path: GetEnumName must produce the declared identifier so the
  // histogram is readable as 'DWG_TYPE_LINE' rather than '0x13'.
  AssertEquals('DWG_TYPE_LINE',   DWGFixedTypeToText(DWG_TYPE_LINE));
  AssertEquals('DWG_TYPE_CIRCLE', DWGFixedTypeToText(DWG_TYPE_CIRCLE));
  AssertEquals('DWG_TYPE_UNUSED', DWGFixedTypeToText(DWG_TYPE_UNUSED));
end;

procedure TDWGLoadContextFixedTypeTest.FixedTypeToTextFallsBackToHexForUndeclaredValue;
var
  Text: String;
begin
  // DWG_OBJECT_TYPE has gaps ($36, $37 are not declared identifiers). Casting
  // a gap integer through the enum yields a value GetEnumName cannot name —
  // the helper must still produce a non-empty 'DWG_TYPE_$NN' fallback so the
  // histogram does not lose the row.
  Text := DWGFixedTypeToText(DWG_OBJECT_TYPE($36));
  AssertTrue('non-empty fallback', Length(Text) > 0);
  AssertTrue('hex marker present', Pos('36', Text) > 0);
end;

procedure TDWGLoadContextFixedTypeTest.CountByFixedTypeBucketsByDistinctFixedType;
var
  Ctx: TDWGZCADLoadContext;
  Mut: PDWGZCADHandleEntry;
  Counters: TDWGFixedTypeCounterArray;
  I, LineBucket, CircleBucket: Integer;
begin
  // Two handles share DWG_TYPE_LINE, one is DWG_TYPE_CIRCLE — the counter
  // array must contain exactly two buckets with counts 2 and 1.
  Ctx := TDWGZCADLoadContext.Create;
  try
    Ctx.RegisterShell($10, dokEntity, MakePtr($A1), 0);
    Ctx.RegisterShell($11, dokEntity, MakePtr($A2), 1);
    Ctx.RegisterShell($12, dokEntity, MakePtr($A3), 2);
    AssertTrue(Ctx.Handles.TryGetMutable($10, Mut)); Mut^.FixedType := DWG_TYPE_LINE;
    AssertTrue(Ctx.Handles.TryGetMutable($11, Mut)); Mut^.FixedType := DWG_TYPE_LINE;
    AssertTrue(Ctx.Handles.TryGetMutable($12, Mut)); Mut^.FixedType := DWG_TYPE_CIRCLE;
    DWGCountByFixedType(Ctx, Counters);
    AssertEquals('two distinct fixedtypes', 2, Length(Counters));
    LineBucket := -1;
    CircleBucket := -1;
    for I := 0 to High(Counters) do begin
      if Counters[I].FixedType = DWG_TYPE_LINE then LineBucket := I;
      if Counters[I].FixedType = DWG_TYPE_CIRCLE then CircleBucket := I;
    end;
    AssertTrue('line bucket present', LineBucket >= 0);
    AssertTrue('circle bucket present', CircleBucket >= 0);
    AssertEquals('line count', 2, Counters[LineBucket].Count);
    AssertEquals('circle count', 1, Counters[CircleBucket].Count);
  finally
    Ctx.Free;
  end;
end;

procedure TDWGLoadContextFixedTypeTest.CountByFixedTypeOrdersByDescendingCount;
var
  Ctx: TDWGZCADLoadContext;
  Mut: PDWGZCADHandleEntry;
  Counters: TDWGFixedTypeCounterArray;
begin
  // 3 LINE, 1 CIRCLE — LINE must appear first.
  Ctx := TDWGZCADLoadContext.Create;
  try
    Ctx.RegisterShell($10, dokEntity, MakePtr($A1), 0);
    Ctx.RegisterShell($11, dokEntity, MakePtr($A2), 1);
    Ctx.RegisterShell($12, dokEntity, MakePtr($A3), 2);
    Ctx.RegisterShell($13, dokEntity, MakePtr($A4), 3);
    AssertTrue(Ctx.Handles.TryGetMutable($10, Mut)); Mut^.FixedType := DWG_TYPE_CIRCLE;
    AssertTrue(Ctx.Handles.TryGetMutable($11, Mut)); Mut^.FixedType := DWG_TYPE_LINE;
    AssertTrue(Ctx.Handles.TryGetMutable($12, Mut)); Mut^.FixedType := DWG_TYPE_LINE;
    AssertTrue(Ctx.Handles.TryGetMutable($13, Mut)); Mut^.FixedType := DWG_TYPE_LINE;
    DWGCountByFixedType(Ctx, Counters);
    AssertEquals(2, Length(Counters));
    AssertEquals('first bucket is the highest count',
      Ord(DWG_TYPE_LINE), Ord(Counters[0].FixedType));
    AssertEquals(3, Counters[0].Count);
    AssertEquals(Ord(DWG_TYPE_CIRCLE), Ord(Counters[1].FixedType));
    AssertEquals(1, Counters[1].Count);
  finally
    Ctx.Free;
  end;
end;

procedure TDWGLoadContextFixedTypeTest.CountByFixedTypeIsEmptyForEmptyContext;
var
  Ctx: TDWGZCADLoadContext;
  Counters: TDWGFixedTypeCounterArray;
begin
  Ctx := TDWGZCADLoadContext.Create;
  try
    DWGCountByFixedType(Ctx, Counters);
    AssertEquals('no handles -> no buckets', 0, Length(Counters));
  finally
    Ctx.Free;
  end;
end;

procedure TDWGLoadContextFixedTypeTest.HasHandlerForReturnsFalseForUnregisteredType;
begin
  // The test unit deliberately does NOT pull entity mappers into its uses
  // clause, so DWG_TYPE_LINE has no handler registered against the shared
  // parser singleton.
  AssertFalse('DWG_TYPE_LINE is unregistered in this test unit',
    HasHandlerFor(DWG_TYPE_LINE));
end;

procedure TDWGLoadContextFixedTypeTest.HasHandlerForReturnsTrueForRegisteredControlObject;
begin
  // uzedwgcontrolobjects.initialization registers DWG_TYPE_SEQEND through
  // RegisterDWGEntityHandler — the lookup must report it as known.
  AssertTrue('DWG_TYPE_SEQEND is registered by uzedwgcontrolobjects',
    HasHandlerFor(DWG_TYPE_SEQEND));
  AssertTrue('DWG_TYPE_DICTIONARY is registered by uzedwgcontrolobjects',
    HasHandlerFor(DWG_TYPE_DICTIONARY));
  AssertTrue('DWG_TYPE_LAYER_CONTROL is registered by uzedwgcontrolobjects',
    HasHandlerFor(DWG_TYPE_LAYER_CONTROL));
end;

procedure TDWGLoadContextFixedTypeTest.HandlesCsvIncludesFixedTypeColumn;
var
  Ctx: TDWGZCADLoadContext;
  Mut: PDWGZCADHandleEntry;
  Path, Text, TempDir: String;
begin
  TempDir := IncludeTrailingPathDelimiter(GetTempDir) +
    'dwgtest_p2_csv_' + IntToStr(Random(MaxInt));
  ForceDirectories(TempDir);
  Path := IncludeTrailingPathDelimiter(TempDir) + 'h.csv';
  try
    Ctx := TDWGZCADLoadContext.Create;
    try
      Ctx.RegisterShell($10, dokEntity, MakePtr($AA01), 7);
      AssertTrue(Ctx.Handles.TryGetMutable($10, Mut));
      Mut^.FixedType := DWG_TYPE_LINE;
      DWGWriteHandlesCsv(Ctx, Path);
      Text := ReadAllTextFileP2(Path);
      AssertTrue('header has FixedType column',
        Pos('FixedType', Text) > 0);
      AssertTrue('row carries DWG_TYPE_LINE',
        Pos(';DWG_TYPE_LINE', Text) > 0);
    finally
      Ctx.Free;
    end;
  finally
    if FileExists(Path) then
      DeleteFile(Path);
    RemoveDir(TempDir);
  end;
end;

procedure TDWGLoadContextFixedTypeTest.SummaryTxtIncludesFixedTypeSection;
var
  Ctx: TDWGZCADLoadContext;
  Mut: PDWGZCADHandleEntry;
  Path, Text, TempDir: String;
begin
  TempDir := IncludeTrailingPathDelimiter(GetTempDir) +
    'dwgtest_p2_txt_' + IntToStr(Random(MaxInt));
  ForceDirectories(TempDir);
  Path := IncludeTrailingPathDelimiter(TempDir) + 's.txt';
  try
    Ctx := TDWGZCADLoadContext.Create;
    try
      Ctx.RegisterShell($10, dokEntity, MakePtr($AA01), 0);
      AssertTrue(Ctx.Handles.TryGetMutable($10, Mut));
      Mut^.FixedType := DWG_TYPE_LINE;
      DWGWriteSummaryTxt(Ctx, '/x/foo.dwg', Path);
      Text := ReadAllTextFileP2(Path);
      AssertTrue('# Handles by fixedtype section',
        Pos('# Handles by fixedtype', Text) > 0);
      AssertTrue('row carries DWG_TYPE_LINE: 1',
        Pos('DWG_TYPE_LINE: 1', Text) > 0);
    finally
      Ctx.Free;
    end;
  finally
    if FileExists(Path) then
      DeleteFile(Path);
    RemoveDir(TempDir);
  end;
end;

procedure TDWGLoadContextFixedTypeTest.SummaryJsonIncludesFixedTypesField;
var
  Ctx: TDWGZCADLoadContext;
  Mut: PDWGZCADHandleEntry;
  Path, Text, TempDir: String;
begin
  TempDir := IncludeTrailingPathDelimiter(GetTempDir) +
    'dwgtest_p2_json_' + IntToStr(Random(MaxInt));
  ForceDirectories(TempDir);
  Path := IncludeTrailingPathDelimiter(TempDir) + 's.json';
  try
    Ctx := TDWGZCADLoadContext.Create;
    try
      Ctx.RegisterShell($10, dokEntity, MakePtr($AA01), 0);
      AssertTrue(Ctx.Handles.TryGetMutable($10, Mut));
      Mut^.FixedType := DWG_TYPE_LINE;
      DWGWriteSummaryJson(Ctx, '/x/foo.dwg', Path);
      Text := ReadAllTextFileP2(Path);
      AssertTrue('fixed_types key', Pos('"fixed_types":', Text) > 0);
      AssertTrue('DWG_TYPE_LINE entry',
        Pos('"DWG_TYPE_LINE": 1', Text) > 0);
    finally
      Ctx.Free;
    end;
  finally
    if FileExists(Path) then
      DeleteFile(Path);
    RemoveDir(TempDir);
  end;
end;

{ ---------- TDWGLoadContextPendingIndexTest ---------- }

procedure TDWGLoadContextPendingIndexTest.OwnerLookupFindsAppendedItem;
var
  Ctx: TDWGZCADLoadContext;
  Pending: PDWGZCADPendingOwner;
  Entity: Pointer;
begin
  // Baseline: a single Append + lookup must return the same row.
  Ctx := TDWGZCADLoadContext.Create;
  try
    Entity := MakePtr($E1);
    Ctx.QueueOwnerResolve(Entity, $10, $20);
    Pending := Ctx.PendingOwners.ItemByEntityHandle($10);
    AssertNotNull('lookup finds queued owner', Pending);
    AssertEquals('entity round-trips through the index',
      PtrInt(Entity), PtrInt(Pending^.Entity));
    AssertEquals('owner handle preserved', Int64($20),
      Int64(Pending^.OwnerHandle));
  finally
    Ctx.Free;
  end;
end;

procedure TDWGLoadContextPendingIndexTest.OwnerLookupReturnsNilForMissingHandle;
var
  Ctx: TDWGZCADLoadContext;
begin
  Ctx := TDWGZCADLoadContext.Create;
  try
    Ctx.QueueOwnerResolve(MakePtr($E1), $10, $20);
    AssertNull('missing handle yields nil',
      Ctx.PendingOwners.ItemByEntityHandle($DEAD));
  finally
    Ctx.Free;
  end;
end;

procedure TDWGLoadContextPendingIndexTest.OwnerLookupReturnsFirstItemForRepeatedHandle;
var
  Ctx: TDWGZCADLoadContext;
  Pending: PDWGZCADPendingOwner;
  EntityA, EntityB: Pointer;
begin
  // Legacy FindByEntityHandle returned the first match. Production code
  // (resolver) relies on this so it can attribute the queued row to the
  // first Append even when a mapper re-queues the same handle later.
  Ctx := TDWGZCADLoadContext.Create;
  try
    EntityA := MakePtr($A1);
    EntityB := MakePtr($B2);
    Ctx.QueueOwnerResolve(EntityA, $10, $100);
    Ctx.QueueOwnerResolve(EntityB, $10, $200);
    AssertEquals('two rows accumulated', 2, Ctx.PendingOwners.Count);
    Pending := Ctx.PendingOwners.ItemByEntityHandle($10);
    AssertNotNull(Pending);
    AssertEquals('first-match contract preserved',
      PtrInt(EntityA), PtrInt(Pending^.Entity));
  finally
    Ctx.Free;
  end;
end;

procedure TDWGLoadContextPendingIndexTest.OwnerLookupHandlesOutOfOrderInsertion;
var
  Ctx: TDWGZCADLoadContext;
  Pending: PDWGZCADPendingOwner;
  E10, E5, E20, E1: Pointer;
begin
  // The sorted index must stay correct regardless of the handle order
  // entries arrive in. Binary search depends on the array being sorted.
  Ctx := TDWGZCADLoadContext.Create;
  try
    E10 := MakePtr($10A);
    E5  := MakePtr($5A);
    E20 := MakePtr($20A);
    E1  := MakePtr($1A);
    Ctx.QueueOwnerResolve(E10, $10, $F0);
    Ctx.QueueOwnerResolve(E5,  $5,  $F0);
    Ctx.QueueOwnerResolve(E20, $20, $F0);
    Ctx.QueueOwnerResolve(E1,  $1,  $F0);

    Pending := Ctx.PendingOwners.ItemByEntityHandle($1);
    AssertNotNull(Pending); AssertEquals(PtrInt(E1), PtrInt(Pending^.Entity));
    Pending := Ctx.PendingOwners.ItemByEntityHandle($5);
    AssertNotNull(Pending); AssertEquals(PtrInt(E5), PtrInt(Pending^.Entity));
    Pending := Ctx.PendingOwners.ItemByEntityHandle($10);
    AssertNotNull(Pending); AssertEquals(PtrInt(E10), PtrInt(Pending^.Entity));
    Pending := Ctx.PendingOwners.ItemByEntityHandle($20);
    AssertNotNull(Pending); AssertEquals(PtrInt(E20), PtrInt(Pending^.Entity));
  finally
    Ctx.Free;
  end;
end;

procedure TDWGLoadContextPendingIndexTest.OwnerClearResetsBothItemsAndIndex;
var
  List: TDWGZCADPendingOwnerList;
  Handles: array[0..0] of TDWGZCADHandle;
begin
  // Clear must empty both FItems and FIndex - if it forgot the index, a
  // subsequent Append followed by a lookup would chase a stale ItemIdx.
  List := TDWGZCADPendingOwnerList.Create;
  try
    Handles[0] := $F0;
    List.AppendCandidates(MakePtr($1), $10, Handles, 1, nil, -1);
    AssertEquals(1, List.Count);
    AssertNotNull(List.ItemByEntityHandle($10));
    List.Clear;
    AssertEquals('items cleared', 0, List.Count);
    AssertNull('index cleared', List.ItemByEntityHandle($10));
    // Re-populate with a different handle and verify lookups still resolve.
    Handles[0] := $F1;
    List.AppendCandidates(MakePtr($2), $20, Handles, 1, nil, -1);
    AssertNull('old handle still missing', List.ItemByEntityHandle($10));
    AssertNotNull('new handle reachable', List.ItemByEntityHandle($20));
  finally
    List.Free;
  end;
end;

procedure TDWGLoadContextPendingIndexTest.RefLookupFindsAppendedItem;
var
  Ctx: TDWGZCADLoadContext;
  Pending: PDWGZCADPendingRef;
  Entity, Layer: Pointer;
begin
  Ctx := TDWGZCADLoadContext.Create;
  try
    Entity := MakePtr($E1);
    Layer := MakePtr($A1);
    Ctx.RegisterShell($10, dokEntity, Entity, 0);
    Ctx.RegisterShell($20, dokLayer, Layer, 1);
    Ctx.QueueRefResolve(Entity, $10, $20, dokLayer, rsLayer, nil);
    Pending := Ctx.PendingRefs.ItemByEntityAndSlot($10, rsLayer);
    AssertNotNull('lookup finds queued ref', Pending);
    AssertEquals('ref handle preserved',
      Int64($20), Int64(Pending^.RefHandle));
    AssertEquals('slot preserved',
      Ord(rsLayer), Ord(Pending^.Slot));
  finally
    Ctx.Free;
  end;
end;

procedure TDWGLoadContextPendingIndexTest.RefLookupReturnsNilForMissingKey;
var
  Ctx: TDWGZCADLoadContext;
begin
  Ctx := TDWGZCADLoadContext.Create;
  try
    Ctx.RegisterShell($10, dokEntity, MakePtr($E1), 0);
    Ctx.QueueRefResolve(MakePtr($E1), $10, 0, dokLayer, rsLayer, nil);
    AssertNull('missing handle yields nil',
      Ctx.PendingRefs.ItemByEntityAndSlot($DEAD, rsLayer));
    AssertNull('right handle, missing slot yields nil',
      Ctx.PendingRefs.ItemByEntityAndSlot($10, rsLineType));
  finally
    Ctx.Free;
  end;
end;

procedure TDWGLoadContextPendingIndexTest.RefLookupKeysSeparatelyOnSlot;
var
  Ctx: TDWGZCADLoadContext;
  PendingL, PendingT: PDWGZCADPendingRef;
  Entity, Layer, LType: Pointer;
begin
  // The same entity handle with two different ref slots must land in two
  // independent index entries. Looking up by slot must return the right one.
  Ctx := TDWGZCADLoadContext.Create;
  try
    Entity := MakePtr($E1);
    Layer  := MakePtr($A1);
    LType  := MakePtr($B1);
    Ctx.RegisterShell($10, dokEntity,   Entity, 0);
    Ctx.RegisterShell($20, dokLayer,    Layer,  1);
    Ctx.RegisterShell($30, dokLineType, LType,  2);
    Ctx.QueueRefResolve(Entity, $10, $20, dokLayer,    rsLayer,    nil);
    Ctx.QueueRefResolve(Entity, $10, $30, dokLineType, rsLineType, nil);
    AssertEquals('two distinct (handle, slot) rows',
      2, Ctx.PendingRefs.Count);
    PendingL := Ctx.PendingRefs.ItemByEntityAndSlot($10, rsLayer);
    PendingT := Ctx.PendingRefs.ItemByEntityAndSlot($10, rsLineType);
    AssertNotNull(PendingL);
    AssertNotNull(PendingT);
    AssertEquals('layer slot points at layer handle',
      Int64($20), Int64(PendingL^.RefHandle));
    AssertEquals('linetype slot points at linetype handle',
      Int64($30), Int64(PendingT^.RefHandle));
  finally
    Ctx.Free;
  end;
end;

procedure TDWGLoadContextPendingIndexTest.RefAppendOrReplaceReusesExistingItemIndex;
var
  Ctx: TDWGZCADLoadContext;
  Pending: PDWGZCADPendingRef;
  Entity, LayerA, LayerB: Pointer;
begin
  // Re-queue for the same (handle, slot) replaces the row in place. The
  // index entry must still point at the same FItems position.
  Ctx := TDWGZCADLoadContext.Create;
  try
    Entity := MakePtr($E1);
    LayerA := MakePtr($A1);
    LayerB := MakePtr($A2);
    Ctx.RegisterShell($10, dokEntity, Entity, 0);
    Ctx.RegisterShell($20, dokLayer,  LayerA, 1);
    Ctx.RegisterShell($21, dokLayer,  LayerB, 2);
    Ctx.QueueRefResolve(Entity, $10, $20, dokLayer, rsLayer, nil);
    Ctx.QueueRefResolve(Entity, $10, $21, dokLayer, rsLayer, nil);
    AssertEquals('replace - no new row', 1, Ctx.PendingRefs.Count);
    Pending := Ctx.PendingRefs.ItemByEntityAndSlot($10, rsLayer);
    AssertNotNull(Pending);
    AssertEquals('latest queue wins',
      Int64($21), Int64(Pending^.RefHandle));
  finally
    Ctx.Free;
  end;
end;

procedure TDWGLoadContextPendingIndexTest.RefAppendOrReplaceDoesNotGrowIndexForReplace;
var
  List: TDWGZCADPendingRefList;
  Idx1, Idx2: Integer;
  Handles: array[0..0] of TDWGZCADHandle;
begin
  // The replace branch must return the existing item index and must not
  // append a stale index entry. Verified through Count + the returned index.
  List := TDWGZCADPendingRefList.Create;
  try
    Handles[0] := $20;
    Idx1 := List.AppendOrReplaceCandidates(MakePtr($E1), $10, Handles, 1,
      dokLayer, rsLayer, nil, False);
    Handles[0] := $21;
    Idx2 := List.AppendOrReplaceCandidates(MakePtr($E1), $10, Handles, 1,
      dokLayer, rsLayer, nil, False);
    AssertEquals('same item slot reused', Idx1, Idx2);
    AssertEquals('no extra items appended', 1, List.Count);
  finally
    List.Free;
  end;
end;

procedure TDWGLoadContextPendingIndexTest.RefClearResetsBothItemsAndIndex;
var
  List: TDWGZCADPendingRefList;
  Handles: array[0..0] of TDWGZCADHandle;
begin
  List := TDWGZCADPendingRefList.Create;
  try
    Handles[0] := $20;
    List.AppendOrReplaceCandidates(MakePtr($E1), $10, Handles, 1,
      dokLayer, rsLayer, nil, False);
    AssertEquals(1, List.Count);
    AssertNotNull(List.ItemByEntityAndSlot($10, rsLayer));
    List.Clear;
    AssertEquals('items cleared', 0, List.Count);
    AssertNull('index cleared', List.ItemByEntityAndSlot($10, rsLayer));
    // Re-populate with a different (handle, slot) and verify it's reachable.
    Handles[0] := $21;
    List.AppendOrReplaceCandidates(MakePtr($E2), $11, Handles, 1,
      dokLineType, rsLineType, nil, False);
    AssertNull('old key still missing',
      List.ItemByEntityAndSlot($10, rsLayer));
    AssertNotNull('new key reachable',
      List.ItemByEntityAndSlot($11, rsLineType));
  finally
    List.Free;
  end;
end;

procedure TDWGLoadContextPendingIndexTest.RefLookupHandlesOutOfOrderInsertion;
var
  List: TDWGZCADPendingRefList;
  Handles: array[0..0] of TDWGZCADHandle;
  Pending: PDWGZCADPendingRef;
begin
  // The composite (Handle, Slot) index must keep entries sorted regardless
  // of insertion order. Insert four rows in shuffled order and verify each
  // is reachable.
  List := TDWGZCADPendingRefList.Create;
  try
    Handles[0] := $100;
    List.AppendOrReplaceCandidates(MakePtr($1), $50, Handles, 1,
      dokLayer, rsLayer, nil, False);
    Handles[0] := $101;
    List.AppendOrReplaceCandidates(MakePtr($2), $10, Handles, 1,
      dokLineType, rsLineType, nil, False);
    Handles[0] := $102;
    List.AppendOrReplaceCandidates(MakePtr($3), $30, Handles, 1,
      dokLayer, rsLayer, nil, False);
    Handles[0] := $103;
    List.AppendOrReplaceCandidates(MakePtr($4), $10, Handles, 1,
      dokLayer, rsLayer, nil, False);
    AssertEquals(4, List.Count);

    Pending := List.ItemByEntityAndSlot($10, rsLayer);
    AssertNotNull(Pending);
    AssertEquals(Int64($103), Int64(Pending^.RefHandle));
    Pending := List.ItemByEntityAndSlot($10, rsLineType);
    AssertNotNull(Pending);
    AssertEquals(Int64($101), Int64(Pending^.RefHandle));
    Pending := List.ItemByEntityAndSlot($30, rsLayer);
    AssertNotNull(Pending);
    AssertEquals(Int64($102), Int64(Pending^.RefHandle));
    Pending := List.ItemByEntityAndSlot($50, rsLayer);
    AssertNotNull(Pending);
    AssertEquals(Int64($100), Int64(Pending^.RefHandle));
  finally
    List.Free;
  end;
end;

procedure TDWGLoadContextPendingIndexTest.QueueOwnerResolveIntegrationKeepsItemReachable;
var
  Ctx: TDWGZCADLoadContext;
  Pending: PDWGZCADPendingOwner;
  Entity, Block: Pointer;
begin
  // End-to-end: queue the owner, run ResolveOwners, then look up the row
  // via FindPendingOwner. The indexed lookup must still hand back the row
  // updated by the resolver.
  Ctx := TDWGZCADLoadContext.Create;
  try
    Entity := MakePtr($E1);
    Block  := MakePtr($B1);
    Ctx.RegisterShell($10, dokEntity,   Entity, 0);
    Ctx.RegisterShell($20, dokBlockDef, Block,  1);
    Ctx.QueueOwnerResolve(Entity, $10, $20);
    Ctx.ResolveOwners;
    Pending := Ctx.FindPendingOwner($10);
    AssertNotNull('row reachable via host-level lookup', Pending);
    AssertEquals('owner attached by resolver',
      PtrInt(Block), PtrInt(Pending^.AttachedOwner));
  finally
    Ctx.Free;
  end;
end;

procedure TDWGLoadContextPendingIndexTest.QueueRefResolveIntegrationKeepsItemReachable;
var
  Ctx: TDWGZCADLoadContext;
  Pending: PDWGZCADPendingRef;
  Entity, Layer: Pointer;
begin
  Ctx := TDWGZCADLoadContext.Create;
  try
    Entity := MakePtr($E1);
    Layer  := MakePtr($A1);
    Ctx.RegisterShell($10, dokEntity, Entity, 0);
    Ctx.RegisterShell($20, dokLayer,  Layer,  1);
    Ctx.QueueRefResolve(Entity, $10, $20, dokLayer, rsLayer, nil);
    Ctx.ResolveRefs;
    Pending := Ctx.FindPendingRef($10, rsLayer);
    AssertNotNull('row reachable via host-level lookup', Pending);
    AssertEquals('ref attached by resolver',
      PtrInt(Layer), PtrInt(Pending^.AttachedRef));
  finally
    Ctx.Free;
  end;
end;

{ ---------- TDWGLoadContextTextStyleNameTest (Issue #1198 P6) ---------- }

procedure TDWGLoadContextTextStyleNameTest.BaseNameUsesFontFileForShapeStyles;
begin
  // Shape styles (used inside LTYPE patterns) carry their font file as the
  // identity; the rule is preserved from the legacy code so existing linetype
  // shapes keep resolving to the same pstyle after the refactor.
  AssertEquals('shape with fontfile uses fontfile as base name',
    'ltshp.shx',
    DWGTextStyleBaseName('Custom', 'ltshp.shx', True, $42));
end;

procedure TDWGLoadContextTextStyleNameTest.BaseNameUsesDecodedNameWhenPresent;
begin
  // Real text styles (IsShape=False) prefer their decoded name. Handle is
  // available but irrelevant on this path — we only fall back to it when the
  // name decoded to an empty string.
  AssertEquals('non-empty name wins over handle fallback',
    'MyStyle',
    DWGTextStyleBaseName('MyStyle', 'arial.ttf', False, $99));
end;

procedure TDWGLoadContextTextStyleNameTest.BaseNameFallsBackToHandleHexWhenNameEmpty;
begin
  // Empty decoded name + non-zero handle: the legacy code lost the original
  // identity by renaming to 'Standard'. The new rule keeps the handle hex
  // so distinct DWG entries with empty names stay distinct in ZCAD.
  AssertEquals('empty name falls back to handle-derived placeholder',
    'dwg_2A',
    DWGTextStyleBaseName('', '', False, $2A));
end;

procedure TDWGLoadContextTextStyleNameTest.BaseNameFallsBackToStandardWhenHandleZero;
begin
  // Defensive guard: when there is no handle either, behaviour reverts to
  // the legacy 'Standard' fallback so synthetic test fixtures or future
  // handle-less callers stay deterministic.
  AssertEquals('zero handle still uses Standard',
    'Standard',
    DWGTextStyleBaseName('', '', False, 0));
end;

procedure TDWGLoadContextTextStyleNameTest.UniquifyAppendsHandleHexSuffix;
begin
  // Collision rename uses the handle hex as a suffix so re-importing the
  // same DWG produces the same name (stable across runs).
  AssertEquals('uniquify appends handle hex suffix',
    'Roman_dwg10',
    DWGTextStyleUniquifyName('Roman', $10));
end;

procedure TDWGLoadContextTextStyleNameTest.UniquifyHandlesZeroHandleDefensively;
begin
  // Handle=0 should still uniquify (callers must never get the same name
  // back as the base) — we emit '_dwg0'.
  AssertEquals('zero handle uniquify emits _dwg0 suffix',
    'A_dwg0',
    DWGTextStyleUniquifyName('A', 0));
end;

procedure TDWGLoadContextTextStyleNameTest.PtrOwnedReturnsFalseForUnregisteredPointer;
var
  Ctx: TDWGZCADLoadContext;
  Ptr: Pointer;
begin
  // No other handle has registered this pointer yet, so the collision
  // detector must return False (caller proceeds with original name).
  Ctx := TDWGZCADLoadContext.Create;
  try
    Ptr := MakePtr($AA);
    AssertFalse('empty registry reports no collision',
      DWGTextStylePtrOwnedByAnotherHandle(Ctx, Ptr, $100));
  finally
    Ctx.Free;
  end;
end;

procedure TDWGLoadContextTextStyleNameTest.PtrOwnedReturnsFalseForSameHandleReregistration;
var
  Ctx: TDWGZCADLoadContext;
  Ptr: Pointer;
begin
  // A handle re-registering itself (e.g. mapper invoked twice for the same
  // STYLE record) must NOT be reported as a collision — that would force a
  // spurious rename. The check filters on Entry.Handle <> ANewHandle.
  Ctx := TDWGZCADLoadContext.Create;
  try
    Ptr := MakePtr($BB);
    AssertTrue('seed registration succeeds',
      Ctx.RegisterShell($100, dokTextStyle, Ptr, 0));
    AssertFalse('same handle does not count as collision',
      DWGTextStylePtrOwnedByAnotherHandle(Ctx, Ptr, $100));
  finally
    Ctx.Free;
  end;
end;

procedure TDWGLoadContextTextStyleNameTest.PtrOwnedReturnsTrueWhenAnotherHandleClaimsPointer;
var
  Ctx: TDWGZCADLoadContext;
  Ptr: Pointer;
begin
  // Two distinct handles cannot share the same pstyle — this is the trigger
  // for the uniquify path in AddTextStyle. The detector must spot it.
  Ctx := TDWGZCADLoadContext.Create;
  try
    Ptr := MakePtr($CC);
    AssertTrue('seed registration for first handle',
      Ctx.RegisterShell($200, dokTextStyle, Ptr, 0));
    AssertTrue('second handle on same pstyle reported as collision',
      DWGTextStylePtrOwnedByAnotherHandle(Ctx, Ptr, $201));
  finally
    Ctx.Free;
  end;
end;

procedure TDWGLoadContextTextStyleNameTest.PtrOwnedIgnoresEntriesOfOtherKinds;
var
  Ctx: TDWGZCADLoadContext;
  Ptr: Pointer;
begin
  // The detector must filter on Entry.Kind = dokTextStyle. A different kind
  // pointing at the same address (theoretically rare but possible in tests)
  // must NOT trip the uniquify path, otherwise unrelated tables would force
  // textstyle renames.
  Ctx := TDWGZCADLoadContext.Create;
  try
    Ptr := MakePtr($DD);
    AssertTrue('seed dokLayer registration on shared ptr',
      Ctx.RegisterShell($300, dokLayer, Ptr, 0));
    AssertFalse('non-textstyle entry does not count as collision',
      DWGTextStylePtrOwnedByAnotherHandle(Ctx, Ptr, $301));
  finally
    Ctx.Free;
  end;
end;

procedure TDWGLoadContextTextStyleNameTest.PtrOwnedReturnsFalseForNilContextOrPtr;
var
  Ctx: TDWGZCADLoadContext;
begin
  // Defensive guards: AddTextStyle calls the detector unconditionally when
  // the load context is wired in. Both a nil context and a nil candidate
  // pointer must short-circuit to False so the caller proceeds normally.
  AssertFalse('nil context short-circuits to false',
    DWGTextStylePtrOwnedByAnotherHandle(nil, MakePtr($EE), $400));
  Ctx := TDWGZCADLoadContext.Create;
  try
    AssertFalse('nil candidate pointer short-circuits to false',
      DWGTextStylePtrOwnedByAnotherHandle(Ctx, nil, $400));
  finally
    Ctx.Free;
  end;
end;

initialization
  RegisterTests([TDWGLoadContextHandleMapTest, TDWGLoadContextResolveTest,
    TDWGLoadContextRefTest, TDWGLoadContextBlockTest,
    TDWGLoadContextTextStyleRefTest, TDWGLoadContextStage6Test,
    TDWGLoadContextSilentFallbackTest,
    TDWGLoadContextWarningAggregateTest,
    TDWGLoadContextSideFilesTest,
    TDWGLoadContextFixedTypeTest,
    TDWGLoadContextPendingIndexTest,
    TDWGLoadContextTextStyleNameTest]);

end.
