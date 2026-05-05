{*************************************************************************** }
{  fpdwg - DWG to ZCAD import context (Stage 2)                              }
{                                                                            }
{        Copyright (C) 2026 Andrey Zubarev <zamtmn@yandex.ru>                }
{                                                                            }
{  This library is free software, licensed under the terms of the GNU        }
{  General Public License as published by the Free Software Foundation,      }
{  either version 3 of the License, or (at your option) any later version.   }
{*************************************************************************** }

{ Stage 2 of TZ_DWG_LOAD_TO_ZCAD: a small import context that decouples DWG
  read order from ZCAD attachment. The unit intentionally avoids any ZCAD
  entity dependency so it can be unit tested in fpdwg_tests against opaque
  pointers / fake owner callbacks. The actual ZCAD-specific allocation,
  AddMi and BuildGeometry calls live in uzefflibredwg2ents.pas. }

unit uzedwgloadcontext;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  SysUtils;

type
  TDWGZCADHandle = QWord;

  { Section 7 of TZ: object kinds the import context recognizes. The kind is
    used by the resolver to decide whether a handle is a valid container
    target. The set is intentionally small; new kinds get added as new entity
    units come online. }
  TDWGZCADObjectKind = (
    dokUnknown,
    dokLayer,
    dokLineType,
    dokTextStyle,
    dokDimStyle,
    dokBlockDef,
    dokModelSpace,
    dokPaperSpace,
    dokContainer,
    dokEntity
  );

  TDWGShellState = (
    msUnseen,
    msCreating,
    msCreated,
    msSkipped,
    msFailed
  );

  TDWGAttachState = (
    asPending,
    asResolving,
    asAttached,
    asFallback,
    asSkipped
  );

  TDWGAttachReason = (
    arResolved,
    arNullOwner,
    arOwnerNotFound,
    arOwnerNotContainer,
    arSelfOwnerCycle,
    arOwnerChainCycle,
    arOwnerSkipped,
    arPending,
    { Stage 3: ref-only outcomes. The owner queue never produces these. }
    arRefNull,
    arRefNotFound,
    arRefKindMismatch
  );

  TDWGZCADHandleEntry = record
    Handle: TDWGZCADHandle;
    Kind: TDWGZCADObjectKind;
    Ptr: Pointer;
    RawIndex: Integer;
    ShellState: TDWGShellState;
  end;
  PDWGZCADHandleEntry = ^TDWGZCADHandleEntry;

  TDWGZCADPendingOwner = record
    Entity: Pointer;
    EntityHandle: TDWGZCADHandle;
    OwnerHandle: TDWGZCADHandle;
    FallbackOwner: Pointer;
    RawIndex: Integer;
    AttachState: TDWGAttachState;
    AttachReason: TDWGAttachReason;
    AttachedOwner: Pointer;
  end;
  PDWGZCADPendingOwner = ^TDWGZCADPendingOwner;

  { Stage 3 (TZ §12.3 / §8.2-8.3): queued visual reference. The owner queue
    handles "where does this entity attach". This queue handles "which
    layer / linetype / textstyle / dimstyle pointer does the entity get".
    Refs are resolved separately from owners because they come from
    different ZCAD tables and have a different fallback policy: a missing
    layer falls back to the system layer, a missing linetype falls back to
    the ByLayer entry. The Slot field tells the attach callback which vp
    field to write so the loader does not need a separate callback per
    reference kind. }
  TDWGZCADRefSlot = (
    rsLayer,
    rsLineType,
    rsTextStyle,
    rsDimStyle
  );

  TDWGZCADPendingRef = record
    Entity: Pointer;
    EntityHandle: TDWGZCADHandle;
    RefHandle: TDWGZCADHandle;
    ExpectedKind: TDWGZCADObjectKind;
    Slot: TDWGZCADRefSlot;
    Fallback: Pointer;
    AttachState: TDWGAttachState;
    AttachReason: TDWGAttachReason;
    AttachedRef: Pointer;
  end;
  PDWGZCADPendingRef = ^TDWGZCADPendingRef;

  TDWGImportSeverity = (wsInfo, wsWarning, wsError);

  TDWGImportWarning = record
    Severity: TDWGImportSeverity;
    Code: Integer;
    Handle: TDWGZCADHandle;
    Text: String;
  end;

  { Callback used by ResolveOwners to perform the actual ZCAD AddMi attachment.
    Owner is either the resolved owner pointer or FallbackOwner. The callback
    is allowed to be nil during pure-context unit tests; in that case the
    resolver still tracks the AttachState so tests can assert routing. }
  TDWGAttachProc = procedure(Entity: Pointer; Owner: Pointer;
    Reason: TDWGAttachReason; Data: Pointer);

  { Stage 3 callback used by ResolveRefs to write the resolved reference
    pointer (PGDBLayerProp / PGDBLtypeProp / PGDBTextStyle / ...) back into
    the entity's vp record. Slot tells the production code which field to
    update; Reason carries diagnostic context for fallback cases. As with
    TDWGAttachProc, the callback is optional so tests can assert routing
    without a real ZCAD entity. }
  TDWGRefAttachProc = procedure(Entity: Pointer; Ref: Pointer;
    Slot: TDWGZCADRefSlot; Reason: TDWGAttachReason; Data: Pointer);

  EDWGLoadContext = class(Exception);

  { Sorted map TDWGZCADHandle -> TDWGZCADHandleEntry. Same insertion strategy
    as inspector/fpdwg_registry: a sorted dynamic array with binary search.
    Keeping this simple avoids pulling FCL generics or a hashmap dependency
    that the inspector tests already proved unnecessary for thousands of
    handles. }
  TDWGZCADHandleMap = class
  private
    FEntries: array of TDWGZCADHandleEntry;
    function FindIndex(AHandle: TDWGZCADHandle; out Index: Integer): Boolean;
    procedure InsertAt(Index: Integer; const Entry: TDWGZCADHandleEntry);
  public
    function RegisterShell(AHandle: TDWGZCADHandle;
      AKind: TDWGZCADObjectKind; APtr: Pointer; ARawIndex: Integer;
      AState: TDWGShellState): Boolean;
    function TryGet(AHandle: TDWGZCADHandle;
      out Entry: TDWGZCADHandleEntry): Boolean;
    function TryGetMutable(AHandle: TDWGZCADHandle;
      out Entry: PDWGZCADHandleEntry): Boolean;
    function Count: Integer;
    function EntryAt(Index: Integer): PDWGZCADHandleEntry;
    procedure Clear;
  end;

  TDWGZCADPendingOwnerList = class
  private
    FItems: array of TDWGZCADPendingOwner;
    function FindByEntityHandle(AHandle: TDWGZCADHandle;
      out Index: Integer): Boolean;
  public
    function Append(AEntity: Pointer; AEntityHandle, AOwnerHandle: TDWGZCADHandle;
      AFallbackOwner: Pointer; ARawIndex: Integer): Integer;
    function ItemAt(Index: Integer): PDWGZCADPendingOwner;
    function ItemByEntityHandle(AHandle: TDWGZCADHandle): PDWGZCADPendingOwner;
    function Count: Integer;
    procedure Clear;
  end;

  { Stage 3 backing store for queued visual references. The same entity may
    have multiple refs (layer + linetype + style) so the list is keyed on
    (entity handle, slot); the production code is allowed to overwrite a
    previously queued ref for the same slot, which keeps the resolver
    deterministic when a mapper is called twice for the same handle. }
  TDWGZCADPendingRefList = class
  private
    FItems: array of TDWGZCADPendingRef;
    function FindByEntityAndSlot(AHandle: TDWGZCADHandle;
      ASlot: TDWGZCADRefSlot; out Index: Integer): Boolean;
  public
    function AppendOrReplace(AEntity: Pointer;
      AEntityHandle, ARefHandle: TDWGZCADHandle;
      AExpectedKind: TDWGZCADObjectKind; ASlot: TDWGZCADRefSlot;
      AFallback: Pointer): Integer;
    function ItemAt(Index: Integer): PDWGZCADPendingRef;
    function ItemByEntityAndSlot(AHandle: TDWGZCADHandle;
      ASlot: TDWGZCADRefSlot): PDWGZCADPendingRef;
    function Count: Integer;
    procedure Clear;
  end;

  { Section 7 / 5.4 of TZ: import-time context. Holds shell registry,
    pending owner queue, cycle-safe resolver stack, warnings and stats.
    The context has no knowledge of ZCAD entity classes; integration is done
    by the AttachProc callback configured by the production loader. }
  TDWGZCADLoadContext = class
  private
    FHandles: TDWGZCADHandleMap;
    FPendingOwners: TDWGZCADPendingOwnerList;
    FPendingRefs: TDWGZCADPendingRefList;
    FResolveStack: array of TDWGZCADHandle;
    FWarnings: array of TDWGImportWarning;
    FAttachProc: TDWGAttachProc;
    FAttachData: Pointer;
    FRefAttachProc: TDWGRefAttachProc;
    FRefAttachData: Pointer;
    FFallbackOwner: Pointer;
    FFallbackLayer: Pointer;
    FFallbackLineType: Pointer;
    FFallbackTextStyle: Pointer;
    FAttachCount: Integer;
    FFallbackCount: Integer;
    FCycleCount: Integer;
    FRefAttachCount: Integer;
    FRefFallbackCount: Integer;
    procedure PushStack(Handle: TDWGZCADHandle);
    procedure PopStack;
    function StackContains(Handle: TDWGZCADHandle): Boolean;
    procedure AddWarning(Severity: TDWGImportSeverity; Code: Integer;
      Handle: TDWGZCADHandle; const Text: String);
    procedure DoAttach(Pending: PDWGZCADPendingOwner; Owner: Pointer;
      State: TDWGAttachState; Reason: TDWGAttachReason);
    procedure ResolvePending(Pending: PDWGZCADPendingOwner);
    procedure DoRefAttach(Pending: PDWGZCADPendingRef; Ref: Pointer;
      State: TDWGAttachState; Reason: TDWGAttachReason);
    procedure ResolveRef(Pending: PDWGZCADPendingRef);
    function FallbackForSlot(ASlot: TDWGZCADRefSlot): Pointer;
  public
    constructor Create;
    destructor Destroy; override;

    { Configuration }
    procedure SetAttachProc(AProc: TDWGAttachProc; AData: Pointer = nil);
    procedure SetRefAttachProc(AProc: TDWGRefAttachProc;
      AData: Pointer = nil);
    procedure SetFallbackOwner(AOwner: Pointer);
    procedure SetFallbackLayer(ALayer: Pointer);
    procedure SetFallbackLineType(ALineType: Pointer);
    procedure SetFallbackTextStyle(ATextStyle: Pointer);

    { Phase 2: Shell registration }
    function RegisterShell(AHandle: TDWGZCADHandle;
      AKind: TDWGZCADObjectKind; APtr: Pointer; ARawIndex: Integer = -1
      ): Boolean;
    function MarkShellState(AHandle: TDWGZCADHandle;
      AState: TDWGShellState): Boolean;

    { Phase 2.5: Pending owner queue }
    procedure QueueOwnerResolve(AEntity: Pointer;
      AEntityHandle, AOwnerHandle: TDWGZCADHandle;
      AFallbackOwner: Pointer = nil; ARawIndex: Integer = -1);

    { Phase 2.6 (Stage 3): Pending visual reference queue. ASlot tells the
      attach callback which vp field to write; AExpectedKind is checked when
      the ref handle resolves so a layer cannot end up in the linetype slot.
      Passing 0 for ARefHandle means "no ref provided"; the resolver will
      route the entity directly to the registered fallback. }
    procedure QueueRefResolve(AEntity: Pointer;
      AEntityHandle, ARefHandle: TDWGZCADHandle;
      AExpectedKind: TDWGZCADObjectKind; ASlot: TDWGZCADRefSlot;
      AFallback: Pointer = nil);

    { Phase 3: Resolve owners (cycle-safe). Idempotent: a second call leaves
      already-attached entities alone. }
    procedure ResolveOwners;

    { Phase 3.5 (Stage 3): Resolve queued visual references. Idempotent:
      every pending ref is processed once and a second call is a no-op. }
    procedure ResolveRefs;

    { Lookup helpers }
    function TryGetEntry(AHandle: TDWGZCADHandle;
      out Entry: TDWGZCADHandleEntry): Boolean;
    function FindPending(AEntityHandle: TDWGZCADHandle
      ): PDWGZCADPendingOwner;
    function FindPendingRef(AEntityHandle: TDWGZCADHandle;
      ASlot: TDWGZCADRefSlot): PDWGZCADPendingRef;

    { Diagnostics }
    function WarningCount: Integer;
    function WarningAt(Index: Integer): TDWGImportWarning;
    property AttachCount: Integer read FAttachCount;
    property FallbackCount: Integer read FFallbackCount;
    property CycleCount: Integer read FCycleCount;
    property RefAttachCount: Integer read FRefAttachCount;
    property RefFallbackCount: Integer read FRefFallbackCount;
    property FallbackOwner: Pointer read FFallbackOwner;
    property FallbackLayer: Pointer read FFallbackLayer;
    property FallbackLineType: Pointer read FFallbackLineType;
    property FallbackTextStyle: Pointer read FFallbackTextStyle;
    property Handles: TDWGZCADHandleMap read FHandles;
    property PendingOwners: TDWGZCADPendingOwnerList read FPendingOwners;
    property PendingRefs: TDWGZCADPendingRefList read FPendingRefs;
  end;

const
  { Diagnostic codes; kept as integer constants so log filters can use them
    without importing this unit's enum. Numbering picks up where the inspector
    logger left off (1300 range used for resolver). }
  DWG_WARN_OWNER_NULL          = 1401;
  DWG_WARN_OWNER_NOT_FOUND     = 1402;
  DWG_WARN_OWNER_NOT_CONTAINER = 1403;
  DWG_WARN_OWNER_SELF_CYCLE    = 1404;
  DWG_WARN_OWNER_CHAIN_CYCLE   = 1405;
  DWG_WARN_OWNER_SKIPPED       = 1406;
  DWG_WARN_DUPLICATE_HANDLE    = 1407;
  { Stage 3 (TZ §12.3 / §13): visual-reference resolution diagnostics. The
    spec calls for separate codes per failure mode so a log filter can spot
    e.g. layer-not-found independently of linetype-kind-mismatch. }
  DWG_WARN_REF_NULL            = 1408;
  DWG_WARN_REF_NOT_FOUND       = 1409;
  DWG_WARN_REF_KIND_MISMATCH   = 1410;

function DWGAttachReasonToText(Reason: TDWGAttachReason): String;

implementation

function DWGAttachReasonToText(Reason: TDWGAttachReason): String;
begin
  case Reason of
    arResolved:           Result := 'resolved';
    arNullOwner:          Result := 'null owner';
    arOwnerNotFound:      Result := 'owner not found';
    arOwnerNotContainer:  Result := 'owner is not a container';
    arSelfOwnerCycle:     Result := 'self owner cycle';
    arOwnerChainCycle:    Result := 'owner chain cycle';
    arOwnerSkipped:       Result := 'owner skipped';
    arPending:            Result := 'pending';
    arRefNull:            Result := 'null ref';
    arRefNotFound:        Result := 'ref not found';
    arRefKindMismatch:    Result := 'ref kind mismatch';
  else
    Result := 'unknown';
  end;
end;

{ ---------- TDWGZCADHandleMap ---------- }

function TDWGZCADHandleMap.FindIndex(AHandle: TDWGZCADHandle;
  out Index: Integer): Boolean;
var
  L, H, M: Integer;
begin
  Result := False;
  L := 0;
  H := High(FEntries);
  while L <= H do
  begin
    M := L + (H - L) div 2;
    if FEntries[M].Handle = AHandle then
    begin
      Index := M;
      Exit(True);
    end
    else if FEntries[M].Handle < AHandle then
      L := M + 1
    else
      H := M - 1;
  end;
  Index := L;
end;

procedure TDWGZCADHandleMap.InsertAt(Index: Integer;
  const Entry: TDWGZCADHandleEntry);
var
  I: Integer;
begin
  SetLength(FEntries, Length(FEntries) + 1);
  for I := High(FEntries) downto Index + 1 do
    FEntries[I] := FEntries[I - 1];
  FEntries[Index] := Entry;
end;

function TDWGZCADHandleMap.RegisterShell(AHandle: TDWGZCADHandle;
  AKind: TDWGZCADObjectKind; APtr: Pointer; ARawIndex: Integer;
  AState: TDWGShellState): Boolean;
var
  Index: Integer;
  Entry: TDWGZCADHandleEntry;
begin
  if FindIndex(AHandle, Index) then
    Exit(False);
  Entry.Handle := AHandle;
  Entry.Kind := AKind;
  Entry.Ptr := APtr;
  Entry.RawIndex := ARawIndex;
  Entry.ShellState := AState;
  InsertAt(Index, Entry);
  Result := True;
end;

function TDWGZCADHandleMap.TryGet(AHandle: TDWGZCADHandle;
  out Entry: TDWGZCADHandleEntry): Boolean;
var
  Index: Integer;
begin
  Result := FindIndex(AHandle, Index);
  if Result then
    Entry := FEntries[Index];
end;

function TDWGZCADHandleMap.TryGetMutable(AHandle: TDWGZCADHandle;
  out Entry: PDWGZCADHandleEntry): Boolean;
var
  Index: Integer;
begin
  Entry := nil;
  Result := FindIndex(AHandle, Index);
  if Result then
    Entry := @FEntries[Index];
end;

function TDWGZCADHandleMap.Count: Integer;
begin
  Result := Length(FEntries);
end;

function TDWGZCADHandleMap.EntryAt(Index: Integer): PDWGZCADHandleEntry;
begin
  if (Index < 0) or (Index > High(FEntries)) then
    raise EDWGLoadContext.CreateFmt('Handle map index %d out of range', [Index]);
  Result := @FEntries[Index];
end;

procedure TDWGZCADHandleMap.Clear;
begin
  SetLength(FEntries, 0);
end;

{ ---------- TDWGZCADPendingOwnerList ---------- }

function TDWGZCADPendingOwnerList.FindByEntityHandle(AHandle: TDWGZCADHandle;
  out Index: Integer): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(FItems) do
    if FItems[I].EntityHandle = AHandle then
    begin
      Index := I;
      Exit(True);
    end;
  Index := -1;
  Result := False;
end;

function TDWGZCADPendingOwnerList.Append(AEntity: Pointer;
  AEntityHandle, AOwnerHandle: TDWGZCADHandle; AFallbackOwner: Pointer;
  ARawIndex: Integer): Integer;
var
  Item: TDWGZCADPendingOwner;
begin
  FillChar(Item, SizeOf(Item), 0);
  Item.Entity := AEntity;
  Item.EntityHandle := AEntityHandle;
  Item.OwnerHandle := AOwnerHandle;
  Item.FallbackOwner := AFallbackOwner;
  Item.RawIndex := ARawIndex;
  Item.AttachState := asPending;
  Item.AttachReason := arPending;
  Item.AttachedOwner := nil;
  SetLength(FItems, Length(FItems) + 1);
  FItems[High(FItems)] := Item;
  Result := High(FItems);
end;

function TDWGZCADPendingOwnerList.ItemAt(Index: Integer
  ): PDWGZCADPendingOwner;
begin
  if (Index < 0) or (Index > High(FItems)) then
    raise EDWGLoadContext.CreateFmt('Pending owner index %d out of range',
      [Index]);
  Result := @FItems[Index];
end;

function TDWGZCADPendingOwnerList.ItemByEntityHandle(AHandle: TDWGZCADHandle
  ): PDWGZCADPendingOwner;
var
  Index: Integer;
begin
  if FindByEntityHandle(AHandle, Index) then
    Result := @FItems[Index]
  else
    Result := nil;
end;

function TDWGZCADPendingOwnerList.Count: Integer;
begin
  Result := Length(FItems);
end;

procedure TDWGZCADPendingOwnerList.Clear;
begin
  SetLength(FItems, 0);
end;

{ ---------- TDWGZCADPendingRefList ---------- }

function TDWGZCADPendingRefList.FindByEntityAndSlot(AHandle: TDWGZCADHandle;
  ASlot: TDWGZCADRefSlot; out Index: Integer): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(FItems) do
    if (FItems[I].EntityHandle = AHandle) and (FItems[I].Slot = ASlot) then
    begin
      Index := I;
      Exit(True);
    end;
  Index := -1;
  Result := False;
end;

function TDWGZCADPendingRefList.AppendOrReplace(AEntity: Pointer;
  AEntityHandle, ARefHandle: TDWGZCADHandle;
  AExpectedKind: TDWGZCADObjectKind; ASlot: TDWGZCADRefSlot;
  AFallback: Pointer): Integer;
var
  Existing: Integer;
  Item: TDWGZCADPendingRef;
begin
  FillChar(Item, SizeOf(Item), 0);
  Item.Entity := AEntity;
  Item.EntityHandle := AEntityHandle;
  Item.RefHandle := ARefHandle;
  Item.ExpectedKind := AExpectedKind;
  Item.Slot := ASlot;
  Item.Fallback := AFallback;
  Item.AttachState := asPending;
  Item.AttachReason := arPending;
  Item.AttachedRef := nil;
  if FindByEntityAndSlot(AEntityHandle, ASlot, Existing) then
  begin
    FItems[Existing] := Item;
    Result := Existing;
  end
  else
  begin
    SetLength(FItems, Length(FItems) + 1);
    FItems[High(FItems)] := Item;
    Result := High(FItems);
  end;
end;

function TDWGZCADPendingRefList.ItemAt(Index: Integer): PDWGZCADPendingRef;
begin
  if (Index < 0) or (Index > High(FItems)) then
    raise EDWGLoadContext.CreateFmt('Pending ref index %d out of range',
      [Index]);
  Result := @FItems[Index];
end;

function TDWGZCADPendingRefList.ItemByEntityAndSlot(AHandle: TDWGZCADHandle;
  ASlot: TDWGZCADRefSlot): PDWGZCADPendingRef;
var
  Index: Integer;
begin
  if FindByEntityAndSlot(AHandle, ASlot, Index) then
    Result := @FItems[Index]
  else
    Result := nil;
end;

function TDWGZCADPendingRefList.Count: Integer;
begin
  Result := Length(FItems);
end;

procedure TDWGZCADPendingRefList.Clear;
begin
  SetLength(FItems, 0);
end;

{ ---------- TDWGZCADLoadContext ---------- }

constructor TDWGZCADLoadContext.Create;
begin
  inherited Create;
  FHandles := TDWGZCADHandleMap.Create;
  FPendingOwners := TDWGZCADPendingOwnerList.Create;
  FPendingRefs := TDWGZCADPendingRefList.Create;
end;

destructor TDWGZCADLoadContext.Destroy;
begin
  FHandles.Free;
  FPendingOwners.Free;
  FPendingRefs.Free;
  inherited Destroy;
end;

procedure TDWGZCADLoadContext.SetAttachProc(AProc: TDWGAttachProc;
  AData: Pointer);
begin
  FAttachProc := AProc;
  FAttachData := AData;
end;

procedure TDWGZCADLoadContext.SetRefAttachProc(AProc: TDWGRefAttachProc;
  AData: Pointer);
begin
  FRefAttachProc := AProc;
  FRefAttachData := AData;
end;

procedure TDWGZCADLoadContext.SetFallbackOwner(AOwner: Pointer);
begin
  FFallbackOwner := AOwner;
end;

procedure TDWGZCADLoadContext.SetFallbackLayer(ALayer: Pointer);
begin
  FFallbackLayer := ALayer;
end;

procedure TDWGZCADLoadContext.SetFallbackLineType(ALineType: Pointer);
begin
  FFallbackLineType := ALineType;
end;

procedure TDWGZCADLoadContext.SetFallbackTextStyle(ATextStyle: Pointer);
begin
  FFallbackTextStyle := ATextStyle;
end;

function TDWGZCADLoadContext.RegisterShell(AHandle: TDWGZCADHandle;
  AKind: TDWGZCADObjectKind; APtr: Pointer; ARawIndex: Integer): Boolean;
begin
  Result := FHandles.RegisterShell(AHandle, AKind, APtr, ARawIndex, msCreated);
  if not Result then
    AddWarning(wsWarning, DWG_WARN_DUPLICATE_HANDLE, AHandle,
      Format('Duplicate handle %s ignored; first shell remains indexed',
        [IntToHex(AHandle, 1)]));
end;

function TDWGZCADLoadContext.MarkShellState(AHandle: TDWGZCADHandle;
  AState: TDWGShellState): Boolean;
var
  Entry: PDWGZCADHandleEntry;
begin
  Result := FHandles.TryGetMutable(AHandle, Entry);
  if Result then
    Entry^.ShellState := AState;
end;

procedure TDWGZCADLoadContext.QueueOwnerResolve(AEntity: Pointer;
  AEntityHandle, AOwnerHandle: TDWGZCADHandle; AFallbackOwner: Pointer;
  ARawIndex: Integer);
var
  Fallback: Pointer;
begin
  Fallback := AFallbackOwner;
  if Fallback = nil then
    Fallback := FFallbackOwner;
  FPendingOwners.Append(AEntity, AEntityHandle, AOwnerHandle, Fallback,
    ARawIndex);
end;

function TDWGZCADLoadContext.TryGetEntry(AHandle: TDWGZCADHandle;
  out Entry: TDWGZCADHandleEntry): Boolean;
begin
  Result := FHandles.TryGet(AHandle, Entry);
end;

function TDWGZCADLoadContext.FindPending(AEntityHandle: TDWGZCADHandle
  ): PDWGZCADPendingOwner;
begin
  Result := FPendingOwners.ItemByEntityHandle(AEntityHandle);
end;

procedure TDWGZCADLoadContext.PushStack(Handle: TDWGZCADHandle);
begin
  SetLength(FResolveStack, Length(FResolveStack) + 1);
  FResolveStack[High(FResolveStack)] := Handle;
end;

procedure TDWGZCADLoadContext.PopStack;
begin
  if Length(FResolveStack) > 0 then
    SetLength(FResolveStack, Length(FResolveStack) - 1);
end;

function TDWGZCADLoadContext.StackContains(Handle: TDWGZCADHandle): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(FResolveStack) do
    if FResolveStack[I] = Handle then
      Exit(True);
  Result := False;
end;

procedure TDWGZCADLoadContext.AddWarning(Severity: TDWGImportSeverity;
  Code: Integer; Handle: TDWGZCADHandle; const Text: String);
var
  W: TDWGImportWarning;
begin
  W.Severity := Severity;
  W.Code := Code;
  W.Handle := Handle;
  W.Text := Text;
  SetLength(FWarnings, Length(FWarnings) + 1);
  FWarnings[High(FWarnings)] := W;
end;

procedure TDWGZCADLoadContext.DoAttach(Pending: PDWGZCADPendingOwner;
  Owner: Pointer; State: TDWGAttachState; Reason: TDWGAttachReason);
begin
  Pending^.AttachState := State;
  Pending^.AttachedOwner := Owner;
  Pending^.AttachReason := Reason;
  if State = asAttached then
    Inc(FAttachCount)
  else if State = asFallback then
    Inc(FFallbackCount);
  if Reason in [arSelfOwnerCycle, arOwnerChainCycle] then
    Inc(FCycleCount);
  if (Owner <> nil) and Assigned(FAttachProc) then
    FAttachProc(Pending^.Entity, Owner, Reason, FAttachData);
end;

procedure TDWGZCADLoadContext.ResolvePending(Pending: PDWGZCADPendingOwner);
var
  OwnerEntry: TDWGZCADHandleEntry;
  OwnerPending: PDWGZCADPendingOwner;
  Fallback: Pointer;
begin
  if Pending = nil then
    Exit;

  // Section 5.4: idempotency. A second call must not double-attach.
  if Pending^.AttachState in [asAttached, asFallback, asSkipped] then
    Exit;

  // Section 5.4: detect re-entry (cycle through the resolve stack).
  if Pending^.AttachState = asResolving then
  begin
    AddWarning(wsWarning, DWG_WARN_OWNER_CHAIN_CYCLE, Pending^.EntityHandle,
      Format('Owner chain cycle for entity %s, breaking',
        [IntToHex(Pending^.EntityHandle, 1)]));
    Fallback := Pending^.FallbackOwner;
    if Fallback = nil then
      Fallback := FFallbackOwner;
    DoAttach(Pending, Fallback, asFallback, arOwnerChainCycle);
    Exit;
  end;

  Pending^.AttachState := asResolving;
  PushStack(Pending^.EntityHandle);
  try
    Fallback := Pending^.FallbackOwner;
    if Fallback = nil then
      Fallback := FFallbackOwner;

    if Pending^.OwnerHandle = 0 then
    begin
      AddWarning(wsInfo, DWG_WARN_OWNER_NULL, Pending^.EntityHandle,
        Format('Entity %s has null owner; using fallback root',
          [IntToHex(Pending^.EntityHandle, 1)]));
      DoAttach(Pending, Fallback, asFallback, arNullOwner);
      Exit;
    end;

    if Pending^.OwnerHandle = Pending^.EntityHandle then
    begin
      AddWarning(wsWarning, DWG_WARN_OWNER_SELF_CYCLE, Pending^.EntityHandle,
        Format('Self-owner cycle on entity %s; using fallback root',
          [IntToHex(Pending^.EntityHandle, 1)]));
      DoAttach(Pending, Fallback, asFallback, arSelfOwnerCycle);
      Exit;
    end;

    if not FHandles.TryGet(Pending^.OwnerHandle, OwnerEntry) then
    begin
      AddWarning(wsWarning, DWG_WARN_OWNER_NOT_FOUND, Pending^.EntityHandle,
        Format('Owner %s not found for entity %s; using fallback root',
          [IntToHex(Pending^.OwnerHandle, 1),
           IntToHex(Pending^.EntityHandle, 1)]));
      DoAttach(Pending, Fallback, asFallback, arOwnerNotFound);
      Exit;
    end;

    if not (OwnerEntry.Kind in
        [dokBlockDef, dokModelSpace, dokPaperSpace, dokContainer]) then
    begin
      AddWarning(wsWarning, DWG_WARN_OWNER_NOT_CONTAINER, Pending^.EntityHandle,
        Format('Owner %s for entity %s is not a container (kind=%d); '+
               'using fallback root',
          [IntToHex(Pending^.OwnerHandle, 1),
           IntToHex(Pending^.EntityHandle, 1),
           Ord(OwnerEntry.Kind)]));
      DoAttach(Pending, Fallback, asFallback, arOwnerNotContainer);
      Exit;
    end;

    // Section 5.3: ensure owner itself is resolved before attaching child.
    OwnerPending := FindPending(Pending^.OwnerHandle);
    if OwnerPending <> nil then
    begin
      if OwnerPending^.AttachState = asResolving then
      begin
        AddWarning(wsWarning, DWG_WARN_OWNER_CHAIN_CYCLE,
          Pending^.EntityHandle,
          Format('Owner chain cycle %s -> %s detected; using fallback root',
            [IntToHex(Pending^.EntityHandle, 1),
             IntToHex(Pending^.OwnerHandle, 1)]));
        DoAttach(Pending, Fallback, asFallback, arOwnerChainCycle);
        Exit;
      end;
      ResolvePending(OwnerPending);
      if OwnerPending^.AttachState = asSkipped then
      begin
        AddWarning(wsWarning, DWG_WARN_OWNER_SKIPPED,
          Pending^.EntityHandle,
          Format('Owner %s for entity %s was skipped; using fallback root',
            [IntToHex(Pending^.OwnerHandle, 1),
             IntToHex(Pending^.EntityHandle, 1)]));
        DoAttach(Pending, Fallback, asFallback, arOwnerSkipped);
        Exit;
      end;
    end;

    if StackContains(Pending^.OwnerHandle) then
    begin
      AddWarning(wsWarning, DWG_WARN_OWNER_CHAIN_CYCLE, Pending^.EntityHandle,
        Format('Owner chain cycle reaches %s through %s; using fallback root',
          [IntToHex(Pending^.OwnerHandle, 1),
           IntToHex(Pending^.EntityHandle, 1)]));
      DoAttach(Pending, Fallback, asFallback, arOwnerChainCycle);
      Exit;
    end;

    DoAttach(Pending, OwnerEntry.Ptr, asAttached, arResolved);
  finally
    PopStack;
    if Pending^.AttachState = asResolving then
      Pending^.AttachState := asSkipped;
  end;
end;

procedure TDWGZCADLoadContext.ResolveOwners;
var
  I: Integer;
begin
  SetLength(FResolveStack, 0);
  for I := 0 to FPendingOwners.Count - 1 do
    ResolvePending(FPendingOwners.ItemAt(I));
end;

procedure TDWGZCADLoadContext.QueueRefResolve(AEntity: Pointer;
  AEntityHandle, ARefHandle: TDWGZCADHandle;
  AExpectedKind: TDWGZCADObjectKind; ASlot: TDWGZCADRefSlot;
  AFallback: Pointer);
var
  Fallback: Pointer;
begin
  Fallback := AFallback;
  if Fallback = nil then
    Fallback := FallbackForSlot(ASlot);
  FPendingRefs.AppendOrReplace(AEntity, AEntityHandle, ARefHandle,
    AExpectedKind, ASlot, Fallback);
end;

function TDWGZCADLoadContext.FindPendingRef(AEntityHandle: TDWGZCADHandle;
  ASlot: TDWGZCADRefSlot): PDWGZCADPendingRef;
begin
  Result := FPendingRefs.ItemByEntityAndSlot(AEntityHandle, ASlot);
end;

function TDWGZCADLoadContext.FallbackForSlot(ASlot: TDWGZCADRefSlot): Pointer;
begin
  case ASlot of
    rsLayer:     Result := FFallbackLayer;
    rsLineType:  Result := FFallbackLineType;
    rsTextStyle: Result := FFallbackTextStyle;
  else
    Result := nil;
  end;
end;

procedure TDWGZCADLoadContext.DoRefAttach(Pending: PDWGZCADPendingRef;
  Ref: Pointer; State: TDWGAttachState; Reason: TDWGAttachReason);
begin
  Pending^.AttachState := State;
  Pending^.AttachedRef := Ref;
  Pending^.AttachReason := Reason;
  if State = asAttached then
    Inc(FRefAttachCount)
  else if State = asFallback then
    Inc(FRefFallbackCount);
  if Assigned(FRefAttachProc) then
    FRefAttachProc(Pending^.Entity, Ref, Pending^.Slot, Reason,
      FRefAttachData);
end;

procedure TDWGZCADLoadContext.ResolveRef(Pending: PDWGZCADPendingRef);
var
  Entry: TDWGZCADHandleEntry;
  Fallback: Pointer;
begin
  if Pending = nil then
    Exit;

  // Idempotency (TZ §5.4): a second ResolveRefs call must not re-attach.
  if Pending^.AttachState in [asAttached, asFallback, asSkipped] then
    Exit;

  Fallback := Pending^.Fallback;
  if Fallback = nil then
    Fallback := FallbackForSlot(Pending^.Slot);

  if Pending^.RefHandle = 0 then
  begin
    AddWarning(wsInfo, DWG_WARN_REF_NULL, Pending^.EntityHandle,
      Format('Entity %s has null ref in slot %d; using fallback',
        [IntToHex(Pending^.EntityHandle, 1), Ord(Pending^.Slot)]));
    DoRefAttach(Pending, Fallback, asFallback, arRefNull);
    Exit;
  end;

  if not FHandles.TryGet(Pending^.RefHandle, Entry) then
  begin
    AddWarning(wsWarning, DWG_WARN_REF_NOT_FOUND, Pending^.EntityHandle,
      Format('Ref %s (slot %d) not found for entity %s; using fallback',
        [IntToHex(Pending^.RefHandle, 1), Ord(Pending^.Slot),
         IntToHex(Pending^.EntityHandle, 1)]));
    DoRefAttach(Pending, Fallback, asFallback, arRefNotFound);
    Exit;
  end;

  if Entry.Kind <> Pending^.ExpectedKind then
  begin
    AddWarning(wsWarning, DWG_WARN_REF_KIND_MISMATCH, Pending^.EntityHandle,
      Format('Ref %s for entity %s has kind %d, expected %d; using fallback',
        [IntToHex(Pending^.RefHandle, 1),
         IntToHex(Pending^.EntityHandle, 1),
         Ord(Entry.Kind), Ord(Pending^.ExpectedKind)]));
    DoRefAttach(Pending, Fallback, asFallback, arRefKindMismatch);
    Exit;
  end;

  if Entry.Ptr = nil then
  begin
    // Shell registered without a backing object: treat as not found so the
    // entity still ends up with a usable reference rather than nil.
    AddWarning(wsWarning, DWG_WARN_REF_NOT_FOUND, Pending^.EntityHandle,
      Format('Ref %s for entity %s registered with nil ptr; using fallback',
        [IntToHex(Pending^.RefHandle, 1),
         IntToHex(Pending^.EntityHandle, 1)]));
    DoRefAttach(Pending, Fallback, asFallback, arRefNotFound);
    Exit;
  end;

  DoRefAttach(Pending, Entry.Ptr, asAttached, arResolved);
end;

procedure TDWGZCADLoadContext.ResolveRefs;
var
  I: Integer;
begin
  for I := 0 to FPendingRefs.Count - 1 do
    ResolveRef(FPendingRefs.ItemAt(I));
end;

function TDWGZCADLoadContext.WarningCount: Integer;
begin
  Result := Length(FWarnings);
end;

function TDWGZCADLoadContext.WarningAt(Index: Integer): TDWGImportWarning;
begin
  if (Index < 0) or (Index > High(FWarnings)) then
    raise EDWGLoadContext.CreateFmt('Warning index %d out of range', [Index]);
  Result := FWarnings[Index];
end;

end.
