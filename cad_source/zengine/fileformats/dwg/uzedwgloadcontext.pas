{*************************************************************************** }
{  fpdwg - DWG to ZCAD import context (Stage 2, refactored 5.x R2)           }
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
  AddMi and BuildGeometry calls live in uzefflibredwg2ents.pas.

  Refactor R2 (per TZ_DWG_LOAD_TO_ZCAD_AUDIT §3.2 / TZ §6.5): the previous
  999-line monolith is split into four units. Public surface is preserved
  via interface re-export so existing callers (uzefflibredwg2ents.pas,
  uzedwgtestloadcontext.pas) keep compiling unchanged. }

unit uzedwgloadcontext;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  SysUtils,
  // Re-exported so old callers see TDWGZCADHandle, the diagnostic codes,
  // attach-callback types and DWGAttachReasonToText through this single unit.
  uzedwgtypes,
  uzedwgdiagnostics,
  uzedwgresolver;

type
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
      AFallback: Pointer; AInlineRef: Boolean = False): Integer;
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
  TDWGZCADLoadContext = class(TDWGResolverHost)
  private
    FHandles: TDWGZCADHandleMap;
    FPendingOwners: TDWGZCADPendingOwnerList;
    FPendingRefs: TDWGZCADPendingRefList;
    FWarnings: TDWGImportWarningList;
    FStats: TDWGImportStats;
    FResolver: TDWGZCADResolver;
    FAttachProc: TDWGAttachProc;
    FAttachData: Pointer;
    FRefAttachProc: TDWGRefAttachProc;
    FRefAttachData: Pointer;
    FFallbackOwner: Pointer;
    FFallbackLayer: Pointer;
    FFallbackLineType: Pointer;
    FFallbackTextStyle: Pointer;
    FFallbackDimStyle: Pointer;
  public
    { TDWGResolverHost surface (R2): exposed publicly so the resolver can
      reach the registry and warning sink through the abstract base. }
    function TryGetEntry(AHandle: TDWGZCADHandle;
      out Entry: TDWGZCADHandleEntry): Boolean; override;
    function FindPendingOwner(AEntityHandle: TDWGZCADHandle
      ): PDWGZCADPendingOwner; override;
    function GetFallbackOwner: Pointer; override;
    function FallbackForSlot(ASlot: TDWGZCADRefSlot): Pointer; override;
    procedure InvokeOwnerAttach(Entity, Owner: Pointer;
      Reason: TDWGAttachReason); override;
    procedure InvokeRefAttach(Entity, Ref: Pointer; Slot: TDWGZCADRefSlot;
      Reason: TDWGAttachReason); override;
    procedure RaiseWarning(Severity: TDWGImportSeverity; Code: Integer;
      Handle: TDWGZCADHandle; const Text: String); override;
    function GetStatsRef: PDWGImportStats; override;

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
    procedure SetFallbackDimStyle(ADimStyle: Pointer);

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
      route the entity directly to the registered fallback. AInlineRef marks a
      DWG enum value (for example ltype_flags=ByLayer) that has no handle by
      design and must attach without fallback diagnostics. }
    procedure QueueRefResolve(AEntity: Pointer;
      AEntityHandle, ARefHandle: TDWGZCADHandle;
      AExpectedKind: TDWGZCADObjectKind; ASlot: TDWGZCADRefSlot;
      AFallback: Pointer = nil; AInlineRef: Boolean = False);

    { Phase 3: Resolve owners (cycle-safe). Idempotent: a second call leaves
      already-attached entities alone. }
    procedure ResolveOwners;

    { Phase 3.5 (Stage 3): Resolve queued visual references. Idempotent:
      every pending ref is processed once and a second call is a no-op. }
    procedure ResolveRefs;

    { Lookup helpers }
    function FindPending(AEntityHandle: TDWGZCADHandle
      ): PDWGZCADPendingOwner;
    function FindPendingRef(AEntityHandle: TDWGZCADHandle;
      ASlot: TDWGZCADRefSlot): PDWGZCADPendingRef;

    { Diagnostics }
    function WarningCount: Integer;
    function WarningAt(Index: Integer): TDWGImportWarning;
    property AttachCount: Integer read FStats.AttachCount;
    property FallbackCount: Integer read FStats.FallbackCount;
    property CycleCount: Integer read FStats.CycleCount;
    property RefAttachCount: Integer read FStats.RefAttachCount;
    property RefFallbackCount: Integer read FStats.RefFallbackCount;
    property FallbackOwner: Pointer read FFallbackOwner;
    property FallbackLayer: Pointer read FFallbackLayer;
    property FallbackLineType: Pointer read FFallbackLineType;
    property FallbackTextStyle: Pointer read FFallbackTextStyle;
    property FallbackDimStyle: Pointer read FFallbackDimStyle;
    property Handles: TDWGZCADHandleMap read FHandles;
    property PendingOwners: TDWGZCADPendingOwnerList read FPendingOwners;
    property PendingRefs: TDWGZCADPendingRefList read FPendingRefs;
  end;

implementation

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
  begin
    // R4 (TZ §3.4): a raw-scan placeholder (Kind=dokUnknown, Ptr=nil) is not
    // a duplicate — the mapper that runs later is allowed to upgrade it to
    // its real kind/ptr while keeping the RawIndex captured by Phase 1.
    if (FEntries[Index].Kind = dokUnknown)
       and (FEntries[Index].Ptr = nil)
       and (AKind <> dokUnknown) then
    begin
      FEntries[Index].Kind := AKind;
      FEntries[Index].Ptr := APtr;
      FEntries[Index].ShellState := AState;
      // Mappers usually pass ARawIndex=-1 (they no longer have the index in
      // hand). Keep the value the raw scan captured so the registry stays
      // consistent with the LibreDWG layout.
      if (ARawIndex >= 0) and (FEntries[Index].RawIndex < 0) then
        FEntries[Index].RawIndex := ARawIndex;
      Exit(True);
    end;
    Exit(False);
  end;
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
  AFallback: Pointer; AInlineRef: Boolean): Integer;
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
  Item.InlineRef := AInlineRef;
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
  FWarnings := TDWGImportWarningList.Create;
  FStats.Clear;
  FResolver := TDWGZCADResolver.Create(Self);
end;

destructor TDWGZCADLoadContext.Destroy;
begin
  FResolver.Free;
  FWarnings.Free;
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

procedure TDWGZCADLoadContext.SetFallbackDimStyle(ADimStyle: Pointer);
begin
  FFallbackDimStyle := ADimStyle;
end;

function TDWGZCADLoadContext.RegisterShell(AHandle: TDWGZCADHandle;
  AKind: TDWGZCADObjectKind; APtr: Pointer; ARawIndex: Integer): Boolean;
begin
  Result := FHandles.RegisterShell(AHandle, AKind, APtr, ARawIndex, msCreated);
  if not Result then
    FWarnings.Add(wsWarning, DWG_WARN_DUPLICATE_HANDLE, AHandle,
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

function TDWGZCADLoadContext.FindPendingOwner(AEntityHandle: TDWGZCADHandle
  ): PDWGZCADPendingOwner;
begin
  Result := FPendingOwners.ItemByEntityHandle(AEntityHandle);
end;

procedure TDWGZCADLoadContext.RaiseWarning(Severity: TDWGImportSeverity;
  Code: Integer; Handle: TDWGZCADHandle; const Text: String);
begin
  FWarnings.Add(Severity, Code, Handle, Text);
end;

procedure TDWGZCADLoadContext.InvokeOwnerAttach(Entity, Owner: Pointer;
  Reason: TDWGAttachReason);
begin
  if Assigned(FAttachProc) then
    FAttachProc(Entity, Owner, Reason, FAttachData);
end;

procedure TDWGZCADLoadContext.InvokeRefAttach(Entity, Ref: Pointer;
  Slot: TDWGZCADRefSlot; Reason: TDWGAttachReason);
begin
  if Assigned(FRefAttachProc) then
    FRefAttachProc(Entity, Ref, Slot, Reason, FRefAttachData);
end;

function TDWGZCADLoadContext.GetFallbackOwner: Pointer;
begin
  Result := FFallbackOwner;
end;

function TDWGZCADLoadContext.FallbackForSlot(ASlot: TDWGZCADRefSlot): Pointer;
begin
  case ASlot of
    rsLayer:     Result := FFallbackLayer;
    rsLineType,
    rsLayerLineType:
      Result := FFallbackLineType;
    rsTextStyle: Result := FFallbackTextStyle;
    rsDimStyle:  Result := FFallbackDimStyle;
  else
    Result := nil;
  end;
end;

function TDWGZCADLoadContext.GetStatsRef: PDWGImportStats;
begin
  Result := @FStats;
end;

procedure TDWGZCADLoadContext.ResolveOwners;
var
  I: Integer;
begin
  FResolver.ResetStack;
  for I := 0 to FPendingOwners.Count - 1 do
    FResolver.ResolvePending(FPendingOwners.ItemAt(I));
end;

procedure TDWGZCADLoadContext.QueueRefResolve(AEntity: Pointer;
  AEntityHandle, ARefHandle: TDWGZCADHandle;
  AExpectedKind: TDWGZCADObjectKind; ASlot: TDWGZCADRefSlot;
  AFallback: Pointer; AInlineRef: Boolean);
var
  Fallback: Pointer;
begin
  Fallback := AFallback;
  if Fallback = nil then
    Fallback := FallbackForSlot(ASlot);
  FPendingRefs.AppendOrReplace(AEntity, AEntityHandle, ARefHandle,
    AExpectedKind, ASlot, Fallback, AInlineRef);
end;

function TDWGZCADLoadContext.FindPendingRef(AEntityHandle: TDWGZCADHandle;
  ASlot: TDWGZCADRefSlot): PDWGZCADPendingRef;
begin
  Result := FPendingRefs.ItemByEntityAndSlot(AEntityHandle, ASlot);
end;

procedure TDWGZCADLoadContext.ResolveRefs;
var
  I: Integer;
begin
  for I := 0 to FPendingRefs.Count - 1 do
    FResolver.ResolveRef(FPendingRefs.ItemAt(I));
end;

function TDWGZCADLoadContext.WarningCount: Integer;
begin
  Result := FWarnings.Count;
end;

function TDWGZCADLoadContext.WarningAt(Index: Integer): TDWGImportWarning;
begin
  Result := FWarnings.Item(Index);
end;

end.
