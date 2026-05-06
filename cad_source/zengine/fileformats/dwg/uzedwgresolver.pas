{*************************************************************************** }
{  fpdwg - DWG to ZCAD import context: resolver (Stage 5.x R2)               }
{                                                                            }
{        Copyright (C) 2026 Andrey Zubarev <zamtmn@yandex.ru>                }
{                                                                            }
{  This library is free software, licensed under the terms of the GNU        }
{  General Public License as published by the Free Software Foundation,      }
{  either version 3 of the License, or (at your option) any later version.   }
{*************************************************************************** }

{ Refactor R2 (per TZ_DWG_LOAD_TO_ZCAD_AUDIT §3.2 / TZ §5.4):
  cycle-safe owner / ref resolver. Lives in its own unit so the load context
  no longer carries the resolve algorithm in the same file as the registry
  and pending queues. The resolver depends on the context only through the
  IDWGResolverHost surface (see below): handle map lookup, pending lookup,
  fallback configuration, attach callbacks, warning sink and stats. }

unit uzedwgresolver;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  SysUtils, uzedwgtypes, uzedwgdiagnostics;

type
  { Minimum surface a resolver host must expose. The load context descends
    from this so the resolver can run without seeing the rest of the
    context's state. Keeping it an abstract class (not an interface) avoids
    pulling COM-style refcount semantics into a TObject-managed graph. }
  TDWGResolverHost = class
  public
    function TryGetEntry(AHandle: TDWGZCADHandle;
      out Entry: TDWGZCADHandleEntry): Boolean; virtual; abstract;
    function FindPendingOwner(AEntityHandle: TDWGZCADHandle
      ): PDWGZCADPendingOwner; virtual; abstract;

    function GetFallbackOwner: Pointer; virtual; abstract;
    function FallbackForSlot(ASlot: TDWGZCADRefSlot): Pointer;
      virtual; abstract;

    procedure InvokeOwnerAttach(Entity, Owner: Pointer;
      Reason: TDWGAttachReason); virtual; abstract;
    procedure InvokeRefAttach(Entity, Ref: Pointer; Slot: TDWGZCADRefSlot;
      Reason: TDWGAttachReason); virtual; abstract;

    procedure RaiseWarning(Severity: TDWGImportSeverity; Code: Integer;
      Handle: TDWGZCADHandle; const Text: String); virtual; abstract;
    function GetStatsRef: PDWGImportStats; virtual; abstract;
  end;

  { Stand-alone resolver state. Holds only the cycle-detection stack so a
    second concurrent resolver (e.g. a future raw-scan pre-resolver) can
    coexist with the main load context. The host supplies pending lookup
    and warning sinks, so this class has no entity-specific knowledge. }
  TDWGZCADResolver = class
  private
    FHost: TDWGResolverHost;
    FResolveStack: array of TDWGZCADHandle;
    procedure PushStack(Handle: TDWGZCADHandle);
    procedure PopStack;
    function StackContains(Handle: TDWGZCADHandle): Boolean;
    procedure FinishOwner(Pending: PDWGZCADPendingOwner; Owner: Pointer;
      State: TDWGAttachState; Reason: TDWGAttachReason);
    procedure FinishRef(Pending: PDWGZCADPendingRef; Ref: Pointer;
      State: TDWGAttachState; Reason: TDWGAttachReason);
  public
    constructor Create(AHost: TDWGResolverHost);
    procedure ResetStack;
    procedure ResolvePending(Pending: PDWGZCADPendingOwner);
    procedure ResolveRef(Pending: PDWGZCADPendingRef);
  end;

implementation

constructor TDWGZCADResolver.Create(AHost: TDWGResolverHost);
begin
  inherited Create;
  FHost := AHost;
end;

procedure TDWGZCADResolver.PushStack(Handle: TDWGZCADHandle);
begin
  SetLength(FResolveStack, Length(FResolveStack) + 1);
  FResolveStack[High(FResolveStack)] := Handle;
end;

procedure TDWGZCADResolver.PopStack;
begin
  if Length(FResolveStack) > 0 then
    SetLength(FResolveStack, Length(FResolveStack) - 1);
end;

function TDWGZCADResolver.StackContains(Handle: TDWGZCADHandle): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(FResolveStack) do
    if FResolveStack[I] = Handle then
      Exit(True);
  Result := False;
end;

procedure TDWGZCADResolver.ResetStack;
begin
  SetLength(FResolveStack, 0);
end;

procedure TDWGZCADResolver.FinishOwner(Pending: PDWGZCADPendingOwner;
  Owner: Pointer; State: TDWGAttachState; Reason: TDWGAttachReason);
var
  Stats: PDWGImportStats;
begin
  Pending^.AttachState := State;
  Pending^.AttachedOwner := Owner;
  Pending^.AttachReason := Reason;
  Stats := FHost.GetStatsRef;
  if State = asAttached then
    Inc(Stats^.AttachCount)
  else if State = asFallback then
    Inc(Stats^.FallbackCount);
  if Reason in [arSelfOwnerCycle, arOwnerChainCycle] then
    Inc(Stats^.CycleCount);
  if Owner <> nil then
    FHost.InvokeOwnerAttach(Pending^.Entity, Owner, Reason);
end;

procedure TDWGZCADResolver.ResolvePending(Pending: PDWGZCADPendingOwner);
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
    FHost.RaiseWarning(wsWarning, DWG_WARN_OWNER_CHAIN_CYCLE,
      Pending^.EntityHandle,
      Format('Owner chain cycle for entity %s, breaking',
        [IntToHex(Pending^.EntityHandle, 1)]));
    Fallback := Pending^.FallbackOwner;
    if Fallback = nil then
      Fallback := FHost.GetFallbackOwner;
    FinishOwner(Pending, Fallback, asFallback, arOwnerChainCycle);
    Exit;
  end;

  Pending^.AttachState := asResolving;
  PushStack(Pending^.EntityHandle);
  try
    Fallback := Pending^.FallbackOwner;
    if Fallback = nil then
      Fallback := FHost.GetFallbackOwner;

    if Pending^.OwnerHandle = 0 then
    begin
      FHost.RaiseWarning(wsInfo, DWG_WARN_OWNER_NULL, Pending^.EntityHandle,
        Format('Entity %s has null owner; using fallback root',
          [IntToHex(Pending^.EntityHandle, 1)]));
      FinishOwner(Pending, Fallback, asFallback, arNullOwner);
      Exit;
    end;

    if Pending^.OwnerHandle = Pending^.EntityHandle then
    begin
      FHost.RaiseWarning(wsWarning, DWG_WARN_OWNER_SELF_CYCLE,
        Pending^.EntityHandle,
        Format('Self-owner cycle on entity %s; using fallback root',
          [IntToHex(Pending^.EntityHandle, 1)]));
      FinishOwner(Pending, Fallback, asFallback, arSelfOwnerCycle);
      Exit;
    end;

    if not FHost.TryGetEntry(Pending^.OwnerHandle, OwnerEntry) then
    begin
      FHost.RaiseWarning(wsWarning, DWG_WARN_OWNER_NOT_FOUND,
        Pending^.EntityHandle,
        Format('Owner %s not found for entity %s; using fallback root',
          [IntToHex(Pending^.OwnerHandle, 1),
           IntToHex(Pending^.EntityHandle, 1)]));
      FinishOwner(Pending, Fallback, asFallback, arOwnerNotFound);
      Exit;
    end;

    if not (OwnerEntry.Kind in
        [dokBlockDef, dokModelSpace, dokPaperSpace, dokContainer]) then
    begin
      FHost.RaiseWarning(wsWarning, DWG_WARN_OWNER_NOT_CONTAINER,
        Pending^.EntityHandle,
        Format('Owner %s for entity %s is not a container (kind=%d); '+
               'using fallback root',
          [IntToHex(Pending^.OwnerHandle, 1),
           IntToHex(Pending^.EntityHandle, 1),
           Ord(OwnerEntry.Kind)]));
      FinishOwner(Pending, Fallback, asFallback, arOwnerNotContainer);
      Exit;
    end;

    // Section 5.3: ensure owner itself is resolved before attaching child.
    OwnerPending := FHost.FindPendingOwner(Pending^.OwnerHandle);
    if OwnerPending <> nil then
    begin
      if OwnerPending^.AttachState = asResolving then
      begin
        FHost.RaiseWarning(wsWarning, DWG_WARN_OWNER_CHAIN_CYCLE,
          Pending^.EntityHandle,
          Format('Owner chain cycle %s -> %s detected; using fallback root',
            [IntToHex(Pending^.EntityHandle, 1),
             IntToHex(Pending^.OwnerHandle, 1)]));
        FinishOwner(Pending, Fallback, asFallback, arOwnerChainCycle);
        Exit;
      end;
      ResolvePending(OwnerPending);
      if OwnerPending^.AttachState = asSkipped then
      begin
        FHost.RaiseWarning(wsWarning, DWG_WARN_OWNER_SKIPPED,
          Pending^.EntityHandle,
          Format('Owner %s for entity %s was skipped; using fallback root',
            [IntToHex(Pending^.OwnerHandle, 1),
             IntToHex(Pending^.EntityHandle, 1)]));
        FinishOwner(Pending, Fallback, asFallback, arOwnerSkipped);
        Exit;
      end;
    end;

    if StackContains(Pending^.OwnerHandle) then
    begin
      FHost.RaiseWarning(wsWarning, DWG_WARN_OWNER_CHAIN_CYCLE,
        Pending^.EntityHandle,
        Format('Owner chain cycle reaches %s through %s; using fallback root',
          [IntToHex(Pending^.OwnerHandle, 1),
           IntToHex(Pending^.EntityHandle, 1)]));
      FinishOwner(Pending, Fallback, asFallback, arOwnerChainCycle);
      Exit;
    end;

    FinishOwner(Pending, OwnerEntry.Ptr, asAttached, arResolved);
  finally
    PopStack;
    if Pending^.AttachState = asResolving then
      Pending^.AttachState := asSkipped;
  end;
end;

procedure TDWGZCADResolver.FinishRef(Pending: PDWGZCADPendingRef;
  Ref: Pointer; State: TDWGAttachState; Reason: TDWGAttachReason);
var
  Stats: PDWGImportStats;
begin
  Pending^.AttachState := State;
  Pending^.AttachedRef := Ref;
  Pending^.AttachReason := Reason;
  Stats := FHost.GetStatsRef;
  if State = asAttached then
    Inc(Stats^.RefAttachCount)
  else if State = asFallback then
    Inc(Stats^.RefFallbackCount);
  FHost.InvokeRefAttach(Pending^.Entity, Ref, Pending^.Slot, Reason);
end;

procedure TDWGZCADResolver.ResolveRef(Pending: PDWGZCADPendingRef);
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
    Fallback := FHost.FallbackForSlot(Pending^.Slot);

  if Pending^.RefHandle = 0 then
  begin
    FHost.RaiseWarning(wsInfo, DWG_WARN_REF_NULL, Pending^.EntityHandle,
      Format('Entity %s has null ref in slot %d; using fallback',
        [IntToHex(Pending^.EntityHandle, 1), Ord(Pending^.Slot)]));
    FinishRef(Pending, Fallback, asFallback, arRefNull);
    Exit;
  end;

  if not FHost.TryGetEntry(Pending^.RefHandle, Entry) then
  begin
    FHost.RaiseWarning(wsWarning, DWG_WARN_REF_NOT_FOUND,
      Pending^.EntityHandle,
      Format('Ref %s (slot %d) not found for entity %s; using fallback',
        [IntToHex(Pending^.RefHandle, 1), Ord(Pending^.Slot),
         IntToHex(Pending^.EntityHandle, 1)]));
    FinishRef(Pending, Fallback, asFallback, arRefNotFound);
    Exit;
  end;

  if Entry.Kind <> Pending^.ExpectedKind then
  begin
    FHost.RaiseWarning(wsWarning, DWG_WARN_REF_KIND_MISMATCH,
      Pending^.EntityHandle,
      Format('Ref %s for entity %s has kind %d, expected %d; using fallback',
        [IntToHex(Pending^.RefHandle, 1),
         IntToHex(Pending^.EntityHandle, 1),
         Ord(Entry.Kind), Ord(Pending^.ExpectedKind)]));
    FinishRef(Pending, Fallback, asFallback, arRefKindMismatch);
    Exit;
  end;

  if Entry.Ptr = nil then
  begin
    // Shell registered without a backing object: treat as not found so the
    // entity still ends up with a usable reference rather than nil.
    FHost.RaiseWarning(wsWarning, DWG_WARN_REF_NOT_FOUND,
      Pending^.EntityHandle,
      Format('Ref %s for entity %s registered with nil ptr; using fallback',
        [IntToHex(Pending^.RefHandle, 1),
         IntToHex(Pending^.EntityHandle, 1)]));
    FinishRef(Pending, Fallback, asFallback, arRefNotFound);
    Exit;
  end;

  FinishRef(Pending, Entry.Ptr, asAttached, arResolved);
end;

end.
