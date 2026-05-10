{*************************************************************************** }
{  fpdwg - DWG import finalize / post-process (Stage 5.x R7 / Phase 4)       }
{                                                                            }
{        Copyright (C) 2026 Andrey Zubarev <zamtmn@yandex.ru>                }
{                                                                            }
{  This library is free software, licensed under the terms of the GNU        }
{  General Public License as published by the Free Software Foundation,      }
{  either version 3 of the License, or (at your option) any later version.   }
{*************************************************************************** }

{ Refactor R7 (per TZ_DWG_LOAD_TO_ZCAD_AUDIT §3.7 / TZ §4.3, §9.1):
  Phase 4 of the DWG load. After ResolveRefs/ResolveOwners every entity
  owns its parent and its visual-property pointers. Phase 4 is the place
  where we mirror the DXF loader's per-entity post-processing chain that
  Stage 2 deferred:

      BuildGeometry(drawing)
      FormatAfterDXFLoad(drawing, dc)
      FromDXFPostProcessAfterAdd

  The audit spells out the contract:

    procedure FinalizeImport(var Ctx: TDWGZCADLoadContext;
      Drawing: PTSimpleDrawing; const DC: TDrawContext);

  Block-definition contents are deliberately skipped here — the drawing
  already runs BlockDefArray.FormatEntity over the whole table at a higher
  level (see BtnOpenDXFClick / mainform glue). Running per-entity Build
  inside a block-def again would duplicate that work and break the
  "block content форматируется при использовании INSERT или финальной
  formatting-фазе drawing" rule from §12.4.

  Pulling BuildGeometry out of DWGAttachEntity (where Stage 2 left it as a
  workaround) makes attachment idempotent again: refs and owners can be
  re-resolved without geometry being rebuilt as a side effect. }

unit uzedwgfinalize;

{$Include zengineconfig.inc}
{$Mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  uzbLogIntf,
  SysUtils,
  uzedrawingsimple,
  uzeentity,
  uzeentblockinsert,
  uzeentgenericsubentry,
  uzeconsts,
  uzgldrawcontext,
  uzedwgtypes,
  uzedwgloadcontext;

{ Phase 4 entry point. Walks every dokEntity entry in Ctx.Handles, looks up
  the resolved owner via the pending-owner queue, and runs the DXF-style
  post-processing chain when the owner is not a block definition. Safe to
  call with a nil Drawing (no-op) or an empty Ctx. }
procedure FinalizeImport(Ctx: TDWGZCADLoadContext;
  Drawing: PTSimpleDrawing; var DC: TDrawContext);

implementation

{ Stage 4 (TZ §12.4): mirror the DWGOwnerIsBlockDef helper from
  uzedwgimport.pas without exporting it — the original lives there because
  attach needs it; we keep a private copy here so finalize stays a leaf
  unit (no circular dependency back into uzedwgimport). }
function FinalizeOwnerIsBlockDef(Ctx: TDWGZCADLoadContext;
  Owner: Pointer): Boolean;
var
  I: Integer;
  Entry: PDWGZCADHandleEntry;
begin
  Result := False;
  if (Ctx = nil) or (Owner = nil) then
    Exit;
  for I := 0 to Ctx.Handles.Count - 1 do begin
    Entry := Ctx.Handles.EntryAt(I);
    if (Entry^.Kind = dokBlockDef) and (Entry^.Ptr = Owner) then
      Exit(True);
  end;
end;

function FinalizeOwnerIsBlockInsert(Ctx: TDWGZCADLoadContext;
  Owner: Pointer): Boolean;
var
  I: Integer;
  Entry: PDWGZCADHandleEntry;
begin
  Result := False;
  if (Ctx = nil) or (Owner = nil) then
    Exit;
  for I := 0 to Ctx.Handles.Count - 1 do begin
    Entry := Ctx.Handles.EntryAt(I);
    if (Entry^.Kind = dokBlockInsert) and (Entry^.Ptr = Owner) then
      Exit(True);
  end;
end;

procedure FinalizeEntityGeometry(Pobj: PGDBObjEntity;
  Drawing: PTSimpleDrawing; var DC: TDrawContext);
begin
  Pobj^.BuildGeometry(Drawing^);
  Pobj^.FormatAfterDXFLoad(Drawing^, DC);
  Pobj^.FromDXFPostProcessAfterAdd;
end;

procedure AttachDeferredInsertChildren(Ctx: TDWGZCADLoadContext;
  Drawing: PTSimpleDrawing; var DC: TDrawContext; var Processed: Integer);
var
  I: Integer;
  Entry: PDWGZCADHandleEntry;
  Pobj: PGDBObjEntity;
  Pending: PDWGZCADPendingOwner;
  Owner: Pointer;
  Insert: PGDBObjBlockInsert;
  InsertIndex: Integer;
begin
  for I := 0 to Ctx.Handles.Count - 1 do begin
    Entry := Ctx.Handles.EntryAt(I);
    if Entry^.Kind <> dokEntity then
      Continue;
    Pobj := PGDBObjEntity(Entry^.Ptr);
    if Pobj = nil then
      Continue;
    Pending := Ctx.FindPendingOwner(Entry^.Handle);
    if Pending = nil then
      Continue;
    Owner := Pending^.AttachedOwner;
    if Owner = nil then
      Owner := Pending^.FallbackOwner;
    if not FinalizeOwnerIsBlockInsert(Ctx, Owner) then
      Continue;

    Insert := PGDBObjBlockInsert(Owner);
    InsertIndex := Insert^.ConstObjArray.AddPEntity(Pobj^);
    Pobj^.correctobjects(PGDBObjEntity(Insert), InsertIndex);
    FinalizeEntityGeometry(Pobj, Drawing, DC);
    Inc(Processed);
  end;
end;

procedure FinalizeImport(Ctx: TDWGZCADLoadContext;
  Drawing: PTSimpleDrawing; var DC: TDrawContext);
var
  I: Integer;
  Entry: PDWGZCADHandleEntry;
  Pobj: PGDBObjEntity;
  Pending: PDWGZCADPendingOwner;
  Owner: Pointer;
  ProcessedEntities, SkippedBlockDef, SkippedInsertChild, SkippedNoOwner,
  VisualWarnings: Integer;
begin
  if (Ctx = nil) or (Drawing = nil) then
    Exit;

  ProcessedEntities := 0;
  SkippedBlockDef := 0;
  SkippedInsertChild := 0;
  SkippedNoOwner := 0;
  VisualWarnings := 0;

  for I := 0 to Ctx.Handles.Count - 1 do begin
    Entry := Ctx.Handles.EntryAt(I);
    if not (Entry^.Kind in [dokEntity, dokBlockInsert]) then
      Continue;
    Pobj := PGDBObjEntity(Entry^.Ptr);
    if Pobj = nil then
      Continue;

    // Lookup the resolved owner via the pending-owner queue. The queue is
    // not cleared by ResolveOwners so we can use it as a post-resolve index
    // without keeping a parallel list. An entity that never made it through
    // resolve still has the pending row but with AttachedOwner=nil — we use
    // the FallbackOwner (model-space root) the same way DWGAttachEntity
    // would have done.
    Pending := Ctx.FindPendingOwner(Entry^.Handle);
    if Pending = nil then begin
      Inc(SkippedNoOwner);
      zDebugLn(['{WHM}DWG finalize skip entity ', IntToHex(Entry^.Handle, 1),
        ': no pending owner row']);
      Continue;
    end;
    Owner := Pending^.AttachedOwner;
    if Owner = nil then
      Owner := Pending^.FallbackOwner;
    if Owner = nil then begin
      Inc(SkippedNoOwner);
      zDebugLn(['{WHM}DWG finalize skip entity ', IntToHex(Entry^.Handle, 1),
        ': no resolved or fallback owner']);
      Continue;
    end;

    // Block-def contents stay deferred (TZ §12.4). Counting them so the
    // diagnostic line below can show how the registry was split.
    if FinalizeOwnerIsBlockDef(Ctx, Owner) then begin
      Inc(SkippedBlockDef);
      Continue;
    end;
    if FinalizeOwnerIsBlockInsert(Ctx, Owner) then begin
      Inc(SkippedInsertChild);
      Continue;
    end;

    if Pobj^.vp.Layer = nil then begin
      Inc(VisualWarnings);
      zDebugLn(['{WHM}DWG finalize entity ', IntToHex(Entry^.Handle, 1),
        ' has nil layer after ref resolve']);
    end else if (not Pobj^.vp.Layer^._on) or Pobj^.vp.Layer^._freeze then begin
      Inc(VisualWarnings);
      zDebugLn(['{WHM}DWG finalize entity ', IntToHex(Entry^.Handle, 1),
        ' is on hidden layer ', Pobj^.vp.Layer^.Name]);
    end;
    if Pobj^.vp.LineType = nil then begin
      Inc(VisualWarnings);
      zDebugLn(['{WHM}DWG finalize entity ', IntToHex(Entry^.Handle, 1),
        ' has nil linetype after ref resolve']);
    end;

    FinalizeEntityGeometry(Pobj, Drawing, DC);
    Inc(ProcessedEntities);
  end;

  AttachDeferredInsertChildren(Ctx, Drawing, DC, ProcessedEntities);

  zDebugLn(['{WH}DWG finalize: built=', ProcessedEntities,
    ', deferred_blockdef=', SkippedBlockDef,
    ', deferred_insert_child=', SkippedInsertChild,
    ', no_owner=', SkippedNoOwner,
    ', visual_warnings=', VisualWarnings]);
end;

end.
