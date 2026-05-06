{*************************************************************************** }
{  fpdwg - DWG import lifecycle and shared load state (Stage 5.x R6)         }
{                                                                            }
{        Copyright (C) 2026 Andrey Zubarev <zamtmn@yandex.ru>                }
{                                                                            }
{  This library is free software, licensed under the terms of the GNU        }
{  General Public License as published by the Free Software Foundation,      }
{  either version 3 of the License, or (at your option) any later version.   }
{*************************************************************************** }

{ Refactor R6 (per TZ_DWG_LOAD_TO_ZCAD_AUDIT §3.6 / TZ §6.5):
  per-file load lifecycle (BeginDWGImport / EndDWGImport), the shared
  TDWGZCADLoadContext singleton and the attach callbacks used by the
  resolver. Mapper units in dwg/uzedwgtables.pas, dwg/uzedwgblocks.pas
  and dwg/entities/* read the global LoadCtx via GetLoadCtx and push their
  shells / pending refs through it.

  Splitting the lifecycle out of uzefflibredwg2ents.pas lets that unit
  shrink to a registration / compatibility facade as required by §6.4. }

unit uzedwgimport;

{$Include zengineconfig.inc}
{$Mode delphi}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  uzbLogIntf,
  SysUtils,
  dwg, dwgproc,
  uzeentgenericsubentry, uzedrawingsimple,
  uzeentity,
  uzestyleslayers, uzestyleslinetypes, uzestylestexts,
  uzeentabstracttext,
  uzeenttext, uzeentmtext,
  uzeconsts,
  uzeTypes,
  uzedwgloadcontext;

{ Stage 2 hooks called by uzefflibredwg.pas around parseDwg_Data. They open
  and close the per-file load context that decouples DWG read order from ZCAD
  attachment (TZ §5.3 / §12.2). Begin must be called before parseDwg_Data,
  End after. End is responsible for calling ResolveOwners and finalizing
  attached entities. The functions are no-ops if called out of order. }
procedure BeginDWGImport(var ZContext: TZDrawingContext);
procedure EndDWGImport(var ZContext: TZDrawingContext);

{ Mapper-side accessors. Each entity unit calls GetLoadCtx() to enqueue its
  shell / pending owner / pending refs and consults GetLoadDrawing() only
  on the legacy fallback path (BeginDWGImport not called). Returning nil is
  a valid signal that the loader is not active. }
function GetLoadCtx: TDWGZCADLoadContext;
function GetLoadDrawing: PTSimpleDrawing;

{ Stage 5 helper extracted from uzefflibredwg2ents.pas: register the entity
  shell + pending owner + layer/linetype/textstyle refs in one call. The
  WantTextStyle / TextStyleHandle pair is set by TEXT/MTEXT mappers and
  ignored by everything else. }
procedure DWGRegisterEntityShell(pobj: PGDBObjEntity;
  var DWGObject: Dwg_Object;
  WantTextStyle: Boolean; TextStyleHandle: QWord);

implementation

var
  LoadCtx: TDWGZCADLoadContext = nil;
  LoadDrawing: PTSimpleDrawing = nil;

function GetLoadCtx: TDWGZCADLoadContext;
begin
  Result := LoadCtx;
end;

function GetLoadDrawing: PTSimpleDrawing;
begin
  Result := LoadDrawing;
end;

{ Stage 4 (TZ §12.4): "block content форматируется при использовании INSERT
  или финальной formatting-фазе drawing". DWGAttachEntity must skip
  BuildGeometry when the resolved owner is a block definition because the
  drawing's BlockDefArray.FormatEntity will run BuildGeometry+formatEntity
  for every block-def child later. Walking the handle map keeps the test
  pipeline (which never registers block defs) on the existing fast path. }
function DWGOwnerIsBlockDef(Owner: Pointer): Boolean;
var
  i: Integer;
  Entry: PDWGZCADHandleEntry;
begin
  Result := False;
  if (Owner = nil) or (LoadCtx = nil) then
    Exit;
  for i := 0 to LoadCtx.Handles.Count - 1 do begin
    Entry := LoadCtx.Handles.EntryAt(i);
    if (Entry^.Kind = dokBlockDef) and (Entry^.Ptr = Owner) then
      Exit(True);
  end;
end;

procedure DWGAttachEntity(Entity: Pointer; Owner: Pointer;
  Reason: TDWGAttachReason; Data: Pointer);
var
  pobj: PGDBObjEntity;
  newowner: PGDBObjGenericSubEntry;
  ownerIsBlockDef: Boolean;
begin
  // Reason is forwarded to the logger so unresolved fallbacks are visible to
  // human reviewers without a separate diagnostic pass.
  pobj := PGDBObjEntity(Entity);
  newowner := PGDBObjGenericSubEntry(Owner);
  if (pobj = nil) or (newowner = nil) then
    Exit;

  newowner^.AddMi(PGDBObjSubordinated(pobj));
  if Reason <> arResolved then
    zDebugLn(['{WHM}LINE ', HexStr(PtrUInt(pobj), 16),
      ' attached via fallback (', DWGAttachReasonToText(Reason), ')']);

  // Stage 4 (TZ §12.4): defer block-def content geometry. BuildGeometry must
  // still run for entities landing in model/paper space (or fallback root) so
  // they appear on screen, but block-definition contents are formatted later
  // — either by an INSERT that materialises the block, or by the drawing's
  // global formatting pass (BlockDefArray.FormatEntity). Running BuildGeometry
  // here for a block-def child would duplicate the work that FormatEntity
  // does and pollute the deferred-formatting contract the spec calls out by
  // name ("block content форматируется при использовании INSERT или финальной
  // formatting-фазе drawing").
  ownerIsBlockDef := False;
  if LoadCtx <> nil then
    ownerIsBlockDef := DWGOwnerIsBlockDef(newowner);

  if (LoadDrawing <> nil) and (not ownerIsBlockDef) then begin
    PGDBObjEntity(pobj)^.BuildGeometry(LoadDrawing^);
    // FormatAfterDXFLoad/FromDXFPostProcessAfterAdd belong to the DXF code path
    // and require a TDrawContext that the DWG pipeline does not yet thread
    // through Stage 2. BuildGeometry is enough to satisfy the original bug
    // (line never built) without dragging the DXF post-processing chain in.
  end;
end;

{ Stage 3 (TZ §12.3): write a resolved visual-property pointer back into the
  entity's vp record. Owner attachment may not have happened yet (refs are
  resolved before owners) so this routine must NOT touch geometry — only the
  vp slot. BuildGeometry runs later from DWGAttachEntity once the owner is
  known. Reason is logged on fallback so a reviewer can spot which slot took
  the system-layer / ByLayer branch. }
procedure DWGAttachRef(Entity: Pointer; Ref: Pointer; Slot: TDWGZCADRefSlot;
  Reason: TDWGAttachReason; Data: Pointer);
var
  pobj: PGDBObjEntity;
begin
  pobj := PGDBObjEntity(Entity);
  if pobj = nil then
    Exit;

  case Slot of
    rsLayer:
      begin
        pobj^.vp.Layer := PGDBLayerProp(Ref);
        if Reason <> arResolved then
          zDebugLn(['{WHM}entity ', HexStr(PtrUInt(pobj), 16),
            ' layer fallback (', DWGAttachReasonToText(Reason), ')']);
      end;
    rsLineType:
      begin
        pobj^.vp.LineType := PGDBLtypeProp(Ref);
        if Reason <> arResolved then
          zDebugLn(['{WHM}entity ', HexStr(PtrUInt(pobj), 16),
            ' linetype fallback (', DWGAttachReasonToText(Reason), ')']);
      end;
    rsTextStyle:
      begin
        // Stage 5 (TZ §12.5): TEXT/MTEXT entities carry a TXTStyle pointer on
        // GDBObjText / GDBObjMText. Branch on GetObjType so we can refuse to
        // write the slot for any other entity that may end up queued by
        // mistake (the loader is allowed to queue defensively without knowing
        // whether the target supports the slot).
        if (pobj^.GetObjType = GDBtextID) or (pobj^.GetObjType = GDBMTextID) then
          PGDBObjText(pobj)^.TXTStyle := PGDBTextStyle(Ref);
        if Reason <> arResolved then
          zDebugLn(['{WHM}entity ', HexStr(PtrUInt(pobj), 16),
            ' textstyle fallback (', DWGAttachReasonToText(Reason), ')']);
      end;
    rsDimStyle:
      begin
        // Same as above — reserved for Stage 4 dimension mappers.
      end;
  end;
end;

procedure BeginDWGImport(var ZContext: TZDrawingContext);
var
  ByLayerLT: PGDBLtypeProp;
  SysLayer: PGDBLayerProp;
  StdStyle: PGDBTextStyle;
begin
  if LoadCtx <> nil then begin
    zDebugLn(['{WHM}DWG load context already active; force-resetting']);
    FreeAndNil(LoadCtx);
  end;
  LoadCtx := TDWGZCADLoadContext.Create;
  LoadDrawing := ZContext.PDrawing;
  // Register pObjRoot under handle 0 so any LINE with a missing owner falls
  // back into the model-space root (TZ §5.5: "broken owner -> fallback root").
  LoadCtx.SetFallbackOwner(ZContext.PDrawing^.pObjRoot);
  LoadCtx.SetAttachProc(@DWGAttachEntity, nil);
  LoadCtx.SetRefAttachProc(@DWGAttachRef, nil);

  // Stage 3 fallbacks (TZ §12.3): mirror the DXF loader's behaviour
  // (uzeffdxf.pas:412-427). A LINE with a missing/broken layer ref drops onto
  // the system layer; a missing/broken linetype ref drops onto the ByLayer
  // entry. These tables are pre-populated when the drawing is initialised so
  // the lookups always succeed.
  SysLayer := ZContext.PDrawing^.LayerTable.GetSystemLayer;
  ByLayerLT := PGDBLtypeProp(ZContext.PDrawing^.LTypeStyleTable.getAddres('ByLayer'));
  if ByLayerLT = nil then
    ByLayerLT := ZContext.PDrawing^.LTypeStyleTable.GetSystemLT(TLTByLayer);
  LoadCtx.SetFallbackLayer(SysLayer);
  LoadCtx.SetFallbackLineType(ByLayerLT);
  // The text-style fallback is the first style in the table when present;
  // mappers that genuinely care will overwrite this on a per-entity basis.
  StdStyle := ZContext.PDrawing^.TextStyleTable.FindStyle('Standard', False);
  LoadCtx.SetFallbackTextStyle(StdStyle);

  LoadCtx.RegisterShell(0, dokModelSpace, ZContext.PDrawing^.pObjRoot, -1);
end;

procedure EndDWGImport(var ZContext: TZDrawingContext);
begin
  if LoadCtx = nil then
    Exit;
  try
    // Stage 3: resolve refs first so DWGAttachEntity can BuildGeometry against
    // the right vp slots. Resolving owners afterwards triggers the attach
    // callback which then calls BuildGeometry.
    LoadCtx.ResolveRefs;
    LoadCtx.ResolveOwners;
    zDebugLn(['{WH}DWG owner resolve: attached=', LoadCtx.AttachCount,
      ', fallback=', LoadCtx.FallbackCount,
      ', cycles=', LoadCtx.CycleCount,
      ', refs_attached=', LoadCtx.RefAttachCount,
      ', refs_fallback=', LoadCtx.RefFallbackCount,
      ', warnings=', LoadCtx.WarningCount]);
  finally
    FreeAndNil(LoadCtx);
    LoadDrawing := nil;
  end;
end;

procedure DWGRegisterEntityShell(pobj: PGDBObjEntity;
  var DWGObject: Dwg_Object;
  WantTextStyle: Boolean; TextStyleHandle: QWord);
var
  EntityHandle, OwnerHandle, LayerHandle, LtypeHandle: QWord;
begin
  if LoadCtx = nil then
    Exit;
  EntityHandle := DWGObjectHandleValue(DWGObject);
  if not DWGObjectOwnerHandleValue(DWGObject, OwnerHandle) then
    OwnerHandle := 0;
  if EntityHandle <> 0 then
    LoadCtx.RegisterShell(EntityHandle, dokEntity, pobj, -1);
  LoadCtx.QueueOwnerResolve(pobj, EntityHandle, OwnerHandle);
  if not DWGEntityLayerHandleValue(DWGObject, LayerHandle) then
    LayerHandle := 0;
  if not DWGEntityLineTypeHandleValue(DWGObject, LtypeHandle) then
    LtypeHandle := 0;
  LoadCtx.QueueRefResolve(pobj, EntityHandle, LayerHandle,
    dokLayer, rsLayer, nil);
  LoadCtx.QueueRefResolve(pobj, EntityHandle, LtypeHandle,
    dokLineType, rsLineType, nil);
  if WantTextStyle then
    LoadCtx.QueueRefResolve(pobj, EntityHandle, TextStyleHandle,
      dokTextStyle, rsTextStyle, nil);
end;

initialization
finalization
  if LoadCtx <> nil then
    FreeAndNil(LoadCtx);
end.
