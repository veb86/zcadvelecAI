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
  dwg, dwgproc,uzedwghandle,
  uzeentgenericsubentry, uzedrawingsimple,
  uzeentity,
  uzestyleslayers, uzestyleslinetypes, uzestylestexts, uzestylesdim,
  uzeentabstracttext,
  uzeentsubordinated,
  uzeenttext, uzeentmtext, uzeentblockinsert,
  uzeentdimension, uzeentdimensiongeneric,
  uzeblockdef, UGDBObjBlockdefArray,
  uzeconsts,
  uzeTypes,
  uzeffmanager,
  uzedwgtypes,
  uzedwgloadcontext,
  uzedwgrawscan,
  uzedwgfinalize;

{ Stage 2 hooks called by uzefflibredwg.pas around parseDwg_Data. They open
  and close the per-file load context that decouples DWG read order from ZCAD
  attachment (TZ §5.3 / §12.2). Begin must be called before parseDwg_Data,
  End after. End is responsible for calling ResolveOwners, FinalizeImport
  and freeing the shared load context. The functions are no-ops if called
  out of order.

  R4 (TZ §3.4) adds ScanDWGImport between Begin and parseDwg_Data: a Phase 1
  raw-scan over the LibreDWG object array that pre-registers handle -> raw
  index entries so duplicate detection happens once and mappers can upgrade
  placeholders instead of fighting the duplicate-handle warning. }
procedure BeginDWGImport(var ZContext: TZDrawingContext);
procedure ScanDWGImport(var Raw: Dwg_Data);
procedure EndDWGImport(var ZContext: TZDrawingContext);

{ Mapper-side accessors. Each entity unit calls GetLoadCtx() to enqueue its
  shell / pending owner / pending refs and consults GetLoadDrawing() only
  on the legacy fallback path (BeginDWGImport not called). Returning nil is
  a valid signal that the loader is not active. }
function GetLoadCtx: TDWGZCADLoadContext;
function GetLoadDrawing: PTSimpleDrawing;
function DWGEnsureDimStyle(var Drawing: TSimpleDrawing;
  const Name: string = 'Standard'): PGDBDimStyle;

{ Stage 5 helper extracted from uzefflibredwg2ents.pas: register the entity
  shell + pending owner + layer/linetype/textstyle refs in one call. The
  WantTextStyle / TextStyleHandle pair is set by TEXT/MTEXT mappers and
  ignored by everything else. }
procedure DWGRegisterEntityShell(pobj: PGDBObjEntity;
  var DWGObject: Dwg_Object;
  WantTextStyle: Boolean; TextStyleHandle: QWord;
  AKind: TDWGZCADObjectKind = dokEntity);

implementation

var
  LoadCtx: TDWGZCADLoadContext = nil;
  LoadDrawing: PTSimpleDrawing = nil;
  LoadHasCurrentLayerHandle: Boolean = False;
  LoadCurrentLayerHandle: QWord = 0;

function DWGSystemLineTypeForKind(Kind: TDWGEntityLineTypeKind): PGDBLtypeProp;
var
  Name: string;
  Mode: TLTMode;
begin
  Result := nil;
  if LoadDrawing = nil then
    Exit;

  case Kind of
    dltByBlock:
      begin
        Name := 'ByBlock';
        Mode := TLTByBlock;
      end;
    dltContinuous:
      begin
        Name := 'Continuous';
        Mode := TLTContinous;
      end;
    else
      begin
        Name := 'ByLayer';
        Mode := TLTByLayer;
      end;
  end;

  Result := PGDBLtypeProp(LoadDrawing^.LTypeStyleTable.getAddres(Name));
  if Result = nil then
    Result := LoadDrawing^.LTypeStyleTable.GetSystemLT(Mode);
end;

function GetLoadCtx: TDWGZCADLoadContext;
begin
  Result := LoadCtx;
end;

function GetLoadDrawing: PTSimpleDrawing;
begin
  Result := LoadDrawing;
end;

function DWGEnsureTextStyle(var Drawing: TSimpleDrawing): PGDBTextStyle;
begin
  Result := Drawing.TextStyleTable.FindStyle('Standard', False);
  if Result = nil then
    Result := Drawing.GetCurrentTextStyle;
end;

function DWGEnsureDimStyle(var Drawing: TSimpleDrawing;
  const Name: string): PGDBDimStyle;
var
  TextStyle: PGDBTextStyle;
begin
  Result := PGDBDimStyle(Drawing.DimStyleTable.getAddres(Name));
  if Result = nil then begin
    Result := PGDBDimStyle(Drawing.DimStyleTable.MergeItem(Name, TLOLoad));
    if Result <> nil then begin
      Result^.init(Name);
      Result^.SetDefaultValues;
    end;
  end;
  if Result = nil then
    Exit;
  TextStyle := DWGEnsureTextStyle(Drawing);
  if Result^.Text.DIMTXSTY = nil then
    Result^.Text.DIMTXSTY := TextStyle;
  if Drawing.CurrentDimStyle = nil then
    Drawing.CurrentDimStyle := Result;
end;

function DWGEnsureFallbackBlockDef: PGDBObjBlockdef;
const
  MissingBlockName = '*DWG_MISSING_BLOCK';
begin
  Result := nil;
  if LoadDrawing = nil then
    Exit;
  Result := LoadDrawing^.BlockDefArray.getblockdef(MissingBlockName);
  if Result = nil then
    Result := LoadDrawing^.BlockDefArray.create(MissingBlockName);
end;

function DWGPointerHasKind(P: Pointer; Kind: TDWGZCADObjectKind): Boolean;
var
  I: Integer;
  Entry: PDWGZCADHandleEntry;
begin
  Result := False;
  if (LoadCtx = nil) or (P = nil) then
    Exit;
  for I := 0 to LoadCtx.Handles.Count - 1 do begin
    Entry := LoadCtx.Handles.EntryAt(I);
    if (Entry^.Ptr = P) and (Entry^.Kind = Kind) then
      Exit(True);
  end;
end;

function DWGObjTypeIsDimension(ObjType: TObjID): Boolean;
begin
  case ObjType of
    GDBGenericDimensionID,
    GDBAlignedDimensionID,
    GDBRotatedDimensionID,
    GDBDiametricDimensionID,
    GDBRadialDimensionID:
      Result := True;
  else
    Result := False;
  end;
end;

procedure ApplyDWGCurrentLayer(var ZContext: TZDrawingContext);
var
  Entry: TDWGZCADHandleEntry;
  CurrentLayer: PGDBLayerProp;
begin
  CurrentLayer := nil;
  if (LoadCtx <> nil) and LoadHasCurrentLayerHandle then begin
    if LoadCtx.TryGetEntry(LoadCurrentLayerHandle, Entry) and
       (Entry.Kind = dokLayer) then
      CurrentLayer := PGDBLayerProp(Entry.Ptr);
    if CurrentLayer = nil then
      zDebugLn(['{WHM}DWG current layer handle ',
        IntToHex(LoadCurrentLayerHandle, 1),
        ' did not resolve to a layer; using system layer']);
  end;
  if CurrentLayer = nil then
    CurrentLayer := ZContext.PDrawing^.LayerTable.GetSystemLayer;
  ZContext.PDrawing^.CurrentLayer := CurrentLayer;
  if CurrentLayer <> nil then
    zDebugLn(['{WH}DWG current layer -> ', CurrentLayer^.Name]);
end;

procedure DWGAttachEntity(Entity: Pointer; Owner: Pointer;
  Reason: TDWGAttachReason; Data: Pointer);
var
  pobj: PGDBObjEntity;
  newowner: PGDBObjGenericSubEntry;
begin
  // Reason is forwarded to the logger so unresolved fallbacks are visible to
  // human reviewers without a separate diagnostic pass.
  pobj := PGDBObjEntity(Entity);
  if (pobj = nil) or (Owner = nil) then
    Exit;

  // INSERT-owned ATTRIB entities are appended after the INSERT has built its
  // block geometry. Adding them now would be undone by BuildGeometry clearing
  // ConstObjArray from the block definition.
  if DWGPointerHasKind(Owner, dokBlockInsert) then begin
    pobj^.bp.ListPos.Owner := PGDBObjEntity(Owner);
    if Reason <> arResolved then
      zDebugLn(['{WHM}entity ', HexStr(PtrUInt(pobj), 16),
        ' deferred under INSERT via fallback (',
        DWGAttachReasonToText(Reason), ')']);
    Exit;
  end;

  newowner := PGDBObjGenericSubEntry(Owner);

  newowner^.AddMi(PGDBObjSubordinated(pobj));
  if Reason <> arResolved then
    zDebugLn(['{WHM}entity ', HexStr(PtrUInt(pobj), 16),
      ' attached via fallback (', DWGAttachReasonToText(Reason), ')']);

  // R7 (TZ §3.7): BuildGeometry / FormatAfterDXFLoad / FromDXFPostProcessAfterAdd
  // moved to uzedwgfinalize.FinalizeImport. Attach is back to being just an
  // AddMi: the resolver may revisit a handle without geometry being rebuilt
  // as a side effect, and finalize gets a single place to mirror the DXF
  // post-processing chain (TDrawContext threaded through addfromdwg).
end;

function DWGLayerNameForLog(Layer: PGDBLayerProp): string;
begin
  if Layer = nil then
    Exit('(nil layer)');
  Result := Layer^.Name;
  if Result = '' then
    Result := '(unnamed layer)';
end;

function DWGLTypeNameForLog(LType: PGDBLtypeProp): string;
begin
  if LType = nil then
    Exit('(nil linetype)');
  Result := LType^.Name;
  if Result = '' then
    Result := '(unnamed linetype)';
end;

function DWGRefHandlesForLog(Entity: Pointer; Slot: TDWGZCADRefSlot): string;
var
  I: Integer;
  Pending: PDWGZCADPendingRef;
begin
  Result := '';
  if LoadCtx = nil then
    Exit;
  for I := 0 to LoadCtx.PendingRefs.Count - 1 do begin
    Pending := LoadCtx.PendingRefs.ItemAt(I);
    if (Pending^.Entity = Entity) and (Pending^.Slot = Slot) then begin
      Result := 'handle=' + IntToHex(Pending^.EntityHandle, 1) +
        ' ref=' + IntToHex(Pending^.RefHandle, 1);
      Exit;
    end;
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
  player: PGDBLayerProp;
  pDimStyle: PGDBDimStyle;
  pBlockDef: PGDBObjBlockdef;
  pInsert: PGDBObjBlockInsert;
begin
  case Slot of
    rsLayer:
      begin
        pobj := PGDBObjEntity(Entity);
        if pobj = nil then
          Exit;
        pobj^.vp.Layer := PGDBLayerProp(Ref);
        if Reason <> arResolved then
          zDebugLn(['{WHM}entity ', HexStr(PtrUInt(pobj), 16),
            ' ', DWGRefHandlesForLog(Entity, Slot),
            ' layer fallback (', DWGAttachReasonToText(Reason), ') -> ',
            DWGLayerNameForLog(PGDBLayerProp(Ref))]);
      end;
    rsLineType:
      begin
        pobj := PGDBObjEntity(Entity);
        if pobj = nil then
          Exit;
        pobj^.vp.LineType := PGDBLtypeProp(Ref);
        if Reason <> arResolved then
          zDebugLn(['{WHM}entity ', HexStr(PtrUInt(pobj), 16),
            ' ', DWGRefHandlesForLog(Entity, Slot),
            ' linetype fallback (', DWGAttachReasonToText(Reason),
            ', layer=', DWGLayerNameForLog(pobj^.vp.Layer), ') -> ',
            DWGLTypeNameForLog(PGDBLtypeProp(Ref))]);
      end;
    rsLayerLineType:
      begin
        player := PGDBLayerProp(Entity);
        if player = nil then
          Exit;
        player^.LT := PGDBLtypeProp(Ref);
        if Reason <> arResolved then
          zDebugLn(['{WHM}layer ', DWGLayerNameForLog(player),
            ' ', DWGRefHandlesForLog(Entity, Slot),
            ' linetype fallback (', DWGAttachReasonToText(Reason), ') -> ',
            DWGLTypeNameForLog(PGDBLtypeProp(Ref))])
        else
          zDebugLn(['{WH}layer ', DWGLayerNameForLog(player),
            ' ', DWGRefHandlesForLog(Entity, Slot),
            ' linetype -> ', DWGLTypeNameForLog(PGDBLtypeProp(Ref))]);
      end;
    rsTextStyle:
      begin
        pobj := PGDBObjEntity(Entity);
        if pobj = nil then
          Exit;
        // Stage 5 (TZ §12.5): TEXT/MTEXT entities carry a TXTStyle pointer on
        // GDBObjText / GDBObjMText. Branch on GetObjType so we can refuse to
        // write the slot for any other entity that may end up queued by
        // mistake (the loader is allowed to queue defensively without knowing
        // whether the target supports the slot).
        if (pobj^.GetObjType = GDBtextID) or (pobj^.GetObjType = GDBMTextID) then
          PGDBObjText(pobj)^.TXTStyle := PGDBTextStyle(Ref);
        if Reason <> arResolved then
          zDebugLn(['{WHM}entity ', HexStr(PtrUInt(pobj), 16),
            ' ', DWGRefHandlesForLog(Entity, Slot),
            ' textstyle fallback (', DWGAttachReasonToText(Reason), ')']);
      end;
    rsDimStyle:
      begin
        pobj := PGDBObjEntity(Entity);
        if pobj = nil then
          Exit;
        pDimStyle := PGDBDimStyle(Ref);
        if pDimStyle = nil then begin
          if LoadDrawing <> nil then
            pDimStyle := DWGEnsureDimStyle(LoadDrawing^);
        end;
        if DWGObjTypeIsDimension(pobj^.GetObjType) then begin
          if pobj^.GetObjType = GDBGenericDimensionID then
            PGDBObjGenericDimension(pobj)^.PDimStyle := pDimStyle
          else
            PGDBObjDimension(pobj)^.PDimStyle := pDimStyle;
        end;
        if Reason <> arResolved then
          zDebugLn(['{WHM}entity ', HexStr(PtrUInt(pobj), 16),
            ' ', DWGRefHandlesForLog(Entity, Slot),
            ' dimstyle fallback (', DWGAttachReasonToText(Reason), ')']);
      end;
    rsBlockDef:
      begin
        pobj := PGDBObjEntity(Entity);
        if (pobj = nil) or (pobj^.GetObjType <> GDBBlockInsertID) then
          Exit;
        pInsert := PGDBObjBlockInsert(pobj);
        if (Ref <> nil) and
          ((Reason <> arResolved) or DWGPointerHasKind(Ref, dokBlockDef)) then
          pBlockDef := PGDBObjBlockdef(Ref)
        else begin
          pBlockDef := DWGEnsureFallbackBlockDef;
          if Reason = arResolved then
            zDebugLn(['{WHM}entity ', HexStr(PtrUInt(pobj), 16),
              ' ', DWGRefHandlesForLog(Entity, Slot),
              ' block ref resolves to model/paper space; using empty block'])
          else
            zDebugLn(['{WHM}entity ', HexStr(PtrUInt(pobj), 16),
              ' ', DWGRefHandlesForLog(Entity, Slot),
              ' block fallback (', DWGAttachReasonToText(Reason), ')']);
        end;
        if pBlockDef <> nil then begin
          pInsert^.PDef := pBlockDef;
          pInsert^.Name := pBlockDef^.Name;
          if LoadDrawing <> nil then
            pInsert^.index := LoadDrawing^.BlockDefArray.getindex(pInsert^.Name)
          else
            pInsert^.index := -1;
        end;
      end;
  end;
end;

procedure BeginDWGImport(var ZContext: TZDrawingContext);
var
  ByLayerLT: PGDBLtypeProp;
  SysLayer: PGDBLayerProp;
  StdStyle: PGDBTextStyle;
  StdDimStyle: PGDBDimStyle;
begin
  if LoadCtx <> nil then begin
    zDebugLn(['{WHM}DWG load context already active; force-resetting']);
    FreeAndNil(LoadCtx);
  end;
  LoadCtx := TDWGZCADLoadContext.Create;
  LoadDrawing := ZContext.PDrawing;
  LoadHasCurrentLayerHandle := False;
  LoadCurrentLayerHandle := 0;
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
  StdDimStyle := DWGEnsureDimStyle(ZContext.PDrawing^);
  LoadCtx.SetFallbackDimStyle(StdDimStyle);

  LoadCtx.RegisterShell(0, dokModelSpace, ZContext.PDrawing^.pObjRoot, -1);
end;

procedure ScanDWGImport(var Raw: Dwg_Data);
begin
  // R4 (TZ §3.4): Phase 1 raw scan runs between BeginDWGImport and
  // parseDwg_Data. No-op when the loader is inactive (legacy callers that
  // skipped the lifecycle hooks).
  if LoadCtx = nil then
    Exit;
  LoadHasCurrentLayerHandle :=
    DWGHeaderCurrentLayerHandleValue(Raw, LoadCurrentLayerHandle);
  ScanRawObjects(Raw, LoadCtx);
end;

procedure EndDWGImport(var ZContext: TZDrawingContext);
begin
  if LoadCtx = nil then
    Exit;
  try
    // Phase 3: resolve refs then owners. Attach callbacks fire during
    // ResolveOwners but only do the AddMi step now — geometry builds in
    // Phase 4 below.
    LoadCtx.ResolveRefs;
    ApplyDWGCurrentLayer(ZContext);
    LoadCtx.ResolveOwners;
    zDebugLn(['{WH}DWG owner resolve: attached=', LoadCtx.AttachCount,
      ', fallback=', LoadCtx.FallbackCount,
      ', cycles=', LoadCtx.CycleCount,
      ', refs_attached=', LoadCtx.RefAttachCount,
      ', refs_fallback=', LoadCtx.RefFallbackCount,
      ', warnings=', LoadCtx.WarningCount]);
    // R7 (TZ §3.7): Phase 4 mirrors the DXF post-processing chain
    // (BuildGeometry / FormatAfterDXFLoad / FromDXFPostProcessAfterAdd).
    FinalizeImport(LoadCtx, ZContext.PDrawing, ZContext.DC);
  finally
    FreeAndNil(LoadCtx);
    LoadDrawing := nil;
    LoadHasCurrentLayerHandle := False;
    LoadCurrentLayerHandle := 0;
  end;
end;

procedure DWGRegisterEntityShell(pobj: PGDBObjEntity;
  var DWGObject: Dwg_Object;
  WantTextStyle: Boolean; TextStyleHandle: QWord;
  AKind: TDWGZCADObjectKind);
var
  EntityHandle, OwnerHandle, LayerHandle, LtypeHandle: QWord;
  LtypeKind: TDWGEntityLineTypeKind;
  LtypeFallback: PGDBLtypeProp;
  LtypeInline: Boolean;
  CommonProps: TDWGEntityCommonProps;
  EntMode: Integer;
begin
  if LoadCtx = nil then
    Exit;
  EntityHandle := DWGObjectHandleValue(DWGObject);
  if not DWGObjectOwnerHandleValue(DWGObject, OwnerHandle) then
    OwnerHandle := 0;
  // Issue #1120: when entmode is 1 (paper) or 2 (model) the owner is implicit
  // and DWGObjectOwnerHandleValue tries Dwg^.mspace_block / pspace_block,
  // header_vars.BLOCK_RECORD_*SPACE and block_control.*_space in turn. When
  // all three paths fail OwnerHandle stays 0, the resolver attaches via
  // arNullOwner and the segments only render under the fallback root. Log a
  // hint so future regressions surface in the build log instead of looking
  // like a generic "{WHM} ... attached via fallback (null owner)".
  EntMode := -1;
  if (DWGObject.supertype = DWG_SUPERTYPE_ENTITY) and
     (DWGObject.tio.entity <> nil) then
    EntMode := DWGObject.tio.entity^.entmode;
  if (OwnerHandle = 0) and ((EntMode = 1) or (EntMode = 2)) then
    zDebugLn(['{WH}entmode=', EntMode,
      ' implicit owner unresolved for entity ',
      IntToHex(EntityHandle, 1),
      ' (mspace/pspace_block, header_vars.BLOCK_RECORD_*SPACE,',
      ' block_control.*_space and ownerhandle all empty)']);
  if EntityHandle <> 0 then
    LoadCtx.RegisterShell(EntityHandle, AKind, pobj, -1);
  LoadCtx.QueueOwnerResolve(pobj, EntityHandle, OwnerHandle);

  if DWGEntityCommonPropsValue(DWGObject, CommonProps) then begin
    pobj^.vp.Color := CommonProps.ColorIndex;
    pobj^.vp.LineWeight := CommonProps.LineWeight;
    pobj^.vp.LineTypeScale := CommonProps.LineTypeScale;
    if CommonProps.Invisible then
      zDebugLn(['{WH}DWG entity ', IntToHex(EntityHandle, 1),
        ' is marked invisible in the DWG common entity flags']);
  end;

  if not DWGEntityLayerHandleValue(DWGObject, LayerHandle) then
    LayerHandle := 0;
  if DWGEntityLineTypeRefValue(DWGObject, LtypeKind, LtypeHandle) then begin
    if LtypeKind <> dltHandle then begin
      LtypeHandle := 0;
      LtypeFallback := DWGSystemLineTypeForKind(LtypeKind);
      LtypeInline := LtypeFallback <> nil;
    end else
    begin
      LtypeFallback := nil;
      LtypeInline := False;
    end;
  end else begin
    LtypeHandle := 0;
    LtypeKind := dltMissing;
    LtypeFallback := nil;
    LtypeInline := False;
  end;
  if DWGObject.fixedtype = DWG_TYPE_LINE then
    zDebugLn(['{WH}DWG LINE shell handle=', IntToHex(EntityHandle, 1),
      ', entmode=', EntMode,
      ', owner=', IntToHex(OwnerHandle, 1),
      ', layer_ref=', IntToHex(LayerHandle, 1),
      ', ltype_kind=', DWGEntityLineTypeKindToText(LtypeKind),
      ', ltype_ref=', IntToHex(LtypeHandle, 1),
      ', ltype_flags=', CommonProps.LineTypeFlags,
      ', color=', CommonProps.ColorIndex,
      ', lineweight=', CommonProps.LineWeight,
      ', ltscale=', FloatToStr(CommonProps.LineTypeScale),
      ', invisible=', BoolToStr(CommonProps.Invisible, True)]);
  LoadCtx.QueueRefResolve(pobj, EntityHandle, LayerHandle,
    dokLayer, rsLayer, nil);
  LoadCtx.QueueRefResolve(pobj, EntityHandle, LtypeHandle,
    dokLineType, rsLineType, LtypeFallback, LtypeInline);
  if WantTextStyle then
    LoadCtx.QueueRefResolve(pobj, EntityHandle, TextStyleHandle,
      dokTextStyle, rsTextStyle, nil);
end;

initialization
finalization
  if LoadCtx <> nil then
    FreeAndNil(LoadCtx);
end.
