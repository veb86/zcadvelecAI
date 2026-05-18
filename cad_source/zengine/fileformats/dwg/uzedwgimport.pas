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
  uzedwgdiagnostics,
  uzedwgloadcontext,
  uzedwgrawscan,
  uzedwgblockreserve,
  uzedwgsidefiles,
  uzedwgentityregistry,
  uzedwgtargetedlog,
  uzedwglog,
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
procedure BeginDWGImport(var ZContext: TZDrawingContext;
  const ASourcePath: String = '');
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
procedure DWGCaptureActiveVPortView(const Props: TDWGViewProps);

{ Stage 5 helper extracted from uzefflibredwg2ents.pas: register the entity
  shell + pending owner + layer/linetype/textstyle refs in one call. The
  scalar TextStyleHandle overload is preserved for old callers; TEXT/MTEXT
  mappers pass the raw BITCODE_H so alternate decoded handles survive. }
procedure DWGRegisterEntityShell(pobj: PGDBObjEntity;
  var DWGObject: Dwg_Object;
  WantTextStyle: Boolean; TextStyleHandle: QWord;
  AKind: TDWGZCADObjectKind = dokEntity);
procedure DWGRegisterEntityShellWithTextStyleRef(pobj: PGDBObjEntity;
  var DWGObject: Dwg_Object; TextStyleRef: BITCODE_H;
  AKind: TDWGZCADObjectKind = dokEntity);

implementation

var
  LoadCtx: TDWGZCADLoadContext = nil;
  LoadDrawing: PTSimpleDrawing = nil;
  LoadHasCurrentLayerHandle: Boolean = False;
  LoadCurrentLayerHandle: QWord = 0;
  LoadHasCurrentLineTypeHandle: Boolean = False;
  LoadCurrentLineTypeHandle: QWord = 0;
  LoadHasCurrentTextStyleHandle: Boolean = False;
  LoadCurrentTextStyleHandle: QWord = 0;
  LoadHasCurrentDimStyleHandle: Boolean = False;
  LoadCurrentDimStyleHandle: QWord = 0;
  LoadHasHeaderEntityProps: Boolean = False;
  LoadHeaderEntityProps: TDWGHeaderCurrentEntityProps;
  LoadHasHeaderViewProps: Boolean = False;
  LoadHeaderViewProps: TDWGViewProps;
  LoadHasActiveVPortViewProps: Boolean = False;
  LoadActiveVPortViewProps: TDWGViewProps;
  { Issue #1198 P3: source DWG path threaded through Begin/EndDWGImport so
    the side-file writer can drop *.summary.txt / *.handles.csv next to it.
    Empty string means "no path was supplied" (legacy callers, unit tests). }
  LoadSourcePath: String = '';

function DWGDefaultTextStyleProp: GDBTextStyleProp;
begin
  Result.size := 0;
  Result.oblique := 0;
  Result.wfactor := 1;
end;

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

procedure DWGCaptureActiveVPortView(const Props: TDWGViewProps);
begin
  if (LoadCtx = nil) or (Props.Height <= 0) then
    Exit;
  LoadActiveVPortViewProps := Props;
  LoadHasActiveVPortViewProps := True;
  DWGLogInfoFormatStr(
    'DWG active VPORT view: center=(%s, %s), height=%s, width=%s, has_width=%s, space=%s',
    [FloatToStr(Props.CenterX), FloatToStr(Props.CenterY),
     FloatToStr(Props.Height), FloatToStr(Props.Width),
     BoolToStr(Props.HasWidth, True), DWGViewSpaceToText(Props.Space)]);
end;

function DWGEnsureTextStyle(var Drawing: TSimpleDrawing): PGDBTextStyle;
var
  TextProp: GDBTextStyleProp;
begin
  Result := Drawing.TextStyleTable.FindStyle('Standard', False);
  if Result = nil then begin
    TextProp := DWGDefaultTextStyleProp;
    Result := Drawing.TextStyleTable.addstyle('Standard', '', '',
      TextProp, False);
  end;
  if Drawing.CurrentTextStyle = nil then
    Drawing.CurrentTextStyle := Result;
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
      DWGLogWarningFormatStr(
        'DWG current layer handle %s did not resolve to a layer; using system layer',
        [DWGHandleLogText(LoadCurrentLayerHandle)]);
  end;
  if CurrentLayer = nil then
    CurrentLayer := ZContext.PDrawing^.LayerTable.GetSystemLayer;
  ZContext.PDrawing^.CurrentLayer := CurrentLayer;
  if CurrentLayer <> nil then
    DWGLogInfoFormatStr('DWG current layer -> %s', [CurrentLayer^.Name]);
end;

procedure ApplyDWGCurrentLineType(var ZContext: TZDrawingContext);
var
  Entry: TDWGZCADHandleEntry;
  CurrentLType: PGDBLtypeProp;
begin
  CurrentLType := nil;
  if (LoadCtx <> nil) and LoadHasCurrentLineTypeHandle then begin
    if LoadCtx.TryGetEntry(LoadCurrentLineTypeHandle, Entry) and
       (Entry.Kind = dokLineType) then
      CurrentLType := PGDBLtypeProp(Entry.Ptr);
    if CurrentLType = nil then
      DWGLogWarningFormatStr(
        'DWG current linetype handle %s did not resolve to a linetype; using ByLayer',
        [DWGHandleLogText(LoadCurrentLineTypeHandle)]);
  end;
  if CurrentLType = nil then
    CurrentLType := DWGSystemLineTypeForKind(dltByLayer);
  ZContext.PDrawing^.CurrentLType := CurrentLType;
  if CurrentLType <> nil then
    DWGLogInfoFormatStr('DWG current linetype -> %s', [CurrentLType^.Name]);
end;

procedure ApplyDWGCurrentTextStyle(var ZContext: TZDrawingContext);
var
  Entry: TDWGZCADHandleEntry;
  CurrentStyle: PGDBTextStyle;
begin
  CurrentStyle := nil;
  if (LoadCtx <> nil) and LoadHasCurrentTextStyleHandle then begin
    if LoadCtx.TryGetEntry(LoadCurrentTextStyleHandle, Entry) and
       (Entry.Kind = dokTextStyle) then
      CurrentStyle := PGDBTextStyle(Entry.Ptr);
    if CurrentStyle = nil then
      DWGLogWarningFormatStr(
        'DWG current textstyle handle %s did not resolve to a text style; using Standard',
        [DWGHandleLogText(LoadCurrentTextStyleHandle)]);
  end;
  if (CurrentStyle = nil) or CurrentStyle^.UsedInLTYPE then
    CurrentStyle := DWGEnsureTextStyle(ZContext.PDrawing^);
  ZContext.PDrawing^.CurrentTextStyle := CurrentStyle;
  if CurrentStyle <> nil then
    DWGLogInfoFormatStr('DWG current textstyle -> %s', [CurrentStyle^.Name]);
end;

procedure ApplyDWGCurrentDimStyle(var ZContext: TZDrawingContext);
var
  Entry: TDWGZCADHandleEntry;
  CurrentDimStyle: PGDBDimStyle;
begin
  CurrentDimStyle := nil;
  if (LoadCtx <> nil) and LoadHasCurrentDimStyleHandle then begin
    if LoadCtx.TryGetEntry(LoadCurrentDimStyleHandle, Entry) and
       (Entry.Kind = dokDimStyle) then
      CurrentDimStyle := PGDBDimStyle(Entry.Ptr);
    if CurrentDimStyle = nil then
      DWGLogWarningFormatStr(
        'DWG current dimstyle handle %s did not resolve to a dimstyle; using Standard',
        [DWGHandleLogText(LoadCurrentDimStyleHandle)]);
  end;
  if CurrentDimStyle = nil then
    CurrentDimStyle := DWGEnsureDimStyle(ZContext.PDrawing^);
  ZContext.PDrawing^.CurrentDimStyle := CurrentDimStyle;
  if CurrentDimStyle <> nil then
    DWGLogInfoFormatStr('DWG current dimstyle -> %s', [CurrentDimStyle^.Name]);
end;

procedure ApplyDWGHeaderEntityProps(var ZContext: TZDrawingContext);
begin
  if not LoadHasHeaderEntityProps then
    Exit;
  ZContext.PDrawing^.CColor := LoadHeaderEntityProps.ColorIndex;
  ZContext.PDrawing^.CurrentLineW := LoadHeaderEntityProps.LineWeight;
  ZContext.PDrawing^.CLTScale := LoadHeaderEntityProps.LineTypeScale;
  ZContext.PDrawing^.LTScale := LoadHeaderEntityProps.GlobalLineTypeScale;
  ZContext.PDrawing^.LWDisplay := LoadHeaderEntityProps.LineWeightDisplay;
  DWGLogInfoFormatStr(
    'DWG current entity defaults -> color=%d, lineweight=%d, celtscale=%s, ltscale=%s, lwdisplay=%s',
    [ZContext.PDrawing^.CColor, ZContext.PDrawing^.CurrentLineW,
     FloatToStr(ZContext.PDrawing^.CLTScale),
     FloatToStr(ZContext.PDrawing^.LTScale),
     BoolToStr(ZContext.PDrawing^.LWDisplay, True)]);
end;

function DWGSelectViewProps(out Props: TDWGViewProps; out Source: string): Boolean;
begin
  if LoadHasActiveVPortViewProps then begin
    Props := LoadActiveVPortViewProps;
    Source := 'active VPORT';
    Exit(True);
  end;
  if LoadHasHeaderViewProps and (LoadHeaderViewProps.Space = dvsModelSpace) then
  begin
    Props := LoadHeaderViewProps;
    Source := 'header';
    Exit(True);
  end;
  if LoadHasHeaderViewProps and (LoadHeaderViewProps.Space = dvsPaperSpace) then
    DWGLogInfoFormatStr(
      'DWG header view is paper-space; ignoring it because ZCAD opens DWG drawings in model space',
      []);
  Result := False;
end;

procedure ApplyDWGViewState(var ZContext: TZDrawingContext);
var
  Props: TDWGViewProps;
  Source: string;
  ViewHeightZoom, ViewWidthZoom: Double;
begin
  if ZContext.LoadMode <> TLOLoad then
    Exit;
  if (ZContext.PDrawing = nil) or (ZContext.PDrawing^.pcamera = nil) then
    Exit;
  if not DWGSelectViewProps(Props, Source) then
    Exit;

  ZContext.PDrawing^.pcamera^.prop.point.x := -Props.CenterX;
  ZContext.PDrawing^.pcamera^.prop.point.y := -Props.CenterY;

  if (ZContext.PDrawing^.wa <> nil) and
     (ZContext.PDrawing^.wa.getviewcontrol <> nil) and
     (ZContext.PDrawing^.wa.getviewcontrol.ClientHeight > 0) then
  begin
    ViewHeightZoom := Props.Height /
      ZContext.PDrawing^.wa.getviewcontrol.ClientHeight;
    if ViewHeightZoom > 0 then
      ZContext.PDrawing^.pcamera^.prop.zoom := ViewHeightZoom;
    if Props.HasWidth and
       (ZContext.PDrawing^.wa.getviewcontrol.ClientWidth > 0) then
    begin
      ViewWidthZoom := Props.Width /
        ZContext.PDrawing^.wa.getviewcontrol.ClientWidth;
      if ViewWidthZoom > ZContext.PDrawing^.pcamera^.prop.zoom then
        ZContext.PDrawing^.pcamera^.prop.zoom := ViewWidthZoom;
    end;
  end;

  DWGLogInfoFormatStr(
    'DWG view from %s applied: camera=(%s, %s), zoom=%s',
    [Source, FloatToStr(ZContext.PDrawing^.pcamera^.prop.point.x),
     FloatToStr(ZContext.PDrawing^.pcamera^.prop.point.y),
     FloatToStr(ZContext.PDrawing^.pcamera^.prop.zoom)]);
end;

{ Issue #1198 P4: locate the DWG handle for an entity pointer by walking
  the pending-owner list. Used by the attach callbacks to feed
  LoadCtx.ShouldEmitDetail so the dedup key matches what the resolver
  put into the warning aggregate. Returns 0 if the entity isn't
  queued (legacy callers and Stage 2 fallback paths). }
function DWGOwnerEntityHandleForLog(Entity: Pointer): QWord;
var
  I: Integer;
  Pending: PDWGZCADPendingOwner;
begin
  Result := 0;
  if LoadCtx = nil then
    Exit;
  for I := 0 to LoadCtx.PendingOwners.Count - 1 do begin
    Pending := LoadCtx.PendingOwners.ItemAt(I);
    if Pending^.Entity = Entity then
      Exit(Pending^.EntityHandle);
  end;
end;

function DWGOwnerHandleForLog(Entity: Pointer): QWord;
var
  I: Integer;
  Pending: PDWGZCADPendingOwner;
begin
  Result := 0;
  if LoadCtx = nil then
    Exit;
  for I := 0 to LoadCtx.PendingOwners.Count - 1 do begin
    Pending := LoadCtx.PendingOwners.ItemAt(I);
    if Pending^.Entity = Entity then
      Exit(Pending^.OwnerHandle);
  end;
end;

function DWGRefEntityHandleForLog(Entity: Pointer;
  Slot: TDWGZCADRefSlot): QWord;
var
  I: Integer;
  Pending: PDWGZCADPendingRef;
begin
  Result := 0;
  if LoadCtx = nil then
    Exit;
  for I := 0 to LoadCtx.PendingRefs.Count - 1 do begin
    Pending := LoadCtx.PendingRefs.ItemAt(I);
    if (Pending^.Entity = Entity) and (Pending^.Slot = Slot) then
      Exit(Pending^.EntityHandle);
  end;
end;

function DWGRefHandleForLog(Entity: Pointer;
  Slot: TDWGZCADRefSlot): QWord;
var
  I: Integer;
  Pending: PDWGZCADPendingRef;
begin
  Result := 0;
  if LoadCtx = nil then
    Exit;
  for I := 0 to LoadCtx.PendingRefs.Count - 1 do begin
    Pending := LoadCtx.PendingRefs.ItemAt(I);
    if (Pending^.Entity = Entity) and (Pending^.Slot = Slot) then
      Exit(Pending^.RefHandle);
  end;
end;

{ Issue #1198 P4: gate a per-entity fallback detail line through the
  warning list's dedup tracker. Returns True if the caller should emit
  the DWG log line (first occurrence for this Code+Handle pair) and
  False otherwise — the resolver has already booked the occurrence in
  the aggregate, so the EndDWGImport summary still reports it. When
  there is no active load context (legacy callers) the gate stays open
  so the line is always written. }
function DWGShouldEmitFallbackDetail(Reason: TDWGAttachReason;
  EntityHandle: QWord): Boolean;
var
  Code: Integer;
begin
  if LoadCtx = nil then
    Exit(True);
  Code := DWGCodeForAttachReason(Reason);
  if Code = 0 then
    Exit(True);
  Result := LoadCtx.ShouldEmitDetail(Code, EntityHandle);
end;

procedure DWGAttachEntity(Entity: Pointer; Owner: Pointer;
  Reason: TDWGAttachReason; Data: Pointer);
var
  pobj: PGDBObjEntity;
  newowner: PGDBObjGenericSubEntry;
  EntityHandle: QWord;
  OwnerHandle: QWord;
begin
  // Reason is forwarded to the logger so unresolved fallbacks are visible to
  // human reviewers without a separate diagnostic pass.
  pobj := PGDBObjEntity(Entity);
  if (pobj = nil) or (Owner = nil) then
    Exit;

  EntityHandle := DWGOwnerEntityHandleForLog(Entity);
  OwnerHandle := DWGOwnerHandleForLog(Entity);
  DWGLogInfoFormatStr(
    'DWG [attach] entity=%s owner=%s entity_ptr=%p owner_ptr=%p reason=%s fallback=%s owner_is_insert=%s',
    [DWGHandleLogText(EntityHandle), DWGHandleLogText(OwnerHandle), Entity,
     Owner, DWGAttachReasonToText(Reason), BoolToStr(Reason <> arResolved, True),
     BoolToStr(DWGPointerHasKind(Owner, dokBlockInsert), True)]);

  // Issue #1203: точечный лог факта присоединения сущности к владельцу.
  // Срабатывает, когда целевой handle добрался до фазы attach (т.е. shell
  // зарегистрирован, владелец разрешён). Если этого сообщения нет в логе —
  // объект был отсеян раньше: либо его не было в Phase 1 сканере, либо
  // mapper не зарегистрировал shell, либо resolver не нашёл владельца.
  if TargetedLogHandle(EntityHandle) then
    TargetedLog('attach', EntityHandle,
      Format('reason=%s owner_is_insert=%s',
        [DWGAttachReasonToText(Reason),
         BoolToStr(DWGPointerHasKind(Owner, dokBlockInsert), True)]));

  // INSERT-owned ATTRIB entities are appended after the INSERT has built its
  // block geometry. Adding them now would be undone by BuildGeometry clearing
  // ConstObjArray from the block definition.
  if DWGPointerHasKind(Owner, dokBlockInsert) then begin
    pobj^.bp.ListPos.Owner := PGDBObjEntity(Owner);
    if (Reason <> arResolved) and
       DWGShouldEmitFallbackDetail(Reason, EntityHandle) then
      DWGLogWarningFormatStr(
        'entity %s deferred under INSERT via fallback (%s)',
        [HexStr(PtrUInt(pobj), 16), DWGAttachReasonToText(Reason)]);
    Exit;
  end;

  newowner := PGDBObjGenericSubEntry(Owner);

  newowner^.AddMi(PGDBObjSubordinated(pobj));
  if (Reason <> arResolved) and
     DWGShouldEmitFallbackDetail(Reason, EntityHandle) then
    DWGLogWarningFormatStr('entity %s attached via fallback (%s)',
      [HexStr(PtrUInt(pobj), 16), DWGAttachReasonToText(Reason)]);

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
  EntityHandle: QWord;
  RefHandle: QWord;
begin
  EntityHandle := DWGRefEntityHandleForLog(Entity, Slot);
  RefHandle := DWGRefHandleForLog(Entity, Slot);
  DWGLogInfoFormatStr(
    'DWG [attach-ref] entity=%s ref=%s slot=%s entity_ptr=%p ref_ptr=%p reason=%s fallback=%s',
    [DWGHandleLogText(EntityHandle), DWGHandleLogText(RefHandle),
     DWGRefSlotToLogText(Slot), Entity, Ref, DWGAttachReasonToText(Reason),
     BoolToStr(Reason <> arResolved, True)]);
  // Issue #1203: точечный лог разрешения ссылки. Полезен, чтобы понять,
  // на какой слот (layer/linetype/textstyle/dimstyle/blockdef) ушёл
  // fallback и в каком состоянии (Reason).
  if TargetedLogHandle(EntityHandle) then
    TargetedLog('attach-ref', EntityHandle,
      Format('slot=%d reason=%s ref=%s',
        [Ord(Slot), DWGAttachReasonToText(Reason),
         BoolToStr(Ref <> nil, True)]));
  case Slot of
    rsLayer:
      begin
        pobj := PGDBObjEntity(Entity);
        if pobj = nil then
          Exit;
        pobj^.vp.Layer := PGDBLayerProp(Ref);
        if (Reason <> arResolved) and
           DWGShouldEmitFallbackDetail(Reason, EntityHandle) then
          DWGLogWarningFormatStr(
            'entity %s %s layer fallback (%s) -> %s',
            [HexStr(PtrUInt(pobj), 16), DWGRefHandlesForLog(Entity, Slot),
             DWGAttachReasonToText(Reason),
             DWGLayerNameForLog(PGDBLayerProp(Ref))]);
      end;
    rsLineType:
      begin
        pobj := PGDBObjEntity(Entity);
        if pobj = nil then
          Exit;
        pobj^.vp.LineType := PGDBLtypeProp(Ref);
        if (Reason <> arResolved) and
           DWGShouldEmitFallbackDetail(Reason, EntityHandle) then
          DWGLogWarningFormatStr(
            'entity %s %s linetype fallback (%s, layer=%s) -> %s',
            [HexStr(PtrUInt(pobj), 16), DWGRefHandlesForLog(Entity, Slot),
             DWGAttachReasonToText(Reason), DWGLayerNameForLog(pobj^.vp.Layer),
             DWGLTypeNameForLog(PGDBLtypeProp(Ref))]);
      end;
    rsLayerLineType:
      begin
        player := PGDBLayerProp(Entity);
        if player = nil then
          Exit;
        player^.LT := PGDBLtypeProp(Ref);
        if Reason <> arResolved then begin
          if DWGShouldEmitFallbackDetail(Reason, EntityHandle) then
            DWGLogWarningFormatStr(
              'layer %s %s linetype fallback (%s) -> %s',
              [DWGLayerNameForLog(player), DWGRefHandlesForLog(Entity, Slot),
               DWGAttachReasonToText(Reason),
               DWGLTypeNameForLog(PGDBLtypeProp(Ref))]);
        end
        else
          DWGLogInfoFormatStr('layer %s %s linetype -> %s',
            [DWGLayerNameForLog(player), DWGRefHandlesForLog(Entity, Slot),
             DWGLTypeNameForLog(PGDBLtypeProp(Ref))]);
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
        if (Reason <> arResolved) and
           DWGShouldEmitFallbackDetail(Reason, EntityHandle) then
          DWGLogWarningFormatStr(
            'entity %s %s textstyle fallback (%s)',
            [HexStr(PtrUInt(pobj), 16), DWGRefHandlesForLog(Entity, Slot),
             DWGAttachReasonToText(Reason)]);
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
        if (Reason <> arResolved) and
           DWGShouldEmitFallbackDetail(Reason, EntityHandle) then
          DWGLogWarningFormatStr(
            'entity %s %s dimstyle fallback (%s)',
            [HexStr(PtrUInt(pobj), 16), DWGRefHandlesForLog(Entity, Slot),
             DWGAttachReasonToText(Reason)]);
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
            DWGLogWarningFormatStr(
              'entity %s %s block ref resolves to model/paper space; using empty block',
              [HexStr(PtrUInt(pobj), 16),
               DWGRefHandlesForLog(Entity, Slot)])
          else if DWGShouldEmitFallbackDetail(Reason, EntityHandle) then
            DWGLogWarningFormatStr('entity %s %s block fallback (%s)',
              [HexStr(PtrUInt(pobj), 16), DWGRefHandlesForLog(Entity, Slot),
               DWGAttachReasonToText(Reason)]);
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

procedure BeginDWGImport(var ZContext: TZDrawingContext;
  const ASourcePath: String = '');
var
  ByLayerLT: PGDBLtypeProp;
  SysLayer: PGDBLayerProp;
  StdStyle: PGDBTextStyle;
  StdDimStyle: PGDBDimStyle;
begin
  if LoadCtx <> nil then begin
    DWGLogWarningFormatStr('DWG load context already active; force-resetting',
      []);
    FreeAndNil(LoadCtx);
  end;
  // Issue #1203: перечитываем список целевых handle'ов из константы
  // в самом начале импорта. Если DWG_TARGET_HANDLE_LIST пуста — последующие
  // вызовы TargetedLogXxx будут no-op'ами; если задана — каждое прохождение
  // целевого handle через ключевые точки конвейера будет залогировано.
  TargetedLogRefresh;
  LoadCtx := TDWGZCADLoadContext.Create;
  LoadDrawing := ZContext.PDrawing;
  LoadSourcePath := ASourcePath;
  LoadHasCurrentLayerHandle := False;
  LoadCurrentLayerHandle := 0;
  LoadHasCurrentLineTypeHandle := False;
  LoadCurrentLineTypeHandle := 0;
  LoadHasCurrentTextStyleHandle := False;
  LoadCurrentTextStyleHandle := 0;
  LoadHasCurrentDimStyleHandle := False;
  LoadCurrentDimStyleHandle := 0;
  LoadHasHeaderEntityProps := False;
  LoadHasHeaderViewProps := False;
  LoadHasActiveVPortViewProps := False;
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
  // Ensure the DXF-compatible Standard style exists before table and entity
  // mappers start resolving text-style handles.
  StdStyle := DWGEnsureTextStyle(ZContext.PDrawing^);
  LoadCtx.SetFallbackTextStyle(StdStyle);
  StdDimStyle := DWGEnsureDimStyle(ZContext.PDrawing^);
  LoadCtx.SetFallbackDimStyle(StdDimStyle);

  LoadCtx.RegisterShell(0, dokModelSpace, ZContext.PDrawing^.pObjRoot, -1);
  DWGLogInfoFormatStr('DWG [begin] source=%s drawing=%p root=%p',
    [ASourcePath, ZContext.PDrawing, ZContext.PDrawing^.pObjRoot]);
end;

function DWGHeaderHandleForLog(HasHandle: Boolean; Handle: QWord): string;
begin
  if HasHandle then
    Result := IntToHex(Handle, 1)
  else
    Result := '(missing)';
end;

procedure ScanDWGImport(var Raw: Dwg_Data);
var
  HandlesBefore: Integer;
begin
  // R4 (TZ §3.4): Phase 1 raw scan runs between BeginDWGImport and
  // parseDwg_Data. No-op when the loader is inactive (legacy callers that
  // skipped the lifecycle hooks).
  if LoadCtx = nil then
    Exit;
  LoadHasCurrentLayerHandle :=
    DWGHeaderCurrentLayerHandleValue(Raw, LoadCurrentLayerHandle);
  LoadHasCurrentLineTypeHandle :=
    DWGHeaderCurrentLineTypeHandleValue(Raw, LoadCurrentLineTypeHandle);
  LoadHasCurrentTextStyleHandle :=
    DWGHeaderCurrentTextStyleHandleValue(Raw, LoadCurrentTextStyleHandle);
  LoadHasCurrentDimStyleHandle :=
    DWGHeaderCurrentDimStyleHandleValue(Raw, LoadCurrentDimStyleHandle);
  LoadHasHeaderEntityProps :=
    DWGHeaderCurrentEntityPropsValue(Raw, LoadHeaderEntityProps);
  LoadHasHeaderViewProps :=
    DWGHeaderViewPropsValue(Raw, LoadHeaderViewProps);
  if LoadHasHeaderEntityProps then
    DWGLogInfoFormatStr(
      'DWG header defaults: CLAYER=%s, CELTYPE=%s, TEXTSTYLE=%s, DIMSTYLE=%s, CECOLOR=%d, CELWEIGHT=%d, CELTSCALE=%s, LTSCALE=%s, LWDISPLAY=%s',
      [DWGHeaderHandleForLog(LoadHasCurrentLayerHandle, LoadCurrentLayerHandle),
       DWGHeaderHandleForLog(LoadHasCurrentLineTypeHandle, LoadCurrentLineTypeHandle),
       DWGHeaderHandleForLog(LoadHasCurrentTextStyleHandle, LoadCurrentTextStyleHandle),
       DWGHeaderHandleForLog(LoadHasCurrentDimStyleHandle, LoadCurrentDimStyleHandle),
       LoadHeaderEntityProps.ColorIndex, LoadHeaderEntityProps.LineWeight,
       FloatToStr(LoadHeaderEntityProps.LineTypeScale),
       FloatToStr(LoadHeaderEntityProps.GlobalLineTypeScale),
       BoolToStr(LoadHeaderEntityProps.LineWeightDisplay, True)]);
  if LoadHasHeaderViewProps then
    DWGLogInfoFormatStr('DWG header view: center=(%s, %s), height=%s, space=%s',
      [FloatToStr(LoadHeaderViewProps.CenterX),
       FloatToStr(LoadHeaderViewProps.CenterY),
       FloatToStr(LoadHeaderViewProps.Height),
       DWGViewSpaceToText(LoadHeaderViewProps.Space)]);
  if LoadDrawing <> nil then
    DWGReserveBlockDefCapacity(Raw, LoadDrawing^.BlockDefArray);
  HandlesBefore := LoadCtx.Handles.Count;
  ScanRawObjects(Raw, LoadCtx);
  DWGLogInfoFormatStr(
    'DWG [scan-summary] classes=%d objects=%d alloced_objects=%d entities=%d object_refs=%d handles_registered=%d handles_total=%d',
    [Raw.num_classes, Raw.num_objects, Raw.num_alloced_objects,
     Raw.num_entities, Raw.num_object_refs, LoadCtx.Handles.Count - HandlesBefore,
     LoadCtx.Handles.Count]);
end;

{ Issue #1198 P4: emit one summary line per diagnostic code observed
  during the import. The first occurrence of each (Code, Handle) pair
  was already written to the log by the resolver / attach callbacks;
  this routine adds the "and N more like this" aggregate so the
  bottom of the import log doubles as the warning-by-code histogram. }
procedure DWGEmitWarningSummary(Ctx: TDWGZCADLoadContext);
var
  I: Integer;
  Agg: TDWGImportCodeAggregate;
  Suppressed: Integer;
begin
  if Ctx = nil then
    Exit;
  for I := 0 to Ctx.WarningAggregateCount - 1 do begin
    Agg := Ctx.WarningAggregateAt(I);
    if Agg.TotalCount <= 0 then
      Continue;
    Suppressed := Agg.TotalCount - Agg.DistinctHandles;
    if Suppressed < 0 then
      Suppressed := 0;
    DWGLogWarningFormatStr(
      'DWG warning summary code=%d (%s): %d occurrence(s) across %d handle(s); %d duplicate(s) suppressed from main log',
      [Agg.Code, DWGWarningCodeToShortName(Agg.Code), Agg.TotalCount,
       Agg.DistinctHandles, Suppressed]);
  end;
end;

{ Issue #1198 P2 (TZ §5): emit the fixedtype histogram after Phase 1 has
  populated the handle map. The cross-check the audit asks for is exactly
  "this is what arrived in the file, here is which fixedtypes had a registered
  handler and which did not". The log is gated on the diagnostic mode so the
  default load stays quiet; explicit summary/full/trace requests print one
  line per non-empty fixedtype bucket plus a tail showing how many fixedtypes
  hit the no-handler branch. }
procedure DWGEmitFixedTypeHistogram(Ctx: TDWGZCADLoadContext);
var
  FixedTypes: TDWGFixedTypeCounterArray;
  I: Integer;
  Mode: TDWGDiagMode;
  Total, Unhandled, UnhandledFT: Integer;
  HasH: Boolean;
begin
  if Ctx = nil then
    Exit;
  Mode := DWGDiagModeFromConst;
  if Mode = dmOff then
    Exit;
  DWGCountByFixedType(Ctx, FixedTypes);
  Total := 0;
  Unhandled := 0;
  UnhandledFT := 0;
  for I := 0 to High(FixedTypes) do begin
    HasH := HasHandlerFor(FixedTypes[I].FixedType);
    Inc(Total, FixedTypes[I].Count);
    if not HasH then begin
      Inc(Unhandled, FixedTypes[I].Count);
      Inc(UnhandledFT);
    end;
    DWGLogInfoFormatStr('DWG fixedtype %s: count=%d, has_handler=%s',
      [DWGFixedTypeToText(FixedTypes[I].FixedType), FixedTypes[I].Count,
       BoolToStr(HasH, True)]);
  end;
  DWGLogInfoFormatStr(
    'DWG fixedtype histogram: %d distinct type(s), %d handle(s); %d type(s) without a registered handler (%d handle(s))',
    [Length(FixedTypes), Total, UnhandledFT, Unhandled]);
end;

{ Issue #1198 P3: emit per-DWG diagnostic side-files when the user has
  opted in via DWG_DIAG_MODE=dmSummary|dmFull|dmTrace. Runs after the resolver
  has fully populated PendingOwners / PendingRefs so the CSVs reflect the
  final attach state. Failures during write must not abort the import:
  the side-file writer is strictly diagnostic, so we swallow exceptions
  and log a single warning instead. }
procedure DWGEmitSideFiles(Ctx: TDWGZCADLoadContext; const SourcePath: String);
var
  Mode: TDWGDiagMode;
  Res: TDWGSideFileResult;
  I: Integer;
begin
  if Ctx = nil then
    Exit;
  Mode := DWGDiagModeFromConst;
  if Mode = dmOff then
    Exit;
  try
    Res := DWGWriteSideFiles(Ctx, SourcePath, Mode);
    DWGLogInfoFormatStr('DWG diagnostic side-files (mode=%s): %d file(s) written',
      [DWGDiagModeToString(Mode), Length(Res.FilesWritten)]);
    for I := 0 to High(Res.FilesWritten) do
      DWGLogInfoFormatStr('DWG diagnostic side-file: %s',
        [Res.FilesWritten[I]]);
  except
    on E: Exception do
      DWGLogWarningFormatStr('DWG side-files: failed to write (%s): %s',
        [E.ClassName, E.Message]);
  end;
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
    ApplyDWGCurrentLineType(ZContext);
    ApplyDWGCurrentTextStyle(ZContext);
    ApplyDWGCurrentDimStyle(ZContext);
    ApplyDWGHeaderEntityProps(ZContext);
    ApplyDWGViewState(ZContext);
    LoadCtx.ResolveOwners;
    DWGLogInfoFormatStr(
      'DWG [resolve-summary] handles=%d pending_owners=%d pending_refs=%d attached=%d fallback=%d cycles=%d refs_attached=%d refs_fallback=%d proxy_loaded=%d proxy_failed=%d unknown_entities=%d unknown_objects=%d freed_raw_drops=%d warnings=%d',
      [LoadCtx.Handles.Count, LoadCtx.PendingOwners.Count,
       LoadCtx.PendingRefs.Count, LoadCtx.AttachCount, LoadCtx.FallbackCount,
       LoadCtx.CycleCount, LoadCtx.RefAttachCount, LoadCtx.RefFallbackCount,
       LoadCtx.ProxiesLoaded, LoadCtx.ProxiesFailed, LoadCtx.UnknownEntities,
       LoadCtx.UnknownObjects, LoadCtx.DroppedDueToFreedRaw,
       LoadCtx.WarningCount]);
    DWGEmitWarningSummary(LoadCtx);
    DWGEmitFixedTypeHistogram(LoadCtx);
    DWGEmitSideFiles(LoadCtx, LoadSourcePath);
    // R7 (TZ §3.7): Phase 4 mirrors the DXF post-processing chain
    // (BuildGeometry / FormatAfterDXFLoad / FromDXFPostProcessAfterAdd).
    FinalizeImport(LoadCtx, ZContext.PDrawing, ZContext.DC);
  finally
    FreeAndNil(LoadCtx);
    LoadDrawing := nil;
    LoadSourcePath := '';
    LoadHasCurrentLayerHandle := False;
    LoadCurrentLayerHandle := 0;
    LoadHasCurrentLineTypeHandle := False;
    LoadCurrentLineTypeHandle := 0;
    LoadHasCurrentTextStyleHandle := False;
    LoadCurrentTextStyleHandle := 0;
    LoadHasCurrentDimStyleHandle := False;
    LoadCurrentDimStyleHandle := 0;
    LoadHasHeaderEntityProps := False;
    LoadHasHeaderViewProps := False;
    LoadHasActiveVPortViewProps := False;
  end;
end;

procedure DWGRegisterEntityShellWithTextStyleCandidates(pobj: PGDBObjEntity;
  var DWGObject: Dwg_Object;
  WantTextStyle: Boolean; const TextStyleCandidates: TDWGRefHandleCandidates;
  AKind: TDWGZCADObjectKind);
var
  EntityHandle, OwnerHandle: QWord;
  OwnerCandidates, LayerCandidates, LtypeCandidates: TDWGRefHandleCandidates;
  LtypeKind: TDWGEntityLineTypeKind;
  LtypeFallback: PGDBLtypeProp;
  LtypeInline: Boolean;
  CommonProps: TDWGEntityCommonProps;
  EntMode: Integer;
begin
  if LoadCtx = nil then
    Exit;
  EntityHandle := DWGObjectHandleValue(DWGObject);
  if DWGObjectOwnerHandleCandidatesValue(DWGObject, OwnerCandidates) then
    OwnerHandle := OwnerCandidates.Values[0]
  else begin
    FillChar(OwnerCandidates, SizeOf(OwnerCandidates), 0);
    OwnerHandle := 0;
  end;
  // Issue #1120: when entmode is 1 (paper) or 2 (model) the owner is implicit
  // and DWGObjectOwnerHandleValue tries Dwg^.mspace_block / pspace_block,
  // header_vars.BLOCK_RECORD_*SPACE and block_control.*_space in turn. When
  // all three paths fail OwnerHandle stays 0, the resolver attaches via
  // arNullOwner and the segments only render under the fallback root. Log a
  // hint so future regressions surface in the build log instead of looking
  // like a generic "{WH} ... attached via fallback (null owner)".
  EntMode := -1;
  if (DWGObject.supertype = DWG_SUPERTYPE_ENTITY) and
     (DWGObject.tio.entity <> nil) then
    EntMode := DWGObject.tio.entity^.entmode;
  if (OwnerHandle = 0) and ((EntMode = 1) or (EntMode = 2)) then
    DWGLogWarningFormatStr(
      'entmode=%d implicit owner unresolved for entity %s (mspace/pspace_block, header_vars.BLOCK_RECORD_*SPACE, block_control.*_space and ownerhandle all empty)',
      [EntMode, DWGHandleLogText(EntityHandle)]);
  if EntityHandle <> 0 then
    LoadCtx.RegisterShell(EntityHandle, AKind, pobj, -1);
  // Issue #1203: точечный лог регистрации shell-а сущности. Срабатывает,
  // если EntityHandle или OwnerHandle входят в список целевых.
  TargetedLogPair('register', EntityHandle, OwnerHandle,
    Format('fixedtype=%d kind=%d owner_candidates=%d entmode=%d',
      [Ord(DWGObject.fixedtype), Ord(AKind), OwnerCandidates.Count, EntMode]));
  LoadCtx.QueueOwnerResolveCandidates(pobj, EntityHandle,
    OwnerCandidates.Values, OwnerCandidates.Count);

  if DWGEntityCommonPropsValue(DWGObject, CommonProps) then begin
    pobj^.vp.Color := CommonProps.ColorIndex;
    pobj^.vp.LineWeight := CommonProps.LineWeight;
    pobj^.vp.LineTypeScale := CommonProps.LineTypeScale;
    if CommonProps.Invisible then
      DWGLogInfoFormatStr(
        'DWG entity %s is marked invisible in the DWG common entity flags',
        [DWGHandleLogText(EntityHandle)]);
  end;

  if not DWGEntityLayerHandleCandidatesValue(DWGObject, LayerCandidates) then
    FillChar(LayerCandidates, SizeOf(LayerCandidates), 0);
  if DWGEntityLineTypeRefCandidatesValue(DWGObject, LtypeKind,
    LtypeCandidates) then begin
    if LtypeKind <> dltHandle then begin
      FillChar(LtypeCandidates, SizeOf(LtypeCandidates), 0);
      LtypeFallback := DWGSystemLineTypeForKind(LtypeKind);
      LtypeInline := LtypeFallback <> nil;
    end else
    begin
      LtypeFallback := nil;
      LtypeInline := False;
    end;
  end else begin
    FillChar(LtypeCandidates, SizeOf(LtypeCandidates), 0);
    LtypeKind := dltMissing;
    LtypeFallback := nil;
    LtypeInline := False;
  end;
  //if DWGObject.fixedtype = DWG_TYPE_LINE then
  //  DWGLogInfoFormatStr(
  //    'DWG LINE shell handle=%s, entmode=%d, owner=%s, ltype_kind=%s, color=%d, lineweight=%d, ltscale=%s, invisible=%s',
  //    [DWGHandleLogText(EntityHandle), EntMode, DWGHandleLogText(OwnerHandle),
  //     DWGEntityLineTypeKindToText(LtypeKind), CommonProps.ColorIndex,
  //     CommonProps.LineWeight, FloatToStr(CommonProps.LineTypeScale),
  //     BoolToStr(CommonProps.Invisible, True)]);
  LoadCtx.QueueRefResolveCandidates(pobj, EntityHandle, LayerCandidates.Values,
    LayerCandidates.Count, dokLayer, rsLayer, nil);
  LoadCtx.QueueRefResolveCandidates(pobj, EntityHandle, LtypeCandidates.Values,
    LtypeCandidates.Count, dokLineType, rsLineType, LtypeFallback,
    LtypeInline);
  if WantTextStyle then
    LoadCtx.QueueRefResolveCandidates(pobj, EntityHandle,
      TextStyleCandidates.Values, TextStyleCandidates.Count,
      dokTextStyle, rsTextStyle, nil);
end;

procedure DWGRegisterEntityShell(pobj: PGDBObjEntity;
  var DWGObject: Dwg_Object;
  WantTextStyle: Boolean; TextStyleHandle: QWord;
  AKind: TDWGZCADObjectKind);
var
  TextStyleCandidates: TDWGRefHandleCandidates;
begin
  FillChar(TextStyleCandidates, SizeOf(TextStyleCandidates), 0);
  if TextStyleHandle <> 0 then begin
    TextStyleCandidates.Count := 1;
    TextStyleCandidates.Values[0] := TextStyleHandle;
  end;
  DWGRegisterEntityShellWithTextStyleCandidates(pobj, DWGObject,
    WantTextStyle, TextStyleCandidates, AKind);
end;

procedure DWGRegisterEntityShellWithTextStyleRef(pobj: PGDBObjEntity;
  var DWGObject: Dwg_Object; TextStyleRef: BITCODE_H;
  AKind: TDWGZCADObjectKind);
var
  TextStyleCandidates: TDWGRefHandleCandidates;
begin
  if not DWGRefHandleCandidatesValue(TextStyleRef, TextStyleCandidates) then
    FillChar(TextStyleCandidates, SizeOf(TextStyleCandidates), 0);
  DWGRegisterEntityShellWithTextStyleCandidates(pobj, DWGObject, True,
    TextStyleCandidates, AKind);
end;

initialization
finalization
  if LoadCtx <> nil then
    FreeAndNil(LoadCtx);
end.
