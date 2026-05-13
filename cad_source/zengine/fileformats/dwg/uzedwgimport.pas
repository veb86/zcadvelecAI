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
  uzedwgdiagnostics,
  uzedwgloadcontext,
  uzedwgrawscan,
  uzedwgblockreserve,
  uzedwgsidefiles,
  uzedwgentityregistry,
  uzedwgtargetedlog,
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
  zDebugLn(['{WH}DWG active VPORT view: center=(',
    FloatToStr(Props.CenterX), ', ', FloatToStr(Props.CenterY),
    '), height=', FloatToStr(Props.Height),
    ', width=', FloatToStr(Props.Width),
    ', has_width=', BoolToStr(Props.HasWidth, True),
    ', space=', DWGViewSpaceToText(Props.Space)]);
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
      zDebugLn(['{WH}DWG current layer handle ',
        IntToHex(LoadCurrentLayerHandle, 1),
        ' did not resolve to a layer; using system layer']);
  end;
  if CurrentLayer = nil then
    CurrentLayer := ZContext.PDrawing^.LayerTable.GetSystemLayer;
  ZContext.PDrawing^.CurrentLayer := CurrentLayer;
  if CurrentLayer <> nil then
    zDebugLn(['{WH}DWG current layer -> ', CurrentLayer^.Name]);
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
      zDebugLn(['{WH}DWG current linetype handle ',
        IntToHex(LoadCurrentLineTypeHandle, 1),
        ' did not resolve to a linetype; using ByLayer']);
  end;
  if CurrentLType = nil then
    CurrentLType := DWGSystemLineTypeForKind(dltByLayer);
  ZContext.PDrawing^.CurrentLType := CurrentLType;
  if CurrentLType <> nil then
    zDebugLn(['{WH}DWG current linetype -> ', CurrentLType^.Name]);
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
      zDebugLn(['{WH}DWG current textstyle handle ',
        IntToHex(LoadCurrentTextStyleHandle, 1),
        ' did not resolve to a text style; using Standard']);
  end;
  if (CurrentStyle = nil) or CurrentStyle^.UsedInLTYPE then
    CurrentStyle := DWGEnsureTextStyle(ZContext.PDrawing^);
  ZContext.PDrawing^.CurrentTextStyle := CurrentStyle;
  if CurrentStyle <> nil then
    zDebugLn(['{WH}DWG current textstyle -> ', CurrentStyle^.Name]);
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
      zDebugLn(['{WH}DWG current dimstyle handle ',
        IntToHex(LoadCurrentDimStyleHandle, 1),
        ' did not resolve to a dimstyle; using Standard']);
  end;
  if CurrentDimStyle = nil then
    CurrentDimStyle := DWGEnsureDimStyle(ZContext.PDrawing^);
  ZContext.PDrawing^.CurrentDimStyle := CurrentDimStyle;
  if CurrentDimStyle <> nil then
    zDebugLn(['{WH}DWG current dimstyle -> ', CurrentDimStyle^.Name]);
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
  zDebugLn(['{WH}DWG current entity defaults -> color=',
    ZContext.PDrawing^.CColor,
    ', lineweight=', ZContext.PDrawing^.CurrentLineW,
    ', celtscale=', FloatToStr(ZContext.PDrawing^.CLTScale),
    ', ltscale=', FloatToStr(ZContext.PDrawing^.LTScale),
    ', lwdisplay=', BoolToStr(ZContext.PDrawing^.LWDisplay, True)]);
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
    zDebugLn(['{WH}DWG header view is paper-space; ignoring it because ZCAD ',
      'opens DWG drawings in model space']);
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

  zDebugLn(['{WH}DWG view from ', Source, ' applied: camera=(',
    FloatToStr(ZContext.PDrawing^.pcamera^.prop.point.x), ', ',
    FloatToStr(ZContext.PDrawing^.pcamera^.prop.point.y),
    '), zoom=', FloatToStr(ZContext.PDrawing^.pcamera^.prop.zoom)]);
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

{ Issue #1198 P4: gate a per-entity fallback log line through the
  warning list's dedup tracker. Returns True if the caller should emit
  the zDebugLn line (first occurrence for this Code+Handle pair) and
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
begin
  // Reason is forwarded to the logger so unresolved fallbacks are visible to
  // human reviewers without a separate diagnostic pass.
  pobj := PGDBObjEntity(Entity);
  if (pobj = nil) or (Owner = nil) then
    Exit;

  EntityHandle := DWGOwnerEntityHandleForLog(Entity);

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
      zDebugLn(['{WH}entity ', HexStr(PtrUInt(pobj), 16),
        ' deferred under INSERT via fallback (',
        DWGAttachReasonToText(Reason), ')']);
    Exit;
  end;

  newowner := PGDBObjGenericSubEntry(Owner);

  newowner^.AddMi(PGDBObjSubordinated(pobj));
  if (Reason <> arResolved) and
     DWGShouldEmitFallbackDetail(Reason, EntityHandle) then
    zDebugLn(['{WH}entity ', HexStr(PtrUInt(pobj), 16),
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
  EntityHandle: QWord;
begin
  EntityHandle := DWGRefEntityHandleForLog(Entity, Slot);
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
          zDebugLn(['{WH}entity ', HexStr(PtrUInt(pobj), 16),
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
        if (Reason <> arResolved) and
           DWGShouldEmitFallbackDetail(Reason, EntityHandle) then
          zDebugLn(['{WH}entity ', HexStr(PtrUInt(pobj), 16),
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
        if Reason <> arResolved then begin
          if DWGShouldEmitFallbackDetail(Reason, EntityHandle) then
            zDebugLn(['{WH}layer ', DWGLayerNameForLog(player),
              ' ', DWGRefHandlesForLog(Entity, Slot),
              ' linetype fallback (', DWGAttachReasonToText(Reason), ') -> ',
              DWGLTypeNameForLog(PGDBLtypeProp(Ref))]);
        end
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
        if (Reason <> arResolved) and
           DWGShouldEmitFallbackDetail(Reason, EntityHandle) then
          zDebugLn(['{WH}entity ', HexStr(PtrUInt(pobj), 16),
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
        if (Reason <> arResolved) and
           DWGShouldEmitFallbackDetail(Reason, EntityHandle) then
          zDebugLn(['{WH}entity ', HexStr(PtrUInt(pobj), 16),
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
            zDebugLn(['{WH}entity ', HexStr(PtrUInt(pobj), 16),
              ' ', DWGRefHandlesForLog(Entity, Slot),
              ' block ref resolves to model/paper space; using empty block'])
          else if DWGShouldEmitFallbackDetail(Reason, EntityHandle) then
            zDebugLn(['{WH}entity ', HexStr(PtrUInt(pobj), 16),
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

procedure BeginDWGImport(var ZContext: TZDrawingContext;
  const ASourcePath: String = '');
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
  // Issue #1203: перечитываем список целевых handle'ов из переменной окружения
  // в самом начале импорта. Если ZCAD_DWG_TARGET_HANDLES пуста — последующие
  // вызовы TargetedLogXxx будут no-op'ами; если задана — каждое прохождение
  // целевого handle через ключевые точки конвейера будет залогировано.
  TargetedLogRefreshFromEnv;
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
    zDebugLn(['{WH}DWG header defaults: CLAYER=',
      DWGHeaderHandleForLog(LoadHasCurrentLayerHandle, LoadCurrentLayerHandle),
      ', CELTYPE=',
      DWGHeaderHandleForLog(LoadHasCurrentLineTypeHandle, LoadCurrentLineTypeHandle),
      ', TEXTSTYLE=',
      DWGHeaderHandleForLog(LoadHasCurrentTextStyleHandle, LoadCurrentTextStyleHandle),
      ', DIMSTYLE=',
      DWGHeaderHandleForLog(LoadHasCurrentDimStyleHandle, LoadCurrentDimStyleHandle),
      ', CECOLOR=', LoadHeaderEntityProps.ColorIndex,
      ', CELWEIGHT=', LoadHeaderEntityProps.LineWeight,
      ', CELTSCALE=', FloatToStr(LoadHeaderEntityProps.LineTypeScale),
      ', LTSCALE=', FloatToStr(LoadHeaderEntityProps.GlobalLineTypeScale),
      ', LWDISPLAY=', BoolToStr(LoadHeaderEntityProps.LineWeightDisplay, True)]);
  if LoadHasHeaderViewProps then
    zDebugLn(['{WH}DWG header view: center=(',
      FloatToStr(LoadHeaderViewProps.CenterX), ', ',
      FloatToStr(LoadHeaderViewProps.CenterY), '), height=',
      FloatToStr(LoadHeaderViewProps.Height), ', space=',
      DWGViewSpaceToText(LoadHeaderViewProps.Space)]);
  if LoadDrawing <> nil then
    DWGReserveBlockDefCapacity(Raw, LoadDrawing^.BlockDefArray);
  HandlesBefore := LoadCtx.Handles.Count;
  ScanRawObjects(Raw, LoadCtx);
  zDebugLn(['{WH}DWG raw objects: classes=', Raw.num_classes,
    ', objects=', Raw.num_objects,
    ', alloced_objects=', Raw.num_alloced_objects,
    ', entities=', Raw.num_entities,
    ', object_refs=', Raw.num_object_refs,
    ', handles_registered=', LoadCtx.Handles.Count - HandlesBefore,
    ', handles_total=', LoadCtx.Handles.Count]);
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
    zDebugLn(['{WH}DWG warning summary code=', Agg.Code,
      ' (', DWGWarningCodeToShortName(Agg.Code), '): ',
      Agg.TotalCount, ' occurrence(s) across ',
      Agg.DistinctHandles, ' handle(s); ',
      Suppressed, ' duplicate(s) suppressed from main log']);
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
  Mode := DWGDiagModeFromEnv;
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
    zDebugLn(['{WH}DWG fixedtype ',
      DWGFixedTypeToText(FixedTypes[I].FixedType),
      ': count=', FixedTypes[I].Count,
      ', has_handler=', HasH]);
  end;
  zDebugLn(['{WH}DWG fixedtype histogram: ', Length(FixedTypes),
    ' distinct type(s), ', Total, ' handle(s); ',
    UnhandledFT, ' type(s) without a registered handler (',
    Unhandled, ' handle(s))']);
end;

{ Issue #1198 P3: emit per-DWG diagnostic side-files when the user has
  opted in via ZCAD_DWG_DIAG=summary|full|trace. Runs after the resolver
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
  Mode := DWGDiagModeFromEnv;
  if Mode = dmOff then
    Exit;
  try
    Res := DWGWriteSideFiles(Ctx, SourcePath, Mode);
    zDebugLn(['{WH}DWG diagnostic side-files (mode=',
      DWGDiagModeToString(Mode), '): ',
      Length(Res.FilesWritten), ' file(s) written']);
    for I := 0 to High(Res.FilesWritten) do
      zDebugLn(['{WH}  ', Res.FilesWritten[I]]);
  except
    on E: Exception do
      zDebugLn(['{WH}DWG side-files: failed to write (',
        E.ClassName, '): ', E.Message]);
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
    zDebugLn(['{WH}DWG owner resolve: handles=', LoadCtx.Handles.Count,
      ', pending_owners=', LoadCtx.PendingOwners.Count,
      ', pending_refs=', LoadCtx.PendingRefs.Count,
      ', attached=', LoadCtx.AttachCount,
      ', fallback=', LoadCtx.FallbackCount,
      ', cycles=', LoadCtx.CycleCount,
      ', refs_attached=', LoadCtx.RefAttachCount,
      ', refs_fallback=', LoadCtx.RefFallbackCount,
      ', proxy_loaded=', LoadCtx.ProxiesLoaded,
      ', proxy_failed=', LoadCtx.ProxiesFailed,
      ', unknown_entities=', LoadCtx.UnknownEntities,
      ', unknown_objects=', LoadCtx.UnknownObjects,
      ', freed_raw_drops=', LoadCtx.DroppedDueToFreedRaw,
      ', warnings=', LoadCtx.WarningCount]);
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
    zDebugLn(['{WH}entmode=', EntMode,
      ' implicit owner unresolved for entity ',
      IntToHex(EntityHandle, 1),
      ' (mspace/pspace_block, header_vars.BLOCK_RECORD_*SPACE,',
      ' block_control.*_space and ownerhandle all empty)']);
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
      zDebugLn(['{WH}DWG entity ', IntToHex(EntityHandle, 1),
        ' is marked invisible in the DWG common entity flags']);
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
  //  zDebugLn(['{WH}DWG LINE shell handle=', IntToHex(EntityHandle, 1),
  //    ', entmode=', EntMode,
  //    ', owner=', IntToHex(OwnerHandle, 1),
  //    ', layer_ref=', IntToHex(LayerHandle, 1),
  //    ', ltype_kind=', DWGEntityLineTypeKindToText(LtypeKind),
  //    ', ltype_ref=', IntToHex(LtypeHandle, 1),
  //    ', ltype_flags=', CommonProps.LineTypeFlags,
  //    ', color=', CommonProps.ColorIndex,
  //    ', lineweight=', CommonProps.LineWeight,
  //    ', ltscale=', FloatToStr(CommonProps.LineTypeScale),
  //    ', invisible=', BoolToStr(CommonProps.Invisible, True)]);
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
