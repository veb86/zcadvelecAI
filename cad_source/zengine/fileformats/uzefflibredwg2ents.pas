{
*****************************************************************************
*                                                                           *
*  This file is part of the ZCAD                                            *
*                                                                           *
*  See the file GPL-3.0.txt, included in this distribution,                 *
*  for details about the copyright.                                         *
*                                                                           *
*  This program is distributed in the hope that it will be useful,          *
*  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
*                                                                           *
*****************************************************************************
}
{
@author(Andrey Zubarev <zamtmn@yandex.ru>)
}

unit uzeffLibreDWG2Ents;
{$Include zengineconfig.inc}
{$Mode delphi}{$H+}
{$ModeSwitch advancedrecords}
interface
uses
  uzbLogIntf,
  SysUtils,
  dwg,dwgproc,
  uzeentgenericsubentry,uzedrawingsimple,
  uzbstrproc,
  uzestyleslayers,uzestyleslinetypes,uzestylestexts,
  uzeentline,uzeentity,uzeentitiesprop,//uzgldrawcontext,
  uzeentcircle,uzeentarc,uzeentpoint,uzeentlwpolyline,
  uzeenttext,uzeentmtext,uzeentabstracttext,
  uzeconsts,
  uzegeometrytypes,uzegeometry,
  UGDBPolyLine2DArray,
  uzeblockdef,UGDBObjBlockdefArray,
  uzeTypes,
  uzedwgloadcontext,
  uzeffLibreDWG,
  uzeffmanager;

{ Stage 2 hooks called by uzefflibredwg.pas around parseDwg_Data. They open
  and close the per-file load context that decouples DWG read order from ZCAD
  attachment (TZ §5.3 / §12.2). Begin must be called before parseDwg_Data,
  End after. End is responsible for calling ResolveOwners and finalizing
  attached entities. The functions are no-ops if called out of order. }
procedure BeginDWGImport(var ZContext:TZDrawingContext);
procedure EndDWGImport(var ZContext:TZDrawingContext);

implementation
type
  // Stage 3: dwg.pp does not export PDwg_Object_STYLE so we declare it here
  // to keep the AddTextStyle signature symmetrical with AddLayer / AddLineType.
  PDwg_Object_STYLE = ^Dwg_Object_STYLE;

{ Per-file load context. The DWG loader is invoked sequentially (no thread-
  per-file) so a unit-level instance is sufficient. Begin/End assert the
  expected lifecycle and clear the slot on End so a stale context can never
  leak into a subsequent load. }
var
  LoadCtx:TDWGZCADLoadContext=nil;
  LoadDrawing:PTSimpleDrawing=nil;

{ Stage 4 (TZ §12.4): "block content форматируется при использовании INSERT
  или финальной formatting-фазе drawing". DWGAttachEntity must skip
  BuildGeometry when the resolved owner is a block definition because the
  drawing's BlockDefArray.FormatEntity will run BuildGeometry+formatEntity
  for every block-def child later. Walking the handle map keeps the test
  pipeline (which never registers block defs) on the existing fast path. }
function DWGOwnerIsBlockDef(Owner:Pointer):Boolean;
var
  i:Integer;
  Entry:PDWGZCADHandleEntry;
begin
  Result:=False;
  if (Owner=nil) or (LoadCtx=nil) then
    Exit;
  for i:=0 to LoadCtx.Handles.Count-1 do begin
    Entry:=LoadCtx.Handles.EntryAt(i);
    if (Entry^.Kind=dokBlockDef) and (Entry^.Ptr=Owner) then
      Exit(True);
  end;
end;

procedure DWGAttachEntity(Entity:Pointer;Owner:Pointer;
  Reason:TDWGAttachReason;Data:Pointer);
var
  pobj:PGDBObjEntity;
  newowner:PGDBObjGenericSubEntry;
  ownerIsBlockDef:Boolean;
begin
  // Reason is forwarded to the logger so unresolved fallbacks are visible to
  // human reviewers without a separate diagnostic pass.
  pobj:=PGDBObjEntity(Entity);
  newowner:=PGDBObjGenericSubEntry(Owner);
  if (pobj=nil) or (newowner=nil) then
    Exit;

  newowner^.AddMi(PGDBObjSubordinated(pobj));
  if Reason<>arResolved then
    zDebugLn(['{WHM}LINE ',HexStr(PtrUInt(pobj),16),
      ' attached via fallback (',DWGAttachReasonToText(Reason),')']);

  // Stage 4 (TZ §12.4): defer block-def content geometry. BuildGeometry must
  // still run for entities landing in model/paper space (or fallback root) so
  // they appear on screen, but block-definition contents are formatted later
  // — either by an INSERT that materialises the block, or by the drawing's
  // global formatting pass (BlockDefArray.FormatEntity). Running BuildGeometry
  // here for a block-def child would duplicate the work that FormatEntity
  // does and pollute the deferred-formatting contract the spec calls out by
  // name ("block content форматируется при использовании INSERT или финальной
  // formatting-фазе drawing").
  ownerIsBlockDef:=False;
  if LoadCtx<>nil then
    ownerIsBlockDef:=DWGOwnerIsBlockDef(newowner);

  if (LoadDrawing<>nil) and (not ownerIsBlockDef) then begin
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
procedure DWGAttachRef(Entity:Pointer;Ref:Pointer;Slot:TDWGZCADRefSlot;
  Reason:TDWGAttachReason;Data:Pointer);
var
  pobj:PGDBObjEntity;
begin
  pobj:=PGDBObjEntity(Entity);
  if pobj=nil then
    Exit;

  case Slot of
    rsLayer:
      begin
        pobj^.vp.Layer:=PGDBLayerProp(Ref);
        if Reason<>arResolved then
          zDebugLn(['{WHM}entity ',HexStr(PtrUInt(pobj),16),
            ' layer fallback (',DWGAttachReasonToText(Reason),')']);
      end;
    rsLineType:
      begin
        pobj^.vp.LineType:=PGDBLtypeProp(Ref);
        if Reason<>arResolved then
          zDebugLn(['{WHM}entity ',HexStr(PtrUInt(pobj),16),
            ' linetype fallback (',DWGAttachReasonToText(Reason),')']);
      end;
    rsTextStyle:
      begin
        // Stage 5 (TZ §12.5): TEXT/MTEXT entities carry a TXTStyle pointer on
        // GDBObjText / GDBObjMText. Branch on GetObjType so we can refuse to
        // write the slot for any other entity that may end up queued by
        // mistake (the loader is allowed to queue defensively without knowing
        // whether the target supports the slot).
        if (pobj^.GetObjType=GDBtextID) or (pobj^.GetObjType=GDBMTextID) then
          PGDBObjText(pobj)^.TXTStyle:=PGDBTextStyle(Ref);
        if Reason<>arResolved then
          zDebugLn(['{WHM}entity ',HexStr(PtrUInt(pobj),16),
            ' textstyle fallback (',DWGAttachReasonToText(Reason),')']);
      end;
    rsDimStyle:
      begin
        // Same as above — reserved for Stage 4 dimension mappers.
      end;
  end;
end;

procedure BeginDWGImport(var ZContext:TZDrawingContext);
var
  ByLayerLT:PGDBLtypeProp;
  SysLayer:PGDBLayerProp;
  StdStyle:PGDBTextStyle;
begin
  if LoadCtx<>nil then begin
    zDebugLn(['{WHM}DWG load context already active; force-resetting']);
    FreeAndNil(LoadCtx);
  end;
  LoadCtx:=TDWGZCADLoadContext.Create;
  LoadDrawing:=ZContext.PDrawing;
  // Register pObjRoot under handle 0 so any LINE with a missing owner falls
  // back into the model-space root (TZ §5.5: "broken owner -> fallback root").
  LoadCtx.SetFallbackOwner(ZContext.PDrawing^.pObjRoot);
  LoadCtx.SetAttachProc(@DWGAttachEntity,nil);
  LoadCtx.SetRefAttachProc(@DWGAttachRef,nil);

  // Stage 3 fallbacks (TZ §12.3): mirror the DXF loader's behaviour
  // (uzeffdxf.pas:412-427). A LINE with a missing/broken layer ref drops onto
  // the system layer; a missing/broken linetype ref drops onto the ByLayer
  // entry. These tables are pre-populated when the drawing is initialised so
  // the lookups always succeed.
  SysLayer:=ZContext.PDrawing^.LayerTable.GetSystemLayer;
  ByLayerLT:=PGDBLtypeProp(ZContext.PDrawing^.LTypeStyleTable.getAddres('ByLayer'));
  if ByLayerLT=nil then
    ByLayerLT:=ZContext.PDrawing^.LTypeStyleTable.GetSystemLT(TLTByLayer);
  LoadCtx.SetFallbackLayer(SysLayer);
  LoadCtx.SetFallbackLineType(ByLayerLT);
  // The text-style fallback is the first style in the table when present;
  // mappers that genuinely care will overwrite this on a per-entity basis.
  StdStyle:=ZContext.PDrawing^.TextStyleTable.FindStyle('Standard',False);
  LoadCtx.SetFallbackTextStyle(StdStyle);

  LoadCtx.RegisterShell(0,dokModelSpace,ZContext.PDrawing^.pObjRoot,-1);
end;

procedure EndDWGImport(var ZContext:TZDrawingContext);
begin
  if LoadCtx=nil then
    Exit;
  try
    // Stage 3: resolve refs first so DWGAttachEntity can BuildGeometry against
    // the right vp slots. Resolving owners afterwards triggers the attach
    // callback which then calls BuildGeometry.
    LoadCtx.ResolveRefs;
    LoadCtx.ResolveOwners;
    zDebugLn(['{WH}DWG owner resolve: attached=',LoadCtx.AttachCount,
      ', fallback=',LoadCtx.FallbackCount,
      ', cycles=',LoadCtx.CycleCount,
      ', refs_attached=',LoadCtx.RefAttachCount,
      ', refs_fallback=',LoadCtx.RefFallbackCount,
      ', warnings=',LoadCtx.WarningCount]);
  finally
    FreeAndNil(LoadCtx);
    LoadDrawing:=nil;
  end;
end;

procedure AddLayer(var ZContext:TZDrawingContext;var DWGContext:TDWGCtx;var DWGObject:Dwg_Object;PDWGLayer:PDwg_Object_LAYER);
var
  player:PGDBLayerProp;
  name:string;
  Handle,LtHandle:QWord;
begin
  BITCODE_T2Text(PDWGLayer^.name,DWGContext,name);
  zDebugLn(['{WH}Layer: ',name]);
  if DWGContext.DWGVer>R_2007 then
    name:=Tria_Utf8ToAnsi(name);
  player:=ZContext.PDrawing^.LayerTable.MergeItem(name,ZContext.LoadMode);
  if player<>nil then begin
    player^.init(name);
    player^.color:=PDWGLayer^.color.index;
    player^.lineweight:=PDWGLayer^.linewt;
    player^._on:=(PDWGLayer^.off=0);
    player^._lock:=(PDWGLayer^.locked<>0);
    player^._print:=(PDWGLayer^.plotflag<>0);
    //desk:AnsiString;
  end;
  if LoadCtx<>nil then begin
    Handle:=DWGObjectHandleValue(DWGObject);
    if Handle<>0 then
      LoadCtx.RegisterShell(Handle,dokLayer,player,-1);
    // Stage 3 (TZ §8.3): defer the layer.LT linetype assignment until all
    // LTYPE shells have been registered. Use the layer pointer as the
    // "entity" so DWGAttachRef writes into the layer's LT slot once the
    // ltype handle resolves. RefHandle=0 is allowed: it falls back to
    // ByLayer/Continuous via the registered slot fallback.
    if (player<>nil) and (Handle<>0) then begin
      if not DWGLayerLineTypeHandleValue(PDWGLayer,LtHandle) then
        LtHandle:=0;
      LoadCtx.QueueRefResolve(player,Handle,LtHandle,
        dokLineType,rsLineType,nil);
    end;
  end;
end;

procedure AddLineType(var ZContext:TZDrawingContext;var DWGContext:TDWGCtx;var DWGObject:Dwg_Object;PDWGLType:PDwg_Object_LTYPE);
var
  pltype:PGDBLtypeProp;
  name:string;
  Handle:QWord;
begin
  BITCODE_T2Text(PDWGLType^.name,DWGContext,name);
  zDebugLn(['{WH}LineType: ',name]);
  if DWGContext.DWGVer>R_2007 then
    name:=Tria_Utf8ToAnsi(name);
  // Stage 3 (TZ §12.3): create the linetype in the table so refs can resolve
  // to a real pointer. We mirror the DXF loader semantics — a name collision
  // with a previously-loaded entry is left alone (TLOMerge respected).
  pltype:=PGDBLtypeProp(ZContext.PDrawing^.LTypeStyleTable.MergeItem(name,
    ZContext.LoadMode));
  if pltype<>nil then begin
    if pltype^.Name='' then
      pltype^.init(name);
    // Length / dash arrays are not yet decoded — Stage 3 only needs the
    // name+pointer to exist so that entity refs land in the correct slot.
    // Stage 4 will populate dasharray + strokesarray + description from the
    // DWG dashes.
  end;
  if LoadCtx<>nil then begin
    Handle:=DWGObjectHandleValue(DWGObject);
    if Handle<>0 then
      LoadCtx.RegisterShell(Handle,dokLineType,pltype,-1);
  end;
end;

procedure AddTextStyle(var ZContext:TZDrawingContext;var DWGContext:TDWGCtx;var DWGObject:Dwg_Object;PDWGStyle:PDwg_Object_STYLE);
var
  pstyle:PGDBTextStyle;
  name:string;
  Handle:QWord;
begin
  BITCODE_T2Text(PDWGStyle^.name,DWGContext,name);
  zDebugLn(['{WH}TextStyle: ',name]);
  if DWGContext.DWGVer>R_2007 then
    name:=Tria_Utf8ToAnsi(name);
  // Stage 3 (TZ §12.3, "начать STYLE mapper"): just create the pointer so
  // future TEXT/MTEXT mappers have something to reference. Font file and
  // metrics fields are decoded in Stage 4 alongside text geometry support.
  pstyle:=ZContext.PDrawing^.TextStyleTable.FindStyle(name,False);
  if pstyle=nil then
    pstyle:=ZContext.PDrawing^.TextStyleTable.FindStyle('Standard',False);
  if LoadCtx<>nil then begin
    Handle:=DWGObjectHandleValue(DWGObject);
    if Handle<>0 then
      LoadCtx.RegisterShell(Handle,dokTextStyle,pstyle,-1);
  end;
end;

{ Stage 4 (TZ §12.4): a block header may describe model-space, paper-space or
  a user block. The first two are containers but not user-visible blocks, so
  they must NOT spawn a BlockDef and must NOT count as duplicates against
  user-named blocks. LibreDWG exposes `mspace_block` / `pspace_block` on the
  Dwg_Data root: comparing the object pointer is locale/version safe and
  cheaper than a name match. We still keep a name fallback because the layout
  pointers are only populated for real DWG files — fixture-driven tests that
  build a Dwg_Object by hand can reach this code path with parent=nil. }
function DWGBlockHeaderIsModelSpace(var DWGObject:Dwg_Object;
  const Name:string):Boolean;
var
  Dwg:PDwg_Data;
  upper:string;
begin
  Dwg:=PDwg_Data(DWGObject.parent);
  if (Dwg<>nil) and (Dwg^.mspace_block<>nil) and
     (Pointer(Dwg^.mspace_block)=@DWGObject) then
    Exit(True);
  upper:=UpperCase(Name);
  Result:=(upper='*MODEL_SPACE') or (upper='$MODEL_SPACE');
end;

function DWGBlockHeaderIsPaperSpace(var DWGObject:Dwg_Object;
  const Name:string):Boolean;
var
  Dwg:PDwg_Data;
  upper:string;
begin
  Dwg:=PDwg_Data(DWGObject.parent);
  if (Dwg<>nil) and (Dwg^.pspace_block<>nil) and
     (Pointer(Dwg^.pspace_block)=@DWGObject) then
    Exit(True);
  upper:=UpperCase(Name);
  Result:=(upper='*PAPER_SPACE') or (upper='$PAPER_SPACE') or
          (Copy(upper,1,12)='*PAPER_SPACE');
end;

procedure AddBlockHeader(var ZContext:TZDrawingContext;var DWGContext:TDWGCtx;var DWGObject:Dwg_Object;PDWGBlock_Header:PDwg_Object_BLOCK_HEADER);
var
  name:string;
  Handle:QWord;
  pBlockDef:PGDBObjBlockdef;
  existingIdx:Integer;
  isModel,isPaper:Boolean;
begin
  BITCODE_T2Text(PDWGBlock_Header^.name,DWGContext,name);
  if DWGContext.DWGVer>R_2007 then
    name:=Tria_Utf8ToAnsi(name);
  zDebugLn(['{WH}BlockHeader: ',name]);

  if LoadCtx=nil then
    Exit;
  Handle:=DWGObjectHandleValue(DWGObject);
  if Handle=0 then
    Exit;

  // Stage 4 (TZ §12.4): model and paper space are containers, not user blocks.
  // The model-space root is already registered under handle 0 in
  // BeginDWGImport so child entities with a null/missing owner fall back to
  // it; here we additionally register the real model-space handle so entities
  // whose ownerhandle points to model_space land in the same drawing root and
  // not into a freshly created BlockDef. Paper space gets its own kind so
  // future paper-space layouts can be wired without changing this routine.
  isModel:=DWGBlockHeaderIsModelSpace(DWGObject,name);
  isPaper:=(not isModel) and DWGBlockHeaderIsPaperSpace(DWGObject,name);

  if isModel then begin
    LoadCtx.RegisterShell(Handle,dokModelSpace,
      ZContext.PDrawing^.pObjRoot,-1);
    Exit;
  end;
  if isPaper then begin
    // Stage 4 leaves paper-space contents in fallback root for now (TZ §12.4
    // covers recognition, full paper-space rendering is its own future stage).
    // Registering with pObjRoot keeps child resolve working without
    // pretending we have a separate paper-space drawing.
    LoadCtx.RegisterShell(Handle,dokPaperSpace,
      ZContext.PDrawing^.pObjRoot,-1);
    Exit;
  end;

  // Stage 4 (TZ §12.4): a real user block. Honour LoadMode the same way
  // LayerTable.MergeItem does — TLOMerge keeps the existing definition,
  // TLOLoad reuses it (and lets later mappers overwrite fields). The
  // BlockDefArray itself does not provide MergeItem so we inline the lookup.
  existingIdx:=ZContext.PDrawing^.BlockDefArray.getindex(name);
  if existingIdx>=0 then begin
    pBlockDef:=@PBlockdefArray(
      ZContext.PDrawing^.BlockDefArray.parray)[existingIdx];
    if ZContext.LoadMode=TLOMerge then begin
      // Duplicate-by-name in merge mode: do not overwrite, but still register
      // the new handle so children inside this duplicate landed under the
      // already-loaded block definition.
      zDebugLn(['{WH}BlockHeader: duplicate "',name,'" merged']);
    end;
  end else begin
    pBlockDef:=ZContext.PDrawing^.BlockDefArray.create(name);
    if pBlockDef<>nil then begin
      pBlockDef^.Base.x:=PDWGBlock_Header^.base_pt.x;
      pBlockDef^.Base.y:=PDWGBlock_Header^.base_pt.y;
      pBlockDef^.Base.z:=PDWGBlock_Header^.base_pt.z;
    end;
  end;
  LoadCtx.RegisterShell(Handle,dokBlockDef,pBlockDef,-1);
end;

procedure AddBlock(var ZContext:TZDrawingContext;var DWGContext:TDWGCtx;var DWGObject:Dwg_Object;PDWGBlock_Header:PDwg_Object_BLOCK_HEADER);
var
  name:string;
begin
  BITCODE_T2Text(PDWGBlock_Header^.name,DWGContext,name);
  zDebugLn(['{WH}Block: ',name]);
end;

procedure AddLineEntity(var ZContext:TZDrawingContext;var DWGContext:TDWGCtx;var DWGObject:Dwg_Object;PLine:PDwg_Entity_LINE);
var
  pobj:PGDBObjEntity;
  Endpoints:TDWGLineEndpoints;
  EntityHandle,OwnerHandle,LayerHandle,LtypeHandle:QWord;
begin
  // Stage 2 (TZ §12.2): allocate with nil owner, fill geometry, register the
  // shell + pending owner. The actual AddMi happens in DWGAttachEntity when
  // ResolveOwners runs after parseDwg_Data. The line is *never* added to
  // pObjRoot here — that was the original bug that caused entities to attach
  // to the model-space root before their block-def owner was visible.
  pobj:=AllocAndInitLine(nil);
  DWGCopyLineEndpoints(PLine^,Endpoints);
  PGDBObjLine(pobj)^.CoordInOCS.lBegin.x:=Endpoints.StartX;
  PGDBObjLine(pobj)^.CoordInOCS.lBegin.y:=Endpoints.StartY;
  PGDBObjLine(pobj)^.CoordInOCS.lBegin.z:=Endpoints.StartZ;
  PGDBObjLine(pobj)^.CoordInOCS.lEnd.x:=Endpoints.EndX;
  PGDBObjLine(pobj)^.CoordInOCS.lEnd.y:=Endpoints.EndY;
  PGDBObjLine(pobj)^.CoordInOCS.lEnd.z:=Endpoints.EndZ;

  if LoadCtx<>nil then begin
    EntityHandle:=DWGObjectHandleValue(DWGObject);
    if not DWGObjectOwnerHandleValue(DWGObject,OwnerHandle) then
      OwnerHandle:=0;
    if EntityHandle<>0 then
      LoadCtx.RegisterShell(EntityHandle,dokEntity,pobj,-1);
    LoadCtx.QueueOwnerResolve(pobj,EntityHandle,OwnerHandle);
    // Stage 3 (TZ §12.3): queue layer + linetype refs so vp.Layer / vp.LineType
    // are populated before BuildGeometry runs. Handles missing/broken refs
    // by routing to the registered fallbacks (system layer / ByLayer).
    if not DWGEntityLayerHandleValue(DWGObject,LayerHandle) then
      LayerHandle:=0;
    if not DWGEntityLineTypeHandleValue(DWGObject,LtypeHandle) then
      LtypeHandle:=0;
    LoadCtx.QueueRefResolve(pobj,EntityHandle,LayerHandle,
      dokLayer,rsLayer,nil);
    LoadCtx.QueueRefResolve(pobj,EntityHandle,LtypeHandle,
      dokLineType,rsLineType,nil);
  end else begin
    // Compatibility fallback: if BeginDWGImport was not called the loader
    // still works (legacy single-pass behaviour). New callers always go
    // through Begin/End, so this branch is exercised only by future
    // experimental hosts that bypass the standard pipeline.
    ZContext.PDrawing^.pObjRoot^.AddMi(PGDBObjSubordinated(pobj));
  end;
end;

{ Stage 5 (TZ §12.5): every entity mapper below follows the AddLineEntity
  pattern: allocate with nil owner, copy scalars from the LibreDWG payload via
  the testable DWGCopy*Props helpers in dwgproc, then push handle bookkeeping
  into the load context (RegisterShell + QueueOwnerResolve + QueueRefResolve
  for layer/linetype/textstyle as applicable). The actual AddMi happens later
  inside DWGAttachEntity once ResolveOwners runs. The LoadCtx=nil branch is a
  legacy fallback so an experimental host that bypasses BeginDWGImport still
  shows the entity instead of leaking memory. }

procedure DWGRegisterEntityShell(pobj:PGDBObjEntity;var DWGObject:Dwg_Object;
  WantTextStyle:Boolean;TextStyleHandle:QWord);
var
  EntityHandle,OwnerHandle,LayerHandle,LtypeHandle:QWord;
begin
  if LoadCtx=nil then
    Exit;
  EntityHandle:=DWGObjectHandleValue(DWGObject);
  if not DWGObjectOwnerHandleValue(DWGObject,OwnerHandle) then
    OwnerHandle:=0;
  if EntityHandle<>0 then
    LoadCtx.RegisterShell(EntityHandle,dokEntity,pobj,-1);
  LoadCtx.QueueOwnerResolve(pobj,EntityHandle,OwnerHandle);
  if not DWGEntityLayerHandleValue(DWGObject,LayerHandle) then
    LayerHandle:=0;
  if not DWGEntityLineTypeHandleValue(DWGObject,LtypeHandle) then
    LtypeHandle:=0;
  LoadCtx.QueueRefResolve(pobj,EntityHandle,LayerHandle,
    dokLayer,rsLayer,nil);
  LoadCtx.QueueRefResolve(pobj,EntityHandle,LtypeHandle,
    dokLineType,rsLineType,nil);
  if WantTextStyle then
    LoadCtx.QueueRefResolve(pobj,EntityHandle,TextStyleHandle,
      dokTextStyle,rsTextStyle,nil);
end;

procedure AddCircleEntity(var ZContext:TZDrawingContext;
  var DWGContext:TDWGCtx;var DWGObject:Dwg_Object;PCircle:PDwg_Entity_CIRCLE);
var
  pobj:PGDBObjCircle;
  Props:TDWGCircleProps;
begin
  pobj:=AllocAndInitCircle(nil);
  DWGCopyCircleProps(PCircle^,Props);
  pobj^.Local.p_insert.x:=Props.CenterX;
  pobj^.Local.p_insert.y:=Props.CenterY;
  pobj^.Local.p_insert.z:=Props.CenterZ;
  pobj^.Radius:=Props.Radius;
  if LoadCtx<>nil then
    DWGRegisterEntityShell(PGDBObjEntity(pobj),DWGObject,False,0)
  else
    ZContext.PDrawing^.pObjRoot^.AddMi(PGDBObjSubordinated(pobj));
end;

procedure AddArcEntity(var ZContext:TZDrawingContext;
  var DWGContext:TDWGCtx;var DWGObject:Dwg_Object;PArc:PDwg_Entity_ARC);
var
  pobj:PGDBObjArc;
  Props:TDWGArcProps;
begin
  pobj:=AllocAndInitArc(nil);
  DWGCopyArcProps(PArc^,Props);
  pobj^.Local.p_insert.x:=Props.CenterX;
  pobj^.Local.p_insert.y:=Props.CenterY;
  pobj^.Local.p_insert.z:=Props.CenterZ;
  pobj^.r:=Props.Radius;
  pobj^.startangle:=Props.StartAngle;
  pobj^.endangle:=Props.EndAngle;
  if LoadCtx<>nil then
    DWGRegisterEntityShell(PGDBObjEntity(pobj),DWGObject,False,0)
  else
    ZContext.PDrawing^.pObjRoot^.AddMi(PGDBObjSubordinated(pobj));
end;

procedure AddPointEntity(var ZContext:TZDrawingContext;
  var DWGContext:TDWGCtx;var DWGObject:Dwg_Object;PPoint:PDwg_Entity_POINT);
var
  pobj:PGDBObjPoint;
  Props:TDWGPointProps;
begin
  pobj:=AllocAndInitPoint(nil);
  DWGCopyPointProps(PPoint^,Props);
  pobj^.P_insertInOCS.x:=Props.X;
  pobj^.P_insertInOCS.y:=Props.Y;
  pobj^.P_insertInOCS.z:=Props.Z;
  if LoadCtx<>nil then
    DWGRegisterEntityShell(PGDBObjEntity(pobj),DWGObject,False,0)
  else
    ZContext.PDrawing^.pObjRoot^.AddMi(PGDBObjSubordinated(pobj));
end;

procedure AddLWPolylineEntity(var ZContext:TZDrawingContext;
  var DWGContext:TDWGCtx;var DWGObject:Dwg_Object;PLWP:PDwg_Entity_LWPOLYLINE);
var
  pobj:PGDBObjLWPolyline;
  Props:TDWGLWPolylineProps;
  i,n,wcount:Integer;
  pp:PzePoint2d;
  pw:PGLLWWidth;
begin
  pobj:=AllocAndInitLWpolyline(nil);
  DWGCopyLWPolylineProps(PLWP^,Props);
  pobj^.Closed:=Props.Closed;
  pobj^.Local.p_insert.x:=0;
  pobj^.Local.p_insert.y:=0;
  pobj^.Local.p_insert.z:=Props.Elevation;
  n:=Length(Props.Vertices);
  pobj^.Vertex2D_in_OCS_Array.SetCount(n);
  // Width-array sizing mirrors SetLWpolylineGeomProps: closed polylines have
  // a width per segment, open ones one fewer (count-1) so the last vertex has
  // no outgoing segment. Empty polylines get an empty array.
  if pobj^.Closed then
    wcount:=n
  else if n>0 then
    wcount:=n-1
  else
    wcount:=0;
  pobj^.Width2D_in_OCS_Array.SetCount(wcount);
  for i:=0 to n-1 do begin
    pp:=pobj^.Vertex2D_in_OCS_Array.getDataMutable(i);
    pp^.x:=Props.Vertices[i].X;
    pp^.y:=Props.Vertices[i].Y;
    if i<wcount then begin
      pw:=pobj^.Width2D_in_OCS_Array.getDataMutable(i);
      pw^.startw:=Props.Vertices[i].StartWidth;
      pw^.endw:=Props.Vertices[i].EndWidth;
      pw^.hw:=(pw^.startw<>0) or (pw^.endw<>0);
    end;
  end;
  if LoadCtx<>nil then
    DWGRegisterEntityShell(PGDBObjEntity(pobj),DWGObject,False,0)
  else
    ZContext.PDrawing^.pObjRoot^.AddMi(PGDBObjSubordinated(pobj));
end;

procedure AddTextEntity(var ZContext:TZDrawingContext;
  var DWGContext:TDWGCtx;var DWGObject:Dwg_Object;PText:PDwg_Entity_TEXT);
var
  pobj:PGDBObjText;
  Props:TDWGTextProps;
  StyleHandle:QWord;
  WantStyle:Boolean;
  uniValue:UnicodeString;
begin
  pobj:=AllocAndInitText(nil);
  DWGCopyTextProps(PText^,DWGContext.DWGVer,Props);
  pobj^.Local.p_insert.x:=Props.InsertX;
  pobj^.Local.p_insert.y:=Props.InsertY;
  pobj^.Local.p_insert.z:=Props.InsertZ;
  pobj^.textprop.size:=Props.Height;
  if Props.WidthFactor<>0 then
    pobj^.textprop.wfactor:=Props.WidthFactor
  else
    pobj^.textprop.wfactor:=1;
  pobj^.textprop.oblique:=Props.Oblique;
  // Stage 5 (TZ §12.5): DWGSafeDecodeText already returns the payload as a
  // RTL string (UTF-16 path goes through punicodechar -> system codepage).
  // Assigning to a UnicodeString-typed Content lets FPC promote it back via
  // the default conversion. This mirrors the existing DXF loader where the
  // content widening happens at field assignment.
  uniValue:=UnicodeString(Props.Value);
  pobj^.Content:=uniValue;
  WantStyle:=DWGTextStyleHandleValue(PText,StyleHandle);
  if not WantStyle then
    StyleHandle:=0;
  if LoadCtx<>nil then
    DWGRegisterEntityShell(PGDBObjEntity(pobj),DWGObject,True,StyleHandle)
  else
    ZContext.PDrawing^.pObjRoot^.AddMi(PGDBObjSubordinated(pobj));
end;

procedure AddMTextEntity(var ZContext:TZDrawingContext;
  var DWGContext:TDWGCtx;var DWGObject:Dwg_Object;PMText:PDwg_Entity_MTEXT);
var
  pobj:PGDBObjMText;
  Props:TDWGMTextProps;
  StyleHandle:QWord;
  WantStyle:Boolean;
  uniValue:UnicodeString;
begin
  pobj:=AllocAndInitMText(nil);
  DWGCopyMTextProps(PMText^,DWGContext.DWGVer,Props);
  pobj^.Local.p_insert.x:=Props.InsertX;
  pobj^.Local.p_insert.y:=Props.InsertY;
  pobj^.Local.p_insert.z:=Props.InsertZ;
  pobj^.textprop.size:=Props.TextHeight;
  pobj^.Width:=Props.RectWidth;
  if Props.LineSpaceFactor<>0 then
    pobj^.linespacef:=Props.LineSpaceFactor
  else
    pobj^.linespacef:=1;
  // Same widening contract as AddTextEntity — DWGSafeDecodeText already
  // returned the payload as RTL string; the FPC compiler promotes it.
  uniValue:=UnicodeString(Props.Value);
  pobj^.Content:=uniValue;
  WantStyle:=DWGMTextStyleHandleValue(PMText,StyleHandle);
  if not WantStyle then
    StyleHandle:=0;
  if LoadCtx<>nil then
    DWGRegisterEntityShell(PGDBObjEntity(pobj),DWGObject,True,StyleHandle)
  else
    ZContext.PDrawing^.pObjRoot^.AddMi(PGDBObjSubordinated(pobj));
end;

initialization
  ZCDWGParser.RegisterDWGObjectLoadProc(DWG_TYPE_LAYER,@AddLayer);
  ZCDWGParser.RegisterDWGObjectLoadProc(DWG_TYPE_LTYPE,@AddLineType);
  ZCDWGParser.RegisterDWGObjectLoadProc(DWG_TYPE_STYLE,@AddTextStyle);
  ZCDWGParser.RegisterDWGObjectLoadProc(DWG_TYPE_BLOCK_HEADER,@AddBlockHeader);

  ZCDWGParser.RegisterDWGEntityLoadProc(DWG_TYPE_LINE,@AddLineEntity);
  ZCDWGParser.RegisterDWGEntityLoadProc(DWG_TYPE_BLOCK,@AddBlock);
  // Stage 5 (TZ §12.5): MVP entity coverage. Each handler is registered once;
  // double-registration would raise rsHandlerAlreadyReg via the parser.
  ZCDWGParser.RegisterDWGEntityLoadProc(DWG_TYPE_CIRCLE,@AddCircleEntity);
  ZCDWGParser.RegisterDWGEntityLoadProc(DWG_TYPE_ARC,@AddArcEntity);
  ZCDWGParser.RegisterDWGEntityLoadProc(DWG_TYPE_POINT,@AddPointEntity);
  ZCDWGParser.RegisterDWGEntityLoadProc(DWG_TYPE_LWPOLYLINE,@AddLWPolylineEntity);
  ZCDWGParser.RegisterDWGEntityLoadProc(DWG_TYPE_TEXT,@AddTextEntity);
  ZCDWGParser.RegisterDWGEntityLoadProc(DWG_TYPE_MTEXT,@AddMTextEntity);
finalization
  if LoadCtx<>nil then
    FreeAndNil(LoadCtx);
end.
