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
  uzestyleslayers,
  uzeentline,uzeentity,//uzgldrawcontext,
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
//type
  //PDwg_Entity_LINE=^Dwg_Entity_LINE;
  //PDwg_Object_LAYER=^Dwg_Object_LAYER;

{ Per-file load context. The DWG loader is invoked sequentially (no thread-
  per-file) so a unit-level instance is sufficient. Begin/End assert the
  expected lifecycle and clear the slot on End so a stale context can never
  leak into a subsequent load. }
var
  LoadCtx:TDWGZCADLoadContext=nil;
  LoadDrawing:PTSimpleDrawing=nil;

procedure DWGAttachEntity(Entity:Pointer;Owner:Pointer;
  Reason:TDWGAttachReason;Data:Pointer);
var
  pobj:PGDBObjEntity;
  newowner:PGDBObjGenericSubEntry;
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

  if LoadDrawing<>nil then begin
    PGDBObjEntity(pobj)^.BuildGeometry(LoadDrawing^);
    // FormatAfterDXFLoad/FromDXFPostProcessAfterAdd belong to the DXF code path
    // and require a TDrawContext that the DWG pipeline does not yet thread
    // through Stage 2. BuildGeometry is enough to satisfy the original bug
    // (line never built) without dragging the DXF post-processing chain in.
  end;
end;

procedure BeginDWGImport(var ZContext:TZDrawingContext);
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
  LoadCtx.RegisterShell(0,dokModelSpace,ZContext.PDrawing^.pObjRoot,-1);
end;

procedure EndDWGImport(var ZContext:TZDrawingContext);
begin
  if LoadCtx=nil then
    Exit;
  try
    LoadCtx.ResolveOwners;
    zDebugLn(['{WH}DWG owner resolve: attached=',LoadCtx.AttachCount,
      ', fallback=',LoadCtx.FallbackCount,
      ', cycles=',LoadCtx.CycleCount,
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
  Handle:QWord;
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
    //LT:Pointer;
    player^._on:=(PDWGLayer^.off=0);
    player^._lock:=(PDWGLayer^.locked<>0);
    player^._print:=(PDWGLayer^.plotflag<>0);
    //desk:AnsiString;
  end;
  if LoadCtx<>nil then begin
    Handle:=DWGObjectHandleValue(DWGObject);
    if Handle<>0 then
      LoadCtx.RegisterShell(Handle,dokLayer,player,-1);
  end;
end;

procedure AddLineType(var ZContext:TZDrawingContext;var DWGContext:TDWGCtx;var DWGObject:Dwg_Object;PDWGLType:PDwg_Object_LTYPE);
var
  //player:PGDBLayerProp;
  name:string;
  Handle:QWord;
begin
  BITCODE_T2Text(PDWGLType^.name,DWGContext,name);
  zDebugLn(['{WH}LineType: ',name]);
  if LoadCtx<>nil then begin
    Handle:=DWGObjectHandleValue(DWGObject);
    if Handle<>0 then
      LoadCtx.RegisterShell(Handle,dokLineType,nil,-1);
  end;
end;

procedure AddBlockHeader(var ZContext:TZDrawingContext;var DWGContext:TDWGCtx;var DWGObject:Dwg_Object;PDWGBlock_Header:PDwg_Object_BLOCK_HEADER);
var
  name:string;
  Handle:QWord;
begin
  BITCODE_T2Text(PDWGBlock_Header^.name,DWGContext,name);
  zDebugLn(['{WH}BlockHeader: ',name]);
  if LoadCtx<>nil then begin
    Handle:=DWGObjectHandleValue(DWGObject);
    if Handle<>0 then
      // Stage 2 does not yet build a real BlockDef object; we register the
      // block header handle as a container so children resolve to fallback
      // root rather than landing on a non-container layer/linetype. The
      // owner pointer is the model-space root for now: when the BlockDef
      // mapper lands the kind stays dokBlockDef but Ptr will be replaced.
      LoadCtx.RegisterShell(Handle,dokBlockDef,
        ZContext.PDrawing^.pObjRoot,-1);
  end;
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
  EntityHandle,OwnerHandle:QWord;
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
  end else begin
    // Compatibility fallback: if BeginDWGImport was not called the loader
    // still works (legacy single-pass behaviour). New callers always go
    // through Begin/End, so this branch is exercised only by future
    // experimental hosts that bypass the standard pipeline.
    ZContext.PDrawing^.pObjRoot^.AddMi(PGDBObjSubordinated(pobj));
  end;
end;

initialization
  ZCDWGParser.RegisterDWGObjectLoadProc(DWG_TYPE_LAYER,@AddLayer);
  ZCDWGParser.RegisterDWGObjectLoadProc(DWG_TYPE_LTYPE,@AddLineType);
  ZCDWGParser.RegisterDWGObjectLoadProc(DWG_TYPE_BLOCK_HEADER,@AddBlockHeader);

  ZCDWGParser.RegisterDWGEntityLoadProc(DWG_TYPE_LINE,@AddLineEntity);
  ZCDWGParser.RegisterDWGEntityLoadProc(DWG_TYPE_BLOCK,@AddBlock);
finalization
  if LoadCtx<>nil then
    FreeAndNil(LoadCtx);
end.
