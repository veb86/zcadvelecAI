{*************************************************************************** }
{  fpdwg - DWG style table mappers (Stage 5.x R6)                            }
{                                                                            }
{        Copyright (C) 2026 Andrey Zubarev <zamtmn@yandex.ru>                }
{                                                                            }
{  This library is free software, licensed under the terms of the GNU        }
{  General Public License as published by the Free Software Foundation,      }
{  either version 3 of the License, or (at your option) any later version.   }
{*************************************************************************** }

{ Refactor R6 (per TZ_DWG_LOAD_TO_ZCAD_AUDIT §3.6 / TZ §6.5):
  LAYER, LTYPE, STYLE table mappers extracted from uzefflibredwg2ents.pas.
  Each registers itself through uzedwgentityregistry in the initialization
  section so the orchestration unit (uzefflibredwg.pas) does not need to
  know the individual handler names. Layer-LineType pending refs are
  enqueued here so the ResolveRefs pass can wire them once both shells
  exist (TZ §8.3). }

unit uzedwgtables;

{$Include zengineconfig.inc}
{$Mode delphi}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  uzbLogIntf,
  SysUtils,
  dwg, dwgproc,
  uzedrawingsimple,
  uzbstrproc,
  uzestyleslayers, uzestyleslinetypes, uzestylestexts,
  uzeTypes,
  uzedwgloadcontext,
  uzedwgentityregistry,
  uzedwgimport;

implementation

type
  // Stage 3: dwg.pp does not export PDwg_Object_STYLE so we declare it here
  // to keep the AddTextStyle signature symmetrical with AddLayer / AddLineType.
  PDwg_Object_STYLE = ^Dwg_Object_STYLE;

procedure AddLayer(var ZContext: TZDrawingContext; var DWGContext: TDWGCtx;
  var DWGObject: Dwg_Object; PDWGLayer: PDwg_Object_LAYER);
var
  player: PGDBLayerProp;
  name: string;
  Handle, LtHandle: QWord;
  Ctx: TDWGZCADLoadContext;
begin
  BITCODE_T2Text(PDWGLayer^.name, DWGContext, name);
  zDebugLn(['{WH}Layer: ', name]);
  if DWGContext.DWGVer > R_2007 then
    name := Tria_Utf8ToAnsi(name);
  player := ZContext.PDrawing^.LayerTable.MergeItem(name, ZContext.LoadMode);
  if player <> nil then begin
    player^.init(name);
    player^.color := PDWGLayer^.color.index;
    player^.lineweight := PDWGLayer^.linewt;
    player^._on := (PDWGLayer^.off = 0);
    player^._lock := (PDWGLayer^.locked <> 0);
    player^._print := (PDWGLayer^.plotflag <> 0);
    //desk:AnsiString;
  end;
  Ctx := GetLoadCtx;
  if Ctx <> nil then begin
    Handle := DWGObjectHandleValue(DWGObject);
    if Handle <> 0 then
      Ctx.RegisterShell(Handle, dokLayer, player, -1);
    // Stage 3 (TZ §8.3): defer the layer.LT linetype assignment until all
    // LTYPE shells have been registered. Use the layer pointer as the
    // "entity" so DWGAttachRef writes into the layer's LT slot once the
    // ltype handle resolves. RefHandle=0 is allowed: it falls back to
    // ByLayer/Continuous via the registered slot fallback.
    if (player <> nil) and (Handle <> 0) then begin
      if not DWGLayerLineTypeHandleValue(PDWGLayer, LtHandle) then
        LtHandle := 0;
      Ctx.QueueRefResolve(player, Handle, LtHandle,
        dokLineType, rsLineType, nil);
    end;
  end;
end;

procedure AddLineType(var ZContext: TZDrawingContext; var DWGContext: TDWGCtx;
  var DWGObject: Dwg_Object; PDWGLType: PDwg_Object_LTYPE);
var
  pltype: PGDBLtypeProp;
  name: string;
  Handle: QWord;
  Ctx: TDWGZCADLoadContext;
begin
  BITCODE_T2Text(PDWGLType^.name, DWGContext, name);
  zDebugLn(['{WH}LineType: ', name]);
  if DWGContext.DWGVer > R_2007 then
    name := Tria_Utf8ToAnsi(name);
  // Stage 3 (TZ §12.3): create the linetype in the table so refs can resolve
  // to a real pointer. We mirror the DXF loader semantics — a name collision
  // with a previously-loaded entry is left alone (TLOMerge respected).
  pltype := PGDBLtypeProp(ZContext.PDrawing^.LTypeStyleTable.MergeItem(name,
    ZContext.LoadMode));
  if pltype <> nil then begin
    if pltype^.Name = '' then
      pltype^.init(name);
    // Length / dash arrays are not yet decoded — Stage 3 only needs the
    // name+pointer to exist so that entity refs land in the correct slot.
    // Stage 4 will populate dasharray + strokesarray + description from the
    // DWG dashes.
  end;
  Ctx := GetLoadCtx;
  if Ctx <> nil then begin
    Handle := DWGObjectHandleValue(DWGObject);
    if Handle <> 0 then
      Ctx.RegisterShell(Handle, dokLineType, pltype, -1);
  end;
end;

procedure AddTextStyle(var ZContext: TZDrawingContext; var DWGContext: TDWGCtx;
  var DWGObject: Dwg_Object; PDWGStyle: PDwg_Object_STYLE);
var
  pstyle: PGDBTextStyle;
  name: string;
  Handle: QWord;
  Ctx: TDWGZCADLoadContext;
begin
  BITCODE_T2Text(PDWGStyle^.name, DWGContext, name);
  zDebugLn(['{WH}TextStyle: ', name]);
  if DWGContext.DWGVer > R_2007 then
    name := Tria_Utf8ToAnsi(name);
  // Stage 3 (TZ §12.3, "начать STYLE mapper"): just create the pointer so
  // future TEXT/MTEXT mappers have something to reference. Font file and
  // metrics fields are decoded in Stage 4 alongside text geometry support.
  pstyle := ZContext.PDrawing^.TextStyleTable.FindStyle(name, False);
  if pstyle = nil then
    pstyle := ZContext.PDrawing^.TextStyleTable.FindStyle('Standard', False);
  Ctx := GetLoadCtx;
  if Ctx <> nil then begin
    Handle := DWGObjectHandleValue(DWGObject);
    if Handle <> 0 then
      Ctx.RegisterShell(Handle, dokTextStyle, pstyle, -1);
  end;
end;

initialization
  RegisterDWGObjectHandler(DWG_TYPE_LAYER, @AddLayer);
  RegisterDWGObjectHandler(DWG_TYPE_LTYPE, @AddLineType);
  RegisterDWGObjectHandler(DWG_TYPE_STYLE, @AddTextStyle);
end.
