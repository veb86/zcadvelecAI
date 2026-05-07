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
  dwg, dwgproc, uzedwghandle,
  uzedrawingsimple,
  uzbstrproc,
  uzestyleslayers, uzestyleslinetypes, uzestylestexts, uzestylesdim,
  uzeTypes,
  uzedwgloadcontext,
  uzedwgentityregistry,
  uzeffmanager,
  uzedwgtypes,
  uzedwgimport;

implementation

type
  PDwg_Object_DIMSTYLE = ^Dwg_Object_DIMSTYLE;

procedure AddLayer(var ZContext: TZDrawingContext; var DWGContext: TDWGCtx;
  var DWGObject: Dwg_Object; PDWGLayer: PDwg_Object_LAYER);
var
  player: PGDBLayerProp;
  name: string;
  LayerProps: TDWGLayerVisualProps;
  Handle, LtHandle: QWord;
  ContinuousLT: PGDBLtypeProp;
  Ctx: TDWGZCADLoadContext;
begin
  BITCODE_T2Text(PDWGLayer^.name, DWGContext, name);
  zDebugLn(['{WH}Layer: ', name]);
  if DWGContext.DWGVer > R_2007 then
    name := Tria_Utf8ToAnsi(name);
  player := ZContext.PDrawing^.LayerTable.MergeItem(name, ZContext.LoadMode);
  if player <> nil then begin
    player^.init(name);
    DWGLayerVisualPropsValue(PDWGLayer, LayerProps);
    player^.color := LayerProps.ColorIndex;
    player^.lineweight := LayerProps.LineWeight;
    player^._on := LayerProps.On;
    player^._lock := LayerProps.Locked;
    player^._print := LayerProps.Plot;
    zDebugLn(['{WH}layer ', name,
      ' visual color=', player^.color,
      ', lineweight=', player^.lineweight,
      ', on=', BoolToStr(player^._on, True),
      ', raw_off=', BoolToStr(PDWGLayer^.off <> 0, True),
      ', locked=', BoolToStr(player^._lock, True),
      ', plot=', BoolToStr(player^._print, True),
      ', color.index=', PDWGLayer^.color.index,
      ', color.raw=', PDWGLayer^.color.raw,
      ', color.rgb=$', IntToHex(PDWGLayer^.color.rgb, 8),
      ', color.method=', DWGColorMethodToText(PDWGLayer^.color.method),
      '($', IntToHex(Ord(PDWGLayer^.color.method), 2), ')',
      ', color.flag=', PDWGLayer^.color.flag]);
    if DWGColorLooksLikeLostACI(PDWGLayer^.color) then
      zDebugLn(['{WH}layer ', name,
        ' color diagnostic: LibreDWG reported ACI white without raw index; ',
        'original DWG layer ACI may be unavailable after RGB normalization']);
    //desk:AnsiString;
  end;
  Ctx := GetLoadCtx;
  if Ctx <> nil then begin
    Handle := DWGObjectHandleValue(DWGObject);
    if Handle <> 0 then
      Ctx.RegisterShell(Handle, dokLayer, player, -1);
    // Stage 3 (TZ §8.3): defer the layer.LT assignment until all LTYPE shells
    // have been registered. Issue #1122: this uses a layer-specific ref slot
    // because the target pointer is PGDBLayerProp, not PGDBObjEntity.
    if (player <> nil) and (Handle <> 0) then begin
      if not DWGLayerLineTypeHandleValue(PDWGLayer, LtHandle) then
        LtHandle := 0;
      ContinuousLT := PGDBLtypeProp(ZContext.PDrawing^.LTypeStyleTable.getAddres(
        'Continuous'));
      if ContinuousLT = nil then
        ContinuousLT := ZContext.PDrawing^.LTypeStyleTable.GetSystemLT(
          TLTContinous);
      Ctx.QueueRefResolve(player, Handle, LtHandle,
        dokLineType, rsLayerLineType, ContinuousLT);
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
  Props: TDWGTextStyleProps;
  TextProp: GDBTextStyleProp;
  name, StyleName, FontFile, FontFamily: string;
  Handle: QWord;
  Ctx: TDWGZCADLoadContext;
  UsedInLType: Boolean;
begin
  if not DWGTextStylePropsValue(PDWGStyle, DWGContext.DWGVer, Props) then
    Exit;
  name := Props.Name;
  zDebugLn(['{WH}TextStyle: ', name]);
  FontFile := Props.FontFile;
  FontFamily := '';
  if DWGContext.DWGVer > R_2007 then begin
    name := Tria_Utf8ToAnsi(name);
    FontFile := Tria_Utf8ToAnsi(FontFile);
  end;
  UsedInLType := Props.IsShape;
  if UsedInLType and (FontFile <> '') then
    StyleName := FontFile
  else if name <> '' then
    StyleName := name
  else
    StyleName := 'Standard';

  TextProp.size := Props.TextSize;
  TextProp.wfactor := Props.WidthFactor;
  TextProp.oblique := Props.ObliqueAngle;

  pstyle := ZContext.PDrawing^.TextStyleTable.FindStyle(StyleName,
    UsedInLType);
  if pstyle <> nil then begin
    if ZContext.LoadMode = TLOLoad then
      pstyle := ZContext.PDrawing^.TextStyleTable.setstyle(StyleName,
        FontFile, FontFamily, TextProp, UsedInLType);
  end else
    pstyle := ZContext.PDrawing^.TextStyleTable.addstyle(StyleName,
      FontFile, FontFamily, TextProp, UsedInLType);

  Ctx := GetLoadCtx;
  if Ctx <> nil then begin
    Handle := DWGObjectHandleValue(DWGObject);
    if Handle <> 0 then
      Ctx.RegisterShell(Handle, dokTextStyle, pstyle, -1);
    if (pstyle <> nil) and (not UsedInLType) and
       ((Ctx.FallbackTextStyle = nil) or
        (CompareText(StyleName, 'Standard') = 0)) then
      Ctx.SetFallbackTextStyle(pstyle);
  end;
  if (pstyle <> nil) and (not UsedInLType) and
     (ZContext.PDrawing^.CurrentTextStyle = nil) then
    ZContext.PDrawing^.CurrentTextStyle := pstyle;
end;

procedure ApplyDimStyleScalars(PDimStyle: PGDBDimStyle;
  PDWGDimStyle: PDwg_Object_DIMSTYLE);
begin
  if (PDimStyle = nil) or (PDWGDimStyle = nil) then
    Exit;
  if PDWGDimStyle^.DIMEXE <> 0 then
    PDimStyle^.Lines.DIMEXE := PDWGDimStyle^.DIMEXE;
  if PDWGDimStyle^.DIMEXO <> 0 then
    PDimStyle^.Lines.DIMEXO := PDWGDimStyle^.DIMEXO;
  if PDWGDimStyle^.DIMDLE <> 0 then
    PDimStyle^.Lines.DIMDLE := PDWGDimStyle^.DIMDLE;
  if PDWGDimStyle^.DIMCEN <> 0 then
    PDimStyle^.Lines.DIMCEN := PDWGDimStyle^.DIMCEN;
  if PDWGDimStyle^.DIMLWD <> 0 then
    PDimStyle^.Lines.DIMLWD := PDWGDimStyle^.DIMLWD;
  if PDWGDimStyle^.DIMLWE <> 0 then
    PDimStyle^.Lines.DIMLWE := PDWGDimStyle^.DIMLWE;
  if PDWGDimStyle^.DIMCLRD_N <> 0 then
    PDimStyle^.Lines.DIMCLRD := PDWGDimStyle^.DIMCLRD_N;
  if PDWGDimStyle^.DIMCLRE_N <> 0 then
    PDimStyle^.Lines.DIMCLRE := PDWGDimStyle^.DIMCLRE_N;

  if PDWGDimStyle^.DIMSCALE <> 0 then
    PDimStyle^.Units.DIMSCALE := PDWGDimStyle^.DIMSCALE;
  if PDWGDimStyle^.DIMLFAC <> 0 then
    PDimStyle^.Units.DIMLFAC := PDWGDimStyle^.DIMLFAC;
  if PDWGDimStyle^.DIMRND <> 0 then
    PDimStyle^.Units.DIMRND := PDWGDimStyle^.DIMRND;
  if PDWGDimStyle^.DIMDEC <> 0 then
    PDimStyle^.Units.DIMDEC := PDWGDimStyle^.DIMDEC;
  if PDWGDimStyle^.DIMZIN <> 0 then
    PDimStyle^.Units.DIMZIN := PDWGDimStyle^.DIMZIN;

  if PDWGDimStyle^.DIMASZ <> 0 then
    PDimStyle^.Arrows.DIMASZ := PDWGDimStyle^.DIMASZ;

  if PDWGDimStyle^.DIMTXT <> 0 then
    PDimStyle^.Text.DIMTXT := PDWGDimStyle^.DIMTXT;
  if PDWGDimStyle^.DIMGAP <> 0 then
    PDimStyle^.Text.DIMGAP := PDWGDimStyle^.DIMGAP;
  if PDWGDimStyle^.DIMCLRT_N <> 0 then
    PDimStyle^.Text.DIMCLRT := PDWGDimStyle^.DIMCLRT_N;
  PDimStyle^.Text.DIMTIH := PDWGDimStyle^.DIMTIH <> 0;
  PDimStyle^.Text.DIMTOH := PDWGDimStyle^.DIMTOH <> 0;
end;

procedure AddDimStyle(var ZContext: TZDrawingContext; var DWGContext: TDWGCtx;
  var DWGObject: Dwg_Object; PDWGDimStyle: PDwg_Object_DIMSTYLE);
var
  PDimStyle: PGDBDimStyle;
  Name: string;
  Handle: QWord;
  Ctx: TDWGZCADLoadContext;
begin
  BITCODE_T2Text(PDWGDimStyle^.name, DWGContext, Name);
  if Name = '' then
    Name := 'Standard';
  zDebugLn(['{WH}DimStyle: ', Name]);
  if DWGContext.DWGVer > R_2007 then
    Name := Tria_Utf8ToAnsi(Name);

  PDimStyle := PGDBDimStyle(ZContext.PDrawing^.DimStyleTable.getAddres(Name));
  if PDimStyle = nil then begin
    PDimStyle := PGDBDimStyle(ZContext.PDrawing^.DimStyleTable.MergeItem(Name,
      ZContext.LoadMode));
    if PDimStyle <> nil then begin
      PDimStyle^.init(Name);
      PDimStyle^.SetDefaultValues;
    end;
  end;
  if PDimStyle = nil then
    PDimStyle := DWGEnsureDimStyle(ZContext.PDrawing^);
  ApplyDimStyleScalars(PDimStyle, PDWGDimStyle);
  if (PDimStyle <> nil) and (PDimStyle^.Text.DIMTXSTY = nil) then
    PDimStyle^.Text.DIMTXSTY := ZContext.PDrawing^.TextStyleTable.FindStyle(
      'Standard', False);
  if (PDimStyle <> nil) and (ZContext.PDrawing^.CurrentDimStyle = nil) then
    ZContext.PDrawing^.CurrentDimStyle := PDimStyle;

  Ctx := GetLoadCtx;
  if Ctx <> nil then begin
    Handle := DWGObjectHandleValue(DWGObject);
    if Handle <> 0 then
      Ctx.RegisterShell(Handle, dokDimStyle, PDimStyle, -1);
  end;
end;

initialization
  RegisterDWGObjectHandler(DWG_TYPE_LAYER, @AddLayer);
  RegisterDWGObjectHandler(DWG_TYPE_LTYPE, @AddLineType);
  RegisterDWGObjectHandler(DWG_TYPE_STYLE, @AddTextStyle);
  RegisterDWGObjectHandler(DWG_TYPE_DIMSTYLE, @AddDimStyle);
end.
