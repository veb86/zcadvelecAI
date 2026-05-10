{*************************************************************************** }
{  fpdwg - DWG ATTRIB / ATTDEF entity mapper (Stage 6)                      }
{                                                                            }
{        Copyright (C) 2026 Andrey Zubarev <zamtmn@yandex.ru>                }
{                                                                            }
{  This library is free software, licensed under the terms of the GNU        }
{  General Public License as published by the Free Software Foundation,      }
{  either version 3 of the License, or (at your option) any later version.   }
{*************************************************************************** }

unit uzedwgentattrib;

{$Include zengineconfig.inc}
{$Mode delphi}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  uzbLogIntf,
  SysUtils,
  dwg, dwgproc, uzedwghandle, uzedwgtext,
  uzedrawingsimple,
  uzegeometry,
  uzeenttext, uzeentabstracttext, uzeentity,
  uzeentsubordinated,
  uzedwgentityregistry,
  uzeffmanager,
  uzedwgimport;

implementation

type
  PDwg_Entity_ATTRIB = ^Dwg_Entity_ATTRIB;
  PDwg_Entity_ATTDEF = ^Dwg_Entity_ATTDEF;

function DWGTextJustify(Horiz, Vert: Integer): TTextJustify;
begin
  if (Vert < 0) or (Vert > 3) or (Horiz < 0) or (Horiz > 4) then
    Exit(jstl);
  Result := jt[Vert, Horiz];
end;

procedure ApplyTextRotation(PObj: PGDBObjText; Rotation: Double);
begin
  if Rotation = 0 then
    Exit;
  PObj^.Local.basis.ox := GetXfFromZ(PObj^.Local.basis.oz);
  PObj^.Local.basis.ox := VectorTransform3D(PObj^.Local.basis.ox,
    CreateAffineRotationMatrix(PObj^.Local.basis.oz, -Rotation));
end;

procedure ApplyAttribText(PObj: PGDBObjText; var DWGContext: TDWGCtx;
  const InsertPoint, AlignPoint: BITCODE_2DPOINT; Elevation, Height,
  WidthFactor, Oblique, Rotation: Double; Generation, HorizAlignment,
  VertAlignment: Integer; TextValue: BITCODE_T);
var
  Value: string;
begin
  if DWGTextUsesAlignmentPoint(0, HorizAlignment, VertAlignment) then begin
    PObj^.Local.p_insert.x := AlignPoint.x;
    PObj^.Local.p_insert.y := AlignPoint.y;
  end else begin
    PObj^.Local.p_insert.x := InsertPoint.x;
    PObj^.Local.p_insert.y := InsertPoint.y;
  end;
  PObj^.Local.p_insert.z := Elevation;
  PObj^.P_drawInOCS := NulVertex;
  PObj^.textprop.size := Height;
  if WidthFactor <> 0 then
    PObj^.textprop.wfactor := WidthFactor
  else
    PObj^.textprop.wfactor := 1;
  PObj^.textprop.oblique := Oblique;
  PObj^.textprop.justify := DWGTextJustify(HorizAlignment, VertAlignment);
  PObj^.textprop.backward := (Generation and 2) <> 0;
  PObj^.textprop.upsidedown := (Generation and 4) <> 0;
  DWGSafeDecodeText(TextValue, DWGContext.DWGVer, DWGContext.DWGCodePage,
    Value);
  PObj^.Content := UnicodeString(Value);
  ApplyTextRotation(PObj, Rotation);
end;

procedure RegisterAttribShell(PObj: PGDBObjText; var ZContext: TZDrawingContext;
  var DWGObject: Dwg_Object; StyleRef: BITCODE_H);
var
  StyleHandle: QWord;
begin
  if not DWGRefHandleValue(StyleRef, StyleHandle) then
    StyleHandle := 0;
  if GetLoadCtx <> nil then
    DWGRegisterEntityShell(PGDBObjEntity(PObj), DWGObject, True,
      StyleHandle)
  else
    ZContext.PDrawing^.pObjRoot^.AddMi(PGDBObjSubordinated(PObj));
end;

procedure AddAttribEntity(var ZContext: TZDrawingContext;
  var DWGContext: TDWGCtx; var DWGObject: Dwg_Object;
  PAttrib: PDwg_Entity_ATTRIB);
var
  PObj: PGDBObjText;
  TagText: string;
begin
  if PAttrib = nil then
    Exit;

  PObj := AllocAndInitText(nil);
  ApplyAttribText(PObj, DWGContext, PAttrib^.ins_pt,
    PAttrib^.alignment_pt, PAttrib^.elevation, PAttrib^.height,
    PAttrib^.width_factor, PAttrib^.oblique_angle, PAttrib^.rotation,
    PAttrib^.generation, PAttrib^.horiz_alignment,
    PAttrib^.vert_alignment, PAttrib^.text_value);
  DWGSafeDecodeText(PAttrib^.tag, DWGContext.DWGVer, DWGContext.DWGCodePage,
    TagText);
  zDebugLn(['{WH}DWG ATTRIB handle=', IntToHex(DWGObjectHandleValue(
    DWGObject), 1), ' tag=', TagText, ' flags=', PAttrib^.flags]);
  RegisterAttribShell(PObj, ZContext, DWGObject, PAttrib^.style);
end;

procedure AddAttDefEntity(var ZContext: TZDrawingContext;
  var DWGContext: TDWGCtx; var DWGObject: Dwg_Object;
  PAttDef: PDwg_Entity_ATTDEF);
var
  PObj: PGDBObjText;
  TagText, PromptText: string;
begin
  if PAttDef = nil then
    Exit;

  PObj := AllocAndInitText(nil);
  ApplyAttribText(PObj, DWGContext, PAttDef^.ins_pt,
    PAttDef^.alignment_pt, PAttDef^.elevation, PAttDef^.height,
    PAttDef^.width_factor, PAttDef^.oblique_angle, PAttDef^.rotation,
    PAttDef^.generation, PAttDef^.horiz_alignment,
    PAttDef^.vert_alignment, PAttDef^.default_value);
  DWGSafeDecodeText(PAttDef^.tag, DWGContext.DWGVer,
    DWGContext.DWGCodePage, TagText);
  DWGSafeDecodeText(PAttDef^.prompt, DWGContext.DWGVer,
    DWGContext.DWGCodePage, PromptText);
  zDebugLn(['{WH}DWG ATTDEF handle=', IntToHex(DWGObjectHandleValue(
    DWGObject), 1), ' tag=', TagText, ' prompt=', PromptText,
    ' flags=', PAttDef^.flags]);
  RegisterAttribShell(PObj, ZContext, DWGObject, PAttDef^.style);
end;

initialization
  RegisterDWGEntityHandler(DWG_TYPE_ATTRIB, @AddAttribEntity);
  RegisterDWGEntityHandler(DWG_TYPE_ATTDEF, @AddAttDefEntity);
end.
