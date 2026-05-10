{*************************************************************************** }
{  fpdwg - DWG TEXT entity mapper (Stage 5.x R6)                             }
{                                                                            }
{        Copyright (C) 2026 Andrey Zubarev <zamtmn@yandex.ru>                }
{                                                                            }
{  This library is free software, licensed under the terms of the GNU        }
{  General Public License as published by the Free Software Foundation,      }
{  either version 3 of the License, or (at your option) any later version.   }
{*************************************************************************** }

unit uzedwgenttext;

{$Include zengineconfig.inc}
{$Mode delphi}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  SysUtils,
  dwg, dwgproc,uzedwghandle,
  uzedrawingsimple,
  uzegeometry,
  uzeenttext, uzeentabstracttext, uzeentity,
  uzeentsubordinated,
  uzedwgloadcontext,
  uzedwgentityregistry,
  uzeffmanager,
  uzedwgimport;

implementation

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

procedure AddTextEntity(var ZContext: TZDrawingContext;
  var DWGContext: TDWGCtx; var DWGObject: Dwg_Object; PText: PDwg_Entity_TEXT);
var
  pobj: PGDBObjText;
  Props: TDWGTextProps;
  StyleHandle: QWord;
  TextX, TextY, TextZ: Double;
  WantStyle: Boolean;
  uniValue: UnicodeString;
begin
  pobj := AllocAndInitText(nil);
  DWGCopyTextProps(PText^, DWGContext.DWGVer, DWGContext.DWGCodePage, Props);
  DWGTextEffectiveInsertPoint(Props, TextX, TextY, TextZ);
  pobj^.Local.p_insert.x := TextX;
  pobj^.Local.p_insert.y := TextY;
  pobj^.Local.p_insert.z := TextZ;
  pobj^.P_drawInOCS := NulVertex;
  pobj^.textprop.size := Props.Height;
  if Props.WidthFactor <> 0 then
    pobj^.textprop.wfactor := Props.WidthFactor
  else
    pobj^.textprop.wfactor := 1;
  pobj^.textprop.oblique := Props.Oblique;
  pobj^.textprop.justify := DWGTextJustify(Props.HorizAlignment,
    Props.VertAlignment);
  pobj^.textprop.backward := (Props.Generation and 2) <> 0;
  pobj^.textprop.upsidedown := (Props.Generation and 4) <> 0;
  // Stage 5 (TZ §12.5): DWGSafeDecodeText already returns the payload as a
  // RTL string (UTF-16 path goes through punicodechar -> system codepage).
  // Assigning to a UnicodeString-typed Content lets FPC promote it back via
  // the default conversion.
  uniValue := UnicodeString(Props.Value);
  pobj^.Content := uniValue;
  ApplyTextRotation(pobj, Props.Rotation);
  WantStyle := DWGTextStyleHandleValue(PText, StyleHandle);
  if not WantStyle then
    StyleHandle := 0;
  if GetLoadCtx <> nil then
    DWGRegisterEntityShell(PGDBObjEntity(pobj), DWGObject, True, StyleHandle)
  else
    ZContext.PDrawing^.pObjRoot^.AddMi(PGDBObjSubordinated(pobj));
end;

initialization
  RegisterDWGEntityHandler(DWG_TYPE_TEXT, @AddTextEntity);
end.
