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
  dwg, dwgproc,
  uzedrawingsimple,
  uzeenttext, uzeentabstracttext, uzeentity,
  uzeentsubordinated,
  uzedwgloadcontext,
  uzedwgentityregistry,
  uzedwgimport;

implementation

procedure AddTextEntity(var ZContext: TZDrawingContext;
  var DWGContext: TDWGCtx; var DWGObject: Dwg_Object; PText: PDwg_Entity_TEXT);
var
  pobj: PGDBObjText;
  Props: TDWGTextProps;
  StyleHandle: QWord;
  WantStyle: Boolean;
  uniValue: UnicodeString;
begin
  pobj := AllocAndInitText(nil);
  DWGCopyTextProps(PText^, DWGContext.DWGVer, Props);
  pobj^.Local.p_insert.x := Props.InsertX;
  pobj^.Local.p_insert.y := Props.InsertY;
  pobj^.Local.p_insert.z := Props.InsertZ;
  pobj^.textprop.size := Props.Height;
  if Props.WidthFactor <> 0 then
    pobj^.textprop.wfactor := Props.WidthFactor
  else
    pobj^.textprop.wfactor := 1;
  pobj^.textprop.oblique := Props.Oblique;
  // Stage 5 (TZ §12.5): DWGSafeDecodeText already returns the payload as a
  // RTL string (UTF-16 path goes through punicodechar -> system codepage).
  // Assigning to a UnicodeString-typed Content lets FPC promote it back via
  // the default conversion.
  uniValue := UnicodeString(Props.Value);
  pobj^.Content := uniValue;
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
