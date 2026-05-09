{*************************************************************************** }
{  fpdwg - DWG MTEXT entity mapper (Stage 5.x R6)                            }
{                                                                            }
{        Copyright (C) 2026 Andrey Zubarev <zamtmn@yandex.ru>                }
{                                                                            }
{  This library is free software, licensed under the terms of the GNU        }
{  General Public License as published by the Free Software Foundation,      }
{  either version 3 of the License, or (at your option) any later version.   }
{*************************************************************************** }

unit uzedwgentmtext;

{$Include zengineconfig.inc}
{$Mode delphi}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  SysUtils,
  dwg, dwgproc,uzedwghandle,
  uzedrawingsimple,
  uzeentmtext, uzeentabstracttext, uzeentity,
  uzeentsubordinated,
  uzedwgloadcontext,
  uzedwgentityregistry,
  uzeffmanager,
  uzedwgimport;

implementation

const
  DWGMTextJustifyToZCAD: array[TDWGMTextJustify] of TTextJustify =
    (jstl, jstc, jstr, jsml, jsmc, jsmr, jsbl, jsbc, jsbr);

function DWGMTextAttachmentToZCADJustify(Attachment: Integer): TTextJustify;
begin
  Result := DWGMTextJustifyToZCAD[DWGMTextAttachmentToJustify(Attachment)];
end;

procedure AddMTextEntity(var ZContext: TZDrawingContext;
  var DWGContext: TDWGCtx; var DWGObject: Dwg_Object;
  PMText: PDwg_Entity_MTEXT);
var
  pobj: PGDBObjMText;
  Props: TDWGMTextProps;
  StyleHandle: QWord;
  WantStyle: Boolean;
  uniValue: UnicodeString;
begin
  pobj := AllocAndInitMText(nil);
  DWGCopyMTextProps(PMText^, DWGContext.DWGVer, DWGContext.DWGCodePage,
    Props);
  pobj^.Local.p_insert.x := Props.InsertX;
  pobj^.Local.p_insert.y := Props.InsertY;
  pobj^.Local.p_insert.z := Props.InsertZ;
  pobj^.textprop.size := Props.TextHeight;
  pobj^.textprop.justify := DWGMTextAttachmentToZCADJustify(Props.Attachment);
  pobj^.Width := Props.RectWidth;
  if Props.LineSpaceFactor <> 0 then
    pobj^.linespacef := Props.LineSpaceFactor
  else
    pobj^.linespacef := 1;
  // Same widening contract as AddTextEntity — DWGSafeDecodeText already
  // returned the payload as RTL string; the FPC compiler promotes it.
  uniValue := UnicodeString(Props.Value);
  pobj^.Content := uniValue;
  WantStyle := DWGMTextStyleHandleValue(PMText, StyleHandle);
  if not WantStyle then
    StyleHandle := 0;
  if GetLoadCtx <> nil then
    DWGRegisterEntityShell(PGDBObjEntity(pobj), DWGObject, True, StyleHandle)
  else
    ZContext.PDrawing^.pObjRoot^.AddMi(PGDBObjSubordinated(pobj));
end;

initialization
  RegisterDWGEntityHandler(DWG_TYPE_MTEXT, @AddMTextEntity);
end.
