{*************************************************************************** }
{  fpdwg - DWG LINE entity mapper (Stage 5.x R6)                             }
{                                                                            }
{        Copyright (C) 2026 Andrey Zubarev <zamtmn@yandex.ru>                }
{                                                                            }
{  This library is free software, licensed under the terms of the GNU        }
{  General Public License as published by the Free Software Foundation,      }
{  either version 3 of the License, or (at your option) any later version.   }
{*************************************************************************** }

{ Refactor R6 (per TZ_DWG_LOAD_TO_ZCAD_AUDIT §3.6 / TZ §6.5):
  AddLineEntity extracted from uzefflibredwg2ents.pas. Self-registers
  through uzedwgentityregistry; fallback path (LoadCtx=nil) attaches to
  pObjRoot directly so an experimental host that bypasses BeginDWGImport
  still yields a visible entity. }

unit uzedwgentline;

{$Include zengineconfig.inc}
{$Mode delphi}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  SysUtils,
  dwg, dwgproc,
  uzedrawingsimple,
  uzeentline, uzeentity,
  uzeentsubordinated,
  uzedwgloadcontext,
  uzedwgentityregistry,
  uzedwgimport;

implementation

procedure AddLineEntity(var ZContext: TZDrawingContext;
  var DWGContext: TDWGCtx; var DWGObject: Dwg_Object; PLine: PDwg_Entity_LINE);
var
  pobj: PGDBObjEntity;
  Endpoints: TDWGLineEndpoints;
  EntityHandle, OwnerHandle, LayerHandle, LtypeHandle: QWord;
  Ctx: TDWGZCADLoadContext;
begin
  // Stage 2 (TZ §12.2): allocate with nil owner, fill geometry, register the
  // shell + pending owner. The actual AddMi happens in DWGAttachEntity when
  // ResolveOwners runs after parseDwg_Data. The line is *never* added to
  // pObjRoot here — that was the original bug that caused entities to attach
  // to the model-space root before their block-def owner was visible.
  pobj := AllocAndInitLine(nil);
  DWGCopyLineEndpoints(PLine^, Endpoints);
  PGDBObjLine(pobj)^.CoordInOCS.lBegin.x := Endpoints.StartX;
  PGDBObjLine(pobj)^.CoordInOCS.lBegin.y := Endpoints.StartY;
  PGDBObjLine(pobj)^.CoordInOCS.lBegin.z := Endpoints.StartZ;
  PGDBObjLine(pobj)^.CoordInOCS.lEnd.x := Endpoints.EndX;
  PGDBObjLine(pobj)^.CoordInOCS.lEnd.y := Endpoints.EndY;
  PGDBObjLine(pobj)^.CoordInOCS.lEnd.z := Endpoints.EndZ;

  Ctx := GetLoadCtx;
  if Ctx <> nil then begin
    EntityHandle := DWGObjectHandleValue(DWGObject);
    if not DWGObjectOwnerHandleValue(DWGObject, OwnerHandle) then
      OwnerHandle := 0;
    if EntityHandle <> 0 then
      Ctx.RegisterShell(EntityHandle, dokEntity, pobj, -1);
    Ctx.QueueOwnerResolve(pobj, EntityHandle, OwnerHandle);
    // Stage 3 (TZ §12.3): queue layer + linetype refs so vp.Layer / vp.LineType
    // are populated before BuildGeometry runs. Handles missing/broken refs
    // by routing to the registered fallbacks (system layer / ByLayer).
    if not DWGEntityLayerHandleValue(DWGObject, LayerHandle) then
      LayerHandle := 0;
    if not DWGEntityLineTypeHandleValue(DWGObject, LtypeHandle) then
      LtypeHandle := 0;
    Ctx.QueueRefResolve(pobj, EntityHandle, LayerHandle,
      dokLayer, rsLayer, nil);
    Ctx.QueueRefResolve(pobj, EntityHandle, LtypeHandle,
      dokLineType, rsLineType, nil);
  end else begin
    // Compatibility fallback: if BeginDWGImport was not called the loader
    // still works (legacy single-pass behaviour). New callers always go
    // through Begin/End, so this branch is exercised only by future
    // experimental hosts that bypass the standard pipeline.
    ZContext.PDrawing^.pObjRoot^.AddMi(PGDBObjSubordinated(pobj));
  end;
end;

initialization
  RegisterDWGEntityHandler(DWG_TYPE_LINE, @AddLineEntity);
end.
