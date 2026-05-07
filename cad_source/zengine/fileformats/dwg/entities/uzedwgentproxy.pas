{*************************************************************************** }
{  fpdwg - DWG proxy / unknown fallback mapper (Stage 7)                     }
{                                                                            }
{        Copyright (C) 2026 Andrey Zubarev <zamtmn@yandex.ru>                }
{                                                                            }
{  This library is free software, licensed under the terms of the GNU        }
{  General Public License as published by the Free Software Foundation,      }
{  either version 3 of the License, or (at your option) any later version.   }
{*************************************************************************** }

{ Stage 7 (TZ §12.7 / audit §4.2): preserve copyable ACAD_PROXY_ENTITY
  graphics through the existing ZCAD proxy entity, and make unsupported
  unknown/opaque objects visible in diagnostics instead of silently ignoring
  them. Raw LibreDWG pointers are never stored past this mapper call. }

unit uzedwgentproxy;

{$Include zengineconfig.inc}
{$Mode delphi}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  uzbLogIntf,
  SysUtils,
  dwg, dwgproc,
  uzedwghandle,
  uzedrawingsimple,
  uzeentity,
  uzeentsubordinated,
  uzeentacdproxy,
  uzedwgentityregistry,
  uzeffmanager,
  uzedwgtypes,
  uzedwgdiagnostics,
  uzedwgimport;

implementation

function Stage7Stats: PDWGImportStats;
begin
  Result := nil;
  if GetLoadCtx <> nil then
    Result := GetLoadCtx.GetStatsRef;
end;

function DWGVersionToDXFFileVersion(Version: DWG_VERSION_TYPE): Integer;
begin
  if Version = R_INVALID then
    Exit(0);
  if Ord(Version) >= Ord(R_2007a) then
    Result := 1021
  else if Ord(Version) >= Ord(R_2004a) then
    Result := 1018
  else
    Result := 1015;
end;

procedure MarkSkipped(Handle: TDWGZCADHandle);
begin
  if (Handle <> 0) and (GetLoadCtx <> nil) then
    GetLoadCtx.MarkShellState(Handle, msSkipped);
end;

procedure Warn(Severity: TDWGImportSeverity; Code: Integer;
  Handle: TDWGZCADHandle; const Text: string);
begin
  if GetLoadCtx <> nil then
    GetLoadCtx.RaiseWarning(Severity, Code, Handle, Text);
end;

procedure ApplyCommonProps(Pobj: PGDBObjEntity; var DWGObject: Dwg_Object);
var
  Props: TDWGEntityCommonProps;
begin
  if (Pobj = nil) or not DWGEntityCommonPropsValue(DWGObject, Props) then
    Exit;
  Pobj^.vp.Color := Props.ColorIndex;
  Pobj^.vp.LineWeight := Props.LineWeight;
  Pobj^.vp.LineTypeScale := Props.LineTypeScale;
end;

procedure AddProxyEntity(var ZContext: TZDrawingContext;
  var DWGContext: TDWGCtx; var DWGObject: Dwg_Object;
  PProxy: PDwg_Entity_PROXY_ENTITY);
var
  Handle: TDWGZCADHandle;
  Payload: TDWGProxyEntityPayload;
  Proxy: PGDBObjAcdProxy;
  Pobj: PGDBObjEntity;
  Stats: PDWGImportStats;
begin
  Handle := DWGObjectHandleValue(DWGObject);
  Stats := Stage7Stats;

  if PProxy = nil then begin
    if Stats <> nil then begin
      Inc(Stats^.ProxiesFailed);
      Inc(Stats^.DroppedDueToFreedRaw);
    end;
    Warn(wsError, DWG_WARN_PROXY_CORRUPT, Handle,
      Format('ACAD_PROXY_ENTITY %s has no LibreDWG proxy payload; skipped',
        [IntToHex(Handle, 1)]));
    MarkSkipped(Handle);
    Exit;
  end;

  DWGCopyProxyEntityPayload(PProxy, Payload);
  if not Payload.HasGraphic then begin
    if Stats <> nil then
      Inc(Stats^.ProxiesFailed);
    Warn(wsWarning, DWG_WARN_PROXY_NO_GRAPHICS, Handle,
      Format('ACAD_PROXY_ENTITY %s has no proxy graphic bytes; skipped',
        [IntToHex(Handle, 1)]));
    MarkSkipped(Handle);
    Exit;
  end;

  Proxy := AllocAndInitAcdProxy(nil);
  Proxy^.SetProxyGraphicData(Payload.Graphic,
    Payload.ProxyID, Payload.ClassID, Payload.EntityDataSize, 0,
    Payload.DWGVersions, Payload.FromDXF,
    DWGVersionToDXFFileVersion(DWGContext.DWGVer));
  Pobj := PGDBObjEntity(Proxy);
  ApplyCommonProps(Pobj, DWGObject);

  if Stats <> nil then
    Inc(Stats^.ProxiesLoaded);
  zDebugLn(['{WH}DWG PROXY_ENTITY handle=', IntToHex(Handle, 1),
    ' graphic_bytes=', Length(Payload.Graphic),
    ' class=', Payload.ClassID,
    ' dwgver=', DWG_V2Str(DWGContext.DWGVer)]);

  if GetLoadCtx <> nil then
    DWGRegisterEntityShell(Pobj, DWGObject, False, 0)
  else
    ZContext.PDrawing^.pObjRoot^.AddMi(PGDBObjSubordinated(Pobj));
end;

procedure AddProxyObject(var ZContext: TZDrawingContext;
  var DWGContext: TDWGCtx; var DWGObject: Dwg_Object; PObject: Pointer);
var
  Handle: TDWGZCADHandle;
  Stats: PDWGImportStats;
begin
  Handle := DWGObjectHandleValue(DWGObject);
  Stats := Stage7Stats;
  if Stats <> nil then begin
    Inc(Stats^.UnknownObjects);
    Inc(Stats^.ProxiesFailed);
  end;
  Warn(wsInfo, DWG_WARN_PROXY_NO_GRAPHICS, Handle,
    Format('ACAD_PROXY_OBJECT %s is non-graphical; skipped',
      [IntToHex(Handle, 1)]));
  MarkSkipped(Handle);
end;

procedure AddUnknownEntity(var ZContext: TZDrawingContext;
  var DWGContext: TDWGCtx; var DWGObject: Dwg_Object; PEntity: Pointer);
var
  Handle: TDWGZCADHandle;
  Stats: PDWGImportStats;
begin
  Handle := DWGObjectHandleValue(DWGObject);
  Stats := Stage7Stats;
  if Stats <> nil then begin
    Inc(Stats^.UnknownEntities);
    Inc(Stats^.DroppedDueToFreedRaw);
  end;
  Warn(wsWarning, DWG_WARN_UNKNOWN_ENTITY, Handle,
    Format('Unknown DWG entity type %d at handle %s has no stable copied fallback; skipped',
      [Ord(DWGObject.fixedtype), IntToHex(Handle, 1)]));
  MarkSkipped(Handle);
end;

procedure AddOpaqueEntityWithoutProxy(var ZContext: TZDrawingContext;
  var DWGContext: TDWGCtx; var DWGObject: Dwg_Object; PEntity: Pointer);
var
  Handle: TDWGZCADHandle;
  Stats: PDWGImportStats;
begin
  Handle := DWGObjectHandleValue(DWGObject);
  Stats := Stage7Stats;
  if Stats <> nil then begin
    Inc(Stats^.UnknownEntities);
    Inc(Stats^.DroppedDueToFreedRaw);
  end;
  Warn(wsWarning, DWG_WARN_UNKNOWN_NO_COPY, Handle,
    Format('Unsupported opaque DWG entity type %d at handle %s has no proxy graphic fallback; skipped',
      [Ord(DWGObject.fixedtype), IntToHex(Handle, 1)]));
  MarkSkipped(Handle);
end;

procedure AddUnknownObject(var ZContext: TZDrawingContext;
  var DWGContext: TDWGCtx; var DWGObject: Dwg_Object; PObject: Pointer);
var
  Handle: TDWGZCADHandle;
  Stats: PDWGImportStats;
begin
  Handle := DWGObjectHandleValue(DWGObject);
  Stats := Stage7Stats;
  if Stats <> nil then begin
    Inc(Stats^.UnknownObjects);
    Inc(Stats^.DroppedDueToFreedRaw);
  end;
  Warn(wsWarning, DWG_WARN_UNKNOWN_OBJECT, Handle,
    Format('Unknown DWG object type %d at handle %s has no ZCAD object fallback; skipped',
      [Ord(DWGObject.fixedtype), IntToHex(Handle, 1)]));
  MarkSkipped(Handle);
end;

initialization
  RegisterDWGEntityHandler(DWG_TYPE_PROXY_ENTITY, @AddProxyEntity);
  RegisterDWGObjectHandler(DWG_TYPE_PROXY_OBJECT, @AddProxyObject);
  RegisterDWGEntityHandler(DWG_TYPE_UNKNOWN_ENT, @AddUnknownEntity);
  RegisterDWGObjectHandler(DWG_TYPE_UNKNOWN_OBJ, @AddUnknownObject);
  RegisterDWGEntityHandler(DWG_TYPE__3DSOLID, @AddOpaqueEntityWithoutProxy);
  RegisterDWGEntityHandler(DWG_TYPE_REGION, @AddOpaqueEntityWithoutProxy);
  RegisterDWGEntityHandler(DWG_TYPE_BODY, @AddOpaqueEntityWithoutProxy);
end.
