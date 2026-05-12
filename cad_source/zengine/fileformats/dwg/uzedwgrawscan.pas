{*************************************************************************** }
{  fpdwg - DWG raw object scan (Stage 5.x R4 / Phase 1)                      }
{                                                                            }
{        Copyright (C) 2026 Andrey Zubarev <zamtmn@yandex.ru>                }
{                                                                            }
{  This library is free software, licensed under the terms of the GNU        }
{  General Public License as published by the Free Software Foundation,      }
{  either version 3 of the License, or (at your option) any later version.   }
{*************************************************************************** }

{ Refactor R4 (per TZ_DWG_LOAD_TO_ZCAD_AUDIT §3.4 / TZ §4.3, §5.3):
  Phase 1 raw scan over `Raw.&object[i]` carried out before any allocation
  takes place. The scan does three things, all of which used to be tangled
  with mapper allocation in `parseDwg_Data`:

    * register handle -> raw index in TDWGZCADHandleEntry.RawIndex (the
      field has existed since R2 but mappers always passed -1);
    * detect duplicate handles up front so DWG_WARN_DUPLICATE_HANDLE is
      raised exactly once per duplicate, not every time a mapper tries to
      RegisterShell the same handle later;
    * record supertype/fixedtype on the placeholder entry so future
      routing decisions (Stages 6-8: INSERT, HATCH, DIM) can branch on the
      raw type without re-walking the LibreDWG array.

  After ScanRawObjects every well-formed handle is present in the registry
  with kind=dokUnknown and ptr=nil. Mapper-side RegisterShell upgrades the
  placeholder to its real kind / pointer; a duplicate now means the mapper
  saw the same handle twice for real, which is what the warning is for. }

unit uzedwgrawscan;

{$Include zengineconfig.inc}
{$Mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  SysUtils,
  dwg,
  uzedwgtypes,
  uzedwghandle,
  uzedwgloadcontext;

type
  { Per-object metadata captured by the raw scan so callers can branch on the
    DWG supertype/fixedtype without dereferencing the LibreDWG array a second
    time. The record is intentionally small — it is stored alongside the
    handle map entry, not as a separate parallel array. }
  TDWGRawObjectInfo = record
    Handle:    TDWGZCADHandle;
    RawIndex:  Integer;
    Supertype: DWG_OBJECT_SUPERTYPE;
    FixedType: DWG_OBJECT_TYPE;
  end;

{ Phase 1 entry point. Iterates every Raw.&object[i] and pre-registers the
  handle map. Safe to call with an empty Raw (num_objects=0) or with a Ctx
  that already holds entries — existing entries are left alone, the scan
  only fills in placeholders for handles that have not been registered yet
  (this keeps unit tests that pre-seed the registry working without change). }
procedure ScanRawObjects(var Raw: Dwg_Data; Ctx: TDWGZCADLoadContext);

implementation

procedure ScanRawObjects(var Raw: Dwg_Data; Ctx: TDWGZCADLoadContext);
var
  I: BITCODE_BL;
  Handle: TDWGZCADHandle;
  Existing: TDWGZCADHandleEntry;
  MutEntry: PDWGZCADHandleEntry;
begin
  if Ctx = nil then
    Exit;
  if Raw.num_objects = 0 then
    Exit;
  // Walking the array via &object[i] mirrors parseDwg_Data so any pointer
  // arithmetic mistake in the binding would surface in both places at once.
  I := 0;
  while I < Raw.num_objects do begin
    Handle := DWGObjectHandleValue(Raw.&object[I]);
    // Handle 0 is reserved for the model-space root (registered in
    // BeginDWGImport) and for raw entries LibreDWG could not decode. Skipping
    // them here keeps the duplicate detector from firing on every truncated
    // object in a partial-read scenario.
    if Handle <> 0 then begin
      if Ctx.Handles.TryGet(Handle, Existing) then begin
        // A real duplicate at the raw level: two LibreDWG entries claim the
        // same handle. Only warn if the existing entry was not itself a raw
        // placeholder (RawIndex >= 0 means the placeholder slot is taken).
        if Existing.RawIndex >= 0 then
          Ctx.RaiseWarning(wsWarning, DWG_WARN_DUPLICATE_HANDLE, Handle,
            Format('Raw scan: duplicate handle %s at index %d (first seen at %d)',
              [IntToHex(Handle, 1), I, Existing.RawIndex]));
      end else begin
        // Place a kind=dokUnknown, ptr=nil placeholder. Mappers running later
        // will upgrade it; if no mapper claims the handle the placeholder
        // still records the raw index, which is what TZ §6.5 asks for.
        Ctx.RegisterShell(Handle, dokUnknown, nil, Integer(I));
      end;
      // Issue #1198 P2 (TZ §5): capture fixedtype on every placeholder so
      // the histogram diagnostic can enumerate raw-object kinds without a
      // second walk. The write goes through TryGetMutable so the duplicate
      // branch above still records the type (LibreDWG sometimes emits the
      // same handle twice in proxy/zombie blocks; we want the count to
      // reflect both entries).
      if Ctx.Handles.TryGetMutable(Handle, MutEntry) and (MutEntry <> nil) then
        MutEntry^.FixedType := Raw.&object[I].fixedtype;
    end;
    Inc(I);
  end;
end;

end.
