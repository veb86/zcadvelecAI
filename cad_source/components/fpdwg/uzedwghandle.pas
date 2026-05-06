{*************************************************************************** }
{  fpdwg - DWG handle helpers (Stage 1, refactored 5.x R3)                   }
{                                                                            }
{        Copyright (C) 2026 Andrey Zubarev <zamtmn@yandex.ru>                }
{                                                                            }
{  This library is free software, licensed under the terms of the GNU        }
{  General Public License as published by the Free Software Foundation,      }
{  either version 3 of the License, or (at your option) any later version.   }
{*************************************************************************** }

{ Refactor R3 (per TZ_DWG_LOAD_TO_ZCAD_AUDIT §3.3): handle-extraction
  helpers moved out of dwgproc.pp so the binding unit stays focused on the
  libredwg dynamic load surface. Each helper accepts already-decoded raw
  LibreDWG records so callers can drive them from fake fixtures in
  fpdwg_tests without libredwg.so being present.

  dwgproc.pp re-exports these names through its interface uses clause so
  existing callers (uzefflibredwg2ents.pas, uzedwgtestdwgproc.pas) keep
  compiling unchanged. }

unit uzedwghandle;

{$IFDEF FPC}
  {$PACKRECORDS C}
  {$Mode objfpc}{$H+}
  {$ModeSwitch advancedrecords}
{$ENDIF}

interface

uses
  dwg;

type
  { Stage 5 (TZ §12.5): the LibreDWG bindings only expose PDwg_Entity_LINE,
    so we declare the entity pointer aliases the Stage 5 mappers and the
    text-style helpers below need. Kept here (next to the helpers that use
    them) instead of in the binding unit so the binding stays a pure
    libredwg surface. }
  PDwg_Entity_TEXT  = ^Dwg_Entity_TEXT;
  PDwg_Entity_MTEXT = ^Dwg_Entity_MTEXT;

{ Object handle: the stable QWord identifier the import context indexes by. }
function DWGObjectHandleValue(const Obj: Dwg_Object): QWord;

{ Owner handle on a Dwg_Object. Returns False when the object has no owner
  (typical for top-level objects). The actual lookup goes through
  DWGRefHandleValue so a present-but-zero ownerhandle is treated as missing. }
function DWGObjectOwnerHandleValue(const Obj: Dwg_Object;
  out Value: QWord): Boolean;

{ Generic BITCODE_H decoder: returns False when the ref is nil or both
  absolute_ref and handleref.value are zero. The two fields are checked in
  order because LibreDWG fills absolute_ref for resolved cross-references
  but leaves it at 0 for soft pointers, which then carry the value in
  handleref.value. }
function DWGRefHandleValue(Ref: BITCODE_H; out Value: QWord): Boolean;

{ Stage 3 (TZ §12.3): pull layer / linetype refs off an entity-typed
  Dwg_Object. Returns False when Obj is not an entity, or when the ref is
  missing/null. Object-typed records (LAYER, LTYPE, BLOCK_HEADER) do not
  expose these slots, so callers reading from a non-entity object get False. }
function DWGEntityLayerHandleValue(const Obj: Dwg_Object;
  out Value: QWord): Boolean;
function DWGEntityLineTypeHandleValue(const Obj: Dwg_Object;
  out Value: QWord): Boolean;
function DWGLayerLineTypeHandleValue(const PLayer: PDwg_Object_LAYER;
  out Value: QWord): Boolean;

{ Stage 5 (TZ §12.5): TEXT/MTEXT mappers need the style ref. Returning
  False allows the caller to fall back to the registered text-style
  fallback (typically Standard). }
function DWGTextStyleHandleValue(const PText: PDwg_Entity_TEXT;
  out Value: QWord): Boolean;
function DWGMTextStyleHandleValue(const PMText: PDwg_Entity_MTEXT;
  out Value: QWord): Boolean;

implementation

function DWGObjectHandleValue(const Obj: Dwg_Object): QWord;
begin
  Result := Obj.handle.value;
end;

function DWGRefHandleValue(Ref: BITCODE_H; out Value: QWord): Boolean;
begin
  Value := 0;
  if Ref = nil then
    Exit(False);
  if Ref^.absolute_ref <> 0 then
  begin
    Value := Ref^.absolute_ref;
    Exit(True);
  end;
  if Ref^.handleref.value <> 0 then
  begin
    Value := Ref^.handleref.value;
    Exit(True);
  end;
  Result := False;
end;

function DWGObjectOwnerHandleValue(const Obj: Dwg_Object;
  out Value: QWord): Boolean;
begin
  Value := 0;
  case Obj.supertype of
    DWG_SUPERTYPE_ENTITY:
      if Obj.tio.entity <> nil then
        Exit(DWGRefHandleValue(Obj.tio.entity^.ownerhandle, Value));
    DWG_SUPERTYPE_OBJECT:
      if Obj.tio.&object <> nil then
        Exit(DWGRefHandleValue(Obj.tio.&object^.ownerhandle, Value));
  end;
  Result := False;
end;

function DWGEntityLayerHandleValue(const Obj: Dwg_Object;
  out Value: QWord): Boolean;
begin
  Value := 0;
  if (Obj.supertype <> DWG_SUPERTYPE_ENTITY) or (Obj.tio.entity = nil) then
    Exit(False);
  Result := DWGRefHandleValue(Obj.tio.entity^.layer, Value);
end;

function DWGEntityLineTypeHandleValue(const Obj: Dwg_Object;
  out Value: QWord): Boolean;
begin
  Value := 0;
  if (Obj.supertype <> DWG_SUPERTYPE_ENTITY) or (Obj.tio.entity = nil) then
    Exit(False);
  Result := DWGRefHandleValue(Obj.tio.entity^.ltype, Value);
end;

function DWGLayerLineTypeHandleValue(const PLayer: PDwg_Object_LAYER;
  out Value: QWord): Boolean;
begin
  Value := 0;
  if PLayer = nil then
    Exit(False);
  Result := DWGRefHandleValue(PLayer^.ltype, Value);
end;

function DWGTextStyleHandleValue(const PText: PDwg_Entity_TEXT;
  out Value: QWord): Boolean;
begin
  Value := 0;
  if PText = nil then
    Exit(False);
  Result := DWGRefHandleValue(PText^.style, Value);
end;

function DWGMTextStyleHandleValue(const PMText: PDwg_Entity_MTEXT;
  out Value: QWord): Boolean;
begin
  Value := 0;
  if PMText = nil then
    Exit(False);
  Result := DWGRefHandleValue(PMText^.style, Value);
end;

end.
