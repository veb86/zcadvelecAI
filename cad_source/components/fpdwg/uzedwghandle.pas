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
  DWGRefHandleValue so a present-but-zero ownerhandle is treated as missing.

  Issue #1118 / #1120: LibreDWG marks model/paper-space ownership via the
  BITCODE_BB `entmode` field instead of populating `ownerhandle`:
    entmode=0  -> no owner (top-level)
    entmode=1  -> implicit owner is paper-space block (ownerhandle is null)
    entmode=2  -> implicit owner is model-space block (ownerhandle is null)
    entmode=3  -> explicit ownerhandle (read it)
  When entmode is 1 or 2 we resolve the implicit owner from the parent
  Dwg_Data with three fall-through paths (mirroring libredwg's own
  dwg_model_space_object() / dwg_paper_space_object() helpers):
    A. Dwg^.mspace_block / pspace_block (resolved BLOCK_HEADER pointer)
    B. Dwg^.header_vars.BLOCK_RECORD_MSPACE / BLOCK_RECORD_PSPACE
    C. Dwg^.block_control.model_space / paper_space
  Issue #1120 (testdwg2007.dwg) showed that path A alone is not enough:
  some decode paths leave mspace_block nil while the header_vars and/or
  block_control entries do carry a usable handle reference. Without paths
  B/C the entity falls back to ownerhandle (null for entmode=1/2), which
  drops segments to the arNullOwner fallback root and they never render. }
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
var
  Ent: ^Dwg_Object_Entity;
  Dwg: ^_dwg_struct;
  ImplicitOwner: ^Dwg_Object;
  HRef: BITCODE_H;
begin
  Value := 0;
  case Obj.supertype of
    DWG_SUPERTYPE_ENTITY:
      if Obj.tio.entity <> nil then
      begin
        Ent := Obj.tio.entity;
        // Issue #1118 / #1120: prefer entmode-derived implicit owner over
        // ownerhandle. LibreDWG only fills ownerhandle when entmode=3; for
        // entmode=1/2 the owner is paper/model space and ownerhandle is null.
        // Fall through to ownerhandle when entmode=0 (no owner) or 3.
        //
        // Issue #1120: the original fix only consulted Dwg^.mspace_block /
        // pspace_block. Those pointers stay nil in some decode paths even
        // after a successful libredwg load (testdwg2007.dwg shows entmode=2
        // LINE entities with mspace_block = nil), so we add two more
        // fall-throughs that mirror libredwg's own dwg_model_space_object()
        // helper:
        //   1. Dwg^.header_vars.BLOCK_RECORD_MSPACE / BLOCK_RECORD_PSPACE
        //      (BITCODE_H — the recorded handle reference)
        //   2. Dwg^.block_control.model_space / paper_space
        //      (BITCODE_H — the BLOCK_CONTROL table entry)
        // Either of these gives us the handle without needing the resolved
        // pointer, which is what we ultimately want.
        if (Ent^.entmode = 1) or (Ent^.entmode = 2) then
        begin
          Dwg := Obj.parent;
          if Dwg <> nil then
          begin
            // Path A: resolved block-header pointer (preferred when set).
            if Ent^.entmode = 2 then
              ImplicitOwner := Dwg^.mspace_block
            else
              ImplicitOwner := Dwg^.pspace_block;
            if ImplicitOwner <> nil then
            begin
              Value := ImplicitOwner^.handle.value;
              if Value <> 0 then
                Exit(True);
            end;
            // Path B: header_vars.BLOCK_RECORD_*SPACE handle reference.
            if Ent^.entmode = 2 then
              HRef := Dwg^.header_vars.BLOCK_RECORD_MSPACE
            else
              HRef := Dwg^.header_vars.BLOCK_RECORD_PSPACE;
            if DWGRefHandleValue(HRef, Value) then
              Exit(True);
            // Path C: block_control table entry.
            if Ent^.entmode = 2 then
              HRef := Dwg^.block_control.model_space
            else
              HRef := Dwg^.block_control.paper_space;
            if DWGRefHandleValue(HRef, Value) then
              Exit(True);
          end;
          // entmode signalled an implicit owner but no path resolved it
          // (parent missing or the layout records were not loaded). Fall
          // through to ownerhandle so the legacy path still has a chance.
          Value := 0;
        end;
        Exit(DWGRefHandleValue(Ent^.ownerhandle, Value));
      end;
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
