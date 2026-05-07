{*************************************************************************** }
{  fpdwg - DWG INSERT / MINSERT entity mapper (Stage 6)                     }
{                                                                            }
{        Copyright (C) 2026 Andrey Zubarev <zamtmn@yandex.ru>                }
{                                                                            }
{  This library is free software, licensed under the terms of the GNU        }
{  General Public License as published by the Free Software Foundation,      }
{  either version 3 of the License, or (at your option) any later version.   }
{*************************************************************************** }

unit uzedwgentinsert;

{$Include zengineconfig.inc}
{$Mode delphi}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  uzbLogIntf,
  SysUtils,
  dwg, dwgproc, uzedwghandle, uzedwgtext,
  uzedrawingsimple,
  uzeentblockinsert, uzeentity,
  uzeentsubordinated,
  uzeblockdef,
  uzedwgloadcontext,
  uzedwgentityregistry,
  uzeffmanager,
  uzedwgtypes,
  uzedwgimport;

implementation

type
  PDwg_Entity_INSERT = ^Dwg_Entity_INSERT;
  PDwg_Entity_MINSERT = ^Dwg_Entity_MINSERT;

procedure ApplyInsertTransform(PObj: PGDBObjBlockInsert;
  const InsertPoint, Scale: BITCODE_3DPOINT; Rotation: Double);
begin
  PObj^.Local.p_insert.x := InsertPoint.x;
  PObj^.Local.p_insert.y := InsertPoint.y;
  PObj^.Local.p_insert.z := InsertPoint.z;

  PObj^.scale.x := Scale.x;
  PObj^.scale.y := Scale.y;
  PObj^.scale.z := Scale.z;
  if PObj^.scale.x = 0 then
    PObj^.scale.x := 1;
  if PObj^.scale.y = 0 then
    PObj^.scale.y := 1;
  if PObj^.scale.z = 0 then
    PObj^.scale.z := 1;

  PObj^.rotate := Rotation;
end;

function FindBlockDefByName(var ZContext: TZDrawingContext;
  const BlockName: string): PGDBObjBlockdef;
begin
  Result := nil;
  if BlockName <> '' then
    Result := ZContext.PDrawing^.BlockDefArray.getblockdef(BlockName);
end;

procedure QueueBlockDefRef(PObj: PGDBObjBlockInsert;
  var ZContext: TZDrawingContext; var DWGObject: Dwg_Object;
  BlockHeader: BITCODE_H; const InitialName: string);
var
  Ctx: TDWGZCADLoadContext;
  EntityHandle, BlockHandle: QWord;
  FallbackBlock: PGDBObjBlockdef;
begin
  Ctx := GetLoadCtx;
  if Ctx = nil then
    Exit;

  EntityHandle := DWGObjectHandleValue(DWGObject);
  if not DWGRefHandleValue(BlockHeader, BlockHandle) then
    BlockHandle := 0;

  FallbackBlock := FindBlockDefByName(ZContext, InitialName);
  Ctx.QueueRefResolve(PGDBObjEntity(PObj), EntityHandle, BlockHandle,
    dokBlockDef, rsBlockDef, FallbackBlock);
end;

procedure DecodeInsertBlockName(var DWGContext: TDWGCtx;
  PInsert: PDwg_Entity_INSERT; out BlockName: string);
begin
  BlockName := '';
  if PInsert <> nil then
    DWGSafeDecodeText(PInsert^.block_name, DWGContext.DWGVer, BlockName);
end;

procedure AddInsertEntity(var ZContext: TZDrawingContext;
  var DWGContext: TDWGCtx; var DWGObject: Dwg_Object;
  PInsert: PDwg_Entity_INSERT);
var
  PObj: PGDBObjBlockInsert;
  BlockHandle: QWord;
  BlockName: string;
begin
  if PInsert = nil then
    Exit;

  PObj := AllocAndInitBlockInsert(nil);
  ApplyInsertTransform(PObj, PInsert^.ins_pt, PInsert^.scale,
    PInsert^.rotation);
  DecodeInsertBlockName(DWGContext, PInsert, BlockName);
  PObj^.Name := BlockName;
  if not DWGRefHandleValue(PInsert^.block_header, BlockHandle) then
    BlockHandle := 0;

  zDebugLn(['{WH}DWG INSERT handle=', IntToHex(DWGObjectHandleValue(
    DWGObject), 1),
    ' block_ref=', IntToHex(BlockHandle, 1),
    ' name=', BlockName,
    ' attribs=', PInsert^.num_owned]);

  if GetLoadCtx <> nil then begin
    DWGRegisterEntityShell(PGDBObjEntity(PObj), DWGObject, False, 0,
      dokBlockInsert);
    QueueBlockDefRef(PObj, ZContext, DWGObject, PInsert^.block_header,
      BlockName);
  end else
    ZContext.PDrawing^.pObjRoot^.AddMi(PGDBObjSubordinated(PObj));
end;

procedure AddMInsertEntity(var ZContext: TZDrawingContext;
  var DWGContext: TDWGCtx; var DWGObject: Dwg_Object;
  PMInsert: PDwg_Entity_MINSERT);
var
  PObj: PGDBObjBlockInsert;
begin
  if PMInsert = nil then
    Exit;

  PObj := AllocAndInitBlockInsert(nil);
  ApplyInsertTransform(PObj, PMInsert^.ins_pt, PMInsert^.scale,
    PMInsert^.rotation);

  if (PMInsert^.num_cols > 1) or (PMInsert^.num_rows > 1) then
    zDebugLn(['{WHM}DWG MINSERT handle=', IntToHex(DWGObjectHandleValue(
      DWGObject), 1),
      ' array ', PMInsert^.num_cols, 'x', PMInsert^.num_rows,
      ' spacing=(', FloatToStr(PMInsert^.col_spacing), ',',
      FloatToStr(PMInsert^.row_spacing),
      ') imported as base INSERT']);

  if GetLoadCtx <> nil then begin
    DWGRegisterEntityShell(PGDBObjEntity(PObj), DWGObject, False, 0,
      dokBlockInsert);
    QueueBlockDefRef(PObj, ZContext, DWGObject, PMInsert^.block_header, '');
  end else
    ZContext.PDrawing^.pObjRoot^.AddMi(PGDBObjSubordinated(PObj));
end;

initialization
  RegisterDWGEntityHandler(DWG_TYPE_INSERT, @AddInsertEntity);
  RegisterDWGEntityHandler(DWG_TYPE_MINSERT, @AddMInsertEntity);
end.
