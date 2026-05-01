unit fpdwg_map_block;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  dwg,
  fpdwg_types,
  fpdwg_factory,
  fpdwg_model_base,
  fpdwg_model_blocks;

type
  TDWGBlockHeaderMapper = class(TInterfacedObject, IDWGObjectMapper)
  public
    function CreateObject(const Raw: Dwg_Object;
      const Ctx: TDWGBuilderContext): TDWGObject;
    procedure FillObject(Obj: TDWGObject; const Raw: Dwg_Object;
      const Ctx: TDWGBuilderContext);
  end;

implementation

uses
  fpdwg_libredwg_utils;

function PointFromRaw(const P: BITCODE_3DPOINT): TDWGPoint3D;
begin
  Result.X := P.x;
  Result.Y := P.y;
  Result.Z := P.z;
end;

function TDWGBlockHeaderMapper.CreateObject(const Raw: Dwg_Object;
  const Ctx: TDWGBuilderContext): TDWGObject;
begin
  Result := TDWGBlockHeader.Create;
end;

procedure TDWGBlockHeaderMapper.FillObject(Obj: TDWGObject;
  const Raw: Dwg_Object; const Ctx: TDWGBuilderContext);
var
  BlockHeader: TDWGBlockHeader;
  RawBlock: PDwg_Object_BLOCK_HEADER;
begin
  if not (Obj is TDWGBlockHeader) then
    Exit;

  BlockHeader := TDWGBlockHeader(Obj);
  BlockHeader.OwnerHandle :=
    SyntheticHandleRef(DWG_SYNTHETIC_BLOCK_RECORD_TABLE_HANDLE);

  if (Raw.tio.&object = nil) or
     (Raw.tio.&object^.tio.BLOCK_HEADER = nil) then
  begin
    BlockHeader.Status := osFailed;
    if Ctx.Logger <> nil then
      Ctx.Logger.Error(1101,
        'BLOCK_HEADER object has no LibreDWG BLOCK_HEADER data',
        BlockHeader.Handle);
    Exit;
  end;

  RawBlock := Raw.tio.&object^.tio.BLOCK_HEADER;
  BlockHeader.BlockName := SafeDecodeText(RawBlock^.name, Ctx.Codepage,
    Ctx.Logger);
  BlockHeader.BasePoint := PointFromRaw(RawBlock^.base_pt);
  BlockHeader.BlockEntityHandle := HandleRefFromBitCode(RawBlock^.block_entity);
  BlockHeader.FirstEntityHandle := HandleRefFromBitCode(RawBlock^.first_entity);
  BlockHeader.LastEntityHandle := HandleRefFromBitCode(RawBlock^.last_entity);
  BlockHeader.EndBlockEntityHandle :=
    HandleRefFromBitCode(RawBlock^.endblk_entity);
  BlockHeader.LayoutHandle := HandleRefFromBitCode(RawBlock^.layout);
end;

end.
