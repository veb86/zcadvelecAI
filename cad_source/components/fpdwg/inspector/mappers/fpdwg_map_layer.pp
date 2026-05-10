unit fpdwg_map_layer;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  dwg,
  fpdwg_types,
  fpdwg_factory,
  fpdwg_model_base,
  fpdwg_model_tables;

type
  TDWGLayerMapper = class(TInterfacedObject, IDWGObjectMapper)
  public
    function CreateObject(const Raw: Dwg_Object;
      const Ctx: TDWGBuilderContext): TDWGObject;
    procedure FillObject(Obj: TDWGObject; const Raw: Dwg_Object;
      const Ctx: TDWGBuilderContext);
  end;

implementation

uses
  fpdwg_libredwg_utils;

const
  DWG_LAYER_FLAG_FROZEN = 1;
  DWG_LAYER_FLAG_OFF = 2;
  DWG_LAYER_FLAG_LOCKED = 8;
  DWG_LAYER_FLAG_PLOT = 16;

function TDWGLayerMapper.CreateObject(const Raw: Dwg_Object;
  const Ctx: TDWGBuilderContext): TDWGObject;
begin
  Result := TDWGLayer.Create;
end;

procedure TDWGLayerMapper.FillObject(Obj: TDWGObject; const Raw: Dwg_Object;
  const Ctx: TDWGBuilderContext);
var
  Layer: TDWGLayer;
  RawLayer: PDwg_Object_LAYER;
begin
  if not (Obj is TDWGLayer) then
    Exit;

  Layer := TDWGLayer(Obj);
  Layer.OwnerHandle := SyntheticHandleRef(DWG_SYNTHETIC_LAYER_TABLE_HANDLE);

  if (Raw.tio.&object = nil) or (Raw.tio.&object^.tio.LAYER = nil) then
  begin
    Layer.Status := osFailed;
    if Ctx.Logger <> nil then
      Ctx.Logger.Error(1101, 'LAYER object has no LibreDWG LAYER data',
        Layer.Handle);
    Exit;
  end;

  RawLayer := Raw.tio.&object^.tio.LAYER;
  Layer.LayerName := SafeDecodeText(RawLayer^.name, Ctx.Codepage, Ctx.Logger);
  Layer.ColorIndex := RawLayer^.color.index;
  Layer.LineWeight := RawLayer^.linewt;
  Layer.Off := (RawLayer^.off <> 0) or
    ((RawLayer^.flag0 and DWG_LAYER_FLAG_OFF) <> 0);
  Layer.Frozen := (RawLayer^.frozen <> 0) or
    ((RawLayer^.flag0 and DWG_LAYER_FLAG_FROZEN) <> 0);
  Layer.Locked := (RawLayer^.locked <> 0) or
    ((RawLayer^.flag0 and DWG_LAYER_FLAG_LOCKED) <> 0);
  Layer.Plot := (RawLayer^.plotflag <> 0) or
    ((RawLayer^.flag0 and DWG_LAYER_FLAG_PLOT) <> 0);
  Layer.LinetypeHandle := HandleRefFromBitCode(RawLayer^.ltype);
end;

end.
