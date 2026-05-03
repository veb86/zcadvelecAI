unit fpdwg_map_layer_control;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  dwg,
  fpdwg_factory,
  fpdwg_model_base,
  fpdwg_model_tables;

type
  TDWGLayerControlMapper = class(TInterfacedObject, IDWGObjectMapper)
  public
    function CreateObject(const Raw: Dwg_Object;
      const Ctx: TDWGBuilderContext): TDWGObject;
    procedure FillObject(Obj: TDWGObject; const Raw: Dwg_Object;
      const Ctx: TDWGBuilderContext);
  end;

implementation

uses
  fpdwg_libredwg_utils,
  fpdwg_types;

function TDWGLayerControlMapper.CreateObject(const Raw: Dwg_Object;
  const Ctx: TDWGBuilderContext): TDWGObject;
begin
  Result := TDWGTableControl.Create;
end;

procedure TDWGLayerControlMapper.FillObject(Obj: TDWGObject;
  const Raw: Dwg_Object; const Ctx: TDWGBuilderContext);
var
  LayerControl: TDWGTableControl;
  RawLayerControl: ^Dwg_Object_LAYER_CONTROL;
  I: Integer;
begin
  if not (Obj is TDWGTableControl) then
    Exit;

  LayerControl := TDWGTableControl(Obj);
  LayerControl.TableKind := 'LAYER';
  LayerControl.Name := 'LAYER table control';
  LayerControl.DxfName := 'LAYER_CONTROL';

  if (Raw.tio.&object = nil) or (Raw.tio.&object^.tio.LAYER_CONTROL = nil) then
  begin
    LayerControl.Status := osFailed;
    if Ctx.Logger <> nil then
      Ctx.Logger.Error(1101,
        'LAYER_CONTROL object has no LibreDWG LAYER_CONTROL data',
        LayerControl.Handle);
    Exit;
  end;

  RawLayerControl := Raw.tio.&object^.tio.LAYER_CONTROL;
  SetLength(LayerControl.EntryHandles, RawLayerControl^.num_entries);
  for I := 0 to High(LayerControl.EntryHandles) do
    LayerControl.EntryHandles[I] :=
      HandleRefFromBitCode(RawLayerControl^.entries[I]);
end;

end.
