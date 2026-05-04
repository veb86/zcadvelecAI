unit fpdwg_map_arc;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  dwg,
  fpdwg_types,
  fpdwg_factory,
  fpdwg_model_base,
  fpdwg_model_entities;

type
  TDWGArcMapper = class(TInterfacedObject, IDWGObjectMapper)
  public
    function CreateObject(const Raw: Dwg_Object;
      const Ctx: TDWGBuilderContext): TDWGObject;
    procedure FillObject(Obj: TDWGObject; const Raw: Dwg_Object;
      const Ctx: TDWGBuilderContext);
  end;

implementation

function PointFromRaw(const P: BITCODE_3BD): TDWGPoint3D;
begin
  Result.X := P.x;
  Result.Y := P.y;
  Result.Z := P.z;
end;

function ExtrusionFromRaw(const P: BITCODE_BE): TDWGPoint3D;
begin
  Result.X := P.x;
  Result.Y := P.y;
  Result.Z := P.z;
end;

function TDWGArcMapper.CreateObject(const Raw: Dwg_Object;
  const Ctx: TDWGBuilderContext): TDWGObject;
begin
  Result := TDWGArc.Create;
end;

procedure TDWGArcMapper.FillObject(Obj: TDWGObject; const Raw: Dwg_Object;
  const Ctx: TDWGBuilderContext);
var
  Arc: TDWGArc;
  RawArc: ^Dwg_Entity_ARC;
begin
  if not (Obj is TDWGArc) then
    Exit;

  Arc := TDWGArc(Obj);
  FillCommonEntityFields(Arc, Raw, Ctx);

  if (Raw.tio.entity = nil) or (Raw.tio.entity^.tio.ARC = nil) then
  begin
    Arc.Status := osFailed;
    if Ctx.Logger <> nil then
      Ctx.Logger.Error(1101, 'ARC object has no LibreDWG ARC data',
        Arc.Handle);
    Exit;
  end;

  RawArc := Raw.tio.entity^.tio.ARC;
  Arc.Center := PointFromRaw(RawArc^.center);
  Arc.Radius := RawArc^.radius;
  Arc.Thickness := RawArc^.thickness;
  Arc.Extrusion := ExtrusionFromRaw(RawArc^.extrusion);
  Arc.StartAngle := RawArc^.start_angle;
  Arc.EndAngle := RawArc^.end_angle;
end;

end.
