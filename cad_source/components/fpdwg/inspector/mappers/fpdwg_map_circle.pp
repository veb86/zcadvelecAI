unit fpdwg_map_circle;

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
  TDWGCircleMapper = class(TInterfacedObject, IDWGObjectMapper)
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

function TDWGCircleMapper.CreateObject(const Raw: Dwg_Object;
  const Ctx: TDWGBuilderContext): TDWGObject;
begin
  Result := TDWGCircle.Create;
end;

procedure TDWGCircleMapper.FillObject(Obj: TDWGObject; const Raw: Dwg_Object;
  const Ctx: TDWGBuilderContext);
var
  Circle: TDWGCircle;
  RawCircle: ^Dwg_Entity_CIRCLE;
begin
  if not (Obj is TDWGCircle) then
    Exit;

  Circle := TDWGCircle(Obj);
  FillCommonEntityFields(Circle, Raw, Ctx);

  if (Raw.tio.entity = nil) or (Raw.tio.entity^.tio.CIRCLE = nil) then
  begin
    Circle.Status := osFailed;
    if Ctx.Logger <> nil then
      Ctx.Logger.Error(1101, 'CIRCLE object has no LibreDWG CIRCLE data',
        Circle.Handle);
    Exit;
  end;

  RawCircle := Raw.tio.entity^.tio.CIRCLE;
  Circle.Center := PointFromRaw(RawCircle^.center);
  Circle.Radius := RawCircle^.radius;
  Circle.Thickness := RawCircle^.thickness;
  Circle.Extrusion := ExtrusionFromRaw(RawCircle^.extrusion);
end;

end.
