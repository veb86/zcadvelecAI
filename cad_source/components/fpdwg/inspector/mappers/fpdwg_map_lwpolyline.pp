unit fpdwg_map_lwpolyline;

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
  TDWGLWPolylineMapper = class(TInterfacedObject, IDWGObjectMapper)
  public
    function CreateObject(const Raw: Dwg_Object;
      const Ctx: TDWGBuilderContext): TDWGObject;
    procedure FillObject(Obj: TDWGObject; const Raw: Dwg_Object;
      const Ctx: TDWGBuilderContext);
  end;

implementation

function PointFromRaw(const P: BITCODE_2RD): TDWGPoint2D;
begin
  Result.X := P.x;
  Result.Y := P.y;
end;

function ExtrusionFromRaw(const P: BITCODE_BE): TDWGPoint3D;
begin
  Result.X := P.x;
  Result.Y := P.y;
  Result.Z := P.z;
end;

function TDWGLWPolylineMapper.CreateObject(const Raw: Dwg_Object;
  const Ctx: TDWGBuilderContext): TDWGObject;
begin
  Result := TDWGLWPolyline.Create;
end;

procedure TDWGLWPolylineMapper.FillObject(Obj: TDWGObject;
  const Raw: Dwg_Object; const Ctx: TDWGBuilderContext);
var
  LWPolyline: TDWGLWPolyline;
  RawLWPolyline: ^Dwg_Entity_LWPOLYLINE;
  I: Integer;
begin
  if not (Obj is TDWGLWPolyline) then
    Exit;

  LWPolyline := TDWGLWPolyline(Obj);
  FillCommonEntityFields(LWPolyline, Raw, Ctx);

  if (Raw.tio.entity = nil) or (Raw.tio.entity^.tio.LWPOLYLINE = nil) then
  begin
    LWPolyline.Status := osFailed;
    if Ctx.Logger <> nil then
      Ctx.Logger.Error(1101, 'LWPOLYLINE object has no LibreDWG LWPOLYLINE data',
        LWPolyline.Handle);
    Exit;
  end;

  RawLWPolyline := Raw.tio.entity^.tio.LWPOLYLINE;
  LWPolyline.Flag := RawLWPolyline^.flag;
  LWPolyline.ConstWidth := RawLWPolyline^.const_width;
  LWPolyline.Elevation := RawLWPolyline^.elevation;
  LWPolyline.Thickness := RawLWPolyline^.thickness;
  LWPolyline.Extrusion := ExtrusionFromRaw(RawLWPolyline^.extrusion);

  SetLength(LWPolyline.Points, RawLWPolyline^.num_points);
  if RawLWPolyline^.points <> nil then
    for I := 0 to High(LWPolyline.Points) do
      LWPolyline.Points[I] := PointFromRaw(RawLWPolyline^.points[I]);

  SetLength(LWPolyline.Bulges, RawLWPolyline^.num_bulges);
  if RawLWPolyline^.bulges <> nil then
    for I := 0 to High(LWPolyline.Bulges) do
      LWPolyline.Bulges[I] := RawLWPolyline^.bulges[I];

  SetLength(LWPolyline.VertexIds, RawLWPolyline^.num_vertexids);
  if RawLWPolyline^.vertexids <> nil then
    for I := 0 to High(LWPolyline.VertexIds) do
      LWPolyline.VertexIds[I] := RawLWPolyline^.vertexids[I];

  SetLength(LWPolyline.Widths, RawLWPolyline^.num_widths);
  if RawLWPolyline^.widths <> nil then
    for I := 0 to High(LWPolyline.Widths) do
    begin
      LWPolyline.Widths[I].StartWidth := RawLWPolyline^.widths[I].start;
      LWPolyline.Widths[I].EndWidth := RawLWPolyline^.widths[I].end_;
    end;
end;

end.
