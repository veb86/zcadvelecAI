unit fpdwg_map_text;

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
  TDWGTextMapper = class(TInterfacedObject, IDWGObjectMapper)
  public
    function CreateObject(const Raw: Dwg_Object;
      const Ctx: TDWGBuilderContext): TDWGObject;
    procedure FillObject(Obj: TDWGObject; const Raw: Dwg_Object;
      const Ctx: TDWGBuilderContext);
  end;

implementation

uses
  fpdwg_libredwg_utils;

function Point2DFromRaw(const P: BITCODE_2DPOINT;
  Elevation: Double): TDWGPoint3D;
begin
  Result.X := P.x;
  Result.Y := P.y;
  Result.Z := Elevation;
end;

function ExtrusionFromRaw(const P: BITCODE_BE): TDWGPoint3D;
begin
  Result.X := P.x;
  Result.Y := P.y;
  Result.Z := P.z;
end;

function TDWGTextMapper.CreateObject(const Raw: Dwg_Object;
  const Ctx: TDWGBuilderContext): TDWGObject;
begin
  Result := TDWGText.Create;
end;

procedure TDWGTextMapper.FillObject(Obj: TDWGObject; const Raw: Dwg_Object;
  const Ctx: TDWGBuilderContext);
var
  Text: TDWGText;
  RawText: ^Dwg_Entity_TEXT;
begin
  if not (Obj is TDWGText) then
    Exit;

  Text := TDWGText(Obj);
  FillCommonEntityFields(Text, Raw, Ctx);

  if (Raw.tio.entity = nil) or (Raw.tio.entity^.tio.TEXT = nil) then
  begin
    Text.Status := osFailed;
    if Ctx.Logger <> nil then
      Ctx.Logger.Error(1101, 'TEXT object has no LibreDWG TEXT data',
        Text.Handle);
    Exit;
  end;

  RawText := Raw.tio.entity^.tio.TEXT;
  Text.TextValue := SafeDecodeLibreDWGText(RawText^.text_value, Ctx.Codepage,
    Ctx.Logger);
  Text.InsertPoint := Point2DFromRaw(RawText^.ins_pt, RawText^.elevation);
  Text.AlignmentPoint := Point2DFromRaw(RawText^.alignment_pt,
    RawText^.elevation);
  Text.Extrusion := ExtrusionFromRaw(RawText^.extrusion);
  Text.Thickness := RawText^.thickness;
  Text.ObliqueAngle := RawText^.oblique_angle;
  Text.Rotation := RawText^.rotation;
  Text.Height := RawText^.height;
  Text.WidthFactor := RawText^.width_factor;
  Text.Generation := RawText^.generation;
  Text.HorizontalAlignment := RawText^.horiz_alignment;
  Text.VerticalAlignment := RawText^.vert_alignment;
  Text.StyleHandle := HandleRefFromBitCode(RawText^.style);
end;

end.
