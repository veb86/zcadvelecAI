unit fpdwg_map_line;

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
  TDWGLineMapper = class(TInterfacedObject, IDWGObjectMapper)
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

function TDWGLineMapper.CreateObject(const Raw: Dwg_Object;
  const Ctx: TDWGBuilderContext): TDWGObject;
begin
  Result := TDWGLine.Create;
end;

procedure TDWGLineMapper.FillObject(Obj: TDWGObject; const Raw: Dwg_Object;
  const Ctx: TDWGBuilderContext);
var
  Line: TDWGLine;
  RawLine: PDwg_Entity_LINE;
begin
  if not (Obj is TDWGLine) then
    Exit;

  Line := TDWGLine(Obj);
  FillCommonEntityFields(Line, Raw, Ctx);

  if (Raw.tio.entity = nil) or (Raw.tio.entity^.tio.LINE = nil) then
  begin
    Line.Status := osFailed;
    if Ctx.Logger <> nil then
      Ctx.Logger.Error(1101, 'LINE object has no LibreDWG LINE data',
        Line.Handle);
    Exit;
  end;

  RawLine := Raw.tio.entity^.tio.LINE;
  Line.StartPoint := PointFromRaw(RawLine^.start);
  Line.EndPoint := PointFromRaw(RawLine^.end_);
end;

end.
