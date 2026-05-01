unit fpdwg_map_linetype;

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
  TDWGLinetypeMapper = class(TInterfacedObject, IDWGObjectMapper)
  public
    function CreateObject(const Raw: Dwg_Object;
      const Ctx: TDWGBuilderContext): TDWGObject;
    procedure FillObject(Obj: TDWGObject; const Raw: Dwg_Object;
      const Ctx: TDWGBuilderContext);
  end;

implementation

uses
  fpdwg_libredwg_utils;

function TDWGLinetypeMapper.CreateObject(const Raw: Dwg_Object;
  const Ctx: TDWGBuilderContext): TDWGObject;
begin
  Result := TDWGLinetype.Create;
end;

procedure TDWGLinetypeMapper.FillObject(Obj: TDWGObject;
  const Raw: Dwg_Object; const Ctx: TDWGBuilderContext);
var
  Linetype: TDWGLinetype;
  RawLType: PDwg_Object_LTYPE;
begin
  if not (Obj is TDWGLinetype) then
    Exit;

  Linetype := TDWGLinetype(Obj);
  Linetype.OwnerHandle := SyntheticHandleRef(DWG_SYNTHETIC_LTYPE_TABLE_HANDLE);

  if (Raw.tio.&object = nil) or (Raw.tio.&object^.tio.LTYPE = nil) then
  begin
    Linetype.Status := osFailed;
    if Ctx.Logger <> nil then
      Ctx.Logger.Error(1101, 'LTYPE object has no LibreDWG LTYPE data',
        Linetype.Handle);
    Exit;
  end;

  RawLType := Raw.tio.&object^.tio.LTYPE;
  Linetype.LinetypeName := SafeDecodeText(RawLType^.name, Ctx.Codepage,
    Ctx.Logger);
  Linetype.Description := SafeDecodeText(RawLType^.description, Ctx.Codepage,
    Ctx.Logger);
  Linetype.PatternLength := RawLType^.pattern_len;
end;

end.
