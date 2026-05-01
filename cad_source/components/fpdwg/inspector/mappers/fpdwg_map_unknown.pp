unit fpdwg_map_unknown;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  SysUtils,
  dwg,
  fpdwg_types,
  fpdwg_factory,
  fpdwg_model_base,
  fpdwg_model_unknown;

type
  TDWGUnknownMapper = class(TInterfacedObject, IDWGObjectMapper)
  public
    function CreateObject(const Raw: Dwg_Object;
      const Ctx: TDWGBuilderContext): TDWGObject;
    procedure FillObject(Obj: TDWGObject; const Raw: Dwg_Object;
      const Ctx: TDWGBuilderContext);
  end;

implementation

uses
  fpdwg_libredwg_utils;

procedure AppendRawBytes(var Dest: TBytes; Source: Pointer; Count: SizeUInt);
var
  OldLen: SizeInt;
begin
  if (Source = nil) or (Count = 0) then
    Exit;

  OldLen := Length(Dest);
  SetLength(Dest, OldLen + SizeInt(Count));
  Move(Source^, Dest[OldLen], Count);
end;

function UnknownBitsByteCount(BitCount: UInt32): UInt32;
begin
  Result := (BitCount + 7) div 8;
end;

function TDWGUnknownMapper.CreateObject(const Raw: Dwg_Object;
  const Ctx: TDWGBuilderContext): TDWGObject;
begin
  Result := TDWGUnknownObject.Create;
end;

procedure TDWGUnknownMapper.FillObject(Obj: TDWGObject;
  const Raw: Dwg_Object; const Ctx: TDWGBuilderContext);
var
  Unknown: TDWGUnknownObject;
begin
  if not (Obj is TDWGUnknownObject) then
    Exit;

  Unknown := TDWGUnknownObject(Obj);
  Unknown.Supertype := Raw.supertype;
  Unknown.Size := Raw.size;
  Unknown.BitSize := Raw.bitsize;
  Unknown.UnknownBitsSize := Raw.num_unknown_bits;
  Unknown.UnknownRestSize := Raw.num_unknown_rest;
  Unknown.Reason := 'mapper not registered for ' +
    DWGObjectTypeName(Raw.fixedtype);

  if Ctx.DumpUnknown then
  begin
    SetLength(Unknown.RawBytes, 0);
    AppendRawBytes(Unknown.RawBytes, Raw.unknown_bits,
      UnknownBitsByteCount(Raw.num_unknown_bits));
    AppendRawBytes(Unknown.RawBytes, Raw.unknown_rest, Raw.num_unknown_rest);
  end;
end;

end.
