unit fpdwg_model_unknown;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  SysUtils,
  dwg,
  fpdwg_types,
  fpdwg_model_base;

type
  TDWGUnknownObject = class(TDWGObject)
  public
    Supertype: DWG_OBJECT_SUPERTYPE;
    Size: UInt32;
    BitSize: UInt32;
    UnknownBitsSize: UInt32;
    UnknownRestSize: UInt32;
    Reason: string;
    RawBytes: TBytes;
    constructor Create; override;
  end;

  TDWGDuplicateHandleObject = class(TDWGObject)
  public
    OriginalHandle: TDWGHandle;
    ConflictWith: TDWGObject;
    constructor CreateDuplicate(DuplicateObject: TDWGObject;
      AConflictWith: TDWGObject);
  end;

implementation

constructor TDWGUnknownObject.Create;
begin
  inherited Create;
  DomainType := dotUnknown;
  Supertype := DWG_SUPERTYPE_OBJECT;
  Size := 0;
  BitSize := 0;
  UnknownBitsSize := 0;
  UnknownRestSize := 0;
  Reason := '';
  SetLength(RawBytes, 0);
end;

constructor TDWGDuplicateHandleObject.CreateDuplicate(
  DuplicateObject: TDWGObject; AConflictWith: TDWGObject);
begin
  inherited Create;
  if DuplicateObject <> nil then
  begin
    Handle := DuplicateObject.Handle;
    OwnerHandle := DuplicateObject.OwnerHandle;
    RawObjectType := DuplicateObject.RawObjectType;
    Version := DuplicateObject.Version;
    RawIndex := DuplicateObject.RawIndex;
    Name := DuplicateObject.Name;
    DxfName := DuplicateObject.DxfName;
  end;
  OriginalHandle := Handle;
  ConflictWith := AConflictWith;
  DomainType := dotUnknown;
  Status := osBroken;
end;

end.
