unit fpdwg_filter;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  dwg,
  fpdwg_types;

type
  TDWGFilterDecision = (
    fdMaterialize,
    fdStub,
    fdSkip
  );

  TFilterStrategy = class
  public
    function Decide(const Raw: Dwg_Object): TDWGFilterDecision; virtual; abstract;
  end;

  TFilterAll = class(TFilterStrategy)
  public
    function Decide(const Raw: Dwg_Object): TDWGFilterDecision; override;
  end;

  TFilterByDomainType = class(TFilterStrategy)
  private
    FDomainTypes: array of TDWGDomainObjectType;
    function AllowsDomainType(DomainType: TDWGDomainObjectType): Boolean;
  public
    constructor Create(const ADomainTypes: array of TDWGDomainObjectType);
    function Decide(const Raw: Dwg_Object): TDWGFilterDecision; override;
  end;

function DWGDomainTypeFromRawType(RawType: DWG_OBJECT_TYPE
  ): TDWGDomainObjectType;

implementation

function DWGDomainTypeFromRawType(RawType: DWG_OBJECT_TYPE
  ): TDWGDomainObjectType;
begin
  case RawType of
    DWG_TYPE_LAYER:
      Result := dotLayer;
    DWG_TYPE_LTYPE:
      Result := dotLinetype;
    DWG_TYPE_STYLE:
      Result := dotStyle;
    DWG_TYPE_BLOCK_HEADER:
      Result := dotBlockHeader;
    DWG_TYPE_BLOCK:
      Result := dotBlock;
    DWG_TYPE_LINE:
      Result := dotLine;
    DWG_TYPE_CIRCLE:
      Result := dotCircle;
    DWG_TYPE_LWPOLYLINE:
      Result := dotLWPolyline;
    DWG_TYPE_TEXT, DWG_TYPE_MTEXT:
      Result := dotText;
  else
    Result := dotUnknown;
  end;
end;

function TFilterAll.Decide(const Raw: Dwg_Object): TDWGFilterDecision;
begin
  Result := fdMaterialize;
end;

constructor TFilterByDomainType.Create(
  const ADomainTypes: array of TDWGDomainObjectType);
var
  I: Integer;
begin
  inherited Create;
  SetLength(FDomainTypes, Length(ADomainTypes));
  for I := Low(ADomainTypes) to High(ADomainTypes) do
    FDomainTypes[I] := ADomainTypes[I];
end;

function TFilterByDomainType.AllowsDomainType(
  DomainType: TDWGDomainObjectType): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(FDomainTypes) do
    if FDomainTypes[I] = DomainType then
      Exit(True);
  Result := False;
end;

function TFilterByDomainType.Decide(const Raw: Dwg_Object
  ): TDWGFilterDecision;
begin
  if AllowsDomainType(DWGDomainTypeFromRawType(Raw.fixedtype)) then
    Result := fdMaterialize
  else
    Result := fdStub;
end;

end.
