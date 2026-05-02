unit fpdwg_model_entities;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Math,
  fpdwg_types,
  fpdwg_model_base;

type
  TDWGLine = class(TDWGEntity)
  public
    StartPoint: TDWGPoint3D;
    EndPoint: TDWGPoint3D;
    constructor Create; override;
    function Length3D: Double;
    function LengthXY: Double;
  end;

  TDWGCircle = class(TDWGEntity)
  public
    Center: TDWGPoint3D;
    Radius: Double;
    Thickness: Double;
    Extrusion: TDWGPoint3D;
    constructor Create; override;
    function Diameter: Double;
  end;

implementation

constructor TDWGLine.Create;
begin
  inherited Create;
  DomainType := dotLine;
  StartPoint.X := 0.0;
  StartPoint.Y := 0.0;
  StartPoint.Z := 0.0;
  EndPoint.X := 0.0;
  EndPoint.Y := 0.0;
  EndPoint.Z := 0.0;
end;

function TDWGLine.Length3D: Double;
begin
  Result := Sqrt(Sqr(EndPoint.X - StartPoint.X) +
                 Sqr(EndPoint.Y - StartPoint.Y) +
                 Sqr(EndPoint.Z - StartPoint.Z));
end;

function TDWGLine.LengthXY: Double;
begin
  Result := Sqrt(Sqr(EndPoint.X - StartPoint.X) +
                 Sqr(EndPoint.Y - StartPoint.Y));
end;

constructor TDWGCircle.Create;
begin
  inherited Create;
  DomainType := dotCircle;
  Center.X := 0.0;
  Center.Y := 0.0;
  Center.Z := 0.0;
  Radius := 0.0;
  Thickness := 0.0;
  Extrusion.X := 0.0;
  Extrusion.Y := 0.0;
  Extrusion.Z := 1.0;
end;

function TDWGCircle.Diameter: Double;
begin
  Result := Radius * 2.0;
end;

end.
