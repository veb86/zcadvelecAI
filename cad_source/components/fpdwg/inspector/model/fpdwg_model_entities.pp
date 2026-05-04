unit fpdwg_model_entities;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Math,
  fpdwg_types,
  fpdwg_model_base;

type
  TDWGPoint2D = record
    X: Double;
    Y: Double;
  end;

  TDWGPolylineWidth = record
    StartWidth: Double;
    EndWidth: Double;
  end;

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

  TDWGLWPolyline = class(TDWGEntity)
  public
    Flag: Integer;
    ConstWidth: Double;
    Elevation: Double;
    Thickness: Double;
    Extrusion: TDWGPoint3D;
    Points: array of TDWGPoint2D;
    Bulges: array of Double;
    VertexIds: array of Integer;
    Widths: array of TDWGPolylineWidth;
    constructor Create; override;
    destructor Destroy; override;
    function IsClosed: Boolean;
    function PointCount: Integer;
  end;

  TDWGText = class(TDWGEntity)
  public
    TextValue: string;
    InsertPoint: TDWGPoint3D;
    AlignmentPoint: TDWGPoint3D;
    Extrusion: TDWGPoint3D;
    Thickness: Double;
    ObliqueAngle: Double;
    Rotation: Double;
    Height: Double;
    WidthFactor: Double;
    Generation: Integer;
    HorizontalAlignment: Integer;
    VerticalAlignment: Integer;
    StyleHandle: TDWGHandleRef;
    Style: TDWGObject;
    constructor Create; override;
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

constructor TDWGLWPolyline.Create;
begin
  inherited Create;
  DomainType := dotLWPolyline;
  Flag := 0;
  ConstWidth := 0.0;
  Elevation := 0.0;
  Thickness := 0.0;
  Extrusion.X := 0.0;
  Extrusion.Y := 0.0;
  Extrusion.Z := 1.0;
  SetLength(Points, 0);
  SetLength(Bulges, 0);
  SetLength(VertexIds, 0);
  SetLength(Widths, 0);
end;

destructor TDWGLWPolyline.Destroy;
begin
  SetLength(Points, 0);
  SetLength(Bulges, 0);
  SetLength(VertexIds, 0);
  SetLength(Widths, 0);
  inherited Destroy;
end;

function TDWGLWPolyline.IsClosed: Boolean;
begin
  Result := (Flag and Integer(FLAG_LWPOLYLINE_CLOSED)) <> 0;
end;

function TDWGLWPolyline.PointCount: Integer;
begin
  Result := Length(Points);
end;

constructor TDWGText.Create;
begin
  inherited Create;
  DomainType := dotText;
  TextValue := '';
  InsertPoint.X := 0.0;
  InsertPoint.Y := 0.0;
  InsertPoint.Z := 0.0;
  AlignmentPoint.X := 0.0;
  AlignmentPoint.Y := 0.0;
  AlignmentPoint.Z := 0.0;
  Extrusion.X := 0.0;
  Extrusion.Y := 0.0;
  Extrusion.Z := 1.0;
  Thickness := 0.0;
  ObliqueAngle := 0.0;
  Rotation := 0.0;
  Height := 0.0;
  WidthFactor := 1.0;
  Generation := 0;
  HorizontalAlignment := 0;
  VerticalAlignment := 0;
  StyleHandle := TDWGHandleRef.Null;
  Style := nil;
end;

end.
