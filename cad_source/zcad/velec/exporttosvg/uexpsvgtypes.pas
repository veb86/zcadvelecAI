// uexpsvgtypes.pas
unit uexpsvgtypes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, uzcinterface, uzgldrawcontext, uzegeometrytypes;

type
  // Базовые типы для SVG
  TSVGPoint = record
    X, Y: Double;
  end;

  TSVGRect = record
    MinX, MinY, MaxX, MaxY: Double;
  end;

  // Простой класс для накопления геометрии перед записью в SVG
  TSVGGeometryBuilder = class
  private
    FPoints: array of TSVGPoint;
    FBounds: TSVGRect;
    FHasData: Boolean;
    procedure UpdateBounds(const Pt: TSVGPoint);
  public
    constructor Create;
    procedure AddPoint(const X, Y: Double);
    function GetBounds: TSVGRect;
    function HasGeometry: Boolean;
    procedure Clear;
  end;

implementation

constructor TSVGGeometryBuilder.Create;
begin
  inherited;
  FHasData := False;
  Clear;
end;

procedure TSVGGeometryBuilder.Clear;
begin
  SetLength(FPoints, 0);
  FHasData := False;
  FBounds.MinX := 0; FBounds.MinY := 0;
  FBounds.MaxX := 0; FBounds.MaxY := 0;
end;

procedure TSVGGeometryBuilder.UpdateBounds(const Pt: TSVGPoint);
begin
  if not FHasData then
  begin
    FBounds.MinX := Pt.X;
    FBounds.MaxX := Pt.X;
    FBounds.MinY := Pt.Y;
    FBounds.MaxY := Pt.Y;
    FHasData := True;
  end
  else
  begin
    if Pt.X < FBounds.MinX then FBounds.MinX := Pt.X;
    if Pt.X > FBounds.MaxX then FBounds.MaxX := Pt.X;
    if Pt.Y < FBounds.MinY then FBounds.MinY := Pt.Y;
    if Pt.Y > FBounds.MaxY then FBounds.MaxY := Pt.Y;
  end;
end;

procedure TSVGGeometryBuilder.AddPoint(const X, Y: Double);
var
  Pt: TSVGPoint;
begin
  Pt.X := X;
  Pt.Y := Y;
  UpdateBounds(Pt);
  SetLength(FPoints, Length(FPoints) + 1);
  FPoints[High(FPoints)] := Pt;
end;

function TSVGGeometryBuilder.GetBounds: TSVGRect;
begin
  Result := FBounds;
end;

function TSVGGeometryBuilder.HasGeometry: Boolean;
begin
  Result := FHasData;
end;

end.