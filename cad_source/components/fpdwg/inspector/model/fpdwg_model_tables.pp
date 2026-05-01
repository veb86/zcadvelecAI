unit fpdwg_model_tables;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  fpdwg_types,
  fpdwg_model_base;

type
  TDWGLinetype = class(TDWGTableRecord)
  public
    LinetypeName: string;
    Description: string;
    PatternLength: Double;
    constructor Create; override;
  end;

  TDWGLayer = class(TDWGTableRecord)
  public
    LayerName: string;
    ColorIndex: Integer;
    LineWeight: Integer;
    Off: Boolean;
    Locked: Boolean;
    Plot: Boolean;
    LinetypeHandle: TDWGHandleRef;
    Linetype: TDWGLinetype;
    constructor Create; override;
  end;

implementation

constructor TDWGLinetype.Create;
begin
  inherited Create;
  DomainType := dotLinetype;
  LinetypeName := '';
  Description := '';
  PatternLength := 0.0;
end;

constructor TDWGLayer.Create;
begin
  inherited Create;
  DomainType := dotLayer;
  LayerName := '';
  ColorIndex := 0;
  LineWeight := 0;
  Off := False;
  Locked := False;
  Plot := True;
  LinetypeHandle := TDWGHandleRef.Null;
  Linetype := nil;
end;

end.
