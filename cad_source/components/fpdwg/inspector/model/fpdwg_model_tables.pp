unit fpdwg_model_tables;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  fpdwg_types,
  fpdwg_model_base;

type
  TDWGTableControl = class(TDWGObject)
  public
    TableKind: string;
    EntryHandles: array of TDWGHandleRef;
    constructor Create; override;
  end;

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
    Frozen: Boolean;
    Locked: Boolean;
    Plot: Boolean;
    LinetypeHandle: TDWGHandleRef;
    Linetype: TDWGLinetype;
    constructor Create; override;
  end;

implementation

constructor TDWGTableControl.Create;
begin
  inherited Create;
  DomainType := dotSyntheticTable;
  TableKind := '';
  SetLength(EntryHandles, 0);
end;

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
  Frozen := False;
  Locked := False;
  Plot := True;
  LinetypeHandle := TDWGHandleRef.Null;
  Linetype := nil;
end;

end.
