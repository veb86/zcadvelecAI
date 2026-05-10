unit fpdwg_test_reporter;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  fpcunit,
  testregistry;

type
  TFPDWGReporterTest = class(TTestCase)
  published
    procedure JSONReporterProducesStableSnapshot;
    procedure TextReporterIncludesSummaryStatsAndWarnings;
    procedure ObjectDetailByHandleRendersLineGeometry;
    procedure ObjectDetailByHandleRendersArcGeometry;
  end;

implementation

uses
  SysUtils,
  dwg,
  fpdwg_types,
  fpdwg_document,
  fpdwg_reporter,
  fpdwg_validator,
  fpdwg_model_base,
  fpdwg_model_entities,
  fpdwg_model_tables,
  fpdwg_model_unknown;

function Ref(AHandle: TDWGHandle): TDWGHandleRef;
begin
  Result.Value := AHandle;
  Result.Source := hsAbsoluteRef;
end;

function NewSampleDocument(out Line: TDWGLine; out Arc: TDWGArc;
  out Unknown: TDWGUnknownObject): TDWGDocument;
var
  Layer: TDWGLayer;
  Linetype: TDWGLinetype;
begin
  Result := TDWGDocument.Create(lmTolerant, nil);
  Result.FileName := 'samples/simple.dwg';
  Result.Version := dvR2007;
  Result.Codepage := 65001;
  Result.RawObjectCount := 5;
  Result.RawClassCount := 2;

  Linetype := TDWGLinetype.Create;
  Linetype.Handle := $20;
  Linetype.RawObjectType := DWG_TYPE_LTYPE;
  Linetype.LinetypeName := 'Continuous';
  Linetype.Description := 'Solid line';
  Linetype.PatternLength := 0.0;
  Linetype.Status := osResolved;

  Layer := TDWGLayer.Create;
  Layer.Handle := $10;
  Layer.RawObjectType := DWG_TYPE_LAYER;
  Layer.LayerName := 'Walls';
  Layer.ColorIndex := 1;
  Layer.LineWeight := 25;
  Layer.Locked := True;
  Layer.Plot := True;
  Layer.LinetypeHandle := Ref($20);
  Layer.Linetype := Linetype;
  Layer.Status := osResolved;

  Line := TDWGLine.Create;
  Line.Handle := $40;
  Line.RawObjectType := DWG_TYPE_LINE;
  Line.OwnerHandle := Ref($30);
  Line.LayerHandle := Ref($10);
  Line.LinetypeHandle := Ref($20);
  Line.Layer := Layer;
  Line.Linetype := Linetype;
  Line.ColorIndex := 256;
  Line.LineWeight := 18;
  Line.Visible := True;
  Line.StartPoint.X := 0.0;
  Line.StartPoint.Y := 0.0;
  Line.StartPoint.Z := 1.0;
  Line.EndPoint.X := 3.0;
  Line.EndPoint.Y := 4.0;
  Line.EndPoint.Z := 13.0;
  Line.Status := osResolved;

  Arc := TDWGArc.Create;
  Arc.Handle := $50;
  Arc.RawObjectType := DWG_TYPE_ARC;
  Arc.OwnerHandle := Ref($30);
  Arc.LayerHandle := Ref($10);
  Arc.LinetypeHandle := Ref($20);
  Arc.Layer := Layer;
  Arc.Linetype := Linetype;
  Arc.ColorIndex := 2;
  Arc.LineWeight := 18;
  Arc.Visible := True;
  Arc.Center.X := 10.0;
  Arc.Center.Y := 20.0;
  Arc.Center.Z := 0.0;
  Arc.Radius := 5.0;
  Arc.Thickness := 0.25;
  Arc.Extrusion.Z := 1.0;
  Arc.StartAngle := 0.5;
  Arc.EndAngle := 1.5;
  Arc.Status := osResolved;

  Unknown := TDWGUnknownObject.Create;
  Unknown.Handle := $60;
  Unknown.RawObjectType := DWG_TYPE_CIRCLE;
  Unknown.DxfName := 'ACAD_PROXY_ENTITY';
  Unknown.Supertype := DWG_SUPERTYPE_ENTITY;
  Unknown.Size := 42;
  Unknown.BitSize := 104;
  Unknown.UnknownBitsSize := 16;
  Unknown.UnknownRestSize := 3;
  Unknown.Reason := 'mapper not registered for DWG_TYPE_CIRCLE';
  Unknown.Status := osResolved;

  Result.AddObject(Layer);
  Result.AddObject(Linetype);
  Result.AddObject(Line);
  Result.AddObject(Arc);
  Result.AddObject(Unknown);
end;

function NewSampleValidation(Line: TDWGLine;
  Unknown: TDWGUnknownObject): TDWGValidationResult;
var
  BrokenRef: TDWGBrokenReference;
  Orphan: TDWGOrphanObject;
begin
  Result := TDWGValidationResult.Create;

  BrokenRef.Source := Line;
  BrokenRef.SourceHandle := Line.Handle;
  BrokenRef.TargetHandle := $99;
  BrokenRef.RefName := 'layer';
  BrokenRef.Required := True;
  BrokenRef.Reason := 'target handle not found';
  Result.AddBrokenRef(BrokenRef);

  Orphan.Obj := Unknown;
  Orphan.Handle := Unknown.Handle;
  Orphan.Reason := 'owner reference is null';
  Result.AddOrphan(Orphan);
end;

procedure TFPDWGReporterTest.JSONReporterProducesStableSnapshot;
var
  Doc: TDWGDocument;
  Reporter: TDWGJSONReporter;
  Line: TDWGLine;
  Arc: TDWGArc;
  Unknown: TDWGUnknownObject;
  Expected: string;
begin
  Doc := NewSampleDocument(Line, Arc, Unknown);
  try
    Reporter := TDWGJSONReporter.Create(Doc, nil);
    try
      Expected :=
        '{' + LineEnding +
        '  "file": "samples/simple.dwg",' + LineEnding +
        '  "version": "R_2007",' + LineEnding +
        '  "codepage": 65001,' + LineEnding +
        '  "summary": {"objects": 5, "uniqueHandles": 5, "duplicateHandles": 0, "rawObjects": 5, "rawClasses": 2, "layers": 1, "linetypes": 1, "lines": 1, "arcs": 1, "unknown": 1, "warnings": 0},' + LineEnding +
        '  "layers": [' + LineEnding +
        '    {"handle": "10", "name": "Walls", "color": 1, "lineweight": 25, "off": false, "frozen": false, "locked": true, "plot": true, "linetype": "20", "linetypeName": "Continuous", "status": "resolved"}' + LineEnding +
        '  ],' + LineEnding +
        '  "linetypes": [' + LineEnding +
        '    {"handle": "20", "name": "Continuous", "description": "Solid line", "patternLength": 0, "status": "resolved"}' + LineEnding +
        '  ],' + LineEnding +
        '  "lines": [' + LineEnding +
        '    {"handle": "40", "owner": "30", "layer": "10", "layerName": "Walls", "linetype": "20", "linetypeName": "Continuous", "color": 256, "lineweight": 18, "visible": true, "status": "resolved", "geometry": {"start": [0, 0, 1], "end": [3, 4, 13], "length_3d": 13, "length_xy": 5}}' + LineEnding +
        '  ],' + LineEnding +
        '  "arcs": [' + LineEnding +
        '    {"handle": "50", "owner": "30", "layer": "10", "layerName": "Walls", "linetype": "20", "linetypeName": "Continuous", "color": 2, "lineweight": 18, "visible": true, "status": "resolved", "geometry": {"center": [10, 20, 0], "radius": 5, "thickness": 0.25, "extrusion": [0, 0, 1], "startAngle": 0.5, "endAngle": 1.5}}' + LineEnding +
        '  ],' + LineEnding +
        '  "unknown": [' + LineEnding +
        '    {"handle": "60", "rawType": "DWG_TYPE_CIRCLE", "dxfname": "ACAD_PROXY_ENTITY", "supertype": "DWG_SUPERTYPE_ENTITY", "reason": "mapper not registered for DWG_TYPE_CIRCLE", "size": 42, "bitsize": 104, "unknownBits": 16, "unknownRest": 3, "rawBytes": 0, "status": "resolved"}' + LineEnding +
        '  ],' + LineEnding +
        '  "objects": [' + LineEnding +
        '    {"handle": "10", "rawType": "DWG_TYPE_LAYER", "domainType": "layer", "status": "resolved", "owner": null, "layer": {"handle": "10", "name": "Walls", "color": 1, "lineweight": 25, "off": false, "frozen": false, "locked": true, "plot": true, "linetype": "20", "linetypeName": "Continuous", "status": "resolved"}},' + LineEnding +
        '    {"handle": "20", "rawType": "DWG_TYPE_LTYPE", "domainType": "linetype", "status": "resolved", "owner": null, "linetype": {"handle": "20", "name": "Continuous", "description": "Solid line", "patternLength": 0, "status": "resolved"}},' + LineEnding +
        '    {"handle": "40", "rawType": "DWG_TYPE_LINE", "domainType": "line", "status": "resolved", "owner": "30", "line": {"handle": "40", "owner": "30", "layer": "10", "layerName": "Walls", "linetype": "20", "linetypeName": "Continuous", "color": 256, "lineweight": 18, "visible": true, "status": "resolved", "geometry": {"start": [0, 0, 1], "end": [3, 4, 13], "length_3d": 13, "length_xy": 5}}},' + LineEnding +
        '    {"handle": "50", "rawType": "DWG_TYPE_ARC", "domainType": "arc", "status": "resolved", "owner": "30", "arc": {"handle": "50", "owner": "30", "layer": "10", "layerName": "Walls", "linetype": "20", "linetypeName": "Continuous", "color": 2, "lineweight": 18, "visible": true, "status": "resolved", "geometry": {"center": [10, 20, 0], "radius": 5, "thickness": 0.25, "extrusion": [0, 0, 1], "startAngle": 0.5, "endAngle": 1.5}}},' + LineEnding +
        '    {"handle": "60", "rawType": "DWG_TYPE_CIRCLE", "domainType": "unknown", "status": "resolved", "owner": null, "unknown": {"handle": "60", "rawType": "DWG_TYPE_CIRCLE", "dxfname": "ACAD_PROXY_ENTITY", "supertype": "DWG_SUPERTYPE_ENTITY", "reason": "mapper not registered for DWG_TYPE_CIRCLE", "size": 42, "bitsize": 104, "unknownBits": 16, "unknownRest": 3, "rawBytes": 0, "status": "resolved"}}' + LineEnding +
        '  ],' + LineEnding +
        '  "warnings": [' + LineEnding +
        '  ]' + LineEnding +
        '}' + LineEnding;

      AssertEquals(Expected, Reporter.Render);
    finally
      Reporter.Free;
    end;
  finally
    Doc.Free;
  end;
end;

procedure TFPDWGReporterTest.TextReporterIncludesSummaryStatsAndWarnings;
var
  Doc: TDWGDocument;
  Validation: TDWGValidationResult;
  Reporter: TDWGTextReporter;
  Line: TDWGLine;
  Arc: TDWGArc;
  Unknown: TDWGUnknownObject;
  Report: string;
begin
  Doc := NewSampleDocument(Line, Arc, Unknown);
  Validation := NewSampleValidation(Line, Unknown);
  try
    Reporter := TDWGTextReporter.Create(Doc, Validation);
    try
      Report := Reporter.Render;
      AssertTrue(Pos('Warnings: 2', Report) > 0);
      AssertTrue(Pos('Layers:', Report) > 0);
      AssertTrue(Pos('length_3d=13.000', Report) > 0);
      AssertTrue(Pos('Arcs:', Report) > 0);
      AssertTrue(Pos('start_angle=0.500', Report) > 0);
      AssertTrue(Pos(
        'broken_ref source=40 ref=layer target=99 required=true reason="target handle not found"',
        Report) > 0);
      AssertTrue(Pos('orphan handle=60 reason="owner reference is null"',
        Report) > 0);
    finally
      Reporter.Free;
    end;
  finally
    Validation.Free;
    Doc.Free;
  end;
end;

procedure TFPDWGReporterTest.ObjectDetailByHandleRendersLineGeometry;
var
  Doc: TDWGDocument;
  Reporter: TDWGJSONReporter;
  Line: TDWGLine;
  Arc: TDWGArc;
  Unknown: TDWGUnknownObject;
  Detail: string;
begin
  Doc := NewSampleDocument(Line, Arc, Unknown);
  try
    Reporter := TDWGJSONReporter.Create(Doc, nil);
    try
      Detail := Reporter.RenderObjectDetail($40);
      AssertTrue(Pos('"handle": "40"', Detail) > 0);
      AssertTrue(Pos('"length_3d": 13', Detail) > 0);
      AssertTrue(Pos('"length_xy": 5', Detail) > 0);
    finally
      Reporter.Free;
    end;
  finally
    Doc.Free;
  end;
end;

procedure TFPDWGReporterTest.ObjectDetailByHandleRendersArcGeometry;
var
  Doc: TDWGDocument;
  Reporter: TDWGJSONReporter;
  Line: TDWGLine;
  Arc: TDWGArc;
  Unknown: TDWGUnknownObject;
  Detail: string;
begin
  Doc := NewSampleDocument(Line, Arc, Unknown);
  try
    Reporter := TDWGJSONReporter.Create(Doc, nil);
    try
      Detail := Reporter.RenderObjectDetail($50);
      AssertTrue(Pos('"handle": "50"', Detail) > 0);
      AssertTrue(Pos('"radius": 5', Detail) > 0);
      AssertTrue(Pos('"startAngle": 0.5', Detail) > 0);
    finally
      Reporter.Free;
    end;
  finally
    Doc.Free;
  end;
end;

begin
  RegisterTests([TFPDWGReporterTest]);
end.
