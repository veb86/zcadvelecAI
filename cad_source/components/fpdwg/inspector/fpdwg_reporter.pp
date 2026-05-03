unit fpdwg_reporter;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes,
  SysUtils,
  fpdwg_types,
  fpdwg_document,
  fpdwg_validator,
  fpdwg_model_base;

type
  TDWGReportFormat = (
    drfText,
    drfJSON
  );

  TDWGReportStats = record
    ObjectCount: Integer;
    UniqueHandleCount: Integer;
    DuplicateHandleCount: Integer;
    RawObjectCount: Integer;
    RawClassCount: Integer;
    LayerCount: Integer;
    LinetypeCount: Integer;
    LineCount: Integer;
    TextCount: Integer;
    UnknownCount: Integer;
    WarningCount: Integer;
  end;

  TDWGReportOptions = record
    IncludeSummary: Boolean;
    IncludeLayers: Boolean;
    IncludeLinetypes: Boolean;
    IncludeLines: Boolean;
    IncludeTexts: Boolean;
    IncludeUnknown: Boolean;
    IncludeObjects: Boolean;
    IncludeWarnings: Boolean;
    class function Default: TDWGReportOptions; static;
  end;

  EDWGReporterError = class(Exception);

  TDWGBaseReporter = class
  protected
    FDocument: TDWGDocument;
    FValidation: TDWGValidationResult;
    FOptions: TDWGReportOptions;
    function CollectStats: TDWGReportStats;
    function FindObject(AHandle: TDWGHandle; out Obj: TDWGObject): Boolean;
  public
    constructor Create(ADocument: TDWGDocument;
      AValidation: TDWGValidationResult = nil);
    property Options: TDWGReportOptions read FOptions write FOptions;
    function Render: string; virtual; abstract;
    function RenderObjectDetail(AHandle: TDWGHandle): string; virtual; abstract;
  end;

  TDWGTextReporter = class(TDWGBaseReporter)
  private
    procedure AddBlank(Lines: TStrings);
    procedure AppendSummary(Lines: TStrings);
    procedure AppendLayers(Lines: TStrings);
    procedure AppendLinetypes(Lines: TStrings);
    procedure AppendLines(Lines: TStrings);
    procedure AppendTexts(Lines: TStrings);
    procedure AppendUnknown(Lines: TStrings);
    procedure AppendWarnings(Lines: TStrings);
    procedure AppendObjectDetail(Lines: TStrings; Obj: TDWGObject);
  public
    function Render: string; override;
    function RenderObjectDetail(AHandle: TDWGHandle): string; override;
  end;

  TDWGJSONReporter = class(TDWGBaseReporter)
  private
    function SummaryJSON(const Stats: TDWGReportStats): string;
    function LayerJSON(Obj: TDWGObject): string;
    function LinetypeJSON(Obj: TDWGObject): string;
    function LineJSON(Obj: TDWGObject): string;
    function TextJSON(Obj: TDWGObject): string;
    function UnknownJSON(Obj: TDWGObject): string;
    function ObjectJSON(Obj: TDWGObject): string;
    procedure AppendObjectArray(Lines: TStrings; const Name: string;
      Items: TStrings; AddComma: Boolean);
    procedure CollectLayerItems(Items: TStrings);
    procedure CollectLinetypeItems(Items: TStrings);
    procedure CollectLineItems(Items: TStrings);
    procedure CollectTextItems(Items: TStrings);
    procedure CollectUnknownItems(Items: TStrings);
    procedure CollectObjectItems(Items: TStrings);
    procedure CollectWarningItems(Items: TStrings);
  public
    function Render: string; override;
    function RenderObjectDetail(AHandle: TDWGHandle): string; override;
  end;

function RenderDWGReport(ADocument: TDWGDocument;
  AValidation: TDWGValidationResult; AFormat: TDWGReportFormat): string;
function DWGVersionToReportString(Version: TDWGVersion): string;

implementation

uses
  dwg,
  fpdwg_libredwg_utils,
  fpdwg_model_blocks,
  fpdwg_model_entities,
  fpdwg_model_tables,
  fpdwg_model_unknown;

class function TDWGReportOptions.Default: TDWGReportOptions;
begin
  Result.IncludeSummary := True;
  Result.IncludeLayers := True;
  Result.IncludeLinetypes := True;
  Result.IncludeLines := True;
  Result.IncludeTexts := True;
  Result.IncludeUnknown := True;
  Result.IncludeObjects := True;
  Result.IncludeWarnings := True;
end;

function DWGVersionToReportString(Version: TDWGVersion): string;
begin
  case Version of
    dvInvalid: Result := 'invalid';
    dvR13: Result := 'R_13';
    dvR14: Result := 'R_14';
    dvR2000: Result := 'R_2000';
    dvR2004: Result := 'R_2004';
    dvR2007: Result := 'R_2007';
    dvR2010: Result := 'R_2010';
    dvR2013: Result := 'R_2013';
    dvR2018: Result := 'R_2018';
    dvAfter: Result := 'after_R_2018';
  else
    Result := 'unknown';
  end;
end;

function BooleanText(Value: Boolean): string;
begin
  if Value then
    Result := 'true'
  else
    Result := 'false';
end;

function InvariantFormatSettings: TFormatSettings;
begin
  Result := DefaultFormatSettings;
  Result.DecimalSeparator := '.';
end;

function FloatText(Value: Double): string;
begin
  Result := FormatFloat('0.000', Value, InvariantFormatSettings);
end;

function FloatJSON(Value: Double): string;
begin
  Result := FloatToStrF(Value, ffFixed, 18, 6, InvariantFormatSettings);
  while (Pos('.', Result) > 0) and (Length(Result) > 0) and
        (Result[Length(Result)] = '0') do
    Delete(Result, Length(Result), 1);
  if (Length(Result) > 0) and (Result[Length(Result)] = '.') then
    Delete(Result, Length(Result), 1);
  if (Result = '') or (Result = '-0') then
    Result := '0';
end;

function HandleText(AHandle: TDWGHandle): string;
begin
  if AHandle = 0 then
    Result := '0'
  else
    Result := IntToHex(AHandle, 1);
end;

function HandleJSON(AHandle: TDWGHandle): string;
begin
  Result := '"' + HandleText(AHandle) + '"';
end;

function HandleRefJSON(const Ref: TDWGHandleRef): string;
begin
  if Ref.IsNull then
    Result := 'null'
  else
    Result := HandleJSON(Ref.Value);
end;

function QuoteText(const Value: string): string;
begin
  Result := '"' + StringReplace(Value, '"', '\"', [rfReplaceAll]) + '"';
end;

function JSONString(const Value: string): string;
var
  I: Integer;
  C: Char;
begin
  Result := '"';
  for I := 1 to Length(Value) do
  begin
    C := Value[I];
    case C of
      '"': Result := Result + '\"';
      '\': Result := Result + '\\';
      #8: Result := Result + '\b';
      #9: Result := Result + '\t';
      #10: Result := Result + '\n';
      #12: Result := Result + '\f';
      #13: Result := Result + '\r';
    else
      if Ord(C) < 32 then
        Result := Result + '\u' + IntToHex(Ord(C), 4)
      else
        Result := Result + C;
    end;
  end;
  Result := Result + '"';
end;

function OptionalJSONString(const Value: string; HasValue: Boolean): string;
begin
  if HasValue then
    Result := JSONString(Value)
  else
    Result := 'null';
end;

function PointText(const P: TDWGPoint3D): string;
begin
  Result := Format('(%s, %s, %s)', [
    FloatText(P.X),
    FloatText(P.Y),
    FloatText(P.Z)
  ]);
end;

function PointJSON(const P: TDWGPoint3D): string;
begin
  Result := Format('[%s, %s, %s]', [
    FloatJSON(P.X),
    FloatJSON(P.Y),
    FloatJSON(P.Z)
  ]);
end;

function DWGSupertypeName(Supertype: DWG_OBJECT_SUPERTYPE): string;
begin
  WriteStr(Result, Supertype);
  if Result = '' then
    Result := Format('DWG_OBJECT_SUPERTYPE(%d)', [Ord(Supertype)]);
end;

function ObjectDisplayName(Obj: TDWGObject): string;
begin
  Result := '';
  if Obj = nil then
    Exit;

  if Obj is TDWGLayer then
    Result := TDWGLayer(Obj).LayerName
  else if Obj is TDWGLinetype then
    Result := TDWGLinetype(Obj).LinetypeName
  else if Obj is TDWGBlockHeader then
    Result := TDWGBlockHeader(Obj).BlockName
  else if Obj is TDWGSyntheticTable then
    Result := TDWGSyntheticTable(Obj).TableKind
  else if Obj.Name <> '' then
    Result := Obj.Name
  else if Obj.DxfName <> '' then
    Result := Obj.DxfName;

  if Result = '' then
    Result := HandleText(Obj.Handle);
end;

function ResolvedOrRefText(Obj: TDWGObject; const Ref: TDWGHandleRef): string;
begin
  if Obj <> nil then
    Result := ObjectDisplayName(Obj)
  else if not Ref.IsNull then
    Result := Ref.ToString
  else
    Result := 'null';
end;

function ResolvedNameJSON(Obj: TDWGObject): string;
begin
  Result := OptionalJSONString(ObjectDisplayName(Obj), Obj <> nil);
end;

constructor TDWGBaseReporter.Create(ADocument: TDWGDocument;
  AValidation: TDWGValidationResult);
begin
  inherited Create;
  if ADocument = nil then
    raise EDWGReporterError.Create('Cannot render nil DWG document');
  FDocument := ADocument;
  FValidation := AValidation;
  FOptions := TDWGReportOptions.Default;
end;

function TDWGBaseReporter.CollectStats: TDWGReportStats;
var
  I: Integer;
  Obj: TDWGObject;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.ObjectCount := FDocument.Registry.Count;
  Result.UniqueHandleCount := FDocument.Registry.UniqueCount;
  Result.DuplicateHandleCount := FDocument.Registry.DuplicateCount;
  Result.RawObjectCount := FDocument.RawObjectCount;
  Result.RawClassCount := FDocument.RawClassCount;

  for I := 0 to FDocument.Registry.Count - 1 do
  begin
    Obj := FDocument.Registry.ObjectAt(I);
    if Obj is TDWGLayer then
      Inc(Result.LayerCount)
    else if Obj is TDWGLinetype then
      Inc(Result.LinetypeCount)
    else if Obj is TDWGLine then
      Inc(Result.LineCount)
    else if Obj is TDWGText then
      Inc(Result.TextCount)
    else if Obj is TDWGUnknownObject then
      Inc(Result.UnknownCount);
  end;

  if FValidation <> nil then
    Result.WarningCount := FValidation.BrokenRefCount +
      FValidation.OrphanCount + FValidation.CycleCount;
end;

function TDWGBaseReporter.FindObject(AHandle: TDWGHandle;
  out Obj: TDWGObject): Boolean;
begin
  Result := FDocument.Registry.TryGet(AHandle, Obj);
end;

procedure TDWGTextReporter.AddBlank(Lines: TStrings);
begin
  if (Lines.Count > 0) and (Lines[Lines.Count - 1] <> '') then
    Lines.Add('');
end;

procedure TDWGTextReporter.AppendSummary(Lines: TStrings);
var
  Stats: TDWGReportStats;
begin
  Stats := CollectStats;
  Lines.Add(Format('File: %s', [FDocument.FileName]));
  Lines.Add(Format('DWG version: %s',
    [DWGVersionToReportString(FDocument.Version)]));
  Lines.Add(Format('Codepage: %d', [FDocument.Codepage]));
  Lines.Add(Format('Objects: %d', [Stats.ObjectCount]));
  Lines.Add(Format('Unique handles: %d', [Stats.UniqueHandleCount]));
  Lines.Add(Format('Duplicate handles: %d', [Stats.DuplicateHandleCount]));
  Lines.Add(Format('Raw objects: %d', [Stats.RawObjectCount]));
  Lines.Add(Format('Raw classes: %d', [Stats.RawClassCount]));
  Lines.Add(Format('Layers: %d', [Stats.LayerCount]));
  Lines.Add(Format('Linetypes: %d', [Stats.LinetypeCount]));
  Lines.Add(Format('Lines: %d', [Stats.LineCount]));
  Lines.Add(Format('Texts: %d', [Stats.TextCount]));
  Lines.Add(Format('Unknown: %d', [Stats.UnknownCount]));
  Lines.Add(Format('Warnings: %d', [Stats.WarningCount]));
end;

procedure TDWGTextReporter.AppendLayers(Lines: TStrings);
var
  I: Integer;
  Obj: TDWGObject;
  Layer: TDWGLayer;
  LTypeName: string;
begin
  AddBlank(Lines);
  Lines.Add('Layers:');
  for I := 0 to FDocument.Registry.Count - 1 do
  begin
    Obj := FDocument.Registry.ObjectAt(I);
    if not (Obj is TDWGLayer) then
      Continue;
    Layer := TDWGLayer(Obj);
    if Layer.Linetype <> nil then
      LTypeName := Layer.Linetype.LinetypeName
    else if not Layer.LinetypeHandle.IsNull then
      LTypeName := Layer.LinetypeHandle.ToString
    else
      LTypeName := 'null';
    Lines.Add(Format(
      '  [%s] %s color=%d lineweight=%d off=%s locked=%s plot=%s linetype=%s status=%s',
      [HandleText(Layer.Handle), QuoteText(Layer.LayerName), Layer.ColorIndex,
       Layer.LineWeight, BooleanText(Layer.Off), BooleanText(Layer.Locked),
       BooleanText(Layer.Plot), LTypeName,
       DWGObjectStatusToString(Layer.Status)]));
  end;
end;

procedure TDWGTextReporter.AppendLinetypes(Lines: TStrings);
var
  I: Integer;
  Obj: TDWGObject;
  Linetype: TDWGLinetype;
begin
  AddBlank(Lines);
  Lines.Add('Linetypes:');
  for I := 0 to FDocument.Registry.Count - 1 do
  begin
    Obj := FDocument.Registry.ObjectAt(I);
    if not (Obj is TDWGLinetype) then
      Continue;
    Linetype := TDWGLinetype(Obj);
    Lines.Add(Format(
      '  [%s] %s description=%s pattern_length=%s status=%s',
      [HandleText(Linetype.Handle), QuoteText(Linetype.LinetypeName),
       QuoteText(Linetype.Description), FloatText(Linetype.PatternLength),
       DWGObjectStatusToString(Linetype.Status)]));
  end;
end;

procedure TDWGTextReporter.AppendLines(Lines: TStrings);
var
  I: Integer;
  Obj: TDWGObject;
  Line: TDWGLine;
begin
  AddBlank(Lines);
  Lines.Add('Lines:');
  for I := 0 to FDocument.Registry.Count - 1 do
  begin
    Obj := FDocument.Registry.ObjectAt(I);
    if not (Obj is TDWGLine) then
      Continue;
    Line := TDWGLine(Obj);
    Lines.Add(Format(
      '  LINE handle=%s owner=%s layer=%s linetype=%s color=%d lineweight=%d visible=%s status=%s',
      [HandleText(Line.Handle), Line.OwnerHandle.ToString,
       ResolvedOrRefText(Line.Layer, Line.LayerHandle),
       ResolvedOrRefText(Line.Linetype, Line.LinetypeHandle),
       Line.ColorIndex, Line.LineWeight, BooleanText(Line.Visible),
       DWGObjectStatusToString(Line.Status)]));
    Lines.Add(Format('    start=%s', [PointText(Line.StartPoint)]));
    Lines.Add(Format('    end=%s', [PointText(Line.EndPoint)]));
    Lines.Add(Format('    length_3d=%s', [FloatText(Line.Length3D)]));
    Lines.Add(Format('    length_xy=%s', [FloatText(Line.LengthXY)]));
  end;
end;

procedure TDWGTextReporter.AppendTexts(Lines: TStrings);
var
  I: Integer;
  Obj: TDWGObject;
  Text: TDWGText;
begin
  AddBlank(Lines);
  Lines.Add('Texts:');
  for I := 0 to FDocument.Registry.Count - 1 do
  begin
    Obj := FDocument.Registry.ObjectAt(I);
    if not (Obj is TDWGText) then
      Continue;
    Text := TDWGText(Obj);
    Lines.Add(Format(
      '  TEXT handle=%s owner=%s layer=%s linetype=%s style=%s color=%d lineweight=%d visible=%s status=%s value=%s',
      [HandleText(Text.Handle), Text.OwnerHandle.ToString,
       ResolvedOrRefText(Text.Layer, Text.LayerHandle),
       ResolvedOrRefText(Text.Linetype, Text.LinetypeHandle),
       ResolvedOrRefText(Text.Style, Text.StyleHandle),
       Text.ColorIndex, Text.LineWeight, BooleanText(Text.Visible),
       DWGObjectStatusToString(Text.Status), QuoteText(Text.TextValue)]));
    Lines.Add(Format('    insert=%s', [PointText(Text.InsertPoint)]));
    Lines.Add(Format('    alignment=%s', [PointText(Text.AlignmentPoint)]));
    Lines.Add(Format('    height=%s rotation=%s width_factor=%s',
      [FloatText(Text.Height), FloatText(Text.Rotation),
       FloatText(Text.WidthFactor)]));
  end;
end;

procedure TDWGTextReporter.AppendUnknown(Lines: TStrings);
var
  I: Integer;
  Obj: TDWGObject;
  Unknown: TDWGUnknownObject;
begin
  AddBlank(Lines);
  Lines.Add('Unknown:');
  for I := 0 to FDocument.Registry.Count - 1 do
  begin
    Obj := FDocument.Registry.ObjectAt(I);
    if not (Obj is TDWGUnknownObject) then
      Continue;
    Unknown := TDWGUnknownObject(Obj);
    Lines.Add(Format(
      '  handle=%s rawType=%s dxfname=%s reason=%s size=%d bitsize=%d status=%s',
      [HandleText(Unknown.Handle), DWGObjectTypeName(Unknown.RawObjectType),
       QuoteText(Unknown.DxfName), QuoteText(Unknown.Reason),
       Unknown.Size, Unknown.BitSize,
       DWGObjectStatusToString(Unknown.Status)]));
  end;
end;

procedure TDWGTextReporter.AppendWarnings(Lines: TStrings);
var
  I: Integer;
  BrokenRef: TDWGBrokenReference;
  Orphan: TDWGOrphanObject;
  Cycle: TDWGOwnerCycle;
begin
  AddBlank(Lines);
  Lines.Add('Warnings:');
  if FValidation = nil then
    Exit;

  for I := 0 to FValidation.BrokenRefCount - 1 do
  begin
    BrokenRef := FValidation.BrokenRefAt(I);
    Lines.Add(Format(
      '  broken_ref source=%s ref=%s target=%s required=%s reason=%s',
      [HandleText(BrokenRef.SourceHandle), BrokenRef.RefName,
       HandleText(BrokenRef.TargetHandle), BooleanText(BrokenRef.Required),
       QuoteText(BrokenRef.Reason)]));
  end;

  for I := 0 to FValidation.OrphanCount - 1 do
  begin
    Orphan := FValidation.OrphanAt(I);
    Lines.Add(Format('  orphan handle=%s reason=%s',
      [HandleText(Orphan.Handle), QuoteText(Orphan.Reason)]));
  end;

  for I := 0 to FValidation.CycleCount - 1 do
  begin
    Cycle := FValidation.CycleAt(I);
    Lines.Add(Format('  owner_cycle start=%s repeated=%s message=%s',
      [HandleText(Cycle.StartHandle), HandleText(Cycle.RepeatedHandle),
       QuoteText(Cycle.Message)]));
  end;
end;

procedure TDWGTextReporter.AppendObjectDetail(Lines: TStrings; Obj: TDWGObject);
var
  Layer: TDWGLayer;
  Linetype: TDWGLinetype;
  Line: TDWGLine;
  Text: TDWGText;
  BlockHeader: TDWGBlockHeader;
  Unknown: TDWGUnknownObject;
begin
  Lines.Add(Format('Object %s:', [HandleText(Obj.Handle)]));
  Lines.Add(Format('  rawType=%s', [DWGObjectTypeName(Obj.RawObjectType)]));
  Lines.Add(Format('  domainType=%s',
    [DWGDomainObjectTypeToString(Obj.DomainType)]));
  Lines.Add(Format('  status=%s', [DWGObjectStatusToString(Obj.Status)]));
  Lines.Add(Format('  owner=%s', [Obj.OwnerHandle.ToString]));

  if Obj is TDWGLayer then
  begin
    Layer := TDWGLayer(Obj);
    Lines.Add(Format('  name=%s', [QuoteText(Layer.LayerName)]));
    Lines.Add(Format('  color=%d', [Layer.ColorIndex]));
    Lines.Add(Format('  lineweight=%d', [Layer.LineWeight]));
    Lines.Add(Format('  linetype=%s', [Layer.LinetypeHandle.ToString]));
  end
  else if Obj is TDWGLinetype then
  begin
    Linetype := TDWGLinetype(Obj);
    Lines.Add(Format('  name=%s', [QuoteText(Linetype.LinetypeName)]));
    Lines.Add(Format('  description=%s', [QuoteText(Linetype.Description)]));
    Lines.Add(Format('  pattern_length=%s',
      [FloatText(Linetype.PatternLength)]));
  end
  else if Obj is TDWGLine then
  begin
    Line := TDWGLine(Obj);
    Lines.Add(Format('  layer=%s', [Line.LayerHandle.ToString]));
    Lines.Add(Format('  linetype=%s', [Line.LinetypeHandle.ToString]));
    Lines.Add(Format('  start=%s', [PointText(Line.StartPoint)]));
    Lines.Add(Format('  end=%s', [PointText(Line.EndPoint)]));
    Lines.Add(Format('  length_3d=%s', [FloatText(Line.Length3D)]));
    Lines.Add(Format('  length_xy=%s', [FloatText(Line.LengthXY)]));
  end
  else if Obj is TDWGText then
  begin
    Text := TDWGText(Obj);
    Lines.Add(Format('  text=%s', [QuoteText(Text.TextValue)]));
    Lines.Add(Format('  layer=%s', [Text.LayerHandle.ToString]));
    Lines.Add(Format('  linetype=%s', [Text.LinetypeHandle.ToString]));
    Lines.Add(Format('  style=%s', [Text.StyleHandle.ToString]));
    Lines.Add(Format('  insert=%s', [PointText(Text.InsertPoint)]));
    Lines.Add(Format('  alignment=%s', [PointText(Text.AlignmentPoint)]));
    Lines.Add(Format('  height=%s', [FloatText(Text.Height)]));
    Lines.Add(Format('  rotation=%s', [FloatText(Text.Rotation)]));
    Lines.Add(Format('  width_factor=%s', [FloatText(Text.WidthFactor)]));
  end
  else if Obj is TDWGBlockHeader then
  begin
    BlockHeader := TDWGBlockHeader(Obj);
    Lines.Add(Format('  name=%s', [QuoteText(BlockHeader.BlockName)]));
    Lines.Add(Format('  base_point=%s', [PointText(BlockHeader.BasePoint)]));
  end
  else if Obj is TDWGUnknownObject then
  begin
    Unknown := TDWGUnknownObject(Obj);
    Lines.Add(Format('  reason=%s', [QuoteText(Unknown.Reason)]));
    Lines.Add(Format('  size=%d', [Unknown.Size]));
    Lines.Add(Format('  bitsize=%d', [Unknown.BitSize]));
  end;
end;

function TDWGTextReporter.Render: string;
var
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    if FOptions.IncludeSummary then
      AppendSummary(Lines);
    if FOptions.IncludeLayers then
      AppendLayers(Lines);
    if FOptions.IncludeLinetypes then
      AppendLinetypes(Lines);
    if FOptions.IncludeLines then
      AppendLines(Lines);
    if FOptions.IncludeTexts then
      AppendTexts(Lines);
    if FOptions.IncludeUnknown then
      AppendUnknown(Lines);
    if FOptions.IncludeWarnings then
      AppendWarnings(Lines);
    Result := Lines.Text;
  finally
    Lines.Free;
  end;
end;

function TDWGTextReporter.RenderObjectDetail(AHandle: TDWGHandle): string;
var
  Lines: TStringList;
  Obj: TDWGObject;
begin
  Lines := TStringList.Create;
  try
    if FindObject(AHandle, Obj) then
      AppendObjectDetail(Lines, Obj)
    else
      Lines.Add(Format('Object %s not found', [HandleText(AHandle)]));
    Result := Lines.Text;
  finally
    Lines.Free;
  end;
end;

function TDWGJSONReporter.SummaryJSON(const Stats: TDWGReportStats): string;
begin
  Result := '{' +
    '"objects": ' + IntToStr(Stats.ObjectCount) + ', ' +
    '"uniqueHandles": ' + IntToStr(Stats.UniqueHandleCount) + ', ' +
    '"duplicateHandles": ' + IntToStr(Stats.DuplicateHandleCount) + ', ' +
    '"rawObjects": ' + IntToStr(Stats.RawObjectCount) + ', ' +
    '"rawClasses": ' + IntToStr(Stats.RawClassCount) + ', ' +
    '"layers": ' + IntToStr(Stats.LayerCount) + ', ' +
    '"linetypes": ' + IntToStr(Stats.LinetypeCount) + ', ' +
    '"lines": ' + IntToStr(Stats.LineCount) + ', ' +
    '"texts": ' + IntToStr(Stats.TextCount) + ', ' +
    '"unknown": ' + IntToStr(Stats.UnknownCount) + ', ' +
    '"warnings": ' + IntToStr(Stats.WarningCount) +
    '}';
end;

function TDWGJSONReporter.LayerJSON(Obj: TDWGObject): string;
var
  Layer: TDWGLayer;
begin
  Layer := TDWGLayer(Obj);
  Result := '{' +
    '"handle": ' + HandleJSON(Layer.Handle) + ', ' +
    '"name": ' + JSONString(Layer.LayerName) + ', ' +
    '"color": ' + IntToStr(Layer.ColorIndex) + ', ' +
    '"lineweight": ' + IntToStr(Layer.LineWeight) + ', ' +
    '"off": ' + BooleanText(Layer.Off) + ', ' +
    '"locked": ' + BooleanText(Layer.Locked) + ', ' +
    '"plot": ' + BooleanText(Layer.Plot) + ', ' +
    '"linetype": ' + HandleRefJSON(Layer.LinetypeHandle) + ', ' +
    '"linetypeName": ' + ResolvedNameJSON(Layer.Linetype) + ', ' +
    '"status": ' + JSONString(DWGObjectStatusToString(Layer.Status)) +
    '}';
end;

function TDWGJSONReporter.LinetypeJSON(Obj: TDWGObject): string;
var
  Linetype: TDWGLinetype;
begin
  Linetype := TDWGLinetype(Obj);
  Result := '{' +
    '"handle": ' + HandleJSON(Linetype.Handle) + ', ' +
    '"name": ' + JSONString(Linetype.LinetypeName) + ', ' +
    '"description": ' + JSONString(Linetype.Description) + ', ' +
    '"patternLength": ' + FloatJSON(Linetype.PatternLength) + ', ' +
    '"status": ' + JSONString(DWGObjectStatusToString(Linetype.Status)) +
    '}';
end;

function TDWGJSONReporter.LineJSON(Obj: TDWGObject): string;
var
  Line: TDWGLine;
begin
  Line := TDWGLine(Obj);
  Result := '{' +
    '"handle": ' + HandleJSON(Line.Handle) + ', ' +
    '"owner": ' + HandleRefJSON(Line.OwnerHandle) + ', ' +
    '"layer": ' + HandleRefJSON(Line.LayerHandle) + ', ' +
    '"layerName": ' + ResolvedNameJSON(Line.Layer) + ', ' +
    '"linetype": ' + HandleRefJSON(Line.LinetypeHandle) + ', ' +
    '"linetypeName": ' + ResolvedNameJSON(Line.Linetype) + ', ' +
    '"color": ' + IntToStr(Line.ColorIndex) + ', ' +
    '"lineweight": ' + IntToStr(Line.LineWeight) + ', ' +
    '"visible": ' + BooleanText(Line.Visible) + ', ' +
    '"status": ' + JSONString(DWGObjectStatusToString(Line.Status)) + ', ' +
    '"geometry": {' +
      '"start": ' + PointJSON(Line.StartPoint) + ', ' +
      '"end": ' + PointJSON(Line.EndPoint) + ', ' +
      '"length_3d": ' + FloatJSON(Line.Length3D) + ', ' +
      '"length_xy": ' + FloatJSON(Line.LengthXY) +
    '}' +
    '}';
end;

function TDWGJSONReporter.TextJSON(Obj: TDWGObject): string;
var
  Text: TDWGText;
begin
  Text := TDWGText(Obj);
  Result := '{' +
    '"handle": ' + HandleJSON(Text.Handle) + ', ' +
    '"owner": ' + HandleRefJSON(Text.OwnerHandle) + ', ' +
    '"layer": ' + HandleRefJSON(Text.LayerHandle) + ', ' +
    '"layerName": ' + ResolvedNameJSON(Text.Layer) + ', ' +
    '"linetype": ' + HandleRefJSON(Text.LinetypeHandle) + ', ' +
    '"linetypeName": ' + ResolvedNameJSON(Text.Linetype) + ', ' +
    '"style": ' + HandleRefJSON(Text.StyleHandle) + ', ' +
    '"styleName": ' + ResolvedNameJSON(Text.Style) + ', ' +
    '"color": ' + IntToStr(Text.ColorIndex) + ', ' +
    '"lineweight": ' + IntToStr(Text.LineWeight) + ', ' +
    '"visible": ' + BooleanText(Text.Visible) + ', ' +
    '"status": ' + JSONString(DWGObjectStatusToString(Text.Status)) + ', ' +
    '"text": ' + JSONString(Text.TextValue) + ', ' +
    '"geometry": {' +
      '"insert": ' + PointJSON(Text.InsertPoint) + ', ' +
      '"alignment": ' + PointJSON(Text.AlignmentPoint) + ', ' +
      '"extrusion": ' + PointJSON(Text.Extrusion) + ', ' +
      '"thickness": ' + FloatJSON(Text.Thickness) + ', ' +
      '"obliqueAngle": ' + FloatJSON(Text.ObliqueAngle) + ', ' +
      '"rotation": ' + FloatJSON(Text.Rotation) + ', ' +
      '"height": ' + FloatJSON(Text.Height) + ', ' +
      '"widthFactor": ' + FloatJSON(Text.WidthFactor) +
    '}, ' +
    '"generation": ' + IntToStr(Text.Generation) + ', ' +
    '"horizontalAlignment": ' + IntToStr(Text.HorizontalAlignment) + ', ' +
    '"verticalAlignment": ' + IntToStr(Text.VerticalAlignment) +
    '}';
end;

function TDWGJSONReporter.UnknownJSON(Obj: TDWGObject): string;
var
  Unknown: TDWGUnknownObject;
begin
  Unknown := TDWGUnknownObject(Obj);
  Result := '{' +
    '"handle": ' + HandleJSON(Unknown.Handle) + ', ' +
    '"rawType": ' + JSONString(DWGObjectTypeName(Unknown.RawObjectType)) + ', ' +
    '"dxfname": ' + JSONString(Unknown.DxfName) + ', ' +
    '"supertype": ' + JSONString(DWGSupertypeName(Unknown.Supertype)) + ', ' +
    '"reason": ' + JSONString(Unknown.Reason) + ', ' +
    '"size": ' + IntToStr(Unknown.Size) + ', ' +
    '"bitsize": ' + IntToStr(Unknown.BitSize) + ', ' +
    '"unknownBits": ' + IntToStr(Unknown.UnknownBitsSize) + ', ' +
    '"unknownRest": ' + IntToStr(Unknown.UnknownRestSize) + ', ' +
    '"rawBytes": ' + IntToStr(Length(Unknown.RawBytes)) + ', ' +
    '"status": ' + JSONString(DWGObjectStatusToString(Unknown.Status)) +
    '}';
end;

function TDWGJSONReporter.ObjectJSON(Obj: TDWGObject): string;
var
  BlockHeader: TDWGBlockHeader;
  Duplicate: TDWGDuplicateHandleObject;
begin
  Result := '{' +
    '"handle": ' + HandleJSON(Obj.Handle) + ', ' +
    '"rawType": ' + JSONString(DWGObjectTypeName(Obj.RawObjectType)) + ', ' +
    '"domainType": ' + JSONString(DWGDomainObjectTypeToString(Obj.DomainType)) + ', ' +
    '"status": ' + JSONString(DWGObjectStatusToString(Obj.Status)) + ', ' +
    '"owner": ' + HandleRefJSON(Obj.OwnerHandle);

  if Obj is TDWGLayer then
    Result := Result + ', "layer": ' + LayerJSON(Obj)
  else if Obj is TDWGLinetype then
    Result := Result + ', "linetype": ' + LinetypeJSON(Obj)
  else if Obj is TDWGLine then
    Result := Result + ', "line": ' + LineJSON(Obj)
  else if Obj is TDWGText then
    Result := Result + ', "text": ' + TextJSON(Obj)
  else if Obj is TDWGUnknownObject then
    Result := Result + ', "unknown": ' + UnknownJSON(Obj)
  else if Obj is TDWGBlockHeader then
  begin
    BlockHeader := TDWGBlockHeader(Obj);
    Result := Result + ', "blockHeader": {' +
      '"name": ' + JSONString(BlockHeader.BlockName) + ', ' +
      '"basePoint": ' + PointJSON(BlockHeader.BasePoint) + ', ' +
      '"blockEntity": ' + HandleRefJSON(BlockHeader.BlockEntityHandle) + ', ' +
      '"firstEntity": ' + HandleRefJSON(BlockHeader.FirstEntityHandle) + ', ' +
      '"lastEntity": ' + HandleRefJSON(BlockHeader.LastEntityHandle) + ', ' +
      '"endBlockEntity": ' + HandleRefJSON(BlockHeader.EndBlockEntityHandle) + ', ' +
      '"layout": ' + HandleRefJSON(BlockHeader.LayoutHandle) +
      '}';
  end
  else if Obj is TDWGDuplicateHandleObject then
  begin
    Duplicate := TDWGDuplicateHandleObject(Obj);
    Result := Result + ', "duplicate": {' +
      '"originalHandle": ' + HandleJSON(Duplicate.OriginalHandle) + ', ' +
      '"conflictWith": ';
    if Duplicate.ConflictWith <> nil then
      Result := Result + HandleJSON(Duplicate.ConflictWith.Handle)
    else
      Result := Result + 'null';
    Result := Result + '}';
  end
  else if Obj is TDWGSyntheticTable then
    Result := Result + ', "tableKind": ' +
      JSONString(TDWGSyntheticTable(Obj).TableKind);

  Result := Result + '}';
end;

procedure TDWGJSONReporter.AppendObjectArray(Lines: TStrings;
  const Name: string; Items: TStrings; AddComma: Boolean);
var
  I: Integer;
  Suffix: string;
begin
  Lines.Add(Format('  "%s": [', [Name]));
  for I := 0 to Items.Count - 1 do
  begin
    if I < Items.Count - 1 then
      Suffix := ','
    else
      Suffix := '';
    Lines.Add('    ' + Items[I] + Suffix);
  end;
  if AddComma then
    Lines.Add('  ],')
  else
    Lines.Add('  ]');
end;

procedure TDWGJSONReporter.CollectLayerItems(Items: TStrings);
var
  I: Integer;
  Obj: TDWGObject;
begin
  for I := 0 to FDocument.Registry.Count - 1 do
  begin
    Obj := FDocument.Registry.ObjectAt(I);
    if Obj is TDWGLayer then
      Items.Add(LayerJSON(Obj));
  end;
end;

procedure TDWGJSONReporter.CollectLinetypeItems(Items: TStrings);
var
  I: Integer;
  Obj: TDWGObject;
begin
  for I := 0 to FDocument.Registry.Count - 1 do
  begin
    Obj := FDocument.Registry.ObjectAt(I);
    if Obj is TDWGLinetype then
      Items.Add(LinetypeJSON(Obj));
  end;
end;

procedure TDWGJSONReporter.CollectLineItems(Items: TStrings);
var
  I: Integer;
  Obj: TDWGObject;
begin
  for I := 0 to FDocument.Registry.Count - 1 do
  begin
    Obj := FDocument.Registry.ObjectAt(I);
    if Obj is TDWGLine then
      Items.Add(LineJSON(Obj));
  end;
end;

procedure TDWGJSONReporter.CollectTextItems(Items: TStrings);
var
  I: Integer;
  Obj: TDWGObject;
begin
  for I := 0 to FDocument.Registry.Count - 1 do
  begin
    Obj := FDocument.Registry.ObjectAt(I);
    if Obj is TDWGText then
      Items.Add(TextJSON(Obj));
  end;
end;

procedure TDWGJSONReporter.CollectUnknownItems(Items: TStrings);
var
  I: Integer;
  Obj: TDWGObject;
begin
  for I := 0 to FDocument.Registry.Count - 1 do
  begin
    Obj := FDocument.Registry.ObjectAt(I);
    if Obj is TDWGUnknownObject then
      Items.Add(UnknownJSON(Obj));
  end;
end;

procedure TDWGJSONReporter.CollectObjectItems(Items: TStrings);
var
  I: Integer;
begin
  for I := 0 to FDocument.Registry.Count - 1 do
    Items.Add(ObjectJSON(FDocument.Registry.ObjectAt(I)));
end;

procedure TDWGJSONReporter.CollectWarningItems(Items: TStrings);
var
  I: Integer;
  BrokenRef: TDWGBrokenReference;
  Orphan: TDWGOrphanObject;
  Cycle: TDWGOwnerCycle;
begin
  if FValidation = nil then
    Exit;

  for I := 0 to FValidation.BrokenRefCount - 1 do
  begin
    BrokenRef := FValidation.BrokenRefAt(I);
    Items.Add('{' +
      '"kind": "broken_ref", ' +
      '"source": ' + HandleJSON(BrokenRef.SourceHandle) + ', ' +
      '"ref": ' + JSONString(BrokenRef.RefName) + ', ' +
      '"target": ' + HandleJSON(BrokenRef.TargetHandle) + ', ' +
      '"required": ' + BooleanText(BrokenRef.Required) + ', ' +
      '"reason": ' + JSONString(BrokenRef.Reason) +
      '}');
  end;

  for I := 0 to FValidation.OrphanCount - 1 do
  begin
    Orphan := FValidation.OrphanAt(I);
    Items.Add('{' +
      '"kind": "orphan", ' +
      '"handle": ' + HandleJSON(Orphan.Handle) + ', ' +
      '"reason": ' + JSONString(Orphan.Reason) +
      '}');
  end;

  for I := 0 to FValidation.CycleCount - 1 do
  begin
    Cycle := FValidation.CycleAt(I);
    Items.Add('{' +
      '"kind": "owner_cycle", ' +
      '"start": ' + HandleJSON(Cycle.StartHandle) + ', ' +
      '"repeated": ' + HandleJSON(Cycle.RepeatedHandle) + ', ' +
      '"message": ' + JSONString(Cycle.Message) +
      '}');
  end;
end;

function TDWGJSONReporter.Render: string;
var
  Lines, Items: TStringList;
  Stats: TDWGReportStats;
begin
  Lines := TStringList.Create;
  Items := TStringList.Create;
  try
    Stats := CollectStats;
    Lines.Add('{');
    Lines.Add('  "file": ' + JSONString(FDocument.FileName) + ',');
    Lines.Add('  "version": ' + JSONString(DWGVersionToReportString(FDocument.Version)) + ',');
    Lines.Add('  "codepage": ' + IntToStr(FDocument.Codepage) + ',');

    if FOptions.IncludeSummary then
      Lines.Add('  "summary": ' + SummaryJSON(Stats) + ',');

    if FOptions.IncludeLayers then
    begin
      Items.Clear;
      CollectLayerItems(Items);
      AppendObjectArray(Lines, 'layers', Items, True);
    end;

    if FOptions.IncludeLinetypes then
    begin
      Items.Clear;
      CollectLinetypeItems(Items);
      AppendObjectArray(Lines, 'linetypes', Items, True);
    end;

    if FOptions.IncludeLines then
    begin
      Items.Clear;
      CollectLineItems(Items);
      AppendObjectArray(Lines, 'lines', Items, True);
    end;

    if FOptions.IncludeTexts then
    begin
      Items.Clear;
      CollectTextItems(Items);
      AppendObjectArray(Lines, 'texts', Items, True);
    end;

    if FOptions.IncludeUnknown then
    begin
      Items.Clear;
      CollectUnknownItems(Items);
      AppendObjectArray(Lines, 'unknown', Items, True);
    end;

    if FOptions.IncludeObjects then
    begin
      Items.Clear;
      CollectObjectItems(Items);
      AppendObjectArray(Lines, 'objects', Items, True);
    end;

    Items.Clear;
    if FOptions.IncludeWarnings then
      CollectWarningItems(Items);
    AppendObjectArray(Lines, 'warnings', Items, False);

    Lines.Add('}');
    Result := Lines.Text;
  finally
    Items.Free;
    Lines.Free;
  end;
end;

function TDWGJSONReporter.RenderObjectDetail(AHandle: TDWGHandle): string;
var
  Lines: TStringList;
  Obj: TDWGObject;
begin
  Lines := TStringList.Create;
  try
    Lines.Add('{');
    if FindObject(AHandle, Obj) then
      Lines.Add('  "object": ' + ObjectJSON(Obj))
    else
    begin
      Lines.Add('  "error": "handle not found",');
      Lines.Add('  "handle": ' + HandleJSON(AHandle));
    end;
    Lines.Add('}');
    Result := Lines.Text;
  finally
    Lines.Free;
  end;
end;

function RenderDWGReport(ADocument: TDWGDocument;
  AValidation: TDWGValidationResult; AFormat: TDWGReportFormat): string;
var
  Reporter: TDWGBaseReporter;
begin
  case AFormat of
    drfText: Reporter := TDWGTextReporter.Create(ADocument, AValidation);
    drfJSON: Reporter := TDWGJSONReporter.Create(ADocument, AValidation);
  else
    raise EDWGReporterError.Create('Unsupported DWG report format');
  end;

  try
    Result := Reporter.Render;
  finally
    Reporter.Free;
  end;
end;

end.
