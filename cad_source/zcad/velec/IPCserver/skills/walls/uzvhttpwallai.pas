unit uzvhttpwallai;

{$mode objfpc}{$H+}

interface

uses
  fpjson, uzvhttpwallgeometry;

const
  MaxWallVertices = 4096;

function JSONWallNumber(Data: TJSONData; out Number: Double): Boolean;
function JSONWallPoint(Data: TJSONData; DefaultZ: Double; out P: TWallPoint): Boolean;
function CandidatePayload(const SessionID: string; Minimum, Maximum: Double;
  const Segments: TWallSegments; const Candidates: TWallCandidates): string;
function ReadRecognition(const Response: TJSONObject; const SessionID: string;
  Minimum, Maximum: Double; const Segments: TWallSegments;
  const Candidates: TWallCandidates; out Rejected, NonWalls, Unknown: Integer): TWallRecognitions;

implementation

uses
  SysUtils, Math;

function JSONWallNumber(Data: TJSONData; out Number: Double): Boolean;
begin
  Result := (Data <> nil) and (Data.JSONType = jtNumber);
  if Result then begin
    Number := Data.AsFloat;
    Result := not (IsNan(Number) or IsInfinite(Number));
  end;
end;

function JSONWallPoint(Data: TJSONData; DefaultZ: Double; out P: TWallPoint): Boolean;
var
  Items: TJSONArray;
begin
  Result := False;
  if (Data = nil) or (Data.JSONType <> jtArray) then Exit;
  Items := TJSONArray(Data);
  if (Items.Count < 2) or (Items.Count > 3) then Exit;
  P.Z := DefaultZ;
  Result := JSONWallNumber(Items.Items[0], P.X) and
    JSONWallNumber(Items.Items[1], P.Y) and
    ((Items.Count = 2) or JSONWallNumber(Items.Items[2], P.Z));
end;

function PointJSON(const P: TWallPoint): TJSONArray;
begin
  Result := TJSONArray.Create([P.X, P.Y, P.Z]);
end;

function AxisJSON(const Axis: TWallAxis): TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('start', PointJSON(Axis.First));
  Result.Add('end', PointJSON(Axis.Last));
end;

function CandidateID(Index: Integer): string;
begin
  Result := 'C' + IntToStr(Index + 1);
end;

function CandidatePayload(const SessionID: string; Minimum, Maximum: Double;
  const Segments: TWallSegments; const Candidates: TWallCandidates): string;
var
  Root, Item, Edge: TJSONObject;
  List, Edges, Handles: TJSONArray;
  I, J: Integer;
  S: TWallSegment;
begin
  Root := TJSONObject.Create;
  try
    Root.Add('session_id', SessionID);
    Root.Add('operation', 'recognize_walls');
    Root.Add('units', 'drawing_units');
    Root.Add('coordinate_system', 'WCS');
    Root.Add('min_thickness', Minimum);
    Root.Add('max_thickness', Maximum);
    List := TJSONArray.Create;
    Root.Add('candidates', List);
    for I := 0 to High(Candidates) do begin
      Item := TJSONObject.Create;
      List.Add(Item);
      Item.Add('candidate_id', CandidateID(I));
      Item.Add('thickness', Candidates[I].Thickness);
      Item.Add('overlap_length', Candidates[I].Overlap);
      Item.Add('longitudinal_offset', Candidates[I].Offset);
      Item.Add('angle_radians', Candidates[I].Angle);
      Item.Add('centerline', AxisJSON(Candidates[I].Center));
      Handles := TJSONArray.Create;
      Item.Add('source_handles', Handles);
      Edges := TJSONArray.Create;
      Item.Add('boundaries', Edges);
      for J := 0 to 1 do begin
        S := Segments[Candidates[I].Segments[J]];
        Handles.Add(S.Handle);
        Edge := AxisJSON(Candidates[I].Edges[J]);
        Edges.Add(Edge);
        Edge.Add('handle', S.Handle);
        Edge.Add('temporary_handle', S.TemporaryHandle);
        Edge.Add('source_type', S.EntityType);
        Edge.Add('layer', S.Layer);
        Edge.Add('segment_index', S.SegmentIndex);
        Edge.Add('source_length', S.Length);
        Edge.Add('source_direction', PointJSON(S.Direction));
      end;
    end;
    Result := Root.AsJSON;
  finally
    Root.Free;
  end;
end;

function MatchingHandles(Data: TJSONData; const A, B: string): Boolean;
var
  X, Y: string;
begin
  Result := False;
  if (Data = nil) or (Data.JSONType <> jtArray) or (Data.Count <> 2) or
    (Data.Items[0].JSONType <> jtString) or (Data.Items[1].JSONType <> jtString) then
    Exit;
  X := Data.Items[0].AsString;
  Y := Data.Items[1].AsString;
  Result := (SameText(X, A) and SameText(Y, B)) or
    (SameText(X, B) and SameText(Y, A));
end;

function ReadRecognition(const Response: TJSONObject; const SessionID: string;
  Minimum, Maximum: Double; const Segments: TWallSegments;
  const Candidates: TWallCandidates; out Rejected, NonWalls, Unknown: Integer): TWallRecognitions;
var
  List, Item, Data, AxisData, PointsData: TJSONData;
  Seen: array of Boolean;
  I, J, Index, Count: Integer;
  Recognized: TWallRecognition;
  ID: string;
  IsWall, Valid: Boolean;
  Z: Double;
  Temp: TWallPoint;
begin
  Result := nil;
  Rejected := 0;
  NonWalls := 0;
  Unknown := 0;
  Data := Response.Find('session_id', jtString);
  if (Data = nil) or (Data.AsString <> SessionID) then
    raise Exception.Create('Invalid wall result session_id');
  List := Response.Find('walls', jtArray);
  if List = nil then
    raise Exception.Create('Wall result must contain a walls array');

  SetLength(Seen, Length(Candidates));
  SetLength(Result, Min(List.Count, Length(Candidates)));
  Count := 0;
  for I := 0 to List.Count - 1 do begin
    Item := List.Items[I];
    Inc(Rejected);
    if Item.JSONType <> jtObject then Continue;
    Data := TJSONObject(Item).Find('candidate_id', jtString);
    if Data = nil then Continue;
    ID := Data.AsString;
    if (Copy(ID, 1, 1) <> 'C') or
      not TryStrToInt(Copy(ID, 2, MaxInt), Index) or
      (Index < 1) or (Index > Length(Candidates)) then begin
      Inc(Unknown);
      Continue;
    end;
    Dec(Index);
    if (ID <> CandidateID(Index)) or Seen[Index] then Continue;
    Data := TJSONObject(Item).Find('is_wall', jtBoolean);
    if Data = nil then Continue;
    IsWall := Data.AsBoolean;
    if not JSONWallNumber(TJSONObject(Item).Find('confidence'), Recognized.Confidence) or
      (Recognized.Confidence < 0) or (Recognized.Confidence > 1) then
      Continue;
    if not MatchingHandles(TJSONObject(Item).Find('source_handles'),
      Segments[Candidates[Index].Segments[0]].Handle,
      Segments[Candidates[Index].Segments[1]].Handle) then
      Continue;
    if not IsWall then begin
      Seen[Index] := True;
      Inc(NonWalls);
      Dec(Rejected);
      Continue;
    end;
    Recognized.Candidate := Index;
    if not JSONWallNumber(TJSONObject(Item).Find('thickness'), Recognized.Thickness) or
      (Recognized.Thickness < Minimum) or (Recognized.Thickness > Maximum) or
      (Abs(Recognized.Thickness - Candidates[Index].Thickness) >
       WallAxisTolerance * Candidates[Index].Thickness) then
      Continue;
    AxisData := TJSONObject(Item).Find('centerline', jtObject);
    if AxisData = nil then Continue;
    PointsData := TJSONObject(AxisData).Find('vertices', jtArray);
    if (PointsData = nil) or (PointsData.Count < 2) or
      (PointsData.Count > MaxWallVertices) then Continue;
    Recognized.Vertices := nil;
    SetLength(Recognized.Vertices, PointsData.Count);
    Z := Candidates[Index].Center.First.Z;
    Valid := True;
    for J := 0 to PointsData.Count - 1 do begin
      if not JSONWallPoint(PointsData.Items[J], Z, Recognized.Vertices[J]) or
        ((PointsData.Items[J].Count = 2) and
          (Abs(Candidates[Index].Center.Last.Z - Z) > WallEpsilon)) then begin
        Valid := False;
        Break;
      end;
    end;
    if not Valid then Continue;
    if not ValidWallAxis(Recognized.Vertices, Candidates[Index]) then begin
      // Направление перечисления вершин не влияет на результат.
      for J := 0 to Length(Recognized.Vertices) div 2 - 1 do begin
        Temp := Recognized.Vertices[J];
        Recognized.Vertices[J] := Recognized.Vertices[High(Recognized.Vertices) - J];
        Recognized.Vertices[High(Recognized.Vertices) - J] := Temp;
      end;
      if not ValidWallAxis(Recognized.Vertices, Candidates[Index]) then Continue;
    end;
    Seen[Index] := True;
    Dec(Rejected);
    Result[Count] := Recognized;
    Inc(Count);
  end;
  SetLength(Result, Count);
end;

end.
