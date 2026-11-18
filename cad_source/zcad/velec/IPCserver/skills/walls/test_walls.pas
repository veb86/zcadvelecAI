program test_walls;

{$mode objfpc}{$H+}

uses
  SysUtils, fpjson, jsonparser, uzvhttpwallgeometry, uzvhttpwallai;

procedure Check(Condition: Boolean; const MessageText: string);
begin
  if not Condition then raise Exception.Create(MessageText);
end;

function Segment(const A, B: TWallPoint; const Handle: string): TWallSegment;
begin
  Result := Default(TWallSegment);
  Result.Axis.First := A;
  Result.Axis.Last := B;
  Check(WallDirection(Result.Axis, Result.Direction, Result.Length), 'Test segment is too short');
  Result.Handle := Handle;
  Result.EntityType := 'LINE';
end;

procedure TestGeometry;
var
  Segments: TWallSegments;
  Candidates: TWallCandidates;
  Stats: TWallStatistics;
  Item: TWallCandidate;
  Total: Double;
begin
  SetLength(Segments, 2);
  Segments[0] := Segment(WallPoint(0, 0, 75), WallPoint(15, 0, 75), 'A');
  Segments[1] := Segment(WallPoint(20, 2, 75), WallPoint(5, 2, 75), 'B');
  Candidates := FindWallCandidates(Segments, 2, 2, Stats);
  Check(Length(Candidates) = 1, 'Reversed edges must match at either thickness bound');
  Check(WallNear(Candidates[0].Center.First, WallPoint(5, 1, 75)), 'Clipped WCS start');
  Check(WallNear(Candidates[0].Center.Last, WallPoint(15, 1, 75)), 'Clipped WCS end');
  Check(Abs(Candidates[0].Overlap - 10) < WallEpsilon, 'Overlap length');
  Check(Abs(Candidates[0].Offset - 5) < WallEpsilon, 'Longitudinal offset');
  Check(Length(FindWallCandidates(Segments, 3, 4, Stats)) = 0, 'Minimum thickness filter');
  Check(Stats.OutsideThickness = 1, 'Thickness rejection diagnostics');
  Check(Length(FindWallCandidates(Segments, 1, 1.9, Stats)) = 0, 'Maximum thickness filter');
  Check(not ValidWallThickness(0, 3) and not ValidWallThickness(4, 3), 'Invalid limits');
  Segments[1] := Segment(WallPoint(15, 2, 75), WallPoint(30, 2, 75), 'B');
  Check(Length(FindWallCandidates(Segments, 1, 3, Stats)) = 0, 'Touching ends do not overlap');
  Segments[1] := Segment(WallPoint(15, 2, 75), WallPoint(0, 3, 75), 'B');
  Check(Length(FindWallCandidates(Segments, 1, 4, Stats)) = 0, 'Angle beyond tolerance');
  Segments[1] := Segment(WallPoint(15, 2, 75), WallPoint(0, 2.2, 75), 'B');
  Check(Length(FindWallCandidates(Segments, 1, 3, Stats)) = 1, 'Angle within tolerance');
  Check(Length(FindWallCandidates(Segments, 1, 2.15, Stats)) = 0, 'Thickness across overlap');

  SetLength(Segments, 4);
  Segments[0] := Segment(WallPoint(0, 0, 0), WallPoint(20, 0, 0), 'A');
  Segments[1] := Segment(WallPoint(0, 2, 0), WallPoint(10, 2, 0), 'B');
  Segments[2] := Segment(WallPoint(15, 2, 0), WallPoint(5, 2, 0), 'C');
  Segments[3] := Segment(WallPoint(10, 2, 0), WallPoint(0, 2, 0), 'D');
  Candidates := FindWallCandidates(Segments, 1, 3, Stats);
  Total := 0;
  for Item in Candidates do Total := Total + Item.Overlap;
  Check(Length(Candidates) = 2, 'Full duplicate removed and partial duplicate clipped');
  Check(Abs(Total - 15) < WallEpsilon, 'Union of overlaps is preserved');
end;

procedure TestRecognition;
const
  Response = '{"session_id":"S","walls":[' +
    '{"candidate_id":"C1","is_wall":true,"confidence":0.8,"thickness":2,' +
    '"source_handles":["B","A"],"centerline":{"vertices":[[8,1],[0,1]]}},' +
    '{"candidate_id":"C2","is_wall":true,"confidence":0.5,"thickness":2,' +
    '"source_handles":["C","D"],"centerline":{"vertices":[[0,11,5],[8,11,5]]}},' +
    '{"candidate_id":"C1","is_wall":false,"confidence":0.8,"source_handles":["B","A"]},' +
    '{"candidate_id":"C3","is_wall":false,"confidence":0.2,"source_handles":["E","F"]},' +
    '{"candidate_id":"C4","is_wall":true,"confidence":0.9,"thickness":2,' +
    '"source_handles":["G","H"],"centerline":{"vertices":[[0,31],[8,36]]}},' +
    '{"candidate_id":"C999","is_wall":true},' +
    '{"candidate_id":"C4","is_wall":true,"confidence":0.9,"thickness":2,' +
    '"source_handles":["H","wrong"],"centerline":{"vertices":[[0,31],[8,31]]}},null]}';
var
  Segments: TWallSegments;
  Candidates: TWallCandidates;
  Recognized: TWallRecognitions;
  Stats: TWallStatistics;
  RequestData, Data: TJSONObject;
  I, Rejected, NonWalls, Unknown: Integer;
  Y: Double;
  Failed: Boolean;
begin
  SetLength(Segments, 8);
  for I := 0 to High(Segments) do begin
    Y := (I div 2) * 10 + (I mod 2) * 2;
    Segments[I] := Segment(WallPoint(0, Y, 5), WallPoint(8, Y, 5), Chr(Ord('A') + I));
  end;
  Candidates := FindWallCandidates(Segments, 1, 3, Stats);
  Check(Length(Candidates) = 4, 'Four independent fixtures');
  RequestData := TJSONObject(GetJSON(CandidatePayload('S', 1, 3, Segments, Candidates)));
  Data := TJSONObject(GetJSON(Response));
  try
    Check(RequestData.Get('coordinate_system', '') = 'WCS', 'WCS declaration');
    Check(RequestData.Arrays['candidates'].Objects[0].Arrays['boundaries'].Count = 2, 'Boundary data');
    Recognized := ReadRecognition(Data, 'S', 1, 3, Segments, Candidates, Rejected, NonWalls, Unknown);
    Check((Rejected = 5) and (NonWalls = 1) and (Unknown = 1), 'Invalid rows and classifications');
    Check(Length(Recognized) = 2, 'Accept valid rows of a mixed response');
    Check(WallNear(Recognized[0].Vertices[0], WallPoint(0, 1, 5)), '2D result retains WCS Z');
    Check(WallNear(Recognized[1].Vertices[0], WallPoint(0, 11, 5)), 'Rows own their vertices');
    Failed := False;
    try
      Recognized := ReadRecognition(Data, 'other', 1, 3, Segments, Candidates, Rejected, NonWalls, Unknown);
    except
      on E: Exception do Failed := True;
    end;
    Check(Failed, 'Wrong session rejected');
  finally
    Data.Free;
    RequestData.Free;
  end;
end;

begin
  TestGeometry;
  TestRecognition;
  WriteLn('Wall geometry and recognition tests passed');
end.
