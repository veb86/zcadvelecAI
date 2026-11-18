unit uzvhttpwallgeometry;

{$mode objfpc}{$H+}

interface

const
  WallEpsilon = 1e-6;
  WallMinOverlap = 0.001;
  WallAngleTolerance = 0.0174532925199433; // Один градус, в радианах.
  WallAxisTolerance = 0.25; // Доля толщины для допустимой поправки оси.

type
  TWallPoint = record
    X, Y, Z: Double;
  end;
  TWallAxis = record
    First, Last: TWallPoint;
  end;
  TWallVertices = array of TWallPoint;

  TWallScopeKind = (wskSelection, wskWindow, wskHandles);
  TWallStrings = array of string;
  TWallScope = record
    Kind: TWallScopeKind;
    Window: TWallAxis;
    Handles: TWallStrings;
  end;

  TWallSegment = record
    Axis: TWallAxis;
    Direction: TWallPoint;
    Length: Double;
    Handle, Layer, EntityType: string;
    TemporaryHandle: Boolean;
    Source, SegmentIndex: Integer;
  end;
  TWallSegments = array of TWallSegment;

  TWallCandidate = record
    Segments: array[0..1] of Integer;
    Edges: array[0..1] of TWallAxis;
    Center: TWallAxis;
    Angle, Thickness, Overlap, Offset: Double;
  end;
  TWallCandidates = array of TWallCandidate;
  TWallStatistics = record
    Rejected, OutsideThickness, Eligible: Int64;
    MinFound, MaxFound: Double;
  end;

  TWallRecognition = record
    Candidate: Integer;
    Thickness, Confidence: Double;
    Vertices: TWallVertices;
  end;
  TWallRecognitions = array of TWallRecognition;

function WallPoint(X, Y, Z: Double): TWallPoint;
function FiniteWallPoint(const P: TWallPoint): Boolean;
function WallDistance(const A, B: TWallPoint): Double;
function WallNear(const A, B: TWallPoint): Boolean;
function WallDirection(const Axis: TWallAxis; out Dir: TWallPoint;
  out Len: Double): Boolean;
function ValidWallThickness(Minimum, Maximum: Double): Boolean;
function ValidWallAxis(const Vertices: TWallVertices;
  const Candidate: TWallCandidate): Boolean;
function FindWallCandidates(const Segments: TWallSegments; Minimum, Maximum: Double;
  out Stats: TWallStatistics): TWallCandidates;

implementation

uses
  Math;

function WallPoint(X, Y, Z: Double): TWallPoint;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
end;

function FiniteWallPoint(const P: TWallPoint): Boolean;
begin
  Result := not (IsNan(P.X) or IsInfinite(P.X) or
    IsNan(P.Y) or IsInfinite(P.Y) or IsNan(P.Z) or IsInfinite(P.Z));
end;

function Delta(const A, B: TWallPoint): TWallPoint;
begin
  Result := WallPoint(B.X - A.X, B.Y - A.Y, B.Z - A.Z);
end;

function Dot(const A, B: TWallPoint): Double;
begin
  Result := A.X * B.X + A.Y * B.Y + A.Z * B.Z;
end;

function Along(const A, Dir: TWallPoint; T: Double): TWallPoint;
begin
  Result := WallPoint(A.X + T * Dir.X, A.Y + T * Dir.Y, A.Z + T * Dir.Z);
end;

function Interpolate(const Axis: TWallAxis; T: Double): TWallPoint;
begin
  Result := Along(Axis.First, Delta(Axis.First, Axis.Last), T);
end;

function WallDistance(const A, B: TWallPoint): Double;
var
  D: TWallPoint;
begin
  D := Delta(A, B);
  Result := Hypot(Hypot(D.X, D.Y), D.Z);
end;

function WallNear(const A, B: TWallPoint): Boolean;
begin
  Result := WallDistance(A, B) <= WallEpsilon;
end;

function WallDirection(const Axis: TWallAxis; out Dir: TWallPoint;
  out Len: Double): Boolean;
var
  D: TWallPoint;
begin
  Len := WallDistance(Axis.First, Axis.Last);
  Result := (Len > WallEpsilon) and not (IsNan(Len) or IsInfinite(Len));
  if Result then begin
    D := Delta(Axis.First, Axis.Last);
    Dir := WallPoint(D.X / Len, D.Y / Len, D.Z / Len);
  end;
end;

function ValidWallThickness(Minimum, Maximum: Double): Boolean;
begin
  Result := not (IsNan(Minimum) or IsNan(Maximum) or
    IsInfinite(Minimum) or IsInfinite(Maximum)) and
    (Minimum > 0) and (Maximum > Minimum);
end;

function ValidWallAxis(const Vertices: TWallVertices;
  const Candidate: TWallCandidate): Boolean;
var
  Dir, P: TWallPoint;
  Len, T, Previous: Double;
  I: Integer;
begin
  Result := False;
  if (Length(Vertices) < 2) or
    not WallDirection(Candidate.Center, Dir, Len) then
    Exit;
  Previous := -2 * WallEpsilon;
  for I := 0 to High(Vertices) do begin
    P := Vertices[I];
    if not FiniteWallPoint(P) then
      Exit;
    T := Dot(Delta(Candidate.Center.First, P), Dir);
    if IsNan(T) or IsInfinite(T) or (T < -WallEpsilon) or (T > Len + WallEpsilon) or
      (T - Previous <= WallEpsilon) or
      (WallDistance(P, Along(Candidate.Center.First, Dir, T)) >
        Max(WallEpsilon, Candidate.Thickness * WallAxisTolerance)) then
      Exit;
    Previous := T;
  end;
  Result := True;
end;

procedure SetExtent(var C: TWallCandidate; Lo, Hi: Double);
var
  I: Integer;
  Edge: TWallAxis;
begin
  for I := 0 to 1 do begin
    Edge := C.Edges[I];
    C.Edges[I].First := Interpolate(Edge, Lo);
    C.Edges[I].Last := Interpolate(Edge, Hi);
  end;
  C.Center.First := Along(C.Edges[0].First,
    Delta(C.Edges[0].First, C.Edges[1].First), 0.5);
  C.Center.Last := Along(C.Edges[0].Last,
    Delta(C.Edges[0].Last, C.Edges[1].Last), 0.5);
  C.Overlap := C.Overlap * (Hi - Lo);
  C.Thickness := (WallDistance(C.Edges[0].First, C.Edges[1].First) +
    WallDistance(C.Edges[0].Last, C.Edges[1].Last)) * 0.5;
end;

function MatchPair(const A, B: TWallSegment; Minimum, Maximum: Double;
  out C: TWallCandidate; out Outside: Boolean): Boolean;
var
  Dir, D: TWallPoint;
  Cosine, Norm, B0, B1, A1, Lo, Hi, EdgeMin, EdgeMax, T: Double;
begin
  Result := False;
  Outside := False;
  Cosine := Dot(A.Direction, B.Direction);
  C.Angle := ArcCos(Min(1.0, Abs(Cosine)));
  if C.Angle > WallAngleTolerance then
    Exit;
  if Cosine >= 0 then T := 1 else T := -1;
  D := Along(A.Direction, B.Direction, T);
  Norm := Sqrt(Dot(D, D));
  Dir := WallPoint(D.X / Norm, D.Y / Norm, D.Z / Norm);

  A1 := A.Length * Dot(A.Direction, Dir);
  B0 := Dot(Delta(A.Axis.First, B.Axis.First), Dir);
  B1 := Dot(Delta(A.Axis.First, B.Axis.Last), Dir);
  Lo := Max(0.0, Min(B0, B1));
  Hi := Min(A1, Max(B0, B1));
  C.Overlap := Hi - Lo;
  if C.Overlap < WallMinOverlap then
    Exit;
  C.Offset := Abs(Min(B0, B1));
  C.Edges[0].First := Interpolate(A.Axis, Lo / A1);
  C.Edges[0].Last := Interpolate(A.Axis, Hi / A1);
  C.Edges[1].First := Interpolate(B.Axis, (Lo - B0) / (B1 - B0));
  C.Edges[1].Last := Interpolate(B.Axis, (Hi - B0) / (B1 - B0));

  // Для почти параллельных граней проверяем весь общий интервал.
  D := Delta(C.Edges[0].First, C.Edges[1].First);
  Dir := Delta(C.Edges[0].Last, C.Edges[1].Last);
  EdgeMax := Max(Sqrt(Dot(D, D)), Sqrt(Dot(Dir, Dir)));
  Dir := Delta(D, Dir);
  T := Dot(Dir, Dir);
  if T > 0 then
    D := Along(D, Dir, EnsureRange(-Dot(D, Dir) / T, 0.0, 1.0));
  EdgeMin := Sqrt(Dot(D, D));
  Outside := (EdgeMin < Minimum - WallEpsilon * Minimum) or
    (EdgeMax > Maximum + WallEpsilon * Maximum);
  if Outside then
    Exit;
  SetExtent(C, 0, 1);
  Result := (C.Thickness >= Minimum) and (C.Thickness <= Maximum);
  Outside := not Result;
end;

function SameLine(const A, B: TWallAxis): Boolean;
var
  Dir: TWallPoint;
  Len, T: Double;
begin
  Result := False;
  if not WallDirection(A, Dir, Len) then
    Exit;
  T := Dot(Delta(A.First, B.First), Dir);
  if not WallNear(B.First, Along(A.First, Dir, T)) then
    Exit;
  T := Dot(Delta(A.First, B.Last), Dir);
  Result := WallNear(B.Last, Along(A.First, Dir, T));
end;

function CoveredInterval(const A, B: TWallCandidate; out Lo, Hi: Double): Boolean;
var
  Dir: TWallPoint;
  Len, T0, T1: Double;
begin
  Result := False;
  if not ((SameLine(A.Edges[0], B.Edges[0]) and SameLine(A.Edges[1], B.Edges[1])) or
    (SameLine(A.Edges[0], B.Edges[1]) and SameLine(A.Edges[1], B.Edges[0]))) then
    Exit;
  if not WallDirection(A.Center, Dir, Len) then
    Exit;
  T0 := Dot(Delta(A.Center.First, B.Center.First), Dir);
  T1 := Dot(Delta(A.Center.First, B.Center.Last), Dir);
  Lo := Max(0.0, Min(T0, T1) / Len);
  Hi := Min(1.0, Max(T0, T1) / Len);
  Result := (Hi - Lo) * Len > WallEpsilon;
end;

function FindWallCandidates(const Segments: TWallSegments; Minimum, Maximum: Double;
  out Stats: TWallStatistics): TWallCandidates;
type
  TSweepEntry = record
    Index: Integer;
    Lo, Hi: Double;
  end;
  TInterval = record
    Lo, Hi: Double;
  end;
var
  Sweep: array of TSweepEntry;
  Remaining: array of TInterval;
  I, J, K, R, Count, Axis: Integer;
  Candidate, Part: TWallCandidate;
  Lo, Hi, MinX, MinY, MinZ, MaxX, MaxY, MaxZ: Double;
  Outside: Boolean;

  function Coordinate(const P: TWallPoint): Double;
  begin
    case Axis of
      0: Result := P.X;
      1: Result := P.Y;
      else Result := P.Z;
    end;
  end;

  procedure Sort(L, H: Integer);
  var
    A, B: Integer;
    Pivot, Temp: TSweepEntry;
  begin
    A := L;
    B := H;
    Pivot := Sweep[L + (H - L) div 2];
    repeat
      while (Sweep[A].Lo < Pivot.Lo) or
        ((Sweep[A].Lo = Pivot.Lo) and (Sweep[A].Index < Pivot.Index)) do Inc(A);
      while (Sweep[B].Lo > Pivot.Lo) or
        ((Sweep[B].Lo = Pivot.Lo) and (Sweep[B].Index > Pivot.Index)) do Dec(B);
      if A <= B then begin
        Temp := Sweep[A];
        Sweep[A] := Sweep[B];
        Sweep[B] := Temp;
        Inc(A);
        Dec(B);
      end;
    until A > B;
    if L < B then Sort(L, B);
    if A < H then Sort(A, H);
  end;

begin
  Result := nil;
  Stats := Default(TWallStatistics);
  if (Length(Segments) < 2) or not ValidWallThickness(Minimum, Maximum) then
    Exit;
  MinX := Infinity;
  MinY := Infinity;
  MinZ := Infinity;
  MaxX := NegInfinity;
  MaxY := NegInfinity;
  MaxZ := NegInfinity;
  for I := 0 to High(Segments) do
    with Segments[I].Axis do begin
      MinX := Min(MinX, Min(First.X, Last.X));
      MinY := Min(MinY, Min(First.Y, Last.Y));
      MinZ := Min(MinZ, Min(First.Z, Last.Z));
      MaxX := Max(MaxX, Max(First.X, Last.X));
      MaxY := Max(MaxY, Max(First.Y, Last.Y));
      MaxZ := Max(MaxZ, Max(First.Z, Last.Z));
    end;
  Axis := 0;
  if MaxY - MinY > MaxX - MinX then Axis := 1;
  if MaxZ - MinZ > Max(MaxX - MinX, MaxY - MinY) then Axis := 2;
  SetLength(Sweep, Length(Segments));
  for I := 0 to High(Segments) do begin
    Sweep[I].Index := I;
    Lo := Coordinate(Segments[I].Axis.First);
    Hi := Coordinate(Segments[I].Axis.Last);
    Sweep[I].Lo := Min(Lo, Hi);
    Sweep[I].Hi := Max(Lo, Hi);
  end;
  Sort(0, High(Sweep));

  Count := 0;
  for I := 0 to High(Sweep) do
    for J := I + 1 to High(Sweep) do begin
      if Sweep[J].Lo - Sweep[I].Hi > Maximum * (1 + WallEpsilon) then begin
        Inc(Stats.Rejected, Length(Sweep) - J);
        Break;
      end;
      if not MatchPair(Segments[Sweep[I].Index], Segments[Sweep[J].Index],
        Minimum, Maximum, Candidate, Outside) then begin
        if Outside then Inc(Stats.OutsideThickness);
        Inc(Stats.Rejected);
        Continue;
      end;
      Inc(Stats.Eligible);
      Candidate.Segments[0] := Sweep[I].Index;
      Candidate.Segments[1] := Sweep[J].Index;

      // Убираем покрытые интервалы совпадающих граней, сохраняя два
      // фактических источника для каждой оставшейся части.
      SetLength(Remaining, 1);
      Remaining[0].Lo := 0;
      Remaining[0].Hi := 1;
      for K := 0 to Count - 1 do
        if CoveredInterval(Candidate, Result[K], Lo, Hi) then
          for R := High(Remaining) downto 0 do begin
            if (Hi <= Remaining[R].Lo) or (Lo >= Remaining[R].Hi) then
              Continue;
            if (Lo > Remaining[R].Lo) and (Hi < Remaining[R].Hi) then begin
              SetLength(Remaining, Length(Remaining) + 1);
              Remaining[High(Remaining)].Lo := Hi;
              Remaining[High(Remaining)].Hi := Remaining[R].Hi;
            end;
            if Lo > Remaining[R].Lo then
              Remaining[R].Hi := Lo
            else
              Remaining[R].Lo := Hi;
          end;
      K := Count;
      for R := 0 to High(Remaining) do begin
        if (Remaining[R].Hi - Remaining[R].Lo) * Candidate.Overlap < WallMinOverlap then
          Continue;
        Part := Candidate;
        SetExtent(Part, Remaining[R].Lo, Remaining[R].Hi);
        if (Part.Thickness < Minimum) or (Part.Thickness > Maximum) then
          Continue;
        if Count = Length(Result) then
          SetLength(Result, 2 * Count + 16);
        Result[Count] := Part;
        if Count = 0 then begin
          Stats.MinFound := Part.Thickness;
          Stats.MaxFound := Part.Thickness;
        end else begin
          Stats.MinFound := Min(Stats.MinFound, Part.Thickness);
          Stats.MaxFound := Max(Stats.MaxFound, Part.Thickness);
        end;
        Inc(Count);
      end;
      if K = Count then Inc(Stats.Rejected);
    end;
  SetLength(Result, Count);
  // При пустом результате отличаем отсутствие перекрывающихся
  // параллельных линий от пар, исключенных по толщине ранним отсевом.
  if (Count = 0) and (Stats.OutsideThickness = 0) then
    for I := 0 to High(Sweep) do
      for J := I + 1 to High(Sweep) do
        if Sweep[J].Lo - Sweep[I].Hi > Maximum * (1 + WallEpsilon) then begin
          MatchPair(Segments[Sweep[I].Index], Segments[Sweep[J].Index],
            Minimum, Maximum, Candidate, Outside);
          if Outside then begin
            Inc(Stats.OutsideThickness);
            Exit;
          end;
        end;
end;

end.
