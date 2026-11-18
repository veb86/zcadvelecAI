unit uzvhttpwallcollector;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  uzeentity, uzeentgenericsubentry, uzegeometrytypes, uzvhttpwallgeometry;

const
  WallPreviewLayer = 'ZCAD_AI_WALL_PREVIEW';

type
  TWallSource = record
    Entity: PGDBObjEntity;
    Handle, Signature: string;
    TemporaryHandle: Boolean;
  end;
  TWallSources = array of TWallSource;
  TWallSourceFilter = function(Entity: PGDBObjEntity): Boolean;

function ToWallPoint(const P: TzePoint3d): TWallPoint;
function ToCADPoint(const P: TWallPoint): TzePoint3d;
function SourceSignature(Entity: PGDBObjEntity): string;
procedure CollectWallSegments(Root: PGDBObjGenericSubEntry; const Scope: TWallScope;
  IgnoreSource: TWallSourceFilter; out Sources: TWallSources; out Segments: TWallSegments);
function WallSourcesUnchanged(Root: PGDBObjGenericSubEntry;
  const Sources: TWallSources; const Segments: TWallSegments;
  const Candidates: TWallCandidates): Boolean;

implementation

uses
  Classes, SysUtils, Math, MD5, gzctnrVectorTypes, uzeconsts, uzeentline,
  uzeentlwpolyline, uzestyleslayers;

function ToWallPoint(const P: TzePoint3d): TWallPoint;
begin
  Result := WallPoint(P.X, P.Y, P.Z);
end;

function ToCADPoint(const P: TWallPoint): TzePoint3d;
begin
  Result := TzePoint3d.Make(P.X, P.Y, P.Z);
end;

function OriginalHandle(Entity: PGDBObjEntity): string;
begin
  Result := '';
  if (Entity^.PExtAttrib <> nil) and (Entity^.PExtAttrib^.dwgHandle <> 0) then
    Result := IntToHex(Entity^.PExtAttrib^.dwgHandle, 1);
end;

function EntityLayer(Entity: PGDBObjEntity): string;
begin
  Result := '';
  if Entity^.vp.Layer <> nil then
    Result := Entity^.vp.Layer^.Name;
end;

function SourceSignature(Entity: PGDBObjEntity): string;
var
  Hash: TMD5Context;
  Digest: TMD5Digest;
  S: string;
  P: PGDBObjLWPolyline;
  L: GDBLineProp;
  Point: TWallPoint;
  I, N: Integer;
  Width, D: Double;
begin
  MD5Init(Hash);
  S := EntityLayer(Entity);
  if S <> '' then MD5Update(Hash, S[1], Length(S));
  S := OriginalHandle(Entity);
  if S <> '' then MD5Update(Hash, S[1], Length(S));
  case Entity^.GetObjType of
    GDBLineID: begin
      L := PGDBObjLine(Entity)^.getCoordInWCS;
      Point := ToWallPoint(L.lBegin);
      MD5Update(Hash, Point, SizeOf(Point));
      Point := ToWallPoint(L.lEnd);
      MD5Update(Hash, Point, SizeOf(Point));
    end;
    GDBLWPolylineID: begin
      P := PGDBObjLWPolyline(Entity);
      MD5Update(Hash, P^.Closed, SizeOf(P^.Closed));
      N := P^.Vertex3D_in_WCS_Array.Count;
      MD5Update(Hash, N, SizeOf(N));
      for I := 0 to N - 1 do begin
        Point := ToWallPoint(P^.Vertex3D_in_WCS_Array.getDataMutable(I)^);
        MD5Update(Hash, Point, SizeOf(Point));
      end;
      for I := 0 to P^.SgmntsParams.Count - 1 do
        with P^.SgmntsParams.getDataMutable(I)^.data do begin
          D := bulge;
          MD5Update(Hash, D, SizeOf(D));
          Width := startw;
          MD5Update(Hash, Width, SizeOf(Width));
          Width := endw;
          MD5Update(Hash, Width, SizeOf(Width));
          MD5Update(Hash, hw, SizeOf(hw));
        end;
    end;
    else Exit('');
  end;
  MD5Final(Hash, Digest);
  Result := MD5Print(Digest);
end;

function InScope(Entity: PGDBObjEntity; const Scope: TWallScope;
  const Handle: string): Boolean;
var
  I: Integer;
begin
  case Scope.Kind of
    wskSelection: Result := Entity^.Selected;
    wskWindow:
      with Entity^.vp.BoundingBox, Scope.Window do
        Result := (pMin.X >= Min(First.X, Last.X) - WallEpsilon) and
          (pMax.X <= Max(First.X, Last.X) + WallEpsilon) and
          (pMin.Y >= Min(First.Y, Last.Y) - WallEpsilon) and
          (pMax.Y <= Max(First.Y, Last.Y) + WallEpsilon);
    wskHandles: begin
      for I := 0 to High(Scope.Handles) do
        if SameText(Handle, Scope.Handles[I]) then Exit(True);
      Result := False;
    end;
  end;
end;

procedure CollectWallSegments(Root: PGDBObjGenericSubEntry; const Scope: TWallScope;
  IgnoreSource: TWallSourceFilter; out Sources: TWallSources; out Segments: TWallSegments);
var
  Entity: PGDBObjEntity;
  Poly: PGDBObjLWPolyline;
  Line: GDBLineProp;
  Iterator: itrec;
  I, J, HandleIndex, Edges, VertexCount, SourceCount, SegmentCount: Integer;
  Handle, Layer: string;
  UsedHandles: TStringList;
  NextHandle: QWord;
  Source: TWallSource;

  procedure AddSegment(const A, B: TzePoint3d; Index: Integer);
  var
    S: TWallSegment;
  begin
    S.Axis.First := ToWallPoint(A);
    S.Axis.Last := ToWallPoint(B);
    if not FiniteWallPoint(S.Axis.First) or not FiniteWallPoint(S.Axis.Last) or
      not WallDirection(S.Axis, S.Direction, S.Length) then Exit;
    S.Source := SourceCount;
    S.Handle := Source.Handle;
    S.Layer := Layer;
    S.TemporaryHandle := Source.TemporaryHandle;
    if Entity^.GetObjType = GDBLineID then S.EntityType := 'LINE'
    else S.EntityType := 'LWPOLYLINE';
    S.SegmentIndex := Index;
    if SegmentCount = Length(Segments) then
      SetLength(Segments, 2 * SegmentCount + 32);
    Segments[SegmentCount] := S;
    Inc(SegmentCount);
  end;

begin
  Sources := nil;
  Segments := nil;
  SourceCount := 0;
  SegmentCount := 0;
  UsedHandles := TStringList.Create;
  try
    UsedHandles.Sorted := True;
    UsedHandles.Duplicates := dupIgnore;
    Entity := Root^.ObjArray.beginiterate(Iterator);
    while Entity <> nil do begin
      Handle := OriginalHandle(Entity);
      if Handle <> '' then
        if UsedHandles.Find(Handle, HandleIndex) then
          UsedHandles.Objects[HandleIndex] := TObject(PtrUInt(1))
        else
          UsedHandles.Add(Handle);
      Entity := Root^.ObjArray.iterate(Iterator);
    end;

    NextHandle := High(QWord);
    Entity := Root^.ObjArray.beginiterate(Iterator);
    while Entity <> nil do begin
      if (Entity^.GetObjType in [GDBLineID, GDBLWPolylineID]) and
        not IgnoreSource(Entity) then begin
        Layer := EntityLayer(Entity);
        Handle := OriginalHandle(Entity);
        Source.TemporaryHandle := (Handle = '') or
          (UsedHandles.Find(Handle, HandleIndex) and (UsedHandles.Objects[HandleIndex] <> nil));
        if Source.TemporaryHandle then begin
          repeat
            Handle := IntToHex(NextHandle, 16);
            Dec(NextHandle);
          until UsedHandles.IndexOf(Handle) < 0;
        end;
        if InScope(Entity, Scope, Handle) then begin
          Source.Entity := Entity;
          Source.Handle := Handle;
          I := SegmentCount;
          case Entity^.GetObjType of
            GDBLineID: begin
              Line := PGDBObjLine(Entity)^.getCoordInWCS;
              AddSegment(Line.lBegin, Line.lEnd, 0);
            end;
            GDBLWPolylineID: begin
              Poly := PGDBObjLWPolyline(Entity);
              VertexCount := Poly^.Vertex3D_in_WCS_Array.Count;
              Edges := VertexCount - Ord(not Poly^.Closed);
              for J := 0 to Min(Edges, Poly^.SgmntsParams.Count) - 1 do
                if Poly^.SgmntsParams.getDataMutable(J)^.data.bulge = 0 then
                  AddSegment(Poly^.Vertex3D_in_WCS_Array.getDataMutable(J)^,
                    Poly^.Vertex3D_in_WCS_Array.getDataMutable((J + 1) mod VertexCount)^, J);
            end;
          end;
          if SegmentCount > I then begin
            Source.Signature := SourceSignature(Entity);
            if SourceCount = Length(Sources) then
              SetLength(Sources, 2 * SourceCount + 32);
            Sources[SourceCount] := Source;
            Inc(SourceCount);
          end;
        end;
      end;
      Entity := Root^.ObjArray.iterate(Iterator);
    end;
  finally
    UsedHandles.Free;
  end;
  SetLength(Segments, SegmentCount);
  SetLength(Sources, SourceCount);
end;

function WallSourcesUnchanged(Root: PGDBObjGenericSubEntry;
  const Sources: TWallSources; const Segments: TWallSegments;
  const Candidates: TWallCandidates): Boolean;
var
  Pending: TStringList;
  Required: array of Boolean;
  I, J, Index: Integer;
  Entity: PGDBObjEntity;
  Iterator: itrec;
begin
  SetLength(Required, Length(Sources));
  for I := 0 to High(Candidates) do
    for J := 0 to 1 do
      Required[Segments[Candidates[I].Segments[J]].Source] := True;
  Pending := TStringList.Create;
  try
    for I := 0 to High(Sources) do
      if Required[I] then
        Pending.AddObject(IntToHex(PtrUInt(Sources[I].Entity), SizeOf(Pointer) * 2),
          TObject(PtrInt(I)));
    Pending.Sorted := True;
    J := Pending.Count;
    Entity := Root^.ObjArray.beginiterate(Iterator);
    while (Entity <> nil) and (J > 0) do begin
      if Pending.Find(IntToHex(PtrUInt(Entity), SizeOf(Pointer) * 2), Index) then begin
        I := PtrInt(Pending.Objects[Index]);
        if SourceSignature(Entity) <> Sources[I].Signature then Exit(False);
        Dec(J);
      end;
      Entity := Root^.ObjArray.iterate(Iterator);
    end;
    Result := J = 0;
  finally
    Pending.Free;
  end;
end;

end.
