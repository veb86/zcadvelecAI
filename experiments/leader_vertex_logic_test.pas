program leader_vertex_logic_test;
{$Mode delphi}{$H+}
{ Standalone, runnable proof of the core vertex-management algorithm used by
  the LEADER command (uzccommand_leader.RebuildLeaderVertices + the Undo menu
  item). It reproduces the exact point-sequencing logic against the real
  GDBPoint3dArray container, without the graphics/style chain that the full
  fpcunit suite (uzctentleader) needs to format the entity.

  Run:  fpc <paths> experiments/leader_vertex_logic_test.pas && ./leader_vertex_logic_test
  Exit code 0 == PASS. }
uses
  SysUtils, Math, uzegeometrytypes, uzegeometry, UGDBPoint3DArray;

var
  Failures: Integer = 0;

procedure Check(Cond: Boolean; const Msg: string);
begin
  if Cond then
    writeln('  ok   - ', Msg)
  else
  begin
    writeln('  FAIL - ', Msg);
    Inc(Failures);
  end;
end;

{ Vertices the command would hand to the entity: every user point, plus the
  live cursor point while still picking (AddPreview). Mirrors
  uzccommand_leader.RebuildLeaderVertices verbatim, but writes into a plain
  array so the test is self-contained. }
procedure RebuildLeaderVertices(var UserPoints: GDBPoint3dArray;
                                var Output: GDBPoint3dArray;
                                AddPreview: Boolean; const PreviewPoint: TzePoint3d);
var
  i: Integer;
begin
  Output.Clear;
  for i := 0 to UserPoints.Count - 1 do
    Output.PushBackData(UserPoints.getDataMutable(i)^);
  if AddPreview then
    Output.PushBackData(PreviewPoint);
end;

var
  UserPoints, Vertices: GDBPoint3dArray;
  V: PzePoint3d;
begin
  UserPoints.init(16);
  Vertices.init(16);

  // Two source points entered by the user (the first two prompts).
  UserPoints.PushBackData(CreateVertex(0, 0, 0));
  UserPoints.PushBackData(CreateVertex(10, 0, 0));

  // Preview: cursor at (5,5) adds a temporary third vertex.
  RebuildLeaderVertices(UserPoints, Vertices, True, CreateVertex(5, 5, 0));
  Check(Vertices.Count = 3, 'preview adds a temporary cursor vertex (2+1)');
  V := Vertices.getDataMutable(2);
  Check(SameValue(V^.x, 5.0) and SameValue(V^.y, 5.0), 'last vertex is the cursor point');

  // User clicks a third point (14,0); preview continues at (20,20).
  UserPoints.PushBackData(CreateVertex(14, 0, 0));
  RebuildLeaderVertices(UserPoints, Vertices, True, CreateVertex(20, 20, 0));
  Check(Vertices.Count = 4, 'after a click: 3 user points + preview');

  // Undo menu item removes the last entered point.
  UserPoints.DeleteElement(UserPoints.Count - 1);
  Check(UserPoints.Count = 2, 'Undo removes the last user point');

  // Command finish rebuilds with no preview point.
  RebuildLeaderVertices(UserPoints, Vertices, False, NulVertex);
  Check(Vertices.Count = 2, 'final leader is built from user points only');
  V := Vertices.getDataMutable(1);
  Check(SameValue(V^.x, 10.0) and SameValue(V^.y, 0.0), 'last vertex after Undo is (10,0)');

  writeln;
  if Failures = 0 then
  begin
    writeln('PASS: leader vertex/undo logic');
    Halt(0);
  end
  else
  begin
    writeln('FAILED: ', Failures, ' check(s)');
    Halt(1);
  end;
end.
