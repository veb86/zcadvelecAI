program test_bulge_copy;
{$mode delphi}{$H+}
uses
  dwg, dwgproc;
var
  LWP: Dwg_Entity_LWPOLYLINE;
  Props: TDWGLWPolylineProps;
  Points: array[0..1] of BITCODE_2RD;
  Bulges: array[0..1] of BITCODE_BD;
  ok: Boolean;
begin
  FillChar(LWP, SizeOf(LWP), 0);
  FillChar(Points, SizeOf(Points), 0);
  Bulges[0] := 0.5;
  Bulges[1] := -0.25;
  Points[0].x := 1.0; Points[0].y := 2.0;
  Points[1].x := 3.0; Points[1].y := 4.0;
  LWP.num_points := 2;
  LWP.points := @Points[0];
  LWP.num_bulges := 2;
  LWP.bulges := @Bulges[0];
  DWGCopyLWPolylineProps(LWP, Props);
  ok := (Length(Props.Vertices) = 2)
    and (Abs(Props.Vertices[0].Bulge - 0.5) < 1e-12)
    and (Abs(Props.Vertices[1].Bulge - (-0.25)) < 1e-12);
  writeln('v0.bulge=', Props.Vertices[0].Bulge:0:4,
    ' v1.bulge=', Props.Vertices[1].Bulge:0:4);
  if ok then
    writeln('RESULT: PASS')
  else begin
    writeln('RESULT: FAIL');
    Halt(1);
  end;
end.
