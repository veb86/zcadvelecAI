{ Reproduces issue #1365: unqualified alignment constants resolve to the
  colliding acadtable_stub enum -> compile error. }
program repro_bug;
{$mode objfpc}{$H+}
uses fpstypes_stub, acadtable_stub;
function HorAlignToGroup(AHor: TsHorAlignment): Integer;
begin
  case AHor of
    haCenter: Result := 1;
    haRight:  Result := 2;
    else      Result := 0;
  end;
end;
function VertAlignToGroup(AVert: TsVertAlignment): Integer;
begin
  case AVert of
    vaCenter: Result := 1;
    vaBottom: Result := 2;
    else      Result := 0;
  end;
end;
begin
  WriteLn(HorAlignToGroup(haCenter), VertAlignToGroup(vaBottom));
end.
