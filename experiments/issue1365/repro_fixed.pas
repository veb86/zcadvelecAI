{ The fix from PR: qualify fpspreadsheet alignment constants with the unit name. }
program repro_fixed;
{$mode objfpc}{$H+}
uses fpstypes_stub, acadtable_stub;
function HorAlignToGroup(AHor: TsHorAlignment): Integer;
begin
  case AHor of
    fpstypes_stub.haCenter: Result := 1;
    fpstypes_stub.haRight:  Result := 2;
    else                    Result := 0;
  end;
end;
function VertAlignToGroup(AVert: TsVertAlignment): Integer;
begin
  case AVert of
    fpstypes_stub.vaCenter: Result := 1;
    fpstypes_stub.vaBottom: Result := 2;
    else                    Result := 0;
  end;
end;
begin
  if (HorAlignToGroup(fpstypes_stub.haCenter) = 1)
     and (HorAlignToGroup(fpstypes_stub.haRight) = 2)
     and (HorAlignToGroup(fpstypes_stub.haLeft) = 0)
     and (VertAlignToGroup(fpstypes_stub.vaCenter) = 1)
     and (VertAlignToGroup(fpstypes_stub.vaBottom) = 2)
     and (VertAlignToGroup(fpstypes_stub.vaTop) = 0) then
    WriteLn('PASS: all alignment groups map correctly')
  else
  begin
    WriteLn('FAIL'); Halt(1);
  end;
end.
