{ Minimal stub mirroring uzeacadtable_types: declares the SAME identifiers
  haLeft/haCenter/haRight and vaTop/vaBottom that collide with fpsTypes. }
unit acadtable_stub;
{$mode objfpc}{$H+}
interface
type
  THorzAlign = (haLeft, haCenter, haRight);
  TVertAlign = (vaTop, vaMiddle, vaBottom);
implementation
end.
