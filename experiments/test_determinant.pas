program test_determinant;
{$mode objfpc}{$H+}
uses
  SysUtils, Math;

function MatrixDetInternal(const a1, a2, a3, b1, b2, b3, c1, c2, c3: Double): Double; inline;
begin
  Result := a1 * (b2 * c3 - b3 * c2) -
            b1 * (a2 * c3 - a3 * c2) +
            c1 * (a2 * b3 - a3 * b2);
end;

procedure TestDeterminant;
var
  det: Double;
begin
  Writeln('Testing determinant calculations:');

  // 90° rotation matrix
  det := MatrixDetInternal(0, -1, 0,
                          1,  0, 0,
                          0,  0, 1);
  Writeln(Format('90° rotation det: %.6f (expected: 1.0)', [det]));

  // 180° rotation matrix
  det := MatrixDetInternal(-1, 0, 0,
                           0, -1, 0,
                           0,  0, 1);
  Writeln(Format('180° rotation det: %.6f (expected: 1.0)', [det]));

  // X mirror matrix
  det := MatrixDetInternal(-1, 0, 0,
                           0,  1, 0,
                           0,  0, 1);
  Writeln(Format('X mirror det: %.6f (expected: -1.0)', [det]));

  // Y=X mirror matrix
  det := MatrixDetInternal(0, 1, 0,
                          1, 0, 0,
                          0, 0, 1);
  Writeln(Format('Y=X mirror det: %.6f (expected: -1.0)', [det]));

  // Scale 2x matrix
  det := MatrixDetInternal(2, 0, 0,
                          0, 2, 0,
                          0, 0, 2);
  Writeln(Format('Scale 2x det: %.6f (expected: 8.0)', [det]));
end;

begin
  TestDeterminant;
end.