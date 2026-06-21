# Reproduction for issue #1365 — uzvspreadsheet compile error

`uzvspreadsheet_dimensions.pas` uses both `fpsTypes` (TsHorAlignment /
TsVertAlignment) and `uzeacadtable_types`. The latter declares
`THorzAlign = (haLeft, haCenter, haRight)` and
`TVertAlign = (vaTop, vaMiddle, vaBottom)`, whose member names collide with
the fpspreadsheet constants. Because `uzeacadtable_types` is listed last in
the `uses` clause, the unqualified `haCenter` / `haRight` / `vaBottom` case
labels resolve to the acadtable enum, producing:

    Error: Constant and CASE types do not match
    Error: Duplicate case label   (vaBottom ord 2 == vaCenter ord 2)

## Run

    fpc -Mobjfpc repro_bug.pas      # fails with the issue's errors
    fpc -Mobjfpc repro_fixed.pas && ./repro_fixed   # PASS

The fix qualifies the constants with the `fpsTypes` unit name
(`fpsTypes.haCenter`, ...), matching what was applied in the source file.
