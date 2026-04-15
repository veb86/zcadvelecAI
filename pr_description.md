## Summary
Fixes AcadTable cell text styles loaded from DXF table styles.

## Root Cause
`GDBObjAcadTable.ResolveCellStyle` correctly resolved the per-row DXF table text style (`title`, `header`, `data`), but `BuildVisualRepresentation` ignored that result and assigned every generated `MText` the first text style in the drawing. As a result, all cells rendered with the same style, typically `Standard`.

## Changes
- resolve `MText.TXTStyle` from the style name calculated for each cell
- keep a fallback chain: requested style -> `Standard` -> first available text style
- add a regression test that loads `cad_source/test/tablerazdel.dxf` and checks that table text uses multiple styles, including `newtext`

## Verification
- Added automated regression test in `cad_source/zengine/tests/uzctacadtable.pas`
- Local test execution is blocked in this environment because the repo test Makefile requires Lazarus `lazbuild` at `/workspace/lazarus/lazbuild`, which is not installed here

Fixes veb86/zcadvelecAI#902
