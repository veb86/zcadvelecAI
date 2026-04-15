## Summary
- restrict generic `MTEXT` wrapping to word boundaries by default
- enable word-then-character wrapping explicitly for AcadTable cell text
- add unit tests covering both plain MTEXT and table-cell wrapping behavior

## Reproduction
- open `cad_source\test+testtable.dxf`
- inspect the table cell containing `35` in row 2, column 3
- before this change, long unspaced text used generic MTEXT character wrapping everywhere, which caused incorrect behavior outside the table-specific case

## Testing
- added `TMTextWrapTest` in `cad_source/zengine/tests/uzctmtextwrap.pas`
- local execution of `cad_source/zengine/tests` is blocked in this environment because `lazbuild` is not installed (`/workspace/lazarus/lazbuild: not found`)

Fixes veb86/zcadvelecAI#895
