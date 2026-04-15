## Summary
Fixes `veb86/zcadvelecAI#898` by adding Proxy Graphic support for `OpCode=32` (`PolylineWithNormals`), which restores the shelf/divider line inside the proxy-rendered multileader block from `cad_source/test/mleaderblock.dxf`.

Also includes the current `master` changes for `veb86/zcadvelecAI#895`, which restrict generic `MTEXT` wrapping to word boundaries and keep character-by-character wrapping only for AcadTable cell text.

## Reproduction

1. Open `cad_source/test/mleaderblock.dxf`.
2. Inspect the proxy-rendered multileader block.
3. Before this change, the horizontal shelf/divider line inside the block is missing.
4. Open `cad_source/test/testtable.dxf`.
5. Inspect the table cell wrapping behavior.
6. Before the `master` fix, long unspaced text was character-wrapped in generic MTEXT, not just in table cells.

## Changes

- registered a new proxy parser module for `OpCode=32` in `uzeentacdproxy`
- added `uzeentproxyparserpolylinewithnormals.pas` to read the vertex list and trailing normal vector from Proxy Graphic data
- added a regression test in `uzctentproxy` that parses the proxy graphic from `mleaderblock.dxf` and asserts the expected shelf segment exists
- kept the parser behavior aligned with `ezdxf`: vertices are rendered as a polyline and the shared normal is currently ignored
- merged the `master` MTEXT wrap-mode changes so generic MTEXT stays word-wrapped while table cells explicitly use word-then-character wrapping
- kept the `TMTextWrapTest` coverage from `master`

## Testing

- Added automated regression coverage in `cad_source/zengine/tests/uzctentproxy.pas`
- Added `TMTextWrapTest` in `cad_source/zengine/tests/uzctmtextwrap.pas`
- Local compile/test execution could not be completed in this environment because `fpc`/`lazbuild` are not installed
