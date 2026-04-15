## Summary

Fixes `veb86/zcadvelecAI#898` by adding Proxy Graphic support for `OpCode=32` (`PolylineWithNormals`), which restores the shelf/divider line inside the proxy-rendered multileader block from `cad_source/test/mleaderblock.dxf`.

## Reproduction

1. Open `cad_source/test/mleaderblock.dxf`.
2. Inspect the proxy-rendered multileader block.
3. Before this change, the horizontal shelf/divider line inside the block is missing.

## Changes

- registered a new proxy parser module for `OpCode=32` in `uzeentacdproxy`
- added `uzeentproxyparserpolylinewithnormals.pas` to read the vertex list and trailing normal vector from Proxy Graphic data
- added a regression test in `uzctentproxy` that parses the proxy graphic from `mleaderblock.dxf` and asserts the expected shelf segment exists
- kept the parser behavior aligned with `ezdxf`: vertices are rendered as a polyline and the shared normal is currently ignored

## Testing

- Added automated regression coverage in `cad_source/zengine/tests/uzctentproxy.pas`
- Local compile/test execution could not be completed in this environment because `fpc` is not installed (`/bin/bash: line 1: fpc: command not found`)
