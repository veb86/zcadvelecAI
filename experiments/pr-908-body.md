## Summary
- fix AcadTable break properties in the object inspector so repeated selection no longer dereferences stale property storage
- rebuild the break-direction enum data on each inspector open to avoid duplicated or incorrect values
- add DXF regression coverage for break settings from tablerazdel2.dxf

## Verification
- code review of AcadTable multiproperty registration and per-entity extraction paths
- attempted local Pascal test build via `make -C cad_source/zengine/tests clean all`, but this environment does not provide `lazbuild` or `fpc`

## Issue Reference
Fixes veb86/zcadvelecAI#860
