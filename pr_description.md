## Summary

Fixes veb86/zcadvelecAI#1163.

- Treat LibreDWG read codes that contain critical bits as fatal in the ZCAD DWG/DXF loader.
- Share the same critical-code mask with the fpdwg inspector instead of keeping separate implementations.
- Decode LibreDWG read-code bit flags in diagnostics, including the issue log code `2368` as `DWG_ERR_VALUEOUTOFBOUNDS,DWG_ERR_SECTIONNOTFOUND,DWG_ERR_INVALIDDWG`.
- Add extra loader diagnostics for raw DWG counts, registered handles, pending owners, and pending refs so partial/failed loads are easier to investigate.

## Reproduction

1. Load the DWG 2018 file from the issue log, where LibreDWG returns read code `2368`.
2. Before this change, the loader printed `Success: 2368`, treated the result as recoverable, and continued into parsing an unusable partial DWG structure that built zero entities.
3. After this change, the loader logs the decoded critical flags and aborts parsing before owner/entity resolution, preserving DWG counts for diagnosis.

## Testing

- Added `TFPDWGProcReadCodeTest.Issue1163ReadCode2368IsCritical`.
- Added `TFPDWGProcReadCodeTest.NonCriticalReadCodeStaysLoadable`.
- `git diff --check`
- `make checkvars`
- Pascal test execution was not run locally because this workspace does not have `fpc` or `lazbuild` installed.
