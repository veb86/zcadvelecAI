#!/usr/bin/env python3
"""Regression check for DWG loader timing instrumentation."""

from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]


def read_source(relative_path):
    return (ROOT / relative_path).read_text(encoding="utf-8")


def require_all(source, tokens):
    missing = [token for token in tokens if token not in source]
    assert not missing, "missing instrumentation tokens: " + ", ".join(missing)


def test_timing_log_helper_is_available():
    source = read_source("cad_source/zengine/fileformats/dwg/uzedwgtimerlog.pas")
    require_all(
        source,
        [
            "unit uzedwgtimerlog;",
            "DWG_TIMER_LOG_MODULE_NAME = 'DWGTIMER'",
            "procedure DWGTimerLogTiming",
            "DWG timing: phase=%s elapsed_ms=%d",
            "programlog.RegisterModule(DWG_TIMER_LOG_MODULE_NAME)",
        ],
    )


def test_regular_dwg_log_does_not_own_timing_output():
    source = read_source("cad_source/zengine/fileformats/dwg/uzedwglog.pas")
    assert "DWGLogTiming" not in source
    assert "DWG timing:" not in source


def test_timing_callers_use_dwgtimer_module():
    for relative_path in [
        "cad_source/zengine/fileformats/uzefflibredwg.pas",
        "cad_source/zengine/fileformats/dwg/uzedwgimport.pas",
        "cad_source/zengine/fileformats/dwg/uzedwgfinalize.pas",
    ]:
        source = read_source(relative_path)
        assert "uzedwgtimerlog" in source, relative_path
        assert "DWGTimerLogTiming(" in source or "DWGFinishTimer(" in source or "DWGTimerLogDone(" in source
        assert "DWGLogTiming(" not in source, relative_path
        assert "DWGLogTimerDone(" not in source, relative_path


def test_top_level_dwg_load_phases_are_timed():
    source = read_source("cad_source/zengine/fileformats/uzefflibredwg.pas")
    require_all(
        source,
        [
            "addfromdwg.load-libredwg",
            "addfromdwg.read-file",
            "addfromdwg.parse-data",
            "addfromdwg.free-data",
            "addfromdwg.total",
        ],
    )


def test_dwg_import_lifecycle_phases_are_timed():
    source = read_source("cad_source/zengine/fileformats/dwg/uzedwgimport.pas")
    require_all(
        source,
        [
            "dwg-import.begin",
            "dwg-import.scan.header",
            "dwg-import.scan.raw-objects",
            "dwg-import.resolve-refs",
            "dwg-import.resolve-owners",
            "dwg-import.diagnostics",
            "dwg-import.sidefiles",
            "dwg-import.finalize",
            "dwg-import.cleanup",
            "dwg-import.end-total",
        ],
    )


def test_finalize_subphases_are_timed():
    source = read_source("cad_source/zengine/fileformats/dwg/uzedwgfinalize.pas")
    require_all(
        source,
        [
            "dwg-finalize.owner-cache",
            "dwg-finalize.entity-loop",
            "dwg-finalize.insert-children",
            "dwg-finalize.total",
        ],
    )


def test_timing_call_count_stays_broad_enough():
    source = "\n".join(
        [
            read_source("cad_source/zengine/fileformats/uzefflibredwg.pas"),
            read_source("cad_source/zengine/fileformats/dwg/uzedwgimport.pas"),
            read_source("cad_source/zengine/fileformats/dwg/uzedwgfinalize.pas"),
        ]
    )
    timing_calls = (
        source.count("DWGTimerLogTiming(")
        + source.count("DWGFinishTimer(")
        + source.count("DWGTimerLogDone(")
    )
    assert timing_calls >= 20


if __name__ == "__main__":
    test_timing_log_helper_is_available()
    test_top_level_dwg_load_phases_are_timed()
    test_dwg_import_lifecycle_phases_are_timed()
    test_finalize_subphases_are_timed()
    test_timing_call_count_stays_broad_enough()
    print("DWG timing instrumentation checks passed")
