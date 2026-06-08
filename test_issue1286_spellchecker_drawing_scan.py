#!/usr/bin/env python3
"""
Regression checks for issue 1286 spellchecker drawing scan.
"""

from pathlib import Path


ROOT = Path(__file__).resolve().parent
SPELL_FORM = (
    ROOT
    / "cad_source"
    / "zcad"
    / "velec"
    / "uzvfspellchecker"
    / "uzvfspellform.pas"
)
SPELL_REGISTER = ROOT / "cad_source" / "zcad" / "register" / "uzcregspellchecker.pas"


def read_text(path: Path) -> str:
    return path.read_text(encoding="utf-8")


def extract_procedure(source: str, procedure_name: str) -> str:
    marker = f"procedure {procedure_name}"
    start = source.index(marker)
    candidates = [
        position
        for position in (
            source.find("\nprocedure ", start + len(marker)),
            source.find("\nfunction ", start + len(marker)),
        )
        if position != -1
    ]
    end = min(candidates) if candidates else len(source)
    return source[start:end]


def test_spellchecker_form_exposes_current_drawing_scan():
    form_pas = read_text(SPELL_FORM)

    assert "procedure CheckCurrentDrawing;" in form_pas
    assert "uzcdrawings" in form_pas
    assert "uzeentity" in form_pas
    assert "uzeenttext" in form_pas
    assert "uzeconsts" in form_pas
    assert "gzctnrVectorTypes" in form_pas


def test_refresh_action_scans_drawing_and_logs_even_without_loaded_text():
    form_pas = read_text(SPELL_FORM)
    refresh_proc = extract_procedure(
        form_pas, "TSpellCheckerForm.RefreshActionExecute"
    )

    assert "CheckCurrentDrawing;" in refresh_proc
    assert "Length(FCurrentText) > 0" not in refresh_proc
    assert "TSpellCheckerForm.RefreshActionExecute: refresh requested" in refresh_proc


def test_current_drawing_scan_extracts_text_and_mtext_content():
    form_pas = read_text(SPELL_FORM)

    assert "GetCurrentDWG^.GetCurrentROOT^.ObjArray.beginiterate" in form_pas
    assert "GetCurrentDWG^.GetCurrentROOT^.ObjArray.iterate" in form_pas
    assert "GDBTextID" in form_pas
    assert "GDBMTextID" in form_pas
    assert "PGDBObjText(EntityPtr)^.Content" in form_pas
    assert "PGDBObjText(EntityPtr)^.Template" in form_pas


def test_current_drawing_scan_writes_diagnostic_log_entries():
    form_pas = read_text(SPELL_FORM)

    assert "TSpellCheckerForm.CheckCurrentDrawing: start" in form_pas
    assert (
        "TSpellCheckerForm.CheckCurrentDrawing: scanned entities=%d, "
        in form_pas
    )
    assert (
        "TSpellCheckerForm.LoadCurrentDrawingText: text entity type=%d, "
        in form_pas
    )
    assert "TSpellCheckerForm.ResetResultsWithMessage:" in form_pas


def test_registered_form_setup_runs_initial_drawing_scan():
    register_pas = read_text(SPELL_REGISTER)

    assert "uzclog" in register_pas
    assert "TSpellCheckerForm(Form).CheckCurrentDrawing" in register_pas
    assert "uzvfspellcheckerSetupProc: setup started" in register_pas


if __name__ == "__main__":
    test_spellchecker_form_exposes_current_drawing_scan()
    test_refresh_action_scans_drawing_and_logs_even_without_loaded_text()
    test_current_drawing_scan_extracts_text_and_mtext_content()
    test_current_drawing_scan_writes_diagnostic_log_entries()
    test_registered_form_setup_runs_initial_drawing_scan()
    print("issue 1286 spellchecker drawing scan checks passed")
