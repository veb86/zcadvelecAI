#!/usr/bin/env python3
"""Regression tests for issue #1292 spellchecker error zoom behavior."""

from pathlib import Path
import re


ROOT = Path(__file__).resolve().parent
SPELL_DIR = ROOT / "cad_source" / "zcad" / "velec" / "uzvfspellchecker"
FORM_PAS = SPELL_DIR / "uzvfspellform.pas"
FORM_LFM = SPELL_DIR / "uzvfspellform.lfm"
ZOOM_PAS = SPELL_DIR / "uzvfspellzoom.pas"


def read_text(path: Path) -> str:
    return path.read_text(encoding="utf-8", errors="ignore")


def extract_routine(source: str, routine_name: str) -> str:
    pattern = (
        rf"procedure\s+{re.escape(routine_name)}\b.*?"
        rf"(?=\nprocedure\s+|\nfunction\s+|\ninitialization|\nfinalization|\Z)"
    )
    match = re.search(pattern, source, flags=re.IGNORECASE | re.DOTALL)
    assert match, f"{routine_name} routine is missing"
    return match.group(0)


def test_spellchecker_zoom_helper_is_separate_unit() -> None:
    assert ZOOM_PAS.exists(), "spell error zoom logic must live in a separate unit"

    zoom_source = read_text(ZOOM_PAS)
    assert "unit uzvfspellzoom;" in zoom_source
    assert "uzvfspelldata" in zoom_source
    assert "function ZoomToSpellError" in zoom_source
    assert "function TryGetSpellErrorWordVolume" in zoom_source


def test_errors_tree_double_click_invokes_zoom_without_applying_suggestions() -> None:
    form_source = read_text(FORM_PAS)
    lfm_source = read_text(FORM_LFM)

    assert "uzvfspellzoom" in form_source
    assert "procedure ErrorsTreeDblClick(Sender: TObject);" in form_source
    assert "OnDblClick = ErrorsTreeDblClick" in lfm_source

    handler = extract_routine(form_source, "TSpellCheckerForm.ErrorsTreeDblClick")
    assert "ZoomToSpellError(GetFocusedError)" in handler
    assert "ReplaceCurrentErrorWithSuggestion" not in handler
    assert "ApplySelectedSuggestion" not in handler


def test_zoom_uses_existing_view_area_zoom_to_volume_without_selection_side_effects() -> None:
    zoom_source = read_text(ZOOM_PAS)

    assert "drawings.GetCurrentDWG^.wa.ZoomToVolume" in zoom_source
    assert "ZoomSel" not in zoom_source
    assert "GetSelObjArray" not in zoom_source


def test_mtext_word_volume_uses_error_position_and_rendered_line_layout() -> None:
    zoom_source = read_text(ZOOM_PAS)

    assert "PGDBObjMText" in zoom_source
    assert "EntityPosition" in zoom_source
    assert "ErrorWord" in zoom_source
    assert "Text.beginiterate" in zoom_source
    assert "PosEx" in zoom_source
    assert "WordOffset" in zoom_source
    assert "MeasureTextAdvance" in zoom_source


def test_zoom_falls_back_to_entity_volume_when_word_volume_is_unavailable() -> None:
    zoom_source = read_text(ZOOM_PAS)

    assert "ZoomToSpellEntity" in zoom_source
    assert "CreateDrawingRC" in zoom_source
    assert "getonlyoutbound" in zoom_source


if __name__ == "__main__":
    test_spellchecker_zoom_helper_is_separate_unit()
    test_errors_tree_double_click_invokes_zoom_without_applying_suggestions()
    test_zoom_uses_existing_view_area_zoom_to_volume_without_selection_side_effects()
    test_mtext_word_volume_uses_error_position_and_rendered_line_layout()
    test_zoom_falls_back_to_entity_volume_when_word_volume_is_unavailable()
    print("issue 1292 spellchecker error zoom checks passed")
