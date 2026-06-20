#!/usr/bin/env python3
"""Regression tests for issue #1296.

Two features are covered:

Task 1: Adding a selected error word to a user dictionary (created on demand,
        saved/loaded the same way and in the same writable folder as panels).
Task 2: Double-click zoom enlarges the word (as if 3x larger) so the zoom is
        not too close, with the enlarge coefficient defined as a constant.
"""

from pathlib import Path
import re


ROOT = Path(__file__).resolve().parent
SPELL_DIR = ROOT / "cad_source" / "zcad" / "velec" / "uzvfspellchecker"
FORM_PAS = SPELL_DIR / "uzvfspellform.pas"
FORM_LFM = SPELL_DIR / "uzvfspellform.lfm"
ZOOM_PAS = SPELL_DIR / "uzvfspellzoom.pas"
USERDICT_PAS = SPELL_DIR / "uzvfspelluserdict.pas"
SPELLER_PAS = ROOT / "cad_source" / "zcad" / "misc" / "uzcspeller.pas"


def read_text(path: Path) -> str:
    return path.read_text(encoding="utf-8", errors="ignore")


def extract_routine(source: str, routine_name: str) -> str:
    pattern = (
        rf"(?:procedure|function)\s+{re.escape(routine_name)}\b.*?"
        rf"(?=\nprocedure\s+|\nfunction\s+|\ninitialization|\nfinalization|\Z)"
    )
    match = re.search(pattern, source, flags=re.IGNORECASE | re.DOTALL)
    assert match, f"{routine_name} routine is missing"
    return match.group(0)


# --- Task 2: zoom enlargement ------------------------------------------------

def test_zoom_enlarge_factor_is_a_constant() -> None:
    zoom_source = read_text(ZOOM_PAS)

    # The size-increase coefficient must be a named constant (per the issue).
    match = re.search(
        r"WORD_ZOOM_ENLARGE_FACTOR\s*=\s*([0-9]+)", zoom_source)
    assert match, "WORD_ZOOM_ENLARGE_FACTOR constant is missing"
    # The issue asks for a 3x enlargement.
    assert match.group(1) == "3", "enlarge factor must be 3"


def test_zoom_enlarges_word_volume_before_zooming() -> None:
    zoom_source = read_text(ZOOM_PAS)

    assert "procedure EnlargeVolume" in zoom_source

    # Extract from the implementation section (skip the interface declaration).
    impl = zoom_source[zoom_source.index("\nimplementation"):]
    handler = extract_routine(impl, "ZoomToSpellError")
    # The word volume must be enlarged with the constant before zooming.
    enlarge_pos = handler.find("EnlargeVolume(volume, WORD_ZOOM_ENLARGE_FACTOR)")
    zoom_pos = handler.find("wa.ZoomToVolume(volume)")
    assert enlarge_pos != -1, "ZoomToSpellError must enlarge the word volume"
    assert zoom_pos != -1, "ZoomToSpellError must zoom to the volume"
    assert enlarge_pos < zoom_pos, "enlargement must happen before zooming"


def test_enlarge_volume_scales_around_center() -> None:
    zoom_source = read_text(ZOOM_PAS)
    routine = extract_routine(zoom_source, "EnlargeVolume")

    # Scales the bounding box around its center by Factor.
    assert "center.x := (Volume.LBN.x + Volume.RTF.x) / 2" in routine
    assert "(Volume.LBN.x - center.x) * Factor" in routine
    assert "(Volume.RTF.x - center.x) * Factor" in routine


# --- Task 1: user dictionary -------------------------------------------------

def test_user_dictionary_unit_exists() -> None:
    assert USERDICT_PAS.exists(), "user dictionary logic must live in a unit"

    source = read_text(USERDICT_PAS)
    assert "unit uzvfspelluserdict;" in source
    assert "function AddWordToUserDictionary" in source


def test_user_dictionary_saved_in_writable_config_like_panels() -> None:
    speller_source = read_text(SPELLER_PAS)

    # Saved/loaded the same way and folder as panels (GetWritableFilePath +
    # the dictionaries subfolder), mirroring SaveLayout.
    assert "function GetUserDictionaryPath" in speller_source
    speller_impl = speller_source[speller_source.index("\nimplementation"):]
    path_routine = extract_routine(speller_impl, "GetUserDictionaryPath")
    assert "GetWritableFilePath" in path_routine
    assert "CFSdictionariesDir" in path_routine
    assert "CUserDictionaryFile" in speller_source


def test_user_dictionary_uses_hunspell_format_with_aff_pair() -> None:
    source = read_text(USERDICT_PAS)

    # hunspell .dic: first line is the word count.
    save_routine = extract_routine(source, "SaveUserWords")
    assert "IntToStr(AList.Count)" in save_routine

    # A paired .aff file is required by hunspell.
    assert "procedure EnsureAffFile" in source
    aff_routine = extract_routine(source, "EnsureAffFile")
    assert "ChangeFileExt(ADicPath, '.aff')" in aff_routine


def test_user_dictionary_aff_declares_utf8_encoding() -> None:
    # The dictionary stores UTF-8 words; the paired .aff must declare the
    # encoding (SET UTF-8) so hunspell does not fall back to ISO8859-1 and
    # fail to recognize the added word (issue #1296, follow-up comment 2).
    source = read_text(USERDICT_PAS)

    match = re.search(r"CUserDictAffContent\s*=\s*'SET UTF-8'", source)
    assert match, "user .aff must declare SET UTF-8"

    aff_routine = extract_routine(source, "EnsureAffFile")
    assert "CUserDictAffContent" in aff_routine, (
        "EnsureAffFile must write the SET UTF-8 affix content")


def test_add_word_creates_dictionary_and_reloads_speller() -> None:
    source = read_text(USERDICT_PAS)
    impl = source[source.index("\nimplementation"):]
    routine = extract_routine(impl, "AddWordToUserDictionary")

    assert "GetUserDictionaryPath" in routine
    assert "SaveUserWords" in routine
    assert "EnsureAffFile" in routine
    # The new word must be recognized immediately in-session.
    assert "ReloadSpellChecker" in routine


def test_speller_loads_user_dictionary_at_startup_and_reload() -> None:
    speller_source = read_text(SPELLER_PAS)

    assert "procedure ReloadSpellChecker" in speller_source
    speller_impl = speller_source[speller_source.index("\nimplementation"):]
    create_routine = extract_routine(speller_impl, "CreateSpellChecker")
    # User dictionary is loaded on top of the configured dictionaries.
    assert "GetUserDictionaryPath" in create_routine
    assert "LoadDictionary" in create_routine

    reload_routine = extract_routine(speller_impl, "ReloadSpellChecker")
    assert "DestroySpellChecker" in reload_routine
    assert "CreateSpellChecker" in reload_routine


def test_form_has_add_to_dictionary_action_and_button() -> None:
    form_source = read_text(FORM_PAS)
    lfm_source = read_text(FORM_LFM)

    assert "uzvfspelluserdict" in form_source
    assert "AddToDictionaryAction: TAction" in form_source
    assert "procedure AddToDictionaryActionExecute(Sender: TObject);" in form_source

    handler = extract_routine(
        form_source, "TSpellCheckerForm.AddToDictionaryActionExecute")
    assert "GetFocusedError" in handler
    assert "AddWordToUserDictionary" in handler
    # After adding, the drawing is re-checked so the word stops being an error.
    assert "CheckCurrentDrawing" in handler

    assert "object AddToDictionaryAction: TAction" in lfm_source
    assert "object AddToDictionaryButton: TToolButton" in lfm_source
    assert "Action = AddToDictionaryAction" in lfm_source


if __name__ == "__main__":
    test_zoom_enlarge_factor_is_a_constant()
    test_zoom_enlarges_word_volume_before_zooming()
    test_enlarge_volume_scales_around_center()
    test_user_dictionary_unit_exists()
    test_user_dictionary_saved_in_writable_config_like_panels()
    test_user_dictionary_uses_hunspell_format_with_aff_pair()
    test_add_word_creates_dictionary_and_reloads_speller()
    test_speller_loads_user_dictionary_at_startup_and_reload()
    test_form_has_add_to_dictionary_action_and_button()
    print("issue 1296 spellchecker user dictionary checks passed")
