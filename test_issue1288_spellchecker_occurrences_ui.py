#!/usr/bin/env python3
"""
Regression checks for issue 1288 spellchecker occurrence listing and UI.
"""

from pathlib import Path


ROOT = Path(__file__).resolve().parent
SPELLCHECKER_DIR = (
    ROOT / "cad_source" / "zcad" / "velec" / "uzvfspellchecker"
)
SPELL_DATA = SPELLCHECKER_DIR / "uzvfspelldata.pas"
SPELL_FORM = SPELLCHECKER_DIR / "uzvfspellform.pas"
SPELL_LOGIC = SPELLCHECKER_DIR / "uzvfspelllogic.pas"
SPELL_LFM = SPELLCHECKER_DIR / "uzvfspellform.lfm"


def read_text(path: Path) -> str:
    return path.read_text(encoding="utf-8")


def extract_routine(source: str, routine_name: str) -> str:
    markers = [f"procedure {routine_name}", f"function {routine_name}"]
    implementation = source.find("\nimplementation")
    search_from = implementation if implementation != -1 else 0
    starts = [source.find(marker, search_from) for marker in markers]
    starts = [start for start in starts if start != -1]
    assert starts, f"{routine_name} not found"
    start = min(starts)
    candidates = [
        position
        for position in (
            source.find("\nprocedure ", start + 1),
            source.find("\nfunction ", start + 1),
        )
        if position != -1
    ]
    end = min(candidates) if candidates else len(source)
    return source[start:end]


def test_spell_errors_are_stored_per_occurrence_not_per_unique_word():
    data_pas = read_text(SPELL_DATA)
    logic_pas = read_text(SPELL_LOGIC)
    find_all_errors = extract_routine(logic_pas, "FindAllErrors")
    append_errors = extract_routine(logic_pas, "AppendErrorsFromText")

    assert "OccurrenceCount" not in data_pas
    assert "FindErrorByWord" not in data_pas
    assert "IncrementOccurrence" not in data_pas
    assert "FindErrorByWord" not in find_all_errors
    assert "IncrementOccurrence" not in find_all_errors
    assert "FindErrorByWord" not in append_errors
    assert "IncrementOccurrence" not in append_errors
    assert "ErrorManager.AddError(currentWord, sentence, absolutePosition" in append_errors
    assert "AEntityPtr, currentPos" in append_errors
    assert "AppendErrorsFromText(AText, ErrorManager)" in find_all_errors
    assert "found %d errors" in find_all_errors
    assert "unique errors" not in find_all_errors
    assert "AppendErrorsFromText" in logic_pas
    assert "IsWordDelimiter" in logic_pas
    assert "AChar in [#10, #13]" in logic_pas


def test_errors_tree_has_no_count_column():
    form_pas = read_text(SPELL_FORM)
    form_lfm = read_text(SPELL_LFM)

    assert "COL_ERROR_COUNT" not in form_pas
    assert "Количество" not in form_lfm
    assert "OccurrenceCount" not in form_pas


def test_errors_and_suggestions_are_stacked_vertically():
    form_lfm = read_text(SPELL_LFM)

    errors_section = form_lfm[
        form_lfm.index("object ErrorsTree"):form_lfm.index("object SuggestionsTree")
    ]
    suggestions_section = form_lfm[
        form_lfm.index("object SuggestionsTree"):form_lfm.index("object ActionList")
    ]

    assert "Align = alTop" in errors_section
    assert "Align = alClient" in suggestions_section
    assert "Width = 900" in errors_section
    assert "Width = 900" in suggestions_section


def test_selected_suggestion_can_be_applied_from_button_or_double_click():
    form_pas = read_text(SPELL_FORM)
    form_lfm = read_text(SPELL_LFM)

    assert "ApplySuggestionAction: TAction" in form_pas
    assert "ApplySuggestionButton: TToolButton" in form_pas
    assert "procedure ApplySuggestionActionExecute" in form_pas
    assert "procedure SuggestionsTreeDblClick" in form_pas
    assert "procedure TSpellCheckerForm.ApplySelectedSuggestion" in form_pas
    assert "ReplaceCurrentErrorWithSuggestion" in form_pas
    assert "ApplySuggestionToEntity" in form_pas
    assert "TUndoCmdSaveEntityState.CreateAndPush" in form_pas
    assert "OnDblClick = SuggestionsTreeDblClick" in form_lfm
    assert "Caption = 'Применить'" in form_lfm


def test_sentence_label_includes_error_word_and_context():
    form_pas = read_text(SPELL_FORM)
    focus_changed = extract_routine(
        form_pas, "TSpellCheckerForm.ErrorsTreeFocusChanged"
    )

    assert "function TSpellCheckerForm.FormatSentenceMessage" in form_pas
    assert 'Ошибка: "%s". Контекст: %s' in form_pas
    assert "SentenceLabel.Caption := FormatSentenceMessage(errorPtr)" in focus_changed


if __name__ == "__main__":
    test_spell_errors_are_stored_per_occurrence_not_per_unique_word()
    test_errors_tree_has_no_count_column()
    test_errors_and_suggestions_are_stacked_vertically()
    test_selected_suggestion_can_be_applied_from_button_or_double_click()
    test_sentence_label_includes_error_word_and_context()
    print("issue 1288 spellchecker occurrence and UI checks passed")
