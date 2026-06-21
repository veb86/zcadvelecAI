#!/usr/bin/env python3
"""Regression tests for issue #1361.

Feature: a dedicated form (and command) to manage the user spelling dictionary.

The form must:
  * show a fast list of the user dictionary words using the *same* control
    (TLazVirtualStringTree) as the error word list in uzvfspellform;
  * provide a search field to quickly filter a long list of words;
  * provide a "delete" button that removes the focused word from the user
    dictionary.

To support the form, the user-dictionary unit (uzvfspelluserdict) gains:
  * LoadUserDictionaryWords  - read every stored word, and
  * RemoveWordFromUserDictionary - delete a word and reload the speller.

The tests below verify these pieces exist in the source and are wired together,
mirroring the source-level style of the other spellchecker regression tests.
"""

from pathlib import Path
import re


ROOT = Path(__file__).resolve().parent
SPELL_DIR = ROOT / "cad_source" / "zcad" / "velec" / "uzvfspellchecker"
USERDICT_PAS = SPELL_DIR / "uzvfspelluserdict.pas"
FORM_PAS = SPELL_DIR / "uzvfuserdictform.pas"
FORM_LFM = SPELL_DIR / "uzvfuserdictform.lfm"
REGISTER_PAS = (
    ROOT / "cad_source" / "zcad" / "register" / "uzcregspellchecker.pas")
COMMAND_PAS = (
    ROOT / "cad_source" / "zcad" / "commands"
    / "uzccommand_spelluserdict.pas")
ZCAD_PAS = ROOT / "cad_source" / "zcad.pas"


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


# --- uzvfspelluserdict: load + remove API -----------------------------------

def test_userdict_exposes_load_and_remove() -> None:
    source = read_text(USERDICT_PAS)

    # Both must be declared in the interface so the form can call them.
    interface = source[:source.index("\nimplementation")]
    assert "function LoadUserDictionaryWords(AList: TStrings): boolean;" in \
        interface, "LoadUserDictionaryWords must be declared in interface"
    assert "function RemoveWordFromUserDictionary(const AWord: string): " \
        "boolean;" in interface, (
        "RemoveWordFromUserDictionary must be declared in interface")


def test_load_words_reads_dictionary() -> None:
    source = read_text(USERDICT_PAS)
    impl = source[source.index("\nimplementation"):]
    routine = extract_routine(impl, "LoadUserDictionaryWords")

    assert "GetUserDictionaryPath" in routine
    # Reuses the existing per-line loader that strips the count/affixes/BOM.
    assert "LoadUserWords" in routine


def test_remove_word_saves_and_reloads() -> None:
    source = read_text(USERDICT_PAS)
    impl = source[source.index("\nimplementation"):]
    routine = extract_routine(impl, "RemoveWordFromUserDictionary")

    assert "GetUserDictionaryPath" in routine
    assert "IndexOf" in routine
    assert "Delete" in routine
    assert "SaveUserWords" in routine
    # The removed word must stop being recognized immediately in-session.
    assert "ReloadSpellChecker" in routine


# --- Form: list (same control), search, delete ------------------------------

def test_form_unit_exists() -> None:
    assert FORM_PAS.exists(), "user dictionary form unit must exist"
    assert FORM_LFM.exists(), "user dictionary form layout must exist"
    source = read_text(FORM_PAS)
    assert "unit uzvfuserdictform;" in source
    assert "TUserDictForm = class(TForm)" in source
    assert "uzvfspelluserdict" in source, "form must use the user-dict unit"


def test_form_uses_same_list_control_as_errors() -> None:
    source = read_text(FORM_PAS)
    lfm = read_text(FORM_LFM)
    # Same control type as the error word list in uzvfspellform.
    assert "WordsTree: TLazVirtualStringTree" in source
    assert "object WordsTree: TLazVirtualStringTree" in lfm
    # Text of each row is provided through OnGetText, like the errors tree.
    assert "procedure WordsTreeGetText" in source
    assert "OnGetText = WordsTreeGetText" in lfm


def test_form_has_search_field_that_filters() -> None:
    source = read_text(FORM_PAS)
    lfm = read_text(FORM_LFM)

    assert "SearchEdit: TEdit" in source
    assert "object SearchEdit: TEdit" in lfm
    assert "OnChange = SearchEditChange" in lfm

    # Changing the search text re-applies the filter.
    handler = extract_routine(source, "TUserDictForm.SearchEditChange")
    assert "ApplyFilter" in handler

    # The filter narrows the displayed words by the (case-insensitive) query.
    apply_filter = extract_routine(source, "TUserDictForm.ApplyFilter")
    assert "SearchEdit.Text" in apply_filter
    assert "FFilteredWords" in apply_filter
    # Case-insensitive substring match over UTF-8 words.
    assert "UTF8LowerCase" in apply_filter
    assert "Pos(filter" in apply_filter


def test_form_has_delete_action_and_button() -> None:
    source = read_text(FORM_PAS)
    lfm = read_text(FORM_LFM)

    assert "DeleteWordAction: TAction" in source
    assert "procedure DeleteWordActionExecute(Sender: TObject);" in source

    assert "object DeleteWordAction: TAction" in lfm
    assert "object DeleteWordButton: TToolButton" in lfm
    assert "Action = DeleteWordAction" in lfm

    handler = extract_routine(source, "TUserDictForm.DeleteWordActionExecute")
    # Removes the focused word and refreshes the list.
    assert "GetFocusedWord" in handler
    assert "RemoveWordFromUserDictionary" in handler
    assert "ReloadWords" in handler


def test_form_reload_loads_words() -> None:
    source = read_text(FORM_PAS)
    handler = extract_routine(source, "TUserDictForm.ReloadWords")
    assert "LoadUserDictionaryWords" in handler
    assert "ApplyFilter" in handler


# --- Registration + command -------------------------------------------------

def test_form_is_registered() -> None:
    source = read_text(REGISTER_PAS)
    assert "uzvfuserdictform" in source, "register unit must use the form unit"
    assert "RegisterZCADFormInfo('uzvfuserdict'" in source, (
        "the user dictionary form must be registered under name 'uzvfuserdict'")
    assert "TUserDictForm" in source


def test_command_opens_the_form() -> None:
    assert COMMAND_PAS.exists(), "SpellUserDict command unit must exist"
    source = read_text(COMMAND_PAS)
    assert "unit uzccommand_spelluserdict;" in source
    assert "CreateZCADCommand(@SpellUserDict_com,'SpellUserDict'" in source
    # The command shows the registered form.
    assert "ShowForm" in source
    assert "uzvfuserdict" in source


def test_command_registered_in_project() -> None:
    source = read_text(ZCAD_PAS)
    assert "uzcCommand_SpellUserDict" in source, (
        "the command unit must be referenced from zcad.pas")


if __name__ == "__main__":
    test_userdict_exposes_load_and_remove()
    test_load_words_reads_dictionary()
    test_remove_word_saves_and_reloads()
    test_form_unit_exists()
    test_form_uses_same_list_control_as_errors()
    test_form_has_search_field_that_filters()
    test_form_has_delete_action_and_button()
    test_form_reload_loads_words()
    test_form_is_registered()
    test_command_opens_the_form()
    test_command_registered_in_project()
    print("issue 1361 spellchecker user dictionary form checks passed")
