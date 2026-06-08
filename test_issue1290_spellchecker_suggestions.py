#!/usr/bin/env python3
"""
Regression checks for issue 1290 spellchecker suggestion parsing.
"""

from pathlib import Path


ROOT = Path(__file__).resolve().parent
SPELL_LOGIC = (
    ROOT
    / "cad_source"
    / "zcad"
    / "velec"
    / "uzvfspellchecker"
    / "uzvfspelllogic.pas"
)


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


def test_bracketed_speller_details_are_parsed_as_suggestion_list():
    logic_pas = read_text(SPELL_LOGIC)
    helper = extract_routine(logic_pas, "ExtractSuggestionListText")
    helper_one_line = " ".join(helper.split())

    assert "openBracketPos := Pos('[', ADetails)" in helper
    assert "while (closeBracketPos > openBracketPos) and" in helper
    assert "(ADetails[closeBracketPos] <> ']')" in helper
    assert (
        "Copy(ADetails, openBracketPos + 1, "
        "closeBracketPos - openBracketPos - 1)"
    ) in helper_one_line


def test_get_suggestions_splits_only_clean_suggestion_payload():
    logic_pas = read_text(SPELL_LOGIC)
    get_suggestions = extract_routine(logic_pas, "GetSuggestions")

    assert "suggestionDetails := ExtractSuggestionListText(errorDetails)" in get_suggestions
    assert "Result.DelimitedText := suggestionDetails" in get_suggestions
    assert "Result.DelimitedText := errorDetails" not in get_suggestions


if __name__ == "__main__":
    test_bracketed_speller_details_are_parsed_as_suggestion_list()
    test_get_suggestions_splits_only_clean_suggestion_payload()
    print("issue 1290 spellchecker suggestion parsing checks passed")
