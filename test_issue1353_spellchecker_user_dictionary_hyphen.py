#!/usr/bin/env python3
"""Regression tests for issue #1353.

Problem: a word that has a hyphen ("-") at its beginning or end (e.g. "АВ-"
or "-12") was *always* reported as a spelling error, even after the user
added it to the user dictionary.

Root cause: the spell engine (uSpeller.SpellTextSimple) treats a hyphen — and
any other single-byte UTF-8 character that is not a Latin letter — as a word
break, so it only ever looks up the alphabetic "core" of the token, without
the surrounding hyphens. The word-extraction in uzvfspelllogic, however, keeps
the hyphen, so the word added to the dictionary contained the hyphen and never
matched the engine's lookup. The fix normalizes the word before saving it to
the user dictionary by stripping leading/trailing break characters so the
stored entry matches what the engine actually searches for.

The tests below:
  * verify the fix is present in the source (NormalizeUserWord + its use), and
  * simulate the two tokenizers to prove that, after normalization, the stored
    word equals the token the engine looks up (so the dictionary match works).
"""

from pathlib import Path
import re


ROOT = Path(__file__).resolve().parent
SPELL_DIR = ROOT / "cad_source" / "zcad" / "velec" / "uzvfspellchecker"
USERDICT_PAS = SPELL_DIR / "uzvfspelluserdict.pas"


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


# --- Behavioral model of the two tokenizers ---------------------------------
#
# Both helpers operate on raw UTF-8 bytes, exactly like the Pascal code, which
# indexes AnsiString byte by byte.

def _is_break_byte(b: int) -> bool:
    """Mirror of IsBreakByte: a break is any single-byte (ASCII) char that is
    not a Latin letter. Multi-byte UTF-8 bytes (>= 0x80) are letters."""
    if b >= 0x80:
        return False
    ch = chr(b)
    return not (("a" <= ch <= "z") or ("A" <= ch <= "Z"))


def normalize_user_word(word: str) -> str:
    """Mirror of NormalizeUserWord: strip leading/trailing break bytes."""
    data = word.encode("utf-8")
    start = 0
    end = len(data)
    while start < end and _is_break_byte(data[start]):
        start += 1
    while end > start and _is_break_byte(data[end - 1]):
        end -= 1
    return data[start:end].decode("utf-8")


def engine_first_lookup_token(word: str) -> str:
    """Mirror of uSpeller.SpellTextSimple.GetWord: skip leading break bytes,
    then take the maximal run of non-break bytes — that is the token the engine
    actually looks up in the dictionaries."""
    data = word.encode("utf-8")
    i = 0
    while i < len(data) and _is_break_byte(data[i]):
        i += 1
    start = i
    while i < len(data) and not _is_break_byte(data[i]):
        i += 1
    return data[start:i].decode("utf-8")


# --- Behavioral tests -------------------------------------------------------

HYPHEN_WORDS = ["АВ-", "-АВ", "слово-", "-слово", "ВЛ-", "-12привет"]


def test_engine_strips_edge_hyphens_so_raw_word_never_matches() -> None:
    """Without normalization the stored word keeps the hyphen, but the engine
    looks up the core without it — so a dictionary holding the raw word never
    matches (this is the bug)."""
    for w in HYPHEN_WORDS:
        looked_up = engine_first_lookup_token(w)
        assert looked_up != w, (
            f"engine should strip edge hyphen(s) from {w!r}, got {looked_up!r}")


def test_normalized_word_matches_engine_lookup_token() -> None:
    """After normalization the stored word equals the engine's lookup token, so
    adding it to the dictionary makes the word recognized (the fix)."""
    for w in HYPHEN_WORDS:
        assert normalize_user_word(w) == engine_first_lookup_token(w), (
            f"normalized {w!r} must equal the engine lookup token")


def test_normalization_keeps_plain_words_intact() -> None:
    for w in ["привет", "hello", "ГОСТ", "café"]:
        assert normalize_user_word(w) == w


def test_internal_hyphen_is_preserved() -> None:
    # Only leading/trailing breaks are stripped; an internal hyphen stays.
    assert normalize_user_word("кто-то") == "кто-то"


# --- Source-level checks of the implemented fix -----------------------------

def test_userdict_has_normalize_helper() -> None:
    source = read_text(USERDICT_PAS)
    assert "function IsBreakByte" in source, "IsBreakByte helper is missing"
    assert "function NormalizeUserWord" in source, (
        "NormalizeUserWord helper is missing")

    is_break = extract_routine(source, "IsBreakByte")
    # Latin letters are word characters; everything else single-byte is a break.
    assert "Ord(AByte) < $80" in is_break
    assert "['a'..'z', 'A'..'Z']" in is_break

    normalize = extract_routine(source, "NormalizeUserWord")
    # Strips from both ends.
    assert "IsBreakByte(AWord[startIdx])" in normalize
    assert "IsBreakByte(AWord[endIdx])" in normalize


def test_add_word_uses_normalization() -> None:
    source = read_text(USERDICT_PAS)
    impl = source[source.index("\nimplementation"):]
    routine = extract_routine(impl, "AddWordToUserDictionary")
    assert "NormalizeUserWord(" in routine, (
        "AddWordToUserDictionary must normalize the word before storing it")


if __name__ == "__main__":
    test_engine_strips_edge_hyphens_so_raw_word_never_matches()
    test_normalized_word_matches_engine_lookup_token()
    test_normalization_keeps_plain_words_intact()
    test_internal_hyphen_is_preserved()
    test_userdict_has_normalize_helper()
    test_add_word_uses_normalization()
    print("issue 1353 spellchecker user dictionary hyphen checks passed")
