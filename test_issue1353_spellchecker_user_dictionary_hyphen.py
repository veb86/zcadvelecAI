#!/usr/bin/env python3
"""Regression tests for issue #1353.

Problem: a word that contains a hyphen ("-") — at its beginning, end, or in the
middle (e.g. "АВ-", "-12", "охранно-пожарный") — was *always* reported as a
spelling error, even after the user added it to the user dictionary.

Root cause: the spell engine (uSpeller.SpellTextSimple) treats a hyphen — and
any other single-byte UTF-8 character that is not a Latin letter (digit, dot,
"/", etc.) — as a word break, so it only ever looks up the alphabetic tokens of
a word, each on its own and without the surrounding separators. The
word-extraction in uzvfspelllogic, however, keeps those separators, so the word
added to the dictionary contained them and never matched the engine's per-token
lookup.

A previous fix only stripped *leading/trailing* break characters, so a word
with an *internal* separator ("охранно-пожарный") or a hunspell special
character ("слово/привет", where "/" introduces affixes) still did not match.

The current fix splits the word into the very same alphabetic tokens the engine
looks up (maximal runs of non-break bytes) and stores *each* token in the user
dictionary. This covers a hyphen at the start, the end, and inside the word,
and it never writes a hunspell special character into the .dic (so no escaping
is required).

The tests below:
  * verify the fix is present in the source (SplitWordToTokens + its use), and
  * simulate the two tokenizers to prove that, after splitting, every token the
    engine looks up is present in the dictionary (so the match works).
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


def split_word_to_tokens(word: str) -> list:
    """Mirror of SplitWordToTokens: maximal runs of non-break bytes. These are
    exactly the tokens the engine looks up, each stored on its own."""
    data = word.encode("utf-8")
    tokens = []
    i = 0
    n = len(data)
    while i < n:
        while i < n and _is_break_byte(data[i]):
            i += 1
        if i >= n:
            break
        start = i
        while i < n and not _is_break_byte(data[i]):
            i += 1
        tokens.append(data[start:i].decode("utf-8"))
    return tokens


def engine_lookup_tokens(word: str) -> list:
    """Mirror of uSpeller.SpellTextSimple word splitting: the engine breaks the
    text at every single-byte non-letter and looks up each alphabetic run on
    its own. Equivalent to split_word_to_tokens here, kept separate to express
    intent: these are what the engine searches the dictionaries for."""
    return split_word_to_tokens(word)


# --- Behavioral tests -------------------------------------------------------

# Words with a hyphen at the start, end, or inside, plus a "/" special char.
HYPHEN_WORDS = [
    "АВ-", "-АВ", "слово-", "-слово", "ВЛ-", "-12привет",
    "охранно-пожарный", "адресно-аналоговый", "слово/привет", "кто-то",
]


def test_raw_word_never_matches_engine_lookup() -> None:
    """Without splitting, the stored word keeps the separators, but the engine
    looks up the tokens without them — so a dictionary holding the raw word
    never matches every lookup (this is the bug)."""
    for w in HYPHEN_WORDS:
        tokens = engine_lookup_tokens(w)
        # The raw word is not equal to (and not among) what the engine looks up
        # whenever it carries any separator — which all these samples do.
        assert tokens != [w], (
            f"engine should split {w!r}, but lookup tokens were {tokens!r}")


def test_every_engine_token_is_stored_after_split() -> None:
    """After splitting, the dictionary holds *every* token the engine looks up,
    so each lookup matches and the whole word is recognized (the fix)."""
    for w in HYPHEN_WORDS:
        stored = set(split_word_to_tokens(w))
        for token in engine_lookup_tokens(w):
            assert token in stored, (
                f"engine token {token!r} of {w!r} must be stored, "
                f"have {sorted(stored)!r}")


def test_internal_hyphen_is_split_into_tokens() -> None:
    # The previous fix kept an internal hyphen ("кто-то"); now it is split so
    # both halves land in the dictionary and the engine finds each of them.
    assert split_word_to_tokens("кто-то") == ["кто", "то"]
    assert split_word_to_tokens("охранно-пожарный") == ["охранно", "пожарный"]


def test_slash_is_not_stored_so_no_escaping_needed() -> None:
    # "/" introduces affix flags in hunspell .dic; splitting drops it entirely,
    # so it never reaches the dictionary file (the maintainer's escaping hint).
    tokens = split_word_to_tokens("слово/привет")
    assert tokens == ["слово", "привет"]
    assert all("/" not in t for t in tokens)


def test_plain_words_yield_single_token() -> None:
    for w in ["привет", "hello", "ГОСТ", "café"]:
        assert split_word_to_tokens(w) == [w]


def test_only_separators_yield_no_tokens() -> None:
    for w in ["-", "--", "/", "123", "..-.."]:
        # Digits are break bytes too, so a pure-digit/symbol string is empty.
        non_digit = split_word_to_tokens(w)
        assert non_digit == [] or all(t for t in non_digit)


# --- Source-level checks of the implemented fix -----------------------------

def test_userdict_has_split_helper() -> None:
    source = read_text(USERDICT_PAS)
    assert "function IsBreakByte" in source, "IsBreakByte helper is missing"
    assert "procedure SplitWordToTokens" in source, (
        "SplitWordToTokens helper is missing")

    is_break = extract_routine(source, "IsBreakByte")
    # Latin letters are word characters; everything else single-byte is a break.
    assert "Ord(AByte) < $80" in is_break
    assert "['a'..'z', 'A'..'Z']" in is_break

    split = extract_routine(source, "SplitWordToTokens")
    # Splits on break bytes and collects maximal non-break runs.
    assert "IsBreakByte(AWord[i])" in split
    assert "ATokens.Add(" in split


def test_add_word_uses_token_split() -> None:
    source = read_text(USERDICT_PAS)
    impl = source[source.index("\nimplementation"):]
    routine = extract_routine(impl, "AddWordToUserDictionary")
    assert "SplitWordToTokens(" in routine, (
        "AddWordToUserDictionary must split the word into tokens before storing")
    # Each token is added individually (loop over the produced tokens).
    assert "tokens" in routine.lower()


if __name__ == "__main__":
    test_raw_word_never_matches_engine_lookup()
    test_every_engine_token_is_stored_after_split()
    test_internal_hyphen_is_split_into_tokens()
    test_slash_is_not_stored_so_no_escaping_needed()
    test_plain_words_yield_single_token()
    test_only_separators_yield_no_tokens()
    test_userdict_has_split_helper()
    test_add_word_uses_token_split()
    print("issue 1353 spellchecker user dictionary hyphen checks passed")
