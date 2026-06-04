#!/usr/bin/env python3
"""
Regression checks for issue 1284 spellchecker result type usage.
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
SPELL_README = (
    ROOT
    / "cad_source"
    / "zcad"
    / "velec"
    / "uzvfspellchecker"
    / "README.md"
)


def read_text(path: Path) -> str:
    return path.read_text(encoding="utf-8")


def test_spellchecker_logic_uses_existing_speller_result_type():
    logic_pas = read_text(SPELL_LOGIC)

    assert "TSpeller.TSpellResult" not in logic_pas
    assert "spellResult: TSpeller.TLangHandle;" in logic_pas
    assert "SpellTextSimple(AWord, ErrorDetails, spellOpts)" in logic_pas
    assert "spellResult = TSpeller.WrongLang" in logic_pas


def test_spellchecker_readme_documents_actual_result_type():
    readme_md = read_text(SPELL_README)
    actual_signature = (
        "SpellTextSimple(Text: string; var Details: string; Opts: TSpellOpts): "
        "TLangHandle"
    )
    invalid_signature = (
        "SpellTextSimple(Text: string; var Details: string; Opts: TSpellOpts): "
        "TSpellResult"
    )

    assert actual_signature in readme_md
    assert invalid_signature not in readme_md


if __name__ == "__main__":
    test_spellchecker_logic_uses_existing_speller_result_type()
    test_spellchecker_readme_documents_actual_result_type()
    print("issue 1284 spellchecker result type checks passed")
