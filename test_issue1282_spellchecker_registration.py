#!/usr/bin/env python3
"""
Regression checks for issue 1282 spellchecker form integration.
"""

from pathlib import Path


ROOT = Path(__file__).resolve().parent
CAD_SOURCE = ROOT / "cad_source"
SPELLCHECKER_DIR = CAD_SOURCE / "zcad" / "velec" / "uzvfspellchecker"


def read_text(path: Path) -> str:
    return path.read_text(encoding="utf-8")


def test_spellchecker_form_uses_lazarus_virtualtree_unit():
    form_pas = read_text(SPELLCHECKER_DIR / "uzvfspellform.pas")
    form_lfm = read_text(SPELLCHECKER_DIR / "uzvfspellform.lfm")

    assert "laz.VirtualTrees" in form_pas
    assert "StdCtrls, ExtCtrls, VirtualTrees" not in form_pas
    assert "TLazVirtualStringTree" in form_pas
    assert "object ErrorsTree: TLazVirtualStringTree" in form_lfm
    assert "object SuggestionsTree: TLazVirtualStringTree" in form_lfm


def test_spellchecker_path_is_in_lazarus_project_search_path():
    lpi = read_text(CAD_SOURCE / "zcad.lpi")

    assert "zcad/velec/uzvfspellchecker" in lpi


def test_spellchecker_form_is_registered_for_show_command():
    zcad_pas = read_text(CAD_SOURCE / "zcad.pas")
    register_pas = read_text(CAD_SOURCE / "zcad" / "register" / "uzcregspellchecker.pas")

    assert "uzcregspellchecker" in zcad_pas
    assert "uzvfspellform" in register_pas
    assert "RegisterZCADFormInfo('uzvfspellchecker'" in register_pas
    assert "TSpellCheckerForm" in register_pas
    assert "@SpellCheckerForm" in register_pas


if __name__ == "__main__":
    test_spellchecker_form_uses_lazarus_virtualtree_unit()
    test_spellchecker_path_is_in_lazarus_project_search_path()
    test_spellchecker_form_is_registered_for_show_command()
    print("issue 1282 spellchecker registration checks passed")
