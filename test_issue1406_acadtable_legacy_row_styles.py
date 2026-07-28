#!/usr/bin/env python3
"""Regression contract for legacy AcadTable row styles in issue 1406."""

from pathlib import Path


ROOT = Path(__file__).resolve().parent
MODEL = ROOT / "cad_source/zcad/velec/acadtable/uzeacadtable_model.pas"
EDIT = (
    ROOT
    / "cad_source/zcad/velec/uzvspreadsheet/"
    "uzvspreadsheet_cmdeditacadtable.pas"
)
TESTS = ROOT / "cad_source/zengine/tests/uzctacadtable.pas"
FIXTURE = ROOT / "experiments/issue1399/zcadtablevyrav.txt"


def compact(text: str) -> str:
    return "".join(text.split()).lower()


def pairs(path: Path) -> list[tuple[str, str]]:
    lines = path.read_text(encoding="utf-8").splitlines()
    return [
        (lines[index].strip(), lines[index + 1].strip())
        for index in range(0, len(lines) - 1, 2)
    ]


def test_zcad_fixture_has_no_modern_row_style_object():
    values = pairs(FIXTURE)
    assert ("0", "ACAD_TABLE") in values
    assert ("100", "AcDbTableContent") not in values
    assert ("1", "TABLEROW_BEGIN") not in values


def test_accessor_returns_the_effective_legacy_row_style():
    model = compact(MODEL.read_text(encoding="utf-8"))
    assert "elseiffforcedatastyleallrowsthenresult:=2" in model
    assert "elseresult:=min(arow,2)" in model


def test_editor_and_fpunit_regression_use_effective_row_styles():
    edit = compact(EDIT.read_text(encoding="utf-8"))
    tests = TESTS.read_text(encoding="utf-8")
    # CellStyleTypeAt falls back to RowStyleTypeAt when a legacy table has no
    # explicit per-cell style, preserving the old editor shading behavior.
    assert "atable^.cellstyletypeat(row,col)" in edit
    assert "LoadsLegacyRowStylesForSpreadsheetEditing" in tests
    assert "zcadtablevyrav.txt" in tests


if __name__ == "__main__":
    test_zcad_fixture_has_no_modern_row_style_object()
    test_accessor_returns_the_effective_legacy_row_style()
    test_editor_and_fpunit_regression_use_effective_row_styles()
    print("issue 1406 legacy AcadTable row-style checks passed")
