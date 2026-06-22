#!/usr/bin/env python3
"""
Regression checks for issue 1359 follow-up feedback.

The author requested two changes to the spreadsheet cell-size linkage:
  1. Drop the duplicate read-only ACAD-value field; keep a single editable
     field for column width and a single one for row height.
  2. Apply a width/height change to every selected column/row, not only the
     active cell's column/row.

These are static source checks (the Lazarus GUI cannot be exercised
head-less here); the pure range logic is covered by the standalone Pascal
test experiments/test_dimension_range.pas.
"""

from pathlib import Path


ROOT = Path(__file__).resolve().parent
GUI_PAS = (
    ROOT
    / "cad_source"
    / "zcad"
    / "velec"
    / "uzvspreadsheet"
    / "uzvspreadsheet_gui.pas"
)
DIMENSIONS_PAS = (
    ROOT
    / "cad_source"
    / "zcad"
    / "velec"
    / "uzvspreadsheet"
    / "uzvspreadsheet_dimensions.pas"
)


def read_text(path: Path) -> str:
    return path.read_text(encoding="utf-8")


def test_duplicate_acad_fields_removed():
    gui = read_text(GUI_PAS)
    # The read-only ACAD-value duplicate fields must be gone entirely.
    assert "FEditColWidthAcad" not in gui
    assert "FEditRowHeightAcad" not in gui
    # Exactly one editable field remains for each dimension.
    assert gui.count("FEditColWidthWS: TEdit") == 1
    assert gui.count("FEditRowHeightWS: TEdit") == 1
    # Only two dimension edits are constructed (col width + row height).
    assert gui.count(":= CreateDimensionEdit(") == 2


def test_dimensions_unit_exposes_range_setters():
    dimensions = read_text(DIMENSIONS_PAS)
    assert "function SetWorksheetColWidthsMM(" in dimensions
    assert "function SetWorksheetRowHeightsMM(" in dimensions
    # Range setters iterate over the requested span.
    assert "for ColIdx := AStartCol to AEndCol do" in dimensions
    assert "for RowIdx := AStartRow to AEndRow do" in dimensions


def test_apply_uses_selection_range():
    gui = read_text(GUI_PAS)
    # Column width is applied across the selected column range.
    assert "SetWorksheetColWidthsMM(worksheet, startCol, endCol, widthMM)" in gui
    # Row height is applied across the selected row range.
    assert (
        "SetWorksheetRowHeightsMM(worksheet, startRow, endRow, heightMM)" in gui
    )
    # Both apply procedures consult the grid selection.
    assert gui.count("GetSelectedRange(startRow, endRow, startCol, endCol)") == 2


if __name__ == "__main__":
    test_duplicate_acad_fields_removed()
    test_dimensions_unit_exposes_range_setters()
    test_apply_uses_selection_range()
    print("ALL TESTS PASSED")
