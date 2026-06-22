#!/usr/bin/env python3
"""
Regression checks for issue #1368: spreadsheet cell-type colouring.

The task adds three toolbar commands to the spreadsheet editor (Title, Header,
Data) that fill the background of the selected cells so the user visually sees
the cell type, and — on export to GDBObjAcadTable — classifies each row by the
colour of its cells (a row becomes Title/Header only when ALL its cells share
that colour; otherwise it stays Data).

These are static source checks. The pure colour/unanimity logic is also covered
by the standalone Pascal test experiments/test_issue1368_classification.lpr, and
the model accessors by the FPUnit tests in cad_source/zengine/tests/uzctacadtable.pas
(SetRowStyleTypesAssignsRowStyles / RebuildResetsRowStyleTypes).
"""

from pathlib import Path


ROOT = Path(__file__).resolve().parent
SHEET = ROOT / "cad_source" / "zcad" / "velec" / "uzvspreadsheet"
CMD_CELLSTYLE = SHEET / "uzvspreadsheet_cmdcellstyle.pas"
CMD_COMMANDS = SHEET / "uzvspreadsheet_commands.pas"
CMD_CREATE = SHEET / "uzvspreadsheet_cmdcreateacadtable.pas"
MODEL = (
    ROOT / "cad_source" / "zcad" / "velec" / "acadtable" / "uzeacadtable_model.pas"
)
IMG_DIR = (
    ROOT
    / "environment"
    / "runtimefiles"
    / "AllCPU-AllOS"
    / "common"
    / "data"
    / "images"
    / "actions"
    / "velec"
)
SVG_DIR = ROOT / "cad_source" / "images" / "velec"


def read_text(path: Path) -> str:
    return path.read_text(encoding="utf-8")


def test_module_defines_required_colours():
    src = read_text(CMD_CELLSTYLE)
    # TsColor encoding is $00BBGGRR — reverse byte order of HTML #RRGGBB.
    # Title #D9E2F3 -> $00F3E2D9, Header #E7E6E6 -> $00E6E6E7, Data #FFFFFF.
    assert "CZVTitleBgColor:  TsColor = $00F3E2D9" in src
    assert "CZVHeaderBgColor: TsColor = $00E6E6E7" in src
    assert "CZVDataBgColor:   TsColor = $00FFFFFF" in src
    # The HTML references must be documented for reviewers.
    assert "#D9E2F3" in src
    assert "#E7E6E6" in src
    assert "#FFFFFF" in src


def test_module_registers_three_commands():
    src = read_text(CMD_CELLSTYLE)
    for name in ("CellStyleTitle", "CellStyleHeader", "CellStyleData"):
        assert f"'{name}'" in src, name
    # Each command paints the background only (no text-height changes).
    assert "WriteBackgroundColor" in src
    assert "WriteFontSize" not in src
    assert "WriteFont" not in src


def test_apply_uses_selection_range():
    src = read_text(CMD_CELLSTYLE)
    assert "procedure ApplyCellStyleToSelection(" in src
    assert "GetNormalizedSelection(" in src
    # Iterates over the whole selected range.
    assert "for row := startRow to endRow do" in src
    assert "for col := startCol to endCol do" in src


def test_unanimity_classification_logic():
    src = read_text(CMD_CELLSTYLE)
    assert "function ClassifyCellBackgroundColor(" in src
    assert "function DetectRowStyleKind(" in src
    assert "function CollectRowStyleTypes(" in src
    # Title -> 0, Header -> 1, Data -> 2 (base row style indices).
    assert "function CellStyleKindToRowStyleIndex(" in src
    # The unanimity rule: any differing cell drops the row to Data.
    detect = src[src.rindex("function DetectRowStyleKind("):]
    detect = detect[: detect.index("function CollectRowStyleTypes(")]
    assert "else if kind <> firstKind then" in detect
    assert "Result := zvskData;" in detect
    assert "Result := firstKind;" in detect


def test_commands_module_includes_new_unit():
    src = read_text(CMD_COMMANDS)
    assert "uzvspreadsheet_cmdcellstyle" in src


def test_export_collects_and_sets_row_styles():
    src = read_text(CMD_CREATE)
    assert "uzvspreadsheet_cmdcellstyle" in src
    assert "CollectRowStyleTypes(worksheet, rowCount, colCount)" in src
    assert "pt^.SetRowStyleTypes(rowStyleTypes)" in src
    # Must be applied before the geometry is formatted.
    set_idx = src.index("SetRowStyleTypes(rowStyleTypes)")
    fmt_idx = src.index("pt^.FormatEntity(")
    assert set_idx < fmt_idx


def test_model_supports_row_style_types():
    src = read_text(MODEL)
    assert "FRowStyleTypes: array of Integer;" in src
    assert "procedure SetRowStyleTypes(const ATypes: array of Integer);" in src
    assert "function RowStyleTypeAt(ARow: Integer): Integer;" in src
    # BuildGeometry must consult explicit row styles before the legacy flags.
    assert "if RowStyleTypeAt(RowIdx) >= 0 then" in src
    assert "StyleRowIndex := RowStyleTypeAt(RowIdx)" in src
    # Rebuild resets explicit row styles, and initnul initialises the array.
    assert src.count("System.SetLength(FRowStyleTypes, 0)") >= 2


def test_icons_present():
    for kind in ("title", "header", "data"):
        png = IMG_DIR / f"sheet_cell_{kind}.png"
        svg = SVG_DIR / f"sheet_cell_{kind}.svg"
        assert png.is_file(), png
        assert svg.is_file(), svg
        # PNG magic header.
        assert png.read_bytes()[:8] == b"\x89PNG\r\n\x1a\n"


if __name__ == "__main__":
    test_module_defines_required_colours()
    test_module_registers_three_commands()
    test_apply_uses_selection_range()
    test_unanimity_classification_logic()
    test_commands_module_includes_new_unit()
    test_export_collects_and_sets_row_styles()
    test_model_supports_row_style_types()
    test_icons_present()
    print("ALL TESTS PASSED")
