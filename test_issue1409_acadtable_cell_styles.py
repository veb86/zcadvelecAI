#!/usr/bin/env python3
"""Regression contract for per-cell AcadTable styles (issue #1409)."""

from pathlib import Path


ROOT = Path(__file__).resolve().parent
SHEET = ROOT / "cad_source" / "zcad" / "velec" / "uzvspreadsheet"
ACADTABLE = ROOT / "cad_source" / "zcad" / "velec" / "acadtable"


def read(path: Path) -> str:
    return path.read_text(encoding="utf-8")


def test_spreadsheet_collects_every_cell_style():
    source = read(SHEET / "uzvspreadsheet_cmdcellstyle.pas")
    assert "function CollectCellStyleTypes(" in source
    assert "Result[row * AColCount + col]" in source
    assert "ClassifyCellBackgroundColor" in source


def test_create_and_save_pass_cell_styles_to_model():
    for name in (
        "uzvspreadsheet_cmdcreateacadtable.pas",
        "uzvspreadsheet_cmdsaveacadtable.pas",
    ):
        source = read(SHEET / name)
        assert "CollectCellStyleTypes(" in source, name
        assert "SetCellStyleTypes(" in source, name


def test_edit_restores_every_cell_style_from_model():
    source = read(SHEET / "uzvspreadsheet_cmdeditacadtable.pas")
    assert "ATable^.CellStyleTypeAt(Row, Col)" in source
    assert "CellStyleKindColor(" in source
    assert "WriteRowType(" not in source


def test_model_keeps_cell_styles_independent():
    source = read(ACADTABLE / "uzeacadtable_model.pas")
    assert "procedure SetCellStyleTypes(const ATypes: array of Integer);" in source
    assert "function CellStyleTypeAt(ARow, ACol: Integer): Integer;" in source
    assert "FCells[RowIdx][ColIdx].StyleType" in source


def test_dxf_writes_style_for_each_cell():
    # Group 172 of a cell is the "cell flag value", not the cell style, and
    # AutoCAD writes 0 there (see cad_source/test/tablebugheader.dxf).  The
    # style AutoCAD actually reads is written per cell into the TABLECONTENT
    # object - see test_issue1409_tablecontent_dxf.py.
    source = read(ACADTABLE / "uzeacadtable_dxf_write.pas")
    assert "function CellStyleTypeAt(" in source
    assert "dxfIntegerout(AOutStream, 172, CAcadCellFlagValue);" in source
    assert "dxfIntegerout(AOutStream, 172, StyleType);" not in source
    assert "AcadCellStyleId(CellStyleTypeAt(APart, RowIdx, ColIdx));" in source
    assert "AddAcadTableContentRecord(" in source


if __name__ == "__main__":
    test_spreadsheet_collects_every_cell_style()
    test_create_and_save_pass_cell_styles_to_model()
    test_edit_restores_every_cell_style_from_model()
    test_model_keeps_cell_styles_independent()
    test_dxf_writes_style_for_each_cell()
    print("ALL TESTS PASSED")
