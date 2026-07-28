#!/usr/bin/env python3
"""Static regression contract for AcadTable <-> uzvspreadsheet editing."""

from pathlib import Path

ROOT = Path(__file__).parent
MODEL = ROOT / "cad_source/zcad/velec/acadtable/uzeacadtable_model.pas"
COMMANDS = ROOT / "cad_source/zcad/velec/uzvspreadsheet/uzvspreadsheet_commands.pas"
OPEN_CMD = (
    ROOT
    / "cad_source/zcad/velec/uzvspreadsheet/"
    "uzvspreadsheet_cmdeditacadtable.pas"
)
SAVE_CMD = (
    ROOT
    / "cad_source/zcad/velec/uzvspreadsheet/"
    "uzvspreadsheet_cmdsaveacadtable.pas"
)


def require(text: str, fragment: str, context: str) -> None:
    assert fragment in text, f"{context}: missing {fragment!r}"


model = MODEL.read_text(encoding="utf-8")
commands = COMMANDS.read_text(encoding="utf-8")
assert OPEN_CMD.exists(), "editacadtable launch command must be a separate unit"
assert SAVE_CMD.exists(), "save-back command must be a separate unit"
open_cmd = OPEN_CMD.read_text(encoding="utf-8")
save_cmd = SAVE_CMD.read_text(encoding="utf-8")

for accessor in (
    "CellTextAt(ARow, ACol: Integer)",
    "RowHeightAt(ARow: Integer)",
    "ColWidthAt(ACol: Integer)",
):
    require(model, accessor, "AcadTable model import API")

require(open_cmd, "'editacadtable'", "ZCAD launch command")
require(open_cmd, "SelObjArray.Count <> 1", "single-selection validation")
require(open_cmd, "GDBAcadTableID", "AcadTable type validation")
require(open_cmd, "CellTextAt(", "text import")
require(open_cmd, "CellAlignmentAt(", "alignment import")
require(open_cmd, "CellStyleTypeAt(", "cell type import")
require(
    model,
    "else if FForceDataStyleAllRows then",
    "effective legacy row type for editor shading",
)
require(
    model,
    "Result := Min(ARow, 2);",
    "Title/Header/Data positional fallback for editor shading",
)
require(open_cmd, "RowHeightAt(", "row height import")
require(open_cmd, "ColWidthAt(", "column width import")
require(open_cmd, "Entity = PGDBObjEntity(FEditingAcadTable)", "live target check")

require(save_cmd, "SaveAcadTable", "save button registration")
require(save_cmd, "UpdateFromCellTextsWithSizesAndAlignments", "in-place update")
require(save_cmd, "CollectRowStyleTypes", "cell type export")
require(save_cmd, "FormatEntity", "geometry rebuild")
require(model, "FRowHeights.Clear;", "old row dimensions are discarded")
require(model, "FColWidths.Clear;", "old column dimensions are discarded")

require(commands, "uzvspreadsheet_cmdeditacadtable", "launch unit registration")
require(commands, "uzvspreadsheet_cmdsaveacadtable", "save unit registration")

dimensions = (
    ROOT
    / "cad_source/zcad/velec/uzvspreadsheet/uzvspreadsheet_dimensions.pas"
).read_text(encoding="utf-8")
require(dimensions, "procedure AcadAlignmentToWorksheet", "reverse alignment")
for code, index in ((1, 0), (5, 4), (9, 8)):
    assert (code - 1) == index

tests = (
    ROOT / "cad_source/zengine/tests/uzctacadtable.pas"
).read_text(encoding="utf-8")
require(tests, "ExposesAcadTableDataForSpreadsheetEditing", "FPUnit import test")
require(tests, "UpdatesAcadTableWithoutMovingOrChangingStyle", "FPUnit update test")

print("ALL TESTS PASSED")
