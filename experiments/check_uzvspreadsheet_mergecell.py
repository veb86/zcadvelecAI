#!/usr/bin/env python3
"""Static smoke test for the uzvspreadsheet merge-cell toolbar command."""

from pathlib import Path
import struct


ROOT = Path(__file__).resolve().parents[1]
SPREADSHEET_DIR = ROOT / "cad_source" / "zcad" / "velec" / "uzvspreadsheet"
IMAGES_DIR = ROOT / "cad_source" / "images" / "velec"
RUNTIME_IMAGES_DIR = (
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


def read_text(path: Path) -> str:
    if not path.exists():
        raise AssertionError(f"Missing required file: {path.relative_to(ROOT)}")
    return path.read_text(encoding="utf-8")


def require(text: str, needle: str, path: Path) -> None:
    if needle not in text:
        raise AssertionError(
            f"Missing {needle!r} in {path.relative_to(ROOT)}"
        )


def require_png(path: Path, expected_size: tuple[int, int]) -> None:
    if not path.exists():
        raise AssertionError(f"Missing runtime icon: {path.relative_to(ROOT)}")

    data = path.read_bytes()
    if data[:8] != b"\x89PNG\r\n\x1a\n":
        raise AssertionError(f"Not a PNG file: {path.relative_to(ROOT)}")

    width, height = struct.unpack(">II", data[16:24])
    if (width, height) != expected_size:
        raise AssertionError(
            f"Unexpected PNG size for {path.relative_to(ROOT)}: "
            f"{width}x{height}, expected {expected_size[0]}x{expected_size[1]}"
        )


def main() -> None:
    command_path = SPREADSHEET_DIR / "uzvspreadsheet_cmdmergecells.pas"
    actions_path = SPREADSHEET_DIR / "uzvspreadsheet_actions.pas"
    gui_path = SPREADSHEET_DIR / "uzvspreadsheet_gui.pas"
    svg_path = IMAGES_DIR / "sheet_merge_cells.svg"
    png_path = RUNTIME_IMAGES_DIR / "sheet_merge_cells.png"

    command_text = read_text(command_path)
    require(command_text, "unit uzvspreadsheet_cmdmergecells;", command_path)
    require(command_text, "procedure ExecuteMergeCells", command_path)
    require(command_text, "aWorksheetGrid.MergeCells", command_path)

    actions_text = read_text(actions_path)
    for needle in (
        "FActMergeCells: TAction;",
        "procedure OnActMergeCellsExecute(Sender: TObject);",
        "property ActMergeCells: TAction read FActMergeCells;",
        "uzvspreadsheet_cmdmergecells",
        "FActMergeCells.ImageIndex := ImagesManager.GetImageIndex('velec/sheet_merge_cells')",
        "ExecuteMergeCells(FWorkbookSource, FWorksheetGrid);",
    ):
        require(actions_text, needle, actions_path)

    gui_text = read_text(gui_path)
    for needle in (
        "FBtnMergeCells: TToolButton;",
        "FBtnMergeCells.Action := FSpreadsheetActions.ActMergeCells;",
        "FBtnMergeCells.ImageIndex := ImagesManager.GetImageIndex('velec/sheet_merge_cells')",
    ):
        require(gui_text, needle, gui_path)

    read_text(svg_path)
    require_png(png_path, (256, 256))
    print("uzvspreadsheet merge-cell command wiring is present")


if __name__ == "__main__":
    main()
