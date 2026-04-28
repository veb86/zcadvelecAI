#!/usr/bin/env python3
"""Regression check for issue #1018 restore-table grid detection.

The test parses the issue DXF fixture and mirrors the table recovery line
extraction/grid rules from ucvrtdata.pas, ucvrtanalyzer.pas, and
ucvrtbuilder.pas. It catches absolute tolerance/min-cell thresholds that are
too large for compact CAD tables.
"""

from __future__ import annotations

import re
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
DATA_FILE = ROOT / "cad_source/test/recoverytablenotwork.dxf"
CONSTANTS_FILE = ROOT / "cad_source/zcad/velec/ucvrestoretable/ucvrtdata.pas"


def read_pascal_constants() -> dict[str, float]:
    text = CONSTANTS_FILE.read_text(encoding="utf-8", errors="replace")
    constants: dict[str, float] = {}
    for name in (
        "MIN_CELL_WIDTH",
        "MIN_CELL_HEIGHT",
        "MIN_TABLE_LINE_LENGTH",
        "COORDINATE_TOLERANCE",
        "LINE_ORIENTATION_TOLERANCE",
    ):
        match = re.search(rf"\b{name}\s*=\s*([0-9]+(?:\.[0-9]+)?)\s*;", text)
        if match:
            constants[name] = float(match.group(1))

    # Backward-compatible fallbacks make this script demonstrate the pre-fix
    # failure before the dedicated line/cell thresholds exist.
    constants.setdefault("MIN_TABLE_LINE_LENGTH", constants["MIN_CELL_HEIGHT"])
    constants.setdefault(
        "LINE_ORIENTATION_TOLERANCE", constants["COORDINATE_TOLERANCE"]
    )
    return constants


def iter_dxf_pairs(path: Path):
    lines = path.read_text(encoding="utf-8", errors="replace").splitlines()
    for i in range(0, len(lines) - 1, 2):
        try:
            code = int(lines[i].strip())
        except ValueError:
            continue
        yield code, lines[i + 1].strip()


def iter_entities(path: Path):
    current = None
    for code, value in iter_dxf_pairs(path):
        if code == 0:
            if current is not None:
                yield current
            current = {"type": value, "data": []}
        elif current is not None:
            current["data"].append((code, value))
    if current is not None:
        yield current


def parse_line_entities(path: Path) -> list[tuple[float, float, float, float]]:
    lines = []
    for entity in iter_entities(path):
        if entity["type"] != "LINE":
            continue
        values = {}
        for code, value in entity["data"]:
            if code in (10, 20, 11, 21):
                values[code] = float(value)
        if all(code in values for code in (10, 20, 11, 21)):
            lines.append((values[10], values[20], values[11], values[21]))
    return lines


def unique_positions(positions: list[float], tolerance: float) -> list[float]:
    result: list[float] = []
    for position in positions:
        if not any(abs(existing - position) <= tolerance for existing in result):
            result.append(position)
    return sorted(result)


def build_grid_counts(
    lines: list[tuple[float, float, float, float]],
    constants: dict[str, float],
):
    horizontal_positions = []
    vertical_positions = []
    orientation_tolerance = constants["LINE_ORIENTATION_TOLERANCE"]
    min_line_length = constants["MIN_TABLE_LINE_LENGTH"]

    for x1, y1, x2, y2 in lines:
        dx = abs(x2 - x1)
        dy = abs(y2 - y1)
        if dy < orientation_tolerance and dx > min_line_length:
            horizontal_positions.append(y1)
        if dx < orientation_tolerance and dy > min_line_length:
            vertical_positions.append(x1)

    unique_y = unique_positions(horizontal_positions, constants["COORDINATE_TOLERANCE"])
    unique_x = unique_positions(vertical_positions, constants["COORDINATE_TOLERANCE"])

    row_count = sum(
        1
        for bottom, top in zip(unique_y, unique_y[1:])
        if top - bottom >= constants["MIN_CELL_HEIGHT"]
    )
    column_count = sum(
        1
        for left, right in zip(unique_x, unique_x[1:])
        if right - left >= constants["MIN_CELL_WIDTH"]
    )
    return len(horizontal_positions), len(vertical_positions), row_count, column_count


def main() -> None:
    constants = read_pascal_constants()
    lines = parse_line_entities(DATA_FILE)
    horizontal_count, vertical_count, row_count, column_count = build_grid_counts(
        lines, constants
    )

    print(f"line entities: {len(lines)}")
    print(f"horizontal lines: {horizontal_count}")
    print(f"vertical lines: {vertical_count}")
    print(f"rows: {row_count}")
    print(f"columns: {column_count}")

    assert len(lines) == 15
    assert horizontal_count == 6
    assert vertical_count == 6
    assert row_count == 5
    assert column_count == 5


if __name__ == "__main__":
    main()
