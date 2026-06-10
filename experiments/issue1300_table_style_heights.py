#!/usr/bin/env python3
"""
issue #1300 — AcadTable: split table continuation parts ignore table style.

A horizontally split table is stored in DXF as several ACAD_TABLE entities
that all reference the SAME table style (group code 342). ZCAD merges them
into one AcadTable object (main part + continuation parts). The bug: only the
main part had ApplyDXFTableStyle() applied; continuation parts kept the default
TTableStyle (CAcadTableDefaultTextHeight = 2.5), so their cell text rendered
giant and misaligned ("текст разъехался").

This script confirms, straight from the sample DXF, that:
  * all ACAD_TABLE parts share one style handle, and
  * that style's text heights are far below the 2.5 default,
which is exactly what the regression test threshold (MaxSize < 2.5) relies on.

Pure-stdlib DXF group reader — no external deps.
"""
import sys
from pathlib import Path

DXF = Path(__file__).resolve().parents[1] / "cad_source/test/tablerazdel.dxf"
DEFAULT_TEXT_HEIGHT = 2.5  # CAcadTableDefaultTextHeight (uzeacadtable_types)


def read_groups(path):
    lines = path.read_text(errors="replace").splitlines()
    for i in range(0, len(lines) - 1, 2):
        yield lines[i].strip(), lines[i + 1].strip()


def main():
    if not DXF.exists():
        sys.exit(f"sample not found: {DXF}")

    groups = list(read_groups(DXF))

    # Collect the style handle (342) of every ACAD_TABLE entity.
    table_style_handles = []
    in_entities = False
    cur_type = None
    for code, val in groups:
        if code == "2" and val == "ENTITIES":
            in_entities = True
        if code == "2" and val == "OBJECTS":
            in_entities = False
        if not in_entities:
            continue
        if code == "0":
            cur_type = val
        if code == "342" and cur_type == "ACAD_TABLE":
            table_style_handles.append(val)

    # Collect TABLESTYLE text heights (group 140) per style handle (group 5).
    style_heights = {}
    cur_type = None
    cur_handle = None
    for code, val in groups:
        if code == "0":
            cur_type = val
            cur_handle = None
        if cur_type == "TABLESTYLE" and code == "5":
            cur_handle = val
            style_heights.setdefault(cur_handle, [])
        if cur_type == "TABLESTYLE" and code == "140" and cur_handle:
            style_heights[cur_handle].append(float(val))

    print(f"ACAD_TABLE entities: {len(table_style_handles)}")
    print(f"  style handles (342): {table_style_handles}")
    print(f"TABLESTYLE heights (140): {style_heights}")

    assert len(table_style_handles) >= 2, "expected a split table (>=2 parts)"
    assert len(set(table_style_handles)) == 1, \
        "all split parts must share one table style handle"

    handle = table_style_handles[0]
    heights = style_heights.get(handle, [])
    assert heights, f"no TABLESTYLE heights for handle {handle}"
    assert max(heights) < DEFAULT_TEXT_HEIGHT, (
        f"style heights {heights} must be below the {DEFAULT_TEXT_HEIGHT} "
        "default — otherwise the regression test threshold is meaningless"
    )

    print(
        f"OK: {len(table_style_handles)} parts share style '{handle}'; "
        f"max text height {max(heights)} < default {DEFAULT_TEXT_HEIGHT}. "
        "Continuation parts must therefore render below 2.5 once the style "
        "is applied to every part (the fix)."
    )


if __name__ == "__main__":
    main()
