#!/usr/bin/env python3
"""Dump AcadTable cell styles stored in a DXF (issue #1409).

Usage: python3 dump_cell_styles.py <file.dxf>

Prints, for every TABLECONTENT object found:
  * the style id of each row  (TABLEROW_BEGIN  / 90)
  * the style id of each cell (TABLECELL_BEGIN / 90)
where 1 = _TITLE, 2 = _HEADER, 3 = _DATA (see the CELLSTYLEMAP object).

This is the structure AutoCAD actually reads. Group code 172 inside the
ACAD_TABLE entity is the cell *flag* value and is ignored by AutoCAD, which
is why styles used to collapse onto whole rows.
"""

import sys
from pathlib import Path

NAMES = {1: "Title", 2: "Header", 3: "Data"}


def pairs(path):
    lines = [l.strip() for l in Path(path).read_text(
        encoding="utf-8", errors="replace").splitlines()]
    return [(lines[i], lines[i + 1]) for i in range(0, len(lines) - 1, 2)]


def main(path):
    p = pairs(path)
    content = 0
    for i, (code, value) in enumerate(p):
        if code == "0" and value == "TABLECONTENT":
            content += 1
            print(f"--- TABLECONTENT #{content} (handle {p[i + 1][1]})")
        elif value == "TABLECELL_BEGIN" and p[i + 1][0] == "90":
            sid = int(p[i + 1][1])
            print(f"  cell  style={sid} ({NAMES.get(sid, 'inherit/unknown')})")
        elif value == "TABLEROW_BEGIN" and p[i + 1][0] == "90":
            sid = int(p[i + 1][1])
            print(f"  ROW   style={sid} ({NAMES.get(sid, 'inherit/unknown')})")
    if not content:
        print("no TABLECONTENT object found - "
              "cell styles are not stored in this file")


if __name__ == "__main__":
    if len(sys.argv) != 2:
        sys.exit(__doc__)
    main(sys.argv[1])
