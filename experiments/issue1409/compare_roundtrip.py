#!/usr/bin/env python3
"""Compare a ZCAD-saved DXF against the same file re-saved by AutoCAD.

Usage: python3 compare_roundtrip.py <ours.dxf> <autocad.dxf>

This is the tool used to find the root cause of issue #1409.  It reports, for
both files:

  * whether a CELLSTYLEMAP object exists (the object that *defines* what cell
    style ids 1/2/3 mean — without it AutoCAD throws the per-cell styles away
    and falls back to "row 1 Title, row 2 Header, rest Data");
  * the reference groups of every ACAD_TABLE entity (330 owner, 342 table
    style, 343 anonymous block);
  * the row and cell style ids stored in each TABLECONTENT.
"""

import sys
from collections import Counter
from pathlib import Path

NAMES = {1: "Title", 2: "Header", 3: "Data"}


def pairs(path):
    lines = [l.strip() for l in Path(path).read_text(
        encoding="utf-8", errors="replace").splitlines()]
    return [(lines[i], lines[i + 1]) for i in range(0, len(lines) - 1, 2)]


def report(path):
    p = pairs(path)
    print(f"=== {path}")
    kinds = Counter(v for c, v in p if c == "0")
    for name in ("ACAD_TABLE", "TABLESTYLE", "CELLSTYLEMAP", "TABLECONTENT"):
        print(f"  {name:<14} {kinds.get(name, 0)}")

    for i, (code, value) in enumerate(p):
        if code == "0" and value == "ACAD_TABLE":
            refs = {}
            for c, v in p[i + 1:i + 60]:
                if c == "0":
                    break
                if c in ("330", "342", "343"):
                    refs.setdefault(c, v)
            print(f"  ACAD_TABLE handle={p[i + 1][1]} refs={refs}")

    content = 0
    rows, cells = [], []
    for i, (code, value) in enumerate(p):
        if code == "0" and value == "TABLECONTENT":
            if content:
                print(f"    rows={rows} cells={cells}")
            content += 1
            rows, cells = [], []
            print(f"  TABLECONTENT #{content} handle={p[i + 1][1]}")
        elif value == "TABLEROW_BEGIN" and p[i + 1][0] == "90":
            rows.append(int(p[i + 1][1]))
        elif value == "TABLECELL_BEGIN" and p[i + 1][0] == "90":
            cells.append(int(p[i + 1][1]))
    if content:
        print(f"    rows={rows} cells={cells}")
        print("    legend: " + ", ".join(f"{k}={v}" for k, v in NAMES.items()))


def main(argv):
    if not argv:
        sys.exit(__doc__)
    for path in argv:
        report(path)


if __name__ == "__main__":
    main(sys.argv[1:])
