#!/usr/bin/env python3
"""Issue #1339 (Bug 2) — prove the cause of "table text invisible in AutoCAD
until a transform forces a regen".

We compare the original AutoCAD-saved DXF against the ZCAD re-saved DXF
(both present in cad_source/test/) and show that ZCAD drops the table's
extension-dictionary content subtree:

    ACAD_TABLE (entity, handle EB)
      └─ 360 xdict ─▶ DICTIONARY (357)
                        └─ ACAD_XREC_ROUNDTRIP ─▶ XRECORD (358)
                                                    └─ TABLECONTENT (2B5)   <- cell data AutoCAD renders
                        (and)                       TABLEGEOMETRY (2B6)      <- cached geometry

AutoCAD 2010+ renders table cell text from the AcDbTableContent object. When
it is missing, AutoCAD draws the grid but no text until a regen (triggered by
move/rotate/scale) rebuilds the display — exactly the reported symptom.

The subtree carries NO binary (group 310) data and only three kinds of
external handle references:
  * 330 -> ACAD_TABLE entities (handles ZCAD already preserves: EB/2B7/307)
  * 340 -> text STYLE  "Standard"   (renumbered by ZCAD on save)
  * 340 -> TABLESTYLE  "Standard"   (renumbered by ZCAD on save)
so preserving it through round-trip is a bounded, well-specified change.

This script asserts the *current* (buggy) state: the AutoCAD file has the
subtree, the ZCAD file does not. Once the fix lands and a real ZCAD build
re-saves the file, re-running this against the new output should show the
subtree preserved (flip EXPECT_PRESERVED to True for that check).
"""
import sys, os
from collections import Counter

EXPECT_PRESERVED = False  # current ZCAD output drops the subtree (the bug)


def type_counts(path):
    with open(path, 'r', errors='replace') as f:
        L = [l.rstrip('\n').rstrip('\r') for l in f]
    c = Counter()
    for i in range(0, len(L) - 1, 2):
        if L[i].strip() == '0':
            c[L[i + 1].strip()] += 1
    return c


def main():
    base = os.path.join(os.path.dirname(__file__), '..', 'cad_source', 'test')
    ok = True
    for n in (1, 2, 3, 4):
        acad = type_counts(os.path.join(base, f'acadtablerazdel2007_{n}.dxf'))
        zc_path = os.path.join(base, f'zcadtablerazdel2007_{n}.dxf')
        if not os.path.exists(zc_path):
            print(f"file {n}: ZCAD re-saved file missing, skipping")
            continue
        zc = type_counts(zc_path)
        a_content = acad.get('TABLECONTENT', 0)
        a_geom = acad.get('TABLEGEOMETRY', 0)
        z_content = zc.get('TABLECONTENT', 0)
        z_geom = zc.get('TABLEGEOMETRY', 0)

        # AutoCAD original must contain the rendering subtree.
        has_subtree_acad = a_content >= 1 and a_geom >= 1
        # ZCAD output: currently drops it (bug); after fix should preserve it.
        zc_preserves = z_content >= 1 and z_geom >= 1

        good = has_subtree_acad and (zc_preserves == EXPECT_PRESERVED)
        ok = ok and good
        print(f"file {n}: AutoCAD TABLECONTENT={a_content} TABLEGEOMETRY={a_geom} | "
              f"ZCAD TABLECONTENT={z_content} TABLEGEOMETRY={z_geom} "
              f"-> {'OK' if good else 'UNEXPECTED'}")

    verdict = ("confirmed: ZCAD currently DROPS the table content subtree "
               "(Bug 2 root cause)") if not EXPECT_PRESERVED else \
              "verified: ZCAD now preserves the table content subtree"
    print(verdict if ok else "UNEXPECTED RESULT")
    sys.exit(0 if ok else 1)


if __name__ == '__main__':
    main()
