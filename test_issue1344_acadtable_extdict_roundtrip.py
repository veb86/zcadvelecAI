#!/usr/bin/env python3
"""
Documentation check for issue 1344 AcadTable AutoCAD extension dictionary round-trip.

NOTE
----
The original issue-1344 work tried to preserve and remap AutoCAD's full
extension-dictionary subtree (TABLECONTENT/TABLEGEOMETRY) verbatim on save.
That heavy approach (PR #1379 and the preceding subtree-preservation commit)
was reverted by the maintainer because it "broke a lot". The source-contract
tests that asserted the presence of that reverted machinery were therefore
removed — they referenced code that no longer exists.

What remains here is the still-valid structural documentation of an AutoCAD
gold-standard file: a split table reaches its round-trip XRECORD through the
entity's extension dictionary, and that XRECORD points back at the table's
TABLECONTENT/TABLEGEOMETRY objects. Issue 1378 reproduces the dictionary-chain
linkage minimally (without copying the whole subtree).
"""

from pathlib import Path


ROOT = Path(__file__).resolve().parent
SAMPLE_DXF = ROOT / "cad_source" / "test" / "acadtablerazdel2007_1.dxf"


def dxf_pairs(path: Path) -> list[tuple[str, str]]:
    lines = path.read_text(encoding="utf-8", errors="ignore").splitlines()
    return [(lines[i].strip(), lines[i + 1].strip()) for i in range(0, len(lines) - 1, 2)]


def objects_by_handle(path: Path) -> dict[str, list[tuple[str, str]]]:
    pairs = dxf_pairs(path)
    objects: dict[str, list[tuple[str, str]]] = {}
    i = 0
    while i < len(pairs):
        if pairs[i] == ("0", "SECTION") and i + 1 < len(pairs) and pairs[i + 1] == ("2", "OBJECTS"):
            i += 2
            while i < len(pairs) and pairs[i] != ("0", "ENDSEC"):
                if pairs[i][0] == "0":
                    start = i
                    i += 1
                    while i < len(pairs) and pairs[i][0] != "0":
                        i += 1
                    obj = pairs[start:i]
                    handle = next((value.upper() for code, value in obj if code == "5"), None)
                    if handle:
                        objects[handle] = obj
                    continue
                i += 1
        i += 1
    return objects


def test_sample_acad_table_contains_autocad_content_subtree():
    pairs = dxf_pairs(SAMPLE_DXF)
    table_start = pairs.index(("0", "ACAD_TABLE"))
    table_pairs = []
    for pair in pairs[table_start:]:
        if table_pairs and pair[0] == "0":
            break
        table_pairs.append(pair)

    ext_dict_handle = None
    for idx, pair in enumerate(table_pairs):
        if pair == ("102", "{ACAD_XDICTIONARY"):
            ext_dict_handle = table_pairs[idx + 1][1].upper()
            break
    assert ext_dict_handle == "357"

    objects = objects_by_handle(SAMPLE_DXF)
    assert objects[ext_dict_handle][0] == ("0", "DICTIONARY")
    xrecord_handle = next(value.upper() for code, value in objects[ext_dict_handle] if code == "360")
    assert objects[xrecord_handle][0] == ("0", "XRECORD")

    xrecord = objects[xrecord_handle]
    assert ("102", "ACAD_ROUNDTRIP_2008_TABLE_ENTITY") in xrecord
    assert objects[next(value.upper() for code, value in xrecord if code == "360")][0] == (
        "0",
        "TABLECONTENT",
    )
    assert objects[next(value.upper() for code, value in xrecord if code == "361")][0] == (
        "0",
        "TABLEGEOMETRY",
    )


if __name__ == "__main__":
    test_sample_acad_table_contains_autocad_content_subtree()
    print("issue 1344 AcadTable extension dictionary round-trip checks passed")
