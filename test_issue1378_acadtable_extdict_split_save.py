#!/usr/bin/env python3
"""
Regression checks for issue 1378: split ACAD_TABLE saved to DXF 2007 opens
in AutoCAD as one whole "long sheet" instead of a broken table.

Root cause
----------
When ZCAD saves a broken table to DXF 2007 it splits it into several
ACAD_TABLE entities and emits an ACAD_ROUNDTRIP_2008_TABLE_ENTITY XRECORD that
carries the break height. In the buggy output that XRECORD was written as an
*orphan* object (``330 0``) with no extension dictionary on the main table
entity, so AutoCAD never reached it. The entity break flag (``292 1``) still
makes AutoCAD show the break-height grip, but without the XRECORD the break
height is lost and the segments are merged into one long table — exactly the
reported symptom.

AutoCAD attaches the very same XRECORD through the main entity's extension
dictionary:

    ACAD_TABLE  --(102 {ACAD_XDICTIONARY 360})-->  DICTIONARY
    DICTIONARY  --(3 ACAD_XREC_ROUNDTRIP 360)  -->  XRECORD
    XRECORD     --(330 owner)                   -->  DICTIONARY

The fix makes ZCAD emit that chain for the main part of a broken table.
"""

from pathlib import Path


ROOT = Path(__file__).resolve().parent
SOURCE_DXF = ROOT / "cad_source" / "test" / "tablebugheader.dxf"
BROKEN_SAVE_DXF = ROOT / "cad_source" / "test" / "tablebugheader3.dxf"
ACADTABLE_MODEL = (
    ROOT / "cad_source" / "zcad" / "velec" / "acadtable" / "uzeacadtable_model.pas"
)
ACADTABLE_WRITER = (
    ROOT / "cad_source" / "zcad" / "velec" / "acadtable" / "uzeacadtable_dxf_write.pas"
)


def read_text(path: Path) -> str:
    return path.read_text(encoding="utf-8", errors="replace")


def compact(text: str) -> str:
    return "".join(text.split()).lower()


def dxf_pairs(path: Path) -> list[tuple[str, str]]:
    lines = read_text(path).splitlines()
    return [
        (lines[i].strip(), lines[i + 1].strip())
        for i in range(0, len(lines) - 1, 2)
    ]


def dxf_entity_counts(path: Path) -> dict[str, int]:
    counts: dict[str, int] = {}
    for code, value in dxf_pairs(path):
        if code == "0":
            name = value.upper()
            counts[name] = counts.get(name, 0) + 1
    return counts


def objects_by_handle(path: Path) -> dict[str, list[tuple[str, str]]]:
    """Group every ``0``-delimited object by its first ``5`` handle value."""
    pairs = dxf_pairs(path)
    objects: dict[str, list[tuple[str, str]]] = {}
    i = 0
    while i < len(pairs):
        if pairs[i][0] == "0":
            start = i
            i += 1
            while i < len(pairs) and pairs[i][0] != "0":
                i += 1
            obj = pairs[start:i]
            handle = next((v.upper() for c, v in obj if c == "5"), None)
            if handle and handle not in objects:
                objects[handle] = obj
            continue
        i += 1
    return objects


def extension_dictionary_handle(obj: list[tuple[str, str]]) -> str | None:
    for idx, pair in enumerate(obj):
        if pair == ("102", "{ACAD_XDICTIONARY"):
            return obj[idx + 1][1].upper()
    return None


def test_fixture_shows_split_save_root_cause():
    """The source is one table; the broken save explodes it into several."""
    source_counts = dxf_entity_counts(SOURCE_DXF)
    broken_counts = dxf_entity_counts(BROKEN_SAVE_DXF)

    assert source_counts["ACAD_TABLE"] == 1
    assert source_counts["TABLECONTENT"] == 1
    assert source_counts["TABLEGEOMETRY"] == 1

    assert broken_counts["ACAD_TABLE"] > source_counts["ACAD_TABLE"]
    assert broken_counts.get("TABLECONTENT", 0) == 0
    assert broken_counts.get("TABLEGEOMETRY", 0) == 0


def test_source_table_links_roundtrip_through_extension_dictionary():
    """AutoCAD's own file reaches the round-trip XRECORD via the entity's
    extension dictionary — the structure the fix reproduces."""
    objects = objects_by_handle(SOURCE_DXF)

    table = next(
        obj for obj in objects.values() if obj and obj[0] == ("0", "ACAD_TABLE")
    )
    dict_handle = extension_dictionary_handle(table)
    assert dict_handle is not None, "source table must carry ACAD_XDICTIONARY"

    dictionary = objects[dict_handle]
    assert dictionary[0] == ("0", "DICTIONARY")
    assert ("3", "ACAD_XREC_ROUNDTRIP") in dictionary
    # The dictionary is owned by the table entity it belongs to.
    table_handle = table[next(i for i, p in enumerate(table) if p[0] == "5")][1].upper()
    assert ("330", table_handle) in dictionary

    xrec_handle = next(v.upper() for c, v in dictionary if c == "360")
    xrecord = objects[xrec_handle]
    assert xrecord[0] == ("0", "XRECORD")
    assert ("102", "ACAD_ROUNDTRIP_2008_TABLE_ENTITY") in xrecord
    # The XRECORD is owned by the dictionary, not orphaned.
    assert ("330", dict_handle) in xrecord


def test_broken_save_has_orphan_roundtrip_without_extension_dictionary():
    """Documents the buggy output that the fix must eliminate: the round-trip
    XRECORD is orphaned (``330 0``) and no table carries ACAD_XDICTIONARY."""
    pairs = dxf_pairs(BROKEN_SAVE_DXF)
    assert ("102", "{ACAD_XDICTIONARY") not in pairs

    objects = objects_by_handle(BROKEN_SAVE_DXF)
    xrecord = next(
        obj
        for obj in objects.values()
        if ("102", "ACAD_ROUNDTRIP_2008_TABLE_ENTITY") in obj
    )
    # Orphan owner — AutoCAD never reaches the break height stored here.
    assert ("330", "0") in xrecord


def test_writer_attaches_roundtrip_through_extension_dictionary():
    """The writer now emits the entity ACAD_XDICTIONARY, the owning DICTIONARY
    and a dictionary-owned XRECORD instead of an orphan record."""
    writer = compact(read_text(ACADTABLE_WRITER))

    # Per-part flag and dictionary bookkeeping.
    assert "hascontinuations:boolean" in writer
    assert "registermainentitydict(" in writer
    assert "lookupmainentitydict(" in writer

    # ACAD_XDICTIONARY reference written on the main entity.
    assert "dxfstringwithoutencodeout(aoutstream,102,'{acad_xdictionary')" in writer

    # The owning DICTIONARY with the ACAD_XREC_ROUNDTRIP entry.
    assert "dxfstringwithoutencodeout(aoutstream,0,'dictionary')" in writer
    assert "dxfstringwithoutencodeout(aoutstream,3,'acad_xrec_roundtrip')" in writer

    # The XRECORD is owned by the dictionary (with reactors), no longer orphaned.
    assert "dxfstringwithoutencodeout(aoutstream,102,'{acad_reactors')" in writer
    # The orphan fallback (330 '0') only remains when no dictionary was made.
    assert "if hasdict then" in compact_keep_spaces(read_text(ACADTABLE_WRITER))


def test_model_marks_main_part_with_continuations():
    """The model flags the main write-part when continuations exist so the
    writer knows to emit the extension dictionary."""
    model = compact(read_text(ACADTABLE_MODEL))
    assert "apart.hascontinuations:=length(fcontinuationparts)>0" in model
    assert "apart.hascontinuations:=false" in model


def compact_keep_spaces(text: str) -> str:
    """Lowercase + collapse runs of whitespace to single spaces (keeps word
    boundaries for multi-token Pascal phrases)."""
    return " ".join(text.split()).lower()


if __name__ == "__main__":
    test_fixture_shows_split_save_root_cause()
    test_source_table_links_roundtrip_through_extension_dictionary()
    test_broken_save_has_orphan_roundtrip_without_extension_dictionary()
    test_writer_attaches_roundtrip_through_extension_dictionary()
    test_model_marks_main_part_with_continuations()
    print("issue 1378 AcadTable split extension dictionary checks passed")
