#!/usr/bin/env python3
"""
Regression checks for the split ACAD_TABLE DXF save path (issues #1378 / #1381).

History
-------
Issue #1378 first reported that a broken (split) ACAD_TABLE saved to DXF 2007
opened in AutoCAD as one whole "long sheet" instead of a broken table. The
initial fix tried to reproduce AutoCAD's own round-trip chain
(``ACAD_XDICTIONARY`` -> ``DICTIONARY`` -> ``ACAD_ROUNDTRIP_2008_TABLE_ENTITY``
XRECORD carrying the break height) so AutoCAD would rebuild the break.

That approach was intentionally abandoned for issue #1381: making AutoCAD read a
*correct* broken table is very hard and, more importantly, undesirable — a valid
round-trip table is merged back into one object by AutoCAD, which is the opposite
of what the user wants. The goal is simply that the drawing *looks* in AutoCAD
exactly as it was saved in ZCAD: the split table must appear as several separate
tables.

Current strategy (issue #1381)
------------------------------
ZCAD writes each part of a broken table as an independent ``ACAD_TABLE`` entity
and, on the MODEL write path (used once the original raw DXF is invalidated by an
edit), generates a per-part anonymous block ``*T<N>`` that contains that part's
LINE + MTEXT geometry rendered at the local origin. Each entity references its
block by name (group code 2) and by BLOCK_RECORD handle (group code 343):

    ACAD_TABLE  --(2  *T<N>)-->  BLOCK  (LINE + MTEXT geometry of the part)
    ACAD_TABLE  --(343 handle)-> BLOCK_RECORD of that block

AutoCAD renders proxy/round-trip ``AcDbTable`` entities from their associated
anonymous block, so with a valid per-part block + group 343 every part draws its
own geometry and the parts appear as separate tables — matching ZCAD. Without
them (the old MODEL output) the continuation parts referenced non-existent blocks
and opened empty, which is exactly the symptom reported on issue #1381.

ZCAD's own split marker (a private ``ZCAD_SPLIT_TABLE_ENTITY`` XRECORD) is kept
so ZCAD still reloads the file as a single broken table (functionality
preserved); the AutoCAD-recognised ``ACAD_ROUNDTRIP_2008_TABLE_ENTITY`` marker is
deliberately NOT emitted.
"""

from pathlib import Path


ROOT = Path(__file__).resolve().parent
SOURCE_DXF = ROOT / "cad_source" / "test" / "tablebugheader.dxf"
# Corrected split save produced by the MODEL write path (issue #1381). Every part
# is an ACAD_TABLE that references its own generated block via group 2 + group 343.
SPLIT_SAVE_DXF = ROOT / "cad_source" / "test" / "tablebugheader3.dxf"
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


def acad_tables(path: Path) -> list[list[tuple[str, str]]]:
    """Every ACAD_TABLE entity as its raw list of group-code/value pairs."""
    pairs = dxf_pairs(path)
    tables: list[list[tuple[str, str]]] = []
    i = 0
    while i < len(pairs):
        if pairs[i] == ("0", "ACAD_TABLE"):
            start = i
            i += 1
            while i < len(pairs) and pairs[i][0] != "0":
                i += 1
            tables.append(pairs[start:i])
            continue
        i += 1
    return tables


def block_geometry_counts(path: Path) -> dict[str, tuple[int, int]]:
    """Map block name -> (LINE count, MTEXT count) for every ``*T`` block."""
    pairs = dxf_pairs(path)
    counts: dict[str, tuple[int, int]] = {}
    i = 0
    while i < len(pairs):
        if pairs[i] == ("0", "BLOCK"):
            j = i + 1
            name = None
            while j < len(pairs) and pairs[j][0] != "0":
                if pairs[j][0] == "2" and name is None:
                    name = pairs[j][1]
                j += 1
            lines = mtexts = 0
            while j < len(pairs) and pairs[j] != ("0", "ENDBLK"):
                if pairs[j] == ("0", "LINE"):
                    lines += 1
                elif pairs[j] == ("0", "MTEXT"):
                    mtexts += 1
                j += 1
            if name and name.startswith("*T"):
                counts[name] = (lines, mtexts)
            i = j
            continue
        i += 1
    return counts


def block_record_handles(path: Path) -> dict[str, str]:
    """Map block name -> BLOCK_RECORD handle (from the OBJECTS/TABLES section)."""
    pairs = dxf_pairs(path)
    handles: dict[str, str] = {}
    i = 0
    while i < len(pairs):
        if pairs[i] == ("0", "BLOCK_RECORD"):
            j = i + 1
            handle = name = None
            while j < len(pairs) and pairs[j][0] != "0":
                if pairs[j][0] == "5" and handle is None:
                    handle = pairs[j][1].upper()
                if pairs[j][0] == "2" and name is None:
                    name = pairs[j][1]
                j += 1
            if name and handle and name not in handles:
                handles[name] = handle
            i = j
            continue
        i += 1
    return handles


def test_fixture_shows_split_save_root_cause():
    """The source is one table; the split save explodes it into several."""
    source_counts = dxf_entity_counts(SOURCE_DXF)
    split_counts = dxf_entity_counts(SPLIT_SAVE_DXF)

    assert source_counts["ACAD_TABLE"] == 1
    assert source_counts["TABLECONTENT"] == 1
    assert source_counts["TABLEGEOMETRY"] == 1

    assert split_counts["ACAD_TABLE"] > source_counts["ACAD_TABLE"]
    assert split_counts.get("TABLECONTENT", 0) == 0
    assert split_counts.get("TABLEGEOMETRY", 0) == 0


def test_source_table_links_roundtrip_through_extension_dictionary():
    """AutoCAD's own source file reaches its round-trip XRECORD via the entity's
    extension dictionary — kept as documentation of AutoCAD's native structure."""
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


def test_split_save_renders_each_part_from_its_own_block():
    """Issue #1381: every part of the split save references its own generated
    block by name (group 2) and BLOCK_RECORD handle (group 343), and that block
    exists with real geometry — so AutoCAD draws every part, not just the first."""
    tables = acad_tables(SPLIT_SAVE_DXF)
    assert len(tables) > 1, "split save must contain several ACAD_TABLE parts"

    geometry = block_geometry_counts(SPLIT_SAVE_DXF)
    records = block_record_handles(SPLIT_SAVE_DXF)

    for table in tables:
        # group 2 is the associated anonymous block name.
        block_name = next(v for c, v in table if c == "2")
        assert block_name.startswith("*T"), block_name

        # group 343 points at that block's BLOCK_RECORD.
        handle_343 = next((v.upper() for c, v in table if c == "343"), None)
        assert handle_343 is not None, f"{block_name}: missing group 343"
        assert records.get(block_name) == handle_343, (
            f"{block_name}: group 343 {handle_343} must match its BLOCK_RECORD "
            f"handle {records.get(block_name)}"
        )

        # The block exists and actually carries the part's geometry.
        assert block_name in geometry, f"{block_name}: block definition missing"
        lines, mtexts = geometry[block_name]
        assert lines > 0 and mtexts > 0, (
            f"{block_name}: block must contain LINE + MTEXT geometry, got "
            f"{lines} lines / {mtexts} mtexts"
        )


def test_split_save_keeps_private_marker_and_no_autocad_roundtrip():
    """ZCAD's private split marker is kept (so ZCAD reloads it as one broken
    table) while the AutoCAD-recognised round-trip marker is deliberately absent
    (so AutoCAD keeps the parts separate instead of merging them)."""
    text = read_text(SPLIT_SAVE_DXF)
    assert text.count("ZCAD_SPLIT_TABLE_ENTITY") >= 1
    assert text.count("ACAD_ROUNDTRIP_2008_TABLE_ENTITY") == 0


def test_writer_emits_block_name_and_block_record_handle():
    """The writer emits the per-part block name (group 2) and looks up its
    BLOCK_RECORD handle (group 343) from the name->handle map."""
    writer = compact(read_text(ACADTABLE_WRITER))

    # Per-part block name field and the group-2 emission.
    assert "blockname:string;" in writer
    assert "dxfstringwithoutencodeout(aoutstream,2,blockname);" in writer

    # group 343 resolved from the BLOCK_RECORD name->handle map.
    assert "blocknamehandlemap.mygetvalue(" in writer
    assert "dxfstringwithoutencodeout(aoutstream,343,blockrecordhandle);" in writer


def test_model_generates_per_part_blocks_before_save():
    """The model generates a geometry block for each part before save and
    propagates its name onto the write-part records."""
    model = compact(read_text(ACADTABLE_MODEL))
    assert "procedureensuresplitpartblocks(" in model
    assert "generateuniquetableblockname" in model
    # Main part and continuation parts both carry their generated block name.
    assert "apart.blockname:=fmainpartblockname;" in model
    assert "registerbeforesavedxfproc(@ensureacadtablesplitblocksbeforesave);" in model


if __name__ == "__main__":
    test_fixture_shows_split_save_root_cause()
    test_source_table_links_roundtrip_through_extension_dictionary()
    test_split_save_renders_each_part_from_its_own_block()
    test_split_save_keeps_private_marker_and_no_autocad_roundtrip()
    test_writer_emits_block_name_and_block_record_handle()
    test_model_generates_per_part_blocks_before_save()
    print("issue 1378/1381 AcadTable split save checks passed")
