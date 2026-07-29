#!/usr/bin/env python3
"""Contract for the round-trip terminator of an AcadTable (issue #1409).

Symptom reported on the issue: whatever per-cell styles ZCAD writes into the
round-trip TABLECONTENT object, AutoCAD keeps painting the table with its own
positional rule (row 0 -> Title, row 1 -> Header, every other row -> Data).

Root cause: the ``ACAD_ROUNDTRIP_2008_TABLE_ENTITY`` XRECORD written by ZCAD
was truncated.  Every AutoCAD-authored reference in this repository ends that
record with a hard pointer ``361`` to a ``TABLEGEOMETRY`` object:

    XRECORD(ACAD_ROUNDTRIP_2008_TABLE_ENTITY)
        360 -> TABLECONTENT      (per-cell styles live here)
        ...
        361 -> TABLEGEOMETRY     (cell geometry cache, terminates the record)

Without the ``361`` terminator - and without the TABLEGEOMETRY object and its
CLASS declaration - AutoCAD treats the round-trip record as incomplete and
drops TABLECONTENT altogether, which is the only place a per-cell style can be
stored (the legacy AcDbTable entity has no per-cell style at all: group 172 is
the "cell flag value").

This module pins the terminator, the geometry object, its class record and the
two collateral AcDbTable fields that AutoCAD relies on while parsing.
"""

import re
from pathlib import Path

ROOT = Path(__file__).resolve().parent
WRITER = (
    ROOT
    / "cad_source"
    / "zcad"
    / "velec"
    / "acadtable"
    / "uzeacadtable_dxf_write.pas"
)
DXFOUT = ROOT / "cad_source" / "zengine" / "fileformats" / "uzeffdxfout.pas"
LAYOUT = (
    ROOT
    / "cad_source"
    / "zcad"
    / "velec"
    / "acadtable"
    / "uzeacadtable_layout.pas"
)
READER = (
    ROOT
    / "cad_source"
    / "zcad"
    / "velec"
    / "acadtable"
    / "uzeacadtable_dxf_read.pas"
)
REFERENCE_DXF = ROOT / "cad_source" / "test" / "tablebugheader.dxf"
EMPTY_CELL_DXF = ROOT / "cad_source" / "test" / "tablerazdel.dxf"


def read(path: Path) -> str:
    return path.read_text(encoding="utf-8", errors="replace")


def emitted_pairs(source: str):
    """Group code / literal value pairs emitted by the writer, in order."""
    pattern = re.compile(
        r"dxf(?:StringWithoutEncode|Integer|Double)[Oo]ut\("
        r"\s*AOutStream\s*,\s*(\d+)\s*,\s*(.+?)\s*\)\s*;",
        re.S,
    )
    return [
        (int(c), " ".join(v.split()).strip("'"))
        for c, v in pattern.findall(source)
    ]


def procedure_body(source: str, name: str) -> str:
    match = re.search(r"^(?:procedure|function)\s+" + name + r"\b", source, re.M)
    assert match, name
    body = source[match.start() :]
    end = re.search(r"\n(?:procedure|function)\s", body[20:])
    assert end, name
    return body[: 20 + end.start()]


def dxf_pairs(path: Path):
    lines = [line.strip() for line in read(path).splitlines()]
    return list(zip(lines[0::2], lines[1::2]))


def dxf_records(path: Path, record_type: str):
    records = []
    current = None
    for code, value in dxf_pairs(path):
        if code == "0":
            if current and current[0][1] == record_type:
                records.append(current)
            current = [(code, value)]
        elif current is not None:
            current.append((code, value))
    if current and current[0][1] == record_type:
        records.append(current)
    return records


# --------------------------------------------------------------------------
# Evidence: what AutoCAD itself writes.
# --------------------------------------------------------------------------


def test_reference_roundtrip_xrecord_ends_with_a_geometry_pointer():
    handles = {
        dict(record)["5"]: record[0][1]
        for record in dxf_records(REFERENCE_DXF, "TABLEGEOMETRY")
    }
    assert handles, "reference file has no TABLEGEOMETRY object"

    roundtrip = [
        record
        for record in dxf_records(REFERENCE_DXF, "XRECORD")
        if ("102", "ACAD_ROUNDTRIP_2008_TABLE_ENTITY") in record
    ]
    assert roundtrip, "reference file has no table round-trip XRECORD"
    for record in roundtrip:
        code, value = record[-1]
        assert code == "361", record[-3:]
        assert handles.get(value) == "TABLEGEOMETRY", (value, record[-3:])


def test_reference_declares_the_tablegeometry_class():
    classes = {dict(r)["1"]: dict(r) for r in dxf_records(REFERENCE_DXF, "CLASS")}
    record = classes["TABLEGEOMETRY"]
    assert record["2"] == "AcDbTableGeometry"
    assert record["3"] == "ObjectDBX Classes"
    assert record["90"] == "1152"
    assert record["280"] == "0"
    assert record["281"] == "0"


def test_reference_acdbtable_flags_and_cell_flag_value():
    pairs = dxf_pairs(REFERENCE_DXF)
    start = next(
        i for i, (c, v) in enumerate(pairs) if c == "100" and v == "AcDbTable"
    )
    tail = pairs[start:]
    # flag_for_table_value: 0x02|0x04 (has block) + 0x10 (direction down).
    flag_value = next(v for c, v in tail if c == "90")
    assert flag_value == "22", flag_value
    # Group 172 of every cell is the cell flag value and AutoCAD writes 0.
    end = next(i for i, (c, _) in enumerate(tail) if i and c == "0")
    flags = [int(v) for c, v in tail[:end] if c == "172"]
    assert flags, "no cell written in the reference table"
    assert set(flags) == {0}, sorted(set(flags))


def test_reference_encodes_an_uncached_cell_as_flag_seven_count_zero():
    """AutoCAD's own encoding for a cell whose geometry is not cached."""
    uncached = 0
    for record in dxf_records(EMPTY_CELL_DXF, "TABLEGEOMETRY"):
        codes = [c for c, _ in record]
        for i, (code, value) in enumerate(record):
            if code == "93":
                assert value == "7", value
            if code == "94" and value == "0":
                uncached += 1
        assert "94" in codes
    assert uncached, "no uncached cell in the reference file"


# --------------------------------------------------------------------------
# Contract: what ZCAD must write.
# --------------------------------------------------------------------------


def test_writer_terminates_the_roundtrip_xrecord_with_361():
    body = procedure_body(read(WRITER), "WriteAcadTableContentToDXF")
    pairs = emitted_pairs(body)
    start = next(
        i
        for i, (c, v) in enumerate(pairs)
        if c == 102 and v == "ACAD_ROUNDTRIP_2008_TABLE_ENTITY"
    )
    tail = pairs[start:]
    assert tail[1] == (360, "IntToHex(ARecord.ContentHandle, 0)"), tail[1]
    assert tail[-1] == (361, "IntToHex(ARecord.GeometryHandle, 0)"), tail[-1]
    assert tail[-2] == (90, "CAcadTableRoundTripTailValue"), tail[-2]
    assert "WriteTableGeometryObject(AOutStream, ARecord);" in body


def test_writer_emits_a_tablegeometry_object_owned_by_the_xrecord():
    source = read(WRITER)
    assert "GeometryHandle: TDWGHandle;" in source
    assert "GeometryHandle := NextAnonymousHandle(AIODXFContext);" in source

    body = procedure_body(source, "WriteTableGeometryObject")
    pairs = emitted_pairs(body)
    assert pairs[0] == (0, "TABLEGEOMETRY"), pairs[0]
    assert pairs[1] == (5, "IntToHex(ARecord.GeometryHandle, 0)"), pairs[1]
    assert pairs[2] == (330, "IntToHex(ARecord.XRecordHandle, 0)"), pairs[2]
    assert pairs[3] == (100, "AcDbTableGeometry"), pairs[3]
    assert [c for c, _ in pairs[4:7]] == [90, 91, 92]
    # One geometry block per cell, written as "not cached" the way AutoCAD
    # writes an empty cell.
    assert [c for c, _ in pairs[7:12]] == [93, 40, 41, 330, 94]
    assert pairs[7] == (93, "CAcadTableGeometryDataFlag"), pairs[7]
    assert pairs[11] == (94, "CAcadTableGeometryNoCache"), pairs[11]
    assert "CAcadTableGeometryDataFlag = 7;" in source
    assert "CAcadTableGeometryNoCache = 0;" in source
    assert "ARecord.RowCount * ARecord.ColCount - 1 do" in body


def test_writer_declares_the_tablegeometry_class():
    body = procedure_body(read(WRITER), "WriteAcadTableClassesToDXF")
    assert re.search(
        r"'TABLEGEOMETRY',\s*'AcDbTableGeometry',\s*1152,\s*"
        r"AcadTableCount,\s*0",
        body,
    ), body


def test_writer_uses_the_autocad_flag_set_for_acdbtable():
    source = read(WRITER)
    assert "CAcadTableHasBlockFlags = $02 or $04;" in source
    assert "CAcadTableDirectionDownFlag = $10;" in source
    assert re.search(
        r"CAcadTableDefaultFlags\s*=\s*"
        r"CAcadTableHasBlockFlags\s+or\s+CAcadTableDirectionDownFlag;",
        source,
    )
    header = procedure_body(source, "WriteTableHeader")
    assert (90, "TableFlagsForDXF(APart)") in emitted_pairs(header)
    flags = procedure_body(source, "TableFlagsForDXF")
    assert "Result := APart.TableFlags;" in flags
    assert "Result := CAcadTableDefaultFlags;" in flags


def test_header_suppression_does_not_read_the_has_block_bits():
    """Saving the AutoCAD flag set must not suppress the repeated top rows.

    ``BuildRenderSegments`` used to read 0x02/0x04 as "title/header
    suppressed".  Those are the "table has a block" bits, so a table written
    with the AutoCAD flag value (22 = 0x02|0x04|0x10) and read back would lose
    its repeated Title/Header rows.  Suppression lives in 0x20/0x40.
    """
    layout = read(LAYOUT)
    assert "CAcadTableTitleSuppressedFlag = $20;" in layout
    assert "CAcadTableHeaderSuppressedFlag = $40;" in layout
    body = layout[layout.index("\nimplementation") :]
    assert "(ATableFlags and CAcadTableTitleSuppressedFlag) <> 0" in body
    assert "(ATableFlags and CAcadTableHeaderSuppressedFlag) <> 0" in body
    assert "(ATableFlags and 2) <> 0" not in body
    assert "(ATableFlags and 4) <> 0" not in body

    reader = read(READER)
    assert "Ord((AData.TableFlags and 2) <> 0)" not in reader
    assert "Ord((AData.TableFlags and 4) <> 0)" not in reader
    assert "Ord((AData.TableFlags and $20) <> 0)" in reader
    assert "Ord((AData.TableFlags and $40) <> 0)" in reader

    # The value ZCAD now writes must not set any suppression bit.
    assert (0x02 | 0x04 | 0x10) & (0x20 | 0x40) == 0


def test_writer_keeps_cell_group_172_as_the_cell_flag_value():
    source = read(WRITER)
    assert "CAcadCellFlagValue = 0;" in source
    body = procedure_body(source, "WriteCell")
    pairs = emitted_pairs(body)
    written = [v for c, v in pairs if c == 172]
    assert written == ["CAcadCellFlagValue"], written
    # The style type must not leak into the entity any more.
    assert "CellStyleTypeAt" not in body


def test_tablestyle_extension_dictionary_is_hard_owner():
    body = procedure_body(read(DXFOUT), "WriteCellStyleMapObjectsToStream")
    idx = body.index("'AcDbDictionary'")
    window = body[idx : idx + 600]
    assert re.search(r"dxfPairOut\(outstream,\s*280,\s*'1'\)", window), window
    assert re.search(r"dxfPairOut\(outstream,\s*281,\s*'1'\)", window), window
    assert "ACAD_ROUNDTRIP_2008_TABLESTYLE_CELLSTYLEMAP" in body


def main():
    failures = 0
    for name, fn in sorted(globals().items()):
        if name.startswith("test_") and callable(fn):
            try:
                fn()
            except (AssertionError, KeyError, StopIteration, ValueError) as exc:
                failures += 1
                print(f"FAILED {name}: {exc!r}")
            else:
                print(f"ok {name}")
    if failures:
        raise SystemExit(f"{failures} test(s) failed")
    print("ALL TESTS PASSED")


if __name__ == "__main__":
    main()
