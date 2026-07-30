#!/usr/bin/env python3
"""Contract for per-cell AcadTable styles written into DXF (issue #1409).

The style of a cell cannot be stored in the ACAD_TABLE entity itself: group
code 172 is the "cell flag value", not the style, so AutoCAD ignores it and
falls back to its own rule (row 0 -> Title, row 1 -> Header, rest -> Data).
The real per-cell style lives in the AutoCAD 2008 round-trip TABLECONTENT
object reached through the entity extension dictionary:

    ACAD_TABLE -> {ACAD_XDICTIONARY -> DICTIONARY(ACAD_XREC_ROUNDTRIP)
               -> XRECORD(ACAD_ROUNDTRIP_2008_TABLE_ENTITY) -> TABLECONTENT

inside which ``TABLEROW_BEGIN / 90`` holds the row style id and
``TABLECELL_BEGIN / 90`` holds the individual style id of one cell
(1 = _TITLE, 2 = _HEADER, 3 = _DATA).
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
REFERENCE_DXF = ROOT / "cad_source" / "test" / "tablebugheader.dxf"


def read(path: Path) -> str:
    return path.read_text(encoding="utf-8", errors="replace")


def dxf_pairs(path: Path):
    lines = [line.strip() for line in read(path).splitlines()]
    return list(zip(lines[0::2], lines[1::2]))


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


def procedure_source(source: str, name: str, next_name: str) -> str:
    start = source.index(f"procedure {name}")
    end = source.index(f"procedure {next_name}", start)
    return source[start:end]


def test_entity_references_extension_dictionary():
    source = read(WRITER)
    assert "'{ACAD_XDICTIONARY'" in source
    assert "AddAcadTableContentRecord(" in source
    assert "'ACAD_XREC_ROUNDTRIP'" in source
    assert "'ACAD_ROUNDTRIP_2008_TABLE_ENTITY'" in source
    assert "'TABLECONTENT'" in source


def test_cell_style_id_mapping_is_title_header_data():
    source = read(WRITER)
    assert "CAcadCellStyleIdTitle = 1;" in source
    assert "function AcadCellStyleId(AStyleType: Integer): Integer;" in source
    assert "Result := AStyleType + CAcadCellStyleIdTitle;" in source


def test_each_cell_gets_its_own_style_id():
    source = read(WRITER)
    pairs = emitted_pairs(source)
    idx = [i for i, (c, v) in enumerate(pairs) if v == "TABLECELL_BEGIN"]
    assert idx, "TABLECELL_BEGIN is never written"
    for i in idx:
        assert pairs[i + 1] == (90, "ACellStyleId"), pairs[i + 1]

    idx = [i for i, (c, v) in enumerate(pairs) if v == "TABLEROW_BEGIN"]
    assert idx, "TABLEROW_BEGIN is never written"
    for i in idx:
        assert pairs[i + 1][0] == 90
        assert "RowStyleIds" in pairs[i + 1][1], pairs[i + 1]

    # The style id of a cell is taken per cell, not per row.
    assert "CellStyleIds[RowIdx * ColCount + ColIdx] :=" in source
    assert "AcadCellStyleId(CellStyleTypeAt(APart, RowIdx, ColIdx));" in source


def test_explicit_cell_style_uses_materialized_autocad_cell_format():
    """The TABLEFORMAT containing a cell style id must be active.

    AutoCAD's issue attachment ``test3acad.txt`` retains the requested
    Title/Header/Data ids only when FORMATTEDTABLEDATACELL contains this
    materialized payload.  ZCAD used to write only ``90=1, 170=0`` (an
    inactive/inherited format) next to an explicit TABLECELL style id, so
    AutoCAD discarded the id and fell back to row position.
    """
    source = read(WRITER)
    formatter = procedure_source(
        source, "WriteCellTableFormat", "WriteTableContentCell"
    )
    assert emitted_pairs(formatter) == [
        (1, "TABLEFORMAT_BEGIN"),
        (90, "1"),
        (170, "1"),
        (91, "0"),
        (92, "0"),
        (62, "257"),
        (93, "1"),
        (171, "0"),
        (94, "0"),
        (309, "TABLEFORMAT_END"),
    ]
    assert "WriteContentFormat(AOutStream, 0, 512);" in formatter

    cell_writer = procedure_source(
        source, "WriteTableContentCell", "WriteTableContentObject"
    )
    assert "WriteContentFormat(AOutStream, 3, 4);" in cell_writer
    assert "WriteCellTableFormat(AOutStream);" in cell_writer
    assert "WriteEmptyTableFormat(AOutStream, 1);" not in cell_writer


def test_tablecontent_markers_are_balanced():
    source = read(WRITER)
    start = source.index("procedure WriteContentFormat")
    end = source.index("procedure WriteAcadTableRoundTripObjectsToDXF")
    stack = []
    for _, value in emitted_pairs(source[start:end]):
        if value.endswith("_BEGIN"):
            stack.append(value[: -len("_BEGIN")])
        elif value.endswith("_END") and value != "ACVALUE_END":
            name = value[: -len("_END")]
            assert stack and stack[-1] == name, (name, stack)
            stack.pop()
    assert not stack, stack


def test_reference_autocad_file_uses_row_style_ids():
    """Evidence for the semantics asserted above, from a real AutoCAD file."""
    pairs = dxf_pairs(REFERENCE_DXF)
    styles = [
        int(pairs[i + 1][1])
        for i, (code, value) in enumerate(pairs)
        if value == "TABLEROW_BEGIN" and pairs[i + 1][0] == "90"
    ]
    # Title, two header rows, three data rows - exactly as the table looks.
    assert styles == [1, 2, 2, 3, 3, 3], styles


def test_reference_autocad_file_materializes_cell_formats():
    """A checked-in AutoCAD file confirms the required active prefix."""
    pairs = dxf_pairs(REFERENCE_DXF)
    expected = [
        ("1", "FORMATTEDTABLEDATACELL_BEGIN"),
        ("300", "CELLTABLEFORMAT"),
        ("1", "TABLEFORMAT_BEGIN"),
        ("90", "1"),
        ("170", "1"),
        ("91", "0"),
        ("92", "0"),
        ("62", "257"),
        ("93", "1"),
        ("300", "CONTENTFORMAT"),
        ("1", "CONTENTFORMAT_BEGIN"),
        ("90", "0"),
        ("91", "0"),
        ("92", "512"),
    ]
    assert any(
        pairs[index : index + len(expected)] == expected
        for index in range(len(pairs) - len(expected) + 1)
    )


if __name__ == "__main__":
    test_entity_references_extension_dictionary()
    test_cell_style_id_mapping_is_title_header_data()
    test_each_cell_gets_its_own_style_id()
    test_explicit_cell_style_uses_materialized_autocad_cell_format()
    test_tablecontent_markers_are_balanced()
    test_reference_autocad_file_uses_row_style_ids()
    test_reference_autocad_file_materializes_cell_formats()
    print("ALL TESTS PASSED")
