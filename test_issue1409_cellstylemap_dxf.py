#!/usr/bin/env python3
"""Contract for the CELLSTYLEMAP chain of a saved TABLESTYLE (issue #1409).

Writing the individual style id of every cell into TABLECONTENT
(``TABLECELL_BEGIN / 90`` = 1/2/3) is not enough on its own: those ids are
meaningless unless the drawing also contains the object that *defines* them.
AutoCAD stores those definitions in an ``AcDbCellStyleMap`` reached from the
table style's extension dictionary::

    TABLESTYLE -> {ACAD_XDICTIONARY -> DICTIONARY
                -> (ACAD_ROUNDTRIP_2008_TABLESTYLE_CELLSTYLEMAP)
                -> CELLSTYLEMAP  (90 = 3 entries: _TITLE, _HEADER, _DATA)

Without that object AutoCAD discards the per-cell ids and re-applies its
built-in rule "row 1 = Title, row 2 = Header, the rest = Data" — exactly the
symptom reported in the issue.  The reference layout was extracted from a
file re-saved by AutoCAD, see ``experiments/issue1409/cellstylemap.txt``.

This test also pins the entity-side references that the round-trip needs:
group 342 (table style) and group 330 (owner) on the ACAD_TABLE entity, and
group 340 (table style) in the TABLECONTENT object.  All three used to be
written as an empty/zero handle because the handle stored in the model is the
*pre-renumbering* one; they must be resolved by style name through
``TableStyleNameHandleMap``.
"""

import re
from pathlib import Path

ROOT = Path(__file__).resolve().parent
DXFOUT = ROOT / "cad_source" / "zengine" / "fileformats" / "uzeffdxfout.pas"
WRITER = (
    ROOT
    / "cad_source"
    / "zcad"
    / "velec"
    / "acadtable"
    / "uzeacadtable_dxf_write.pas"
)
MODEL = (
    ROOT / "cad_source" / "zcad" / "velec" / "acadtable" / "uzeacadtable_model.pas"
)
REFERENCE = ROOT / "experiments" / "issue1409" / "cellstylemap.txt"


def read(path: Path) -> str:
    return path.read_text(encoding="utf-8", errors="replace")


def emitted_pairs(source: str, stream: str = "outstream"):
    """Group code / literal value pairs emitted into ``stream``, in order."""
    pattern = re.compile(
        re.escape(stream) + r"\.TXTAddStringEOL\(\s*(?:dxfGroupCode\((\d+)\)"
        r"|(.+?))\s*\)\s*;",
        re.S,
    )
    return pattern.findall(source)


def test_tablestyle_gets_extension_dictionary_and_cellstylemap():
    source = read(DXFOUT)
    assert "WriteCellStyleMapObjectsToStream" in source
    assert "'{ACAD_XDICTIONARY'" in source
    assert "'ACAD_ROUNDTRIP_2008_TABLESTYLE_CELLSTYLEMAP'" in source
    assert "'CELLSTYLEMAP'" in source
    assert "'AcDbCellStyleMap'" in source
    # The map declares exactly three cell styles.
    assert "WriteCellStyleMapEntryToStream(outstream, CS[1], 1, 1, 32768, '_TITLE')" in source
    assert "WriteCellStyleMapEntryToStream(outstream, CS[2], 2, 1, 0, '_HEADER')" in source
    assert "WriteCellStyleMapEntryToStream(outstream, CS[0], 3, 2, 0, '_DATA')" in source


def test_dictionary_and_map_handles_are_preallocated():
    source = read(DXFOUT)
    # Both extra objects need their own handles, allocated together with the
    # table style handle so that nothing else claims them.
    assert "tsDictHandles" in source
    assert "tsMapHandles" in source
    body = source[source.index("procedure PreallocateTableStyleHandles"):]
    body = body[: body.index("\n  begin\n") + 4000]
    assert body.count("Inc(IODXFContext.handle);") >= 3


def test_cellstyle_entry_matches_autocad_reference_structure():
    """Every marker of one reference CELLSTYLE block must be emitted."""
    source = read(DXFOUT)
    entry = source[source.index("procedure WriteCellStyleMapEntryToStream"):]
    entry = entry[: entry.index("\nprocedure WriteCellStyleMapObjectsToStream")]
    for marker in (
        "TABLEFORMAT_BEGIN",
        "CONTENTFORMAT",
        "CONTENTFORMAT_BEGIN",
        "CONTENTFORMAT_END",
        "MARGIN",
        "CELLMARGIN_BEGIN",
        "CELLMARGIN_END",
        "GRIDFORMAT",
        "GRIDFORMAT_BEGIN",
        "GRIDFORMAT_END",
        "TABLEFORMAT_END",
        "CELLSTYLE_BEGIN",
        "CELLSTYLE_END",
    ):
        assert f"'{marker}'" in entry, marker
    # Six grid formats (bits 1,2,4,8,16,32) as in the AutoCAD reference.
    assert "dxfPairOut(outstream, 94, '6');" in entry
    assert "GridBit <= 32" in entry


def test_reference_file_documents_three_cell_styles():
    if not REFERENCE.exists():  # reference kept for documentation only
        return
    text = read(REFERENCE)
    assert text.count("CELLSTYLE_BEGIN") == 3
    for name in ("_TITLE", "_HEADER", "_DATA"):
        assert name in text
    assert re.search(r"^90 3$", text, re.M)


def test_entity_writes_owner_and_table_style_references():
    source = read(WRITER)
    assert "ResolveTableStyleHandle" in source
    # 342 on the ACAD_TABLE entity, resolved by style name.
    assert re.search(
        r"dxfStringWithoutEncodeOut\(AOutStream, 342, TableStyleHandle\)",
        source,
    )
    # 330 owner (*Model_Space) on the entity.
    assert "AcadTableOwnerHandle > 0" in source
    assert re.search(
        r"dxfStringWithoutEncodeOut\(AOutStream, 330,\s*"
        r"IntToHex\(AIODXFContext\.AcadTableOwnerHandle, 0\)\)",
        source,
    )
    # 340 in TABLECONTENT, resolved the same way instead of the hard '0'.
    assert re.search(
        r"dxfStringWithoutEncodeOut\(AOutStream, 340, TableStyleHandle\)",
        source,
    )


def test_write_part_carries_the_table_style_name():
    assert "TableStyleName: String;" in read(WRITER)
    model = read(MODEL)
    assert "APart.TableStyleName := FTableStyle.Name;" in model
    assert "APart.TableStyleName := ASource.TableStyle.Name;" in model


def test_cells_carry_the_roundtrip_checksum():
    source = read(WRITER)
    assert "'ACAD_ROUNDTRIP_2008_CELL_CHECKSUM'" in source
    assert "function CellTextChecksum" in source
    assert "WriteCellChecksumDataMap(AOutStream, AText);" in source


def test_formatted_table_data_trailer_is_a_single_zero():
    """AutoCAD writes 90|0 there; the old 90..94 block broke parsing."""
    source = read(WRITER)
    tail = source[source.index("'AcDbFormattedTableData'"):]
    tail = tail[: tail.index("'AcDbTableContent'")]
    ints = re.findall(r"dxfIntegerout\(AOutStream, (\d+), (\d+)\)", tail)
    # only the TABLEFORMAT block (90/170) plus the single trailing 90|0
    assert ints[-1] == ("90", "0"), ints


def main():
    failures = 0
    for name, fn in sorted(globals().items()):
        if name.startswith("test_") and callable(fn):
            try:
                fn()
            except AssertionError as exc:
                failures += 1
                print(f"FAILED {name}: {exc}")
            else:
                print(f"ok {name}")
    if failures:
        raise SystemExit(f"{failures} test(s) failed")
    print("ALL TESTS PASSED")


if __name__ == "__main__":
    main()
