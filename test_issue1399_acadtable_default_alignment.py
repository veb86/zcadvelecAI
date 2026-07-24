#!/usr/bin/env python3
"""Контракт сохранения стандартного выравнивания AcadTable для issue 1399."""

from pathlib import Path


ROOT = Path(__file__).resolve().parent
FIXTURES = ROOT / "experiments" / "issue1399"
DIMENSIONS = (
    ROOT
    / "cad_source"
    / "zcad"
    / "velec"
    / "uzvspreadsheet"
    / "uzvspreadsheet_dimensions.pas"
)
WRITER = (
    ROOT
    / "cad_source"
    / "zcad"
    / "velec"
    / "acadtable"
    / "uzeacadtable_dxf_write.pas"
)


def compact(text: str) -> str:
    return "".join(text.split()).lower()


def extract(source: str, start_marker: str, end_marker: str) -> str:
    start = source.index(start_marker)
    end = source.index(end_marker, start + len(start_marker))
    return source[start:end]


def acad_table_cell_properties(path: Path) -> list[tuple[str, str]]:
    lines = path.read_text(encoding="utf-8").splitlines()
    pairs = [
        (lines[index].strip(), lines[index + 1].strip())
        for index in range(0, len(lines) - 1, 2)
    ]
    table_start = max(
        index
        for index, pair in enumerate(pairs)
        if pair == ("0", "ACAD_TABLE")
    )
    table_end = next(
        index
        for index in range(table_start + 1, len(pairs))
        if pairs[index][0] == "0"
    )
    table = pairs[table_start:table_end]
    result = []
    for index, pair in enumerate(table):
        if pair == ("301", "CELL_VALUE"):
            properties = dict(table[max(0, index - 12):index])
            result.append((properties["91"], properties["170"]))
    return result


def test_autocad_fixture_marks_alignment_as_cell_override():
    zcad = acad_table_cell_properties(FIXTURES / "zcadtablevyrav.txt")
    autocad = acad_table_cell_properties(FIXTURES / "acadtablevyrav.txt")
    assert len(zcad) == len(autocad) == 12
    assert set(zcad) == {("262144", "1")}
    assert set(autocad) == {("262145", "1")}


def test_default_alignment_is_serialized_as_top_left():
    source = DIMENSIONS.read_text(encoding="utf-8")
    body = compact(
        extract(
            source,
            "function CollectCellAlignments",
            "\nend;\n\nend.",
        )
    )
    default_call = (
        "worksheetalignmenttoacad("
        "fpstypes.hadefault,fpstypes.vadefault)"
    )
    assert default_call in body
    assert "result[rowidx*acolcount+colidx]:=0;" not in body


def test_writer_emits_every_nonzero_alignment():
    source = WRITER.read_text(encoding="utf-8")
    body = compact(
        extract(source, "procedure WriteCell", "procedure WriteTableCells")
    )
    assert "ifalignment<>0then" in body
    assert (
        "ifalignment<>0then"
        "dxfintegerout(aoutstream,91,262145)"
        "elsedxfintegerout(aoutstream,91,262144);"
    ) in body
    assert "dxfintegerout(aoutstream,170,alignment);" in body


if __name__ == "__main__":
    test_autocad_fixture_marks_alignment_as_cell_override()
    test_default_alignment_is_serialized_as_top_left()
    test_writer_emits_every_nonzero_alignment()
    print("issue 1399 AcadTable default alignment checks passed")
