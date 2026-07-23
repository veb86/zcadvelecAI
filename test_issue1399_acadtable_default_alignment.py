#!/usr/bin/env python3
"""Контракт сохранения стандартного выравнивания AcadTable для issue 1399."""

from pathlib import Path


ROOT = Path(__file__).resolve().parent
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
    assert "dxfintegerout(aoutstream,170,alignment);" in body


if __name__ == "__main__":
    test_default_alignment_is_serialized_as_top_left()
    test_writer_emits_every_nonzero_alignment()
    print("issue 1399 AcadTable default alignment checks passed")
