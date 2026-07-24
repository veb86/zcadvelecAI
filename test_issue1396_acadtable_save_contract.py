#!/usr/bin/env python3
"""Контракт сохранения координат и выравнивания AcadTable для issue 1396."""

from pathlib import Path


ROOT = Path(__file__).resolve().parent
MODEL = (
    ROOT / "cad_source" / "zcad" / "velec" / "acadtable"
    / "uzeacadtable_model.pas"
)
WRITER = (
    ROOT / "cad_source" / "zcad" / "velec" / "acadtable"
    / "uzeacadtable_dxf_write.pas"
)
FPUNIT = ROOT / "cad_source" / "zengine" / "tests" / "uzctacadtable.pas"


def compact(text: str) -> str:
    return "".join(text.split()).lower()


def extract_procedure(source: str, marker: str, next_marker: str) -> str:
    start = source.index(marker)
    end = source.index(next_marker, start + len(marker))
    return source[start:end]


def test_writer_uses_current_local_insert_point():
    source = MODEL.read_text(encoding="utf-8")
    body = compact(
        extract_procedure(
            source,
            "procedure GDBObjAcadTable.FillDXFWritePartFromSelf",
            "procedure GDBObjAcadTable.FillDXFWritePartFromContinuation",
        )
    )
    assert "apart.insertpoint:=local.p_insert;" in body
    assert "apart.insertpoint:=finsertpoint;" not in body


def test_alignment_uses_autocad_cell_group_order():
    source = WRITER.read_text(encoding="utf-8")
    body = compact(
        extract_procedure(source, "procedure WriteCell", "procedure WriteTableCells")
    )
    groups = [
        "dxfintegerout(aoutstream,176,rowspan);",
        "dxfintegerout(aoutstream,91,262145)",
        "dxfintegerout(aoutstream,178,0);",
        "dxfdoubleout(aoutstream,145,0);",
        "dxfintegerout(aoutstream,170,alignment);",
        "dxfintegerout(aoutstream,92,0);",
    ]
    positions = [body.index(group) for group in groups]
    assert positions == sorted(positions)


def test_fpunit_reproduces_save_round_trip():
    source = FPUNIT.read_text(encoding="utf-8")
    assert "procedure SavesTransformedInsertPointAndCellAlignmentToDXF;" in source
    assert "Table^.transform(MoveMatrix);" in source
    assert "'145', '0', '170', '9', '92', '0'" in source


if __name__ == "__main__":
    test_writer_uses_current_local_insert_point()
    test_alignment_uses_autocad_cell_group_order()
    test_fpunit_reproduces_save_round_trip()
    print("issue 1396 AcadTable save contract checks passed")
