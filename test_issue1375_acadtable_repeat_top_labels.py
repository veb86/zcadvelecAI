#!/usr/bin/env python3
"""
Regression checks for issue 1375: AcadTable repeat-top labels.

AcadTableBreakRepeatTop must repeat every leading Title/Header row until the
first Data row. A table with row styles Title/Header/Header/Data used to repeat
only Title/Header because the model capped top labels at two positional rows.
"""

from pathlib import Path


ROOT = Path(__file__).resolve().parent
MODEL = (
    ROOT / "cad_source" / "zcad" / "velec" / "acadtable" / "uzeacadtable_model.pas"
)
FPUNIT = ROOT / "cad_source" / "zengine" / "tests" / "uzctacadtable.pas"


def read_text(path: Path) -> str:
    return path.read_text(encoding="utf-8")


def compact(text: str) -> str:
    return "".join(text.split()).lower()


def _extract_function(src: str, name: str) -> str:
    marker = f"function GDBObjAcadTable.{name}"
    start = src.index(marker)
    next_marker = src.index("\nfunction GDBObjAcadTable.", start + 1)
    return src[start:next_marker]


def test_compute_top_label_row_count_uses_explicit_row_styles():
    body = compact(_extract_function(read_text(MODEL), "ComputeTopLabelRowCount"))

    assert "rowstyletypeat(rowidx)" in body
    assert "styletype=2" in body
    assert "styletype=0)or(styletype=1" in body
    assert "inc(result)" in body
    # Keep the legacy positional fallback for old tables without explicit row
    # style metadata, but do not cap explicitly styled tables at two rows.
    assert "length(frowstyletypes)=0" in body


def test_clone_preserves_explicit_row_style_types_for_rt_preview():
    body = compact(_extract_function(read_text(MODEL), "Clone"))

    assert "frowstyletypes" in body
    assert "system.setlength(newtable^.frowstyletypes,length(frowstyletypes))" in body
    assert "newtable^.frowstyletypes[idx]:=frowstyletypes[idx]" in body


def test_fpunit_covers_title_header_header_repeat_top_case():
    test_src = read_text(FPUNIT)
    assert "procedure BreakRepeatTopUsesAllLeadingTitleHeaderRows;" in test_src
    assert "Table^.SetRowStyleTypes([0, 1, 1, 2, 2, 2])" in test_src
    assert "Header 2" in test_src
    assert "ContinuationPartCellText(0, 2, 0)" in test_src
    assert "procedure ClonedPreviewPreservesExplicitRowStyleTypes;" in test_src


if __name__ == "__main__":
    test_compute_top_label_row_count_uses_explicit_row_styles()
    test_clone_preserves_explicit_row_style_types_for_rt_preview()
    test_fpunit_covers_title_header_header_repeat_top_case()
    print("issue 1375 AcadTable repeat-top label checks passed")
