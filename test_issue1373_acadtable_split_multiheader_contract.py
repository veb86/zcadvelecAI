#!/usr/bin/env python3
"""
Regression checks for issue 1373 (split-table follow-up): a legacy ACAD_TABLE
that was broken into several parts and whose repeating top labels span more than
the classic Title+Header pair.

The original issue fixed whole (non-split) tables by reading per-row style types
from the modern AcDbTableContent object. But a *split* table exported as several
legacy ACAD_TABLE entities (cad_source/test/tablebugheader3.dxf) has NO
AcDbTableContent at all, so no explicit row styles reach the model. The legacy
top-label logic then caps the repeat zone at two rows (row 0 = Title, row 1 =
Header), which means a table whose rows 1 AND 2 are both header labels loses the
second header: on a break-height change only rows 0 and 1 repeat and the third
label row is treated as data and duplicated in every continuation part.

The fix infers per-row style types for such tables from the content itself: the
number of leading rows that repeat identically at the start of *every*
continuation part is exactly the label-row count (Title + Header rows) before the
first data row. GDBObjAcadTable.DetectBreakRepeatTopLabels calls
DetectRepeatedTopRowCountRaw and, when no explicit types were supplied
(FRowStyleTypesExplicit = False), rebuilds FRowStyleTypes as
[Title, Header * (L-1), Data * rest]. ComputeTopLabelRowCount /
EffectiveRepeatTopRowCount then honour the true label-row count and the split
keeps all L leading rows in the repeat zone.

These checks lock the source-level wiring (mirroring the issue-1342 / issue-1373
whole-table contract tests) and verify the fixture actually exhibits a
three-row (not two-row) repeat zone across its parts.
"""

from pathlib import Path

ROOT = Path(__file__).resolve().parent
ACADTABLE_MODEL = (
    ROOT / "cad_source" / "zcad" / "velec" / "acadtable" / "uzeacadtable_model.pas"
)
FIXTURE = ROOT / "cad_source" / "test" / "tablebugheader3.dxf"


def read_text(path: Path) -> str:
    return path.read_text(encoding="utf-8", errors="replace")


def compact(text: str) -> str:
    return "".join(text.split()).lower()


# --- source-contract checks ------------------------------------------------


def test_model_tracks_explicit_row_style_flag():
    # A boolean guard distinguishes styles set from outside (AcDbTableContent or
    # the spreadsheet editor) from styles inferred for legacy split tables, so
    # inference never clobbers real data.
    model = compact(read_text(ACADTABLE_MODEL))
    assert "frowstyletypesexplicit:boolean;" in model
    # SetRowStyleTypes marks the styles explicit; inference is gated on the flag.
    assert "frowstyletypesexplicit:=true;" in model
    assert "ifnotfrowstyletypesexplicitthen" in model


def test_model_declares_raw_repeat_detector():
    model = compact(read_text(ACADTABLE_MODEL))
    # The uncapped detector counts leading rows that repeat across every part.
    assert "functiongdbobjacadtable.detectrepeatedtoprowcountraw:integer;" in model
    # It is consumed by DetectBreakRepeatTopLabels to rebuild the row styles.
    assert "repeatcount:=detectrepeatedtoprowcountraw;" in model


def test_inference_rebuilds_title_header_data_rows():
    model = compact(read_text(ACADTABLE_MODEL))
    # Row 0 -> Title(0), rows 1..L-1 -> Header(1), rest -> Data(2).
    assert "system.setlength(frowstyletypes,frowcount);" in model
    assert "(repeatcount>=2)and(repeatcount<frowcount)" in model


def test_clone_copies_explicit_flag():
    # Cloning a table must carry the explicit/inferred distinction so a copied
    # table does not silently re-infer over real styles.
    model = compact(read_text(ACADTABLE_MODEL))
    assert "newtable^.frowstyletypesexplicit:=frowstyletypesexplicit" in model


# --- fixture behaviour check ----------------------------------------------


def _acad_table_entities(text: str):
    lines = text.replace("\r\n", "\n").split("\n")
    pairs = []
    k = 0
    while k < len(lines) - 1:
        pairs.append((lines[k].strip(), lines[k + 1]))
        k += 2
    ents, cur = [], []
    for code, val in pairs:
        if code == "0" and val.strip() == "ACAD_TABLE":
            if cur:
                ents.append(cur)
            cur = []
        cur.append((code, val))
    if cur:
        ents.append(cur)
    return [
        e for e in ents if any(c == "100" and v.strip() == "AcDbTable" for c, v in e)
    ]


def _entity_dims(entity):
    rows = cols = None
    for code, val in entity:
        if code == "91" and rows is None:
            rows = int(val.strip())
        elif code == "92" and cols is None:
            cols = int(val.strip())
        if rows is not None and cols is not None:
            break
    return rows, cols


def _entity_cell_texts(entity):
    return [v for c, v in entity if c == "302"]


def _rows_repeat_across_parts(parts_texts, cols, count):
    """True when the first `count` rows are identical across every part."""
    ref = parts_texts[0]
    for texts in parts_texts[1:]:
        if ref[: count * cols] != texts[: count * cols]:
            return False
    return True


def test_fixture_is_a_legacy_split_table():
    text = read_text(FIXTURE)
    ents = _acad_table_entities(text)
    # Three legacy ACAD_TABLE parts: one main + two continuations.
    assert len(ents) == 3, len(ents)
    # No modern content object exists, so no explicit row styles are available.
    assert "AcDbTableContent" not in text
    assert "ACDBLINKEDTABLEDATA" not in text


def test_fixture_repeat_zone_is_three_rows_not_two():
    text = read_text(FIXTURE)
    ents = _acad_table_entities(text)
    dims = [_entity_dims(e) for e in ents]
    # Every part is 4 rows x 3 columns.
    assert dims == [(4, 3), (4, 3), (4, 3)], dims
    cols = 3
    parts_texts = [_entity_cell_texts(e) for e in ents]
    assert all(len(t) == 12 for t in parts_texts), [len(t) for t in parts_texts]

    # The correct repeat count is the largest L for which all parts share their
    # first L rows -- exactly what DetectRepeatedTopRowCountRaw computes.
    detected = 0
    for count in range(min(dims[0][0], 4), 0, -1):
        if _rows_repeat_across_parts(parts_texts, cols, count):
            detected = count
            break
    # Rows 0 (Title "Титул"), 1 ("1 2 3") and 2 ("a b c") repeat in every part;
    # only row 3 (the data value 2/3/4) differs.
    assert detected == 3, detected
    # The legacy Title+Header cap of two rows would wrongly drop the second
    # header row into the data zone -- the exact bug this fix removes.
    assert detected > 2


if __name__ == "__main__":
    test_model_tracks_explicit_row_style_flag()
    test_model_declares_raw_repeat_detector()
    test_inference_rebuilds_title_header_data_rows()
    test_clone_copies_explicit_flag()
    test_fixture_is_a_legacy_split_table()
    test_fixture_repeat_zone_is_three_rows_not_two()
    print("issue 1373 AcadTable split multi-header contract checks passed")
