#!/usr/bin/env python3
"""
Regression checks for issue 1373: AcadTable with multiple header rows.

The legacy AcDbTable cell-stream parser assumes exactly one Title row and one
Header row, so a table whose rows 1 and 2 are both Header style (as in
cad_source/test/tablebugheader.dxf) used to load its third row as Data. The
real per-row style lives only in the modern AcDbTableContent object, where each
TABLEROW_BEGIN marker carries a group-90 style type (1=Title, 2=Header,
3=Data). The loader pre-scans these and pushes them into the main ACAD_TABLE
through SetRowStyleTypes, which RenderCurrentTable already honors.

These checks lock the source-level wiring (mirroring the issue-1342 contract
tests) and verify the scan extracts the expected types from the fixture.
"""

from pathlib import Path
import re

ROOT = Path(__file__).resolve().parent
DXF_LOADER = ROOT / "cad_source" / "zengine" / "fileformats" / "uzeffdxf.pas"
DXF_SUPPORT = ROOT / "cad_source" / "zengine" / "fileformats" / "uzeffdxfsupport.pas"
ENTITY_BASE = ROOT / "cad_source" / "zengine" / "core" / "entities" / "uzeentity.pas"
ACADTABLE_MODEL = (
    ROOT / "cad_source" / "zcad" / "velec" / "acadtable" / "uzeacadtable_model.pas"
)
FIXTURE = ROOT / "cad_source" / "test" / "tablebugheader.dxf"


def read_text(path: Path) -> str:
    return path.read_text(encoding="utf-8", errors="replace")


def compact(text: str) -> str:
    return "".join(text.split()).lower()


def test_base_entity_declares_row_style_hook():
    entity = compact(read_text(ENTITY_BASE))
    assert "proceduresetrowstyletypes(constatypes:arrayofinteger);virtual;" in entity
    assert (
        "proceduregdbobjentity.setrowstyletypes(constatypes:arrayofinteger);"
        in entity
    )


def test_acadtable_model_overrides_row_style_hook():
    # GDBObjAcadTable is an old-style Pascal `object`, which overrides a virtual
    # base method by redeclaring it `virtual` (same as SetBreakOptionFlags), not
    # with the `override` keyword used by classes.
    model = compact(read_text(ACADTABLE_MODEL))
    assert (
        "proceduresetrowstyletypes(constatypes:arrayofinteger);virtual;" in model
    )


def test_loader_scans_and_applies_row_style_types():
    loader = compact(read_text(DXF_LOADER))
    # the scan reads TABLEROW_BEGIN inside the AcDbTableContent object
    assert "procedurescantablerowstyletypes(" in loader
    assert "'tablerow_begin'" in loader
    assert "acdblinkedtabledata" in loader
    # the scan is invoked during pre-scan and applied to the main table
    assert "scantablerowstyletypes(" in loader
    assert "plastmaintable^.setrowstyletypes(context.tablerowstyletypes)" in loader
    assert "context.tablerowstyletypesvalid" in loader
    # row styles are applied only to whole (non-broken) tables: a table split
    # into continuation parts lists per-part repeated header rows in
    # AcDbTableContent, so those indices don't match the merged table's rows.
    assert "context.tablecontinuationhandles.count=0" in loader


def test_context_stores_row_style_types():
    support = compact(read_text(DXF_SUPPORT))
    assert "tablerowstyletypes:tdxfrowstyletypearray;" in support
    assert "tablerowstyletypesvalid:boolean;" in support


def _extract_objects_section(lines):
    for i in range(len(lines) - 1):
        if lines[i].strip() == "2" and lines[i + 1].strip() == "OBJECTS":
            return lines[i + 2:]
    return []


def _scan_row_style_types(lines):
    obj = _extract_objects_section(lines)

    def try_pair(idx):
        if idx < 0 or idx >= len(obj) - 1:
            return None
        try:
            code = int(obj[idx].strip())
        except ValueError:
            return None
        return code, obj[idx + 1].strip()

    in_content = False
    pending = False
    out = []
    i = 0
    while i < len(obj) - 1:
        p = try_pair(i)
        if p:
            code, val = p
            if code == 100:
                if val.upper() == "ACDBLINKEDTABLEDATA":
                    if in_content:
                        break
                    in_content = True
            elif in_content and code == 1 and val == "TABLEROW_BEGIN":
                pending = True
            elif in_content and pending and code == 90:
                dxf_type = int(val) if val.lstrip("-").isdigit() else 0
                out.append({1: 0, 2: 1, 3: 2}.get(dxf_type, -1))
                pending = False
            i += 2
        else:
            i += 1
    return out


def test_fixture_yields_two_header_rows():
    lines = read_text(FIXTURE).splitlines()
    types = _scan_row_style_types(lines)
    # row 0 = Title, rows 1 & 2 = Header, rows 3..5 = Data
    assert types == [0, 1, 1, 2, 2, 2], types


if __name__ == "__main__":
    test_base_entity_declares_row_style_hook()
    test_acadtable_model_overrides_row_style_hook()
    test_loader_scans_and_applies_row_style_types()
    test_context_stores_row_style_types()
    test_fixture_yields_two_header_rows()
    print("issue 1373 AcadTable multi-header contract checks passed")
