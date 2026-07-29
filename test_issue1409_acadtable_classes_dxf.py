#!/usr/bin/env python3
"""Regression contract for ACAD_TABLE class declarations (issue #1409).

The DXF produced by ZCAD in ``test4.txt`` already contains the per-cell style
ids, a TABLECONTENT object, and a CELLSTYLEMAP object.  AutoCAD nevertheless
falls back to its built-in row styles because those application-defined
objects are absent from the file's CLASSES section.

An AutoCAD-resaved reference declares all three classes used by this export
path before their instances:

* ACAD_TABLE / AcDbTable
* TABLECONTENT / AcDbTableContent
* CELLSTYLEMAP / AcDbCellStyleMap

The generic DXF writer owns the CLASSES section, while the AcadTable module
owns these application-specific definitions.  This test pins the callback
boundary between them and the exact class records found in the reference.
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
REFERENCE = ROOT / "cad_source" / "test" / "tablebugheader.dxf"


def read(path: Path) -> str:
    return path.read_text(encoding="utf-8", errors="replace")


def dxf_records(path: Path, record_type: str):
    lines = [line.strip() for line in read(path).splitlines()]
    pairs = list(zip(lines[0::2], lines[1::2]))
    records = []
    current = None
    for code, value in pairs:
        if code == "0":
            if current and current[0][1] == record_type:
                records.append(current)
            current = [(code, value)]
        elif current is not None:
            current.append((code, value))
    if current and current[0][1] == record_type:
        records.append(current)
    return records


def class_record_map(path: Path):
    result = {}
    for record in dxf_records(path, "CLASS"):
        values = dict(record)
        result[values["1"]] = values
    return result


def test_autocad_reference_declares_the_three_custom_classes():
    classes = class_record_map(REFERENCE)
    expected = {
        "ACAD_TABLE": ("AcDbTable", "1025", "1"),
        "TABLECONTENT": ("AcDbTableContent", "1152", "0"),
        "CELLSTYLEMAP": ("AcDbCellStyleMap", "1152", "0"),
    }
    for name, (cpp_name, proxy_flags, entity_flag) in expected.items():
        record = classes[name]
        assert record["2"] == cpp_name
        assert record["3"] == "ObjectDBX Classes"
        assert record["90"] == proxy_flags
        assert record["280"] == "0"
        assert record["281"] == entity_flag


def test_generic_writer_offers_and_runs_a_classes_callback():
    source = read(DXFOUT)
    assert "TClassesSaveDxfProc" in source
    assert "RegisterClassesSaveDxfProc" in source
    assert "procedure RunClassesSaveDxfProcs" in source
    assert re.search(
        r"inclassessec\s+and\s+\(groupi=0\)\s+and\s+"
        r"\(values=dxfName_ENDSEC\).*?RunClassesSaveDxfProcs",
        source,
        re.S,
    )


def test_acadtable_writer_emits_and_registers_required_classes():
    source = read(WRITER)
    assert "procedure WriteAcadTableClassRecord" in source
    assert "'ObjectDBX Classes'" in source
    assert "procedure WriteAcadTableClassesToDXF" in source
    body = source[source.index("procedure WriteAcadTableClassesToDXF") :]
    body = body[: body.index("\nprocedure ", 20)]
    for marker in (
        "'ACAD_TABLE'",
        "'AcDbTable'",
        "'TABLECONTENT'",
        "'AcDbTableContent'",
        "'CELLSTYLEMAP'",
        "'AcDbCellStyleMap'",
    ):
        assert marker in body
    assert re.search(
        r"'ACAD_TABLE',\s*'AcDbTable',\s*1025,\s*AcadTableCount,\s*1",
        body,
    )
    assert re.search(
        r"'TABLECONTENT',\s*'AcDbTableContent',\s*1152,\s*"
        r"AcadTableCount\*2,\s*0",
        body,
    )
    assert re.search(
        r"'CELLSTYLEMAP',\s*'AcDbCellStyleMap',\s*1152,\s*"
        r"CellStyleMapCount,\s*0",
        body,
    )
    assert "RegisterClassesSaveDxfProc(@WriteAcadTableClassesToDXF)" in source


def main():
    failures = 0
    for name, fn in sorted(globals().items()):
        if name.startswith("test_") and callable(fn):
            try:
                fn()
            except (AssertionError, KeyError) as exc:
                failures += 1
                print(f"FAILED {name}: {exc}")
            else:
                print(f"ok {name}")
    if failures:
        raise SystemExit(f"{failures} test(s) failed")
    print("ALL TESTS PASSED")


if __name__ == "__main__":
    main()
