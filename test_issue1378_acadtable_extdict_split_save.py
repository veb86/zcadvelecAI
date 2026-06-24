#!/usr/bin/env python3
"""
Regression checks for issue 1378: AutoCAD split ACAD_TABLE save.

The fixture tablebugheader.dxf is visually split by AutoCAD even though the
DXF has only one ACAD_TABLE entity. The split/table content metadata is stored
under the entity extension dictionary as TABLECONTENT/TABLEGEOMETRY. ZCAD's
old save output generated multiple ACAD_TABLE entities but dropped that
extension dictionary subtree, so AutoCAD opened the result as one whole table.
"""

from pathlib import Path


ROOT = Path(__file__).resolve().parent
SOURCE_DXF = ROOT / "cad_source" / "test" / "tablebugheader.dxf"
BROKEN_SAVE_DXF = ROOT / "cad_source" / "test" / "tablebugheader3.dxf"
ACADTABLE_MODEL = (
    ROOT
    / "cad_source"
    / "zcad"
    / "velec"
    / "acadtable"
    / "uzeacadtable_model.pas"
)
ACADTABLE_WRITER = (
    ROOT
    / "cad_source"
    / "zcad"
    / "velec"
    / "acadtable"
    / "uzeacadtable_dxf_write.pas"
)


def read_text(path: Path) -> str:
    return path.read_text(encoding="utf-8", errors="replace")


def compact(text: str) -> str:
    return "".join(text.split()).lower()


def dxf_entity_counts(path: Path) -> dict[str, int]:
    lines = read_text(path).splitlines()
    counts: dict[str, int] = {}
    for i in range(0, len(lines) - 1, 2):
        if lines[i].strip() == "0":
            name = lines[i + 1].strip().upper()
            counts[name] = counts.get(name, 0) + 1
    return counts


def test_fixture_shows_missing_tablecontent_root_cause():
    source_counts = dxf_entity_counts(SOURCE_DXF)
    broken_counts = dxf_entity_counts(BROKEN_SAVE_DXF)

    assert source_counts["ACAD_TABLE"] == 1
    assert source_counts["TABLECONTENT"] == 1
    assert source_counts["TABLEGEOMETRY"] == 1

    assert broken_counts["ACAD_TABLE"] > source_counts["ACAD_TABLE"]
    assert broken_counts.get("TABLECONTENT", 0) == 0
    assert broken_counts.get("TABLEGEOMETRY", 0) == 0


def test_model_preserves_raw_one_entity_extdict_save_path():
    model = compact(read_text(ACADTABLE_MODEL))

    assert "frawdxfentityvalidand(frawdxfentity<>'')" in model
    assert "ifcansaverawdxfentitythen" in model
    assert "elseiffrawextdictsubtreevalidand(frawextdictsubtree<>'')then" in model
    assert "system.setlength(rawparts,0)" in model
    assert (
        "writerawacadtablepartstodxf(aoutstream,aiodxfcontext,"
        "frawdxfentity,rawparts,frawextdictsubtree,frawtextstylehandlemap"
    ) in model


def test_writer_keeps_and_remaps_acad_xdictionary_subtree():
    writer = compact(read_text(ACADTABLE_WRITER))

    assert "extdicthandle:string" in writer
    assert "dxfstringwithoutencodeout(aoutstream,102,'{acad_xdictionary')" in writer
    assert "rawacadtableextdicthandle(" in writer
    assert "collectrawextdicthandleremaps(" in writer
    assert "addhandleremap(" in writer
    assert "writerawextdictsubtreetodxf(" in writer
    assert "remaprawextdicthandlevalue(" in writer
    assert "aoutstream.txtaddstringeol(outvalue)" in writer


if __name__ == "__main__":
    test_fixture_shows_missing_tablecontent_root_cause()
    test_model_preserves_raw_one_entity_extdict_save_path()
    test_writer_keeps_and_remaps_acad_xdictionary_subtree()
    print("issue 1378 AcadTable split extension dictionary checks passed")
