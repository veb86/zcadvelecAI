#!/usr/bin/env python3
"""
Regression checks for issue 1344 AcadTable AutoCAD extension dictionary round-trip.
"""

from pathlib import Path


ROOT = Path(__file__).resolve().parent
DXF_LOADER = ROOT / "cad_source" / "zengine" / "fileformats" / "uzeffdxf.pas"
DXF_SUPPORT = ROOT / "cad_source" / "zengine" / "fileformats" / "uzeffdxfsupport.pas"
DXF_OUT = ROOT / "cad_source" / "zengine" / "fileformats" / "uzeffdxfout.pas"
ENTITY_BASE = ROOT / "cad_source" / "zengine" / "core" / "entities" / "uzeentity.pas"
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
SAMPLE_DXF = ROOT / "cad_source" / "test" / "acadtablerazdel2007_1.dxf"


def read_text(path: Path) -> str:
    return path.read_text(encoding="utf-8")


def compact(text: str) -> str:
    return "".join(text.split()).lower()


def dxf_pairs(path: Path) -> list[tuple[str, str]]:
    lines = path.read_text(encoding="utf-8", errors="ignore").splitlines()
    return [(lines[i].strip(), lines[i + 1].strip()) for i in range(0, len(lines) - 1, 2)]


def objects_by_handle(path: Path) -> dict[str, list[tuple[str, str]]]:
    pairs = dxf_pairs(path)
    objects: dict[str, list[tuple[str, str]]] = {}
    i = 0
    while i < len(pairs):
        if pairs[i] == ("0", "SECTION") and i + 1 < len(pairs) and pairs[i + 1] == ("2", "OBJECTS"):
            i += 2
            while i < len(pairs) and pairs[i] != ("0", "ENDSEC"):
                if pairs[i][0] == "0":
                    start = i
                    i += 1
                    while i < len(pairs) and pairs[i][0] != "0":
                        i += 1
                    obj = pairs[start:i]
                    handle = next((value.upper() for code, value in obj if code == "5"), None)
                    if handle:
                        objects[handle] = obj
                    continue
                i += 1
        i += 1
    return objects


def test_sample_acad_table_contains_autocad_content_subtree():
    pairs = dxf_pairs(SAMPLE_DXF)
    table_start = pairs.index(("0", "ACAD_TABLE"))
    table_pairs = []
    for pair in pairs[table_start:]:
        if table_pairs and pair[0] == "0":
            break
        table_pairs.append(pair)

    ext_dict_handle = None
    for idx, pair in enumerate(table_pairs):
        if pair == ("102", "{ACAD_XDICTIONARY"):
            ext_dict_handle = table_pairs[idx + 1][1].upper()
            break
    assert ext_dict_handle == "357"

    objects = objects_by_handle(SAMPLE_DXF)
    assert objects[ext_dict_handle][0] == ("0", "DICTIONARY")
    xrecord_handle = next(value.upper() for code, value in objects[ext_dict_handle] if code == "360")
    assert objects[xrecord_handle][0] == ("0", "XRECORD")

    xrecord = objects[xrecord_handle]
    assert ("102", "ACAD_ROUNDTRIP_2008_TABLE_ENTITY") in xrecord
    assert objects[next(value.upper() for code, value in xrecord if code == "360")][0] == (
        "0",
        "TABLECONTENT",
    )
    assert objects[next(value.upper() for code, value in xrecord if code == "361")][0] == (
        "0",
        "TABLEGEOMETRY",
    )


def test_loader_prescans_table_extension_dictionary_subtrees():
    loader = compact(read_text(DXF_LOADER))
    support = compact(read_text(DXF_SUPPORT))
    entity_base = compact(read_text(ENTITY_BASE))
    model = compact(read_text(ACADTABLE_MODEL))

    assert "tablerawacadtableextdictsubtrees:tstringlist" in support
    assert "tablerawtextstylehandlenames:tstringlist" in support
    assert "scantextstylehandlenames(" in loader
    assert "scanacadtablerawextdictsubtrees(" in loader
    assert "tablecontent" in loader
    assert "tablegeometry" in loader
    assert "setdxfrawextdictsubtree(" in entity_base
    assert "proceduregdbobjacadtable.setdxfrawextdictsubtree(" in model
    assert "frawextdictsubtreevalid:=arawtext<>''" in model


def test_writer_preserves_and_remaps_tablecontent_subtree():
    writer = compact(read_text(ACADTABLE_WRITER))
    dxf_out = compact(read_text(DXF_OUT))
    support = compact(read_text(DXF_SUPPORT))
    model = compact(read_text(ACADTABLE_MODEL))

    assert "textstylenamehandlemap:tstring2stringdictionary" in support
    assert "textstylenamehandlemap.add(" in dxf_out
    assert "frawextdictsubtree" in model
    assert "amainextdictsubtree" in writer
    assert "collectrawextdicthandleremaps(" in writer
    assert "writerawextdictsubtreetodxf(" in writer
    assert "remaprawextdicthandlevalue(" in writer
    assert "rawacadtableextdicthandle(" in writer
    assert "akeepextdict" in writer
    assert "aextdicthandlenew" in writer
    assert "findhandleremap(" in writer
    assert "acadtablebreakoptionflags(" in writer
    assert "aoutstream.txtaddstringeol(outvalue)" in writer


if __name__ == "__main__":
    test_sample_acad_table_contains_autocad_content_subtree()
    test_loader_prescans_table_extension_dictionary_subtrees()
    test_writer_preserves_and_remaps_tablecontent_subtree()
    print("issue 1344 AcadTable extension dictionary round-trip checks passed")
