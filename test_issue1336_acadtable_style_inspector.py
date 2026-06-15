#!/usr/bin/env python3
"""
Regression checks for issue 1336 AcadTable style editing from object inspector.
"""

from pathlib import Path


ROOT = Path(__file__).resolve().parent
REGISTER_PAS = ROOT / "cad_source" / "zcad" / "register" / "uzcregacadtable.pas"
MODEL_PAS = (
    ROOT
    / "cad_source"
    / "zcad"
    / "velec"
    / "acadtable"
    / "uzeacadtable_model.pas"
)
STYLEMANAGER_PAS = (
    ROOT
    / "cad_source"
    / "zcad"
    / "velec"
    / "acadtable"
    / "uzeacadtable_stylemanager.pas"
)


def read_text(path: Path) -> str:
    return path.read_text(encoding="utf-8")


def compact(text: str) -> str:
    return "".join(text.split())


def test_acadtable_style_property_uses_dxf_style_combobox():
    register_pas = read_text(REGISTER_PAS)
    register_compact = compact(register_pas)

    assert "function GetAcadTableStyleNameData" in register_pas
    assert "DXFTableStyleTable" in register_pas
    assert "PTGDBDXFTableStyle" in register_pas
    assert "sysunit^.TypeName2PTD('TEnumData')" in register_pas
    assert (
        "TMainIterateProcsData.Create("
        "@GetAcadTableStyleNameData,@FreeTEnumData)"
    ) in register_compact
    assert (
        "'AcadTableStyleName','Table style',"
        "sysunit^.TypeName2PTD('AnsiString')"
    ) not in register_compact
    assert "procedure AcadTableStyleNameEntChangeProc" in register_pas
    assert "PTEnumData(PVD^.data.Addr.Instance)^.Selected" in register_pas
    assert ".Enums.getDataMutable(NewIndex)" in register_pas
    assert "SetTableStyleName" in register_pas
    assert "YouChanged" in register_pas
    assert (
        "TEntIterateProcsData.Create("
        "nil,@AcadTableStyleNameEntIterateProc,"
        "@AcadTableStyleNameEntChangeProc)"
    ) in register_compact


def test_model_applies_style_name_through_dxf_table_styles():
    model_pas = read_text(MODEL_PAS)
    model_compact = compact(model_pas)

    assert "function SetTableStyleName" in model_pas
    assert "ApplyDXFTableStyleByName" in model_pas
    assert "FTableStyleHandle:=NewHandle" in model_compact
    assert "FContinuationParts[PartIdx].TableStyle:=FTableStyle" in model_compact
    assert "FContinuationParts[PartIdx].TableStyleHandle:=NewHandle" in model_compact
    assert "FGeometryBuilt:=False" in model_compact
    assert "InvalidateRawDXFEntity;" in model_pas
    assert ".TableStyleTable" not in model_pas


def test_stylemanager_resolves_dxf_table_style_by_name():
    stylemanager_pas = read_text(STYLEMANAGER_PAS)

    assert "function ApplyDXFTableStyleByName" in stylemanager_pas
    assert "getAddres(AStyleName)" in stylemanager_pas
    assert "AStyleHandle := StylePtr^.DXFHandle" in stylemanager_pas
    assert "ApplyDXFTableStyleData" in stylemanager_pas


if __name__ == "__main__":
    test_acadtable_style_property_uses_dxf_style_combobox()
    test_model_applies_style_name_through_dxf_table_styles()
    test_stylemanager_resolves_dxf_table_style_by_name()
    print("issue 1336 AcadTable style inspector checks passed")
