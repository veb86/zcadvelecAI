#!/usr/bin/env python3
"""
Regression checks for issue 1342 AcadTable DXF loader compile contract.
"""

from pathlib import Path
import re


ROOT = Path(__file__).resolve().parent
DXF_LOADER = ROOT / "cad_source" / "zengine" / "fileformats" / "uzeffdxf.pas"
ENTITY_BASE = ROOT / "cad_source" / "zengine" / "core" / "entities" / "uzeentity.pas"
ACADTABLE_MODEL = (
    ROOT
    / "cad_source"
    / "zcad"
    / "velec"
    / "acadtable"
    / "uzeacadtable_model.pas"
)


def read_text(path: Path) -> str:
    return path.read_text(encoding="utf-8")


def compact(text: str) -> str:
    return "".join(text.split()).lower()


def test_dxf_loader_calls_break_flags_through_base_entity():
    dxf_loader = compact(read_text(DXF_LOADER))

    assert "plastmaintable^.setbreakoptionflags(" in dxf_loader
    assert "context.tablebreakmanualposition" in dxf_loader
    assert "context.tablebreakmanualheight" in dxf_loader


def test_base_entity_declares_break_flags_hook_for_loader_call():
    entity_base = compact(read_text(ENTITY_BASE))

    assert (
        "proceduresetbreakoptionflags("
        "amanualposition,amanualheight:boolean);virtual;"
    ) in entity_base
    assert (
        "proceduregdbobjentity.setbreakoptionflags("
        "amanualposition,amanualheight:boolean);"
    ) in entity_base


def test_acadtable_model_provides_specialized_break_flags_handler():
    acadtable_model = read_text(ACADTABLE_MODEL)
    acadtable_compact = compact(acadtable_model)

    assert re.search(
        r"procedure\s+SetBreakOptionFlags\s*\("
        r"\s*AManualPosition,\s*AManualHeight:\s*Boolean\s*\)\s*;\s*virtual\s*;",
        acadtable_model,
        re.IGNORECASE,
    )
    assert (
        "proceduregdbobjacadtable.setbreakoptionflags("
        "amanualposition,amanualheight:boolean);"
    ) in acadtable_compact
    assert "fbreakflagsknown:=true" in acadtable_compact
    assert "setbreakmanualpositionforparts(amanualposition)" in acadtable_compact
    assert "setbreakmanualheightforparts(amanualheight)" in acadtable_compact


if __name__ == "__main__":
    test_dxf_loader_calls_break_flags_through_base_entity()
    test_base_entity_declares_break_flags_hook_for_loader_call()
    test_acadtable_model_provides_specialized_break_flags_handler()
    print("issue 1342 AcadTable compile contract checks passed")
