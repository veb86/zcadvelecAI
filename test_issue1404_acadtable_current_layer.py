#!/usr/bin/env python3
"""Regression contract for issue #1404: new AcadTable uses current layer."""

from pathlib import Path


ROOT = Path(__file__).resolve().parent
COMMAND = (
    ROOT
    / "cad_source"
    / "zcad"
    / "velec"
    / "uzvspreadsheet"
    / "uzvspreadsheet_cmdcreateacadtable.pas"
)


def compact(text: str) -> str:
    return "".join(text.split()).lower()


def test_created_acadtable_receives_current_drawing_properties():
    source = compact(COMMAND.read_text(encoding="utf-8"))
    allocation = source.index("pt:=allocandinitacadtable(")
    insertion = source.index(
        "drawings.currentdwg^.constructobjroot.objarray.addpentity(pt^);",
        allocation,
    )
    setup = source[allocation:insertion]

    assert "zcsetentpropfromcurrentdrawingprop(pgdbobjentity(pt));" in setup


if __name__ == "__main__":
    test_created_acadtable_receives_current_drawing_properties()
    print("issue 1404 AcadTable current-layer checks passed")
