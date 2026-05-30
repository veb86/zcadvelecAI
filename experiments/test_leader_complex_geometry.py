#!/usr/bin/env python3
from pathlib import Path
import re


ROOT = Path(__file__).resolve().parents[1]
LEADER_UNIT = ROOT / "cad_source" / "zengine" / "core" / "entities" / "uzeentleader.pas"
LEADER_TEST = ROOT / "cad_source" / "zengine" / "tests" / "uzctentleader.pas"


def read(path: Path) -> str:
    return path.read_text(encoding="utf-8")


def assert_contains(text: str, needle: str, context: str) -> None:
    if needle not in text:
        raise AssertionError(f"{context}: missing {needle!r}")


def test_leader_is_complex_entity() -> None:
    source = read(LEADER_UNIT)
    assert_contains(source, "uzeentcomplex", "leader unit uses")
    assert_contains(source, "GDBObjLeader=object(GDBObjComplex)", "leader type")


def test_leader_builds_arrow_from_dimstyle() -> None:
    source = read(LEADER_UNIT)
    assert_contains(source, "ConstObjArray.CreateInitObj(GDBlineID,@self)", "leader path")
    assert_contains(source, "ENTF_CreateBlockInsert(@self,@self.ConstObjArray", "leader arrow block")
    assert_contains(source, "GetDimBlockParam(-1)", "leader dimstyle arrow")
    assert_contains(source, "Arrows.DIMASZ", "leader arrow scale")
    if not re.search(r"ArrowHeadFlag\s*<>\s*0", source):
        raise AssertionError("leader arrow block must be controlled by ArrowHeadFlag")


def test_pascal_regression_covers_complex_geometry() -> None:
    source = read(LEADER_TEST)
    assert_contains(source, "FormatBuildsLeaderPathAndArrowBlock", "leader regression")
    assert_contains(source, "Leader^.ConstObjArray.Count", "leader regression")
    assert_contains(source, "TSOblique", "leader regression")
    assert_contains(source, "GDBBlockInsertID", "leader regression")


if __name__ == "__main__":
    test_leader_is_complex_entity()
    test_leader_builds_arrow_from_dimstyle()
    test_pascal_regression_covers_complex_geometry()
