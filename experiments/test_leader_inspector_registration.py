#!/usr/bin/env python3
from pathlib import Path
import re
import xml.etree.ElementTree as ET


ROOT = Path(__file__).resolve().parents[1]
LEADER_UNIT = ROOT / "cad_source" / "zcad" / "register" / "uzcregleader.pas"
ZCAD_MAIN = ROOT / "cad_source" / "zcad.pas"
ZCAD_LPI = ROOT / "cad_source" / "zcad.lpi"


def read(path: Path) -> str:
    return path.read_text(encoding="utf-8")


def assert_contains(text: str, needle: str, context: str) -> None:
    if needle not in text:
        raise AssertionError(f"{context}: missing {needle!r}")


def test_leader_registration_unit() -> None:
    source = read(LEADER_UNIT)

    for property_name in (
        "VertexCount",
        "Vertex3DControl_",
        "Length",
        "LeaderDimStyleName",
        "LeaderArrowHeadFlag",
        "LeaderPathType",
        "LeaderAnnotationType",
        "LeaderHookLineDirectionFlag",
        "LeaderHookLineFlag",
        "LeaderTextHeight",
        "LeaderTextWidth",
        "LeaderAnnotationHandle",
        "TotalVertexCount",
        "TotalLength",
    ):
        assert_contains(source, f"'{property_name}'", "leader multiproperty")

    if source.count("GDBLeaderID") < 8:
        raise AssertionError("expected leader registrations")
    if source.count("MPCGeometry") < 3:
        raise AssertionError("expected leader geometry properties")
    if source.count("MPCMisc") < 3:
        raise AssertionError("expected leader misc properties")
    if len(re.findall(r"MPCSummary,GDBLeaderID", source)) < 2:
        raise AssertionError("expected leader summary properties")


def test_leader_vertex_change_proc_uses_pointer_dereference() -> None:
    source = read(LEADER_UNIT)
    proc_match = re.search(
        r"procedure LeaderVertex3DControlFromVarEntChangeProc.*?^end;",
        source,
        re.DOTALL | re.MULTILINE,
    )
    if not proc_match:
        raise AssertionError("missing LeaderVertex3DControlFromVarEntChangeProc")

    proc_source = proc_match.group(0)
    if "pvardesk(pdata).name" in proc_source:
        raise AssertionError("PVarDesk fields must be accessed through pdata^")
    for expected in (
        "pdata^.name=mp.MPName",
        "pdata^.name=mp.MPName+'x'",
        "pdata^.name=mp.MPName+'y'",
        "pdata^.name=mp.MPName+'z'",
        "mp.MPType.CopyValueToInstance(pdata^.data.Addr.Instance,@Vertex3DControl)",
    ):
        assert_contains(proc_source, expected, "leader vertex change proc")


def test_leader_registration_is_wired() -> None:
    assert_contains(read(ZCAD_MAIN), "uzcregleader", "zcad.pas uses")

    tree = ET.parse(ZCAD_LPI)
    units = tree.findall(".//Units")[0]
    declared_count = int(units.attrib["Count"])
    unit_nodes = [child for child in units if re.fullmatch(r"Unit\d+", child.tag)]
    filenames = [
        filename.attrib["Value"]
        for unit in unit_nodes
        for filename in unit.findall("Filename")
    ]

    if declared_count != len(unit_nodes):
        raise AssertionError(
            f"zcad.lpi Units Count={declared_count}, actual Unit nodes={len(unit_nodes)}"
        )
    if "zcad/register/uzcregleader.pas" not in filenames:
        raise AssertionError("zcad.lpi does not include uzcregleader.pas")


if __name__ == "__main__":
    test_leader_registration_unit()
    test_leader_vertex_change_proc_uses_pointer_dereference()
    test_leader_registration_is_wired()
