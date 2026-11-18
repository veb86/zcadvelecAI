"""Exercise wall recognition in a running ZCAD desktop with a real DXF drawing."""

import argparse
import json
import math
import shutil
import subprocess
import sys
import time
from pathlib import Path

from wall_client import IPCError, ipc, recognize


LAYER = "ZCAD_AI_WALL_PREVIEW"
PREFIX = "ZCAD_WALLS_"


def fixture(path):
    data = [(0, "SECTION"), (2, "HEADER"), (9, "$ACADVER"), (1, "AC1015"),
            (0, "ENDSEC"), (0, "SECTION"), (2, "TABLES"),
            (0, "TABLE"), (2, "LAYER"), (70, 2)]
    for name, plot in (("0", 1), (LAYER, 0)):
        data.extend([(0, "LAYER"), (100, "AcDbSymbolTableRecord"),
                     (100, "AcDbLayerTableRecord"), (2, name), (70, 0),
                     (62, 7), (6, "CONTINUOUS"), (290, plot)])
    data.extend([(0, "ENDTAB"), (0, "ENDSEC"), (0, "SECTION"), (2, "ENTITIES")])

    def line(handle, start, end, layer="0"):
        data.extend([(0, "LINE"), (5, handle), (100, "AcDbEntity"), (8, layer), (100, "AcDbLine")])
        data.extend(zip((10, 20, 30, 11, 21, 31), start + end))

    line("A1", (0, 0, 0), (1000, 0, 0))
    line("A2", (1200, 200, 0), (150, 200, 0))
    line("A3", (100, 1100, 0), (900, 1100, 0))
    line("A4", (1600, 0, 0), (2400, 0, 400))
    line("A5", (1600, 200, 0), (2400, 200, 400))
    line("A6", (-150, 1200, 0), (-50, 1200, 0), LAYER)
    data.extend([(0, "LWPOLYLINE"), (5, "A7"), (100, "AcDbEntity"),
                 (8, "0"), (100, "AcDbPolyline"), (90, 5), (70, 1)])
    for x, y, bulge in [(100, 900, .2), (0, 900, .2), (0, 750, .2),
                        (850, 750, .2), (900, 900, 0)]:
        data.extend([(10, x), (20, y), (42, bulge)])
    data.extend([(0, "ENDSEC"), (0, "EOF")])
    path.write_text("".join(f"{code}\n{value}\n" for code, value in data), encoding="ascii")


def classify():
    request = json.load(sys.stdin)
    assert request["coordinate_system"] == "WCS"
    assert len(request["candidates"]) == 3
    handles = {frozenset(candidate["source_handles"]) for candidate in request["candidates"]}
    assert handles == {frozenset(pair) for pair in (("A1", "A2"), ("A3", "A7"), ("A4", "A5"))}
    walls = []
    for candidate in request["candidates"]:
        a, b = candidate["boundaries"]
        axis = candidate["centerline"]
        for end in ("start", "end"):
            for actual, x, y in zip(axis[end], a[end], b[end], strict=True):
                assert math.isclose(actual, (x + y) / 2, abs_tol=1e-8)
        walls.append(
            dict(candidate_id=candidate["candidate_id"], is_wall=True, confidence=.93,
                 thickness=candidate["thickness"], source_handles=candidate["source_handles"],
                 centerline={"vertices": [axis["start"], axis["end"]]}))
    walls.append(dict(candidate_id="C999", is_wall=True))
    json.dump({"session_id": request["session_id"], "walls": walls}, sys.stdout)


def read_dxf(path):
    section = records = None
    entities, layers = [], []
    with path.open(encoding="latin-1") as stream:
        for code, value in zip(stream, stream):
            code, value = int(code), value.strip()
            if code == 0:
                if value == "SECTION":
                    section = None
                if value in ("LINE", "LWPOLYLINE", "POLYLINE") and section == "ENTITIES":
                    records = [(0, value)]
                    entities.append(records)
                elif value == "LAYER" and section == "TABLES":
                    records = [(0, value)]
                    layers.append(records)
                else:
                    records = None
            elif section is None and code == 2:
                section = value
            elif records is not None:
                records.append((code, value))

    preview_layers = [dict(layer) for layer in layers if dict(layer).get(2) == LAYER]
    assert len(preview_layers) == 1 and preview_layers[0].get(290) == "0"
    walls, originals = [], []
    for entity in entities:
        fields = dict(entity)
        values = dict(value.removeprefix(PREFIX).split("=", 1) for code, value in entity
                      if code == 1000 and value.startswith(PREFIX))
        if values:
            assert (1001, "DSTP_XDATA") in entity
            walls.append((fields, values))
        else:
            originals.append(entity)
    return walls, originals


def verify(args):
    args.output.mkdir(parents=True, exist_ok=True)
    dxfin = args.output / "source.dxf"
    fixture(dxfin)

    def command(text):
        subprocess.run(["xdotool", "mousemove", "--window", args.window, *args.console,
                        "click", "1", "type", "--clearmodifiers", text], check=True)
        subprocess.run(["xdotool", "key", "--clearmodifiers", "Return"], check=True)

    def call(name, *arguments):
        return ipc(args.endpoint, "", name, *arguments)

    def error(name, *arguments):
        try:
            return call(name, *arguments)
        except IPCError as failure:
            return str(failure)
        raise AssertionError(f"Expected error from {name}")

    def capture(name):
        if args.screenshots:
            from PIL import ImageGrab
            ImageGrab.grab().save(args.output / f"{name}.png")

    def phase(session, expected):
        for _ in range(20):
            result = call("WALL_PREVIEW", session)
            if result["phase"] == expected:
                return result
            time.sleep(.15)
        raise AssertionError(f"Expected {expected}, saw {result}")

    def exported(name):
        path = args.output / name
        command(f"SaveAs({path.resolve()})")
        for _ in range(30):
            if path.exists() and path.read_bytes().rstrip().endswith(b"EOF"):
                return read_dxf(path)
            time.sleep(.15)
        raise AssertionError(f"Missing saved DXF {path}")

    command(f"Load({dxfin.resolve()})")
    time.sleep(.8)
    command("Zoom(All)")
    time.sleep(.4)
    capture("drawing")
    options = {"window": [[-200, -100], [2500, 1300]],
               "min_thickness": 150, "max_thickness": 250}
    assert error("WALL_ANALYZE", dict(options, min_thickness=250))
    assert error("WALL_ANALYZE", dict(options, window=[[3000, 3000], [4000, 4000]]))
    assert error("WALL_ANALYZE", dict(options, min_thickness=201))

    first = call("WALL_ANALYZE", {"handles": ["A1", "A2"], "min_thickness": 150,
                                 "max_thickness": 250})
    session = first["session_id"]
    assert first["candidate_count"] == 1
    assert "WallSend" in error("WALL_SEND_AI", session)
    assert error("WALL_ANALYZE", options)
    command("WallCancel")
    phase(session, "cancelled")
    assert error("WALL_SEND_AI", session)
    walls, originals = exported("cancelled.dxf")
    assert not walls and len(originals) == 7

    command("WallAnalyze")
    for value in ("150", "250", "-200,-100,0", "2500,1300,0"):
        time.sleep(.25)
        command(value)
    first = call("WALL_PREVIEW")
    assert first["candidate_count"] == 3, first
    session = first["session_id"]
    capture("candidate-preview")
    command("WallSend")
    phase(session, "approved_to_send")
    assert error("WALL_AI_RESULT", {"session_id": "stale", "walls": []})
    classifier = [sys.executable, __file__, "--classify"]
    result = recognize(call, session, classifier, 10)
    assert result["recognized_count"] == 3 and result["unknown_candidate_count"] == 1
    assert result["confidence_min"] == result["confidence_max"] == .93
    assert call("WALL_COMMIT", session)["phase"] == "recognition_preview"
    capture("recognition-preview")

    command("WallCancel")
    phase(session, "cancelled")
    walls, before_commit = exported("preview.dxf")
    assert not walls and before_commit == originals

    session = call("WALL_ANALYZE", options)["session_id"]
    command("WallSend")
    phase(session, "approved_to_send")
    assert recognize(call, session, classifier, 10)["recognized_count"] == 3
    command("WallCommit")
    assert phase(session, "committed")["created_count"] == 3
    capture("committed")
    walls, after_commit = exported("created.dxf")
    assert after_commit == originals
    assert sorted(entity[0] for entity, _ in walls) == ["LWPOLYLINE", "LWPOLYLINE", "POLYLINE"]
    original_handles = {dict(entity)[5] for entity in after_commit}
    for _, metadata in walls:
        assert metadata["OBJECT_TYPE"] == "WALL"
        assert metadata["REPRESENTATION"] == "CENTERLINE"
        assert metadata["SOURCE"] == "AI"
        assert metadata["SESSION_ID"] == session
        assert len(metadata["SOURCE_HANDLES"].split(",")) == 2
        assert set(metadata["SOURCE_HANDLES"].split(",")) <= original_handles
        assert float(metadata["THICKNESS"]) == 200 and float(metadata["CONFIDENCE"]) == .93
    assert len({metadata["WALL_ID"] for _, metadata in walls}) == 3
    assert error("WALL_ANALYZE", options)

    command("Undo")
    walls, after_undo = exported("undone.dxf")
    assert not walls and after_undo == originals
    assert call("WALL_ANALYZE", options)["candidate_count"] == 3
    command("WallCancel")

    reopened = args.output / "reopened.dxf"
    shutil.copyfile(args.output / "created.dxf", reopened)
    command(f"Load({reopened.resolve()})")
    time.sleep(.8)
    assert error("WALL_ANALYZE", options)
    print("Wall preview, approvals, XData round-trip, duplicate prevention and grouped Undo passed")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path)
    parser.add_argument("--window")
    parser.add_argument("--console", nargs=2, default=["120", "713"])
    parser.add_argument("--endpoint", default="http://127.0.0.1:5000/ipc")
    parser.add_argument("--screenshots", action="store_true")
    parser.add_argument("--classify", action="store_true")
    args = parser.parse_args()
    if args.classify:
        classify()
    elif args.window and args.output:
        verify(args)
    else:
        parser.error("Provide --window and --output")


if __name__ == "__main__":
    main()
