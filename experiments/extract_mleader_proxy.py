#!/usr/bin/env python3
"""Extract proxy graphic binary from MULTILEADER DXF entries and decode commands.

DXF MULTILEADER entries can carry an embedded proxy graphic block (group
codes 92 length + 310 hex chunks) inside the AcDbEntity subclass section,
before the AcDbMLeader subclass section starts. This script finds it and
walks the proxy-graphic command stream.
"""

import struct
import sys
from pathlib import Path

OPCODE_NAMES = {
    1: "EXTENTS",
    2: "CIRCLE",
    3: "CIRCLE_3P",
    4: "CIRCULAR_ARC",
    5: "CIRCULAR_ARC_3P",
    6: "POLYLINE",
    7: "POLYGON",
    8: "MESH",
    9: "SHELL",
    10: "TEXT",
    11: "TEXT2",
    12: "XLINE",
    13: "RAY",
    14: "SET_COLOR",
    16: "SET_LAYER",
    18: "SET_LINETYPE",
    19: "SET_MARKER",
    20: "SET_FILL",
    22: "SET_TRUE_COLOR",
    23: "SET_LINEWEIGHT",
    24: "SET_LTSCALE",
    25: "SET_THICKNESS",
    26: "SET_PLOT_STYLE",
    27: "PUSH_CLIP",
    28: "POP_CLIP",
    29: "PUSH_MATRIX",
    30: "PUSH_MATRIX2",
    31: "POP_MATRIX",
    32: "POLYLINE_WITH_NORMALS",
    33: "LWPOLYLINE",
    34: "SET_MATERIAL",
    35: "SET_MAPPER",
    36: "UNICODE_TEXT",
    38: "UNICODE_TEXT2",
    44: "ELLIPTIC_ARC",
}


def read_dxf_pairs(path: Path):
    """Yield (code_str, value_str) tuples; strip whitespace."""
    text = path.read_text(encoding="latin-1").splitlines()
    i = 0
    while i + 1 < len(text):
        code = text[i].strip()
        value = text[i + 1].rstrip("\r\n")
        yield code, value
        i += 2


def extract_proxy_hex(path: Path, entity_name: str) -> str:
    """Walk DXF pairs, collect 310 chunks belonging to the named entity.

    Stop collecting once a subclass other than AcDbEntity (the one carrying
    proxy graphic) is encountered (such as AcDbMLeader).
    """
    in_entity = False
    collecting = False
    chunks = []
    for code, value in read_dxf_pairs(path):
        if code == "0":
            if in_entity:
                # Reached next entity; stop.
                break
            if value == entity_name:
                in_entity = True
                collecting = True
            continue
        if not in_entity:
            continue
        if code == "100":
            # Subclass marker. AcDbEntity carries the proxy-graphic
            # group codes (92, 310 ...). Once a different subclass
            # appears (e.g. AcDbMLeader), stop accumulating.
            collecting = (value == "AcDbEntity")
            continue
        if collecting and code == "310":
            chunks.append(value.strip())
    return "".join(chunks)


def decode_proxy(data: bytes, label: str) -> None:
    print(f"\n{'=' * 60}")
    print(f"=== {label}: {len(data)} bytes ===")
    print(f"{'=' * 60}")
    if len(data) < 8:
        print("Too small")
        return
    chunk_size = struct.unpack_from("<i", data, 0)[0]
    command_count = struct.unpack_from("<i", data, 4)[0]
    print(f"ChunkSize={chunk_size} CommandCount={command_count}")
    offset = 8
    cmd_num = 0
    while offset + 8 <= len(data):
        cmd_size = struct.unpack_from("<i", data, offset)[0]
        cmd_opcode = struct.unpack_from("<i", data, offset + 4)[0]
        name = OPCODE_NAMES.get(cmd_opcode, f"UNKNOWN_{cmd_opcode}")
        cmd_num += 1
        payload_size = cmd_size - 8
        payload = data[offset + 8: offset + cmd_size] if payload_size > 0 else b""
        print(f"  cmd#{cmd_num:3d} off={offset:5d} size={cmd_size:4d} op={cmd_opcode:3d} {name}")

        if cmd_opcode == 6 and payload_size >= 4:  # POLYLINE
            vc = struct.unpack_from("<i", payload, 0)[0]
            print(f"        polyline vertices={vc}")
            for j in range(min(vc, 8)):
                p = 4 + j * 24
                if p + 24 <= payload_size:
                    x, y, z = struct.unpack_from("<3d", payload, p)
                    print(f"           v{j}: ({x:.4f}, {y:.4f}, {z:.4f})")
        elif cmd_opcode == 33 and payload_size >= 4:  # LWPOLYLINE
            vc = struct.unpack_from("<i", payload, 0)[0]
            print(f"        lwpolyline vertices={vc}")
            for j in range(min(vc, 8)):
                p = 4 + j * 24
                if p + 24 <= payload_size:
                    x, y, z = struct.unpack_from("<3d", payload, p)
                    print(f"           v{j}: ({x:.4f}, {y:.4f}, {z:.4f})")
        elif cmd_opcode == 9 and payload_size >= 4:  # SHELL
            vc = struct.unpack_from("<i", payload, 0)[0]
            print(f"        shell vertices={vc}")
            for j in range(min(vc, 8)):
                p = 4 + j * 24
                if p + 24 <= payload_size:
                    x, y, z = struct.unpack_from("<3d", payload, p)
                    print(f"           v{j}: ({x:.4f}, {y:.4f}, {z:.4f})")
        elif cmd_opcode == 36 and payload_size >= 72:  # UNICODE_TEXT
            px, py, pz = struct.unpack_from("<3d", payload, 0)
            print(f"        unicode_text pos=({px:.4f},{py:.4f},{pz:.4f})")

        if cmd_size <= 0:
            print("  invalid size, abort")
            break
        offset += cmd_size

    print(f"  Total commands parsed: {cmd_num}, offset reached {offset}/{len(data)}")


if __name__ == "__main__":
    base = Path(__file__).resolve().parent.parent / "cad_source" / "test"
    for name in ("mleader2000notwork.dxf", "mleader2007notwork.dxf"):
        p = base / name
        hex_data = extract_proxy_hex(p, "MULTILEADER")
        data = bytes.fromhex(hex_data)
        decode_proxy(data, name)
