#!/usr/bin/env python3
"""Decode the UNICODE_TEXT2 command in mleader2007notwork.dxf (cmd#23)
and TEXT2 command in mleader2000notwork.dxf (cmd#22).

Purpose: figure out why the text "ai123456" renders correctly from the
TEXT2 record but not from the UNICODE_TEXT2 record.
"""
import struct
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from extract_mleader_proxy import extract_proxy_hex


def decode_text2(payload: bytes) -> None:
    """OpCode=11 TEXT2 (DXF 2000). Payload starts after the 8-byte cmd
    header which is already stripped."""
    print(f"  TEXT2 payload len={len(payload)}")
    off = 0
    ins = struct.unpack_from("<3d", payload, off); off += 24
    nrm = struct.unpack_from("<3d", payload, off); off += 24
    direc = struct.unpack_from("<3d", payload, off); off += 24
    print(f"    Insert=({ins[0]:.4f},{ins[1]:.4f},{ins[2]:.4f})")
    print(f"    Normal=({nrm[0]:.4f},{nrm[1]:.4f},{nrm[2]:.4f})")
    print(f"    Direction=({direc[0]:.4f},{direc[1]:.4f},{direc[2]:.4f})")

    # ANSI string, null-terminated, padded to 4 bytes
    start = off
    end = payload.index(b"\x00", start)
    text = payload[start:end].decode("latin-1", errors="replace")
    off = end + 1
    # align 4
    while off % 4 != 0:
        off += 1
    print(f"    Text[{end-start} bytes + pad {off-(end+1)}]: {text!r}")

    length = struct.unpack_from("<i", payload, off)[0]; off += 4
    raw = struct.unpack_from("<i", payload, off)[0]; off += 4
    print(f"    Length={length} Raw={raw}")

    height = struct.unpack_from("<d", payload, off)[0]; off += 8
    wfactor = struct.unpack_from("<d", payload, off)[0]; off += 8
    oblique = struct.unpack_from("<d", payload, off)[0]; off += 8
    track = struct.unpack_from("<d", payload, off)[0]; off += 8
    print(f"    Height={height} WFactor={wfactor} Oblique={oblique} Tracking={track}")

    flags = struct.unpack_from("<5I", payload, off); off += 20
    print(f"    Flags[Backward,Upsidedown,Vertical,Underlined,Overlined]={flags}")

    # Font filename (ANSI, padded to 4 bytes)
    start = off
    end = payload.index(b"\x00", start) if b"\x00" in payload[start:] else len(payload)
    font = payload[start:end].decode("latin-1", errors="replace")
    off = end + 1 if end < len(payload) else len(payload)
    while off % 4 != 0 and off < len(payload):
        off += 1
    print(f"    FontFilename={font!r}")

    if off < len(payload):
        start = off
        end = payload.index(b"\x00", start) if b"\x00" in payload[start:] else len(payload)
        big = payload[start:end].decode("latin-1", errors="replace")
        off = end + 1 if end < len(payload) else len(payload)
        while off % 4 != 0 and off < len(payload):
            off += 1
        print(f"    BigFont={big!r}")
    print(f"    consumed={off}/{len(payload)}")


def decode_unicode_text2(payload: bytes) -> None:
    """OpCode=38 UNICODE_TEXT2 (DXF 2007+)."""
    print(f"  UNICODE_TEXT2 payload len={len(payload)}")
    off = 0
    ins = struct.unpack_from("<3d", payload, off); off += 24
    nrm = struct.unpack_from("<3d", payload, off); off += 24
    direc = struct.unpack_from("<3d", payload, off); off += 24
    print(f"    Insert=({ins[0]:.4f},{ins[1]:.4f},{ins[2]:.4f})")
    print(f"    Normal=({nrm[0]:.4f},{nrm[1]:.4f},{nrm[2]:.4f})")
    print(f"    Direction=({direc[0]:.4f},{direc[1]:.4f},{direc[2]:.4f})")

    # UTF-16 LE null-terminated, padded to 4 bytes
    start = off
    # find double zero aligned on even index
    end = start
    while end + 1 < len(payload):
        if payload[end] == 0 and payload[end + 1] == 0 and (end - start) % 2 == 0:
            break
        end += 2
    text_bytes = payload[start:end]
    text = text_bytes.decode("utf-16-le", errors="replace")
    off = end + 2
    while off % 4 != 0:
        off += 1
    print(f"    Text[bytes={end-start}]: {text!r}")

    ignore = struct.unpack_from("<i", payload, off)[0]; off += 4
    raw = struct.unpack_from("<i", payload, off)[0]; off += 4
    print(f"    IgnoreLen={ignore} Raw={raw}")

    height = struct.unpack_from("<d", payload, off)[0]; off += 8
    wfactor = struct.unpack_from("<d", payload, off)[0]; off += 8
    oblique = struct.unpack_from("<d", payload, off)[0]; off += 8
    track = struct.unpack_from("<d", payload, off)[0]; off += 8
    print(f"    Height={height} WFactor={wfactor} Oblique={oblique} Tracking={track}")

    flags = struct.unpack_from("<5I", payload, off); off += 20
    print(f"    Flags[Backward,Upsidedown,Vertical,Underlined,Overlined]={flags}")

    extra = struct.unpack_from("<4I", payload, off); off += 16
    print(f"    Extra[IsBold,IsItalic,Charset,Pitch]={extra}")

    # TypeFace (UTF-16 LE, padded to 4 bytes)
    def read_utf16_padded(b: bytes, o: int):
        s = o
        e = s
        while e + 1 < len(b):
            if b[e] == 0 and b[e + 1] == 0 and (e - s) % 2 == 0:
                break
            e += 2
        txt = b[s:e].decode("utf-16-le", errors="replace")
        o2 = e + 2
        while o2 % 4 != 0 and o2 < len(b):
            o2 += 1
        return txt, o2

    typeface, off = read_utf16_padded(payload, off)
    print(f"    TypeFace={typeface!r} off={off}")
    fontname, off = read_utf16_padded(payload, off)
    print(f"    FontName={fontname!r} off={off}")
    bigfont, off = read_utf16_padded(payload, off)
    print(f"    BigFont={bigfont!r} off={off}")
    print(f"    consumed={off}/{len(payload)}")
    # Hex dump tail
    if off < len(payload):
        print(f"    tail hex: {payload[off:].hex()}")


def run(name: str, opcode: int):
    base = Path(__file__).resolve().parent.parent / "cad_source" / "test"
    p = base / name
    hex_data = extract_proxy_hex(p, "MULTILEADER")
    data = bytes.fromhex(hex_data)
    # header
    offset = 8
    while offset + 8 <= len(data):
        cmd_size = struct.unpack_from("<i", data, offset)[0]
        cmd_op = struct.unpack_from("<i", data, offset + 4)[0]
        if cmd_op == opcode:
            payload = data[offset + 8: offset + cmd_size]
            print(f"=== {name} cmd op={opcode} size={cmd_size} ===")
            if opcode == 11:
                decode_text2(payload)
            elif opcode == 38:
                decode_unicode_text2(payload)
            return
        offset += cmd_size


if __name__ == "__main__":
    run("mleader2000notwork.dxf", 11)
    print()
    run("mleader2007notwork.dxf", 38)
