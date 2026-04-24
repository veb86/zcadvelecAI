#!/usr/bin/env python3
"""Prototype: decode LWPOLYLINE proxy command using BitStream (DWG-style).

The proxy graphic LWPOLYLINE block is bit-packed (per Open Design Spec
LWPLINE 20.4.85). Reading it as plain int+doubles gives garbage (the
existing ZCAD parser currently rejects the data because the declared
vertex count is implausible).
"""

import struct
from pathlib import Path

from extract_mleader_proxy import extract_proxy_hex


class EndOfBuffer(Exception):
    pass


class BitStream:
    def __init__(self, buffer: bytes):
        self.buffer = memoryview(buffer)
        self.bit_index = 0

    def read_bits(self, count: int) -> int:
        index = self.bit_index
        buffer = self.buffer
        next_bit_index = index + count
        if (next_bit_index - 1) >> 3 >= len(buffer):
            raise EndOfBuffer()
        self.bit_index = next_bit_index
        test_bit = 0x80 >> (index & 7)
        test_byte_index = index >> 3
        value = 0
        test_byte = buffer[test_byte_index]
        while count > 0:
            value <<= 1
            if test_byte & test_bit:
                value |= 1
            count -= 1
            test_bit >>= 1
            if not test_bit and count:
                test_bit = 0x80
                test_byte_index += 1
                test_byte = buffer[test_byte_index]
        return value

    def read_aligned_bytes(self, count: int) -> bytes:
        start_index = self.bit_index >> 3
        end_index = start_index + count
        if end_index > len(self.buffer):
            raise EndOfBuffer()
        self.bit_index += count << 3
        return bytes(self.buffer[start_index:end_index])

    def read_float(self) -> float:
        if self.bit_index & 7:
            data = bytes(self.read_bits(8) for _ in range(8))
        else:
            data = self.read_aligned_bytes(8)
        return struct.unpack("<d", data)[0]

    def read_unsigned_byte(self) -> int:
        return self.read_bits(8)

    def read_unsigned_long(self) -> int:
        if self.bit_index & 7:
            l1 = self.read_bits(8)
            l2 = self.read_bits(8)
            l3 = self.read_bits(8)
            l4 = self.read_bits(8)
        else:
            l1, l2, l3, l4 = self.read_aligned_bytes(4)
        return (l4 << 24) + (l3 << 16) + (l2 << 8) + l1

    def read_signed_long(self) -> int:
        v = self.read_unsigned_long()
        if v & 0x80000000:
            return -((~v & 0xFFFFFFFF) + 1)
        return v

    def read_signed_short(self) -> int:
        if self.bit_index & 7:
            l1 = self.read_bits(8)
            l2 = self.read_bits(8)
        else:
            l1, l2 = self.read_aligned_bytes(2)
        v = (l2 << 8) + l1
        if v & 0x8000:
            return v - 0x10000
        return v

    def read_bit_short(self) -> int:
        bits = self.read_bits(2)
        if bits == 0:
            return self.read_signed_short()
        elif bits == 1:
            return self.read_unsigned_byte()
        elif bits == 2:
            return 0
        return 256

    def read_bit_long(self) -> int:
        bits = self.read_bits(2)
        if bits == 0:
            return self.read_signed_long()
        elif bits == 1:
            return self.read_unsigned_byte()
        elif bits == 2:
            return 0
        return 256

    def read_bit_double(self) -> float:
        bits = self.read_bits(2)
        if bits == 0:
            return self.read_float()
        elif bits == 1:
            return 1.0
        elif bits == 2:
            return 0.0
        return 0.0

    def read_bit_double_default(self, default: float) -> float:
        data = struct.pack("<d", default)
        bits = self.read_bits(2)
        if bits == 0:
            return default
        elif bits == 1:
            _data = bytes(self.read_unsigned_byte() for _ in range(4)) + data[4:]
            return struct.unpack("<d", _data)[0]
        elif bits == 2:
            _data = bytearray(data)
            _data[4] = self.read_unsigned_byte()
            _data[5] = self.read_unsigned_byte()
            _data[0] = self.read_unsigned_byte()
            _data[1] = self.read_unsigned_byte()
            _data[2] = self.read_unsigned_byte()
            _data[3] = self.read_unsigned_byte()
            return struct.unpack("<d", _data)[0]
        return self.read_float()


def parse_lwpolyline(data: bytes, dxf_at_least_2010: bool = False):
    bs = BitStream(data)
    num_data_bytes = bs.read_unsigned_long()
    flag = bs.read_bit_short()
    print(f"num_data_bytes={num_data_bytes} flag=0x{flag:x} ({flag})")
    attribs = {}
    if flag & 4:
        attribs["const_width"] = bs.read_bit_double()
    if flag & 8:
        attribs["elevation"] = bs.read_bit_double()
    if flag & 2:
        attribs["thickness"] = bs.read_bit_double()
    if flag & 1:
        attribs["extrusion"] = (bs.read_bit_double(), bs.read_bit_double(), bs.read_bit_double())
    is_closed = bool(flag & 512)
    num_points = bs.read_bit_long()
    print(f"num_points={num_points} closed={is_closed} attribs={attribs}")
    if num_points <= 0:
        return None
    num_bulges = 0
    num_vertex_ids = 0
    num_width = 0
    if flag & 16:
        num_bulges = bs.read_bit_long()
    if dxf_at_least_2010:
        if flag & 1024:
            num_vertex_ids = bs.read_bit_long()
        if flag & 32:
            num_width = bs.read_bit_long()

    vertices = [(bs.read_float(), bs.read_float())]
    prev = vertices[0]
    for _ in range(num_points - 1):
        x = bs.read_bit_double_default(prev[0])
        y = bs.read_bit_double_default(prev[1])
        prev = (x, y)
        vertices.append(prev)

    bulges = [bs.read_bit_double() for _ in range(num_bulges)]
    vertex_ids = [bs.read_bit_long() for _ in range(num_vertex_ids)]
    widths = [(bs.read_bit_double(), bs.read_bit_double()) for _ in range(num_width)]

    return {
        "flag": flag,
        "closed": is_closed,
        "attribs": attribs,
        "vertices": vertices,
        "bulges": bulges,
        "vertex_ids": vertex_ids,
        "widths": widths,
    }


def main():
    base = Path(__file__).resolve().parent.parent / "cad_source" / "test"
    hex_data = extract_proxy_hex(base / "mleader2007notwork.dxf", "MULTILEADER")
    data = bytes.fromhex(hex_data)
    # LWPOLYLINE command at offset 276, size 53, payload from 284 to 329
    offset = 276
    cmd_size = struct.unpack_from("<i", data, offset)[0]
    payload = data[offset + 8: offset + cmd_size]
    print(f"LWPOLYLINE payload: {len(payload)} bytes")
    print(payload.hex())
    print()
    result = parse_lwpolyline(payload, dxf_at_least_2010=False)
    print()
    print(result)


if __name__ == "__main__":
    main()
