#!/usr/bin/env python3
"""Simulate HandleText2 (our new handler) against the real binary data."""
import struct


class Stream:
    def __init__(self, body, unicode_mode=False):
        self.body = body
        self.off = 0
        self.unicode = unicode_mode

    def read_d(self):
        v = struct.unpack_from("<d", self.body, self.off)[0]
        self.off += 8
        return v

    def read_i(self):
        v = struct.unpack_from("<i", self.body, self.off)[0]
        self.off += 4
        return v

    def read_u(self):
        v = struct.unpack_from("<I", self.body, self.off)[0]
        self.off += 4
        return v

    def read_vertex(self):
        return self.read_d(), self.read_d(), self.read_d()

    def read_padded_unicode_string(self):
        """Equivalent of ReadPaddedUnicodeString."""
        if self.unicode:
            end = self.off
            while end + 1 < len(self.body) and (
                self.body[end] != 0 or self.body[end+1] != 0
            ):
                end += 2
            s = self.body[self.off:end].decode('utf-16-le', errors='replace')
            self.off = end + 2
        else:
            end = self.body.find(b'\x00', self.off)
            if end < 0:
                end = len(self.body)
            s = self.body[self.off:end].decode('latin-1', errors='replace')
            self.off = end + 1
        while self.off % 4 != 0 and self.off < len(self.body):
            self.off += 1
        return s


def handle_text2(stream):
    """Mimics the new HandleText2 procedure in Pascal."""
    insert = stream.read_vertex()
    normal = stream.read_vertex()
    direction = stream.read_vertex()

    saved_unicode = stream.unicode
    stream.unicode = False
    try:
        text = stream.read_padded_unicode_string()
        length = stream.read_i()
        raw = stream.read_i()
        height = stream.read_d()
        wfactor = stream.read_d()
        oblique = stream.read_d()
        tracking = stream.read_d()
        flags = [stream.read_u() for _ in range(5)]
        font = stream.read_padded_unicode_string()
        big_font = stream.read_padded_unicode_string()
    finally:
        stream.unicode = saved_unicode

    return {
        'insert': insert, 'normal': normal, 'direction': direction,
        'text': text, 'length': length, 'raw': raw,
        'height': height, 'wfactor': wfactor, 'oblique': oblique,
        'tracking': tracking, 'flags': flags,
        'font': font, 'big_font': big_font,
    }


with open('/tmp/gh-issue-solver-1776753338785/cad_source/test/spdsconstructionline2000.proxy0.bin', 'rb') as f:
    data = f.read()

# Skip header
off = 8
cmd_count = struct.unpack_from("<ii", data, 0)[1]

for i in range(cmd_count):
    size, opcode = struct.unpack_from("<ii", data, off)
    if opcode == 11:
        body = data[off+8:off+size]
        stream = Stream(body, unicode_mode=True)  # simulates DXF 2000 unicode mode
        # Wait, for DXF 2000 files, AUnicodeText = False
        # Let me use False (as the fix is)
        stream.unicode = False
        result = handle_text2(stream)
        print(f"\n=== OpCode 11 at offset {off} ===")
        print(f"  Insert: {result['insert']}")
        print(f"  Text: {result['text']!r}")
        print(f"  Height: {result['height']}")
        print(f"  Font: {result['font']!r}")
        print(f"  BigFont: {result['big_font']!r}")
        print(f"  Final off in body: {stream.off}/{len(body)}")
        if stream.off != len(body):
            print(f"  WARNING: didn't consume all body")
    off += size
