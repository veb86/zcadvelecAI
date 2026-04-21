#!/usr/bin/env python3
"""Detailed decoding of OpCode 11 body, second try.

According to AutoCAD documentation and what we see in the data:
OpCode 11 (Text2) in DXF 2000 format has the following structure:

  Position (3 doubles)
  Normal (3 doubles)
  Direction (3 doubles)
  Text (ANSI null-terminated, padded to DWORD)
  Length (int32) = text length (or -1)
  Raw (int32)
  Height (double)
  WidthFactor (double)
  ObliqueAngle (double)
  -- next parts differ
"""
import struct

# Bodies from both bin files
body1 = bytes.fromhex("90194de2f795a4401868bcc075f89740000000000000000000000000000000000000000000000000000000000000f03f000000000000f03f000000000000000000000000000000003135"
                     "0000"  # 2 byte padding after "15\0"
                     "ffffffff"  # Length = -1
                     "00000000"  # Raw = 0
                     "0000000000e07540"  # Height = 350.0
                     "000000000000f03f"  # WidthFactor = 1.0
                     "0000000000000000"  # Oblique = 0.0
                     "000000000000f03f"  # field A = 1.0
                     "0000000000000000"  # field B = 0.0
                     "0000000000000000"  # field C = 0.0
                     "0000000000000000"  # field D = 0.0
                     "00000000"          # field E = 0 (int32?)
                     "43535f476f7374323330342e7368780000" # "CS_Gost2304.shx\0\0" 17 bytes
                     "000000"             # pad 3 bytes to 4-byte boundary
                     "00")                # pad more?
# Let me count:
# 72 geometry + 4 "15\0\0" + 4 length + 4 raw + 8 height + 8 wfac + 8 oblique = 108
# + 8*4 = 32 more doubles = 140
# + 4 more = 144
# + 17 string = 161 (too much)

print(f"body1 total: {len(body1)}")

# From the hex dump:
# 108: 000000000000f03f 000000000000000000000000000000000000000000000000 00000000 43535f476f7374323330342e73687800 0000000000
# That's from 108 to 156 = 48 bytes
# 48 = 8 + 8 + 8 + 8 + 4 + 12
# Let's try: 4 doubles (32 bytes) + 1 int32 (4 bytes) = 36 bytes + 12 bytes font
# Actually font is 17 bytes plus padding -> 20 bytes total

# Full body 156 bytes actual
actual = bytes.fromhex("90194de2f795a4401868bcc075f89740000000000000000000000000000000000000000000000000000000000000f03f000000000000f03f0000000000000000000000000000000031350000ffffffff000000000000000000e07540000000000000f03f0000000000000000000000000000f03f000000000000000000000000000000000000000043535f476f7374323330342e7368780000000000")

off = 0

def read_d():
    global off
    v = struct.unpack_from("<d", actual, off)[0]; off += 8; return v
def read_i():
    global off
    v = struct.unpack_from("<i", actual, off)[0]; off += 4; return v

print(f"\nPosition: ({read_d():.3f}, {read_d():.3f}, {read_d():.3f})")
print(f"Normal: ({read_d():.3f}, {read_d():.3f}, {read_d():.3f})")
print(f"Direction: ({read_d():.3f}, {read_d():.3f}, {read_d():.3f})")
# off = 72

# Text ANSI padded
start = off
end = actual.find(b'\x00', off)
text = actual[start:end].decode('latin-1')
off = end + 1
while off % 4 != 0:
    off += 1
# off = 76
print(f'Text: "{text}" off={off}')

print(f"Length (i32): {read_i()}")  # 80
print(f"Raw (i32): {read_i()}")  # 84
print(f"Height: {read_d()}")  # 92
print(f"WidthFactor: {read_d()}")  # 100
print(f"Oblique: {read_d()}")  # 108

# At off 108: 000000000000f03f 000000000000000000000000000000000000000000000000 00000000 43535f476f7374323330342e73687800 0000000000
# So next 8 bytes = 1.0 (could be tracking)
print(f"Tracking: {read_d()}")  # 116

# Remaining 40 bytes: 000000000000000000000000000000000000000000000000 00000000 43535f476f7374323330342e73687800 0000000000
# That's 40 bytes
# Let me try: 3 doubles (24 bytes)? or 6 int32 (24 bytes)?
print(f"\nAt off={off}, remaining: {len(actual)-off}")
print(f"Remaining: {actual[off:].hex()}")

# Let's try 3 more doubles for flags in double form? Unlikely
# Try 5 int32 flags:
flags = [read_i() for _ in range(5)]
print(f"Flags (5 x i32): {flags}")  # off +20 = 136

# Now at 136, remaining 20 bytes
print(f"At off={off}, remaining hex: {actual[off:].hex()}")

# Actually we need to find the font string "CS_Gost2304.shx" - its bytes are 43535f...
# Let me search for it:
font_offset = actual.find(b'CS_Gost2304.shx')
print(f"\nFont string found at offset: {font_offset}")
# Length = 15 chars + null = 16 bytes + padding

# That means BEFORE font string, data is: 136-font_offset bytes
# font_offset should be where font begins
