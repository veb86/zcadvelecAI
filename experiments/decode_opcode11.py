#!/usr/bin/env python3
"""Decode OpCode 11 (pgcText2) from the DXF 2000 file proxy graphics."""
import struct

# From 2000 file, after SetLinetype(0), starting at A40000000B000000
# Full sequence (164 bytes total, header 8 bytes + 156 bytes data):
hex2000 = (
    "A40000000B00000090194DE2F795A4401868BCC075"  # size + opcode + start data
    "F89740000000000000000000000000000000000000000000000000000000000000F03F"
    "000000000000F03F0000000000000000"
    "00000000000000003135"  # "15"
    "00"                    # terminator?
    "00FFFFFFFF000000000000000000E07540000000000000F03F"
    "0000000000000000000000000000F03F000000000000000000000000000000000000000043535F47"
    "6F7374323330342E73687800000000"
)

# Concatenate hex from logs (the first occurrence of OpCode 11):
# Parse the content
data_hex = (
    "90194DE2F795A4401868BCC075F89740"  # pos x,y (2 doubles)
    "0000000000000000"  # pos z
    "000000000000000000000000000000000000000000000000"  # Normal vector (3 doubles)
    "000000000000F03F000000000000F03F0000000000000000"  # weirdness here
    # Actually let me reparse
)

# Let me try again. The full data section (after removing 8-byte header):
data_after_header_hex = ("90194DE2F795A4401868BCC075F89740"  # x, y
    "0000000000000000"                                      # z
    "000000000000000000000000000000000000000000000000"      # normal (x=0,y=0,z=0)
    "00000000000000000000000000000000"                      # direction (0,0)
    "0000F03F"                                              # Z-component of direction = 1.0
    "000000000000F03F"
    "0000000000000000"
    "0000000000000000"
    "00000000000000003135001FFFFFFFFF"  # "15" + flags
    "000000000000000000E07540"
    "000000000000F03F"
    "0000000000000000"
    "0000000000000000"
    "0000F03F"
    "0000000000000000"
    "00000000000000000000000043535F476F7374323330342E7368780000")

# Actually, let me extract the exact 156 bytes from the hex dump above.
# Concatenating the continuous hex chunks and removing first 8 bytes (A40000000B000000):
raw_hex = (
    "A40000000B00000090194DE2F795A4401868BCC075"
    "F89740000000000000000000000000000000000000000000000000000000000000F03F"
    "000000000000F03F00000000000000000000000000000000"
    "3135"  # 0x3135 = "15" in ASCII
    "001FFFFFFFFF000000000000000000E07540000000000000F03F"
    "0000000000000000000000000000F03F00000000000000000000000000000000"
    "0000000043535F476F7374323330342E73687800"
    "0000"
)
# That's from the second instance; the first was with "00" between 3135 and FFFF...
# Let me use first:
raw_hex_first = (
    "A40000000B00000090194DE2F795A4401868BCC075"  # 8-byte header, then 16 bytes (first data)
    "F89740000000000000000000000000000000000000000000000000000000000000F03F"
    "000000000000F03F000000000000000000000000000000000000000031350000FFFFFFFF"
    "000000000000000000E07540000000000000F03F0000000000000000000000000000F03F"
    "000000000000000000000000000000000000000043535F476F7374323330342E73687800000000000C00000016"
)
# But 0C00000016 is next OpCode... So drop there

# Strip spaces/non-hex
hex_clean = ''.join(c for c in raw_hex_first if c in '0123456789abcdefABCDEF')
# Remove the next OpCode which starts at the end
# Last full command is "0C00000016...". Before that is "000000000C000000" which we include up to 156 bytes after the 8-byte header

data = bytes.fromhex(hex_clean)
print(f"Total bytes: {len(data)}")

# Parse header
size, opcode = struct.unpack("<ii", data[:8])
print(f"Size={size}, OpCode={opcode}")

body = data[8:8+size-8]  # 156 bytes of body
print(f"Body length: {len(body)}")

# OpCode 11 format per AutoCAD AcGiWorldDraw (unicode text):
# Position (3 doubles = 24 bytes)
# Normal (3 doubles = 24 bytes)
# Direction (3 doubles = 24 bytes)
# Msg (null-terminated widestring/ansi)
# Length (int32)
# Raw (int32)
# Height (double)
# WidthFactor (double)

offset = 0
def read_double():
    global offset
    v = struct.unpack_from("<d", body, offset)[0]
    offset += 8
    return v

def read_int32():
    global offset
    v = struct.unpack_from("<i", body, offset)[0]
    offset += 4
    return v

px, py, pz = read_double(), read_double(), read_double()
print(f"Position: ({px:.3f}, {py:.3f}, {pz:.3f})")

nx, ny, nz = read_double(), read_double(), read_double()
print(f"Normal: ({nx:.3f}, {ny:.3f}, {nz:.3f})")

dx, dy, dz = read_double(), read_double(), read_double()
print(f"Direction: ({dx:.3f}, {dy:.3f}, {dz:.3f})")

# Now read ANSI null-terminated string (DXF 2000 uses ANSI)
print(f"After geometry, offset = {offset}, remaining = {len(body) - offset}")
print(f"Next bytes: {body[offset:offset+30].hex()}")

# Try reading as ANSI null-terminated
start = offset
end = body.find(b'\x00', offset)
if end >= 0:
    text = body[start:end].decode('latin-1')
    print(f'Text ANSI null-term: "{text}" (len={end-start})')
    offset = end + 1
    # Check if padding needed
    # Skip padding to 4-byte boundary
    while offset % 4 != 0:
        offset += 1

print(f"After text, offset = {offset}, remaining = {len(body) - offset}")
print(f"Next bytes (hex): {body[offset:offset+40].hex()}")

# Try Length, Raw, Height, WidthFactor
length = read_int32()
raw = read_int32()
height = read_double()
widthfactor = read_double()
print(f"Length={length}, Raw={raw}, Height={height}, WidthFactor={widthfactor}")

# Next fields: ObliqueAngle + 5 uint32 flags + FontName
print(f"Remaining: {len(body) - offset}, next bytes: {body[offset:offset+40].hex()}")

oblique = read_double()
print(f"ObliqueAngle={oblique}")

# Maybe tracking
tracking = read_double()
print(f"Tracking={tracking}")

flags = []
for i in range(5):
    flags.append(read_int32())
print(f"Flags: {flags}")

# Now font name (ANSI null-term)
print(f"After flags, offset = {offset}, remaining = {len(body) - offset}")
print(f"Next bytes: {body[offset:offset+40].hex()}")
start = offset
end = body.find(b'\x00', offset)
if end >= 0:
    fontname = body[start:end].decode('latin-1')
    print(f'FontName: "{fontname}"')
    offset = end + 1

print(f"Final offset: {offset}/{len(body)}")
print(f"Last bytes: {body[offset:].hex()}")
