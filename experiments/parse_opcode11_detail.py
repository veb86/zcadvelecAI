#!/usr/bin/env python3
"""Detailed decoding of OpCode 11 body."""
import struct

# Take OpCode 11 body from 2000 file (156 bytes):
hex_body = ("90194de2f795a4401868bcc075f89740"  # pos X,Y
            "0000000000000000"                  # pos Z
            "000000000000000000000000000000000000000000000000"  # Normal (x,y,z)
            "00000000000000000000000000000000"  # Dir (x,y,...)
            "0000f03f"                          # Dir Z = 1.0?
            # Wait I think I miscounted. Let me redo.
            )

# Full 156 bytes:
full = bytes.fromhex("90194de2f795a4401868bcc075f89740"
                     "0000000000000000"
                     "00000000000000000000000000000000" # Normal xy
                     "0000f03f"                          # Normal z = 1.0
                     "000000000000f03f"                  # Dir x = 1.0
                     "0000000000000000"                  # Dir y
                     "0000000000000000"                  # Dir z
                     "31350000"                          # "15" ANSI + 2 bytes padding to 4
                     "ffffffff"                          # length = -1
                     "00000000"                          # raw = 0
                     "0000000000e07540"                  # Height = 350.0
                     "000000000000f03f"                  # WidthFactor = 1.0
                     "0000000000000000"                  # Oblique = 0.0
                     "000000000000f03f"                  # Tracking = 1.0
                     "0000000000000000"                  # unknown 1
                     "0000000000000000"                  # unknown 2
                     "0000000000000000"                  # unknown 3
                     "00000000"                          # unknown 4 (4 bytes)
                     "43535f476f7374323330342e7368780000" # "CS_Gost2304.shx\0" = 17 bytes
                     "0000000000")                        # padding to reach 156?

print(f"Constructed length: {len(full)}")

# Actual body from parse:
actual = bytes.fromhex("90194de2f795a4401868bcc075f89740000000000000000000000000000000000000000000000000000000000000f03f000000000000f03f0000000000000000000000000000000031350000ffffffff000000000000000000e07540000000000000f03f0000000000000000000000000000f03f000000000000000000000000000000000000000043535f476f7374323330342e7368780000000000")
print(f"Actual length: {len(actual)}")

# Parse it carefully
off = 0
def reset(v=0):
    global off
    off = v

def read_d():
    global off
    v = struct.unpack_from("<d", actual, off)[0]; off += 8; return v
def read_i():
    global off
    v = struct.unpack_from("<i", actual, off)[0]; off += 4; return v
def read_bytes(n):
    global off
    v = actual[off:off+n]; off += n; return v

print(f"\nPosition: ({read_d():.3f}, {read_d():.3f}, {read_d():.3f})")
print(f"Normal: ({read_d():.3f}, {read_d():.3f}, {read_d():.3f})")
print(f"Direction: ({read_d():.3f}, {read_d():.3f}, {read_d():.3f})")
print(f"Offset after geometry: {off}")

# Now text: ANSI null-term padded to 4
start = off
end = actual.find(b'\x00', off)
text = actual[start:end].decode('latin-1')
off = end + 1
while off % 4 != 0:
    off += 1
print(f'Text: "{text}" off after: {off}')

print(f"Length (i32): {read_i()}")
print(f"Raw (i32): {read_i()}")
print(f"Height: {read_d()}")
print(f"WidthFactor: {read_d()}")
print(f"Oblique: {read_d()}")
print(f"Tracking: {read_d()}")
# Following layout aligns with 5 flags in OpCode 38 format
# But OpCode 11 is simpler... let's read more doubles
print(f"Offset before next: {off}, remaining: {len(actual)-off}")
print(f"Remaining hex: {actual[off:].hex()}")

# Check: Remaining 32 bytes then 17 bytes font string + padding
# remaining = 48 bytes (actual 156-108=48)
# Layout possibility:
#    3 x int32 flags (backward, upsidedown, vertical)? 12 bytes
#    font name (null-term padded) 17 bytes
# Too long, too short to decode cleanly

# OpCode 38 flow:
#   3 geometry, text, length, raw, height, wfactor, oblique, tracking,
#   5 uint32 flags, 4 uint32 extra (bold, italic, charset, pitch),
#   TypeFace (padded unicode), FontName (padded unicode), BigFont (padded unicode)

# OpCode 11 (text2) may skip TypeFace since it's old format.
# Let me try:
#  5 flags
#  FontName (null-term ANSI, padded)
#  BigFont (null-term ANSI, padded)

# Re-try from offset 108
reset(108)
# 5 flags x 4 bytes = 20 bytes
print("\nTrying 5 uint32 flags:")
for i, name in enumerate(["backward", "upsidedown", "vertical", "underlined", "overlined"]):
    v = read_i()
    print(f"  {name} = {v}")

print(f"After flags, off={off}, remaining: {len(actual)-off}")
print(f"Remaining hex: {actual[off:].hex()}")

# Now FontName ANSI padded
start = off
end = actual.find(b'\x00', off)
if end >= 0:
    fn = actual[start:end].decode('latin-1')
    off = end + 1
    while off % 4 != 0:
        off += 1
    print(f'FontName: "{fn}" off={off}')
else:
    print("No null found")

if off < len(actual):
    print(f"BigFont section: {actual[off:].hex()}")
    start = off
    end = actual.find(b'\x00', off)
    if end >= 0:
        bf = actual[start:end].decode('latin-1')
        off = end + 1
        while off % 4 != 0:
            off += 1
        print(f'BigFont: "{bf}" off={off}')
print(f"Final off={off} / {len(actual)}")
