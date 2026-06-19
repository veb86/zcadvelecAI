#!/usr/bin/env python3
"""Issue #1339 — verify decoding of the AcDbTableBreakData "BreakOption" flag
stored in the ACAD_ROUNDTRIP_2008_TABLE_ENTITY XRECORD.

Hypothesis (confirmed empirically below):
  flag bit 3 (value 8)  = AllowManualPositions  -> AcadTableBreakManualPosition
  flag bit 4 (value 16) = AllowManualHeights    -> AcadTableBreakManualHeight

The flag is the FIRST group-90 value inside the roundtrip block (after the
group 70). The first group-40 is the break spacing; the second group-40 is the
first break-range height.
"""
import sys, os

EXPECTED = {
    1: dict(manual_pos=False, manual_h=False),
    2: dict(manual_pos=True,  manual_h=False),
    3: dict(manual_pos=True,  manual_h=True),
    4: dict(manual_pos=False, manual_h=True),
}

def read_pairs(path):
    with open(path, 'r', errors='replace') as f:
        lines = [l.rstrip('\n').rstrip('\r') for l in f]
    i = 0
    while i + 1 < len(lines):
        yield lines[i].strip(), lines[i+1]
        i += 2

def scan_break(path):
    """Return (flags, spacing, breakheight) parsed from the roundtrip xrecord."""
    pairs = list(read_pairs(path))
    in_block = False
    flags = None
    spacing = None
    breakheight = None
    found40 = 0
    found90 = 0
    for code, value in pairs:
        if code == '102' and value.strip() == 'ACAD_ROUNDTRIP_2008_TABLE_ENTITY':
            in_block = True
            continue
        if in_block:
            if code in ('0', '361'):
                break
            if code == '90':
                found90 += 1
                if found90 == 1:
                    flags = int(value.strip())
            elif code == '40':
                v = float(value.strip())
                if found40 == 0:
                    spacing = v
                elif found40 == 1:
                    breakheight = v
                found40 += 1
    return flags, spacing, breakheight

def main():
    base = os.path.join(os.path.dirname(__file__), '..', 'cad_source', 'test')
    ok = True
    for n in (1, 2, 3, 4):
        path = os.path.join(base, f'acadtablerazdel2007_{n}.dxf')
        flags, spacing, bh = scan_break(path)
        manual_pos = bool(flags & 8)
        manual_h = bool(flags & 16)
        exp = EXPECTED[n]
        good = (manual_pos == exp['manual_pos'] and manual_h == exp['manual_h'])
        ok = ok and good
        print(f"file {n}: flags={flags} (bin {flags:05b}) spacing={spacing} bh={bh} "
              f"-> manualPos={manual_pos} manualHeight={manual_h} "
              f"{'OK' if good else 'MISMATCH exp='+str(exp)}")
    print("ALL OK" if ok else "FAILURES")
    sys.exit(0 if ok else 1)

if __name__ == '__main__':
    main()
