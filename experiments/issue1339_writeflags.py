#!/usr/bin/env python3
"""Issue #1339 (write side) — verify that re-encoding the BreakOption flag from
the manual-position/manual-height booleans reproduces the bits AutoCAD wrote.

This mirrors the Pascal logic in
  uzeacadtable_dxf_write.WriteRoundTripRecordToDXF:
    BreakOptionFlags := 3;                       { EnableBreaks(1)+RepeatTopLabels(2) }
    if BreakManualPosition then flags := flags or 8;
    if BreakManualHeight   then flags := flags or 16;

Before the fix ZCAD hard-coded the flag to 3, dropping the manual bits on
re-save. The read side (DetectBreak* / SetBreakOptionFlags) decodes bits 8/16
from the original file; this check confirms the write side puts them back.

We assert the re-encoded flag equals the bits we manage (3|8|16 = 27 mask) of
the original flag. Bits outside that mask (none observed in the test files)
are intentionally not round-tripped by the current writer and are reported.
"""
import sys, os

# Reuse the read-side scanner.
sys.path.insert(0, os.path.dirname(__file__))
from issue1339_breakflags import scan_break, EXPECTED  # noqa: E402

BASE_FLAGS = 3            # EnableBreaks(1) + RepeatTopLabels(2)
ALLOW_MANUAL_POSITIONS = 8
ALLOW_MANUAL_HEIGHTS = 16
MANAGED_MASK = BASE_FLAGS | ALLOW_MANUAL_POSITIONS | ALLOW_MANUAL_HEIGHTS  # 27


def encode_flags(manual_pos, manual_h):
    """Pascal-equivalent flag synthesis used on save."""
    flags = BASE_FLAGS
    if manual_pos:
        flags |= ALLOW_MANUAL_POSITIONS
    if manual_h:
        flags |= ALLOW_MANUAL_HEIGHTS
    return flags


def main():
    base = os.path.join(os.path.dirname(__file__), '..', 'cad_source', 'test')
    ok = True
    for n in (1, 2, 3, 4):
        path = os.path.join(base, f'acadtablerazdel2007_{n}.dxf')
        orig_flags, _spacing, _bh = scan_break(path)

        # Read side: decode the manual booleans (as SetBreakOptionFlags does).
        manual_pos = bool(orig_flags & ALLOW_MANUAL_POSITIONS)
        manual_h = bool(orig_flags & ALLOW_MANUAL_HEIGHTS)

        # Write side: re-encode (as WriteRoundTripRecordToDXF now does).
        new_flags = encode_flags(manual_pos, manual_h)

        managed_orig = orig_flags & MANAGED_MASK
        round_trips = (new_flags == managed_orig)
        extra_bits = orig_flags & ~MANAGED_MASK

        exp = EXPECTED[n]
        matches_expected = (manual_pos == exp['manual_pos'] and
                            manual_h == exp['manual_h'])
        good = round_trips and matches_expected
        ok = ok and good
        note = 'OK' if good else 'MISMATCH'
        if extra_bits:
            note += f' (un-roundtripped bits={extra_bits:#x})'
        print(f"file {n}: orig={orig_flags:05b} -> manualPos={manual_pos} "
              f"manualHeight={manual_h} -> reencoded={new_flags:05b} {note}")

    # The old hard-coded writer always emitted 3 — show it would have failed.
    print("--- regression guard: old writer hard-coded flag=3 ---")
    old_ok = True
    for n in (2, 3, 4):  # files 2..4 carry manual bits
        path = os.path.join(base, f'acadtablerazdel2007_{n}.dxf')
        orig_flags, _, _ = scan_break(path)
        managed_orig = orig_flags & MANAGED_MASK
        if 3 == managed_orig:
            old_ok = False  # old writer would have been correct (unexpected)
    if old_ok:
        print("confirmed: old writer (flag=3) would lose manual bits for files 2-4")
    else:
        print("WARNING: old writer assumption invalid")
        ok = False

    print("ALL OK" if ok else "FAILURES")
    sys.exit(0 if ok else 1)


if __name__ == '__main__':
    main()
