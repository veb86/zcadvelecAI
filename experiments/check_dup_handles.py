#!/usr/bin/env python3
"""Detect duplicate entity handles (DXF group 5) in a DXF file.

Used to investigate issue #1344: after ZCAD resaved
acadtablerazdel2007_4.dxf, AutoCAD refused to open it with
"Неверная метка EB: уже используется" (handle EB already in use).

A valid DXF must have every object handle (group 5) unique. This script
reports any group-5 value that appears more than once, with the line of
each occurrence and the entity type that owns it.

Usage:
    python3 check_dup_handles.py <file.dxf> [<file.dxf> ...]

Exit code is 1 if any file has a duplicate handle, else 0.
"""
import sys


def scan(path):
    """Return dict handle -> list of (line_no, owner_type)."""
    with open(path, 'r', encoding='utf-8', errors='replace') as f:
        lines = [ln.rstrip('\n').rstrip('\r') for ln in f]

    # An object's handle is the FIRST group 5 emitted after its "0 / <TYPE>"
    # marker. Group code 5 (and code 0) also appear deeper inside object data
    # (ACAD_TABLE raw data, MTEXT cache, table style/cell records) where they
    # are NOT structural markers. Those embedded values produce false
    # positives, so we only treat a group 5 as a handle when:
    #   * it is the first 5 after a 0/<TYPE> marker, and
    #   * the <TYPE> looks like a real DXF entity/object name (starts with a
    #     letter) rather than embedded numeric data.
    handles = {}
    last_entity = '?'
    handle_taken = True
    i = 0
    n = len(lines)
    while i < n - 1:
        code = lines[i].strip()
        val = lines[i + 1].strip()
        if code == '0':
            last_entity = val
            handle_taken = False
        elif (code == '5' and not handle_taken
              and last_entity[:1].isalpha()):
            handles.setdefault(val, []).append((i + 2, last_entity))
            handle_taken = True
        i += 1
    return handles


def main(argv):
    if len(argv) < 2:
        print(__doc__)
        return 2
    overall_ok = True
    for path in argv[1:]:
        handles = scan(path)
        dups = {h: locs for h, locs in handles.items() if len(locs) > 1}
        total = sum(len(v) for v in handles.values())
        print(f'{path}: {len(handles)} unique handles, {total} group-5 entries')
        if dups:
            overall_ok = False
            for h, locs in sorted(dups.items()):
                where = ', '.join(f'line {ln} ({owner})' for ln, owner in locs)
                print(f'  DUPLICATE handle {h}: {where}')
        else:
            print('  OK: no duplicate handles')
    return 0 if overall_ok else 1


if __name__ == '__main__':
    sys.exit(main(sys.argv))
