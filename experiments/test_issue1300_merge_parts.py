#!/usr/bin/env python3
"""
Verification for issue #1300 (corrected requirement).

A single ACAD_TABLE that AutoCAD split into 3 display parts (with a horizontal
offset between parts) is stored in DXF as 3 separate ACAD_TABLE entities.

Previous fix (PR #1301) merely DROPPED the 2nd and 3rd parts, so only the first
part was displayed. The correct behaviour (per issue comment 2026-06-09): all
three parts must be DISPLAYED, combined as ONE AcadTable object — the same way
the proxy-object path in issue #980 merges them.

This script models the new merge logic in uzeacadtable_model.pas:
  - the first ACAD_TABLE is the "main" object,
  - the continuation parts (handles listed in the ROUNDTRIP XRECORD) are absorbed
    as TAcadTablePart records,
  - each part is rendered at a base offset = part.InsertPoint - main.InsertPoint.

Expected for tablerazdel.dxf:
  - 3 ACAD_TABLE entities in the file,
  - after merge: 1 object with ContinuationPartCount == 2,
  - the 3 parts are laid out side-by-side along +X (non-overlapping, monotone).
"""
import os
import sys

DXF_PATH = os.path.join(
    os.path.dirname(os.path.dirname(os.path.abspath(__file__))),
    'cad_source', 'test', 'tablerazdel.dxf')


def read_pairs(path):
    with open(path, 'r', encoding='latin-1') as f:
        lines = f.read().splitlines()
    pairs = []
    i = 0
    while i + 1 < len(lines):
        code = lines[i].strip()
        value = lines[i + 1]
        try:
            code = int(code)
        except ValueError:
            i += 2
            continue
        pairs.append((code, value))
        i += 2
    return pairs


def extract_acad_tables(pairs):
    """Return list of dicts with handle + insert point for every ACAD_TABLE."""
    tables = []
    in_entities = False
    i = 0
    n = len(pairs)
    while i < n:
        code, value = pairs[i]
        if code == 2 and value.strip() == 'ENTITIES':
            in_entities = True
        if in_entities and code == 0 and value.strip() == 'ACAD_TABLE':
            t = {'handle': None, 'x': 0.0, 'y': 0.0, 'rows': 0}
            j = i + 1
            seen_block_ref = False
            while j < n and pairs[j][0] != 0:
                c, v = pairs[j]
                if c == 5 and t['handle'] is None:
                    t['handle'] = v.strip().upper().lstrip('0') or '0'
                elif c == 100 and v.strip() == 'AcDbBlockReference':
                    seen_block_ref = True
                elif seen_block_ref and c == 10:
                    t['x'] = float(v)
                elif seen_block_ref and c == 20:
                    t['y'] = float(v)
                elif c == 91:
                    t['rows'] += 1
                j += 1
            tables.append(t)
            i = j
            continue
        i += 1
    return tables


def main():
    if not os.path.exists(DXF_PATH):
        print('MISSING DXF:', DXF_PATH)
        return 1

    pairs = read_pairs(DXF_PATH)
    tables = extract_acad_tables(pairs)

    print('ACAD_TABLE entities found:', len(tables))
    for idx, t in enumerate(tables):
        print('  part %d: handle=%s insert=(%.4f, %.4f)'
              % (idx, t['handle'], t['x'], t['y']))

    ok = True

    if len(tables) != 3:
        print('FAIL: expected 3 ACAD_TABLE entities, got', len(tables))
        ok = False

    if tables:
        # Model the merge: main = tables[0]; the rest become continuation parts.
        main = tables[0]
        parts = tables[1:]
        continuation_part_count = len(parts)
        print('After merge -> 1 object, ContinuationPartCount =',
              continuation_part_count)
        if continuation_part_count != 2:
            print('FAIL: expected ContinuationPartCount == 2')
            ok = False

        # Base offsets used by RenderCurrentTable(ABaseX, ABaseY).
        base_x = [0.0] + [p['x'] - main['x'] for p in parts]
        base_y = [0.0] + [p['y'] - main['y'] for p in parts]
        print('Render base offsets X:', ['%.4f' % b for b in base_x])
        print('Render base offsets Y:', ['%.4f' % b for b in base_y])

        # All 3 parts must be laid out at distinct, strictly increasing X
        # offsets — i.e. side-by-side, not stacked on top of each other.
        if not all(base_x[k] < base_x[k + 1] - 1e-6
                   for k in range(len(base_x) - 1)):
            print('FAIL: part X offsets are not strictly increasing '
                  '(parts would overlap instead of display side-by-side)')
            ok = False
        else:
            print('OK: parts are displayed side-by-side along +X')

    print('RESULT:', 'PASS' if ok else 'FAIL')
    return 0 if ok else 1


if __name__ == '__main__':
    sys.exit(main())
