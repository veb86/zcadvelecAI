#!/usr/bin/env python3
"""
Verify the algorithm implemented in ScanTableContinuationHandles (Pascal).

Reads tablerazdel.dxf, extracts the OBJECTS section, and finds XRECORDs with
marker ACAD_ROUNDTRIP_2008_TABLE_ENTITY. Collects all handles following the
marker via group 330, stopping at group 361 (end of continuation block).
"""
import sys

DXF_PATH = '/tmp/gh-issue-solver-1776779819713/cad_source/test/tablerazdel.dxf'


def extract_section(path, name):
    with open(path, 'r', encoding='latin-1') as f:
        lines = f.read().splitlines()
    i = 0
    out = []
    in_section = False
    while i < len(lines):
        L = lines[i].strip()
        if not in_section:
            if L == '0' and i + 3 < len(lines) and lines[i+1].strip() == 'SECTION' \
               and lines[i+2].strip() == '2' and lines[i+3].strip() == name:
                in_section = True
                out.extend(lines[i:i+4])
                i += 4
                continue
        else:
            out.append(lines[i])
            if L == '0' and i + 1 < len(lines) and lines[i+1].strip() == 'ENDSEC':
                out.append(lines[i+1])
                return '\n'.join(out)
        i += 1
    return '\n'.join(out)


def normalize_handle(s):
    s = s.strip().upper()
    return s.lstrip('0') or '0'


def scan_continuations(raw_objects):
    """Replicates ScanTableContinuationHandles logic."""
    lines = raw_objects.splitlines()
    handles = set()
    in_xrecord = False
    in_roundtrip = False
    i = 0
    while i < len(lines) - 1:
        try:
            code = int(lines[i].strip())
        except ValueError:
            i += 2
            continue
        value = lines[i+1].strip()

        if code == 0:
            in_xrecord = (value.upper() == 'XRECORD')
            in_roundtrip = False
        elif in_xrecord:
            if code == 102 and value.upper() == 'ACAD_ROUNDTRIP_2008_TABLE_ENTITY':
                in_roundtrip = True
            elif in_roundtrip:
                if code == 330:
                    handles.add(normalize_handle(value))
                elif code == 361:
                    in_roundtrip = False

        i += 2
    return sorted(handles)


def main():
    raw = extract_section(DXF_PATH, 'OBJECTS')
    print(f'OBJECTS section size: {len(raw)} chars, {raw.count(chr(10))} lines')
    handles = scan_continuations(raw)
    print(f'Found {len(handles)} continuation handle(s): {handles}')
    # Expected from prior analysis: continuation handles 209 and 259
    expected = {'209', '259'}
    got = set(handles)
    if got == expected:
        print('PASS: continuation handles match expected {209, 259}')
        return 0
    else:
        print(f'FAIL: expected {expected}, got {got}')
        return 1


if __name__ == '__main__':
    sys.exit(main())
