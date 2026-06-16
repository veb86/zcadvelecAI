#!/usr/bin/env python3
"""Analyze a DXF file: list all object handles (group 5/105), all handle
references, detect collisions (duplicate handles) and dangling references.

Usage: dxf_analyze.py <file.dxf>
"""
import sys
from collections import defaultdict

# Group codes that DEFINE an object's handle
HANDLE_DEF_CODES = {5, 105}
# Group codes that REFERENCE a handle (pointers/owners/soft/hard)
REF_CODES = {320, 330, 340, 342, 343, 344, 345, 346, 347, 348, 349, 350,
             360, 361, 390, 391, 480, 481, 1005}


def parse(path):
    pairs = []
    with open(path, 'r', encoding='utf-8', errors='replace') as f:
        lines = f.read().split('\n')
    i = 0
    while i + 1 < len(lines):
        code = lines[i].strip()
        val = lines[i + 1]
        i += 2
        try:
            code = int(code)
        except ValueError:
            continue
        pairs.append((code, val.strip()))
    return pairs


def section_of(pairs):
    """Yield (code, val, section, entity_type) with current section/entity."""
    section = None
    entity = None
    for idx, (code, val) in enumerate(pairs):
        if code == 0 and val == 'SECTION':
            section = None
        elif code == 2 and section is None:
            section = val
        if code == 0:
            entity = val
        yield idx, code, val, section, entity


def main():
    path = sys.argv[1]
    pairs = parse(path)
    handle_defs = defaultdict(list)   # handle -> [(section, entity)]
    refs = []                          # (handle, code, section, entity)
    cur_section = None
    cur_entity = None
    cur_sec_name_pending = False
    for idx, (code, val) in enumerate(pairs):
        if code == 0 and val == 'SECTION':
            cur_section = '?'
            cur_sec_name_pending = True
            continue
        if cur_sec_name_pending and code == 2:
            cur_section = val
            cur_sec_name_pending = False
        if code == 0:
            cur_entity = val
        if code in HANDLE_DEF_CODES:
            h = val.upper().lstrip('0') or '0'
            handle_defs[h].append((cur_section, cur_entity))
        elif code in REF_CODES:
            v = val.strip()
            if v and v not in ('{', '}'):
                h = v.upper().lstrip('0') or '0'
                refs.append((h, code, cur_section, cur_entity))

    print(f"=== {path} ===")
    print(f"Total handle definitions: {sum(len(v) for v in handle_defs.values())}")
    print(f"Unique handles: {len(handle_defs)}")

    # collisions
    collisions = {h: v for h, v in handle_defs.items() if len(v) > 1}
    print(f"\n--- COLLISIONS (duplicate handle defs): {len(collisions)} ---")
    for h, v in sorted(collisions.items()):
        print(f"  handle {h}: {v}")

    # dangling refs
    defined = set(handle_defs.keys())
    dangling = defaultdict(list)
    for h, code, section, entity in refs:
        if h == '0':
            continue
        if h not in defined:
            dangling[h].append((code, section, entity))
    print(f"\n--- DANGLING REFERENCES (ref to undefined handle): {len(dangling)} ---")
    for h, v in sorted(dangling.items()):
        print(f"  ref->{h}: {v}")


if __name__ == '__main__':
    main()
