#!/usr/bin/env python3
"""
Reproduction / verification for issue #1300.

A single ACAD_TABLE that AutoCAD split into 3 display segments is loaded as
3 separate ACAD_TABLE entities instead of one.

Root cause: GDBObjAcadTable.LoadFromDXF (uzeacadtable_model.pas) skipped every
group code BEFORE the first `100 AcDbEntity` subclass marker. The entity handle
(group 5) appears in exactly that region, so PExtAttrib^.dwgHandle was never
populated and stayed 0. The continuation-skip logic in addentitiesfromdxf
(uzeffdxf.pas) requires a non-zero handle to match against
TableContinuationHandles, so it never fired and all 3 parts were kept.

This script simulates both the BUGGY first-loop (skip-until-100) and the FIXED
first-loop (parse shared codes, incl. group 5) over the real ACAD_TABLE entities
in tablerazdel.dxf, then applies the continuation-skip decision.

Expected:
  - continuation handles = {209, 259}
  - BUGGY  -> 3 tables kept (handles never captured -> never skipped)
  - FIXED  -> 1 table kept (209 and 259 recognised as continuations, dropped)
"""
import os
import sys

DXF_PATH = os.path.join(
    os.path.dirname(os.path.dirname(os.path.abspath(__file__))),
    'cad_source', 'test', 'tablerazdel.dxf')

# Group codes handled by GDBObjEntity.LoadFromDXFObjShared (uzeentity.pas).
# Only group 5 matters for the handle, but we list the set for fidelity.
SHARED_CODES = {5, 6, 8, 48, 62, 370, 1001}


def read_pairs(path):
    with open(path, 'r', encoding='latin-1') as f:
        lines = f.read().splitlines()
    return lines


def normalize_handle(s):
    s = s.strip().upper()
    return s.lstrip('0') or '0'


def extract_section(lines, name):
    out, i, in_section = [], 0, False
    while i < len(lines):
        L = lines[i].strip()
        if not in_section:
            if L == '0' and i + 3 < len(lines) and lines[i+1].strip() == 'SECTION' \
               and lines[i+2].strip() == '2' and lines[i+3].strip() == name:
                in_section = True
                i += 4
                continue
        else:
            if L == '0' and i + 1 < len(lines) and lines[i+1].strip() == 'ENDSEC':
                return out
            out.append(lines[i])
        i += 1
    return out


def scan_continuations(obj_lines):
    handles, in_xrecord, in_roundtrip, i = set(), False, False, 0
    while i < len(obj_lines) - 1:
        try:
            code = int(obj_lines[i].strip())
        except ValueError:
            i += 2
            continue
        value = obj_lines[i+1].strip()
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
    return handles


def iter_acad_tables(ent_lines):
    """Yield the (code, value) body of each ACAD_TABLE entity (excluding the
    leading `0 / ACAD_TABLE` pair), up to the next entity (code 0)."""
    i = 0
    while i < len(ent_lines) - 1:
        code = ent_lines[i].strip()
        value = ent_lines[i+1].strip()
        if code == '0' and value == 'ACAD_TABLE':
            body, j = [], i + 2
            while j < len(ent_lines) - 1:
                if ent_lines[j].strip() == '0':
                    break
                body.append((ent_lines[j].strip(), ent_lines[j+1]))
                j += 2
            yield body
            i = j
        else:
            i += 2


def capture_handle(body, parse_shared_before_entity):
    """Simulate GDBObjAcadTable.LoadFromDXF up to the AcDbEntity marker.

    parse_shared_before_entity=False reproduces the buggy skip-until-100 loop.
    parse_shared_before_entity=True reproduces the fix (call LoadFromDXFObjShared)."""
    dwg_handle = 0
    for code_s, value in body:
        try:
            code = int(code_s)
        except ValueError:
            continue
        if code == 100:  # first subclass marker -> first loop ends
            break
        if parse_shared_before_entity and code in SHARED_CODES:
            if code == 5 and dwg_handle == 0:
                dwg_handle = int(value.strip(), 16)
        # else: skipped
    return dwg_handle


def run(parse_shared, continuations, tables):
    kept = []
    for idx, body in enumerate(tables):
        h = capture_handle(body, parse_shared)
        hhex = normalize_handle(format(h, 'X')) if h else '0'
        is_cont = (h != 0) and (hhex in continuations)
        if is_cont:
            decision = 'SKIP (continuation)'
        else:
            decision = 'KEEP'
            kept.append(hhex)
        print(f'  table[{idx}] handle=0x{hhex:<4} dwgHandle={"set" if h else "ZERO"}  -> {decision}')
    return kept


def main():
    lines = read_pairs(DXF_PATH)
    obj = extract_section(lines, 'OBJECTS')
    ent = extract_section(lines, 'ENTITIES')
    continuations = scan_continuations(obj)
    tables = list(iter_acad_tables(ent))

    print(f'ACAD_TABLE entities found: {len(tables)}')
    print(f'Continuation handles: {sorted(continuations)}')
    assert continuations == {'209', '259'}, continuations
    assert len(tables) == 3, len(tables)

    print('\n--- BUGGY first-loop (skip until 100, handle lost) ---')
    kept_buggy = run(False, continuations, tables)
    print(f'  => {len(kept_buggy)} table(s) kept: {kept_buggy}')

    print('\n--- FIXED first-loop (parse shared codes incl. group 5) ---')
    kept_fixed = run(True, continuations, tables)
    print(f'  => {len(kept_fixed)} table(s) kept: {kept_fixed}')

    ok = (len(kept_buggy) == 3) and (len(kept_fixed) == 1) and (kept_fixed == ['EB'])
    print('\nRESULT:', 'PASS' if ok else 'FAIL')
    return 0 if ok else 1


if __name__ == '__main__':
    sys.exit(main())
