#!/usr/bin/env python3
"""
Analyze DXF roundtrip issues with MLEADERSTYLE.
Simulates what ZCAD does when loading and re-saving.
"""
import re
import sys

def parse_dxf_pairs(text):
    """Parse DXF text into list of (code, value) pairs."""
    lines = text.split('\n')
    pairs = []
    i = 0
    while i < len(lines) - 1:
        code_str = lines[i].strip()
        value = lines[i + 1].rstrip('\r')
        try:
            code = int(code_str)
        except ValueError:
            i += 1
            continue
        pairs.append((code, value))
        i += 2
    return pairs

def find_section(pairs, section_name):
    """Find section boundaries (start, end indices)."""
    start = None
    for i, (code, value) in enumerate(pairs):
        if code == 2 and value.strip() == section_name and i > 0:
            if pairs[i-1] == (0, 'SECTION'):
                start = i - 1
        if start is not None and code == 0 and value.strip() == 'ENDSEC':
            return start, i
    return None, None

def extract_block_records(pairs, tables_start, tables_end):
    """Extract BLOCK_RECORD entries from TABLES section."""
    records = {}
    i = tables_start
    while i <= tables_end:
        code, value = pairs[i]
        if code == 0 and value.strip() == 'BLOCK_RECORD':
            handle = ''
            name = ''
            j = i + 1
            while j <= tables_end:
                c, v = pairs[j]
                if c == 0:
                    break
                if c == 5 and not handle:
                    handle = v.strip()
                if c == 2:
                    name = v.strip()
                j += 1
            if handle and name:
                records[handle.upper()] = name
            i = j
            continue
        i += 1
    return records

def extract_block_defs(pairs, blocks_start, blocks_end):
    """Extract BLOCK definitions from BLOCKS section."""
    blocks = {}
    i = blocks_start
    while i <= blocks_end:
        code, value = pairs[i]
        if code == 0 and value.strip() == 'BLOCK':
            name = ''
            handle = ''
            j = i + 1
            while j <= blocks_end:
                c, v = pairs[j]
                if c == 0:
                    break
                if c == 2 and not name:
                    name = v.strip()
                if c == 5 and not handle:
                    handle = v.strip()
                j += 1
            if name:
                blocks[name] = handle
            i = j
            continue
        i += 1
    return blocks

def extract_mleaderstyles(pairs, objects_start, objects_end):
    """Extract MLEADERSTYLE objects from OBJECTS section."""
    styles = []
    i = objects_start
    while i <= objects_end:
        code, value = pairs[i]
        if code == 0 and value.strip() == 'MLEADERSTYLE':
            style = {'handle': '', 'codes': {}}
            j = i + 1
            while j <= objects_end:
                c, v = pairs[j]
                if c == 0:
                    break
                if c == 5 and not style['handle']:
                    style['handle'] = v.strip()
                style['codes'][c] = v.strip()
                j += 1
            styles.append(style)
            i = j
            continue
        i += 1
    return styles

def extract_mleaderstyle_dict(pairs, objects_start, objects_end):
    """Extract ACAD_MLEADERSTYLE dictionary mapping."""
    # Find ACAD_MLEADERSTYLE entry in root dict
    dict_handle = None
    for i in range(objects_start, objects_end):
        code, value = pairs[i]
        if code == 3 and value.strip() == 'ACAD_MLEADERSTYLE':
            if i + 1 <= objects_end:
                nc, nv = pairs[i + 1]
                if nc == 350:
                    dict_handle = nv.strip().upper()
                    break

    if not dict_handle:
        return {}, None

    # Find the dictionary with that handle
    mapping = {}
    i = objects_start
    while i <= objects_end:
        code, value = pairs[i]
        if code == 0 and value.strip() == 'DICTIONARY':
            j = i + 1
            found = False
            while j <= objects_end:
                c, v = pairs[j]
                if c == 0:
                    break
                if c == 5 and v.strip().upper() == dict_handle:
                    found = True
                j += 1
            if found:
                # Read entries
                last_name = ''
                k = i + 1
                while k <= objects_end:
                    c, v = pairs[k]
                    if c == 0:
                        break
                    if c == 3:
                        last_name = v.strip()
                    elif c == 350 and last_name:
                        mapping[v.strip().upper()] = last_name
                        last_name = ''
                    k += 1
                return mapping, dict_handle
        i += 1
    return mapping, dict_handle

def analyze_file(filepath):
    with open(filepath, 'r', errors='ignore') as f:
        text = f.read()

    pairs = parse_dxf_pairs(text)

    # Find sections
    tables_s, tables_e = find_section(pairs, 'TABLES')
    blocks_s, blocks_e = find_section(pairs, 'BLOCKS')
    objects_s, objects_e = find_section(pairs, 'OBJECTS')

    print(f"\n=== Analyzing: {filepath} ===")
    print(f"Sections: TABLES={tables_s}-{tables_e}, BLOCKS={blocks_s}-{blocks_e}, OBJECTS={objects_s}-{objects_e}")

    # Extract data
    block_records = extract_block_records(pairs, tables_s, tables_e)
    block_defs = extract_block_defs(pairs, blocks_s, blocks_e)
    mleader_dict, dict_handle = extract_mleaderstyle_dict(pairs, objects_s, objects_e)
    mleaderstyles = extract_mleaderstyles(pairs, objects_s, objects_e)

    print(f"\nBLOCK_RECORD entries ({len(block_records)}):")
    for h, n in sorted(block_records.items(), key=lambda x: x[1]):
        print(f"  Handle={h}: {n}")

    print(f"\nBLOCK definitions ({len(block_defs)}):")
    for n, h in sorted(block_defs.items()):
        print(f"  {n} (handle={h})")

    # Check for blocks starting with underscore (special annotation blocks)
    special_blocks = {n for n in block_defs if n.startswith('_')}
    special_records = {n for h, n in block_records.items() if n.startswith('_')}
    print(f"\nSpecial blocks (underscore-prefixed):")
    print(f"  In BLOCK_RECORD: {sorted(special_records)}")
    print(f"  In BLOCKS section: {sorted(special_blocks)}")

    # Check consistency
    record_names = set(block_records.values())
    def_names = set(block_defs.keys())
    in_record_not_def = record_names - def_names
    in_def_not_record = def_names - record_names
    if in_record_not_def:
        print(f"\n  WARNING: In BLOCK_RECORD but NOT in BLOCKS: {sorted(in_record_not_def)}")
    if in_def_not_record:
        print(f"\n  WARNING: In BLOCKS but NOT in BLOCK_RECORD: {sorted(in_def_not_record)}")

    print(f"\nACAD_MLEADERSTYLE dictionary (handle={dict_handle}):")
    for h, n in sorted(mleader_dict.items(), key=lambda x: x[1]):
        print(f"  {n} -> handle {h}")

    print(f"\nMLEADERSTYLE objects ({len(mleaderstyles)}):")
    for s in mleaderstyles:
        name = mleader_dict.get(s['handle'].upper(), '(unknown)')
        print(f"\n  Style: {name} (handle={s['handle']})")
        for code_num in [340, 341, 342, 343]:
            if code_num in s['codes']:
                handle_ref = s['codes'][code_num]
                ref_name = block_records.get(handle_ref.upper(), '(not found)')
                print(f"    Code {code_num}: handle={handle_ref} -> {ref_name}")
                # Check if referenced block exists
                if code_num in [341, 343]:
                    if ref_name not in block_defs:
                        print(f"      ** PROBLEM: Block '{ref_name}' referenced but NOT defined in BLOCKS section!")
                    else:
                        print(f"      OK: Block '{ref_name}' exists in BLOCKS section")

if __name__ == '__main__':
    files = [
        'cad_source/test/+mleaderstyle2008.dxf',
        'cad_source/test/+mleaderstyle2008_1.dxf',
        'cad_source/test/+mleaderstyle2008_3.dxf',
    ]
    for f in files:
        try:
            analyze_file(f)
        except Exception as e:
            print(f"Error analyzing {f}: {e}")
