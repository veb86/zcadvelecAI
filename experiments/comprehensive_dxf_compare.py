#!/usr/bin/env python3
"""
Comprehensive comparison of original vs ZCAD-exported DXF to find
what causes AutoCAD to report '_DetailCallout' not defined error.
"""

def parse_dxf(filepath):
    with open(filepath, errors='ignore') as f:
        lines = f.readlines()
    pairs = []
    i = 0
    while i < len(lines) - 1:
        code = lines[i].strip()
        value = lines[i+1].strip()
        try:
            code_int = int(code)
        except:
            i += 1
            continue
        pairs.append((code_int, value, i+1))  # i+1 = line number
        i += 2
    return pairs

def find_section_pairs(pairs, section_name):
    """Return pairs within a section."""
    in_section = False
    result = []
    for i, (code, value, ln) in enumerate(pairs):
        if code == 2 and value == section_name and i > 0 and pairs[i-1][0] == 0 and pairs[i-1][1] == 'SECTION':
            in_section = True
            continue
        if in_section:
            if code == 0 and value == 'ENDSEC':
                break
            result.append((code, value, ln))
    return result

def extract_objects_by_type(pairs, obj_type):
    """Extract objects of a given type with all their properties."""
    objects = []
    in_obj = False
    cur = {}
    for code, value, ln in pairs:
        if code == 0:
            if in_obj and cur:
                objects.append(cur)
            if value == obj_type:
                in_obj = True
                cur = {'_type': obj_type, '_props': [], '_handle': ''}
            else:
                in_obj = False
                cur = {}
        elif in_obj:
            cur['_props'].append((code, value))
            if code == 5 and not cur['_handle']:
                cur['_handle'] = value
            if code == 2:
                cur['_name'] = value
    if in_obj and cur:
        objects.append(cur)
    return objects

def compare_files(orig_file, exported_file):
    print(f"\n{'='*60}")
    print(f"Comparing:")
    print(f"  Original: {orig_file}")
    print(f"  Exported: {exported_file}")
    print(f"{'='*60}")

    orig_pairs = parse_dxf(orig_file)
    exp_pairs = parse_dxf(exported_file)

    # 1. Compare BLOCK_RECORD entries
    print("\n--- BLOCK_RECORD comparison ---")
    for label, pairs in [("Original", orig_pairs), ("Exported", exp_pairs)]:
        tables = find_section_pairs(pairs, 'TABLES')
        block_recs = extract_objects_by_type(tables, 'BLOCK_RECORD')
        names = sorted([obj.get('_name', '?') for obj in block_recs])
        print(f"  {label}: {len(block_recs)} records: {names}")

    # 2. Compare BLOCK definitions
    print("\n--- BLOCK definitions comparison ---")
    orig_blocks_sec = find_section_pairs(orig_pairs, 'BLOCKS')
    exp_blocks_sec = find_section_pairs(exp_pairs, 'BLOCKS')
    orig_blocks = extract_objects_by_type(orig_blocks_sec, 'BLOCK')
    exp_blocks = extract_objects_by_type(exp_blocks_sec, 'BLOCK')
    orig_block_names = sorted([obj.get('_name', '?') for obj in orig_blocks])
    exp_block_names = sorted([obj.get('_name', '?') for obj in exp_blocks])
    print(f"  Original: {len(orig_blocks)} blocks: {orig_block_names}")
    print(f"  Exported: {len(exp_blocks)} blocks: {exp_block_names}")

    missing = set(orig_block_names) - set(exp_block_names)
    extra = set(exp_block_names) - set(orig_block_names)
    if missing:
        print(f"  ** MISSING from export: {sorted(missing)}")
    if extra:
        print(f"  ** EXTRA in export: {sorted(extra)}")

    # 3. Compare MLEADERSTYLE objects
    print("\n--- MLEADERSTYLE objects comparison ---")
    orig_obj = find_section_pairs(orig_pairs, 'OBJECTS')
    exp_obj = find_section_pairs(exp_pairs, 'OBJECTS')
    orig_mls = extract_objects_by_type(orig_obj, 'MLEADERSTYLE')
    exp_mls = extract_objects_by_type(exp_obj, 'MLEADERSTYLE')
    print(f"  Original: {len(orig_mls)} styles")
    print(f"  Exported: {len(exp_mls)} styles")

    # Get block_record maps for handle resolution
    for label, pairs, mls in [("Original", orig_pairs, orig_mls), ("Exported", exp_pairs, exp_mls)]:
        tables = find_section_pairs(pairs, 'TABLES')
        block_recs = extract_objects_by_type(tables, 'BLOCK_RECORD')
        handle_map = {obj['_handle'].upper(): obj.get('_name', '?') for obj in block_recs}

        # Get dict mapping
        obj_sec = find_section_pairs(pairs, 'OBJECTS')
        dict_map = {}
        # Find ACAD_MLEADERSTYLE dict handle
        for i, (c, v, _) in enumerate(obj_sec):
            if c == 3 and v == 'ACAD_MLEADERSTYLE':
                if i+1 < len(obj_sec) and obj_sec[i+1][0] == 350:
                    dh = obj_sec[i+1][1].upper()
                    # Find that dictionary
                    in_dict = False
                    last_name = ''
                    for c2, v2, _ in obj_sec:
                        if c2 == 5 and v2.upper() == dh:
                            in_dict = True
                        elif in_dict:
                            if c2 == 0:
                                break
                            if c2 == 3:
                                last_name = v2
                            elif c2 == 350 and last_name:
                                dict_map[v2.upper()] = last_name
                                last_name = ''
                    break

        print(f"\n  {label} MLEADERSTYLE details:")
        for ml in mls:
            style_name = dict_map.get(ml['_handle'].upper(), '(unknown)')
            print(f"    Style: {style_name} (handle={ml['_handle']})")
            for code, val in ml['_props']:
                if code in [340, 341, 342, 343]:
                    ref_name = handle_map.get(val.upper(), '(NOT FOUND)')
                    status = "OK" if ref_name != '(NOT FOUND)' else "** BROKEN **"
                    print(f"      Code {code}: {val} -> {ref_name} [{status}]")

    # 4. Check CLASSES section
    print("\n--- CLASSES section comparison ---")
    orig_classes = find_section_pairs(orig_pairs, 'CLASSES')
    exp_classes = find_section_pairs(exp_pairs, 'CLASSES')
    orig_class_names = [v for c, v, _ in orig_classes if c == 1]
    exp_class_names = [v for c, v, _ in exp_classes if c == 1]
    print(f"  Original classes: {sorted(orig_class_names)}")
    print(f"  Exported classes: {sorted(exp_class_names)}")
    missing_classes = set(orig_class_names) - set(exp_class_names)
    if missing_classes:
        print(f"  ** MISSING classes: {sorted(missing_classes)}")

    # 5. Check for entity types in blocks
    print("\n--- Block entity types comparison ---")
    for block_name in orig_block_names:
        if block_name.startswith('_'):
            # Find entities in this block in both files
            for label, blocks_sec in [("Original", orig_blocks_sec), ("Exported", exp_blocks_sec)]:
                entities = []
                in_block = False
                for c, v, ln in blocks_sec:
                    if c == 2 and v == block_name and in_block:
                        # Found our block
                        pass
                    if c == 0 and v == 'BLOCK':
                        in_block = False
                    elif c == 2 and v == block_name:
                        in_block = True
                    elif c == 0 and in_block:
                        if v == 'ENDBLK':
                            in_block = False
                        else:
                            entities.append(v)
                if entities:
                    print(f"  {label} Block '{block_name}': entities={entities}")

    # 6. Missing 330 owner handle check
    print("\n--- Block header 330 (owner) check ---")
    for label, blocks_sec in [("Original", orig_blocks_sec), ("Exported", exp_blocks_sec)]:
        for block_name in ['_DetailCallout', '_Dot', '_BoxBlank', '_TagSlot']:
            in_block_header = False
            has_330 = False
            for c, v, ln in blocks_sec:
                if c == 0 and v == 'BLOCK':
                    in_block_header = True
                    has_330 = False
                elif in_block_header:
                    if c == 2 and v == block_name:
                        pass
                    elif c == 330:
                        has_330 = True
                    elif c == 0:
                        break
            if in_block_header:
                status = "has 330" if has_330 else "** MISSING 330 **"
                # We need better logic
            # Simplified check
            found = False
            for i, (c, v, ln) in enumerate(blocks_sec):
                if c == 2 and v == block_name:
                    # Check if there's a 330 in the preceding lines (after BLOCK)
                    for j in range(max(0, i-10), i):
                        if blocks_sec[j][0] == 330:
                            has_330 = True
                            found = True
                            break
                    else:
                        has_330 = False
                        found = True
                    break
            if found:
                status = "has 330" if has_330 else "** MISSING 330 **"
                print(f"  {label} '{block_name}': {status}")

compare_files(
    'cad_source/test/+mleaderstyle2008.dxf',
    'cad_source/test/+mleaderstyle2008_3.dxf'
)
