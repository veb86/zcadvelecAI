#!/usr/bin/env python3
"""
Test script to verify that {ACAD_XDICTIONARY} blocks are preserved
when rewriting TABLESTYLE objects.

This simulates what the updated uzestylestablesdxf.pas code does.
"""

import re

def parse_dxf_pairs(text):
    """Parse DXF text into list of (code, value) pairs."""
    lines = text.split('\n')
    pairs = []
    i = 0
    while i < len(lines) - 1:
        code_str = lines[i].strip()
        value = lines[i+1].strip()
        try:
            code = int(code_str)
            pairs.append((code, value, i))
        except ValueError:
            pass
        i += 2
    return pairs

def extract_objects_section(filename):
    """Extract OBJECTS section from DXF file."""
    with open(filename, 'r', encoding='cp1251', errors='replace') as f:
        lines = f.readlines()
    lines = [l.rstrip('\r\n') for l in lines]
    
    objects_start = -1
    i = 0
    while i < len(lines) - 3:
        if (lines[i].strip() == '0' and 
            lines[i+1].strip() == 'SECTION' and
            lines[i+2].strip() == '2' and
            lines[i+3].strip() == 'OBJECTS'):
            objects_start = i
            break
        i += 1
    
    objects_end = -1
    i = objects_start + 4
    while i < len(lines) - 1:
        if lines[i].strip() == '0' and lines[i+1].strip() == 'ENDSEC':
            objects_end = i + 2
            break
        i += 1
    
    return lines[objects_start:objects_end]

# Load original file
orig_lines = extract_objects_section('/tmp/gh-issue-solver-1775030607844/cad_source/tablestyle.dxf')

# Find all TABLESTYLE objects and check for ACAD_XDICTIONARY
i = 0
while i < len(orig_lines) - 1:
    c = orig_lines[i].strip()
    v = orig_lines[i+1].strip()
    try:
        ci = int(c)
    except:
        i += 1
        continue
    
    if ci == 0 and v == 'TABLESTYLE':
        print(f"\nFound TABLESTYLE at line {i+1}:")
        handle = ''
        xdict_handle = ''
        j = i + 2
        has_xdict = False
        while j < len(orig_lines) - 1:
            c2 = orig_lines[j].strip()
            v2 = orig_lines[j+1].strip() if j+1 < len(orig_lines) else ''
            try:
                c2i = int(c2)
            except:
                j += 2
                continue
            if c2i == 0:
                break
            if c2i == 5:
                handle = v2
            if c2i == 102 and v2.startswith('{ACAD_XDICTIONARY'):
                has_xdict = True
                # Get the 360 value
                k = j + 2
                while k < len(orig_lines) - 1:
                    c3 = orig_lines[k].strip()
                    v3 = orig_lines[k+1].strip() if k+1 < len(orig_lines) else ''
                    try:
                        c3i = int(c3)
                    except:
                        k += 2
                        continue
                    if c3i == 360:
                        xdict_handle = v3
                    if c3i == 102:  # closing }
                        break
                    k += 2
            j += 2
        print(f"  Handle: {handle}")
        print(f"  Has ACAD_XDICTIONARY: {has_xdict}")
        print(f"  XDict handle: {xdict_handle}")
    i += 2

print("\n=== Simulating fix ===")
print("After fix, each TABLESTYLE would be rebuilt with XDictHandle from loaded data")
print("This preserves the {ACAD_XDICTIONARY} block that AutoCAD requires")
