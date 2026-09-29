#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
dxf_parser.py - Robust low-level DXF streaming tokenizer and entity parser.

Specialized for AutoCAD 2007+ (AC1021) ACAD_TABLE structures, entity sections,
and ObjectARX metadata.
"""

import os
import sys
from typing import List, Tuple, Dict, Any, Generator, Optional


class DXFTag:
    __slots__ = ('code', 'value')

    def __init__(self, code: int, value: Any):
        self.code = code
        self.value = value

    def __repr__(self) -> str:
        return f"DXFTag({self.code}, {self.value!r})"


def read_tags(file_path_or_content: str, is_content: bool = False) -> Generator[DXFTag, None, None]:
    """
    Yields (code, value) tags from a DXF file path or raw string content.
    Automatically handles numeric conversion according to DXF group code ranges.
    """
    if is_content:
        lines = file_path_or_content.splitlines()
    else:
        # Try UTF-8 with fallback to CP1251 / latin-1
        for encoding in ('utf-8', 'cp1251', 'latin-1'):
            try:
                with open(file_path_or_content, 'r', encoding=encoding, errors='replace') as f:
                    lines = f.read().splitlines()
                break
            except Exception:
                continue
        else:
            with open(file_path_or_content, 'r', errors='ignore') as f:
                lines = f.read().splitlines()

    i = 0
    total = len(lines)
    while i < total:
        code_line = lines[i].strip()
        if not code_line:
            i += 1
            continue

        try:
            code = int(code_line)
        except ValueError:
            i += 1
            continue

        if i + 1 >= total:
            break

        val_line = lines[i + 1].strip()
        i += 2

        # Convert value based on DXF Group Code specification
        val: Any = val_line
        if (10 <= code < 60) or (140 <= code < 150):
            try:
                val = float(val_line)
            except ValueError:
                val = val_line
        elif (60 <= code < 100) or (170 <= code < 180) or (270 <= code < 290):
            try:
                val = int(val_line)
            except ValueError:
                val = val_line

        yield DXFTag(code, val)


def extract_entities_by_type(tags: List[DXFTag], target_type: str = "ACAD_TABLE") -> List[List[DXFTag]]:
    """
    Extracts all entity tag blocks of a given type (e.g. 'ACAD_TABLE', 'LINE', 'TEXT')
    from the ENTITIES section.
    """
    target_type = target_type.upper()
    entities: List[List[DXFTag]] = []
    current_entity: Optional[List[DXFTag]] = None
    in_entities_section = False

    i = 0
    while i < len(tags):
        tag = tags[i]

        if tag.code == 0 and tag.value == "SECTION":
            if i + 1 < len(tags) and tags[i + 1].code == 2:
                sec_name = str(tags[i + 1].value).upper()
                in_entities_section = (sec_name == "ENTITIES")
                i += 2
                continue

        if tag.code == 0 and tag.value == "ENDSEC":
            in_entities_section = False
            if current_entity is not None:
                entities.append(current_entity)
                current_entity = None
            i += 1
            continue

        if in_entities_section and tag.code == 0:
            if current_entity is not None:
                entities.append(current_entity)
                current_entity = None

            if str(tag.value).upper() == target_type:
                current_entity = [tag]

        elif current_entity is not None:
            current_entity.append(tag)

        i += 1

    if current_entity is not None:
        entities.append(current_entity)

    return entities


def extract_objects_by_type(tags: List[DXFTag], target_type: str) -> List[List[DXFTag]]:
    """
    Extracts all object tag blocks of a given type (e.g. 'IMAGEDEF', 'TABLESTYLE', 'DICTIONARY')
    from the OBJECTS section.
    """
    target_type = target_type.upper()
    objects: List[List[DXFTag]] = []
    current_obj: Optional[List[DXFTag]] = None
    in_objects_section = False

    i = 0
    while i < len(tags):
        tag = tags[i]

        if tag.code == 0 and tag.value == "SECTION":
            if i + 1 < len(tags) and tags[i + 1].code == 2:
                sec_name = str(tags[i + 1].value).upper()
                in_objects_section = (sec_name == "OBJECTS")
                i += 2
                continue

        if tag.code == 0 and tag.value == "ENDSEC":
            in_objects_section = False
            if current_obj is not None:
                objects.append(current_obj)
                current_obj = None
            i += 1
            continue

        if in_objects_section and tag.code == 0:
            if current_obj is not None:
                objects.append(current_obj)
                current_obj = None

            if str(tag.value).upper() == target_type:
                current_obj = [tag]

        elif current_obj is not None:
            current_obj.append(tag)

        i += 1

    if current_obj is not None:
        objects.append(current_obj)

    return objects


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python dxf_parser.py <dxf_file>")
        sys.exit(1)

    filename = sys.argv[1]
    all_tags = list(read_tags(filename))
    print(f"Total tags parsed from {filename}: {len(all_tags)}")
    tables = extract_entities_by_type(all_tags, "ACAD_TABLE")
    print(f"ACAD_TABLE entities found: {len(tables)}")
    images = extract_entities_by_type(all_tags, "IMAGE")
    print(f"IMAGE entities found: {len(images)}")
    imagedefs = extract_objects_by_type(all_tags, "IMAGEDEF")
    print(f"IMAGEDEF objects found: {len(imagedefs)}")
