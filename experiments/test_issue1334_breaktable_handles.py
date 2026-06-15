#!/usr/bin/env python3
"""
Inspect issue #1334 DXF table continuation metadata.

The bug file contains one logical ACAD_TABLE split into 11 physical
ACAD_TABLE entities. This script compares the table handles found in
ENTITIES with continuation handles found in OBJECTS round-trip XRECORDs.
"""
from __future__ import annotations

from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
DXF_PATH = ROOT / "cad_source/test/bugbreaktable.dxf"
ROUNDTRIP_MARKER = "ACAD_ROUNDTRIP_2008_TABLE_ENTITY"


def normalize_handle(value: str) -> str:
    value = value.strip().upper()
    return value.lstrip("0") or "0"


def read_lines(path: Path) -> list[str]:
    return path.read_text(encoding="latin-1").splitlines()


def try_pair(lines: list[str], index: int) -> tuple[int, str] | None:
    if index < 0 or index >= len(lines) - 1:
        return None
    try:
        code = int(lines[index].strip())
    except ValueError:
        return None
    return code, lines[index + 1].strip()


def extract_section_lines(lines: list[str], section_name: str) -> list[str]:
    index = 0
    while index < len(lines) - 3:
        pair = try_pair(lines, index)
        name_pair = try_pair(lines, index + 2)
        if (
            pair == (0, "SECTION")
            and name_pair is not None
            and name_pair[0] == 2
            and name_pair[1].upper() == section_name
        ):
            start = index + 4
            end = start
            while end < len(lines) - 1:
                end_pair = try_pair(lines, end)
                if end_pair == (0, "ENDSEC"):
                    return lines[start:end]
                end += 1
            return lines[start:]
        index += 1
    return []


def iter_acad_table_entities(section: list[str]) -> list[list[tuple[int, str]]]:
    tables: list[list[tuple[int, str]]] = []
    index = 0
    while index < len(section) - 1:
        pair = try_pair(section, index)
        if pair == (0, "ACAD_TABLE"):
            body: list[tuple[int, str]] = []
            cursor = index + 2
            while cursor < len(section) - 1:
                next_pair = try_pair(section, cursor)
                if next_pair is None:
                    cursor += 1
                    continue
                if next_pair[0] == 0:
                    break
                body.append(next_pair)
                cursor += 2
            tables.append(body)
            index = cursor
            continue
        index += 1
    return tables


def scan_roundtrip_blocks(section: list[str]) -> list[list[str]]:
    blocks: list[list[str]] = []
    index = 0
    while index < len(section) - 1:
        pair = try_pair(section, index)
        if pair is not None and pair[0] == 102 and pair[1].upper() == ROUNDTRIP_MARKER:
            handles: list[str] = []
            cursor = index + 2
            while cursor < len(section) - 1:
                next_pair = try_pair(section, cursor)
                if next_pair is None:
                    cursor += 1
                    continue
                code, value = next_pair
                if code in (0, 361):
                    break
                if code == 330:
                    handles.append(normalize_handle(value))
                cursor += 2
            if handles:
                blocks.append(handles)
            index = cursor
            continue
        index += 1
    return blocks


def extract_table_info(table: list[tuple[int, str]]) -> dict[str, object]:
    handle = ""
    insert_x = 0.0
    insert_y = 0.0
    row_count = 0
    col_count = 0
    in_block_ref = False
    in_table = False

    for code, value in table:
        text = value.strip()
        if code == 5 and not handle:
            handle = normalize_handle(text)
        elif code == 100 and text == "AcDbBlockReference":
            in_block_ref = True
            in_table = False
        elif code == 100 and text == "AcDbTable":
            in_block_ref = False
            in_table = True
        elif in_block_ref and code == 10:
            insert_x = float(text)
        elif in_block_ref and code == 20:
            insert_y = float(text)
        elif in_table and code == 91 and row_count == 0:
            row_count = int(text)
        elif in_table and code == 92 and col_count == 0:
            col_count = int(text)

    return {
        "handle": handle,
        "x": insert_x,
        "y": insert_y,
        "rows": row_count,
        "cols": col_count,
    }


def main() -> int:
    lines = read_lines(DXF_PATH)
    entities = extract_section_lines(lines, "ENTITIES")
    objects = extract_section_lines(lines, "OBJECTS")

    tables = [extract_table_info(t) for t in iter_acad_table_entities(entities)]
    continuation_blocks = scan_roundtrip_blocks(objects)
    continuation_handles = {
        handle for block in continuation_blocks for handle in block
    }

    print(f"DXF: {DXF_PATH}")
    print(f"ACAD_TABLE entities: {len(tables)}")
    for index, table in enumerate(tables):
        marker = "continuation" if table["handle"] in continuation_handles else "main"
        print(
            "  table[{index:02d}] handle={handle} rows={rows} cols={cols} "
            "insert=({x:.4f}, {y:.4f}) {marker}".format(
                index=index, marker=marker, **table
            )
        )

    print(f"Round-trip continuation blocks: {len(continuation_blocks)}")
    for index, block in enumerate(continuation_blocks):
        print(f"  block[{index:02d}] handles={','.join(block)}")

    table_handles = {str(table["handle"]) for table in tables}
    missing = sorted(continuation_handles - table_handles)
    main_count = sum(1 for table in tables if table["handle"] not in continuation_handles)
    continuation_count = sum(
        1 for table in tables if table["handle"] in continuation_handles
    )

    print(f"Main tables by metadata: {main_count}")
    print(f"Continuation tables by metadata: {continuation_count}")
    print(f"Continuation handles missing from ENTITIES: {missing}")

    ok = (
        len(tables) == 11
        and main_count == 1
        and continuation_count == 10
        and not missing
    )
    print("RESULT:", "PASS" if ok else "FAIL")
    return 0 if ok else 1


if __name__ == "__main__":
    raise SystemExit(main())
