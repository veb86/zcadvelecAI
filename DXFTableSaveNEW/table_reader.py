#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
table_reader.py - AutoCAD ACAD_TABLE Entity Deep Inspector.

Decodes table entities, table break flags (DXF 90), row heights (DXF 141),
column widths (DXF 142), and extracts comprehensive per-cell metadata:
- Cell Style & Type (Text/Block, font, text height, colors)
- Row Style (Title, Header, Data) & Row Height
- Column Style & Column Width
- Cell Alignment (Middle Center, Top Right, Top Center, etc.)
- Cell Text contents (clean decoded strings without duplicates)
- Cell Merges (col_span, row_span, merged status)
- Overrides, rotation, virtual edges, autofit
"""

import sys
from typing import List, Dict, Any, Optional, Tuple
from dxf_parser import read_tags, extract_entities_by_type, DXFTag


# ==============================================================================
# AutoCAD AcDbTable::TableBreakOption Bitmasks (ObjectARX Specification)
# ==============================================================================
TABLE_BREAK_NONE = 0x01
TABLE_BREAK_ENABLE = 0x02
TABLE_BREAK_AUTO = 0x04
TABLE_BREAK_ALLOW_MANUAL_POSITIONING = 0x08
TABLE_BREAK_REPEAT_HEADER = 0x10

# Alignment mapping according to AutoCAD DXF Group Code 170 / 64
ALIGNMENT_MAP: Dict[int, str] = {
    1: "Top Left",
    2: "Top Center",
    3: "Top Right",
    4: "Middle Left",
    5: "Middle Center",
    6: "Middle Right",
    7: "Bottom Left",
    8: "Bottom Center",
    9: "Bottom Right",
}

# Row type / cell role names according to AutoCAD AcDb::RowType
ROW_STYLE_MAP: Dict[int, str] = {
    1: "Title",
    2: "Header",
    3: "Data",
}

CELL_TYPE_MAP: Dict[int, str] = {
    1: "Text",
    2: "Block",
}


def decode_table_break_flags(flags: int) -> Dict[str, Any]:
    """
    Decodes the DXF group code 90 (Table Break Options) for an AcDbTable entity.
    """
    binary_str = bin(flags)
    hex_str = hex(flags)

    is_break_none = bool(flags & TABLE_BREAK_NONE)
    is_break_enabled = bool(flags & TABLE_BREAK_ENABLE)
    is_auto_height = bool(flags & TABLE_BREAK_AUTO)
    is_manual_positioning = bool(flags & TABLE_BREAK_ALLOW_MANUAL_POSITIONING)
    is_repeat_header = bool(flags & TABLE_BREAK_REPEAT_HEADER)

    explanation_lines = [
        f"Break Flags (DXF 90): {flags} (binary: {binary_str}, hex: {hex_str})",
        f"  Bit 1 (0x1,  kTableBreakNone):                   {'YES' if is_break_none else 'NO'}",
        f"  Bit 2 (0x2,  kTableBreakEnable):                 {'YES' if is_break_enabled else 'NO'} "
        + ("<-- [BREAK ENABLED!]" if is_break_enabled else ""),
        f"  Bit 3 (0x4,  kTableBreakAuto):                   {'YES' if is_auto_height else 'NO'} "
        + ("(Automatic height)" if is_auto_height else ""),
        f"  Bit 4 (0x8,  kTableBreakAllowManualPositioning): {'YES' if is_manual_positioning else 'NO'} "
        + ("(Manual positioning)" if is_manual_positioning else ""),
        f"  Bit 5 (0x10, kTableBreakRepeatHeader):           {'YES' if is_repeat_header else 'NO'} "
        + ("(Repeat headers / custom offsets)" if is_repeat_header else ""),
    ]

    summary = (
        "Table Break is ENABLED with Auto-Height and Repeating Headers"
        if (is_break_enabled and is_auto_height and is_repeat_header)
        else ("Table Break is ENABLED" if is_break_enabled else "Table Break is DISABLED")
    )

    return {
        "raw_flags": flags,
        "binary": binary_str,
        "hex": hex_str,
        "break_none": is_break_none,
        "break_enabled": is_break_enabled,
        "auto_height": is_auto_height,
        "manual_positioning": is_manual_positioning,
        "repeat_header": is_repeat_header,
        "explanation": "\n".join(explanation_lines),
        "summary": summary,
    }


def extract_tablecontent_row_defs(all_tags: List[DXFTag]) -> List[Tuple[int, float]]:
    """
    Extracts row type definitions (Title, Header, Data) and heights from TABLECONTENT objects.
    """
    row_defs: List[Tuple[int, float]] = []
    in_tc = False
    for i, t in enumerate(all_tags):
        if t.code == 0 and t.value == "TABLECONTENT":
            in_tc = True
        elif in_tc and t.code == 0:
            in_tc = False
        if in_tc and t.value == "TABLEROW_BEGIN":
            # Next code 90 is row type, next code 40 is row height
            r_type = next((int(all_tags[k].value) for k in range(i + 1, min(i + 8, len(all_tags))) if all_tags[k].code == 90), 3)
            r_height = next((float(all_tags[k].value) for k in range(i + 1, min(i + 8, len(all_tags))) if all_tags[k].code == 40), 0.0)
            row_defs.append((r_type, r_height))

    return row_defs


def extract_tablestyles(all_tags: List[DXFTag]) -> Dict[str, Dict[str, Any]]:
    """
    Extracts TABLESTYLE objects from the DXF tag stream.
    """
    styles: Dict[str, Dict[str, Any]] = {}
    curr: Optional[Dict[str, Any]] = None
    in_style = False

    for t in all_tags:
        if t.code == 0 and t.value == "TABLESTYLE":
            if curr and "handle" in curr:
                styles[curr["handle"]] = curr
            curr = {"handle": "", "name": "Standard", "cell_styles": {}}
            in_style = True
        elif in_style and t.code == 0:
            if curr and "handle" in curr:
                styles[curr["handle"]] = curr
            curr = None
            in_style = False

        if in_style and curr is not None:
            if t.code == 5 and not curr["handle"]:
                curr["handle"] = str(t.value)
            elif t.code == 3 and curr["name"] == "Standard":
                curr["name"] = str(t.value)

    if curr and "handle" in curr:
        styles[curr["handle"]] = curr

    return styles


def parse_acad_table(
    entity_tags: List[DXFTag],
    index: int = 1,
    row_defs: Optional[List[Tuple[int, float]]] = None,
    tablestyles: Optional[Dict[str, Dict[str, Any]]] = None,
) -> Dict[str, Any]:
    """
    Parses a single ACAD_TABLE entity from its tag stream.
    Extracts all entity attributes, dimensions, break options, and rich cell-by-cell metadata.
    """
    handle = ""
    owner = ""
    layer = "0"
    ins_x = 0.0
    ins_y = 0.0
    ins_z = 0.0

    break_flags: Optional[int] = None
    num_rows = 0
    num_cols = 0
    style_handle = ""
    block_record = ""

    col_widths: List[float] = []
    row_heights: List[float] = []

    last_142_idx = -1
    acdb_table_idx = -1

    for i, tag in enumerate(entity_tags):
        if tag.code == 5 and not handle:
            handle = str(tag.value)
        elif tag.code == 330 and not owner:
            owner = str(tag.value)
        elif tag.code == 8 and layer == "0":
            layer = str(tag.value)
        elif tag.code == 10 and ins_x == 0.0:
            ins_x = float(tag.value) if isinstance(tag.value, (int, float)) else 0.0
        elif tag.code == 20 and ins_y == 0.0:
            ins_y = float(tag.value) if isinstance(tag.value, (int, float)) else 0.0
        elif tag.code == 30 and ins_z == 0.0:
            ins_z = float(tag.value) if isinstance(tag.value, (int, float)) else 0.0

        if tag.code == 100 and str(tag.value) == "AcDbTable":
            acdb_table_idx = i

    if acdb_table_idx != -1:
        for j in range(acdb_table_idx + 1, len(entity_tags)):
            t = entity_tags[j]
            if t.code == 100:
                break
            if t.code == 342 and not style_handle:
                style_handle = str(t.value)
            elif t.code == 343 and not block_record:
                block_record = str(t.value)
            elif t.code == 90 and break_flags is None:
                break_flags = int(t.value)
            elif t.code == 91 and num_rows == 0:
                num_rows = int(t.value)
            elif t.code == 92 and num_cols == 0:
                num_cols = int(t.value)
            elif t.code == 141:
                # DXF Group Code 141: Row Height (repeated for each row)
                if isinstance(t.value, (int, float)):
                    row_heights.append(float(t.value))
            elif t.code == 142:
                # DXF Group Code 142: Column Width (repeated for each column)
                if isinstance(t.value, (int, float)):
                    col_widths.append(float(t.value))
                last_142_idx = j

    if break_flags is None:
        break_flags = 0

    break_info = decode_table_break_flags(break_flags)

    # If rows or cols not explicit, fallback
    if num_cols <= 0:
        num_cols = len(col_widths) if col_widths else 5
    if num_rows <= 0:
        num_rows = len(row_heights) if row_heights else 10

    # Ensure widths and heights arrays match dimensions
    while len(col_widths) < num_cols:
        col_widths.append(2.5)
    while len(row_heights) < num_rows:
        row_heights.append(0.36)

    # -------------------------------------------------------------------------
    # Parse individual cells: each cell begins with DXF Group Code 171
    # -------------------------------------------------------------------------
    cell_tag_slice = entity_tags[last_142_idx + 1 :] if last_142_idx != -1 else []
    raw_cells: List[List[DXFTag]] = []
    curr_cell_tags: List[DXFTag] = []

    for tag in cell_tag_slice:
        if tag.code == 171:
            if curr_cell_tags:
                raw_cells.append(curr_cell_tags)
            curr_cell_tags = [tag]
        elif curr_cell_tags:
            curr_cell_tags.append(tag)

    if curr_cell_tags:
        raw_cells.append(curr_cell_tags)

    parsed_cells: List[Dict[str, Any]] = []
    total_expected_cells = num_rows * num_cols

    for idx, c_tags in enumerate(raw_cells):
        if idx >= total_expected_cells:
            break

        r = idx // num_cols
        c = idx % num_cols

        tag_dict: Dict[int, List[Any]] = {}
        for t in c_tags:
            tag_dict.setdefault(t.code, []).append(t.value)

        # 1. Text resolution: clean string from code 302 or 1 or 300
        text = ""
        if 302 in tag_dict and str(tag_dict[302][-1]).strip():
            text = str(tag_dict[302][-1]).strip()
        elif 1 in tag_dict and str(tag_dict[1][-1]).strip():
            text = str(tag_dict[1][-1]).strip()
        elif 300 in tag_dict and str(tag_dict[300][-1]).strip():
            text = str(tag_dict[300][-1]).strip()

        # Filter internal control keywords
        if text in ("CELL_VALUE", "ACVALUE_END", "Standard", "{"):
            text = ""

        # 2. Cell type (Code 171: 1 = Text, 2 = Block)
        cell_type_code = tag_dict.get(171, [1])[0]
        cell_type_name = CELL_TYPE_MAP.get(cell_type_code, f"Unknown ({cell_type_code})")

        # 3. Merging & Spanning (Codes 173, 175, 176)
        is_merged = bool(tag_dict.get(173, [0])[0])
        col_span = int(tag_dict.get(175, [1])[0])
        row_span = int(tag_dict.get(176, [1])[0])

        # 4. Row Style (Title, Header, Data)
        # Determined via TABLECONTENT row definitions or table structure
        if row_defs and r < len(row_defs):
            r_type_code = row_defs[r][0]
            row_style = ROW_STYLE_MAP.get(r_type_code, "Data")
        else:
            # Fallback heuristic:
            if r == 0 or (col_span == num_cols and not text.isdigit()):
                row_style = "Title"
            elif r == 1 or text.startswith("Header") or text.startswith("H"):
                row_style = "Header"
            else:
                row_style = "Data"

        # 5. Cell Style & Column Style
        cell_style = "по строке/столбцу"
        col_width = col_widths[c] if c < len(col_widths) else 2.5
        col_style = "нет"

        # 6. Alignment (Code 170)
        alignment_code = tag_dict.get(170, [None])[0]
        if alignment_code is not None and alignment_code in ALIGNMENT_MAP:
            alignment_name = f"{ALIGNMENT_MAP[alignment_code]} (DXF 170: {alignment_code})"
        else:
            # Default by row style
            if row_style in ("Title", "Header"):
                alignment_name = "Middle Center (DXF 170: 5) [Inherited]"
                alignment_code = 5
            else:
                alignment_name = "Top Center (DXF 170: 2) [Inherited]"
                alignment_code = 2

        # 7. Text Formatting (Code 7: font/style, Code 140: height, Code 145: rotation)
        text_style = tag_dict.get(7, [None])[0]
        text_height = tag_dict.get(140, [None])[0]
        if text_height is None:
            text_height = 0.25 if row_style == "Title" else 0.18

        rotation = float(tag_dict.get(145, [0.0])[0])
        override_flags = tag_dict.get(91, [None])[0]
        virtual_edge = tag_dict.get(178, [0])[0]
        autofit = bool(tag_dict.get(174, [0])[0])
        color = tag_dict.get(62, [None])[0]
        bg_color = tag_dict.get(63, [None])[0]

        cell_obj = {
            "row": r,
            "col": c,
            "text": text,
            "cell_style": cell_style,
            "cell_type": cell_type_name,
            "cell_type_code": cell_type_code,
            "row_style": row_style,
            "row_height": row_heights[r] if r < len(row_heights) else 0.36,
            "col_style": col_style,
            "col_width": col_width,
            "alignment_code": alignment_code,
            "alignment_name": alignment_name,
            "is_merged": is_merged,
            "col_span": col_span,
            "row_span": row_span,
            "text_style": text_style or "Standard",
            "text_height": text_height,
            "rotation": rotation,
            "override_flags": override_flags,
            "virtual_edge": virtual_edge,
            "autofit": autofit,
            "color": color,
            "bg_color": bg_color,
            "raw_tags_count": len(c_tags),
        }
        parsed_cells.append(cell_obj)

    cell_texts = [c["text"] for c in parsed_cells if c["text"]]
    break_height = sum(row_heights)

    # Detect top labels: initial consecutive rows with row_style in ('Title', 'Header') until first 'Data'
    top_label_rows = []
    top_label_styles = []
    top_labels_height = 0.0
    for r in range(num_rows):
        row_cells = [c for c in parsed_cells if c["row"] == r]
        row_style = row_cells[0]["row_style"] if row_cells else ("Title" if r == 0 else "Header" if r == 1 else "Data")
        if row_style in ("Title", "Header"):
            top_label_rows.append(r)
            top_label_styles.append(row_style)
            top_labels_height += row_heights[r] if r < len(row_heights) else 0.0
        else:
            # Reached first Data row: top labels end here!
            break

    repeat_top_labels = bool(break_info.get("repeat_header", False)) or bool(break_flags & 0x10)
    header_height = top_labels_height if top_label_rows else (row_heights[0] if row_heights else 0.0)
    data_height = break_height - header_height

    return {
        "index": index,
        "handle": handle,
        "owner": owner,
        "layer": layer,
        "insert_point": (ins_x, ins_y, ins_z),
        "style_handle": style_handle,
        "block_record": block_record,
        "break_flags": break_flags,
        "break_info": break_info,
        "break_height": break_height,
        "manual_break_height": round(break_height, 4),
        "header_height": round(header_height, 4),
        "data_height": round(data_height, 4),
        "repeat_top_labels": repeat_top_labels,
        "top_labels_count": len(top_label_rows),
        "top_label_styles": top_label_styles,
        "top_labels_height": round(top_labels_height, 4),
        "num_rows": num_rows,
        "num_cols": num_cols,
        "col_widths": col_widths,
        "row_heights": row_heights,
        "cells": parsed_cells,
        "cell_texts": cell_texts,
        "total_tags": len(entity_tags),
    }


def analyze_tables_in_dxf(file_path: str) -> List[Dict[str, Any]]:
    """
    Loads a DXF file and returns detailed analyses for all ACAD_TABLE entities found.
    Extracts global TABLECONTENT and TABLESTYLE data to accurately resolve row and cell styles.
    """
    all_tags = list(read_tags(file_path))
    row_defs = extract_tablecontent_row_defs(all_tags)
    tablestyles = extract_tablestyles(all_tags)
    table_entities = extract_entities_by_type(all_tags, "ACAD_TABLE")

    results: List[Dict[str, Any]] = []
    for idx, raw_tags in enumerate(table_entities, start=1):
        parsed = parse_acad_table(raw_tags, idx, row_defs, tablestyles)
        results.append(parsed)

    return results


def print_table_report(table_data: Dict[str, Any], show_all_cells: bool = True) -> None:
    """
    Prints a formatted, highly informative report of a single ACAD_TABLE entity,
    including table dimensions, break flags, row styles, column styles, and
    every cell's complete properties.
    """
    print("=" * 80)
    print(f"TABLE FRAGMENT #{table_data['index']} (Handle: 0x{table_data['handle']})")
    print("=" * 80)
    print(f"Owner Handle:    0x{table_data['owner']}")
    print(f"Layer:           {table_data['layer']}")
    print(
        f"Insert Point:    X={table_data['insert_point'][0]:.4f}, "
        f"Y={table_data['insert_point'][1]:.4f}, "
        f"Z={table_data['insert_point'][2]:.4f}"
    )
    print(f"Table Style:     0x{table_data['style_handle']}")
    print(f"Dimensions:      {table_data['num_rows']} rows x {table_data['num_cols']} columns")
    print(f"Break Height:    {table_data['break_height']:.4f} mm (Задание высоты вручную: {table_data['manual_break_height']:.4f} mm)")
    repeat_label_str = "ВКЛЮЧЕНО (Title и Header повторяются на фрагментах)" if table_data["repeat_top_labels"] else "ОТКЛЮЧЕНО"
    top_styles_str = ", ".join(table_data["top_label_styles"]) if table_data["top_label_styles"] else "Нет"
    print(f"Повтор верхних меток: {repeat_label_str}")
    print(f"  └─ Метки до Data:  {table_data['top_labels_count']} строк [{top_styles_str}], высота: {table_data['top_labels_height']:.4f} mm")
    print(f"Column Widths:   {table_data['col_widths']} (DXF Code 142, {len(table_data['col_widths'])} cols)")
    print(f"Row Heights:     {table_data['row_heights']} (DXF Code 141, {len(table_data['row_heights'])} rows)")
    print("-" * 80)
    print("TABLE BREAK ANALYSIS (DXF Group Code 90):")
    print(table_data["break_info"]["explanation"])
    print(f"=> Decoded State: {table_data['break_info']['summary']}")
    print("-" * 80)

    # 1. Grid Summary of Rows with Row Styles
    print("ТАБЛИЧНАЯ СЕТКА И СТИЛИ СТРОК (TITLE, HEADER, DATA):")
    print(f"{'Row':<5} {'Стиль строки':<14} {'Высота':<8} {'Содержимое ячеек по колонкам'}")
    print("-" * 80)

    cells = table_data["cells"]
    num_cols = table_data["num_cols"]
    num_rows = table_data["num_rows"]

    for r in range(num_rows):
        row_cells = [c for c in cells if c["row"] == r]
        if not row_cells:
            continue
        row_style = row_cells[0]["row_style"]
        row_h = row_cells[0]["row_height"]
        texts_preview = " | ".join(
            f"C{c['col']}: '{c['text']}'" if not c["is_merged"] else f"C{c['col']}: (merged)"
            for c in row_cells
        )
        print(f"R{r:<4} {row_style:<14} {row_h:<8.3f} {texts_preview}")

    # 2. Detailed Cell-by-Cell Inspector
    print("-" * 90)
    print(f"ДЕТАЛЬНЫЙ АНАЛИЗ ВСЕХ ЯЧЕЕК (Всего ячеек: {len(cells)}):")
    print(
        f"{'Pos':<7} {'Текст ячейки':<14} {'Стиль ячейки':<18} {'Стиль строки':<13} {'Стиль столбца':<14} "
        f"{'Выравнивание':<26} {'Span / Merged':<15} {'Шрифт / Высота'}"
    )
    print("-" * 90)

    for c in cells:
        pos = f"R{c['row']}C{c['col']}"
        text_disp = repr(c["text"]) if len(c["text"]) <= 12 else repr(c["text"][:9] + "...")
        span_disp = f"{c['row_span']}x{c['col_span']} (Merged)" if c["is_merged"] else f"{c['row_span']}x{c['col_span']}"
        font_h = f"{c['text_style']} / {c['text_height']:.2f}"

        print(
            f"{pos:<7} {text_disp:<14} {c['cell_style']:<18} {c['row_style']:<13} {c['col_style']:<14} "
            f"{c['alignment_name']:<26} {span_disp:<15} {font_h}"
        )

    print("-" * 90)
    print()


def analyze_images_and_xrefs_in_dxf(dxf_path: str) -> Dict[str, Any]:
    """
    Parses and extracts all external references (XREFs), raster image definitions
    (IMAGEDEF), and image entities (IMAGE) from DXF.
    """
    import math

    tags = list(read_tags(dxf_path))
    image_defs: Dict[str, Dict[str, Any]] = {}
    images: List[Dict[str, Any]] = []
    dwg_xrefs: List[Dict[str, Any]] = []
    raster_vars: Dict[str, Any] = {}

    in_objects = False
    in_entities = False
    in_blocks = False

    i = 0
    while i < len(tags):
        tag = tags[i]

        if tag.code == 0 and tag.value == "SECTION":
            if i + 1 < len(tags) and tags[i + 1].code == 2:
                sec = str(tags[i + 1].value).upper()
                in_objects = (sec == "OBJECTS")
                in_entities = (sec == "ENTITIES")
                in_blocks = (sec == "BLOCKS")
                i += 2
                continue

        if tag.code == 0 and tag.value == "ENDSEC":
            in_objects = False
            in_entities = False
            in_blocks = False
            i += 1
            continue

        # 1. BLOCKS: DWG Xrefs
        if in_blocks and tag.code == 0 and tag.value == "BLOCK":
            block_tags = [tag]
            j = i + 1
            while j < len(tags) and tags[j].code != 0:
                block_tags.append(tags[j])
                j += 1
            b_name = next((str(t.value) for t in block_tags if t.code == 2), "")
            b_flags = next((int(t.value) for t in block_tags if t.code == 70), 0)
            b_path = next((str(t.value) for t in block_tags if t.code == 1), "")
            b_handle = next((str(t.value) for t in block_tags if t.code == 5), "")
            if (b_flags & 4) or (b_flags & 32) or b_path:
                dwg_xrefs.append({
                    "name": b_name,
                    "handle": b_handle,
                    "path": b_path,
                    "flags": b_flags,
                    "is_resolved": bool(b_flags & 32),
                })
            i = j
            continue

        # 2. OBJECTS: IMAGEDEF & RASTERVARIABLES
        if in_objects and tag.code == 0:
            if tag.value == "IMAGEDEF":
                def_tags = [tag]
                j = i + 1
                while j < len(tags) and tags[j].code != 0:
                    def_tags.append(tags[j])
                    j += 1
                h = next((str(t.value) for t in def_tags if t.code == 5), "").upper()
                p = next((str(t.value) for t in def_tags if t.code == 1), "")
                pw = next((float(t.value) for t in def_tags if t.code == 10), 0.0)
                ph = next((float(t.value) for t in def_tags if t.code == 20), 0.0)
                sx = next((float(t.value) for t in def_tags if t.code == 11), 1.0)
                sy = next((float(t.value) for t in def_tags if t.code == 21), 1.0)
                loaded = next((int(t.value) for t in def_tags if t.code == 280), 1) == 1
                units = next((int(t.value) for t in def_tags if t.code == 281), 0)
                unit_names = {0: "None", 1: "Millimeter (mm)", 2: "Centimeter (cm)", 5: "Inch (in)"}
                if h:
                    image_defs[h] = {
                        "handle": h,
                        "path": p,
                        "pixel_width": pw,
                        "pixel_height": ph,
                        "pixel_size_x": sx,
                        "pixel_size_y": sy,
                        "is_loaded": loaded,
                        "units": units,
                        "units_name": unit_names.get(units, "Custom"),
                    }
                i = j
                continue
            elif tag.value == "RASTERVARIABLES":
                r_tags = [tag]
                j = i + 1
                while j < len(tags) and tags[j].code != 0:
                    r_tags.append(tags[j])
                    j += 1
                raster_vars = {
                    "image_frame": next((int(t.value) for t in r_tags if t.code == 70), 1),
                    "image_quality": next((int(t.value) for t in r_tags if t.code == 71), 1),
                    "units": next((int(t.value) for t in r_tags if t.code == 72), 5),
                }
                i = j
                continue

        # 3. ENTITIES: IMAGE
        if in_entities and tag.code == 0 and tag.value == "IMAGE":
            img_tags = [tag]
            j = i + 1
            while j < len(tags) and tags[j].code != 0:
                img_tags.append(tags[j])
                j += 1

            h = next((str(t.value) for t in img_tags if t.code == 5), "")
            layer = next((str(t.value) for t in img_tags if t.code == 8), "0")
            ins_x = next((float(t.value) for t in img_tags if t.code == 10), 0.0)
            ins_y = next((float(t.value) for t in img_tags if t.code == 20), 0.0)
            ins_z = next((float(t.value) for t in img_tags if t.code == 30), 0.0)

            ux = next((float(t.value) for t in img_tags if t.code == 11), 1.0)
            uy = next((float(t.value) for t in img_tags if t.code == 21), 0.0)
            uz = next((float(t.value) for t in img_tags if t.code == 31), 0.0)

            vx = next((float(t.value) for t in img_tags if t.code == 12), 0.0)
            vy = next((float(t.value) for t in img_tags if t.code == 22), 1.0)
            vz = next((float(t.value) for t in img_tags if t.code == 32), 0.0)

            uw = next((float(t.value) for t in img_tags if t.code == 13), 100.0)
            vh = next((float(t.value) for t in img_tags if t.code == 23), 100.0)

            def_handle = next((str(t.value) for t in img_tags if t.code == 340), "").upper()
            display_props = next((int(t.value) for t in img_tags if t.code == 70), 7)
            clipping = next((int(t.value) for t in img_tags if t.code == 280), 0) == 1
            brightness = next((int(t.value) for t in img_tags if t.code == 281), 50)
            contrast = next((int(t.value) for t in img_tags if t.code == 282), 50)
            fade = next((int(t.value) for t in img_tags if t.code == 283), 0)

            u_len = math.hypot(ux, uy)
            v_len = math.hypot(vx, vy)
            cad_w = uw * u_len
            cad_h = vh * v_len
            rot_deg = math.degrees(math.atan2(uy, ux))

            img_def = image_defs.get(def_handle)

            images.append({
                "handle": h,
                "layer": layer,
                "insert_point": (ins_x, ins_y, ins_z),
                "u_vector": (ux, uy, uz),
                "v_vector": (vx, vy, vz),
                "pixel_size": (uw, vh),
                "cad_size": (cad_w, cad_h),
                "rotation_deg": rot_deg,
                "def_handle": def_handle,
                "imagedef": img_def,
                "display_props": display_props,
                "clipping": clipping,
                "brightness": brightness,
                "contrast": contrast,
                "fade": fade,
            })
            i = j
            continue

        i += 1

    for img in images:
        h = img["def_handle"].upper()
        if h in image_defs:
            img["imagedef"] = image_defs[h]
        elif len(image_defs) == 1:
            img["imagedef"] = list(image_defs.values())[0]

    return {
        "images": images,
        "imagedefs": image_defs,
        "dwg_xrefs": dwg_xrefs,
        "raster_variables": raster_vars,
    }


def print_xrefs_report(xref_data: Dict[str, Any]):
    images = xref_data.get("images", [])
    imagedefs = xref_data.get("imagedefs", {})
    dwg_xrefs = xref_data.get("dwg_xrefs", [])

    print("=" * 90)
    print("ВНЕШНИЕ ССЫЛКИ И РАСТРОВЫЕ ИЗОБРАЖЕНИЯ (EXTERNAL REFERENCES & IMAGES)")
    print("=" * 90)
    print(f"Обнаружено растровых изображений (IMAGE):       {len(images)}")
    print(f"Обнаружено определений изображений (IMAGEDEF): {len(imagedefs)}")
    print(f"Обнаружено DWG внешних ссылок (XREF BLOCKS):    {len(dwg_xrefs)}")
    print("-" * 90)

    for idx, img in enumerate(images, 1):
        def_info = img.get("imagedef") or {}
        img_path = def_info.get("path", "(путь не задан)")
        ins = img["insert_point"]
        u = img["u_vector"]
        v = img["v_vector"]
        pw, ph = img["pixel_size"]
        cw, ch = img["cad_size"]

        print(f"РАСТРОВОЕ ИЗОБРАЖЕНИЕ #{idx} (Handle: 0x{img['handle']})")
        print(f"  Файл ссылки (IMAGEDEF):  {img_path} [0x{img['def_handle']}]")
        print(f"  Статус загрузки:        {'Загружено (Loaded)' if def_info.get('is_loaded', True) else 'Выгружено (Unloaded)'}")
        print(f"  Слой (Layer):           {img['layer']}")
        print(f"  Точка вставки (WCS):    X={ins[0]:.4f}, Y={ins[1]:.4f}, Z={ins[2]:.4f}")
        print(f"  Размер в пикселях:      {pw:.0f} x {ph:.0f} px")
        print(f"  Вектор U (шаг X):       [{u[0]:.6f}, {u[1]:.6f}]")
        print(f"  Вектор V (шаг Y):       [{v[0]:.6f}, {v[1]:.6f}]")
        print(f"  Размер на чертеже:      {cw:.4f} x {ch:.4f} мм")
        print(f"  Угол поворота:          {img['rotation_deg']:.2f}°")
        print(f"  Свойства изображения:   Яркость={img['brightness']}%, Контраст={img['contrast']}%, Слияние={img['fade']}%")
        print(f"  Контур подрезки (Clip): {'Включен' if img['clipping'] else 'Отключен'}")
        print("-" * 90)

    for idx, xref in enumerate(dwg_xrefs, 1):
        print(f"DWG ВНЕШНЯЯ ССЫЛКА #{idx} (Block: {xref['name']}, Handle: 0x{xref['handle']})")
        print(f"  Путь к чертежу:         {xref['path']}")
        print(f"  Статус разрешения:      {'Разрешена (Resolved)' if xref['is_resolved'] else 'Не найдена'}")
        print("-" * 90)
    print()


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python table_reader.py <file.dxf>")
        sys.exit(1)

    dxf_path = sys.argv[1]
    tables = analyze_tables_in_dxf(dxf_path)
    xrefs_info = analyze_images_and_xrefs_in_dxf(dxf_path)

    print(f"\nAnalyzing: {dxf_path}")
    print(f"Found {len(tables)} ACAD_TABLE entities.")
    print(f"Found {len(xrefs_info['images'])} IMAGE entities and {len(xrefs_info['imagedefs'])} IMAGEDEF objects.\n")

    for tbl in tables:
        print_table_report(tbl)

    print_xrefs_report(xrefs_info)

