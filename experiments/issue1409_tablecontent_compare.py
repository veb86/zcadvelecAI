#!/usr/bin/env python3
"""Compare the TABLECONTENT formatting blocks of AutoCAD table DXFs.

Usage::

    python3 experiments/issue1409_tablecontent_compare.py \
        zcad-export.dxf autocad-resaved.dxf

The regular issue #1409 audit checks the style identifiers and object
references.  This companion experiment reports the TABLEFORMAT payload that
activates those identifiers at cell and row level.  It is intentionally
handle-agnostic so exports of the same table can be compared after AutoCAD
renumbers the drawing.
"""

from collections import Counter
from pathlib import Path
import sys


Pair = tuple[str, str]


def dxf_pairs(path: Path) -> list[Pair]:
    lines = [
        line.strip()
        for line in path.read_text(
            encoding="utf-8", errors="replace"
        ).splitlines()
    ]
    if len(lines) % 2:
        raise ValueError(f"{path}: odd number of DXF lines")
    return list(zip(lines[0::2], lines[1::2]))


def records(pairs: list[Pair], record_type: str) -> list[list[Pair]]:
    result: list[list[Pair]] = []
    current: list[Pair] | None = None
    for pair in pairs:
        if pair[0] == "0":
            if current and current[0] == ("0", record_type):
                result.append(current)
            current = [pair]
        elif current is not None:
            current.append(pair)
    if current and current[0] == ("0", record_type):
        result.append(current)
    return result


def marker_blocks(
    record: list[Pair], begin_marker: str, end_marker: str
) -> list[list[Pair]]:
    result: list[list[Pair]] = []
    start: int | None = None
    for index, (_, value) in enumerate(record):
        if value == begin_marker:
            if start is not None:
                raise ValueError(f"nested {begin_marker}")
            start = index + 1
        elif value == end_marker and start is not None:
            result.append(record[start:index])
            start = None
    if start is not None:
        raise ValueError(f"unterminated {begin_marker}")
    return result


def first_value(block: list[Pair], code: str, default: str = "-") -> str:
    return next(
        (value for item_code, value in block if item_code == code), default
    )


def compact(block: list[Pair]) -> str:
    ignored_markers = {
        "CELLTABLEFORMAT",
        "ROWTABLEFORMAT",
        "TABLEFORMAT_BEGIN",
        "TABLEFORMAT_END",
        "CONTENTFORMAT",
        "CONTENTFORMAT_BEGIN",
        "CONTENTFORMAT_END",
        "MARGIN",
        "CELLMARGIN_BEGIN",
        "CELLMARGIN_END",
        "GRIDFORMAT",
        "GRIDFORMAT_BEGIN",
        "GRIDFORMAT_END",
    }
    return " ".join(
        f"{code}|{value or '<empty>'}"
        for code, value in block
        if value not in ignored_markers
    )


def summarize(path: Path) -> None:
    pairs = dxf_pairs(path)
    tablecontents = records(pairs, "TABLECONTENT")
    if not tablecontents:
        raise ValueError(f"{path}: no TABLECONTENT record")

    print(path)
    for record_index, record in enumerate(tablecontents, 1):
        cell_formats = marker_blocks(
            record,
            "FORMATTEDTABLEDATACELL_BEGIN",
            "FORMATTEDTABLEDATACELL_END",
        )
        table_cells = marker_blocks(record, "TABLECELL_BEGIN", "TABLECELL_END")
        row_formats = marker_blocks(
            record,
            "FORMATTEDTABLEDATAROW_BEGIN",
            "FORMATTEDTABLEDATAROW_END",
        )
        table_rows = marker_blocks(record, "TABLEROW_BEGIN", "TABLEROW_END")
        content_formats = marker_blocks(
            record,
            "FORMATTEDCELLCONTENT_BEGIN",
            "FORMATTEDCELLCONTENT_END",
        )

        if len(cell_formats) != len(table_cells):
            raise ValueError(
                f"{path}: {len(cell_formats)} cell formats but "
                f"{len(table_cells)} TABLECELL blocks"
            )
        if len(row_formats) != len(table_rows):
            raise ValueError(
                f"{path}: {len(row_formats)} row formats but "
                f"{len(table_rows)} TABLEROW blocks"
            )

        print(
            f"  TABLECONTENT #{record_index}: "
            f"{len(table_rows)} rows, {len(table_cells)} cells"
        )
        cell_signatures = Counter(compact(block) for block in cell_formats)
        for count, signature in sorted(
            (count, signature) for signature, count in cell_signatures.items()
        ):
            print(f"    cell format x{count}: {signature}")
        print(
            "    cell style ids: "
            + " ".join(first_value(block, "90") for block in table_cells)
        )
        content_signatures = Counter(compact(block) for block in content_formats)
        for count, signature in sorted(
            (count, signature)
            for signature, count in content_signatures.items()
        ):
            print(f"    content format x{count}: {signature}")

        row_signatures = Counter(compact(block) for block in row_formats)
        for count, signature in sorted(
            (count, signature) for signature, count in row_signatures.items()
        ):
            print(f"    row format x{count}: {signature}")
        print(
            "    row style ids : "
            + " ".join(first_value(block, "90") for block in table_rows)
        )

    for geometry_index, record in enumerate(
        records(pairs, "TABLEGEOMETRY"), 1
    ):
        cache_counts = []
        for index, pair in enumerate(record):
            if pair[0] != "93":
                continue
            cache_counts.append(
                next(
                    (
                        value
                        for code, value in record[index + 1 :]
                        if code == "94"
                    ),
                    "-",
                )
            )
        print(
            f"  TABLEGEOMETRY #{geometry_index} cache counts: "
            + " ".join(cache_counts)
        )
    print()


def main(argv: list[str]) -> int:
    if len(argv) < 2:
        print(__doc__)
        return 1
    for filename in argv[1:]:
        summarize(Path(filename))
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
