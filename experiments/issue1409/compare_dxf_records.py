#!/usr/bin/env python3
"""Compare selected DXF records without being distracted by line formatting.

The files attached to issue #1409 use different whitespace conventions, so a
plain text diff obscures the meaningful group-code differences.  This helper
parses code/value pairs, groups them into records beginning with group code 0,
and prints records selected by type or by a value they contain.

Usage:
    python3 compare_dxf_records.py FILE [FILE ...] \
        --type TABLESTYLE --type CELLSTYLEMAP --contains TABLECONTENT
"""

from __future__ import annotations

import argparse
from dataclasses import dataclass
from pathlib import Path


@dataclass(frozen=True)
class Pair:
    code: str
    value: str
    line: int


def read_pairs(path: Path) -> list[Pair]:
    lines = path.read_text(encoding="utf-8", errors="replace").splitlines()
    return [
        Pair(lines[index].strip(), lines[index + 1].strip(), index + 1)
        for index in range(0, len(lines) - 1, 2)
    ]


def records(pairs: list[Pair]) -> list[list[Pair]]:
    result: list[list[Pair]] = []
    current: list[Pair] = []
    for pair in pairs:
        if pair.code == "0":
            if current:
                result.append(current)
            current = [pair]
        elif current:
            current.append(pair)
    if current:
        result.append(current)
    return result


def selected(
    record: list[Pair], record_types: set[str], contained_values: set[str]
) -> bool:
    if record_types and record[0].value in record_types:
        return True
    values = {pair.value for pair in record}
    return bool(values & contained_values)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("files", type=Path, nargs="+")
    parser.add_argument("--type", dest="record_types", action="append", default=[])
    parser.add_argument("--contains", action="append", default=[])
    args = parser.parse_args()

    record_types = set(args.record_types)
    contained_values = set(args.contains)
    if not record_types and not contained_values:
        parser.error("select at least one record with --type or --contains")

    for path in args.files:
        print(f"=== {path}")
        matches = [
            record
            for record in records(read_pairs(path))
            if selected(record, record_types, contained_values)
        ]
        for number, record in enumerate(matches, start=1):
            print(
                f"--- match {number}: {record[0].value} "
                f"(source line {record[0].line})"
            )
            for pair in record:
                print(f"{pair.code:>3} | {pair.value}")
        if not matches:
            print("(no matching records)")


if __name__ == "__main__":
    main()
