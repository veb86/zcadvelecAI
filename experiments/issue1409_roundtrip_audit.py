#!/usr/bin/env python3
"""Audit the AcadTable round-trip structure of a DXF file (issue #1409).

Usage::

    python3 experiments/issue1409_roundtrip_audit.py file.dxf [file.dxf ...]

For every file it reports the five markers that decide whether AutoCAD keeps
the per-cell styles of an ACAD_TABLE entity:

* does the ``ACAD_ROUNDTRIP_2008_TABLE_ENTITY`` XRECORD end with the hard
  pointer ``361`` that terminates the record;
* is the ``TABLEGEOMETRY`` object it points at actually present (and declared
  in the CLASSES section);
* what the AcDbTable ``90`` flag word and the per-cell ``172`` flag value are;
* which style id each row and each cell carries inside TABLECONTENT.
* whether each CELLSTYLE content format points to a real exported text STYLE.

Run it on a ZCAD export and on an AutoCAD-resaved copy of the same drawing to
see the difference.  Reference files live in ``cad_source/test/``.
"""

import sys
from pathlib import Path


def dxf_pairs(path):
    lines = [line.strip() for line in
             path.read_text(encoding="utf-8", errors="replace").splitlines()]
    return list(zip(lines[0::2], lines[1::2]))


def records(pairs, record_type):
    out, current = [], None
    for code, value in pairs:
        if code == "0":
            if current and current[0][1] == record_type:
                out.append(current)
            current = [(code, value)]
        elif current is not None:
            current.append((code, value))
    if current and current[0][1] == record_type:
        out.append(current)
    return out


def report_roundtrip(pairs):
    geometry = {dict(r).get("5") for r in records(pairs, "TABLEGEOMETRY")}
    xrecords = [r for r in records(pairs, "XRECORD")
                if ("102", "ACAD_ROUNDTRIP_2008_TABLE_ENTITY") in r]
    print(f"  TABLEGEOMETRY objects : {len(geometry)}")
    classes = {dict(r).get("1") for r in records(pairs, "CLASS")}
    print(f"  TABLEGEOMETRY class   : {'TABLEGEOMETRY' in classes}")
    for record in xrecords:
        code, value = record[-1]
        ok = code == "361" and value in geometry
        print(f"  xrecord tail          : {code}|{value}  "
              f"{'OK' if ok else 'TRUNCATED - AutoCAD drops TABLECONTENT'}")
    if not xrecords:
        print("  xrecord tail          : no round-trip XRECORD at all")


def report_entity(pairs):
    for i, (code, value) in enumerate(pairs):
        if code == "100" and value == "AcDbTable":
            tail = pairs[i:]
            end = next(j for j, (c, _) in enumerate(tail) if j and c == "0")
            flags = next(v for c, v in tail if c == "90")
            cells = sorted({v for c, v in tail[:end] if c == "172"})
            print(f"  AcDbTable 90 (flags)  : {flags}")
            print(f"  cell 172 values       : {cells}")
            break


def report_styles(pairs):
    rows = [pairs[i + 1][1] for i, (_, v) in enumerate(pairs)
            if v == "TABLEROW_BEGIN" and pairs[i + 1][0] == "90"]
    cells = [pairs[i + 1][1] for i, (_, v) in enumerate(pairs)
             if v == "TABLECELL_BEGIN" and pairs[i + 1][0] == "90"]
    print(f"  row style ids         : {rows}")
    print(f"  cell style ids        : {cells}")


def report_cellstyle_text_refs(pairs):
    text_styles = {
        next((value for code, value in record if code == "5"), None):
        next((value for code, value in record if code == "2"), None)
        for record in records(pairs, "STYLE")
    }
    text_styles.pop(None, None)

    refs = []
    for record in records(pairs, "CELLSTYLEMAP"):
        for index, pair in enumerate(record):
            if pair != ("1", "CONTENTFORMAT_BEGIN"):
                continue
            content_format = record[index + 1:]
            end = next(
                (offset for offset, item in enumerate(content_format)
                 if item == ("309", "CONTENTFORMAT_END")),
                len(content_format),
            )
            refs.append(next(
                (value for code, value in content_format[:end]
                 if code == "340"),
                None,
            ))

    resolved = [
        f"{handle}->{text_styles[handle]}"
        if handle in text_styles else f"{handle}->UNRESOLVED"
        for handle in refs
    ]
    status = "OK" if refs and all(handle in text_styles for handle in refs) \
        else "INVALID - AutoCAD cannot resolve CELLSTYLE content formats"
    print(f"  CELLSTYLE text refs   : {resolved}  {status}")


def main(argv):
    if len(argv) < 2:
        print(__doc__)
        return 1
    for name in argv[1:]:
        path = Path(name)
        print(path)
        pairs = dxf_pairs(path)
        report_roundtrip(pairs)
        report_entity(pairs)
        report_styles(pairs)
        report_cellstyle_text_refs(pairs)
        print()
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
