#!/usr/bin/env python3
"""Regression checks for DWG owner attach hot-path lookups."""

from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
IMPORT_SOURCE = ROOT / "cad_source/zengine/fileformats/dwg/uzedwgimport.pas"


def read_import_source():
    return IMPORT_SOURCE.read_text(encoding="utf-8")


def extract_between(source, start_token, end_token):
    start = source.index(start_token)
    end = source.index(end_token, start + len(start_token))
    return source[start:end]


def test_owner_attach_uses_context_handle_lookup():
    source = read_import_source()
    attach_body = extract_between(
        source,
        "procedure DWGAttachEntityWithContext",
        "procedure DWGAttachEntity(Entity",
    )

    assert "DWGContextTargetHasKind(Owner, Context, dokBlockInsert)" in attach_body
    assert "DWGPointerHasKind(Owner, dokBlockInsert)" not in attach_body


def test_context_kind_helper_uses_handle_map_not_pointer_scan():
    source = read_import_source()
    helper_body = extract_between(
        source,
        "function DWGContextTargetHasKind",
        "function DWGObjTypeIsDimension",
    )

    assert "Context.TargetHandle" in helper_body
    assert "LoadCtx.Handles.TryGet" in helper_body
    assert "for I :=" not in helper_body
    assert "function DWGPointerHasKind" not in source


def test_block_ref_attach_uses_context_handle_lookup():
    source = read_import_source()
    ref_body = extract_between(
        source,
        "procedure DWGAttachRefWithContext",
        "procedure DWGAttachRef(Entity",
    )

    assert "DWGContextTargetHasKind(Ref, Context, dokBlockDef)" in ref_body
    assert "DWGPointerHasKind(Ref, dokBlockDef)" not in ref_body


if __name__ == "__main__":
    test_owner_attach_uses_context_handle_lookup()
    test_context_kind_helper_uses_handle_map_not_pointer_scan()
    test_block_ref_attach_uses_context_handle_lookup()
    print("DWG owner attach hot-path checks passed")
