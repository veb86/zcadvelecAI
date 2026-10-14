#!/usr/bin/env python3
"""Regression tests for issue #1468: spellchecker must scan text inside blocks."""

from pathlib import Path
import re


ROOT = Path(__file__).resolve().parent
SPELL_DIR = ROOT / "cad_source" / "zcad" / "velec" / "uzvfspellchecker"
FORM_PAS = SPELL_DIR / "uzvfspellform.pas"
DATA_PAS = SPELL_DIR / "uzvfspelldata.pas"
LOGIC_PAS = SPELL_DIR / "uzvfspelllogic.pas"
ZOOM_PAS = SPELL_DIR / "uzvfspellzoom.pas"
BLOCKS_PAS = SPELL_DIR / "uzvfspellblocks.pas"


def read_text(path: Path) -> str:
    return path.read_text(encoding="utf-8", errors="ignore")


def extract_routine(source: str, routine_name: str) -> str:
    pattern = (
        rf"(?:procedure|function)\s+{re.escape(routine_name)}\b.*?"
        rf"(?=\n(?:procedure|function|constructor)\s+|\n\{{|\ninitialization"
        rf"|\nfinalization|\Z)"
    )
    matches = [
        match.group(0)
        for match in re.finditer(pattern, source, flags=re.IGNORECASE | re.DOTALL)
        if re.search(r"\n\s*begin\b", match.group(0), flags=re.IGNORECASE)
    ]
    assert matches, f"{routine_name} routine is missing"
    return matches[0]


def test_block_support_lives_in_separate_unit() -> None:
    assert BLOCKS_PAS.exists(), "block scanning helpers must live in a separate unit"

    blocks_source = read_text(BLOCKS_PAS)
    assert "unit uzvfspellblocks;" in blocks_source
    for name in (
        "function IsSpellBlockInsert",
        "function GetSpellBlockDef",
        "function TryGetSpellBlockMatrix",
        "function TransformSpellVolume",
        "function RebuildSpellBlockInserts",
        "function ApplySpellBlockTextCorrection",
    ):
        assert name in blocks_source, f"{name} is missing"


def test_only_plain_block_inserts_are_scanned() -> None:
    blocks_source = read_text(BLOCKS_PAS)
    routine = extract_routine(blocks_source, "IsSpellBlockInsert")

    # Устройства (GDBDeviceID) не являются обычными блоками и не сканируются
    assert "GDBBlockInsertID" in routine
    assert "GDBDeviceID" not in routine


def test_error_keeps_block_insert_chain() -> None:
    data_source = read_text(DATA_PAS)

    assert "TSpellBlockPath = array of Pointer;" in data_source
    assert re.search(r"BlockPath:\s*TSpellBlockPath;", data_source)
    assert "const ABlockPath: TSpellBlockPath" in data_source
    assert "Result^.BlockPath := Copy(ABlockPath);" in data_source

    logic_source = read_text(LOGIC_PAS)
    assert "const ABlockPath: TSpellBlockPath" in logic_source
    assert "ABlockPath)" in logic_source


def test_scan_walks_model_space_and_descends_into_blocks() -> None:
    form_source = read_text(FORM_PAS)

    load_routine = extract_routine(form_source, "LoadCurrentDrawingText")
    assert "GetCurrentROOT^.ObjArray" in load_routine
    assert "ScanSpellEntity(EntityPtr, nil, State)" in load_routine

    entity_routine = extract_routine(form_source, "ScanSpellEntity")
    assert "IsSpellTextEntity(EntityPtr)" in entity_routine
    assert "IsSpellBlockInsert(EntityPtr)" in entity_routine
    assert "ScanSpellBlockInsert" in entity_routine


def test_each_block_definition_is_scanned_once() -> None:
    form_source = read_text(FORM_PAS)
    routine = extract_routine(form_source, "ScanSpellBlockInsert")

    skip_index = routine.find("ScannedBlockDefs.IndexOf(BlockDef) >= 0")
    add_index = routine.find("ScannedBlockDefs.Add(BlockDef)")
    scan_index = routine.find("BlockDef^.ObjArray.beginiterate")
    assert skip_index >= 0, "already scanned block definitions must be skipped"
    assert add_index > skip_index
    assert scan_index > add_index
    assert "MAX_SPELL_BLOCK_NESTING" in routine

    # Ошибка запоминает первую просканированную вставку блока
    assert "BlockPath[High(BlockPath)] := InsertPtr;" in routine


def test_zoom_transforms_block_local_volume_to_world() -> None:
    zoom_source = read_text(ZOOM_PAS)
    assert "uzvfspellblocks" in zoom_source

    transform = extract_routine(zoom_source, "TransformBlockVolumeToWorld")
    assert "TryGetSpellBlockMatrix(ErrorPtr^.BlockPath" in transform
    assert "TransformSpellVolume(Volume" in transform

    word_volume = extract_routine(zoom_source, "TryGetSpellErrorWordVolume")
    assert "TransformBlockVolumeToWorld(ErrorPtr, Volume)" in word_volume

    entity_zoom = extract_routine(zoom_source, "ZoomToSpellEntity")
    assert "TransformBlockVolumeToWorld(ErrorPtr, volume)" in entity_zoom


def test_block_matrix_goes_from_nested_insert_to_model_insert() -> None:
    blocks_source = read_text(BLOCKS_PAS)
    routine = extract_routine(blocks_source, "TryGetSpellBlockMatrix")

    assert "BlockPath[High(BlockPath)])^.objmatrix" in routine
    assert "downto 0" in routine
    assert "MatrixMultiply(Matrix," in routine


def test_correction_inside_block_updates_definition_with_undo() -> None:
    form_source = read_text(FORM_PAS)
    routine = extract_routine(form_source, "ApplySuggestionToEntity")
    assert "Length(ErrorPtr^.BlockPath) > 0" in routine
    assert "ApplySpellBlockTextCorrection(EntityPtr, ErrorPtr^.BlockPath" in routine

    blocks_source = read_text(BLOCKS_PAS)
    assert "TUndoCmdSpellBlockText = class(TUCmdBase)" in blocks_source
    apply_template = extract_routine(blocks_source, "TUndoCmdSpellBlockText.ApplyTemplate")
    assert "RebuildSpellBlockInserts(FBlockDef)" in apply_template

    correction = extract_routine(blocks_source, "ApplySpellBlockTextCorrection")
    assert "zcStartUndoCommand" in correction
    assert "zcEndUndoCommand" in correction
