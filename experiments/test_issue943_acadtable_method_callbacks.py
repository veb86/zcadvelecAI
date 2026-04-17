from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
MODEL = ROOT / "cad_source/zcad/velec/acadtable/uzeacadtable_model.pas"
MERGE = ROOT / "cad_source/zcad/velec/acadtable/uzeacadtable_merge.pas"


def test_acadtable_merge_callbacks_are_passed_as_method_values():
    model_source = MODEL.read_text(encoding="utf-8")
    merge_source = MERGE.read_text(encoding="utf-8")

    assert "TAcadTableSizeGetter = function(Idx: Integer): Double of object;" in merge_source
    assert "@GetColWidthLocal" not in model_source
    assert "@GetRowHeightLocal" not in model_source
    assert "RowIdx, ColIdx, FMerges, GetColWidthLocal)" in model_source
    assert "RowIdx, ColIdx, FMerges, GetRowHeightLocal)" in model_source


if __name__ == "__main__":
    test_acadtable_merge_callbacks_are_passed_as_method_values()
