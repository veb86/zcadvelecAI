#!/usr/bin/env python3
"""
Контракт компиляции AcadTable после разделения GPoint/GVector.

После обновления zmath нулевая точка и вектор имеют разные типы.
Базис матрицы состоит из векторов, а её перенос задаётся точкой.
Проверки фиксируют этот контракт в AcadTable и зависимых модулях,
которые компилятор достигает после исправления первых ошибок.
"""

from pathlib import Path


ROOT = Path(__file__).resolve().parent
ACADTABLE_MODEL = (
    ROOT / "cad_source" / "zcad" / "velec" / "acadtable" / "uzeacadtable_model.pas"
)
CREATE_ACADTABLE_COMMAND = (
    ROOT
    / "cad_source"
    / "zcad"
    / "velec"
    / "uzvspreadsheet"
    / "uzvspreadsheet_cmdcreateacadtable.pas"
)
ACADTABLE_TESTS = ROOT / "cad_source" / "zengine" / "tests" / "uzctacadtable.pas"
TEST_ARC = ROOT / "cad_source" / "zcad" / "velec" / "testarc.pas"
TEST_ARC2 = ROOT / "cad_source" / "zcad" / "velec" / "testarc2.pas"
ROTATE_LOGIC = (
    ROOT / "cad_source" / "zcad" / "velec" / "rotatecontrol" / "uzvrotate_logic.pas"
)
SVG_BLOCK = (
    ROOT / "cad_source" / "zcad" / "velec" / "exporttosvg" / "uexpsvgblock.pas"
)


def read_model() -> str:
    return ACADTABLE_MODEL.read_text(encoding="utf-8")


def read_create_command() -> str:
    return CREATE_ACADTABLE_COMMAND.read_text(encoding="utf-8")


def read_acadtable_tests() -> str:
    return ACADTABLE_TESTS.read_text(encoding="utf-8")


def read_test_arc() -> str:
    return TEST_ARC.read_text(encoding="utf-8")


def read_test_arc2() -> str:
    return TEST_ARC2.read_text(encoding="utf-8")


def read_rotate_logic() -> str:
    return ROTATE_LOGIC.read_text(encoding="utf-8")


def read_svg_block() -> str:
    return SVG_BLOCK.read_text(encoding="utf-8")


def compact(text: str) -> str:
    return "".join(text.split()).lower()


def procedure_section(model: str, start: str, following: str) -> str:
    compact_model = compact(model)
    start_marker = compact(f"procedure GDBObjAcadTable.{start};")
    following_marker = compact(f"procedure GDBObjAcadTable.{following}")
    start_index = compact_model.index(start_marker)
    following_index = compact_model.index(following_marker, start_index + 1)
    return compact_model[start_index:following_index]


def test_point_fields_use_point_zero_constant():
    model = compact(read_model())

    assert "finsertpoint:=nulpoint;" in model
    assert model.count("blockdef^.base:=nulpoint;") == 2
    assert "finsertpoint:=nulvertex;" not in model
    assert "blockdef^.base:=nulvertex;" not in model


def test_control_point_delta_uses_point_zero_constant():
    model = compact(read_model())

    assert "pdesc.dcoord:=nulpoint;" in model
    assert "pdesc.dcoord:=nulvertex;" not in model


def test_create_command_uses_point_zero_constant():
    command = compact(read_create_command())

    assert "inspt:=nulpoint;" in command
    assert "inspt:=nulvertex;" not in command


def test_pascal_acadtable_tests_use_point_zero_constant():
    tests = compact(read_acadtable_tests())

    assert tests.count("insertpt:=nulpoint;") == 8
    assert "insertpt:=nulvertex;" not in tests


def test_arc_basis_logging_uses_vector_formatter():
    sources = (compact(read_test_arc()), compact(read_test_arc2()))

    for source in sources:
        assert "functionvector3dtostr(constp:tzevector3d):string;" in source
        assert source.count("vector3dtostr(pa^.local.basis.") == 3
        assert "point3dtostr(pa^.local.basis." not in source


def test_rotation_centers_use_point_zero_constant():
    source = compact(read_rotate_logic())

    assert "rotationdata.center:=nulpoint;" in source
    assert "result:=nulpoint;" in source
    assert "rotationdata.center:=nulvertex;" not in source


def test_svg_ellipse_axis_is_converted_to_vector_for_length():
    source = compact(read_svg_block())

    assert "onevertexlength(ellipse^.majoraxis.asvector3d)" in source
    assert "onevertexlength(ellipse^.majoraxis)" not in source


def test_matrix_decomposition_keeps_basis_vectors_and_translation_point():
    section = procedure_section(read_model(), "decomposite", "setrot")

    assert "bx,by,bz:tzevector3d;" in section
    assert "t:tzepoint3d;" in section
    assert "bx:=mtr.mtr.v[0].slice;" in section
    assert "by:=mtr.mtr.v[1].slice;" in section
    assert "bz:=mtr.mtr.v[2].slice;" in section
    assert "t:=mtr.mtr.v[3].slice.aspoint3d;" in section
    assert "pzepoint3d(@mtr.mtr.v[" not in section


def test_rotation_recalculation_uses_basis_vectors():
    section = procedure_section(read_model(), "ReCalcFromObjMatrix", "CalcObjMatrix")

    assert "ox,tv:tzevector3d;" in section
    assert "ox:=getxffromz(local.basis.oz);" in section
    assert "tv:=local.basis.ox;" in section
    assert "tv:=tv*-1;" in section
    assert "scalardot(tv,ox)" in section


if __name__ == "__main__":
    test_point_fields_use_point_zero_constant()
    test_control_point_delta_uses_point_zero_constant()
    test_create_command_uses_point_zero_constant()
    test_pascal_acadtable_tests_use_point_zero_constant()
    test_arc_basis_logging_uses_vector_formatter()
    test_rotation_centers_use_point_zero_constant()
    test_svg_ellipse_axis_is_converted_to_vector_for_length()
    test_matrix_decomposition_keeps_basis_vectors_and_translation_point()
    test_rotation_recalculation_uses_basis_vectors()
    print("issue 1387 AcadTable geometry type contract checks passed")
