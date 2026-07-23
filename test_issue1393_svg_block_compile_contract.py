#!/usr/bin/env python3
"""
Контракт компиляции SVG-экспортера после разделения GPoint/GVector.

MajorAxis уже является вектором, а единичная матрица геометрического
движка называется cOneMatrix.
"""

from pathlib import Path


ROOT = Path(__file__).resolve().parent
SVG_BLOCK = (
    ROOT / "cad_source" / "zcad" / "velec" / "exporttosvg" / "uexpsvgblock.pas"
)


def compact(text: str) -> str:
    return "".join(text.split()).lower()


def test_ellipse_axis_length_uses_vector_directly():
    source = compact(SVG_BLOCK.read_text(encoding="utf-8"))

    assert "onevertexlength(ellipse^.majoraxis)" in source
    assert "ellipse^.majoraxis.asvector3d" not in source


def test_local_svg_exports_use_current_identity_matrix_constant():
    source = compact(SVG_BLOCK.read_text(encoding="utf-8"))

    assert source.count("ftransformer.setfromblockinsert(conematrix)") == 2
    assert "ftransformer.setfromblockinsert(onematrix)" not in source


if __name__ == "__main__":
    test_ellipse_axis_length_uses_vector_directly()
    test_local_svg_exports_use_current_identity_matrix_constant()
    print("issue 1393 SVG block compile contract checks passed")
