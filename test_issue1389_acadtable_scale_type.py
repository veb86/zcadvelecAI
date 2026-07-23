#!/usr/bin/env python3
"""
Контракт компиляции масштаба AcadTable после разделения GPoint/GVector.

Масштаб задаёт компоненты преобразования и поэтому должен храниться как
вектор. Это позволяет без преобразований передавать его в геометрические
функции, принимающие выходной вектор масштаба и строящие матрицу масштаба.
"""

from pathlib import Path


ROOT = Path(__file__).resolve().parent
ACADTABLE_MODEL = (
    ROOT / "cad_source" / "zcad" / "velec" / "acadtable" / "uzeacadtable_model.pas"
)


def compact(text: str) -> str:
    return "".join(text.split()).lower()


def test_acadtable_scale_uses_vector_type():
    model = compact(ACADTABLE_MODEL.read_text(encoding="utf-8"))

    assert "fscale:tzevector3d;" in model
    assert "fscale:tzepoint3d;" not in model
    assert "fscale:=cv3d__1__1__1;" in model
    assert "getpointinocsbybasis(bx,by,bz,t,fscale)" in model
    assert "createscalematrix(fscale)" in model


if __name__ == "__main__":
    test_acadtable_scale_uses_vector_type()
    print("issue 1389 AcadTable scale type contract checks passed")
