#!/usr/bin/env python3
"""
Контракт компиляции матриц переноса после разделения GPoint/GVector.

Центр вращения является точкой, но перенос к центру и обратно задаётся
вектором. Поэтому CreateTranslationMatrix должен получать ACenter.asVector.
"""

from pathlib import Path


ROOT = Path(__file__).resolve().parent
ROTATE_LOGIC = (
    ROOT / "cad_source" / "zcad" / "velec" / "rotatecontrol" / "uzvrotate_logic.pas"
)


def compact(text: str) -> str:
    return "".join(text.split()).lower()


def test_rotation_center_is_converted_to_translation_vectors():
    logic = compact(ROTATE_LOGIC.read_text(encoding="utf-8"))

    assert "createtranslationmatrix(-acenter.asvector)" in logic
    assert "createtranslationmatrix(acenter.asvector)" in logic
    assert "createtranslationmatrix(createvertex(-acenter.x,-acenter.y,-acenter.z))" not in logic
    assert "createtranslationmatrix(acenter)" not in logic


if __name__ == "__main__":
    test_rotation_center_is_converted_to_translation_vectors()
    print("issue 1391 rotate translation type contract checks passed")
