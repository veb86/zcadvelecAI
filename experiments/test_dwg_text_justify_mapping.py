#!/usr/bin/env python3
"""Regression check for DWG TEXT horizontal/vertical justification mapping."""

JT = [
    ["BottomLeft", "BottomCenter", "BottomRight", "BottomLeft", "MiddleCenter"],
    ["Left", "Center", "Right", "BottomLeft", "BottomLeft"],
    ["MiddleLeft", "MiddleCenter", "MiddleRight", "BottomLeft", "BottomLeft"],
    ["TopLeft", "TopCenter", "TopRight", "BottomLeft", "BottomLeft"],
]


def dwg_text_justify(horiz, vert):
    if vert < 0 or vert > 3 or horiz < 0 or horiz > 4:
        return "TopLeft"
    return JT[vert][horiz]


def test_baseline_left_imports_as_bottom_left():
    assert dwg_text_justify(0, 0) == "BottomLeft"


def test_invalid_alignment_falls_back_to_top_left():
    assert dwg_text_justify(6, 0) == "TopLeft"
    assert dwg_text_justify(0, 5) == "TopLeft"


if __name__ == "__main__":
    test_baseline_left_imports_as_bottom_left()
    test_invalid_alignment_falls_back_to_top_left()
    print("DWG TEXT justification mapping checks passed")
