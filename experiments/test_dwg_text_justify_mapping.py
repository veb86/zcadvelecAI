#!/usr/bin/env python3
"""Regression check for DWG TEXT horizontal/vertical justification mapping."""

JT = [
    ["Left", "Center", "Right", "Left", "MiddleCenter", "Left"],
    [
        "BottomLeft",
        "BottomCenter",
        "BottomRight",
        "BottomLeft",
        "MiddleCenter",
        "BottomLeft",
    ],
    [
        "MiddleLeft",
        "MiddleCenter",
        "MiddleRight",
        "MiddleLeft",
        "MiddleCenter",
        "MiddleLeft",
    ],
    ["TopLeft", "TopCenter", "TopRight", "TopLeft", "MiddleCenter", "TopLeft"],
]


def dwg_text_justify(horiz, vert):
    if vert < 0 or vert > 3 or horiz < 0 or horiz > 5:
        return "TopLeft"
    return JT[vert][horiz]


def effective_insert(dataflags, horiz, vert, insert, align):
    if horiz != 0 or vert != 0:
        return align
    return insert


def test_baseline_and_bottom_rows_are_not_swapped():
    assert dwg_text_justify(0, 0) == "Left"
    assert dwg_text_justify(2, 0) == "Right"
    assert dwg_text_justify(0, 1) == "BottomLeft"
    assert dwg_text_justify(2, 1) == "BottomRight"


def test_center_and_middle_center_are_distinct():
    assert dwg_text_justify(1, 0) == "Center"
    assert dwg_text_justify(1, 2) == "MiddleCenter"
    assert dwg_text_justify(4, 0) == "MiddleCenter"
    assert dwg_text_justify(1, 0) != dwg_text_justify(1, 2)


def test_default_left_ignores_alignment_flag_with_zero_alignment_point():
    assert effective_insert(2, 0, 0, (11.0, 12.0, 13.0), (0.0, 0.0, 13.0)) == (
        11.0,
        12.0,
        13.0,
    )


def test_invalid_alignment_falls_back_to_top_left():
    assert dwg_text_justify(6, 0) == "TopLeft"
    assert dwg_text_justify(0, 5) == "TopLeft"


if __name__ == "__main__":
    test_baseline_and_bottom_rows_are_not_swapped()
    test_center_and_middle_center_are_distinct()
    test_default_left_ignores_alignment_flag_with_zero_alignment_point()
    test_invalid_alignment_falls_back_to_top_left()
    print("DWG TEXT justification mapping checks passed")
