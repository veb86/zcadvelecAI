#!/usr/bin/env python3
"""Simulate the fixed Pascal TransformHandlerVertices text-height logic,
given the PUSH_MATRIX data from spdstable.dxf.

ZCAD matrix layout: mtr.v[Row].v[Col], translation lives in row 3
(i.e., mtr.v[3].v[0..2]). The Pascal parser transposes the raw
row-major DXF matrix into that layout:
    mtr.v[Row].v[Col] := data[Col * 4 + Row]

ZCAD treats vectors as row vectors multiplied from the left:
    result.v[c] = sum_r vec.v[r] * mat.v[r].v[c]

Expected: height 2.5 -> 250 after applying matrix(100x scale).
"""
import math


def main():
    # Raw row-major data from the DXF proxy stream (decode_pushmatrix.py).
    raw = [
        [100.0, 0.0, 0.0, -2361.082798995747],
        [0.0, 100.0, 0.0, 1985.7327784934605],
        [0.0, 0.0, 100.0, 0.0],
        [0.0, 0.0, 0.0, 1.0],
    ]
    # Transpose the same way HandlePushMatrix does: translation moves to row 3.
    mat = [[raw[c][r] for c in range(4)] for r in range(4)]
    # Zero the translation (row 3) to get the linear part.
    lin = [row[:] for row in mat]
    lin[3] = [0.0, 0.0, 0.0, 1.0]
    # Apply as row-vector multiply (ZCAD convention).
    h = 2.5
    vec = [0.0, h, 0.0, 1.0]
    tv = [sum(vec[r] * lin[r][c] for r in range(4)) for c in range(4)]
    new_h = math.sqrt(tv[0] ** 2 + tv[1] ** 2 + tv[2] ** 2)
    print(f'Height before: {h}')
    print(f'Height after:  {new_h}')
    print(f'Expected:      250')
    assert abs(new_h - 250.0) < 1e-6, 'Expected 250 after 100x scale'
    print('OK')


if __name__ == '__main__':
    main()
