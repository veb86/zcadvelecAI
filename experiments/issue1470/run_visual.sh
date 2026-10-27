#!/usr/bin/env bash
# Headless OpenGL-проверка после make -C cad_source/zengine/tests meshdisplaytests.
set -euo pipefail
ROOT=$(cd "$(dirname "$0")/../.." && pwd)
cd "$ROOT"
OUT=${FPC_OUT:-/tmp/zcad-nodtests}
mkdir -p "$OUT/mesh-renders"
# shellcheck disable=SC2086
fpc -Mdelphi -dLCLnogui ${FPC_EXTRA_OPTS:-} -Fu"$OUT" -FU"$OUT" -FE"$OUT" \
  experiments/issue1470/capture_mesh.lpr > "$OUT/build-capture-mesh.log" 2>&1
"$OUT/capture_mesh" "$OUT/mesh-renders"
LIBGL_ALWAYS_SOFTWARE=1 LP_NUM_THREADS=1 \
  python3 experiments/issue1470/verify_render.py "$OUT/mesh-renders"
