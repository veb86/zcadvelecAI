#!/usr/bin/env bash
# issue #1436: сборка и запуск cad_source/zengine/tests/roundtrip1436.lpr
# без IDE Lazarus (консольный fpc + исходники Lazarus 3.0, виджетсет nogui).
#
# Требования (Debian/Ubuntu):
#   sudo apt-get install fpc lazarus-src
#   git submodule update --init --depth 1 cad_source/components/{zbaseutils,zcontainers,zmath,zreaders,zunits,zmacros,zundostack}
#
# Использование: experiments/issue1436/build_and_run_roundtrip1436.sh [out.dxf]
set -euo pipefail

ROOT=$(cd "$(dirname "$0")/../.." && pwd)
cd "$ROOT"
L=${LAZARUS_DIR:-/usr/lib/lazarus/3.0}
OUT=${FPC_OUT:-/tmp/fpcout1436}
DXF=${1:-/tmp/roundtrip1436.dxf}
TEMPLATE=environment/runtimefiles/AllCPU-AllOS/common/cfg/components/savetemplate2007.dxf

mkdir -p "$OUT"
DIRS=$(find cad_source/zengine cad_source/zcad \
  cad_source/components/zbaseutils cad_source/components/zcontainers \
  cad_source/components/zmath cad_source/components/zreaders \
  cad_source/components/zunits cad_source/components/zmacros \
  cad_source/components/zundostack -type d | sed 's/^/-Fu/' | tr '\n' ' ')
INC=$(find cad_source -name '*.inc' -exec dirname {} \; | sort -u | sed 's/^/-Fi/' | tr '\n' ' ')

# shellcheck disable=SC2086
fpc -Mdelphi -dLCLnogui -FU"$OUT" -FE"$OUT" \
  -Fu$L/components/lazutils -Fi$L/components/lazutils \
  -Fu$L/ide/packages/ideconfig -Fi$L/ide/packages/ideconfig/include/linux \
  -Fi$L/ide/packages/ideconfig/include/unix \
  -Fu$L/components/buildintf -Fu$L/components/codetools -Fi$L/components/codetools \
  -Fu$L/components/freetype -Fu$L/components/lazcontrols \
  -Fu$L/lcl -Fu$L/lcl/widgetset -Fu$L/lcl/interfaces/nogui \
  -Fi$L/lcl/include -Fi$L/lcl/interfaces/nogui \
  $DIRS $INC cad_source/zengine/tests/roundtrip1436.lpr > "$OUT/build.log" 2>&1 \
  || { grep -E '(Error|Fatal):' "$OUT/build.log"; exit 1; }

"$OUT/roundtrip1436" "$TEMPLATE" "$DXF"
