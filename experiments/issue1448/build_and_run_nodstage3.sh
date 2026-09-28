#!/usr/bin/env bash
# issue #1448: сборка и запуск cad_source/zengine/tests/nodstage3.lpr
# (этап 2 ТЗ TZ_NOD_NamedObjectDictionary.md) без IDE Lazarus
# (консольный fpc + исходники Lazarus 3.0, виджетсет nogui).
#
# Требования (Debian/Ubuntu):
#   sudo apt-get install fpc lazarus-src
#   git submodule update --init --depth 1 cad_source/components/{zbaseutils,zcontainers,zmath,zreaders,zunits,zmacros,zundostack}
#
# Использование:
#   experiments/issue1448/build_and_run_nodstage3.sh           # проверка
#   HEAPTRC=1 experiments/issue1448/build_and_run_nodstage3.sh # сборка с heaptrc (-gh)
set -euo pipefail

ROOT=$(cd "$(dirname "$0")/../.." && pwd)
cd "$ROOT"
L=${LAZARUS_DIR:-/usr/lib/lazarus/3.0}
OUT=${FPC_OUT:-/tmp/fpcout1448}
EXTRA=""
if [ "${HEAPTRC:-0}" = "1" ]; then
  EXTRA="-gh -gl"
  OUT="$OUT-heaptrc"
fi

mkdir -p "$OUT"
DIRS=$(find cad_source/zengine cad_source/zcad \
  cad_source/components/zbaseutils cad_source/components/zcontainers \
  cad_source/components/zmath cad_source/components/zreaders \
  cad_source/components/zunits cad_source/components/zmacros \
  cad_source/components/zundostack -type d | sed 's/^/-Fu/' | tr '\n' ' ')
INC=$(find cad_source -name '*.inc' -exec dirname {} \; | sort -u | sed 's/^/-Fi/' | tr '\n' ' ')

# shellcheck disable=SC2086
fpc -Mdelphi -dLCLnogui $EXTRA -FU"$OUT" -FE"$OUT" \
  -Fu$L/components/lazutils -Fi$L/components/lazutils \
  -Fu$L/ide/packages/ideconfig -Fi$L/ide/packages/ideconfig/include/linux \
  -Fi$L/ide/packages/ideconfig/include/unix \
  -Fu$L/components/buildintf -Fu$L/components/codetools -Fi$L/components/codetools \
  -Fu$L/components/freetype -Fu$L/components/lazcontrols \
  -Fu$L/lcl -Fu$L/lcl/widgetset -Fu$L/lcl/interfaces/nogui \
  -Fi$L/lcl/include -Fi$L/lcl/interfaces/nogui \
  $DIRS $INC cad_source/zengine/tests/nodstage3.lpr > "$OUT/build.log" 2>&1 \
  || { grep -E '(Error|Fatal):' "$OUT/build.log"; exit 1; }

"$OUT/nodstage3" "$ROOT" "$@"
