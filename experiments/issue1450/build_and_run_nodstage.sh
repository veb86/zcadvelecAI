#!/usr/bin/env bash
# issue #1450: сборка и запуск тестов NOD cad_source/zengine/tests/nodstageN.lpr
# (ТЗ TZ_NOD_NamedObjectDictionary.md) без IDE Lazarus
# (консольный fpc + исходники Lazarus 3.0, виджетсет nogui).
#
# Требования (Debian/Ubuntu):
#   sudo apt-get install fpc lazarus-src
#   git submodule update --init --depth 1 cad_source/components/{zbaseutils,zcontainers,zmath,zreaders,zunits,zmacros,zundostack}
#
# Использование:
#   experiments/issue1450/build_and_run_nodstage.sh nodstage4 [аргументы теста]
#   OBJFPC_UNITS="$(ls cad_source/components/zscriptbase/src/*.pas)" \
#     experiments/issue1450/build_and_run_nodstage.sh testacadtable_standalone
#   (roundtrip*/testacadtable_standalone: нужен сабмодуль zscriptbase; его
#   модули без директивы {$mode} — режим objfpc, поэтому они собираются
#   заранее отдельным вызовом fpc)
#   HEAPTRC=1 experiments/issue1450/build_and_run_nodstage.sh nodstage4 # сборка с heaptrc (-gh)
set -euo pipefail

ROOT=$(cd "$(dirname "$0")/../.." && pwd)
cd "$ROOT"
PROG=${1:-nodstage4}
shift || true
L=${LAZARUS_DIR:-/usr/lib/lazarus/3.0}
OUT=${FPC_OUT:-/tmp/fpcout1450}
EXTRA="${FPC_EXTRA_OPTS:-}"
if [ "${HEAPTRC:-0}" = "1" ]; then
  EXTRA="$EXTRA -gh -gl"
  OUT="$OUT-heaptrc"
fi

mkdir -p "$OUT"
DIRS=$(find cad_source/zengine cad_source/zcad \
  cad_source/components/zbaseutils cad_source/components/zcontainers \
  cad_source/components/zmath cad_source/components/zreaders \
  cad_source/components/zunits cad_source/components/zmacros \
  cad_source/components/zundostack -type d | sed 's/^/-Fu/' | tr '\n' ' ')
INC=$(find cad_source -name '*.inc' -exec dirname {} \; | sort -u | sed 's/^/-Fi/' | tr '\n' ' ')

FPCOPTS="-dLCLnogui $EXTRA -FU$OUT -FE$OUT \
  -Fu$L/components/lazutils -Fi$L/components/lazutils \
  -Fu$L/ide/packages/ideconfig -Fi$L/ide/packages/ideconfig/include/linux \
  -Fi$L/ide/packages/ideconfig/include/unix \
  -Fu$L/components/buildintf -Fu$L/components/codetools -Fi$L/components/codetools \
  -Fu$L/components/freetype -Fu$L/components/lazcontrols \
  -Fu$L/lcl -Fu$L/lcl/widgetset -Fu$L/lcl/interfaces/nogui \
  -Fi$L/lcl/include -Fi$L/lcl/interfaces/nogui"

for U in ${OBJFPC_UNITS:-}; do
  # shellcheck disable=SC2086
  fpc -Mobjfpc -Sh $FPCOPTS -Fu"$(dirname "$U")" -Fu"$(dirname "$U")/src" $DIRS $INC "$U" \
    > "$OUT/build-$(basename "$U").log" 2>&1 \
    || { grep -E '(Error|Fatal):' "$OUT/build-$(basename "$U").log"; exit 1; }
done

# shellcheck disable=SC2086
fpc -Mdelphi -dLCLnogui $EXTRA -FU"$OUT" -FE"$OUT" \
  -Fu$L/components/lazutils -Fi$L/components/lazutils \
  -Fu$L/ide/packages/ideconfig -Fi$L/ide/packages/ideconfig/include/linux \
  -Fi$L/ide/packages/ideconfig/include/unix \
  -Fu$L/components/buildintf -Fu$L/components/codetools -Fi$L/components/codetools \
  -Fu$L/components/freetype -Fu$L/components/lazcontrols \
  -Fu$L/lcl -Fu$L/lcl/widgetset -Fu$L/lcl/interfaces/nogui \
  -Fi$L/lcl/include -Fi$L/lcl/interfaces/nogui \
  $DIRS $INC "cad_source/zengine/tests/$PROG.lpr" > "$OUT/build-$PROG.log" 2>&1 \
  || { grep -E '(Error|Fatal):' "$OUT/build-$PROG.log"; exit 1; }

"$OUT/$PROG" "$ROOT" "$@"
