#!/usr/bin/env bash
# issue #1452: сборка и запуск тестов cad_source/zengine/tests/<prog>.lpr
# без IDE — обёртка над experiments/issue1450/build_and_run_nodstage.sh.
#
# Отличие: для roundtrip*/testacadtable_standalone модули zscriptbase
# (режим objfpc) тянут uzctnrtree из zcontainers, у которого нет директивы
# {$mode} (в пакете — delphi). Если он ещё не собран, fpc компилирует его в
# режиме objfpc и падает. Поэтому модули DELPHI_PRE_UNITS собираются заранее
# в режиме delphi, после чего вызывается сценарий этапа 4.
#
# Использование:
#   experiments/issue1452/build_and_run.sh nodstage5
#   experiments/issue1452/build_and_run.sh roundtrip1339
#   (для roundtrip*/testacadtable_standalone OBJFPC_UNITS подставляется сам;
#   нужны сабмодули zscriptbase и zscript)
set -euo pipefail

ROOT=$(cd "$(dirname "$0")/../.." && pwd)
cd "$ROOT"
PROG=${1:-nodstage5}
L=${LAZARUS_DIR:-/usr/lib/lazarus/3.0}
OUT=${FPC_OUT:-/tmp/fpcout1450}
[ "${HEAPTRC:-0}" = "1" ] && OUT="$OUT-heaptrc"
mkdir -p "$OUT"

case "$PROG" in
  roundtrip*|testacadtable_standalone)
    : "${OBJFPC_UNITS:=$(ls cad_source/components/zscriptbase/src/*.pas) cad_source/components/zscript/src/varman.pas}"
    export OBJFPC_UNITS
    # uzeEntTable использует Varman из сабмодуля zscript (режим objfpc):
    #   git submodule update --init --depth 1 cad_source/components/zscript
    export FPC_EXTRA_OPTS="${FPC_EXTRA_OPTS:-} -Fucad_source/components/zscript/src"
    PRE=${DELPHI_PRE_UNITS:-cad_source/components/zcontainers/src/uzctnrtree.pas}
    DIRS=$(find cad_source/zengine cad_source/zcad \
      cad_source/components/zbaseutils cad_source/components/zcontainers \
      cad_source/components/zmath cad_source/components/zreaders \
      cad_source/components/zunits cad_source/components/zmacros \
      cad_source/components/zundostack -type d | sed 's/^/-Fu/' | tr '\n' ' ')
    INC=$(find cad_source -name '*.inc' -exec dirname {} \; | sort -u | sed 's/^/-Fi/' | tr '\n' ' ')
    EXTRA="$FPC_EXTRA_OPTS"
    [ "${HEAPTRC:-0}" = "1" ] && EXTRA="$EXTRA -gh -gl"
    for U in $PRE; do
      # shellcheck disable=SC2086
      fpc -Mdelphi -dLCLnogui $EXTRA -FU"$OUT" -FE"$OUT" \
        -Fu$L/components/lazutils -Fi$L/components/lazutils \
        -Fu$L/lcl -Fu$L/lcl/widgetset -Fu$L/lcl/interfaces/nogui \
        -Fi$L/lcl/include -Fi$L/lcl/interfaces/nogui \
        $DIRS $INC "$U" > "$OUT/build-pre-$(basename "$U").log" 2>&1 \
        || { grep -E '(Error|Fatal):' "$OUT/build-pre-$(basename "$U").log"; exit 1; }
    done
    ;;
esac

exec experiments/issue1450/build_and_run_nodstage.sh "$@"
