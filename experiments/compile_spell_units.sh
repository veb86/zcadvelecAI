#!/usr/bin/env bash
# Сборка experiments/compile_spell_units.lpr (или указанного .lpr) консольным fpc
# с исходниками Lazarus (nogui), по образцу cad_source/zengine/tests/nodtests.sh.
# LAZARUS_DIR — исходники Lazarus.
set -uo pipefail
ROOT=$(cd "$(dirname "$0")/.." && pwd)
cd "$ROOT" || exit 1
L=${LAZARUS_DIR:?set LAZARUS_DIR}
OUT=${FPC_OUT:-/tmp/spellbuild}
mkdir -p "$OUT"
SRC=${1:-experiments/compile_spell_units.lpr}
DIRS=$(find cad_source/zengine cad_source/zcad \
  cad_source/components/zbaseutils cad_source/components/zcontainers \
  cad_source/components/zmath cad_source/components/zreaders \
  cad_source/components/zunits cad_source/components/zmacros \
  cad_source/components/zundostack cad_source/components/fphunspell cad_source/components/zscriptbase cad_source/components/zscript cad_source/components/zobjectinspector cad_source/components/zbaseutilsgui cad_source/components/ztoolbars -type d | sed 's/^/-Fu/' | tr '\n' ' ')
INC=$(find cad_source -name '*.inc' -exec dirname {} \; | sort -u | sed 's/^/-Fi/' | tr '\n' ' ')
LAZ="-Fu$L/components/lazutils -Fi$L/components/lazutils \
  -Fu$L/ide/packages/ideconfig -Fi$L/ide/packages/ideconfig/include/linux \
  -Fi$L/ide/packages/ideconfig/include/unix \
  -Fu$L/components/buildintf -Fu$L/components/codetools -Fi$L/components/codetools \
  -Fu$L/components/freetype -Fu$L/components/lazcontrols \
  -Fu$L/lcl -Fu$L/lcl/widgetset -Fu$L/lcl/interfaces/nogui \
  -Fi$L/lcl/include -Fi$L/lcl/interfaces/nogui -Fu$L/packager/registration -Fu$L/components/anchordocking ${LAZ_EXTRA:-}"
# Предварительная сборка пакетов в их режиме: элементы PREBUILD вида
# objfpc:файл или delphi:файл (пакеты без директивы режима в файлах)
for ITEM in ${PREBUILD:-}; do
  MODE=${ITEM%%:*}
  PKG=${ITEM#*:}
  if [ "$MODE" = objfpc ]; then
    MODEOPTS="-Mobjfpc -Sh -Sm"
  else
    MODEOPTS="-Mdelphi"
  fi
  # shellcheck disable=SC2086
  fpc $MODEOPTS -dLCLnogui -FU"$OUT" $LAZ $DIRS $INC "$PKG" || exit 1
done
# shellcheck disable=SC2086
fpc -Mdelphi -dLCLnogui -FU"$OUT" -FE"$OUT" $LAZ $DIRS $INC "$SRC"
