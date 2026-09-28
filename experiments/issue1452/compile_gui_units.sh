#!/usr/bin/env bash
# issue #1452: проверка компиляции (без линковки) модулей GUI слоя zcad,
# изменённых на этапе 5 (освобождение удалённого стиля таблицы).
# Модули zscriptbase (режим objfpc) берутся из каталога сборки roundtrip*
# (experiments/issue1452/build_and_run.sh roundtrip1339).
#
# Использование:
#   experiments/issue1452/compile_gui_units.sh [модуль.pas ...]
set -euo pipefail

ROOT=$(cd "$(dirname "$0")/../.." && pwd)
cd "$ROOT"
L=${LAZARUS_DIR:-/usr/lib/lazarus/3.0}
PRE=${FPC_OUT:-/tmp/fpcout1450}
OUT=${GUI_OUT:-/tmp/fpcout1452gui}
UNITS=${*:-cad_source/zcad/gui/uzcui_tablestylemanager.pas cad_source/zcad/gui/forms/uzcftablestyles.pas}
rm -rf "$OUT"
mkdir -p "$OUT"
cp "$PRE"/*.ppu "$PRE"/*.o "$OUT"/ 2>/dev/null || true
DIRS=$(find cad_source/zengine cad_source/zcad cad_source/components cad_source/other \
  -type d -not -path '*/.git*' -not -path '*/test*' -not -path '*/example*' \
  | sed 's/^/-Fu/' | tr '\n' ' ')
INC=$(find cad_source -name '*.inc' -exec dirname {} \; | sort -u | sed 's/^/-Fi/' | tr '\n' ' ')
RC=0
for U in $UNITS; do
  # shellcheck disable=SC2086
  if fpc -Mdelphi -dLCLnogui -FU"$OUT" \
    -Fu$L/components/lazutils -Fi$L/components/lazutils \
    -Fu$L/ide/packages/ideconfig -Fi$L/ide/packages/ideconfig/include/linux \
    -Fi$L/ide/packages/ideconfig/include/unix \
    -Fu$L/components/codetools -Fi$L/components/codetools \
    -Fu$L/lcl -Fu$L/lcl/widgetset -Fu$L/lcl/interfaces/nogui \
    -Fi$L/lcl/include -Fi$L/lcl/interfaces/nogui \
    -Fu$L/components/lazcontrols -Fu$L/components/buildintf \
    -Fu$L/components/freetype -Fu$L/components/ideintf -Fu$L/components/synedit \
    $DIRS $INC "$U" > "$OUT/build-$(basename "$U").log" 2>&1; then
    echo "ok:   $U"
  else
    echo "FAIL: $U"
    grep -E '(Error|Fatal):' "$OUT/build-$(basename "$U").log" || true
    RC=1
  fi
done
exit $RC
