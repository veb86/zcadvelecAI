#!/usr/bin/env bash
# Сборка произвольной программы (.lpr) тем же набором путей, что и nodtests.sh.
# Использование: experiments/issue1465/build.sh path/to/prog.lpr [outdir]
set -uo pipefail
ROOT=$(cd "$(dirname "$0")/../.." && pwd)
cd "$ROOT" || exit 1
L=${LAZARUS_DIR:-/tmp/fpcdl/root/usr/lib/lazarus/3.0}
OUT=${2:-/tmp/zcad-issue1465}
mkdir -p "$OUT"
DIRS=$(find cad_source/zengine cad_source/zcad \
  cad_source/components/zbaseutils cad_source/components/zcontainers \
  cad_source/components/zmath cad_source/components/zreaders \
  cad_source/components/zunits cad_source/components/zmacros \
  cad_source/components/zundostack cad_source/components/zscriptbase \
  cad_source/components/zscript cad_source/components/fphunspell \
  cad_source/components/zobjectinspector cad_source/components/zbaseutilsgui -type d | sed 's/^/-Fu/' | tr '\n' ' ')
INC=$(find cad_source -name '*.inc' -exec dirname {} \; | sort -u | sed 's/^/-Fi/' | tr '\n' ' ')
# Пакеты Lazarus без директивы режима (zscriptbase и др.) собираются
# отдельно в objfpc, как в их .lpk; основная сборка берёт готовые .ppu.
for U in cad_source/components/zscriptbase/src/*.pas \
  cad_source/components/zscript/src/varman.pas; do
  # shellcheck disable=SC2086
  fpc -Mobjfpc -Sh -FU"$OUT" $DIRS $INC \
    -Fu$L/components/lazutils -Fi$L/components/lazutils \
    "$U" > "$OUT/build-$(basename "$U" .pas).log" 2>&1 \
    || { grep -E '(Error|Fatal):' "$OUT/build-$(basename "$U" .pas).log"; exit 1; }
done
# shellcheck disable=SC2086
fpc -Mdelphi -dLCLnogui -FU"$OUT" -FE"$OUT" \
  -Fu$L/components/lazutils -Fi$L/components/lazutils \
  -Fu$L/ide/packages/ideconfig -Fi$L/ide/packages/ideconfig/include/linux \
  -Fi$L/ide/packages/ideconfig/include/unix \
  -Fu$L/components/buildintf -Fu$L/components/codetools -Fi$L/components/codetools \
  -Fu$L/components/freetype -Fu$L/components/lazcontrols \
  -Fu$L/components/anchordocking -Fu$L/components/synedit -Fi$L/components/synedit \
  -Fu$L/lcl -Fu$L/lcl/widgetset -Fu$L/lcl/interfaces/nogui \
  -Fi$L/lcl/include -Fi$L/lcl/interfaces/nogui \
  $DIRS $INC "$1" > "$OUT/build-$(basename "$1" .lpr).log" 2>&1
RC=$?
grep -E '(Error|Fatal):' "$OUT/build-$(basename "$1" .lpr).log"
exit $RC
