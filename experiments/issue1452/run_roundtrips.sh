#!/usr/bin/env bash
# issue #1452: прогон roundtrip1339/1381/1436 (issue #1339/#1381/#1436) на
# штатных DXF со стилями таблиц. Бинарники собираются
# experiments/issue1452/build_and_run.sh (для этих тестов нужны сабмодули
# zscript, fphunspell, zobjectinspector, zbaseutilsgui, ztoolbars и пути к
# ним в FPC_EXTRA_OPTS — см. README.md рядом).
# Использование: experiments/issue1452/run_roundtrips.sh <каталог для вывода>
set -uo pipefail
ROOT=$(cd "$(dirname "$0")/../.." && pwd)
cd "$ROOT"
OUT=${FPC_OUT:-/tmp/fpcout1450}
RES=${1:-/tmp/rt1452}
mkdir -p "$RES"
T2007=environment/runtimefiles/AllCPU-AllOS/common/cfg/components/savetemplate2007.dxf
rc=0
for f in +testtable.dxf +testtabletwotable.dxf tablestyleetalon.dxf tablerazdel.dxf; do
  b=$(basename "$f" .dxf | tr -d '+')
  "$OUT/roundtrip1339" "cad_source/test/$f" "$T2007" "$RES/1339_$b.dxf" > "$RES/1339_$b.log" 2>&1
  r=$?; echo "roundtrip1339 $f rc=$r"; [ $r -ne 0 ] && rc=1
done
for f in tablerazdel.dxf acadtablerazdel2007_1.dxf; do
  b=$(basename "$f" .dxf)
  "$OUT/roundtrip1381" "cad_source/test/$f" "$T2007" "$RES/1381_$b.dxf" > "$RES/1381_$b.log" 2>&1
  r=$?; echo "roundtrip1381 $f rc=$r"; [ $r -ne 0 ] && rc=1
done
"$OUT/roundtrip1436" "$T2007" "$RES/1436.dxf" > "$RES/1436.log" 2>&1
r=$?; echo "roundtrip1436 rc=$r"; [ $r -ne 0 ] && rc=1
exit $rc
