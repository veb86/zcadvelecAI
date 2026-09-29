#!/usr/bin/env bash
# issue #1459: проверка компиляции загрузчика DWG (uzedwgimport.pas) с
# подключённой фазой dwg-import.scan.nod (uzedwgnod) — теми же ключами fpc,
# что и nodtests.sh (nodstage9 модуль uzedwgimport не собирает).
# fpdwg (dwg.pp, dwgproc.pp) собирается отдельно в objfpc, как в fpdwg.lpk.
# Нужны подмодули nodtests и zobjectinspector, zbaseutilsgui, zscriptbase.
# Запуск из корня репозитория. Лог — $FPC_OUT/build-uzedwgimport.log.
set -uo pipefail
ROOT=$(cd "$(dirname "$0")/../.." && pwd)
cd "$ROOT" || exit 1
L=${LAZARUS_DIR:-$(ls -d /usr/lib/lazarus/*/ 2>/dev/null | sort -V | tail -n 1)}
L=${L%/}
OUT=${FPC_OUT:-/tmp/zcad-nodtests}-dwgimport
mkdir -p "$OUT"
if ! fpc -Mobjfpc -FU"$OUT" -Fucad_source/components/fpdwg \
  cad_source/components/fpdwg/uzedwghandle.pas \
  > "$OUT/build-fpdwg.log" 2>&1 ||
  ! fpc -Mobjfpc -FU"$OUT" -Fucad_source/components/fpdwg \
  cad_source/components/fpdwg/dwgproc.pp >> "$OUT/build-fpdwg.log" 2>&1; then
  grep -E '(Error|Fatal):' "$OUT/build-fpdwg.log"
  echo "fpdwg: BUILD FAILED"
  exit 1
fi
DIRS=$(find cad_source/zengine cad_source/zcad \
  cad_source/components/zbaseutils cad_source/components/zcontainers \
  cad_source/components/zmath cad_source/components/zreaders \
  cad_source/components/zunits cad_source/components/zmacros \
  cad_source/components/zundostack cad_source/components/zobjectinspector \
  cad_source/components/zbaseutilsgui cad_source/components/zscriptbase -type d | sed 's/^/-Fu/' | tr '\n' ' ')
INC=$(find cad_source -name '*.inc' -exec dirname {} \; | sort -u | sed 's/^/-Fi/' | tr '\n' ' ')
build_import() {
  # shellcheck disable=SC2086
  fpc -Mdelphi -dLCLnogui -FU"$OUT" -FE"$OUT" \
  -Fu$L/components/lazutils -Fi$L/components/lazutils \
  -Fu$L/ide/packages/ideconfig -Fi$L/ide/packages/ideconfig/include/linux \
  -Fi$L/ide/packages/ideconfig/include/unix \
  -Fu$L/components/buildintf -Fu$L/components/codetools -Fi$L/components/codetools \
  -Fu$L/components/freetype -Fu$L/components/lazcontrols \
  -Fu$L/lcl -Fu$L/lcl/widgetset -Fu$L/lcl/interfaces/nogui \
  -Fi$L/lcl/include -Fi$L/lcl/interfaces/nogui \
  $DIRS $INC cad_source/zengine/fileformats/dwg/uzedwgimport.pas \
  > "$OUT/build-uzedwgimport.log" 2>&1
}

# zscriptbase (подмодуль, нужен модулям zcad) без директивы режима
# рассчитан на objfpc с {$H+} (zscriptbase.lpk), а zcontainers — на delphi.
# Первая сборка компилирует zcontainers в delphi; если она падает на
# zscriptbase, его модуль собирается в objfpc и сборка повторяется.
build_zscriptbase() {
  # shellcheck disable=SC2046
  fpc -Mobjfpc -Sh -FU"$OUT" \
    $(find cad_source/components/zscriptbase cad_source/components/zbaseutils \
      cad_source/components/zcontainers cad_source/components/zunits -type d | sed 's/^/-Fu/') \
    -Fu"$L/components/lazutils" $INC \
    cad_source/components/zscriptbase/src/uzsbvarmandef.pas > "$OUT/build-zscriptbase.log" 2>&1
}

if build_import || { grep -q 'uzsbvarmandef' "$OUT/build-uzedwgimport.log" &&
     build_zscriptbase && build_import; }; then
  echo "uzedwgimport: OK"
else
  grep -E '(Error|Fatal):' "$OUT/build-uzedwgimport.log"
  echo "uzedwgimport: BUILD FAILED (log: $OUT/build-uzedwgimport.log)"
  exit 1
fi
