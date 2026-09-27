#!/bin/sh
# Сборка и запуск регрессионного теста issue #1438.
# Модули zengine (zcontainers) требуют режима Delphi, модули fpdwg - objfpc,
# поэтому сначала собирается uzeffdxfsupport, затем сам тест.
DIR=$(cd "$(dirname "$0")" && pwd)
ROOT=$(cd "$DIR/../.." && pwd)
export OUT=${OUT:-/tmp/b1438}
MODE=delphi "$DIR/build_units.sh" \
  "$ROOT/cad_source/zengine/fileformats/uzeffdxfsupport.pas" || exit 1
MODE=objfpc "$DIR/build_units.sh" "$DIR/test_dwg_codepage.lpr" || exit 1
"$OUT/test_dwg_codepage"
