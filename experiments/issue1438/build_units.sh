#!/bin/sh
# Компиляция отдельных модулей ZCAD через fpc (без Lazarus) для проверки
# синтаксиса и типов. Использование: build_units.sh <unit.pas> [...]
ROOT=$(cd "$(dirname "$0")/../.." && pwd)
OUT=${OUT:-/tmp/b1438}
mkdir -p "$OUT"
DIRS=$(find "$ROOT/cad_source" -type f \( -name "*.pas" -o -name "*.pp" -o -name "*.inc" \) \
  -not -path "*/trash/*" -not -path "*/fpspreadsheet/*" -printf "%h\n" | sort -u)
ARGS="-dLCL -dLCLnogui -Fu/usr/lib/lazarus/3.0/components/buildintf -Fi/usr/lib/lazarus/3.0/components/buildintf -Fu/usr/lib/lazarus/3.0/ide/packages/ideconfig -Fi/usr/lib/lazarus/3.0/ide/packages/ideconfig -Fi/usr/lib/lazarus/3.0/ide/packages/ideconfig/include/linux -Fi/usr/lib/lazarus/3.0/ide/packages/ideconfig/include -Fi/usr/lib/lazarus/3.0/ide/include"
# Скомпилированные модули Lazarus (LCL nogui, LazUtils, IDE-пакеты)
for d in $(find /usr/lib/lazarus/3.0 -name "*.ppu" -not -path "*gtk*" -not -path "*qt*" \
  -printf "%h\n" | sort -u); do ARGS="$ARGS -Fu$d"; done
for d in $DIRS; do ARGS="$ARGS -Fu$d -Fi$d"; done
for u in "$@"; do
  fpc -M${MODE:-delphi} -Sh -FU"$OUT" -FE"$OUT" $ARGS "$u" || exit 1
done
