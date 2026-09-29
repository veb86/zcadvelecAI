#!/usr/bin/env bash
# Тесты NOD (ТЗ cad_source/zengine/TZ_NOD_NamedObjectDictionary.md, этап 8):
# сборка cad_source/zengine/tests/nodstageN.lpr без IDE Lazarus (консольный
# fpc + исходники Lazarus, виджетсет nogui) и запуск с корнем репозитория.
# Используется целью nodtests в Makefile и в CI (.github/workflows/nodtests.yml).
#
# Требования (Debian/Ubuntu):
#   sudo apt-get install fpc lazarus-src
#   git submodule update --init --depth 1 cad_source/components/{zbaseutils,zcontainers,zmath,zreaders,zunits,zmacros,zundostack}
#
# Использование (из любого каталога):
#   cad_source/zengine/tests/nodtests.sh               # все nodstage*.lpr
#   cad_source/zengine/tests/nodtests.sh nodstage7     # выбранные тесты
#   HEAPTRC=1 cad_source/zengine/tests/nodtests.sh     # сборка с heaptrc (-gh)
#
# Переменные окружения:
#   LAZARUS_DIR    исходники Lazarus (по умолчанию /usr/lib/lazarus/<версия>)
#   FPC_OUT        каталог сборки (по умолчанию /tmp/zcad-nodtests)
#   FPC_EXTRA_OPTS дополнительные ключи fpc
#
# Код возврата: 0 — все тесты прошли, 1 — хотя бы один не собрался или упал.
# Логи сборки — $FPC_OUT/build-<тест>.log.
set -uo pipefail

ROOT=$(cd "$(dirname "$0")/../../.." && pwd)
cd "$ROOT" || exit 1

L=${LAZARUS_DIR:-}
if [ -z "$L" ]; then
  L=$(ls -d /usr/lib/lazarus/*/ 2>/dev/null | sort -V | tail -n 1)
  L=${L%/}
fi
if [ ! -d "$L/lcl" ]; then
  echo "nodtests: Lazarus sources not found (LAZARUS_DIR='$L')" >&2
  exit 1
fi
OUT=${FPC_OUT:-/tmp/zcad-nodtests}
EXTRA="${FPC_EXTRA_OPTS:-}"
if [ "${HEAPTRC:-0}" = "1" ]; then
  EXTRA="$EXTRA -gh -gl"
  OUT="$OUT-heaptrc"
fi
mkdir -p "$OUT"

if [ $# -gt 0 ]; then
  TESTS=("$@")
else
  TESTS=()
  for F in cad_source/zengine/tests/nodstage*.lpr; do
    TESTS+=("$(basename "$F" .lpr)")
  done
fi

DIRS=$(find cad_source/zengine cad_source/zcad \
  cad_source/components/zbaseutils cad_source/components/zcontainers \
  cad_source/components/zmath cad_source/components/zreaders \
  cad_source/components/zunits cad_source/components/zmacros \
  cad_source/components/zundostack -type d | sed 's/^/-Fu/' | tr '\n' ' ')
INC=$(find cad_source -name '*.inc' -exec dirname {} \; | sort -u | sed 's/^/-Fi/' | tr '\n' ' ')

FAILED=()
for T in "${TESTS[@]}"; do
  echo "=== $T"
  # shellcheck disable=SC2086
  if ! fpc -Mdelphi -dLCLnogui $EXTRA -FU"$OUT" -FE"$OUT" \
    -Fu$L/components/lazutils -Fi$L/components/lazutils \
    -Fu$L/ide/packages/ideconfig -Fi$L/ide/packages/ideconfig/include/linux \
    -Fi$L/ide/packages/ideconfig/include/unix \
    -Fu$L/components/buildintf -Fu$L/components/codetools -Fi$L/components/codetools \
    -Fu$L/components/freetype -Fu$L/components/lazcontrols \
    -Fu$L/lcl -Fu$L/lcl/widgetset -Fu$L/lcl/interfaces/nogui \
    -Fi$L/lcl/include -Fi$L/lcl/interfaces/nogui \
    $DIRS $INC "cad_source/zengine/tests/$T.lpr" > "$OUT/build-$T.log" 2>&1; then
    grep -E '(Error|Fatal):' "$OUT/build-$T.log"
    echo "$T: BUILD FAILED (log: $OUT/build-$T.log)"
    FAILED+=("$T")
    continue
  fi
  if ! "$OUT/$T" "$ROOT"; then
    echo "$T: FAILED"
    FAILED+=("$T")
  fi
done

echo
if [ ${#FAILED[@]} -eq 0 ]; then
  echo "nodtests: all ${#TESTS[@]} tests passed"
else
  echo "nodtests: failed ${#FAILED[@]} of ${#TESTS[@]}: ${FAILED[*]}"
  exit 1
fi
