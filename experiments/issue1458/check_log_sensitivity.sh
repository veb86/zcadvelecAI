#!/usr/bin/env bash
# issue #1458: проверка чувствительности nodstage8 — модули стилей
# uzestylestablesdxfnod/uzestylesmleaderdxfnod временно возвращаются к
# состоянию до этапа 8 (сообщения LM_Info в модуль лога по умолчанию),
# nodstage8 должен упасть; затем рабочие файлы восстанавливаются.
# Запуск из корня репозитория (рабочее дерево этих файлов должно быть чистым).
set -uo pipefail
ROOT=$(cd "$(dirname "$0")/../.." && pwd)
cd "$ROOT" || exit 1
BASE=${1:-751bdf55b}
FILES="cad_source/zengine/styles/uzestylestablesdxfnod.pas cad_source/zengine/styles/uzestylesmleaderdxfnod.pas"
# shellcheck disable=SC2086
git checkout "$BASE" -- $FILES || exit 1
cad_source/zengine/tests/nodtests.sh nodstage8 > /tmp/nodstage8-sensitivity.log 2>&1
rc=$?
# shellcheck disable=SC2086
git checkout HEAD -- $FILES
grep '^FAIL' /tmp/nodstage8-sensitivity.log
if [ $rc -ne 0 ]; then
  echo "sensitivity: OK (nodstage8 fails without the fix)"
else
  echo "sensitivity: FAILED (nodstage8 passes without the fix)"
  exit 1
fi
