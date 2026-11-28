#!/bin/bash
# Диагностика: запуск ZCAD под Xvfb и прогон probe.mjs с файлом шагов.
# probe.sh <каталог cad> <steps.json> <рабочий каталог>
set -u
CAD=$1; STEPS=$2; WORK=$3
HERE=$(cd "$(dirname "$0")" && pwd)
mkdir -p "$WORK/exports"
export ZCAD_EXPORT_DIR="${ZCAD_EXPORT_DIR:-$WORK/exports}"
Xvfb :98 -screen 0 1280x1024x24 >"$WORK/xvfb.log" 2>&1 &
XVFB=$!
export DISPLAY=:98
if command -v openbox >/dev/null; then sleep 1; openbox >"$WORK/wm.log" 2>&1 & fi
printf '%s\n' DWGNew HTTPStart > "$WORK/probe.cmd"
"$CAD/bin/zcad" nosplash notcheckuniqueinstance noloadlayout \
  runscript "$WORK/probe.cmd" >"$WORK/zcad.log" 2>&1 &
PID=$!
if [ "${STRESS:-}" ]; then node "$HERE/stress.mjs" 5000 $STRESS; else node "$HERE/probe.mjs" 5000 "$STEPS"; fi; status=$?
kill $PID 2>/dev/null; sleep 1; kill -9 $PID 2>/dev/null; wait $PID 2>/dev/null
cp /tmp/zcad.log "$WORK/zcad.internal.log" 2>/dev/null
kill $XVFB
exit $status
