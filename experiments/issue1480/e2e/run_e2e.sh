#!/bin/bash
# Сквозная проверка #1480 на собранном ZCAD под Xvfb (Linux).
# Использование: run_e2e.sh <каталог ZCAD cad> <рабочий каталог> [node]
# ZCAD запускается со сценарием DWGNew + HTTPStart, затем e2e_check.mjs
# чертит зоны через HTTP и выгружает чертёж в <work>/exports/zones.dxf,
# а verify_dxf.py проверяет слой, цвет, вершины и переменные зон.
set -u
CAD=$1; WORK=$2; NODE=${3:-node}
HERE=$(cd "$(dirname "$0")" && pwd)
PORT=5000
mkdir -p "$WORK"
Xvfb :99 -screen 0 1280x1024x24 >"$WORK/xvfb.log" 2>&1 &
XVFB=$!
export DISPLAY=:99
# Без оконного менеджера GTK2 под Xvfb зацикливается во время заставки
if command -v openbox >/dev/null; then sleep 1; openbox >"$WORK/wm.log" 2>&1 & fi
export ZCAD_EXPORT_DIR="$WORK/exports" ZCAD_EXPORT_TIMEOUT=60
status=0
printf '%s\n' DWGNew HTTPStart > "$WORK/space.cmd"
"$CAD/bin/zcad" nosplash notcheckuniqueinstance noloadlayout \
  runscript "$WORK/space.cmd" >"$WORK/zcad.log" 2>&1 &
PID=$!
"$NODE" "$HERE/e2e_check.mjs" "$PORT" "$WORK" draw || status=1
sleep 2
import -window root "$WORK/zones.png" 2>/dev/null
# Одна команда Undo в командной строке ZCAD (нужен xdotool): пакет
# END_BATCH (помещения 102 и 103) должен отмениться одним шагом
if [ $status -eq 0 ] && command -v xdotool >/dev/null; then
  WIN=$(xdotool search --onlyvisible --name '^Command line$' | head -1)
  xdotool windowactivate --sync "$WIN"; sleep 1
  xdotool type --delay 50 'Undo'; xdotool key Return; sleep 2
  import -window root "$WORK/undo.png" 2>/dev/null
  "$NODE" "$HERE/e2e_check.mjs" "$PORT" "$WORK" undo || status=1
fi
kill $PID 2>/dev/null; sleep 1; kill -9 $PID 2>/dev/null
wait $PID 2>/dev/null
cp /tmp/zcad.log "$WORK/zcad.internal.log" 2>/dev/null
kill $XVFB
[ $status -eq 0 ] && { python3 -I "$HERE/verify_dxf.py" "$WORK/exports/zones.dxf" || status=1; }
exit $status
