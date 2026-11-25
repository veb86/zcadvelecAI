#!/bin/bash
# Сквозная проверка #1478 на собранном ZCAD под Xvfb (Linux, Docker).
# Использование (внутри контейнера с ZCAD, Xvfb и Node.js):
#   run_e2e.sh <каталог ZCAD cad> <пример DXF> <рабочий каталог> <node>
# Три запуска ZCAD с разными сценариями запуска:
#   nodoc - новый чертёж закрыт после HTTPStart, nosel - без выделения,
#   sel - SelectAll. HTTPStart зарегистрирована с CADWG и без открытого
#   чертежа не выполняется, поэтому NO_DOCUMENT получаем через DWGClose.
#   DWGClose даже для нового чертежа показывает модальный вопрос
#   «Drawing "Unnamed001" not saved. Save?»; пока он открыт, очередь IPC
#   обслуживается и выгрузка видит пустой чертёж (EMPTY_SELECTION). Через
#   xdotool нажимаем «No» (Alt+N), после чего чертежей нет -> NO_DOCUMENT.
set -u
CAD=$1; SAMPLE=$2; WORK=$3; NODE=${4:-node}
HERE=$(cd "$(dirname "$0")" && pwd)
PORT=5000
mkdir -p "$WORK"
Xvfb :99 -screen 0 1280x1024x24 >"$WORK/xvfb.log" 2>&1 &
XVFB=$!
export DISPLAY=:99
# Без оконного менеджера GTK2 под Xvfb зацикливается на событиях configure
# во время заставки; openbox достаточно (если установлен).
if command -v openbox >/dev/null; then sleep 1; openbox >"$WORK/wm.log" 2>&1 & fi
export ZCAD_EXPORT_DIR="$WORK/exports" ZCAD_EXPORT_TIMEOUT=60
status=0

# Отвечает «No» на вопрос о сохранении при DWGClose (окно с заголовком ZCAD)
answer_save_prompt() {
  local win
  for _ in $(seq 240); do
    win=$(xdotool search --onlyvisible --name '^ZCAD$' 2>/dev/null | head -1)
    if [ -n "$win" ]; then
      sleep 5
      import -window root "$WORK/nodoc-save-prompt.png" 2>/dev/null
      xdotool windowactivate --sync "$win" key alt+n
      return
    fi
    sleep 1
  done
}

# Запускает ZCAD со сценарием, ждёт HTTP и выполняет проверки фазы
run_phase() {
  local phase=$1; shift
  printf '%s\n' "$@" > "$WORK/$phase.cmd"
  "$CAD/bin/zcad" nosplash notcheckuniqueinstance noloadlayout \
    runscript "$WORK/$phase.cmd" >"$WORK/zcad-$phase.log" 2>&1 &
  local pid=$!
  [ "$phase" = nodoc ] && command -v xdotool >/dev/null && answer_save_prompt &
  "$NODE" "$HERE/e2e_check.mjs" "$phase" "$PORT" "$WORK" || status=1
  kill $pid 2>/dev/null; sleep 1; kill -9 $pid 2>/dev/null
  wait $pid 2>/dev/null
  cp /tmp/zcad.log "$WORK/zcad-$phase.internal.log" 2>/dev/null
}

run_phase nodoc DWGNew HTTPStart DWGClose
run_phase nosel "Load($SAMPLE)" HTTPStart
run_phase sel "Load($SAMPLE)" SelectAll HTTPStart
kill $XVFB
exit $status
