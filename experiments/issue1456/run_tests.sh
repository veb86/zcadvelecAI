#!/bin/sh
# issue #1456 — этап 7 ТЗ NOD: тесты nodstage0–nodstage7 и независимая
# проверка файлов, сохранённых nodstage7 (запуск из корня репозитория).
rc=0
for t in nodstage0 nodstage1 nodstage2 nodstage3 nodstage4 nodstage5 nodstage6 nodstage7; do
  experiments/issue1452/build_and_run.sh $t || { echo "$t FAILED"; rc=1; }
done
tmp=${TMPDIR:-/tmp}
python3 experiments/issue1450/dxfcheck.py "$tmp/nodstage7_2007.dxf" \
  "$tmp/nodstage7_2007_2007.dxf" "$tmp/nodstage7_2000.dxf"
exit $rc
