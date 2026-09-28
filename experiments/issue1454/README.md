# issue #1454 — этап 6 ТЗ NOD: вспомогательные сценарии

Все сценарии запускаются из корня репозитория. Сборка без IDE — сценарии
этапа 5 (`experiments/issue1452/build_and_run.sh`, см.
`experiments/issue1452/README.md`).

| Файл | Назначение |
|------|------------|
| `mlsdump1454.py <файл.dxf>` | Независимый (без zengine) дамп стилей `MLEADERSTYLE` в формате эталонов `cad_source/zengine/tests/data/nod/stage6/*.txt` — сверка эталонов `nodstage6` и файлов после round-trip. |

## Тесты этапа

```sh
for t in nodstage0 nodstage1 nodstage2 nodstage3 nodstage4 nodstage5 nodstage6; do
  experiments/issue1452/build_and_run.sh $t || echo "$t FAILED"
done
```

`nodstage6 <корень> --update` перезаписывает эталоны
`data/nod/stage6/*.txt` (только при осознанном изменении загрузчика).

## Сверка эталонов

```sh
for f in +mleader2008 mleaderblock mleader2007notwork mleader2000notwork; do
  python3 experiments/issue1454/mlsdump1454.py "cad_source/test/$f.dxf" \
    | diff - "cad_source/zengine/tests/data/nod/stage6/$f.txt" && echo "$f OK"
done
```

Файлы round-trip `nodstage6` остаются во временном каталоге
(`/tmp/nodstage6_*.dxf`); их дамп совпадает с эталоном после удаления
хэндлов:

```sh
strip() { sed -E 's/[0-9A-Fa-f]+:([^ ]*)/:\1/g; s/xdict=[0-9A-Fa-f]*/xdict=/'; }
python3 experiments/issue1454/mlsdump1454.py /tmp/nodstage6_+mleader2008.dxf | strip \
  | diff - <(strip < cad_source/zengine/tests/data/nod/stage6/+mleader2008.txt)
```

Результат: все 4 эталона совпадают с независимым дампом; файлы round-trip
совпадают с точностью до хэндлов. `experiments/issue1450/dxfcheck.py` на
файлах round-trip: единственная неразрешённая ссылка `331:94` — из
шаблона `savetemplate2007.dxf` (есть и в пустом чертеже, была до этапа 6).
