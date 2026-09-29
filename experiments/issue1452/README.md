# issue #1452 — этап 5 ТЗ NOD: вспомогательные сценарии

Все сценарии запускаются из корня репозитория. Сборка без IDE (fpc +
Lazarus 3.0 в `/usr/lib/lazarus/3.0`, каталог сборки `/tmp/fpcout1450`).

| Файл | Назначение |
|------|------------|
| `build_and_run.sh <prog>` | Собирает `cad_source/zengine/tests/<prog>.lpr` и запускает с корнем репозитория. Обёртка над `experiments/issue1450/build_and_run_nodstage.sh`; для `roundtrip*` модули zscriptbase собираются в нужном режиме. |
| `run_roundtrips.sh <каталог>` | Прогон `roundtrip1339`/`roundtrip1381`/`roundtrip1436` на штатных DXF со стилями таблиц (бинарники собрать заранее через `build_and_run.sh`). |
| `compile_gui_units.sh [модули]` | Проверка компиляции (без линковки) модулей GUI `uzcui_tablestylemanager` и `uzcftablestyles` (освобождение удалённого стиля таблицы). Нужны ppu из сборки `roundtrip1339`. |
| `tsequiv1452.lpr` | Сравнение прежнего разбора сырого текста OBJECTS (`ReadTableStylesFromDXFObjects`) с загрузкой через NOD (`LoadTableStylesFromNOD`). Собирается только на коммите до удаления прежнего разбора (`c29dc8732`): `cp experiments/issue1452/tsequiv1452.lpr cad_source/zengine/tests/ && experiments/issue1452/build_and_run.sh tsequiv1452 <outdir> <файлы dxf…>`. |

## Тесты этапа

```sh
for t in nodstage0 nodstage1 nodstage2 nodstage3 nodstage4 nodstage5; do
  experiments/issue1452/build_and_run.sh $t || echo "$t FAILED"
done
```

## Round-trip

Для `roundtrip*` нужны сабмодули zscript, fphunspell, zobjectinspector,
zbaseutilsgui, ztoolbars и пути к ним:

```sh
export FPC_EXTRA_OPTS="-Fucad_source/components/fphunspell \
  -Fucad_source/components/fphunspell/src \
  -Fucad_source/components/fphunspell/dll \
  -Fucad_source/components/fphunspell/src/inc \
  -Fucad_source/components/zobjectinspector \
  -Fucad_source/components/zobjectinspector/src \
  -Fucad_source/components/zobjectinspector/samples \
  -Fucad_source/components/zobjectinspector/samples/objinspbasic \
  -Fucad_source/components/zobjectinspector/samples/objinsprtti \
  -Fucad_source/components/zbaseutilsgui \
  -Fucad_source/components/zbaseutilsgui/src \
  -Fucad_source/components/zbaseutilsgui/examples \
  -Fucad_source/components/zbaseutilsgui/examples/simpleapp \
  -Fucad_source/components/ztoolbars \
  -Fucad_source/components/ztoolbars/src \
  -Fucad_source/components/ztoolbars/examples \
  -Fucad_source/components/ztoolbars/examples/appwithtoolbars \
  -Fucad_source/components/ztoolbars/examples/dockedappwithtoolbars \
  -Fucad_source/components/ztoolbars/examples/appwithtoolbars/bin \
  -Fucad_source/components/ztoolbars/examples/dockedappwithtoolbars/bin \
  -Fu/usr/lib/lazarus/3.0/components/anchordocking \
  -Fu/usr/lib/lazarus/3.0/components/lazcontrols \
  -Fu/usr/lib/lazarus/3.0/components/ideintf \
  -Fu/usr/lib/lazarus/3.0/components/synedit \
  -Fi/usr/lib/lazarus/3.0/components/synedit"
for p in roundtrip1339 roundtrip1381 roundtrip1436; do experiments/issue1452/build_and_run.sh $p; done
experiments/issue1452/run_roundtrips.sh /tmp/rt1452
python3 experiments/issue1450/dxfcheck.py /tmp/rt1452/*.dxf
```

Результат (сравнение с веткой до этапа 5): все прогоны — OK;
`1339_testtable` — неразрешённая ссылка `342:BA` исчезла (стили
aitable/Standard/vebtable сохраняются); в ветке ACAD_TABLESTYLE —
стили исходных файлов вместо одного Standard. Ссылка `331:94`
(tablestyleetalon, 1436) — из шаблона, была и до этапа 5.
