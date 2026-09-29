# issue #1456 — этап 7 ТЗ NOD: вспомогательные сценарии

Все сценарии запускаются из корня репозитория. Сборка без IDE — сценарии
этапа 5 (`experiments/issue1452/build_and_run.sh`, см.
`experiments/issue1452/README.md`).

| Файл | Назначение |
|------|------------|
| `run_tests.sh` | Сборка и запуск `nodstage0`–`nodstage7`, затем независимая (без zengine) проверка файлов, сохранённых `nodstage7`, сценарием `experiments/issue1450/dxfcheck.py` (уникальность хэндлов, разрешимость ссылок, порядок ключей NOD). |

## Файлы теста

`nodstage7` оставляет во временном каталоге:

- `nodstage7_source_2007.dxf` — исходный файл: `tablestyleetalon.dxf` с
  `$DWGCODEPAGE = ANSI_1251`, классом `MYPLUGINOBJ` и ключами NOD
  `MY_PLUGIN`, `MY_HARD`, `ZCAD_DATA`, `ACAD_FIELDLIST`;
- `nodstage7_2007.dxf`, `nodstage7_2007_2007.dxf` — сохранение в DXF 2007
  и повторное сохранение результата;
- `nodstage7_2000.dxf` — сохранение в DXF 2000 (строки в `cp1251`).

Эти файлы — кандидаты для ручной проверки в AutoCAD (`AUDIT`).

## Результат

Тесты `nodstage0`–`nodstage7` проходят. `dxfcheck.py`: в DXF 2000 — OK;
в DXF 2007 единственная неразрешённая ссылка `331:94` — из шаблона
`savetemplate2007.dxf` (есть и в пустом чертеже, была до этапа 7).
Ссылок, ведущих из сохранённых веток в никуда, нет.

Проверка чувствительности (по одному изменению, затем откат): без вызова
`DXFNODPreserveUnknownBranches` в `uzeffdxf.pas` — 28 ошибок `nodstage7`,
без `NODSave.WriteClasses` в `uzeffdxfout.pas` — 11, без
`RegisterPreservedNODApps` — 3.

Трасса этапа — лог NOD (`lem NOD`, выключен по умолчанию): какие ключи
сохранены или пропущены, классы, `APPID`, неразрешённые ссылки.
