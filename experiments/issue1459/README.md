# issue #1459 — этап 9 ТЗ NOD: DWG

Все сценарии запускаются из корня репозитория.

| Файл | Назначение |
|------|------------|
| `build_uzedwgimport.sh` | Проверка компиляции загрузчика DWG `uzedwgimport.pas` с фазой `dwg-import.scan.nod` (`uzedwgnod`). `nodstage9` модуль `uzedwgimport` не собирает (он тянет инспектор объектов и скрипты), поэтому сборка проверяется отдельно. |

Сборка и запуск тестов NOD — штатные средства этапа 8:

```sh
sudo apt-get install fpc lazarus-src
git submodule update --init --depth 1 cad_source/components/{zbaseutils,zcontainers,zmath,zreaders,zunits,zmacros,zundostack}
make -C cad_source/zengine/tests nodtests NODTESTS=nodstage9
HEAPTRC=1 cad_source/zengine/tests/nodtests.sh nodstage9    # с heaptrc

# сборка uzedwgimport.pas — дополнительно нужны подмодули:
git submodule update --init --depth 1 cad_source/components/{zobjectinspector,zbaseutilsgui,zscriptbase}
experiments/issue1459/build_uzedwgimport.sh
```

`libredwg.so` не нужна: `nodstage9` строит объекты DWG в памяти записями
`dwg.pp` (R2004 и R2013, UTF-16 строки R2007+).

## Результат

Тесты `nodstage0`–`nodstage9` проходят, `heaptrc` на `nodstage9` утечек не
показывает. `build_uzedwgimport.sh`: `uzedwgimport: OK`.
