# issue #1458 — этап 8 ТЗ NOD: тесты, документация, лог

Все сценарии запускаются из корня репозитория.

| Файл | Назначение |
|------|------------|
| `check_log_sensitivity.sh [коммит]` | Проверка чувствительности `nodstage8`: модули стилей временно берутся из коммита до этапа 8 (по умолчанию `751bdf55b`), тест должен упасть; файлы восстанавливаются из `HEAD`. |

Сборка и запуск тестов NOD — штатные средства этапа 8:

```sh
sudo apt-get install fpc lazarus-src
git submodule update --init --depth 1 cad_source/components/{zbaseutils,zcontainers,zmath,zreaders,zunits,zmacros,zundostack}
make -C cad_source/zengine/tests nodtests                  # все nodstage*.lpr
make -C cad_source/zengine/tests nodtests NODTESTS=nodstage8
HEAPTRC=1 cad_source/zengine/tests/nodtests.sh nodstage8    # с heaptrc
```

Трасса NOD в ZCAD включается ключом командной строки `lem NOD`.

## Результат

Тесты `nodstage0`–`nodstage8` проходят. Без исправления модулей стилей
`nodstage8` падает: 8–13 сообщений `LM_Info` модулей NOD мимо модуля лога
`NOD` на загрузку и сохранение каждого файла. `heaptrc` (`nodstage8`):
2 неосвобождённых блока — те же, что в `nodstage7` (реестры).
