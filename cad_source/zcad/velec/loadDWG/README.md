# loadDWG

Каталог под исследовательские материалы и техническое задание на новую архитектуру загрузки DWG-файлов через LibreDWG.

## Содержимое

- [TZ_loadDWG.md](TZ_loadDWG.md) — техническое задание: анализ существующих архитектур (DXF и DWG), матрица паритета сущностей и таблиц, целевая модульная структура (модули 300–500 строк, одна ответственность), этапы разработки.

## Связанные issue / PR

- Issue [#1039](https://github.com/veb86/zcadvelecAI/issues/1039) — постановка задачи (исследование и ТЗ, без кодинга).
- PR [#1040](https://github.com/veb86/zcadvelecAI/pull/1040) — этот документ.
- Предшествующие PR по стабилизации LibreDWG-биндинга: #1033, #1035, #1037.

## Связанный код

- Текущая обёртка LibreDWG: `cad_source/zengine/fileformats/uzefflibredwg.pas`, `uzefflibredwg2ents.pas`.
- Биндинг LibreDWG: `cad_source/components/fpdwg/dwg.pp`, `dwgproc.pp`, `fpdwg.pas`.
- Регистрация форматов: `cad_source/zcad/register/uzcregfileformats.pas`.
- Загрузчик DXF (модель для подражания): `cad_source/zengine/fileformats/uzeffdxf.pas`, `uzeffdxfsupport.pas`.
