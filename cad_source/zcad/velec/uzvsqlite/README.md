# Модуль экспорта данных в SQLite (uzvsqlite)

## Описание

Модуль `uzvsqlite` предназначен для гибкого экспорта данных примитивов (устройства, суперлинии, кабели) из приложения ZCAD в базу данных SQLite.

Экспорт осуществляется на основе управляющих таблиц `EXPORT1`, `EXPORT2`, ..., `EXPORTN`, которые содержат инструкции по маппингу данных из примитивов в целевые таблицы SQLite.

**Особенности модуля:**
- Работа с SQLite через библиотеку `sqlite3.dll`
- Полная совместимость с форматом инструкций модуля `uzvaccess`
- Автоматическое создание базы данных при отсутствии
- Поддержка транзакций и UPSERT операций
- Логирование всех операций

## Архитектура

Модуль построен по принципу разделения ответственности на три основных слоя:

### 1. DATA LAYER (слой данных)
- `uzvsqlite_types.pas` — определение типов данных
- `uzvsqlite_config.pas` — конфигурация модуля
- `uzvsqlite_entity_adapter.pas` — адаптер для получения свойств примитивов

### 2. LOGIC LAYER (слой бизнес-логики)
- `uzvsqlite_parser.pas` — парсинг инструкций из EXPORT-таблиц
- `uzvsqlite_validator.pas` — валидация и преобразование типов данных
- `uzvsqlite_logger.pas` — логирование процесса экспорта

### 3. DATABASE LAYER (слой работы с БД)
- `uzvsqlite_connection.pas` — управление подключением к SQLite
- `uzvsqlite_executor.pas` — выполнение SQL-запросов и вставка данных

### 4. ORCHESTRATION LAYER (слой оркестрации)
- `uzvsqlite_exporter.pas` — главный класс TSQLiteExporter
- `uzvsqlite_command.pas` — команда для интеграции с ZCAD

## Формат управляющих таблиц EXPORT

Каждая таблица `EXPORTn` содержит инструкции для экспорта данных в целевую таблицу SQLite.

### Структура таблицы

Все колонки имеют строковый тип. Каждая строка — это инструкция.

**ВАЖНО:** Col1 является столбцом ID с уникальными значениями и игнорируется парсером.
Инструкции начинаются с Col2.

| Col1 (ID)  | Col2       | Col3            | Col4    | Col5                 | Col6+ |
|------------|------------|-----------------|---------|----------------------|-------|
| 1          | tTable     | TargetTableName |         |                      |       |
| 2          | typeData   | device          |         |                      |       |
| 3          | setcolumn  | DeviceName      | string  | NMO_BaseName         |       |
| 4          | setcolumn  | Power           | float   | VPOWER_Value         |       |
| 5          | setcolumn  | Voltage         | integer | VVOLTAGE_Value       |       |

### Поддерживаемые инструкции

#### tTable
Определяет имя целевой таблицы в SQLite.

- **Col1:** ID (игнорируется)
- **Col2:** `tTable`
- **Col3:** Имя целевой таблицы

#### typeData
Определяет тип источника данных.

- **Col1:** ID (игнорируется)
- **Col2:** `typeData`
- **Col3:** Тип данных: `device`, `superline`, `cable`

#### setcolumn
Определяет маппинг колонки.

- **Col1:** ID (игнорируется)
- **Col2:** `setcolumn`
- **Col3:** Имя колонки в целевой таблице
- **Col4:** Тип данных: `string`, `integer`, `float`
- **Col5:** Имя свойства источника (например, `NMO_BaseName`)
- **Col6+:** Зарезервировано для расширений

#### keyColumn (опционально)
Определяет ключевые колонки для UPSERT.

- **Col1:** ID (игнорируется)
- **Col2:** `keyColumn`
- **Col3+:** Имена ключевых колонок

#### const (опционально)
Задает константное значение.

- **Col1:** ID (игнорируется)
- **Col2:** `const`
- **Col3:** Имя колонки
- **Col4:** Константное значение

## Использование

### Базовый пример

```pascal
uses
  uzvsqlite_types, uzvsqlite_config, uzvsqlite_exporter;

var
  config: TExportConfig;
  exporter: TSQLiteExporter;
  result: TExportResult;
begin
  // Создание конфигурации
  config := TExportConfig.Create;
  try
    config.DatabasePath := 'C:\path\to\database.db';
    config.EntityMode := 0;  // Все примитивы
    config.LogFilePath := 'C:\path\to\export.log';

    // Создание экспортера
    exporter := TSQLiteExporter.Create(config);
    try
      // Выполнение экспорта
      result := exporter.Execute;
      try
        // Вывод статистики
        WriteLn(result.GetSummary);
      finally
        result.Free;
      end;
    finally
      exporter.Free;
    end;
  finally
    config.Free;
  end;
end;
```

### Использование через команду ZCAD

```
// В командной строке ZCAD
SQLiteExport
```

Команда откроет диалог выбора файла SQLite и выполнит экспорт.

## Конфигурационный файл

Модуль поддерживает загрузку настроек из INI-файла.

### Пример config.ini

```ini
[Connection]
DatabasePath=C:\Projects\MyProject\data.db
Driver=

[Behavior]
DryRun=false
StrictValidation=true
AllowNullValues=true
ErrorMode=continue

[Performance]
BatchSize=50
RetryAttempts=3
RetryDelay=1000

[Logging]
LogLevel=info
LogFilePath=C:\Logs\sqlite_export.log
LogToGUI=true

[DataSource]
EntityMode=0
EntityModeParam=
```

### Загрузка конфигурации

```pascal
config := TExportConfig.Create;
config.LoadFromFile('config.ini');
```

## Получение свойств примитивов

Модуль использует механизм переменных (variables) для получения свойств примитивов.

### Примеры имен свойств

- `NMO_BaseName` — базовое имя устройства
- `VPOWER_Value` — мощность
- `VVOLTAGE_Value` — напряжение
- `VSPECIFICATION_Name` — спецификация
- `ENTID_Type` — тип примитива

Адаптер автоматически определяет тип примитива и извлекает соответствующие свойства.

## Обработка ошибок

### Режимы работы при ошибках

- **continue** (по умолчанию) — продолжить обработку следующей таблицы
- **stop** — остановить при первой ошибке

### Повторные попытки

Модуль поддерживает автоматические повторные попытки подключения с экспоненциальной задержкой.

```ini
RetryAttempts=3
RetryDelay=1000  ; начальная задержка в мс
```

## Транзакции

Каждая EXPORT-таблица обрабатывается в отдельной транзакции:

1. Начало транзакции
2. Парсинг инструкций
3. Получение данных из примитивов
4. Вставка/обновление данных в SQLite
5. **COMMIT** при успехе или **ROLLBACK** при ошибке

## Логирование

Модуль ведет подробное логирование с несколькими уровнями:

- **DEBUG** — отладочная информация
- **INFO** — информационные сообщения
- **WARNING** — предупреждения
- **ERROR** — ошибки

Логи записываются:
- В файл (если указан `LogFilePath`)
- В GUI ZCAD (если `LogToGUI=true`)
- В programlog ZCAD

## Производительность

### Пакетная вставка

Данные вставляются пакетами для повышения производительности:

```ini
BatchSize=50  ; вставлять по 50 строк за раз
```

### Параметризованные запросы

Все SQL-запросы используют параметры для защиты от SQL-инъекций и повышения производительности.

## Расширяемость

### Добавление новых типов инструкций

Модуль поддерживает регистрацию пользовательских обработчиков инструкций:

```pascal
parser.RegisterInstructionHandler('myinstruction', @HandleMyInstruction);
```

### Добавление новых типов источников данных

Можно зарегистрировать обработчики для новых типов примитивов без изменения кода модуля.

## Примеры управляющих таблиц

### Пример 1: Простой экспорт устройств

Таблица `EXPORT1`:

| Col1 (ID) | Col2      | Col3       | Col4    | Col5              | Col6 |
|-----------|-----------|------------|---------|-------------------|------|
| 1         | tTable    | Devices    |         |                   |      |
| 2         | typeData  | device     |         |                   |      |
| 3         | setcolumn | DeviceName | string  | NMO_BaseName      |      |
| 4         | setcolumn | Power      | float   | VPOWER_Value      |      |
| 5         | setcolumn | Voltage    | integer | VVOLTAGE_Value    |      |
| 6         | setcolumn | Phase      | string  | VPHASE_Value      |      |

### Пример 2: Экспорт с ключевыми колонками

Таблица `EXPORT2`:

| Col1 (ID) | Col2      | Col3       | Col4    | Col5              | Col6 |
|-----------|-----------|------------|---------|-------------------|------|
| 1         | tTable    | Cables     |         |                   |      |
| 2         | typeData  | cable      |         |                   |      |
| 3         | keyColumn | CableName  |         |                   |      |
| 4         | setcolumn | CableName  | string  | NMO_BaseName      |      |
| 5         | setcolumn | Length     | float   | VLENGTH_Value     |      |
| 6         | setcolumn | CrossSection | float | VCROSSSECTION_Value |    |

При наличии `keyColumn` модуль будет обновлять существующие записи вместо вставки дубликатов.

### Пример 3: Экспорт с константами

Таблица `EXPORT3`:

| Col1 (ID) | Col2      | Col3       | Col4    | Col5              | Col6 |
|-----------|-----------|------------|---------|-------------------|------|
| 1         | tTable    | Equipment  |         |                   |      |
| 2         | typeData  | device     |         |                   |      |
| 3         | setcolumn | Name       | string  | NMO_BaseName      |      |
| 4         | const     | Category   | Electrical |                 |      |
| 5         | const     | Status     | Active  |                   |      |

## Тестирование

### Режим Dry Run

Для проверки конфигурации без записи в базу данных используйте режим `DryRun`:

```ini
DryRun=true
```

В этом режиме модуль:
- Подключается к базе данных
- Парсит инструкции
- Получает данные из примитивов
- **НЕ выполняет** вставки/обновления
- Выводит подробную статистику

## Ограничения и особенности

1. SQLite — это файловая БД, не требующая серверного процесса
2. Размер базы данных SQLite ограничен размером файловой системы (теоретически до 140 ТБ)
3. SQLite эффективно работает с небольшими и средними объемами данных
4. Имена колонок с пробелами должны быть заключены в квадратные скобки
5. Требуется наличие `sqlite3.dll` в директории приложения или системе

## Структура файлов

```
cad_source/zcad/velec/uzvsqlite/
├── README.md                          # Документация (этот файл)
├── uzvsqlite_types.pas                # Типы данных
├── uzvsqlite_config.pas               # Конфигурация
├── uzvsqlite_logger.pas               # Логирование
├── uzvsqlite_connection.pas           # Подключение к SQLite
├── uzvsqlite_parser.pas               # Парсер инструкций
├── uzvsqlite_entity_adapter.pas       # Адаптер примитивов
├── uzvsqlite_validator.pas            # Валидация типов
├── uzvsqlite_executor.pas             # Исполнитель экспорта
├── uzvsqlite_exporter.pas             # Главный класс
└── uzvsqlite_command.pas              # Команда ZCAD
```

## Авторы

@author Vladimir Bobrov

## Лицензия

См. файл COPYING.txt в корне проекта ZCAD.
