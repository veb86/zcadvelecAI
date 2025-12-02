# Концепция базы данных устройств и компонентов

## 1. Общие принципы

### 1.1 Цели проектирования
- **Гибкость**: Легкое добавление новых типов устройств без изменения структуры БД
- **Расширяемость**: Поддержка произвольных параметров для разных типов устройств
- **Производительность**: Быстрый поиск и выборка данных
- **Простота использования**: Интуитивная структура для наполнения и редактирования

### 1.2 Выбор технологии
**SQLite** - оптимальный выбор для данной задачи:
- Не требует отдельного сервера БД
- Файл БД легко распространять и версионировать
- Высокая производительность для локальных операций
- Поддержка транзакций и индексов
- Встроенная поддержка в Free Pascal/Lazarus

## 2. Архитектура базы данных

### 2.1 Многоуровневая структура

```
┌─────────────────────┐
│  Категории          │ - Верхний уровень классификации
│  (Categories)       │
└──────────┬──────────┘
           │
           ▼
┌─────────────────────┐
│  Типы устройств     │ - Типы в рамках категории
│  (DeviceTypes)      │
└──────────┬──────────┘
           │
           ▼
┌─────────────────────┐
│  Устройства         │ - Конкретные модели устройств
│  (Devices)          │
└──────────┬──────────┘
           │
           ▼
┌─────────────────────┐
│  Параметры          │ - Технические характеристики
│  (Parameters)       │
└─────────────────────┘
```

### 2.2 Entity-Attribute-Value (EAV) подход

Для хранения разнообразных параметров используется гибридный подход:
- **Базовые параметры** - в основной таблице устройств
- **Специфичные параметры** - через EAV модель

## 3. Структура таблиц

### 3.1 Таблица Categories (Категории)

**Назначение**: Высокоуровневая классификация оборудования

```sql
CREATE TABLE Categories (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL UNIQUE,           -- Название категории (русское)
    name_en TEXT,                         -- Название (английское)
    code TEXT UNIQUE,                     -- Код категории
    description TEXT,                     -- Описание
    icon_path TEXT,                       -- Путь к иконке
    sort_order INTEGER DEFAULT 0,        -- Порядок сортировки
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
```

**Примеры записей**:
- Электроаппараты (circuit breakers, contactors, relays)
- Кабельная продукция (cables, wires)
- Приборы автоматики (sensors, controllers)
- ОПС (fire alarm devices)
- Кабеленесущие системы (cable trays, conduits)

### 3.2 Таблица DeviceTypes (Типы устройств)

**Назначение**: Классификация устройств внутри категории

```sql
CREATE TABLE DeviceTypes (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    category_id INTEGER NOT NULL,        -- Связь с категорией
    name TEXT NOT NULL,                   -- Название типа
    code TEXT UNIQUE,                     -- Код типа
    description TEXT,                     -- Описание
    has_rated_current BOOLEAN DEFAULT 0, -- Имеет номинальный ток
    has_cross_section BOOLEAN DEFAULT 0, -- Имеет сечение
    has_voltage BOOLEAN DEFAULT 0,       -- Имеет напряжение
    has_dimensions BOOLEAN DEFAULT 0,    -- Имеет габариты
    sort_order INTEGER DEFAULT 0,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (category_id) REFERENCES Categories(id) ON DELETE CASCADE
);
```

**Примеры записей**:
- Автоматические выключатели
- Кабель силовой
- Кабель контрольный
- Лотки кабельные
- Извещатели пожарные

### 3.3 Таблица Devices (Устройства)

**Назначение**: Конкретные модели устройств с базовыми характеристиками

```sql
CREATE TABLE Devices (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    device_type_id INTEGER NOT NULL,     -- Связь с типом устройства

    -- Базовая информация
    manufacturer TEXT,                    -- Производитель
    model TEXT NOT NULL,                  -- Модель
    part_number TEXT,                     -- Артикул/каталожный номер
    name_short TEXT,                      -- Краткое название
    name_full TEXT,                       -- Полное название
    description TEXT,                     -- Описание

    -- Документация
    standard TEXT,                        -- ГОСТ/ТУ
    okp_code TEXT,                        -- Код ОКП
    certification TEXT,                   -- Сертификация

    -- Общие электрические параметры
    rated_voltage REAL,                   -- Номинальное напряжение, В
    rated_current REAL,                   -- Номинальный ток, А
    rated_power REAL,                     -- Номинальная мощность, Вт
    voltage_type TEXT,                    -- Тип напряжения (AC/DC)
    frequency REAL,                       -- Частота, Гц

    -- Общие физические параметры
    length REAL,                          -- Длина, мм
    width REAL,                           -- Ширина, мм
    height REAL,                          -- Высота, мм
    weight REAL,                          -- Вес, кг

    -- Эксплуатационные параметры
    ip_rating TEXT,                       -- Степень защиты IP
    operating_temp_min REAL,              -- Мин. температура, °C
    operating_temp_max REAL,              -- Макс. температура, °C

    -- Метаданные
    is_active BOOLEAN DEFAULT 1,         -- Активен
    price REAL,                           -- Цена
    currency TEXT DEFAULT 'RUB',          -- Валюта
    unit_of_measure TEXT DEFAULT 'шт',   -- Единица измерения
    notes TEXT,                           -- Примечания
    data_sheet_url TEXT,                  -- Ссылка на документацию
    image_path TEXT,                      -- Путь к изображению

    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

    FOREIGN KEY (device_type_id) REFERENCES DeviceTypes(id) ON DELETE CASCADE
);
```

### 3.4 Таблица ParameterDefinitions (Определения параметров)

**Назначение**: Определение возможных параметров для разных типов устройств

```sql
CREATE TABLE ParameterDefinitions (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    device_type_id INTEGER,              -- NULL = общий параметр
    name TEXT NOT NULL,                   -- Название параметра
    name_en TEXT,                         -- Название (англ.)
    code TEXT NOT NULL,                   -- Код параметра (для программного использования)
    data_type TEXT NOT NULL,              -- Тип данных: REAL, INTEGER, TEXT, BOOLEAN
    unit TEXT,                            -- Единица измерения
    description TEXT,                     -- Описание
    is_required BOOLEAN DEFAULT 0,       -- Обязательный параметр
    default_value TEXT,                   -- Значение по умолчанию
    min_value REAL,                       -- Минимальное значение
    max_value REAL,                       -- Максимальное значение
    validation_regex TEXT,                -- Регулярное выражение для валидации
    sort_order INTEGER DEFAULT 0,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (device_type_id) REFERENCES DeviceTypes(id) ON DELETE CASCADE,
    UNIQUE(device_type_id, code)
);
```

**Примеры записей для автоматических выключателей**:
- `trip_current` - Ток срабатывания, А
- `trip_curve` - Характеристика срабатывания (B, C, D)
- `breaking_capacity` - Отключающая способность, кА
- `poles_count` - Количество полюсов
- `trip_time_zone` - Время срабатывания в зоне КЗ, мс

**Примеры для кабелей**:
- `core_count` - Количество жил
- `core_cross_section` - Сечение жилы, мм²
- `outer_diameter` - Наружный диаметр, мм
- `continuous_current` - Длительно допустимый ток, А
- `overload_current` - Ток перегрузки, А
- `insulation_type` - Тип изоляции
- `sheath_material` - Материал оболочки

### 3.5 Таблица DeviceParameters (Значения параметров)

**Назначение**: Хранение фактических значений параметров для устройств

```sql
CREATE TABLE DeviceParameters (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    device_id INTEGER NOT NULL,           -- Связь с устройством
    parameter_def_id INTEGER NOT NULL,    -- Связь с определением параметра
    value_text TEXT,                      -- Значение (текстовое)
    value_numeric REAL,                   -- Значение (числовое)
    value_boolean BOOLEAN,                -- Значение (логическое)
    notes TEXT,                           -- Примечания
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (device_id) REFERENCES Devices(id) ON DELETE CASCADE,
    FOREIGN KEY (parameter_def_id) REFERENCES ParameterDefinitions(id) ON DELETE CASCADE,
    UNIQUE(device_id, parameter_def_id)
);
```

### 3.6 Таблица DeviceCompatibility (Совместимость устройств)

**Назначение**: Связи между совместимыми устройствами (например, кабель-автомат)

```sql
CREATE TABLE DeviceCompatibility (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    device_id INTEGER NOT NULL,           -- Основное устройство
    compatible_device_id INTEGER NOT NULL,-- Совместимое устройство
    compatibility_type TEXT,              -- Тип совместимости
    notes TEXT,                           -- Примечания о совместимости
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (device_id) REFERENCES Devices(id) ON DELETE CASCADE,
    FOREIGN KEY (compatible_device_id) REFERENCES Devices(id) ON DELETE CASCADE,
    UNIQUE(device_id, compatible_device_id, compatibility_type)
);
```

### 3.7 Таблица DeviceVariants (Варианты устройств)

**Назначение**: Варианты исполнения одного устройства

```sql
CREATE TABLE DeviceVariants (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    parent_device_id INTEGER NOT NULL,    -- Базовое устройство
    variant_name TEXT NOT NULL,           -- Название варианта
    variant_code TEXT,                    -- Код варианта
    part_number TEXT,                     -- Артикул варианта
    price_modifier REAL DEFAULT 0,       -- Модификатор цены
    description TEXT,                     -- Описание отличий
    is_active BOOLEAN DEFAULT 1,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (parent_device_id) REFERENCES Devices(id) ON DELETE CASCADE
);
```

### 3.8 Таблица VariantParameters (Параметры вариантов)

**Назначение**: Переопределенные параметры для вариантов

```sql
CREATE TABLE VariantParameters (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    variant_id INTEGER NOT NULL,
    parameter_def_id INTEGER NOT NULL,
    value_text TEXT,
    value_numeric REAL,
    value_boolean BOOLEAN,
    FOREIGN KEY (variant_id) REFERENCES DeviceVariants(id) ON DELETE CASCADE,
    FOREIGN KEY (parameter_def_id) REFERENCES ParameterDefinitions(id) ON DELETE CASCADE,
    UNIQUE(variant_id, parameter_def_id)
);
```

## 4. Индексы для производительности

```sql
-- Индексы для быстрого поиска
CREATE INDEX idx_devices_type ON Devices(device_type_id);
CREATE INDEX idx_devices_manufacturer ON Devices(manufacturer);
CREATE INDEX idx_devices_model ON Devices(model);
CREATE INDEX idx_device_types_category ON DeviceTypes(category_id);
CREATE INDEX idx_device_params_device ON DeviceParameters(device_id);
CREATE INDEX idx_device_params_param ON DeviceParameters(parameter_def_id);
CREATE INDEX idx_param_defs_type ON ParameterDefinitions(device_type_id);
CREATE INDEX idx_variants_parent ON DeviceVariants(parent_device_id);

-- Полнотекстовый поиск
CREATE VIRTUAL TABLE DeviceSearch USING fts5(
    device_id,
    manufacturer,
    model,
    name_short,
    name_full,
    description,
    content=Devices
);
```

## 5. Представления (Views)

### 5.1 Полная информация об устройстве

```sql
CREATE VIEW v_DeviceFullInfo AS
SELECT
    d.id,
    d.manufacturer,
    d.model,
    d.part_number,
    d.name_short,
    d.name_full,
    dt.name as device_type,
    dt.code as device_type_code,
    c.name as category,
    c.code as category_code,
    d.rated_voltage,
    d.rated_current,
    d.rated_power,
    d.price,
    d.unit_of_measure
FROM Devices d
JOIN DeviceTypes dt ON d.device_type_id = dt.id
JOIN Categories c ON dt.category_id = c.id;
```

### 5.2 Параметры устройства с определениями

```sql
CREATE VIEW v_DeviceParametersComplete AS
SELECT
    dp.device_id,
    d.model as device_model,
    pd.name as parameter_name,
    pd.code as parameter_code,
    pd.unit,
    pd.data_type,
    CASE
        WHEN pd.data_type = 'TEXT' THEN dp.value_text
        WHEN pd.data_type = 'REAL' THEN CAST(dp.value_numeric AS TEXT)
        WHEN pd.data_type = 'INTEGER' THEN CAST(dp.value_numeric AS TEXT)
        WHEN pd.data_type = 'BOOLEAN' THEN CASE dp.value_boolean WHEN 1 THEN 'Да' ELSE 'Нет' END
    END as value_display
FROM DeviceParameters dp
JOIN ParameterDefinitions pd ON dp.parameter_def_id = pd.id
JOIN Devices d ON dp.device_id = d.id;
```

## 6. Триггеры для автоматизации

### 6.1 Обновление временной метки

```sql
-- Автоматическое обновление updated_at
CREATE TRIGGER update_devices_timestamp
AFTER UPDATE ON Devices
BEGIN
    UPDATE Devices SET updated_at = CURRENT_TIMESTAMP WHERE id = NEW.id;
END;

CREATE TRIGGER update_categories_timestamp
AFTER UPDATE ON Categories
BEGIN
    UPDATE Categories SET updated_at = CURRENT_TIMESTAMP WHERE id = NEW.id;
END;

CREATE TRIGGER update_device_types_timestamp
AFTER UPDATE ON DeviceTypes
BEGIN
    UPDATE DeviceTypes SET updated_at = CURRENT_TIMESTAMP WHERE id = NEW.id;
END;
```

### 6.2 Синхронизация поискового индекса

```sql
CREATE TRIGGER devices_ai AFTER INSERT ON Devices BEGIN
    INSERT INTO DeviceSearch(device_id, manufacturer, model, name_short, name_full, description)
    VALUES (NEW.id, NEW.manufacturer, NEW.model, NEW.name_short, NEW.name_full, NEW.description);
END;

CREATE TRIGGER devices_au AFTER UPDATE ON Devices BEGIN
    UPDATE DeviceSearch
    SET manufacturer = NEW.manufacturer,
        model = NEW.model,
        name_short = NEW.name_short,
        name_full = NEW.name_full,
        description = NEW.description
    WHERE device_id = NEW.id;
END;

CREATE TRIGGER devices_ad AFTER DELETE ON Devices BEGIN
    DELETE FROM DeviceSearch WHERE device_id = OLD.id;
END;
```

## 7. Примеры использования

### 7.1 Добавление категории и типа

```sql
-- Добавляем категорию
INSERT INTO Categories (name, code, description)
VALUES ('Электроаппараты', 'ELAPP', 'Коммутационные и защитные аппараты');

-- Добавляем тип устройства
INSERT INTO DeviceTypes (category_id, name, code, has_rated_current, has_voltage)
VALUES (1, 'Автоматические выключатели', 'BREAKER', 1, 1);
```

### 7.2 Определение параметров для автоматов

```sql
INSERT INTO ParameterDefinitions (device_type_id, name, code, data_type, unit, is_required)
VALUES
    (1, 'Ток срабатывания', 'trip_current', 'REAL', 'А', 1),
    (1, 'Характеристика', 'trip_curve', 'TEXT', '', 1),
    (1, 'Отключающая способность', 'breaking_capacity', 'REAL', 'кА', 1),
    (1, 'Количество полюсов', 'poles_count', 'INTEGER', 'шт', 1),
    (1, 'Время срабатывания', 'trip_time', 'REAL', 'мс', 0);
```

### 7.3 Добавление устройства

```sql
-- Добавляем автомат
INSERT INTO Devices (
    device_type_id, manufacturer, model, part_number,
    name_short, name_full, rated_current, rated_voltage
) VALUES (
    1, 'ABB', 'S203', 'S203-C16',
    'ABB S203 C16', 'Автоматический выключатель ABB S203, хар-ка C, 16А',
    16, 400
);

-- Добавляем параметры автомата
INSERT INTO DeviceParameters (device_id, parameter_def_id, value_numeric, value_text)
VALUES
    (1, 1, 16, NULL),           -- Ток срабатывания 16А
    (1, 2, NULL, 'C'),          -- Характеристика C
    (1, 3, 6, NULL),            -- Отключающая способность 6кА
    (1, 4, 3, NULL);            -- 3 полюса
```

### 7.4 Поиск устройств

```sql
-- Поиск автоматов на 16А
SELECT * FROM v_DeviceFullInfo
WHERE device_type_code = 'BREAKER' AND rated_current = 16;

-- Полнотекстовый поиск
SELECT d.* FROM DeviceSearch ds
JOIN Devices d ON ds.device_id = d.id
WHERE DeviceSearch MATCH 'ABB';

-- Поиск с параметрами
SELECT d.model, pd.name, dp.value_numeric, pd.unit
FROM Devices d
JOIN DeviceParameters dp ON d.id = dp.device_id
JOIN ParameterDefinitions pd ON dp.parameter_def_id = pd.id
WHERE d.device_type_id = 1 AND pd.code = 'trip_current' AND dp.value_numeric >= 16;
```

## 8. Преимущества концепции

### 8.1 Гибкость
- ✅ Легко добавлять новые типы устройств
- ✅ Произвольное количество параметров для каждого типа
- ✅ Расширение без изменения структуры БД

### 8.2 Масштабируемость
- ✅ Поддержка десятков тысяч устройств
- ✅ Эффективные индексы для быстрого поиска
- ✅ Полнотекстовый поиск

### 8.3 Удобство использования
- ✅ Интуитивная структура
- ✅ Готовые представления для частых запросов
- ✅ Автоматизация через триггеры

### 8.4 Интеграция с программой
- ✅ Совместимость с существующими типами (devices.pas)
- ✅ Легко мапится на Pascal объекты
- ✅ Поддержка существующей структуры DeviceDbBaseObject

## 9. План реализации

### Этап 1: Создание базовой структуры
1. Создать SQL скрипт с основными таблицами
2. Добавить индексы
3. Создать представления
4. Добавить триггеры

### Этап 2: Наполнение справочников
1. Заполнить категории
2. Создать типы устройств
3. Определить параметры для каждого типа

### Этап 3: Интеграция с программой
1. Создать Pascal модуль для работы с БД
2. Реализовать CRUD операции
3. Добавить UI для редактирования

### Этап 4: Миграция данных
1. Импортировать существующие устройства
2. Проверить целостность данных
3. Создать резервные копии

### Этап 5: Расширенный функционал
1. Импорт/экспорт в Excel/CSV
2. Синхронизация с внешними каталогами
3. История изменений
4. Версионирование БД

## 10. Дальнейшее развитие

### Возможные расширения:
- **Мультиязычность**: Поддержка нескольких языков для названий и описаний
- **Версионирование**: История изменений устройств
- **Вложенные группы**: Иерархическая структура категорий
- **Теги**: Система тегов для гибкой классификации
- **Связи**: Типовые схемы подключения устройств
- **Расчетные параметры**: Автоматический расчет производных параметров
- **Сертификаты**: Хранение документов и сертификатов
- **Аналоги**: Связи между аналогичными устройствами разных производителей
