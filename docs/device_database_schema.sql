-- ============================================================================
-- Схема базы данных устройств для zcadvelecAI
-- Версия: 1.0
-- Дата создания: 2025-10-17
-- Описание: База данных для хранения информации об электрических устройствах,
--           кабелях, комплектующих и их параметрах
-- ============================================================================

-- ============================================================================
-- 1. КАТЕГОРИИ УСТРОЙСТВ
-- ============================================================================

CREATE TABLE Categories (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL UNIQUE,            -- Название категории (например, "Электроаппараты")
    name_en TEXT,                          -- Название на английском
    code TEXT UNIQUE,                      -- Код категории (например, "ELAPP")
    description TEXT,                      -- Описание категории
    icon_path TEXT,                        -- Путь к иконке категории
    sort_order INTEGER DEFAULT 0,         -- Порядок сортировки
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Триггер для обновления временной метки
CREATE TRIGGER update_categories_timestamp
AFTER UPDATE ON Categories
BEGIN
    UPDATE Categories SET updated_at = CURRENT_TIMESTAMP WHERE id = NEW.id;
END;

-- Индекс для быстрой сортировки
CREATE INDEX idx_categories_sort ON Categories(sort_order);

-- ============================================================================
-- 2. ТИПЫ УСТРОЙСТВ
-- ============================================================================

CREATE TABLE DeviceTypes (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    category_id INTEGER NOT NULL,         -- Связь с категорией
    name TEXT NOT NULL,                    -- Название типа (например, "Автоматические выключатели")
    code TEXT UNIQUE,                      -- Код типа (например, "BREAKER")
    description TEXT,                      -- Описание типа

    -- Флаги наличия стандартных параметров
    has_rated_current BOOLEAN DEFAULT 0,  -- Имеет номинальный ток
    has_cross_section BOOLEAN DEFAULT 0,  -- Имеет сечение (для кабелей)
    has_voltage BOOLEAN DEFAULT 0,        -- Имеет напряжение
    has_power BOOLEAN DEFAULT 0,          -- Имеет мощность
    has_dimensions BOOLEAN DEFAULT 0,     -- Имеет габариты

    sort_order INTEGER DEFAULT 0,         -- Порядок сортировки
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

    FOREIGN KEY (category_id) REFERENCES Categories(id) ON DELETE CASCADE
);

-- Триггер для обновления временной метки
CREATE TRIGGER update_device_types_timestamp
AFTER UPDATE ON DeviceTypes
BEGIN
    UPDATE DeviceTypes SET updated_at = CURRENT_TIMESTAMP WHERE id = NEW.id;
END;

-- Индексы
CREATE INDEX idx_device_types_category ON DeviceTypes(category_id);
CREATE INDEX idx_device_types_sort ON DeviceTypes(sort_order);

-- ============================================================================
-- 3. УСТРОЙСТВА (основная таблица)
-- ============================================================================

CREATE TABLE Devices (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    device_type_id INTEGER NOT NULL,      -- Связь с типом устройства

    -- Базовая информация
    manufacturer TEXT,                     -- Производитель (например, "ABB", "Schneider Electric")
    model TEXT NOT NULL,                   -- Модель (например, "S203")
    part_number TEXT,                      -- Артикул/каталожный номер (например, "S203-C16")
    name_short TEXT,                       -- Краткое название
    name_full TEXT,                        -- Полное название
    description TEXT,                      -- Подробное описание

    -- Документация и стандарты
    standard TEXT,                         -- ГОСТ, ТУ (например, "ГОСТ Р 50345-2010")
    okp_code TEXT,                         -- Код ОКП
    certification TEXT,                    -- Информация о сертификации

    -- Электрические параметры (общие)
    rated_voltage REAL,                    -- Номинальное напряжение, В
    rated_current REAL,                    -- Номинальный ток, А
    rated_power REAL,                      -- Номинальная мощность, Вт
    voltage_type TEXT,                     -- Тип напряжения (AC/DC)
    frequency REAL,                        -- Частота, Гц (обычно 50)

    -- Физические параметры
    length REAL,                           -- Длина, мм
    width REAL,                            -- Ширина, мм
    height REAL,                           -- Высота, мм
    weight REAL,                           -- Вес, кг

    -- Эксплуатационные параметры
    ip_rating TEXT,                        -- Степень защиты (например, "IP20", "IP54")
    operating_temp_min REAL,               -- Минимальная температура эксплуатации, °C
    operating_temp_max REAL,               -- Максимальная температура эксплуатации, °C

    -- Коммерческая информация
    price REAL,                            -- Цена
    currency TEXT DEFAULT 'RUB',           -- Валюта
    unit_of_measure TEXT DEFAULT 'шт',    -- Единица измерения (шт, м, компл)

    -- Дополнительная информация
    notes TEXT,                            -- Примечания
    data_sheet_url TEXT,                   -- Ссылка на техническую документацию
    image_path TEXT,                       -- Путь к изображению устройства

    -- Метаданные
    is_active BOOLEAN DEFAULT 1,          -- Активен (используется для архивирования)
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

    FOREIGN KEY (device_type_id) REFERENCES DeviceTypes(id) ON DELETE CASCADE
);

-- Триггер для обновления временной метки
CREATE TRIGGER update_devices_timestamp
AFTER UPDATE ON Devices
BEGIN
    UPDATE Devices SET updated_at = CURRENT_TIMESTAMP WHERE id = NEW.id;
END;

-- Индексы для производительности
CREATE INDEX idx_devices_type ON Devices(device_type_id);
CREATE INDEX idx_devices_manufacturer ON Devices(manufacturer);
CREATE INDEX idx_devices_model ON Devices(model);
CREATE INDEX idx_devices_active ON Devices(is_active);
CREATE INDEX idx_devices_current ON Devices(rated_current) WHERE rated_current IS NOT NULL;
CREATE INDEX idx_devices_voltage ON Devices(rated_voltage) WHERE rated_voltage IS NOT NULL;

-- ============================================================================
-- 4. ОПРЕДЕЛЕНИЯ ПАРАМЕТРОВ
-- ============================================================================

CREATE TABLE ParameterDefinitions (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    device_type_id INTEGER,               -- NULL = общий параметр для всех типов
    name TEXT NOT NULL,                    -- Название параметра (например, "Ток срабатывания")
    name_en TEXT,                          -- Название на английском
    code TEXT NOT NULL,                    -- Код параметра (например, "trip_current")
    data_type TEXT NOT NULL,               -- Тип данных: REAL, INTEGER, TEXT, BOOLEAN
    unit TEXT,                             -- Единица измерения (А, В, мм, кг и т.д.)
    description TEXT,                      -- Подробное описание параметра

    -- Валидация
    is_required BOOLEAN DEFAULT 0,        -- Обязательный параметр
    default_value TEXT,                    -- Значение по умолчанию
    min_value REAL,                        -- Минимальное допустимое значение
    max_value REAL,                        -- Максимальное допустимое значение
    validation_regex TEXT,                 -- Регулярное выражение для валидации
    allowed_values TEXT,                   -- Список разрешенных значений (через запятую)

    sort_order INTEGER DEFAULT 0,         -- Порядок отображения
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

    FOREIGN KEY (device_type_id) REFERENCES DeviceTypes(id) ON DELETE CASCADE,
    UNIQUE(device_type_id, code)
);

-- Индексы
CREATE INDEX idx_param_defs_type ON ParameterDefinitions(device_type_id);
CREATE INDEX idx_param_defs_code ON ParameterDefinitions(code);

-- ============================================================================
-- 5. ЗНАЧЕНИЯ ПАРАМЕТРОВ УСТРОЙСТВ
-- ============================================================================

CREATE TABLE DeviceParameters (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    device_id INTEGER NOT NULL,            -- Связь с устройством
    parameter_def_id INTEGER NOT NULL,     -- Связь с определением параметра

    -- Значения (хранятся в зависимости от типа данных)
    value_text TEXT,                       -- Текстовое значение
    value_numeric REAL,                    -- Числовое значение
    value_boolean BOOLEAN,                 -- Логическое значение

    notes TEXT,                            -- Примечания к значению
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

    FOREIGN KEY (device_id) REFERENCES Devices(id) ON DELETE CASCADE,
    FOREIGN KEY (parameter_def_id) REFERENCES ParameterDefinitions(id) ON DELETE CASCADE,
    UNIQUE(device_id, parameter_def_id)
);

-- Триггер для обновления временной метки
CREATE TRIGGER update_device_params_timestamp
AFTER UPDATE ON DeviceParameters
BEGIN
    UPDATE DeviceParameters SET updated_at = CURRENT_TIMESTAMP WHERE id = NEW.id;
END;

-- Индексы
CREATE INDEX idx_device_params_device ON DeviceParameters(device_id);
CREATE INDEX idx_device_params_param ON DeviceParameters(parameter_def_id);

-- ============================================================================
-- 6. СОВМЕСТИМОСТЬ УСТРОЙСТВ
-- ============================================================================

CREATE TABLE DeviceCompatibility (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    device_id INTEGER NOT NULL,            -- Основное устройство
    compatible_device_id INTEGER NOT NULL, -- Совместимое устройство
    compatibility_type TEXT,               -- Тип совместимости (cable-breaker, connector-device, etc.)
    compatibility_notes TEXT,              -- Примечания о совместимости
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

    FOREIGN KEY (device_id) REFERENCES Devices(id) ON DELETE CASCADE,
    FOREIGN KEY (compatible_device_id) REFERENCES Devices(id) ON DELETE CASCADE,
    UNIQUE(device_id, compatible_device_id, compatibility_type)
);

-- Индексы
CREATE INDEX idx_compat_device ON DeviceCompatibility(device_id);
CREATE INDEX idx_compat_compatible ON DeviceCompatibility(compatible_device_id);

-- ============================================================================
-- 7. ВАРИАНТЫ УСТРОЙСТВ
-- ============================================================================

CREATE TABLE DeviceVariants (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    parent_device_id INTEGER NOT NULL,     -- Базовое устройство
    variant_name TEXT NOT NULL,            -- Название варианта (например, "1P", "2P", "3P")
    variant_code TEXT,                     -- Код варианта
    part_number TEXT,                      -- Артикул варианта
    price_modifier REAL DEFAULT 0,        -- Модификатор цены (+ или - от базовой)
    description TEXT,                      -- Описание отличий варианта
    is_active BOOLEAN DEFAULT 1,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

    FOREIGN KEY (parent_device_id) REFERENCES Devices(id) ON DELETE CASCADE
);

-- Индексы
CREATE INDEX idx_variants_parent ON DeviceVariants(parent_device_id);

-- ============================================================================
-- 8. ПАРАМЕТРЫ ВАРИАНТОВ
-- ============================================================================

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

-- Индексы
CREATE INDEX idx_variant_params_variant ON VariantParameters(variant_id);

-- ============================================================================
-- 9. ПОЛНОТЕКСТОВЫЙ ПОИСК
-- ============================================================================

CREATE VIRTUAL TABLE DeviceSearch USING fts5(
    device_id UNINDEXED,
    manufacturer,
    model,
    part_number,
    name_short,
    name_full,
    description,
    content=Devices,
    content_rowid=id
);

-- Триггеры для синхронизации полнотекстового поиска
CREATE TRIGGER devices_ai AFTER INSERT ON Devices BEGIN
    INSERT INTO DeviceSearch(device_id, manufacturer, model, part_number, name_short, name_full, description)
    VALUES (NEW.id, NEW.manufacturer, NEW.model, NEW.part_number, NEW.name_short, NEW.name_full, NEW.description);
END;

CREATE TRIGGER devices_au AFTER UPDATE ON Devices BEGIN
    UPDATE DeviceSearch
    SET manufacturer = NEW.manufacturer,
        model = NEW.model,
        part_number = NEW.part_number,
        name_short = NEW.name_short,
        name_full = NEW.name_full,
        description = NEW.description
    WHERE device_id = NEW.id;
END;

CREATE TRIGGER devices_ad AFTER DELETE ON Devices BEGIN
    DELETE FROM DeviceSearch WHERE device_id = OLD.id;
END;

-- ============================================================================
-- 10. ПРЕДСТАВЛЕНИЯ (VIEWS)
-- ============================================================================

-- Полная информация об устройстве
CREATE VIEW v_DeviceFullInfo AS
SELECT
    d.id,
    d.manufacturer,
    d.model,
    d.part_number,
    d.name_short,
    d.name_full,
    d.description,
    dt.name as device_type,
    dt.code as device_type_code,
    c.name as category,
    c.code as category_code,
    d.rated_voltage,
    d.rated_current,
    d.rated_power,
    d.voltage_type,
    d.price,
    d.currency,
    d.unit_of_measure,
    d.is_active
FROM Devices d
JOIN DeviceTypes dt ON d.device_type_id = dt.id
JOIN Categories c ON dt.category_id = c.id;

-- Параметры устройства с определениями
CREATE VIEW v_DeviceParametersComplete AS
SELECT
    dp.device_id,
    d.manufacturer,
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
    END as value_display,
    dp.value_text,
    dp.value_numeric,
    dp.value_boolean
FROM DeviceParameters dp
JOIN ParameterDefinitions pd ON dp.parameter_def_id = pd.id
JOIN Devices d ON dp.device_id = d.id;

-- Иерархия категорий и типов
CREATE VIEW v_CategoryHierarchy AS
SELECT
    c.id as category_id,
    c.name as category_name,
    c.code as category_code,
    dt.id as device_type_id,
    dt.name as device_type_name,
    dt.code as device_type_code,
    COUNT(d.id) as device_count
FROM Categories c
LEFT JOIN DeviceTypes dt ON c.id = dt.category_id
LEFT JOIN Devices d ON dt.id = d.device_type_id AND d.is_active = 1
GROUP BY c.id, dt.id
ORDER BY c.sort_order, dt.sort_order;

-- ============================================================================
-- 11. ВЕРСИОНИРОВАНИЕ СХЕМЫ
-- ============================================================================

CREATE TABLE SchemaVersion (
    version INTEGER PRIMARY KEY,
    description TEXT,
    applied_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

INSERT INTO SchemaVersion (version, description) VALUES (1, 'Initial schema creation');

-- ============================================================================
-- КОНЕЦ СХЕМЫ
-- ============================================================================
