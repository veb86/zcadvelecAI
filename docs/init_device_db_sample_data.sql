-- ============================================================================
-- Инициализация базы данных устройств с примерами данных
-- Версия: 1.0
-- Описание: Скрипт для заполнения БД начальными данными и примерами
-- ============================================================================

-- ============================================================================
-- 1. КАТЕГОРИИ
-- ============================================================================

INSERT INTO Categories (name, code, description, sort_order) VALUES
('Электроаппараты', 'ELAPP', 'Коммутационные и защитные электрические аппараты', 1),
('Кабельная продукция', 'CABLES', 'Силовые и контрольные кабели, провода', 2),
('Приборы ОПС', 'FIRE', 'Оборудование охранно-пожарной сигнализации', 3),
('Приборы автоматики', 'AUTO', 'Датчики, контроллеры, исполнительные механизмы', 4),
('Кабеленесущие системы', 'CABLEWAY', 'Лотки, короба, трубы для прокладки кабелей', 5);

-- ============================================================================
-- 2. ТИПЫ УСТРОЙСТВ
-- ============================================================================

-- Электроаппараты
INSERT INTO DeviceTypes (category_id, name, code, has_rated_current, has_voltage, has_power, sort_order) VALUES
(1, 'Автоматические выключатели', 'BREAKER', 1, 1, 0, 1),
(1, 'УЗО', 'RCD', 1, 1, 0, 2),
(1, 'Дифференциальные автоматы', 'RCBO', 1, 1, 0, 3),
(1, 'Контакторы', 'CONTACTOR', 1, 1, 0, 4),
(1, 'Реле', 'RELAY', 1, 1, 0, 5);

-- Кабельная продукция
INSERT INTO DeviceTypes (category_id, name, code, has_rated_current, has_voltage, has_cross_section, sort_order) VALUES
(2, 'Кабель силовой', 'CABLE_POWER', 1, 1, 1, 1),
(2, 'Кабель контрольный', 'CABLE_CONTROL', 0, 1, 1, 2),
(2, 'Провод', 'WIRE', 1, 1, 1, 3);

-- Приборы ОПС
INSERT INTO DeviceTypes (category_id, name, code, has_voltage, sort_order) VALUES
(3, 'Извещатель дымовой', 'SMOKE_DET', 1, 1),
(3, 'Извещатель тепловой', 'HEAT_DET', 1, 2),
(3, 'Прибор приемно-контрольный', 'FIRE_PANEL', 1, 3);

-- ============================================================================
-- 3. ОПРЕДЕЛЕНИЯ ПАРАМЕТРОВ ДЛЯ АВТОМАТИЧЕСКИХ ВЫКЛЮЧАТЕЛЕЙ
-- ============================================================================

INSERT INTO ParameterDefinitions (device_type_id, name, code, data_type, unit, is_required, sort_order) VALUES
-- Для автоматических выключателей (device_type_id = 1)
(1, 'Ток срабатывания', 'trip_current', 'REAL', 'А', 1, 1),
(1, 'Характеристика срабатывания', 'trip_curve', 'TEXT', '', 1, 2),
(1, 'Отключающая способность', 'breaking_capacity', 'REAL', 'кА', 1, 3),
(1, 'Количество полюсов', 'poles_count', 'INTEGER', 'шт', 1, 4),
(1, 'Время срабатывания в зоне КЗ', 'trip_time_sc', 'REAL', 'мс', 0, 5),
(1, 'Класс ограничения тока', 'current_limitation_class', 'INTEGER', '', 0, 6);

-- ============================================================================
-- 4. ОПРЕДЕЛЕНИЯ ПАРАМЕТРОВ ДЛЯ КАБЕЛЕЙ
-- ============================================================================

INSERT INTO ParameterDefinitions (device_type_id, name, code, data_type, unit, is_required, sort_order) VALUES
-- Для силовых кабелей (device_type_id = 6)
(6, 'Количество жил', 'core_count', 'INTEGER', 'шт', 1, 1),
(6, 'Сечение жилы', 'core_cross_section', 'REAL', 'мм²', 1, 2),
(6, 'Наружный диаметр', 'outer_diameter', 'REAL', 'мм', 0, 3),
(6, 'Длительно допустимый ток', 'continuous_current', 'REAL', 'А', 1, 4),
(6, 'Ток перегрузки', 'overload_current', 'REAL', 'А', 0, 5),
(6, 'Тип изоляции', 'insulation_type', 'TEXT', '', 0, 6),
(6, 'Материал оболочки', 'sheath_material', 'TEXT', '', 0, 7),
(6, 'Материал жилы', 'core_material', 'TEXT', '', 0, 8),
(6, 'Радиус изгиба', 'bending_radius', 'REAL', 'мм', 0, 9);

-- ============================================================================
-- 5. ПРИМЕРЫ УСТРОЙСТВ - АВТОМАТИЧЕСКИЕ ВЫКЛЮЧАТЕЛИ
-- ============================================================================

-- ABB S200 серия
INSERT INTO Devices (device_type_id, manufacturer, model, part_number, name_short, name_full,
                     rated_voltage, rated_current, standard, price, is_active) VALUES
(1, 'ABB', 'S203', 'S203-C16', 'ABB S203 C16', 'Автоматический выключатель ABB S203, характеристика C, 16А, 3P',
 400, 16, 'ГОСТ Р 50345-2010', 850.00, 1),

(1, 'ABB', 'S203', 'S203-C25', 'ABB S203 C25', 'Автоматический выключатель ABB S203, характеристика C, 25А, 3P',
 400, 25, 'ГОСТ Р 50345-2010', 950.00, 1),

(1, 'ABB', 'S201', 'S201-C10', 'ABB S201 C10', 'Автоматический выключатель ABB S201, характеристика C, 10А, 1P',
 230, 10, 'ГОСТ Р 50345-2010', 320.00, 1);

-- Schneider Electric Acti9
INSERT INTO Devices (device_type_id, manufacturer, model, part_number, name_short, name_full,
                     rated_voltage, rated_current, standard, price, is_active) VALUES
(1, 'Schneider Electric', 'iC60N', 'A9F74316', 'SE iC60N C16', 'Автоматический выключатель Schneider Electric iC60N, C, 16А, 3P',
 400, 16, 'ГОСТ Р 50345-2010', 780.00, 1),

(1, 'Schneider Electric', 'iC60N', 'A9F74325', 'SE iC60N C25', 'Автоматический выключатель Schneider Electric iC60N, C, 25А, 3P',
 400, 25, 'ГОСТ Р 50345-2010', 880.00, 1);

-- IEK ВА47-29
INSERT INTO Devices (device_type_id, manufacturer, model, part_number, name_short, name_full,
                     rated_voltage, rated_current, standard, price, is_active) VALUES
(1, 'IEK', 'ВА47-29', 'MVA20-3-016-C', 'IEK ВА47-29 C16', 'Автоматический выключатель IEK ВА47-29, C, 16А, 3P',
 400, 16, 'ГОСТ Р 50345-2010', 185.00, 1);

-- ============================================================================
-- 6. ПАРАМЕТРЫ АВТОМАТОВ
-- ============================================================================

-- ABB S203-C16 (device_id = 1)
INSERT INTO DeviceParameters (device_id, parameter_def_id, value_numeric, value_text) VALUES
(1, 1, 16, NULL),      -- Ток срабатывания 16А
(1, 2, NULL, 'C'),     -- Характеристика C
(1, 3, 6, NULL),       -- Отключающая способность 6кА
(1, 4, 3, NULL);       -- 3 полюса

-- ABB S203-C25 (device_id = 2)
INSERT INTO DeviceParameters (device_id, parameter_def_id, value_numeric, value_text) VALUES
(2, 1, 25, NULL),
(2, 2, NULL, 'C'),
(2, 3, 6, NULL),
(2, 4, 3, NULL);

-- ABB S201-C10 (device_id = 3)
INSERT INTO DeviceParameters (device_id, parameter_def_id, value_numeric, value_text) VALUES
(3, 1, 10, NULL),
(3, 2, NULL, 'C'),
(3, 3, 6, NULL),
(3, 4, 1, NULL);       -- 1 полюс

-- Schneider Electric iC60N-C16 (device_id = 4)
INSERT INTO DeviceParameters (device_id, parameter_def_id, value_numeric, value_text) VALUES
(4, 1, 16, NULL),
(4, 2, NULL, 'C'),
(4, 3, 6, NULL),
(4, 4, 3, NULL);

-- Schneider Electric iC60N-C25 (device_id = 5)
INSERT INTO DeviceParameters (device_id, parameter_def_id, value_numeric, value_text) VALUES
(5, 1, 25, NULL),
(5, 2, NULL, 'C'),
(5, 3, 6, NULL),
(5, 4, 3, NULL);

-- IEK ВА47-29-C16 (device_id = 6)
INSERT INTO DeviceParameters (device_id, parameter_def_id, value_numeric, value_text) VALUES
(6, 1, 16, NULL),
(6, 2, NULL, 'C'),
(6, 3, 4.5, NULL),     -- 4.5кА отключающая способность
(6, 4, 3, NULL);

-- ============================================================================
-- 7. ПРИМЕРЫ УСТРОЙСТВ - КАБЕЛИ
-- ============================================================================

INSERT INTO Devices (device_type_id, manufacturer, model, part_number, name_short, name_full,
                     rated_voltage, standard, unit_of_measure, price, is_active) VALUES
-- ВВГнг(А)-LS
(6, 'Камкабель', 'ВВГнг(А)-LS', 'ВВГнг(А)-LS 3х2.5', 'ВВГнг 3х2.5',
 'Кабель ВВГнг(А)-LS 3х2.5 мм² пониженной пожароопасности', 660, 'ГОСТ 31996-2012', 'м', 45.50, 1),

(6, 'Камкабель', 'ВВГнг(А)-LS', 'ВВГнг(А)-LS 3х4', 'ВВГнг 3х4',
 'Кабель ВВГнг(А)-LS 3х4 мм² пониженной пожароопасности', 660, 'ГОСТ 31996-2012', 'м', 68.00, 1),

(6, 'Камкабель', 'ВВГнг(А)-LS', 'ВВГнг(А)-LS 5х2.5', 'ВВГнг 5х2.5',
 'Кабель ВВГнг(А)-LS 5х2.5 мм² пониженной пожароопасности', 660, 'ГОСТ 31996-2012', 'м', 72.00, 1);

-- ============================================================================
-- 8. ПАРАМЕТРЫ КАБЕЛЕЙ
-- ============================================================================

-- ВВГнг 3х2.5 (device_id = 7)
INSERT INTO DeviceParameters (device_id, parameter_def_id, value_numeric, value_text) VALUES
(7, 7, 3, NULL),                -- Количество жил
(7, 8, 2.5, NULL),              -- Сечение жилы
(7, 9, 10.8, NULL),             -- Наружный диаметр
(7, 10, 25, NULL),              -- Длительно допустимый ток
(7, 12, NULL, 'ПВХ'),           -- Тип изоляции
(7, 13, NULL, 'ПВХ пониженной пожароопасности'), -- Материал оболочки
(7, 14, NULL, 'Медь');          -- Материал жилы

-- ВВГнг 3х4 (device_id = 8)
INSERT INTO DeviceParameters (device_id, parameter_def_id, value_numeric, value_text) VALUES
(8, 7, 3, NULL),
(8, 8, 4, NULL),
(8, 9, 12.1, NULL),
(8, 10, 35, NULL),
(8, 12, NULL, 'ПВХ'),
(8, 13, NULL, 'ПВХ пониженной пожароопасности'),
(8, 14, NULL, 'Медь');

-- ВВГнг 5х2.5 (device_id = 9)
INSERT INTO DeviceParameters (device_id, parameter_def_id, value_numeric, value_text) VALUES
(9, 7, 5, NULL),
(9, 8, 2.5, NULL),
(9, 9, 13.5, NULL),
(9, 10, 19, NULL),               -- ДДТ меньше из-за 5 жил
(9, 12, NULL, 'ПВХ'),
(9, 13, NULL, 'ПВХ пониженной пожароопасности'),
(9, 14, NULL, 'Медь');

-- ============================================================================
-- 9. СОВМЕСТИМОСТЬ УСТРОЙСТВ
-- ============================================================================

-- Совместимость кабелей с автоматами
-- Кабель 3х2.5 (25А) совместим с автоматами до 25А
INSERT INTO DeviceCompatibility (device_id, compatible_device_id, compatibility_type, compatibility_notes) VALUES
(7, 1, 'cable-breaker', 'Кабель выдерживает номинальный ток автомата'),
(7, 2, 'cable-breaker', 'Кабель выдерживает номинальный ток автомата'),
(7, 6, 'cable-breaker', 'Кабель выдерживает номинальный ток автомата');

-- Кабель 3х4 (35А) совместим с автоматами до 32А
INSERT INTO DeviceCompatibility (device_id, compatible_device_id, compatibility_type, compatibility_notes) VALUES
(8, 1, 'cable-breaker', 'Кабель выдерживает номинальный ток автомата'),
(8, 2, 'cable-breaker', 'Кабель выдерживает номинальный ток автомата'),
(8, 6, 'cable-breaker', 'Кабель выдерживает номинальный ток автомата');

-- ============================================================================
-- 10. ВАРИАНТЫ УСТРОЙСТВ
-- ============================================================================

-- Варианты ABB S203 по току
INSERT INTO DeviceVariants (parent_device_id, variant_name, variant_code, part_number, price_modifier) VALUES
(1, '10А', 'C10', 'S203-C10', -50.00),
(1, '20А', 'C20', 'S203-C20', 0.00),
(1, '32А', 'C32', 'S203-C32', 100.00),
(1, '40А', 'C40', 'S203-C40', 150.00);

-- ============================================================================
-- КОНЕЦ ИНИЦИАЛИЗАЦИИ
-- ============================================================================

-- Проверка: вывод статистики
SELECT 'Создано категорий: ' || COUNT(*) as info FROM Categories
UNION ALL
SELECT 'Создано типов устройств: ' || COUNT(*) FROM DeviceTypes
UNION ALL
SELECT 'Создано устройств: ' || COUNT(*) FROM Devices
UNION ALL
SELECT 'Создано определений параметров: ' || COUNT(*) FROM ParameterDefinitions
UNION ALL
SELECT 'Создано значений параметров: ' || COUNT(*) FROM DeviceParameters
UNION ALL
SELECT 'Создано связей совместимости: ' || COUNT(*) FROM DeviceCompatibility
UNION ALL
SELECT 'Создано вариантов устройств: ' || COUNT(*) FROM DeviceVariants;
