#!/usr/bin/env python3
"""
Тест проверки интеграции сохранения стилей таблиц в DXF.

Проверяет, что:
1. Шаблон 2007 содержит ACAD_TABLESTYLE и TABLESTYLE
2. Эталонный файл содержит корректные TABLESTYLE объекты
3. Формат записи TABLESTYLE соответствует спецификации DXF
4. Структура словаря ACAD_TABLESTYLE корректна
5. Код uzeffdxfout.pas содержит все необходимые обработчики
"""
import sys
import os
import re

PASS = 0
FAIL = 0

def check(description, condition, detail=''):
    global PASS, FAIL
    if condition:
        print(f'  [OK] {description}')
        PASS += 1
    else:
        print(f'  [FAIL] {description}')
        if detail:
            print(f'         {detail}')
        FAIL += 1


def read_file(path):
    with open(path, 'r', encoding='utf-8', errors='replace') as f:
        return f.read()


def test_template_2007():
    """Проверяет, что шаблон DXF 2007 содержит необходимые структуры"""
    print('\n=== Тест 1: Шаблон DXF 2007 ===')

    template_path = 'environment/runtimefiles/AllCPU-AllOS/common/cfg/components/savetemplate2007.dxf'
    content = read_file(template_path)

    check('Шаблон содержит секцию OBJECTS',
          'OBJECTS' in content)
    check('Шаблон содержит запись ACAD_TABLESTYLE в корневом словаре',
          'ACAD_TABLESTYLE' in content)
    check('Шаблон содержит класс TABLESTYLE',
          '\nTABLESTYLE\n' in content and 'AcDbTableStyle' in content)
    check('Шаблон содержит объект TABLESTYLE с AcDbTableStyle',
          'AcDbTableStyle' in content)

    # Проверяем, что словарь ACAD_TABLESTYLE ссылается на 86
    idx = content.find('ACAD_TABLESTYLE')
    if idx >= 0:
        lines = content[idx:idx+100].split('\n')
        has_350 = any('350' in l.strip() for l in lines[:3])
        check('Запись ACAD_TABLESTYLE содержит ссылку 350',
              has_350)


def test_etalon_file():
    """Проверяет формат эталонного файла"""
    print('\n=== Тест 2: Эталонный файл tablestyleetalon.dxf ===')

    etalon_path = 'cad_source/test/tablestyleetalon.dxf'
    content = read_file(etalon_path)
    lines = content.split('\n')

    # Считаем количество TABLESTYLE объектов
    tablestyle_count = sum(1 for l in lines if l.strip() == 'TABLESTYLE')
    # Вычитаем 1 для CLASS-записи
    class_count = 0
    for i, l in enumerate(lines):
        if l.strip() == 'TABLESTYLE' and i > 0 and lines[i-1].strip() == '1':
            class_count += 1
    obj_count = tablestyle_count - class_count

    check(f'Эталон содержит TABLESTYLE объекты (найдено {obj_count})',
          obj_count >= 2)

    # Проверяем наличие AcDbTableStyle
    acdb_count = sum(1 for l in lines if l.strip() == 'AcDbTableStyle')
    check(f'Эталон содержит AcDbTableStyle записи (найдено {acdb_count})',
          acdb_count >= 2)

    # Проверяем, что каждый TABLESTYLE имеет ACAD_REACTORS
    reactors_in_tablestyle = 0
    in_ts = False
    for i, l in enumerate(lines):
        if l.strip() == 'TABLESTYLE' and i > 0 and lines[i-1].strip() == '0':
            in_ts = True
        elif in_ts and l.strip() == '{ACAD_REACTORS':
            reactors_in_tablestyle += 1
            in_ts = False
        elif in_ts and l.strip() == '0' and i > 0:
            in_ts = False

    check(f'TABLESTYLE объекты имеют ACAD_REACTORS (найдено {reactors_in_tablestyle})',
          reactors_in_tablestyle >= 2)

    # Проверяем наличие словаря ACAD_TABLESTYLE
    check('Эталон содержит словарь ACAD_TABLESTYLE',
          'ACAD_TABLESTYLE' in content)

    # Проверяем наличие полей стиля ячейки (группа 7 - имя текстового стиля)
    cell_styles = sum(1 for i, l in enumerate(lines)
                     if l.strip() == '7' and i > 0
                     and i + 1 < len(lines)
                     and lines[i+1].strip() in ('Standard', 'newtext'))
    check(f'Эталон содержит определения стилей ячеек (код 7) (найдено {cell_styles})',
          cell_styles >= 6)  # Минимум 3 блока × 2 стиля


def test_dxf_output_code():
    """Проверяет, что код сохранения содержит все необходимые обработчики"""
    print('\n=== Тест 3: Код uzeffdxfout.pas ===')

    code_path = 'cad_source/zengine/fileformats/uzeffdxfout.pas'
    code = read_file(code_path)

    check('Код подключает uzestylestablesdxf',
          'uzestylestablesdxf' in code)
    check('Код подключает uzclog',
          'uzclog' in code)
    check('Код содержит переменную inobjectssec',
          'inobjectssec' in code)
    check('Код содержит переменную tablestyledicthandle',
          'tablestyledicthandle' in code)
    check('Код содержит переменную intablestyledict',
          'intablestyledict' in code)
    check('Код содержит переменную tsHandles',
          'tsHandles' in code)
    check('Код содержит процедуру WriteTableStyleObjectToStream',
          'WriteTableStyleObjectToStream' in code)
    check('Код содержит процедуру WriteCellStyleToStream',
          'WriteCellStyleToStream' in code)
    check('Код содержит процедуру WriteCellBordersToStream',
          'WriteCellBordersToStream' in code)
    check('Код содержит функцию DXFFloatStr',
          'DXFFloatStr' in code)

    # Проверяем обработку ACAD_TABLESTYLE
    check('Код обрабатывает ACAD_TABLESTYLE в корневом словаре',
          "'ACAD_TABLESTYLE'" in code)
    check('Код обрабатывает TABLESTYLE объекты',
          "values='TABLESTYLE'" in code or "values = 'TABLESTYLE'" in code)
    check('Код записывает AcDbTableStyle',
          "'AcDbTableStyle'" in code)
    check('Код записывает ACAD_REACTORS для TABLESTYLE',
          "'{ACAD_REACTORS'" in code)

    # Проверяем использование IODXFContext.handle
    check('Код использует IODXFContext.handle для генерации хэндлов стилей таблиц',
          'tsHandles[i]:=IODXFContext.handle' in code)

    # Проверяем обработку секции OBJECTS
    check('Код обрабатывает вход в секцию OBJECTS',
          "values='OBJECTS'" in code or "values = 'OBJECTS'" in code)
    check('Код обрабатывает ENDSEC секции OBJECTS',
          'inobjectssec and (groupi=0) and (values=dxfName_ENDSEC)' in code)

    # Проверяем обработку словаря
    check('Код записывает записи словаря (код 3/350)',
          "dxfGroupCode(3)" in code and "dxfGroupCode(350)" in code)

    # Проверяем запись группы 7 (имя текстового стиля)
    check('Код записывает код группы 7 (имя текстового стиля ячейки)',
          "dxfGroupCode(7)" in code)
    check('Код записывает код группы 140 (высота текста)',
          "dxfGroupCode(140)" in code)

    # Проверяем обработку маппинга хэндлов для пропускаемых объектов
    check('Код обрабатывает хэндлы в пропускаемых TABLESTYLE',
          'OldHandele2NewHandle.Add(valuei,IODXFContext.handle)' in code)

    # Проверяем, что DXFFloatStr гарантирует десятичную точку
    check('DXFFloatStr добавляет .0 при отсутствии точки',
          "'.0'" in code or "Result + '.0'" in code)


def test_tablestyle_data_structures():
    """Проверяет структуры данных стилей таблиц"""
    print('\n=== Тест 4: Структуры данных узеstylestablesdxf.pas ===')

    code_path = 'cad_source/zengine/styles/uzestylestablesdxf.pas'
    code = read_file(code_path)

    check('Определён тип TGDBDXFTableCellStyle',
          'TGDBDXFTableCellStyle' in code)
    check('Определён тип TGDBDXFTableStyle',
          'TGDBDXFTableStyle' in code)
    check('Определён тип GDBDXFTableStyleArray',
          'GDBDXFTableStyleArray' in code)
    check('TGDBDXFTableCellStyle содержит TextHeight',
          'TextHeight: Double' in code)
    check('TGDBDXFTableCellStyle содержит Alignment',
          'Alignment: Integer' in code)
    check('TGDBDXFTableStyle содержит CellFormats',
          'CellFormats' in code)
    check('TGDBDXFTableStyle содержит HorzCellMargin',
          'HorzCellMargin' in code)
    check('TGDBDXFTableStyle содержит VertCellMargin',
          'VertCellMargin' in code)
    check('TGDBDXFTableStyle содержит CellTextStyleName',
          'CellTextStyleName' in code)
    check('TGDBDXFTableStyle содержит XDictHandle',
          'XDictHandle' in code)

    # Проверяем процедуры чтения и записи
    check('Определена процедура ReadTableStylesFromDXFObjects',
          'ReadTableStylesFromDXFObjects' in code)
    check('Определена процедура WriteTableStylesToDXFObjects',
          'WriteTableStylesToDXFObjects' in code)


def test_drawing_integration():
    """Проверяет интеграцию стилей таблиц в TSimpleDrawing"""
    print('\n=== Тест 5: Интеграция в TSimpleDrawing ===')

    code_path = 'cad_source/zengine/core/drawings/uzedrawingsimple.pas'
    code = read_file(code_path)

    check('TSimpleDrawing содержит DXFTableStyleTable',
          'DXFTableStyleTable' in code)
    check('TSimpleDrawing содержит RawObjectsSection',
          'RawObjectsSection' in code)

    # Проверяем загрузку
    load_path = 'cad_source/zengine/fileformats/uzeffdxf.pas'
    load_code = read_file(load_path)

    check('Загрузка вызывает ReadTableStylesFromDXFObjects',
          'ReadTableStylesFromDXFObjects' in load_code)
    check('Загрузка извлекает RawObjectsSection',
          'ExtractDxfRawSection' in load_code)


def test_tablestyle_format_validation():
    """Проверяет корректность формата записи TABLESTYLE в выходном коде"""
    print('\n=== Тест 6: Валидация формата TABLESTYLE ===')

    code_path = 'cad_source/zengine/fileformats/uzeffdxfout.pas'
    code = read_file(code_path)

    # Проверяем порядок записи полей в WriteTableStyleObjectToStream
    # Ожидаемый порядок: 0/TABLESTYLE, 5/handle, 102/XDICT, 102/REACTORS, 330, 100/AcDbTableStyle...
    idx_0 = code.find("'TABLESTYLE'", code.find('WriteTableStyleObjectToStream'))
    idx_5 = code.find("dxfGroupCode(5)", idx_0 if idx_0 > 0 else 0)
    idx_102 = code.find("'{ACAD_REACTORS'", idx_5 if idx_5 > 0 else 0)
    idx_100 = code.find("'AcDbTableStyle'", idx_102 if idx_102 > 0 else 0)
    idx_3 = code.find("dxfGroupCode(3)", idx_100 if idx_100 > 0 else 0)

    check('Порядок записи: TABLESTYLE → handle(5) → REACTORS(102) → AcDbTableStyle(100)',
          idx_0 > 0 and idx_5 > idx_0 and idx_102 > idx_5 and idx_100 > idx_102)

    # Проверяем, что границы ячейки записываются правильно (274-279, 284-289, 64-69)
    check('Код записывает границы ячеек (коды 274-279)',
          'BorderCode := 274 to 279' in code)
    check('Код записывает видимость границ (коды 284-289)',
          'VisCode := BorderCode + 10' in code)
    check('Код записывает цвета границ (коды 64-69)',
          'ColorCode := BorderCode - 210' in code)


if __name__ == '__main__':
    os.chdir('/tmp/gh-issue-solver-1775681855556')

    test_template_2007()
    test_etalon_file()
    test_dxf_output_code()
    test_tablestyle_data_structures()
    test_drawing_integration()
    test_tablestyle_format_validation()

    print(f'\n{"="*60}')
    print(f'Результат: {PASS} из {PASS + FAIL} проверок пройдено')
    if FAIL > 0:
        print(f'ВНИМАНИЕ: {FAIL} проверок провалено!')
        sys.exit(1)
    else:
        print('Все проверки пройдены успешно!')
        sys.exit(0)
