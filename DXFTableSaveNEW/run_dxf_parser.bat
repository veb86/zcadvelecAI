@echo off
chcp 65001 >nul
title DXF Parser & Table Reader

echo ======================================================================
echo                  DXF Parser & Table Break Analyzer
echo ======================================================================
echo.

REM 1. Проверка наличия Python в системе
where python >nul 2>&1
if %ERRORLEVEL% equ 0 (
    set "PYTHON_CMD=python"
    goto :PYTHON_FOUND
)

where py >nul 2>&1
if %ERRORLEVEL% equ 0 (
    set "PYTHON_CMD=py -3"
    goto :PYTHON_FOUND
)

echo [ОШИБКА] Python не найден в переменной PATH!
echo Пожалуйста, установите Python с сайта https://python.org и отметьте
echo галочку "Add Python to PATH" при установке.
echo.
pause
exit /b 1

:PYTHON_FOUND

REM 2. Определение целевого DXF-файла
set "DXF_FILE=%~1"

if not "%DXF_FILE%"=="" goto :CHECK_FILE_EXISTS

if exist "acadtable2007.dxf" (
    set "DXF_FILE=acadtable2007.dxf"
    goto :CHECK_FILE_EXISTS
)

echo [!] DXF-файл не указан.
set /p "DXF_FILE=Введите путь к DXF-файлу: "

:CHECK_FILE_EXISTS
REM Убираем лишние кавычки, если пользователь их ввел
set "DXF_FILE=%DXF_FILE:"=%"

if exist "%DXF_FILE%" goto :RUN_SCRIPTS

echo.
echo [ОШИБКА] Файл "%DXF_FILE%" не найден!
echo.
pause
exit /b 1

:RUN_SCRIPTS
echo Обрабатываемый файл: "%DXF_FILE%"
echo.

REM 3. Запуск базового парсера DXF
echo ----------------------------------------------------------------------
echo 1. Статистика тегов и блоков - dxf_parser.py:
echo ----------------------------------------------------------------------
%PYTHON_CMD% dxf_parser.py "%DXF_FILE%"
echo.

REM 4. Запуск глубокого анализатора таблиц и Break Flags
if not exist "table_reader.py" goto :RUN_SAVER

echo ----------------------------------------------------------------------
echo 2. Детальный анализ таблиц и флагов разбиения DXF 90 - table_reader.py:
echo ----------------------------------------------------------------------
%PYTHON_CMD% table_reader.py "%DXF_FILE%"
echo.

:RUN_SAVER
REM 5. Запись таблицы в ZCADonlyline.dxf -> ZCADTABLE.dxf
if not exist "dxf_saver.py" goto :FINISH
if not exist "ZCADonlyline.dxf" (
    echo [!] Файл шаблона ZCADonlyline.dxf не найден в текущей директории.
    goto :FINISH
)

echo ----------------------------------------------------------------------
echo 3. Запись таблицы в ZCADonlyline.dxf -^> ZCADTABLE.dxf - dxf_saver.py:
echo ----------------------------------------------------------------------
%PYTHON_CMD% dxf_saver.py ZCADonlyline.dxf ZCADTABLE.dxf "%DXF_FILE%"
echo.

REM 6. Проверка полученного ZCADTABLE.dxf с помощью table_reader.py
if exist "ZCADTABLE.dxf" if exist "table_reader.py" (
    echo ----------------------------------------------------------------------
    echo 4. Контрольная проверка созданного ZCADTABLE.dxf через table_reader.py:
    echo ----------------------------------------------------------------------
    %PYTHON_CMD% table_reader.py ZCADTABLE.dxf
    echo.
)

:FINISH
echo.
echo ======================================================================
echo Обработка успешно завершена.
echo ======================================================================
echo.
pause
