@echo off
chcp 65001 >nul
REM Запуск основного скрипта run_dxf_parser.bat со всеми переданными аргументами
call "%~dp0run_dxf_parser.bat" %*
