{
*****************************************************************************
*                                                                           *
*  This file is part of the ZCAD                                            *
*                                                                           *
*  See the file COPYING.txt, included in this distribution,                 *
*  for details about the copyright.                                         *
*                                                                           *
*  This program is distributed in the hope that it will be useful,          *
*  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
*                                                                           *
*****************************************************************************
}
{
@author(Vladimir Bobrov)
}

{
  Модуль: uzeacadtable_manager
  Назначение: Центральный модуль подсистемы AcadTable — единая точка
  входа. Реэкспортирует основные типы и класс сущности из подмодулей,
  связывает все части подсистемы. При подключении этого модуля
  автоматически регистрируются сущность DXF и свойства инспектора.

  Зависимости (все модули подсистемы):
    uzeacadtable_types      — базовые типы и константы
    uzeacadtable_styles     — инициализация стилей
    uzeacadtable_cell       — логика ячеек
    uzeacadtable_merge      — объединение ячеек
    uzeacadtable_layout     — расчёт геометрии
    uzeacadtable_stylemanager — управление DXF-стилями
    uzeacadtable_edit       — редактирование содержимого
    uzeacadtable_dxf_read   — импорт из DXF
    uzeacadtable_dxf_write  — экспорт в DXF
    uzeacadtable_model      — основная сущность GDBObjAcadTable
    uzeacadtable_inspector  — интеграция с Object Inspector
}

unit uzeacadtable_manager;
{$Mode delphi}{$H+}
{$INCLUDE zengineconfig.inc}

interface

uses
  // Базовый слой (core)
  uzeacadtable_types,
  uzeacadtable_styles,
  uzeacadtable_cell,
  // Стили
  uzeacadtable_stylemanager,
  // Операции
  uzeacadtable_edit,
  uzeacadtable_merge,
  uzeacadtable_layout,
  // DXF
  uzeacadtable_dxf_read,
  uzeacadtable_dxf_write,
  // Сущность
  uzeacadtable_model,
  // UI
  uzeacadtable_inspector;

// Реэкспорт основных типов для удобства подключения
type
  TAcadTableBreakDir = uzeacadtable_types.TAcadTableBreakDirection;

implementation

end.
