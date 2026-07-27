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
  Модуль: uzeacadtable_types
  Назначение: Базовые типы данных для подсистемы AcadTable.
  Содержит перечисления, записи стилей, ячеек, строк, столбцов
  и константы по умолчанию. Не зависит от других модулей AcadTable.
  Зависимости: нет (только системные)
}

unit uzeacadtable_types;
{$Mode delphi}{$H+}
{$INCLUDE zengineconfig.inc}

interface

const
  // Высота строки по умолчанию (в единицах чертежа)
  CAcadTableDefaultRowHeight = 10.0;
  // Ширина столбца по умолчанию (в единицах чертежа)
  CAcadTableDefaultColWidth = 30.0;
  // Высота текста по умолчанию (в единицах чертежа)
  CAcadTableDefaultTextHeight = 2.5;
  // Максимальное количество строк/столбцов — защита от некорректных данных
  CAcadTableMaxDimension = 1000;
  // Максимальное количество ячеек — защита памяти
  CAcadTableMaxCells = 100000;

type
  // Направление разрыва таблицы
  TAcadTableBreakDirection = (atbdRight, atbdDown, atbdLeft);

  // Типы данных ячеек
  TCellDataType = (cdtText, cdtNumber, cdtFormula, cdtBlock);

  // Горизонтальное выравнивание текста в ячейке
  THorzAlign = (haLeft, haCenter, haRight);
  // Вертикальное выравнивание текста в ячейке
  TVertAlign = (vaTop, vaMiddle, vaBottom);

  // Сторона границы ячейки
  TBorderSide = (bsLeft, bsTop, bsRight, bsBottom);
  // Набор сторон границ (для одновременного задания нескольких)
  TBorderSides = set of TBorderSide;

  // Типы переопределений стиля ячейки
  TStyleOverride = (
    soTextStyle,
    soTextHeight,
    soTextColor,
    soHAlign,
    soVAlign,
    soBackground,
    soBorders
  );
  // Набор флагов переопределения стиля
  TStyleOverrides = set of TStyleOverride;

  // Стиль ячейки: значения + маска override.
  // Если флаг переопределения не установлен — брать значение от родителя.
  TCellStyle = record
    // Текстовые свойства
    TextStyle: String;
    TextHeight: Double;
    TextColor: Integer;
    // Выравнивание
    HorzAlign: THorzAlign;
    VertAlign: TVertAlign;
    // Фон ячейки
    HasBackground: Boolean;
    BackgroundColor: Integer;
    // Границы
    Borders: TBorderSides;
    BorderColor: Integer;
    // Флаги переопределения — какие свойства заданы явно
    Overrides: TStyleOverrides;
  end;

  // Табличный стиль (аналог AcDbTableStyle)
  TTableStyle = record
    Name: String;
    // Базовый стиль по умолчанию
    DefaultCell: TCellStyle;
    // Стили для различных типов строк (как в AutoCAD)
    TitleCell: TCellStyle;
    HeaderCell: TCellStyle;
    DataCell: TCellStyle;
    // Размеры по умолчанию
    DefaultRowHeight: Double;
    DefaultColWidth: Double;
  end;

  // Строка таблицы
  TTableRow = record
    Height: Double;
    Style: TCellStyle;
  end;

  // Столбец таблицы
  TTableColumn = record
    Width: Double;
    Style: TCellStyle;
  end;

  // Ячейка таблицы (данные + переопределение стиля)
  TTableCell = record
    DataType: TCellDataType;
    // Контент ячейки
    Text: String;
    Value: Double;
    Formula: String;
    // Для блоков
    BlockName: String;
    // Переопределение стиля
    Style: TCellStyle;
    // Индекс базового стиля этой ячейки: 0=Title, 1=Header, 2=Data.
    // -1 означает наследование типа от строки.
    StyleType: Integer;
    // Выравнивание ячейки из DXF (group 170 в данных ячейки AcDbTable).
    // Значения AutoCAD:
    //   1=TopLeft, 2=TopCenter, 3=TopRight,
    //   4=MiddleLeft, 5=MiddleCenter, 6=MiddleRight,
    //   7=BottomLeft, 8=BottomCenter, 9=BottomRight.
    // 0 = не задано (наследуется от стиля таблицы).
    CellAlignment: Integer;
    // Количество объединённых столбцов (group 175). 1 = нет объединения.
    ColSpan: Integer;
    // Количество объединённых строк (group 176). 1 = нет объединения.
    RowSpan: Integer;
  end;

  // Диапазон объединённых ячеек
  TMergeRange = record
    Row1, Col1: Integer;
    Row2, Col2: Integer;
  end;

  // Сегмент визуализации для разорванной таблицы
  TAcadTableRenderSegment = record
    StartRow: Integer;
    EndRow: Integer;
    OffsetX: Double;
    OffsetY: Double;
  end;

  TTableCellArray = array of array of TTableCell;

  // Именованные псевдонимы динамических массивов. Нужны, чтобы данные
  // таблицы можно было обменивать целиком (присваиванием) между объектом
  // и его частями-продолжениями (issue #1300): для оператора := FPC
  // требует идентичности типов, а не структурной совместимости.
  TTableRowArray = array of TTableRow;
  TTableColumnArray = array of TTableColumn;
  TMergeRangeArray = array of TMergeRange;
  TTableTextArray = array of String;
  // Массив размеров (ширин столбцов или высот строк) в единицах чертежа.
  // Используется для переноса размеров ячеек из электронной таблицы
  // (TsWorksheet) в таблицу ACAD_TABLE при её создании (issue #1359).
  TTableSizeArray = array of Double;
  // Массив выравниваний ячеек в кодировке AutoCAD (group 170, значения 1..9:
  // 1=TopLeft, 2=TopCenter, 3=TopRight, 4=MiddleLeft, 5=MiddleCenter,
  // 6=MiddleRight, 7=BottomLeft, 8=BottomCenter, 9=BottomRight). 0 = не задано
  // (ячейка наследует выравнивание из стиля таблицы). Индексируется как
  // строка*количество_столбцов + столбец. Используется для переноса
  // выравнивания ячеек из электронной таблицы (TsWorksheet) в таблицу
  // ACAD_TABLE при её создании (issue #1363).
  TTableAlignmentArray = array of Integer;

implementation

end.
