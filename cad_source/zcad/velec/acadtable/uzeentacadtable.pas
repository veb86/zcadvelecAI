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
  Модуль: uzeentacadtable
  Назначение: Реализация сущности ACAD_TABLE — таблицы AutoCAD в формате DXF.
  Этап 1: чтение содержимого таблицы (строки, столбцы, текст) и отображение.
  Зависимости: uzeentityfactory, uzeentcomplex, uzeentmtext, uzeentline,
               uzeconsts, uzeffdxfsupport, uzMVReader, uzclog
}

unit uzeentacadtable;
{$Mode delphi}{$H+}
{$INCLUDE zengineconfig.inc}

interface

uses
  uzgldrawcontext, uzedrawingdef, uzeentityfactory, uzeentcomplex,
  uzeentline, uzeentmtext, uzeentsubordinated, uzeentabstracttext,
  uzeentity, uzctnrVectorBytesStream, uzeTypes, uzeconsts,
  uzegeometry, uzegeometrytypes, uzeffdxfsupport, uzMVReader,
  uzbLogIntf, uzclog, SysUtils, uzctnrvectordouble,
  uzestylestablesdxf, gzctnrVectorTypes, Types,uzestylestexts;

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

  // Стиль ячейки: значения + маска override
  // Если флаг переопределения не установлен — брать значение от родителя
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

  // Ячейка таблицы (минималистично — только данные + override)
  TTableCell = record
    DataType: TCellDataType;
    // Контент ячейки
    Text: String;
    Value: Double;
    Formula: String;
    // Для блоков
    BlockName: String;
    // Переопределение стиля (applied on top of table/row/column style)
    Style: TCellStyle;
    // Выравнивание ячейки из DXF (group 170 в данных ячейки AcDbTable).
    // Значение совпадает с кодами выравнивания AutoCAD:
    //   1=TopLeft, 2=TopCenter, 3=TopRight,
    //   4=MiddleLeft, 5=MiddleCenter, 6=MiddleRight,
    //   7=BottomLeft, 8=BottomCenter, 9=BottomRight.
    // 0 = не задано (наследуется от стиля таблицы).
    CellAlignment: Integer;
    // Количество объединённых столбцов (group 175 в DXF). 1 = нет объединения.
    ColSpan: Integer;
    // Количество объединённых строк (group 176 в DXF). 1 = нет объединения.
    RowSpan: Integer;
  end;

  // Диапазон объединённых ячеек
  TMergeRange = record
    Row1, Col1: Integer;
    Row2, Col2: Integer;
  end;

  // Тип указателя на GDBObjAcadTable
  PGDBObjAcadTable = ^GDBObjAcadTable;

  // Сущность ACAD_TABLE — таблица AutoCAD из формата DXF.
  // Хранит геометрию таблицы (точку вставки, размеры строк и столбцов)
  // и текстовое содержимое ячеек.
  // При форматировании строит визуальное представление из линий и текста.
  GDBObjAcadTable = object(GDBObjComplex)
  private
    // Точка вставки таблицы
    FInsertPoint: TzePoint3d;
    // Количество строк
    FRowCount: Integer;
    // Количество столбцов
    FColCount: Integer;
    // Высоты строк (один элемент на строку)
    FRowHeights: TZctnrVectorDouble;
    // Ширины столбцов (один элемент на столбец)
    FColWidths: TZctnrVectorDouble;
    // Тексты ячеек в плоском виде: индекс = строка * FColCount + столбец
    FCellTexts: array of String;
    // Признак того, что геометрия уже была построена
    FGeometryBuilt: Boolean;

    // Хэндл DXF-стиля таблицы (group code 342) — читается при загрузке из DXF
    FTableStyleHandle: String;

    // Флаги свойств таблицы (group code 90):
    //   бит 1 (2)  = строка заголовка (title) подавлена
    //   бит 2 (4)  = строка имён столбцов (header/Название) подавлена
    //   бит 4 (16) = направление таблицы (снизу вверх)
    FTableFlags: Integer;
    // Параметры разрыва таблицы, считанные из DXF.
    FBreakEnabled: Boolean;
    FBreakDirection: TAcadTableBreakDirection;
    FBreakRepeatTopLabels: Boolean;
    FBreakRepeatBottomLabels: Boolean;
    FBreakManualPosition: Boolean;
    FBreakManualHeight: Boolean;
    FBreakSpacing: Double;

    // Новая модель данных (этап 2)
    // Стиль таблицы
    FTableStyle: TTableStyle;
    // Строки таблицы (с размерами и стилем)
    FRows: array of TTableRow;
    // Столбцы таблицы (с размерами и стилем)
    FCols: array of TTableColumn;
    // Ячейки таблицы (только данные + override)
    FCells: array of array of TTableCell;
    // Диапазоны объединённых ячеек
    FMerges: array of TMergeRange;

    // Возвращает высоту строки по индексу или значение по умолчанию
    function GetRowHeight(RowIndex: Integer): Double;
    // Возвращает ширину столбца по индексу или значение по умолчанию
    function GetColWidth(ColIndex: Integer): Double;
    // Возвращает суммарную высоту всех строк
    function GetTotalHeight: Double;
    // Возвращает суммарную ширину всех столбцов
    function GetTotalWidth: Double;
    // Возвращает текст ячейки (строка rowIdx, столбец colIdx)
    function GetCellText(RowIdx, ColIdx: Integer): String;
    // Строит визуальное представление в ConstObjArray
    procedure BuildVisualRepresentation(var ADrawing: TDrawingDef;
      var ADC: TDrawContext);
    // Добавляет ячейку в массив текстов по её линейному индексу
    procedure SetCellTextByIndex(CellIdx: Integer; const AText: String);
    // Вычисляет bounding box таблицы на основе её размеров
    procedure getoutbound(var DC: TDrawContext);
    // Проверяет, находится ли ячейка в диапазоне объединения
    function IsCellMerged(RowIdx, ColIdx: Integer): Boolean;
    // Возвращает координаты главной ячейки объединённого диапазона
    function GetMergeRoot(RowIdx, ColIdx: Integer): TPoint;
    // Возвращает ширину объединённой ячейки в единицах чертежа.
    // Для необъединённой — ширина одного столбца. Для главной в merge — сумма столбцов.
    function GetMergedCellWidth(RowIdx, ColIdx: Integer): Double;
    // Возвращает высоту объединённой ячейки в единицах чертежа.
    // Для необъединённой — высота одной строки. Для главной в merge — сумма строк.
    function GetMergedCellHeight(RowIdx, ColIdx: Integer): Double;
    // Проверяет, нужна ли вертикальная линия между ColIdx и ColIdx+1 на данной строке.
    // Возвращает False, если граница попадает внутрь объединённого диапазона.
    function IsColBorderVisible(RowIdx, ColIdx: Integer): Boolean;
    // Проверяет, нужна ли горизонтальная линия между RowIdx и RowIdx+1 на данном столбце.
    // Возвращает False, если граница попадает внутрь объединённого диапазона.
    function IsRowBorderVisible(RowIdx, ColIdx: Integer): Boolean;
    // Резолвит итоговый стиль ячейки с учётом иерархии
    function ResolveCellStyle(RowIdx, ColIdx: Integer): TCellStyle;
    // Резолвит указатель на текстовый стиль по имени с fallback к Standard/первому.
    function ResolveTextStyle(const AStyleName: String;
      var ADrawing: TDrawingDef): PGDBTextStyle;
    // Применяет DXF-стиль таблицы к внутренней структуре FTableStyle
    procedure ApplyDXFTableStyle(var ADrawing: TDrawingDef);
    // Возвращает имя стиля таблицы
    function GetTableStyleName: String;
    // Определяет базовый стиль строки с учётом флагов подавления title/header.
    // Если title-строка подавлена, индекс 0 относится к Header (Название),
    // если обе подавлены — к Data.
    function GetBaseRowStyle(RowIdx: Integer): TCellStyle;

  public
    constructor initnul(AOwner: PGDBObjGenericWithSubordinated);
    destructor done; virtual;

    procedure LoadFromDXF(var ARdr: TZMemReader; APtu: PExtensionData;
      var ADrawing: TDrawingDef;
      var AContext: TIODXFLoadContext); virtual;
    procedure SaveToDXF(var AOutStream: TZctnrVectorBytes;
      var ADrawing: TDrawingDef;
      var AIODXFContext: TIODXFSaveContext); virtual;
    procedure SaveToDXFFollow(var AOutStream: TZctnrVectorBytes;
      var ADrawing: TDrawingDef;
      var AIODXFContext: TIODXFSaveContext); virtual;
    procedure BuildGeometry(var ADrawing: TDrawingDef); virtual;
    procedure FormatEntity(var ADrawing: TDrawingDef;
      var ADC: TDrawContext;
      AStage: TEFStages = EFAllStages); virtual;
    function IsStagedFormatEntity: Boolean; virtual;
    function Clone(AOwn: Pointer): PGDBObjEntity; virtual;
    function GetObjType: TObjID; virtual;
    function GetObjTypeName: String; virtual;
    function DXFDelayedBuildGeometry: Boolean; virtual;

    // Публичные свойства для инспектора объектов
    // Точка вставки таблицы
    property InsertPoint: TzePoint3d read FInsertPoint;
    // Количество строк
    property RowCount: Integer read FRowCount;
    // Количество столбцов
    property ColCount: Integer read FColCount;
    // Суммарная ширина всех столбцов
    property Width: Double read GetTotalWidth;
    // Суммарная высота всех строк
    property Height: Double read GetTotalHeight;
    // Имя стиля таблицы
    property TableStyleName: String read GetTableStyleName;
    property BreakEnabled: Boolean read FBreakEnabled;
    property BreakDirection: TAcadTableBreakDirection read FBreakDirection;
    property BreakRepeatTopLabels: Boolean read FBreakRepeatTopLabels;
    property BreakRepeatBottomLabels: Boolean read FBreakRepeatBottomLabels;
    property BreakManualPosition: Boolean read FBreakManualPosition;
    property BreakManualHeight: Boolean read FBreakManualHeight;
    property BreakSpacing: Double read FBreakSpacing;
  end;

function AllocAcadTable: Pointer;
function AllocAndInitAcadTable(
  AOwner: PGDBObjGenericWithSubordinated): PGDBObjAcadTable;

implementation

// --- Вспомогательные приватные методы ---

// Возвращает высоту строки по индексу.
// При выходе за границы возвращает константу по умолчанию.
function GDBObjAcadTable.GetRowHeight(RowIndex: Integer): Double;
begin
  if (RowIndex >= 0) and (RowIndex < FRowHeights.Count) then
    Result := FRowHeights.parray^[RowIndex]
  else
    Result := CAcadTableDefaultRowHeight;
end;

// Возвращает ширину столбца по индексу.
// При выходе за границы возвращает константу по умолчанию.
function GDBObjAcadTable.GetColWidth(ColIndex: Integer): Double;
begin
  if (ColIndex >= 0) and (ColIndex < FColWidths.Count) then
    Result := FColWidths.parray^[ColIndex]
  else
    Result := CAcadTableDefaultColWidth;
end;

// Возвращает суммарную высоту всех строк таблицы.
function GDBObjAcadTable.GetTotalHeight: Double;
var
  RowIdx: Integer;
begin
  Result := 0;
  for RowIdx := 0 to FRowCount - 1 do
    Result := Result + GetRowHeight(RowIdx);
end;

// Возвращает суммарную ширину всех столбцов таблицы.
function GDBObjAcadTable.GetTotalWidth: Double;
var
  ColIdx: Integer;
begin
  Result := 0;
  for ColIdx := 0 to FColCount - 1 do
    Result := Result + GetColWidth(ColIdx);
end;

// Возвращает имя стиля таблицы.
function GDBObjAcadTable.GetTableStyleName: String;
begin
  Result := FTableStyle.Name;
end;

// Возвращает текст ячейки по координатам строки и столбца.
// При некорректных индексах возвращает пустую строку.
function GDBObjAcadTable.GetCellText(RowIdx, ColIdx: Integer): String;
var
  CellIndex: Integer;
begin
  Result := '';
  if FColCount <= 0 then
    Exit;
  CellIndex := RowIdx * FColCount + ColIdx;
  if (CellIndex >= 0) and (CellIndex <= High(FCellTexts)) then
    Result := FCellTexts[CellIndex];
end;

// Расширяет массив FCellTexts до нужного индекса и записывает текст.
procedure GDBObjAcadTable.SetCellTextByIndex(CellIdx: Integer;
  const AText: String);
begin
  if CellIdx < 0 then
    Exit;
  // Защита от слишком большого индекса
  if CellIdx >= CAcadTableMaxCells then
  begin
    programlog.LogOutFormatStr(
      'uzeentacadtable: SetCellTextByIndex — индекс %d превышает лимит %d',
      [CellIdx, CAcadTableMaxCells], LM_Info);
    Exit;
  end;
  if CellIdx > High(FCellTexts) then
    System.SetLength(FCellTexts, CellIdx + 1);
  FCellTexts[CellIdx] := AText;
end;

// Инициализирует стиль ячейки значениями по умолчанию.
procedure InitCellStyle(var ACellStyle: TCellStyle);
begin
  ACellStyle.TextStyle := '';
  ACellStyle.TextHeight := CAcadTableDefaultTextHeight;
  ACellStyle.TextColor := 0;
  ACellStyle.HorzAlign := haLeft;
  ACellStyle.VertAlign := vaMiddle;
  ACellStyle.HasBackground := False;
  ACellStyle.BackgroundColor := 0;
  ACellStyle.Borders := [];
  ACellStyle.BorderColor := 0;
  ACellStyle.Overrides := [];
end;

// Проверяет, находится ли ячейка в диапазоне объединения.
function GDBObjAcadTable.IsCellMerged(RowIdx, ColIdx: Integer): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to High(FMerges) do
  begin
    if (RowIdx >= FMerges[i].Row1) and (RowIdx <= FMerges[i].Row2) and
       (ColIdx >= FMerges[i].Col1) and (ColIdx <= FMerges[i].Col2) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

// Возвращает координаты главной ячейки объединённого диапазона.
// Если ячейка не в объединении — возвращает её собственные координаты.
function GDBObjAcadTable.GetMergeRoot(RowIdx, ColIdx: Integer): TPoint;
var
  i: Integer;
begin
  Result.X := ColIdx;
  Result.Y := RowIdx;
  for i := 0 to High(FMerges) do
  begin
    if (RowIdx >= FMerges[i].Row1) and (RowIdx <= FMerges[i].Row2) and
       (ColIdx >= FMerges[i].Col1) and (ColIdx <= FMerges[i].Col2) then
    begin
      Result.X := FMerges[i].Col1;
      Result.Y := FMerges[i].Row1;
      Exit;
    end;
  end;
end;

// Возвращает суммарную ширину объединённой ячейки (главной или необъединённой).
// Для главной ячейки слияния — сумма ширин всех входящих столбцов.
// Для обычной ячейки — ширина одного столбца.
function GDBObjAcadTable.GetMergedCellWidth(RowIdx, ColIdx: Integer): Double;
var
  i, Col2: Integer;
begin
  Col2 := ColIdx; // по умолчанию — только свой столбец
  for i := 0 to High(FMerges) do
  begin
    if (RowIdx >= FMerges[i].Row1) and (RowIdx <= FMerges[i].Row2) and
       (ColIdx = FMerges[i].Col1) then
    begin
      Col2 := FMerges[i].Col2;
      Break;
    end;
  end;
  Result := 0;
  for i := ColIdx to Col2 do
    Result := Result + GetColWidth(i);
end;

// Возвращает суммарную высоту объединённой ячейки (главной или необъединённой).
// Для главной ячейки слияния — сумма высот всех входящих строк.
// Для обычной ячейки — высота одной строки.
function GDBObjAcadTable.GetMergedCellHeight(RowIdx, ColIdx: Integer): Double;
var
  i, Row2: Integer;
begin
  Row2 := RowIdx;
  for i := 0 to High(FMerges) do
  begin
    if (ColIdx >= FMerges[i].Col1) and (ColIdx <= FMerges[i].Col2) and
       (RowIdx = FMerges[i].Row1) then
    begin
      Row2 := FMerges[i].Row2;
      Break;
    end;
  end;
  Result := 0;
  for i := RowIdx to Row2 do
    Result := Result + GetRowHeight(i);
end;

// Проверяет видимость горизонтальной границы между строками RowIdx и RowIdx+1
// на столбце ColIdx. Граница скрыта, если она находится внутри объединённого диапазона.
function GDBObjAcadTable.IsRowBorderVisible(RowIdx, ColIdx: Integer): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to High(FMerges) do
  begin
    if (ColIdx >= FMerges[i].Col1) and (ColIdx <= FMerges[i].Col2) and
       (RowIdx >= FMerges[i].Row1) and (RowIdx < FMerges[i].Row2) then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

// Проверяет видимость вертикальной границы между столбцами ColIdx и ColIdx+1
// на строке RowIdx. Граница скрыта, если она находится внутри объединённого диапазона.
function GDBObjAcadTable.IsColBorderVisible(RowIdx, ColIdx: Integer): Boolean;
var
  i: Integer;
begin
  Result := True;
  // Граница ColIdx/ColIdx+1 скрыта, если существует диапазон, который
  // включает строку RowIdx и содержит ColIdx < Col2 (т.е. граница внутри)
  for i := 0 to High(FMerges) do
  begin
    if (RowIdx >= FMerges[i].Row1) and (RowIdx <= FMerges[i].Row2) and
       (ColIdx >= FMerges[i].Col1) and (ColIdx < FMerges[i].Col2) then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

// Определяет базовый стиль строки по позиции строки в таблице.
// Стиль назначается только по индексу строки, независимо от FTableFlags:
//   RowIdx=0 — Title (Название, высота 38)
//   RowIdx=1 — Header (Заголовок, высота 28)
//   RowIdx>=2 — Data (Данные, высота 18)
// FTableFlags управляет отображением (скрытие строк), но не изменяет
// стилевую принадлежность строк: каждая строка сохраняет свой стиль
// независимо от флагов подавления. Для явного переопределения стиля
// на уровне ячейки используется CellAlignment (group 170 в DXF).
function GDBObjAcadTable.GetBaseRowStyle(RowIdx: Integer): TCellStyle;
begin
  case RowIdx of
    0: Result := FTableStyle.TitleCell;
    1: Result := FTableStyle.HeaderCell;
  else
    Result := FTableStyle.DataCell;
  end;
end;

// Преобразует числовое значение выравнивания AutoCAD в горизонтальный алиас.
// AutoCAD: 1=TopLeft,2=TopCenter,3=TopRight,4-6=Middle*,7-9=Bottom*
function DXFAlignToHorz(Alignment: Integer): THorzAlign;
var
  HorzPart: Integer;
begin
  // Горизонтальная часть — остаток от 3 (0=Left,1=Center,2=Right)
  HorzPart := (Alignment - 1) mod 3;
  case HorzPart of
    1: Result := haCenter;
    2: Result := haRight;
  else
    Result := haLeft;
  end;
end;

// Преобразует числовое значение выравнивания AutoCAD в вертикальный алиас.
// AutoCAD: 1-3=Top, 4-6=Middle, 7-9=Bottom
function DXFAlignToVert(Alignment: Integer): TVertAlign;
var
  VertPart: Integer;
begin
  // Вертикальная часть — округление вниз деления на 3
  VertPart := (Alignment - 1) div 3;
  case VertPart of
    1: Result := vaMiddle;
    2: Result := vaBottom;
  else
    Result := vaTop;
  end;
end;

// Заполняет TCellStyle из TGDBDXFTableCellStyle и имени текстового стиля.
// Устанавливает флаги override для всех явно заданных полей.
procedure FillCellStyleFromDXF(var CellStyle: TCellStyle;
  const ADXF: TGDBDXFTableCellStyle; const ATextStyleName: String);
begin
  InitCellStyle(CellStyle);
  // Имя текстового стиля
  CellStyle.TextStyle := ATextStyleName;
  if ATextStyleName <> '' then
    Include(CellStyle.Overrides, soTextStyle);
  // Высота текста
  if ADXF.TextHeight > 0 then
  begin
    CellStyle.TextHeight := ADXF.TextHeight;
    Include(CellStyle.Overrides, soTextHeight);
  end;
  // Цвет текста
  CellStyle.TextColor := ADXF.TextColor;
  Include(CellStyle.Overrides, soTextColor);
  // Выравнивание
  if ADXF.Alignment > 0 then
  begin
    CellStyle.HorzAlign := DXFAlignToHorz(ADXF.Alignment);
    CellStyle.VertAlign := DXFAlignToVert(ADXF.Alignment);
    Include(CellStyle.Overrides, soHAlign);
    Include(CellStyle.Overrides, soVAlign);
  end;
  // Фон
  if ADXF.BackgroundColorEnabled then
  begin
    CellStyle.HasBackground := True;
    CellStyle.BackgroundColor := ADXF.BackgroundColor;
    Include(CellStyle.Overrides, soBackground);
  end;
end;

// Применяет DXF-стиль таблицы к внутренней структуре FTableStyle.
// Ищет стиль по хэндлу FTableStyleHandle (группа DXF 342 в ACAD_TABLE).
// Если стиль с нужным хэндлом не найден — берёт первый из таблицы как запасной
// вариант для совместимости с файлами без явной ссылки.
// Если стиль найден — заполняет FTableStyle его данными для корректного
// отображения выравнивания, высоты текста и других параметров форматирования.
procedure GDBObjAcadTable.ApplyDXFTableStyle(var ADrawing: TDrawingDef);
var
  DXFStyleTable: PGDBDXFTableStyleArray;
  StylePtr: PTGDBDXFTableStyle;
  IterRec: itrec;
  CellStylePtr: PTGDBDXFTableCellStyle;
  CellFormatsIter: itrec;
  CellIdx: Integer;
begin
  DXFStyleTable := ADrawing.GetDXFTableStyleTable;
  if DXFStyleTable = nil then
  begin
    programlog.LogOutStr(
      'uzeentacadtable: ApplyDXFTableStyle — таблица DXF-стилей недоступна',
      LM_Info);
    Exit;
  end;

  if DXFStyleTable^.count = 0 then
  begin
    programlog.LogOutStr(
      'uzeentacadtable: ApplyDXFTableStyle — таблица DXF-стилей пуста',
      LM_Info);
    Exit;
  end;

  // Ищем стиль по хэндлу из ACAD_TABLE (группа 342).
  // При наличии нескольких стилей это гарантирует применение правильного стиля.
  StylePtr := DXFStyleTable^.GetStyleByHandle(FTableStyleHandle);
  if StylePtr = nil then
  begin
    // Хэндл не совпал ни с одним стилем — используем первый как запасной
    programlog.LogOutFormatStr(
      'uzeentacadtable: ApplyDXFTableStyle — стиль с хэндлом "%s" не найден, ' +
      'используем первый стиль',
      [FTableStyleHandle], LM_Info);
    StylePtr := DXFStyleTable^.beginiterate(IterRec);
    if StylePtr = nil then
      Exit;
  end;

  programlog.LogOutFormatStr(
    'uzeentacadtable: ApplyDXFTableStyle применяем стиль "%s" (хэндл=%s)',
    [StylePtr^.Name, FTableStyleHandle], LM_Info);

  FTableStyle.Name := StylePtr^.Name;

  // Заполняем стили ячеек перебором CellFormats.
  // Порядок хранения в DXF (TABLESTYLE): 0=Data, 1=Title, 2=Header.
  // Это соответствует порядку блоков с group 7 в объекте TABLESTYLE.
  CellIdx := 0;
  CellStylePtr := StylePtr^.CellFormats.beginiterate(CellFormatsIter);
  while (CellStylePtr <> nil) and (CellIdx < 3) do
  begin
    case CellIdx of
      0:
        begin
          // Первый блок в DXF — стиль Data (данные)
          FillCellStyleFromDXF(FTableStyle.DataCell, CellStylePtr^,
            StylePtr^.CellTextStyleName[0]);
          // Стиль данных используется как базовый по умолчанию
          FillCellStyleFromDXF(FTableStyle.DefaultCell, CellStylePtr^,
            StylePtr^.CellTextStyleName[0]);
        end;
      1:
        // Второй блок в DXF — стиль Title (название)
        FillCellStyleFromDXF(FTableStyle.TitleCell, CellStylePtr^,
          StylePtr^.CellTextStyleName[1]);
      2:
        // Третий блок в DXF — стиль Header (заголовок)
        FillCellStyleFromDXF(FTableStyle.HeaderCell, CellStylePtr^,
          StylePtr^.CellTextStyleName[2]);
    end;
    Inc(CellIdx);
    CellStylePtr := StylePtr^.CellFormats.iterate(CellFormatsIter);
  end;

  programlog.LogOutFormatStr(
    'uzeentacadtable: ApplyDXFTableStyle OK стиль="%s" ' +
    'title_height=%.2f header_height=%.2f data_height=%.2f',
    [FTableStyle.Name,
     FTableStyle.TitleCell.TextHeight,
     FTableStyle.HeaderCell.TextHeight,
     FTableStyle.DataCell.TextHeight], LM_Info);
end;

// Резолвит итоговый стиль ячейки с учётом иерархии.
// Приоритет: CellAlignment > Cell > Row > Column > TableStyle > Defaults.
// Высота текста определяется по позиции строки (Title/Header/Data),
// а выравнивание — по group 170 из данных ячейки (CellAlignment).
function GDBObjAcadTable.ResolveCellStyle(RowIdx, ColIdx: Integer): TCellStyle;
var
  TableCell: TTableCell;
  RowStyle: TCellStyle;
  ColStyle: TCellStyle;
  BaseStyle: TCellStyle;
begin
  // Выбираем базовый стиль по позиции строки:
  // RowIdx=0 → TitleCell, RowIdx=1 → HeaderCell, RowIdx>=2 → DataCell.
  BaseStyle := GetBaseRowStyle(RowIdx);

  // Если базовый стиль пуст (нет переопределений) — используем DefaultCell
  if BaseStyle.Overrides = [] then
    BaseStyle := FTableStyle.DefaultCell;

  // Применяем стиль строки
  if (RowIdx >= 0) and (RowIdx <= High(FRows)) then
    RowStyle := FRows[RowIdx].Style
  else
    InitCellStyle(RowStyle);

  // Применяем стиль столбца
  if (ColIdx >= 0) and (ColIdx <= High(FCols)) then
    ColStyle := FCols[ColIdx].Style
  else
    InitCellStyle(ColStyle);

  // Применяем стиль ячейки
  if (RowIdx >= 0) and (RowIdx < FRowCount) and
     (ColIdx >= 0) and (ColIdx < FColCount) and
     (Length(FCells) > RowIdx) and (Length(FCells[RowIdx]) > ColIdx) then
    TableCell := FCells[RowIdx][ColIdx]
  else
    InitCellStyle(TableCell.Style);

  // Резолвим с учётом override флагов (применяем от меньшего приоритета к большему)
  // База
  Result := BaseStyle;

  // Row
  if soTextStyle in RowStyle.Overrides then
    Result.TextStyle := RowStyle.TextStyle;
  if soTextHeight in RowStyle.Overrides then
    Result.TextHeight := RowStyle.TextHeight;
  if soTextColor in RowStyle.Overrides then
    Result.TextColor := RowStyle.TextColor;
  if soHAlign in RowStyle.Overrides then
    Result.HorzAlign := RowStyle.HorzAlign;
  if soVAlign in RowStyle.Overrides then
    Result.VertAlign := RowStyle.VertAlign;
  if soBackground in RowStyle.Overrides then
  begin
    Result.HasBackground := RowStyle.HasBackground;
    Result.BackgroundColor := RowStyle.BackgroundColor;
  end;
  if soBorders in RowStyle.Overrides then
  begin
    Result.Borders := RowStyle.Borders;
    Result.BorderColor := RowStyle.BorderColor;
  end;

  // Column
  if soTextStyle in ColStyle.Overrides then
    Result.TextStyle := ColStyle.TextStyle;
  if soTextHeight in ColStyle.Overrides then
    Result.TextHeight := ColStyle.TextHeight;
  if soTextColor in ColStyle.Overrides then
    Result.TextColor := ColStyle.TextColor;
  if soHAlign in ColStyle.Overrides then
    Result.HorzAlign := ColStyle.HorzAlign;
  if soVAlign in ColStyle.Overrides then
    Result.VertAlign := ColStyle.VertAlign;
  if soBackground in ColStyle.Overrides then
  begin
    Result.HasBackground := ColStyle.HasBackground;
    Result.BackgroundColor := ColStyle.BackgroundColor;
  end;
  if soBorders in ColStyle.Overrides then
  begin
    Result.Borders := ColStyle.Borders;
    Result.BorderColor := ColStyle.BorderColor;
  end;

  // Cell
  if soTextStyle in TableCell.Style.Overrides then
    Result.TextStyle := TableCell.Style.TextStyle;
  if soTextHeight in TableCell.Style.Overrides then
    Result.TextHeight := TableCell.Style.TextHeight;
  if soTextColor in TableCell.Style.Overrides then
    Result.TextColor := TableCell.Style.TextColor;
  if soHAlign in TableCell.Style.Overrides then
    Result.HorzAlign := TableCell.Style.HorzAlign;
  if soVAlign in TableCell.Style.Overrides then
    Result.VertAlign := TableCell.Style.VertAlign;
  if soBackground in TableCell.Style.Overrides then
  begin
    Result.HasBackground := TableCell.Style.HasBackground;
    Result.BackgroundColor := TableCell.Style.BackgroundColor;
  end;
  if soBorders in TableCell.Style.Overrides then
  begin
    Result.Borders := TableCell.Style.Borders;
    Result.BorderColor := TableCell.Style.BorderColor;
  end;

  // Выравнивание из group 170 (CellAlignment) имеет наивысший приоритет.
  // Значения AutoCAD: 1=TopLeft, 2=TopCenter, 3=TopRight,
  //   4=MiddleLeft, 5=MiddleCenter, 6=MiddleRight,
  //   7=BottomLeft, 8=BottomCenter, 9=BottomRight.
  if TableCell.CellAlignment > 0 then
  begin
    Result.HorzAlign := DXFAlignToHorz(TableCell.CellAlignment);
    Result.VertAlign := DXFAlignToVert(TableCell.CellAlignment);
    programlog.LogOutFormatStr(
      'uzeentacadtable: ResolveCellStyle Cell[%d,%d] CellAlignment=%d ' +
      'HorzAlign=%d VertAlign=%d TextHeight=%.2f',
      [RowIdx, ColIdx, TableCell.CellAlignment,
       Ord(Result.HorzAlign), Ord(Result.VertAlign),
       Result.TextHeight], LM_Info);
  end;
end;

function GDBObjAcadTable.ResolveTextStyle(const AStyleName: String;
  var ADrawing: TDrawingDef): PGDBTextStyle;
begin
  Result := nil;

  if AStyleName <> '' then
    Result := ADrawing.GetTextStyleTable^.FindStyle(AStyleName, False);

  if Result = nil then
    Result := ADrawing.GetTextStyleTable^.FindStyle('Standard', False);

  if (Result = nil) and (ADrawing.GetTextStyleTable^.Count > 0) then
    Result := PGDBTextStyle(pointer(ADrawing.GetTextStyleTable^.getDataMutable(0)));
end;

// --- Конструктор и деструктор ---

// Инициализирует сущность с нулевыми значениями.
constructor GDBObjAcadTable.initnul(AOwner: PGDBObjGenericWithSubordinated);
begin
  inherited initnul;
  FInsertPoint := NulVertex;
  FRowCount := 0;
  FColCount := 0;
  FRowHeights.initnul;
  FColWidths.initnul;
  System.SetLength(FCellTexts, 0);
  FGeometryBuilt := False;
  FTableStyleHandle := '';
  FTableFlags := 0;
  FBreakEnabled := False;
  FBreakDirection := atbdRight;
  FBreakRepeatTopLabels := False;
  FBreakRepeatBottomLabels := False;
  FBreakManualPosition := False;
  FBreakManualHeight := False;
  FBreakSpacing := 0;

  // Инициализация новых структур данных (этап 2)
  InitCellStyle(FTableStyle.DefaultCell);
  InitCellStyle(FTableStyle.TitleCell);
  InitCellStyle(FTableStyle.HeaderCell);
  InitCellStyle(FTableStyle.DataCell);
  FTableStyle.DefaultRowHeight := CAcadTableDefaultRowHeight;
  FTableStyle.DefaultColWidth := CAcadTableDefaultColWidth;
  System.SetLength(FRows, 0);
  System.SetLength(FCols, 0);
  System.SetLength(FCells, 0, 0);
  System.SetLength(FMerges, 0);
end;

// Освобождает динамически выделенные ресурсы.
destructor GDBObjAcadTable.done;
begin
  FRowHeights.done;
  FColWidths.done;
  System.SetLength(FCellTexts, 0);
  // Освобождение новых структур данных (этап 2)
  System.SetLength(FRows, 0);
  System.SetLength(FCols, 0);
  System.SetLength(FCells, 0, 0);
  System.SetLength(FMerges, 0);
  inherited done;
end;

// --- Загрузка из DXF ---

// Загружает данные таблицы из DXF-потока.
// Формат ACAD_TABLE: точка вставки (10,20,30), количество строк (91),
// количество столбцов (92), высоты строк (141), ширины столбцов (142),
// текст ячеек (1). Чтение завершается при достижении кода 0.
procedure GDBObjAcadTable.LoadFromDXF(var ARdr: TZMemReader;
  APtu: PExtensionData; var ADrawing: TDrawingDef;
  var AContext: TIODXFLoadContext);
var
  GroupCode: Integer;
  CellText: String;
  DoubleVal: Double;
  CellIndex: Integer;
  SubclassMarker: String;
  RowHeightsRead, ColWidthsRead: Boolean;
  InCellData: Boolean;
  CellTextRead: Boolean;
  RowHeightCount, ColWidthCount: Integer;
  // Флаг: текущая ячейка является виртуальной (объединённой, не главной)
  CellIsVirtual: Boolean;
  // Выравнивание текущей ячейки из group code 170
  CellAlignmentVal: Integer;
  // Количество объединённых столбцов текущей ячейки (group 175)
  CellColSpan: Integer;
  // Количество объединённых строк текущей ячейки (group 176)
  CellRowSpan: Integer;
  // Плоский массив флагов виртуальности ячеек (индекс = строка * FColCount + столбец)
  CellVirtualFlags: array of Boolean;
  // Плоский массив выравнивания ячеек (group 170)
  CellAlignments: array of Integer;
  // Плоский массив количества объединённых столбцов (group 175)
  CellColSpans: array of Integer;
  // Плоский массив количества объединённых строк (group 176)
  CellRowSpans: array of Integer;
  MergeRange: TMergeRange;
  RowIdx, ColIdx: Integer;
begin
  programlog.LogOutStr('uzeentacadtable: LoadFromDXF START', LM_Info);

  CellIndex := 0;
  GroupCode := ARdr.ParseInteger;
  RowHeightsRead := False;
  ColWidthsRead := False;
  InCellData := False;
  CellTextRead := False;
  CellIsVirtual := False;
  CellAlignmentVal := 0;
  CellColSpan := 1;
  CellRowSpan := 1;
  RowHeightCount := 0;
  ColWidthCount := 0;
  System.SetLength(CellVirtualFlags, 0);
  System.SetLength(CellAlignments, 0);
  System.SetLength(CellColSpans, 0);
  System.SetLength(CellRowSpans, 0);

  // Пропускаем до первого 100
  while (GroupCode <> 0) and (GroupCode <> 100) do
  begin
    ARdr.SkipString;
    GroupCode := ARdr.ParseInteger;
  end;

  // Читаем первый маркер подкласса (AcDbEntity)
  if GroupCode = 100 then
  begin
    SubclassMarker := ARdr.ParseString;
    GroupCode := ARdr.ParseInteger;
  end;

  // Пропускаем AcDbEntity до AcDbBlockReference
  while (GroupCode <> 0) and (GroupCode <> 100) do
  begin
    if not LoadFromDXFObjShared(ARdr, GroupCode, APtu, ADrawing, AContext) then
      ARdr.SkipString;
    GroupCode := ARdr.ParseInteger;
  end;

  // Читаем AcDbBlockReference
  if GroupCode = 100 then
  begin
    SubclassMarker := ARdr.ParseString;
    GroupCode := ARdr.ParseInteger;

    // Читаем данные AcDbBlockReference (включая точку вставки)
    while (GroupCode <> 0) and (GroupCode <> 100) do
    begin
      case GroupCode of
        10: FInsertPoint.x := ARdr.ParseDouble;
        20: FInsertPoint.y := ARdr.ParseDouble;
        30: FInsertPoint.z := ARdr.ParseDouble;
      else
        if not LoadFromDXFObjShared(ARdr, GroupCode, APtu, ADrawing, AContext) then
          ARdr.SkipString;
      end;
      GroupCode := ARdr.ParseInteger;
    end;
  end;

  // Читаем AcDbTable (настоящие данные таблицы)
  if GroupCode = 100 then
  begin
    SubclassMarker := ARdr.ParseString;
    GroupCode := ARdr.ParseInteger;
  end;

  // Теперь читаем данные таблицы
  while GroupCode <> 0 do
  begin
    // Пробуем общую обработку (хэндл, слой, цвет и т.д.)
    if not LoadFromDXFObjShared(ARdr, GroupCode, APtu, ADrawing, AContext) then
    begin
      case GroupCode of
        // Точка вставки
        10: FInsertPoint.x := ARdr.ParseDouble;
        20: FInsertPoint.y := ARdr.ParseDouble;
        30: FInsertPoint.z := ARdr.ParseDouble;

        // Количество строк — читаем ТОЛЬКО первый раз (до высот строк)
        91:
        begin
          if not RowHeightsRead then
          begin
            FRowCount := ARdr.ParseInteger;
            if (FRowCount < 0) or (FRowCount > CAcadTableMaxDimension) then
              FRowCount := 0;
            programlog.LogOutFormatStr(
              'uzeentacadtable: LoadFromDXF RowCount=%d',
              [FRowCount], LM_Info);
          end
          else
            ARdr.SkipString; // Это флаг override ячейки
        end;

        // Количество столбцов — читаем ТОЛЬКО первый раз (до высот строк)
        92:
        begin
          if not RowHeightsRead then
          begin
            FColCount := ARdr.ParseInteger;
            if (FColCount < 0) or (FColCount > CAcadTableMaxDimension) then
              FColCount := 0;
            programlog.LogOutFormatStr(
              'uzeentacadtable: LoadFromDXF ColCount=%d',
              [FColCount], LM_Info);
          end
          else
            ARdr.SkipString; // Это флаг override ячейки
        end;

        // Параметры разрыва таблицы из основного блока AcDbTable.
        292:
        begin
          if not InCellData then
            FBreakEnabled := ARdr.ParseInteger <> 0
          else
            ARdr.SkipString;
        end;

        282:
        begin
          if not InCellData then
          begin
            case ARdr.ParseInteger of
              2: FBreakDirection := atbdDown;
              3: FBreakDirection := atbdLeft;
            else
              FBreakDirection := atbdRight;
            end;
          end
          else
            ARdr.SkipString;
        end;

        291:
        begin
          if not InCellData then
            FBreakRepeatTopLabels := ARdr.ParseInteger <> 0
          else
            ARdr.SkipString;
        end;

        294:
        begin
          if not InCellData then
            FBreakRepeatBottomLabels := ARdr.ParseInteger <> 0
          else
            ARdr.SkipString;
        end;

        293:
        begin
          if not InCellData then
            FBreakManualPosition := ARdr.ParseInteger <> 0
          else
            ARdr.SkipString;
        end;

        295:
        begin
          if not InCellData then
            FBreakManualHeight := ARdr.ParseInteger <> 0
          else
            ARdr.SkipString;
        end;

        146:
        begin
          if not InCellData then
            FBreakSpacing := ARdr.ParseDouble
          else
            ARdr.SkipString;
        end;

        // Высота строки (повторяется FRowCount раз)
        141:
        begin
          DoubleVal := ARdr.ParseDouble;
          if DoubleVal <= 0 then
            DoubleVal := CAcadTableDefaultRowHeight;
          FRowHeights.PushBackData(DoubleVal);
          RowHeightsRead := True;
          Inc(RowHeightCount);
          programlog.LogOutFormatStr(
            'uzeentacadtable: LoadFromDXF RowHeight[%d]=%.2f',
            [RowHeightCount - 1, DoubleVal], LM_Info);
        end;

        // Ширина столбца (повторяется FColCount раз)
        142:
        begin
          DoubleVal := ARdr.ParseDouble;
          if DoubleVal <= 0 then
            DoubleVal := CAcadTableDefaultColWidth;
          FColWidths.PushBackData(DoubleVal);
          ColWidthsRead := True;
          Inc(ColWidthCount);
          programlog.LogOutFormatStr(
            'uzeentacadtable: LoadFromDXF ColWidth[%d]=%.2f',
            [ColWidthCount - 1, DoubleVal], LM_Info);
        end;

        // Начало данных ячейки — код 171 (тип ячейки)
        171:
        begin
          InCellData := True;
          CellTextRead := False;
          CellIsVirtual := False;
          CellAlignmentVal := 0;
          CellColSpan := 1;
          CellRowSpan := 1;
          ARdr.SkipString;
        end;

        // Флаг виртуальной (объединённой вспомогательной) ячейки — код 173.
        // Значение 1 означает, что ячейка является частью объединения и не является
        // главной ячейкой диапазона. Такие ячейки не отображаются отдельно.
        173:
        begin
          if InCellData then
            CellIsVirtual := ARdr.ParseInteger <> 0
          else
            ARdr.SkipString;
        end;

        // Выравнивание ячейки — код 170 (в контексте данных ячейки AcDbTable).
        // Значения AutoCAD: 1=TopLeft, 2=TopCenter, 3=TopRight,
        //   4=MiddleLeft, 5=MiddleCenter, 6=MiddleRight,
        //   7=BottomLeft, 8=BottomCenter, 9=BottomRight.
        // Читается только внутри блока ячейки (после 171), вне — пропускается.
        170:
        begin
          if InCellData then
            CellAlignmentVal := ARdr.ParseInteger
          else
            ARdr.SkipString;
        end;

        // Количество объединённых столбцов — код 175 (в контексте данных ячейки).
        // Значение 1 = нет объединения по столбцам, >1 = объединено N столбцов.
        175:
        begin
          if InCellData then
            CellColSpan := ARdr.ParseInteger
          else
            ARdr.SkipString;
        end;

        // Количество объединённых строк — код 176 (в контексте данных ячейки).
        // Значение 1 = нет объединения по строкам, >1 = объединено N строк.
        176:
        begin
          if InCellData then
            CellRowSpan := ARdr.ParseInteger
          else
            ARdr.SkipString;
        end;

        // Текст ячейки — код 302 (DXF 2007+) — приоритет
        302:
        begin
          if InCellData and not CellTextRead then
          begin
            dxfLoadString(ARdr, CellText, AContext.Header);
            SetCellTextByIndex(CellIndex, CellText);
            // Сохраняем данные ячейки в плоские массивы
            if CellIndex >= Length(CellVirtualFlags) then
              System.SetLength(CellVirtualFlags, CellIndex + 1);
            CellVirtualFlags[CellIndex] := CellIsVirtual;
            if CellIndex >= Length(CellAlignments) then
              System.SetLength(CellAlignments, CellIndex + 1);
            CellAlignments[CellIndex] := CellAlignmentVal;
            if CellIndex >= Length(CellColSpans) then
              System.SetLength(CellColSpans, CellIndex + 1);
            CellColSpans[CellIndex] := CellColSpan;
            if CellIndex >= Length(CellRowSpans) then
              System.SetLength(CellRowSpans, CellIndex + 1);
            CellRowSpans[CellIndex] := CellRowSpan;
            programlog.LogOutFormatStr(
              'uzeentacadtable: LoadFromDXF Cell[%d] text="%s" ' +
              'virtual=%d align=%d colSpan=%d rowSpan=%d',
              [CellIndex, CellText, Ord(CellIsVirtual),
               CellAlignmentVal, CellColSpan, CellRowSpan], LM_Info);
            Inc(CellIndex);
            CellTextRead := True;
          end
          else
            ARdr.SkipString;
        end;

        // Текст ячейки (старый формат, код 1) — читаем только если 302 не было
        1:
        begin
          if InCellData and not CellTextRead then
          begin
            dxfLoadString(ARdr, CellText, AContext.Header);
            SetCellTextByIndex(CellIndex, CellText);
            // Сохраняем данные ячейки в плоские массивы
            if CellIndex >= Length(CellVirtualFlags) then
              System.SetLength(CellVirtualFlags, CellIndex + 1);
            CellVirtualFlags[CellIndex] := CellIsVirtual;
            if CellIndex >= Length(CellAlignments) then
              System.SetLength(CellAlignments, CellIndex + 1);
            CellAlignments[CellIndex] := CellAlignmentVal;
            if CellIndex >= Length(CellColSpans) then
              System.SetLength(CellColSpans, CellIndex + 1);
            CellColSpans[CellIndex] := CellColSpan;
            if CellIndex >= Length(CellRowSpans) then
              System.SetLength(CellRowSpans, CellIndex + 1);
            CellRowSpans[CellIndex] := CellRowSpan;
            programlog.LogOutFormatStr(
              'uzeentacadtable: LoadFromDXF Cell[%d] text="%s" ' +
              'virtual=%d align=%d colSpan=%d rowSpan=%d',
              [CellIndex, CellText, Ord(CellIsVirtual),
               CellAlignmentVal, CellColSpan, CellRowSpan], LM_Info);
            Inc(CellIndex);
            CellTextRead := True;
          end
          else
            ARdr.SkipString;
        end;

        // Флаги свойств таблицы (group code 90):
        //   бит 1 (2)  = строка заголовка (title) подавлена
        //   бит 2 (4)  = строка имён столбцов (header/Название) подавлена
        //   бит 4 (16) = направление потока таблицы (снизу вверх)
        // Читаем только до RowHeightsRead: после него 90 означает флаги ячейки.
        90:
        begin
          if not RowHeightsRead then
          begin
            FTableFlags := ARdr.ParseInteger;
            programlog.LogOutFormatStr(
              'uzeentacadtable: LoadFromDXF TableFlags=0x%x (titleSup=%d headerSup=%d)',
              [FTableFlags, Ord((FTableFlags and 2) <> 0),
               Ord((FTableFlags and 4) <> 0)], LM_Info);
          end
          else
            ARdr.SkipString; // Флаг содержимого ячейки — пропускаем
        end;

        // Хэндл стиля таблицы (ссылка на объект TABLESTYLE)
        342:
        begin
          FTableStyleHandle := ARdr.ParseString;
          programlog.LogOutFormatStr(
            'uzeentacadtable: LoadFromDXF TableStyleHandle="%s"',
            [FTableStyleHandle], LM_Info);
        end;

        // Пропускаем служебные коды ячеек
        // (170, 173, 175, 176 обрабатываются выше отдельно)
        11, 21, 31, 93, 94, 95, 96, 172, 174, 177, 178,
        145, 300, 301, 304, 274, 275, 276, 278, 279, 280, 281, 283, 284, 285, 286,
        288, 289, 63, 64, 65, 66, 68, 69, 70, 40, 41, 343, 344, 340:
          ARdr.SkipString;

      else
        // Неизвестный код — пропускаем значение
        ARdr.SkipString;
      end;
    end;

    GroupCode := ARdr.ParseInteger;
  end;

  // Координаты вставки переносим в Local для корректного позиционирования
  Local.P_insert := FInsertPoint;

  // Инициализация новых структур данных (этап 2)
  // Инициализируем табличный стиль
  InitCellStyle(FTableStyle.DefaultCell);
  InitCellStyle(FTableStyle.TitleCell);
  InitCellStyle(FTableStyle.HeaderCell);
  InitCellStyle(FTableStyle.DataCell);
  FTableStyle.DefaultRowHeight := CAcadTableDefaultRowHeight;
  FTableStyle.DefaultColWidth := CAcadTableDefaultColWidth;

  // Инициализируем массивы строк и столбцов
  if (FRowCount > 0) and (FColCount > 0) then
  begin
    // Инициализируем строки
    System.SetLength(FRows, FRowCount);
    for RowHeightCount := 0 to FRowCount - 1 do
    begin
      FRows[RowHeightCount].Height := GetRowHeight(RowHeightCount);
      InitCellStyle(FRows[RowHeightCount].Style);
    end;

    // Инициализируем столбцы
    System.SetLength(FCols, FColCount);
    for ColWidthCount := 0 to FColCount - 1 do
    begin
      FCols[ColWidthCount].Width := GetColWidth(ColWidthCount);
      InitCellStyle(FCols[ColWidthCount].Style);
    end;

    // Инициализируем ячейки (2D массив)
    System.SetLength(FCells, FRowCount, FColCount);
    for RowIdx := 0 to FRowCount - 1 do
      for ColIdx := 0 to FColCount - 1 do
      begin
        FCells[RowIdx][ColIdx].DataType := cdtText;
        FCells[RowIdx][ColIdx].Text :=
          GetCellText(RowIdx, ColIdx);
        FCells[RowIdx][ColIdx].Value := 0;
        FCells[RowIdx][ColIdx].Formula := '';
        FCells[RowIdx][ColIdx].BlockName := '';
        FCells[RowIdx][ColIdx].CellAlignment := 0;
        FCells[RowIdx][ColIdx].ColSpan := 1;
        FCells[RowIdx][ColIdx].RowSpan := 1;
        InitCellStyle(FCells[RowIdx][ColIdx].Style);
        // Копируем данные из временных массивов
        CellIndex := RowIdx * FColCount + ColIdx;
        if CellIndex < Length(CellAlignments) then
          FCells[RowIdx][ColIdx].CellAlignment :=
            CellAlignments[CellIndex];
        if CellIndex < Length(CellColSpans) then
          FCells[RowIdx][ColIdx].ColSpan :=
            CellColSpans[CellIndex];
        if CellIndex < Length(CellRowSpans) then
          FCells[RowIdx][ColIdx].RowSpan :=
            CellRowSpans[CellIndex];
      end;

    // Строим массив объединений FMerges из ColSpan и RowSpan.
    // Каждая невиртуальная ячейка с ColSpan>1 или RowSpan>1 формирует
    // диапазон объединения. Виртуальные ячейки (173=1) не создают диапазонов.
    System.SetLength(FMerges, 0);
    for RowIdx := 0 to FRowCount - 1 do
      for ColIdx := 0 to FColCount - 1 do
      begin
        CellIndex := RowIdx * FColCount + ColIdx;
        // Пропускаем виртуальные ячейки
        if (CellIndex < Length(CellVirtualFlags)) and
           CellVirtualFlags[CellIndex] then
          Continue;
        // Проверяем, есть ли объединение
        if (FCells[RowIdx][ColIdx].ColSpan > 1) or
           (FCells[RowIdx][ColIdx].RowSpan > 1) then
        begin
          MergeRange.Row1 := RowIdx;
          MergeRange.Col1 := ColIdx;
          MergeRange.Row2 :=
            RowIdx + FCells[RowIdx][ColIdx].RowSpan - 1;
          MergeRange.Col2 :=
            ColIdx + FCells[RowIdx][ColIdx].ColSpan - 1;
          // Ограничиваем рамками таблицы
          if MergeRange.Row2 >= FRowCount then
            MergeRange.Row2 := FRowCount - 1;
          if MergeRange.Col2 >= FColCount then
            MergeRange.Col2 := FColCount - 1;
          System.SetLength(FMerges, Length(FMerges) + 1);
          FMerges[High(FMerges)] := MergeRange;
          programlog.LogOutFormatStr(
            'uzeentacadtable: LoadFromDXF Merge[%d] ' +
            '(%d,%d)-(%d,%d) colSpan=%d rowSpan=%d',
            [High(FMerges), MergeRange.Row1, MergeRange.Col1,
             MergeRange.Row2, MergeRange.Col2,
             FCells[RowIdx][ColIdx].ColSpan,
             FCells[RowIdx][ColIdx].RowSpan], LM_Info);
        end;
      end;
  end;

  programlog.LogOutFormatStr(
    'uzeentacadtable: LoadFromDXF END rows=%d cols=%d cells=%d row_heights=%d col_widths=%d merges=%d',
    [FRowCount, FColCount, Length(FCellTexts), FRowHeights.Count, FColWidths.Count,
     Length(FMerges)], LM_Info);
  programlog.LogOutFormatStr(
    'uzeentacadtable: BreakData enabled=%d dir=%d repeatTop=%d repeatBottom=%d manualPos=%d manualHeight=%d spacing=%.3f',
    [Ord(FBreakEnabled), Ord(FBreakDirection), Ord(FBreakRepeatTopLabels),
     Ord(FBreakRepeatBottomLabels), Ord(FBreakManualPosition),
     Ord(FBreakManualHeight), FBreakSpacing], LM_Info);
  programlog.LogOutFormatStr(
    'uzeentacadtable: LoadFromDXF new structures: FRows=%d FCols=%d FCells[0,0] initialized',
    [Length(FRows), Length(FCols)], LM_Info);
end;

// --- Построение визуального представления ---

// Строит линии сетки и тексты ячеек в ConstObjArray.
// Горизонтальные линии — разделители строк (FRowCount + 1 линия).
// Вертикальные линии — разделители столбцов (FColCount + 1 линия).
// Текст добавляется только для непустых ячеек.
// Каждая созданная сущность форматируется через FormatEntity.
procedure GDBObjAcadTable.BuildVisualRepresentation(
  var ADrawing: TDrawingDef;
  var ADC: TDrawContext);
var
  RowIdx, ColIdx: Integer;
  CurrentY, CurrentX: Double;
  TotalWidth, TotalHeight: Double;
  RowH, ColW: Double;
  PLine: PGDBObjLine;
  PMText: PGDBObjMText;
  CellStr: String;
  LineCount, TextCount: Integer;
  CellStyle: TCellStyle;
  TextHeightLocal: Double;
begin
  programlog.LogOutFormatStr(
    'uzeentacadtable: BuildVisualRepresentation START rows=%d cols=%d',
    [FRowCount, FColCount], LM_Info);
  programlog.LogOutFormatStr(
    'uzeentacadtable: BuildVisualRepresentation ConstObjArray before clear=%d',
    [ConstObjArray.Count], LM_Info);
  ConstObjArray.Free;
  programlog.LogOutFormatStr(
    'uzeentacadtable: BuildVisualRepresentation ConstObjArray after clear=%d',
    [ConstObjArray.Count], LM_Info);

  // Пустая таблица — ничего не рисуем
  if (FRowCount <= 0) or (FColCount <= 0) then
  begin
    programlog.LogOutStr(
      'uzeentacadtable: BuildVisualRepresentation — таблица пуста',
      LM_Info);
    Exit;
  end;

  TotalWidth := GetTotalWidth;
  TotalHeight := GetTotalHeight;
  programlog.LogOutFormatStr(
    'uzeentacadtable: BuildVisualRepresentation TotalWidth=%.2f TotalHeight=%.2f',
    [TotalWidth, TotalHeight], LM_Info);

  LineCount := 0;
  TextCount := 0;

  // --- Горизонтальные линии с учётом вертикально объединённых ячеек ---
  // Верхняя и нижняя границы (RowIdx=0, RowIdx=FRowCount) рисуются полностью.
  // Внутренние границы разрываются в местах вертикальных объединений.
  CurrentY := 0;
  for RowIdx := 0 to FRowCount do
  begin
    if (RowIdx = 0) or (RowIdx = FRowCount) then
    begin
      // Крайние линии — рисуем полностью
      pointer(PLine) := ConstObjArray.CreateInitObj(GDBLineID, @Self);
      PLine^.CoordInOCS.lBegin.x := 0;
      PLine^.CoordInOCS.lBegin.y := -CurrentY;
      PLine^.CoordInOCS.lBegin.z := 0;
      PLine^.CoordInOCS.lEnd.x := TotalWidth;
      PLine^.CoordInOCS.lEnd.y := -CurrentY;
      PLine^.CoordInOCS.lEnd.z := 0;
      CopyVPto(PLine^);
      PLine^.FormatEntity(ADrawing, ADC);
      Inc(LineCount);
    end
    else
    begin
      // Внутренняя горизонтальная граница — разрываем при вертикальных объединениях
      CurrentX := 0;
      for ColIdx := 0 to FColCount - 1 do
      begin
        ColW := GetColWidth(ColIdx);
        // IsRowBorderVisible проверяет, что граница RowIdx-1/RowIdx в столбце ColIdx
        // не попадает внутрь вертикально объединённого диапазона
        if IsRowBorderVisible(RowIdx - 1, ColIdx) then
        begin
          pointer(PLine) := ConstObjArray.CreateInitObj(
            GDBLineID, @Self);
          PLine^.CoordInOCS.lBegin.x := CurrentX;
          PLine^.CoordInOCS.lBegin.y := -CurrentY;
          PLine^.CoordInOCS.lBegin.z := 0;
          PLine^.CoordInOCS.lEnd.x := CurrentX + ColW;
          PLine^.CoordInOCS.lEnd.y := -CurrentY;
          PLine^.CoordInOCS.lEnd.z := 0;
          CopyVPto(PLine^);
          PLine^.FormatEntity(ADrawing, ADC);
          Inc(LineCount);
        end;
        CurrentX := CurrentX + ColW;
      end;
    end;

    if RowIdx < FRowCount then
      CurrentY := CurrentY + GetRowHeight(RowIdx);
  end;
  programlog.LogOutFormatStr(
    'uzeentacadtable: BuildVisualRepresentation ' +
    'created %d horizontal line segments',
    [LineCount], LM_Info);

  // --- Вертикальные линии с учётом объединённых ячеек ---
  // Для каждого граничного положения по X рисуем вертикальные отрезки только
  // там, где граница не попадает внутрь объединённого диапазона.
  // Крайние линии (ColIdx=0 и ColIdx=FColCount) всегда рисуются полностью.
  LineCount := 0;
  CurrentX := 0;
  for ColIdx := 0 to FColCount do
  begin
    // Крайние линии (левая/правая рамка) — рисуем всегда полностью
    if (ColIdx = 0) or (ColIdx = FColCount) then
    begin
      pointer(PLine) := ConstObjArray.CreateInitObj(GDBLineID, @Self);
      PLine^.CoordInOCS.lBegin.x := CurrentX;
      PLine^.CoordInOCS.lBegin.y := 0;
      PLine^.CoordInOCS.lBegin.z := 0;
      PLine^.CoordInOCS.lEnd.x   := CurrentX;
      PLine^.CoordInOCS.lEnd.y   := -TotalHeight;
      PLine^.CoordInOCS.lEnd.z   := 0;
      CopyVPto(PLine^);
      PLine^.FormatEntity(ADrawing, ADC);
      Inc(LineCount);
    end
    else
    begin
      // Внутренняя граница: рисуем отрезками, разрывая там, где есть объединение.
      // Для каждой строки проверяем, видна ли граница ColIdx-1/ColIdx.
      CurrentY := 0;
      for RowIdx := 0 to FRowCount - 1 do
      begin
        RowH := GetRowHeight(RowIdx);
        // IsColBorderVisible проверяет, что граница ColIdx-1/ColIdx в строке RowIdx
        // не попадает внутрь объединённого диапазона
        if IsColBorderVisible(RowIdx, ColIdx - 1) then
        begin
          pointer(PLine) := ConstObjArray.CreateInitObj(GDBLineID, @Self);
          PLine^.CoordInOCS.lBegin.x := CurrentX;
          PLine^.CoordInOCS.lBegin.y := -CurrentY;
          PLine^.CoordInOCS.lBegin.z := 0;
          PLine^.CoordInOCS.lEnd.x   := CurrentX;
          PLine^.CoordInOCS.lEnd.y   := -(CurrentY + RowH);
          PLine^.CoordInOCS.lEnd.z   := 0;
          CopyVPto(PLine^);
          PLine^.FormatEntity(ADrawing, ADC);
          Inc(LineCount);
        end;
        CurrentY := CurrentY + RowH;
      end;
    end;

    if ColIdx < FColCount then
      CurrentX := CurrentX + GetColWidth(ColIdx);
  end;
  programlog.LogOutFormatStr(
    'uzeentacadtable: BuildVisualRepresentation created %d vertical line segments',
    [LineCount], LM_Info);

  // --- Текст ячеек ---
  CurrentY := 0;
  TextCount := 0;
  for RowIdx := 0 to FRowCount - 1 do
  begin
    RowH := GetRowHeight(RowIdx);
    CurrentX := 0;

    for ColIdx := 0 to FColCount - 1 do
    begin
      ColW := GetColWidth(ColIdx);

      // Если ячейка объединённая и не является главной — пропускаем
      if IsCellMerged(RowIdx, ColIdx) then
      begin
        if not ((RowIdx = GetMergeRoot(RowIdx, ColIdx).Y) and
                (ColIdx = GetMergeRoot(RowIdx, ColIdx).X)) then
        begin
          CurrentX := CurrentX + ColW;
          Continue;
        end;
      end;

      // Получаем текст из новой структуры FCells или из старой FCellTexts
      CellStr := '';
      if (Length(FCells) > RowIdx) and (Length(FCells[RowIdx]) > ColIdx) then
        CellStr := FCells[RowIdx][ColIdx].Text
      else
        CellStr := GetCellText(RowIdx, ColIdx);

      // Размеры ячейки с учётом объединения (сумма столбцов и строк)
      ColW := GetMergedCellWidth(RowIdx, ColIdx);
      RowH := GetMergedCellHeight(RowIdx, ColIdx);

      programlog.LogOutFormatStr(
        'uzeentacadtable: BuildVisualRepresentation ' +
        'Cell[%d,%d] text="%s" ColW=%.2f RowH=%.2f',
        [RowIdx, ColIdx, CellStr, ColW, RowH], LM_Info);

      if CellStr <> '' then
      begin
        // Резолвим стиль
        CellStyle := ResolveCellStyle(RowIdx, ColIdx);

        pointer(PMText) :=
          ConstObjArray.CreateInitObj(GDBMTextID, @Self);
        PMText^.Template := UTF8ToString(CellStr);

        // Используем высоту текста из стиля
        if CellStyle.TextHeight > 0 then
          PMText^.textprop.size := CellStyle.TextHeight
        else
          PMText^.textprop.size := CAcadTableDefaultTextHeight;

        PMText^.linespacef := 1;
        PMText^.WrapMode := mwmByWordThenChar;
        PMText^.Width := ColW * 0.9;

        // Выравнивание: комбинация горизонтальной и вертикальной составляющих.
        // jstl/jstc/jstr — Top, jsml/jsmc/jsmr — Middle, jsbl/jsbc/jsbr — Bottom.
        case CellStyle.VertAlign of
          vaTop:
            case CellStyle.HorzAlign of
              haLeft:   PMText^.textprop.justify := jstl;
              haCenter: PMText^.textprop.justify := jstc;
              haRight:  PMText^.textprop.justify := jstr;
            else
              PMText^.textprop.justify := jstl;
            end;
          vaMiddle:
            case CellStyle.HorzAlign of
              haLeft:   PMText^.textprop.justify := jsml;
              haCenter: PMText^.textprop.justify := jsmc;
              haRight:  PMText^.textprop.justify := jsmr;
            else
              PMText^.textprop.justify := jsml;
            end;
          vaBottom:
            case CellStyle.HorzAlign of
              haLeft:   PMText^.textprop.justify := jsbl;
              haCenter: PMText^.textprop.justify := jsbc;
              haRight:  PMText^.textprop.justify := jsbr;
            else
              PMText^.textprop.justify := jsbl;
            end;
        else
          PMText^.textprop.justify := jstl;
        end;

        // Позиция текста с учётом выравнивания и размеров ячейки
        TextHeightLocal := PMText^.textprop.size;

        case CellStyle.HorzAlign of
          haLeft:
            PMText^.Local.P_insert.x :=
              CurrentX + TextHeightLocal * 0.5;
          haCenter:
            PMText^.Local.P_insert.x := CurrentX + ColW / 2;
          haRight:
            PMText^.Local.P_insert.x :=
              CurrentX + ColW - TextHeightLocal * 0.5;
        else
          PMText^.Local.P_insert.x :=
            CurrentX + TextHeightLocal * 0.5;
        end;

        // Вертикальная позиция с учётом объединённой высоты ячейки
        case CellStyle.VertAlign of
          vaTop:
            PMText^.Local.P_insert.y :=
              -(CurrentY + TextHeightLocal * 0.5);
          vaMiddle:
            PMText^.Local.P_insert.y :=
              -(CurrentY + RowH / 2);
          vaBottom:
            PMText^.Local.P_insert.y :=
              -(CurrentY + RowH - TextHeightLocal * 0.5);
        else
          PMText^.Local.P_insert.y := -(CurrentY + RowH / 2);
        end;

        PMText^.Local.P_insert.z := 0;
        PMText^.TXTStyle := ResolveTextStyle(CellStyle.TextStyle, ADrawing);

        programlog.LogOutFormatStr(
          'uzeentacadtable: BuildVisualRepresentation ' +
          'MText[%d,%d] insert(%.2f,%.2f) text="%s" ' +
          'halign=%d valign=%d justify=%d height=%.2f style="%s"',
          [RowIdx, ColIdx,
           PMText^.Local.P_insert.x, PMText^.Local.P_insert.y,
           PMText^.Template, Ord(CellStyle.HorzAlign),
           Ord(CellStyle.VertAlign),
           Ord(PMText^.textprop.justify), PMText^.textprop.size,
           CellStyle.TextStyle],
          LM_Info);
        CopyVPto(PMText^);
        PMText^.FormatEntity(ADrawing, ADC);
        Inc(TextCount);
      end;

      CurrentX := CurrentX + GetColWidth(ColIdx);
    end;

    CurrentY := CurrentY + GetRowHeight(RowIdx);
  end;
  programlog.LogOutFormatStr(
    'uzeentacadtable: BuildVisualRepresentation created %d text objects',
    [TextCount], LM_Info);

  programlog.LogOutFormatStr(
    'uzeentacadtable: BuildVisualRepresentation OK ' +
    'rows=%d cols=%d W=%.2f H=%.2f TotalObj=%d',
    [FRowCount, FColCount, TotalWidth, TotalHeight, ConstObjArray.Count], LM_Info);
end;

// Вычисляет bounding box таблицы на основе её размеров.
// Вызывается до BuildGeometry, когда ConstObjArray ещё пуст.
procedure GDBObjAcadTable.getoutbound(var DC: TDrawContext);
var
  TotalWidth, TotalHeight: Double;
  MinX, MinY, MaxX, MaxY: Double;
begin
  if (FRowCount <= 0) or (FColCount <= 0) then
  begin
    // Пустая таблица — минимальный bounding box
    vp.BoundingBox.LBN := VertexAdd(Local.P_insert, CreateVertex(-0.01, -0.01, 0));
    vp.BoundingBox.RTF := VertexAdd(Local.P_insert, CreateVertex(0.01, 0.01, 0));
    Exit;
  end;

  TotalWidth := GetTotalWidth;
  TotalHeight := GetTotalHeight;

  // Таблица строится от (0,0) до (TotalWidth, -TotalHeight) в локальных координатах
  // Затем применяется трансформация (Local.P_insert)
  MinX := Local.P_insert.x;
  MaxX := Local.P_insert.x + TotalWidth;
  MinY := Local.P_insert.y - TotalHeight;
  MaxY := Local.P_insert.y;

  vp.BoundingBox.LBN := CreateVertex(MinX, MinY, Local.P_insert.z);
  vp.BoundingBox.RTF := CreateVertex(MaxX, MaxY, Local.P_insert.z);

  programlog.LogOutFormatStr(
    'uzeentacadtable: getoutbound BB=(%.2f,%.2f)-(%.2f,%.2f) W=%.2f H=%.2f',
    [MinX, MinY, MaxX, MaxY, TotalWidth, TotalHeight], LM_Info);
end;

// --- Методы сущности ---

// Строит геометрию таблицы. Вызывается после добавления в чертёж.
// ObjMatrix уже содержит мировую трансформацию из CalcObjMatrix,
// поэтому FormatEntity дочерних объектов вычислит экранные координаты
// в мировом пространстве — так же, как это делает GDBObjTable.Build.
procedure GDBObjAcadTable.BuildGeometry(var ADrawing: TDrawingDef);
var
  DC: TDrawContext;
begin
  programlog.LogOutFormatStr(
    'uzeentacadtable: BuildGeometry START built=%d rows=%d cols=%d Insert=(%.2f,%.2f)',
    [Ord(FGeometryBuilt), FRowCount, FColCount, FInsertPoint.x, FInsertPoint.y], LM_Info);

  if not FGeometryBuilt then
  begin
    // Применяем DXF-стиль таблицы перед построением геометрии.
    // К этому моменту чертёж уже полностью загружен и DXFTableStyleTable заполнена.
    ApplyDXFTableStyle(ADrawing);

    DC := ADrawing.CreateDrawingRC;
    programlog.LogOutFormatStr(
      'uzeentacadtable: BuildGeometry created DC, ConstObjArray count before=%d',
      [ConstObjArray.Count], LM_Info);

    // Строим визуальное представление (линии и текст)
    BuildVisualRepresentation(ADrawing, DC);

    programlog.LogOutFormatStr(
      'uzeentacadtable: BuildGeometry after BuildVisualRepresentation ConstObjArray count=%d',
      [ConstObjArray.Count], LM_Info);

    FGeometryBuilt := True;

    programlog.LogOutStr(
      'uzeentacadtable: BuildGeometry building ObjTree',
      LM_Info);
    // Строим дерево отрисовки — аналогично GDBObjComplex.BuildGeometry
    inherited BuildGeometry(ADrawing);
    programlog.LogOutFormatStr(
      'uzeentacadtable: BuildGeometry END ObjTree.NodeCount=%d',
      [ConstObjArray.ObjTree.nul.Count], LM_Info);
  end
  else
  begin
    programlog.LogOutStr(
      'uzeentacadtable: BuildGeometry SKIPPED - already built',
      LM_Info);
  end;

  programlog.LogOutStr(
    'uzeentacadtable: BuildGeometry END',
    LM_Info);
end;

// Форматирует сущность для отображения (расчёт матриц и отрисовка).
procedure GDBObjAcadTable.FormatEntity(var ADrawing: TDrawingDef;
  var ADC: TDrawContext; AStage: TEFStages = EFAllStages);
begin
  programlog.LogOutFormatStr(
    'uzeentacadtable: FormatEntity START CalcCS=%d Draw=%d rows=%d cols=%d Visible=%d',
    [Ord(EFCalcEntityCS in AStage), Ord(EFDraw in AStage), FRowCount, FColCount, Ord(Visible)], LM_Info);
  if EFCalcEntityCS in AStage then
  begin
    if Assigned(EntExtensions) then
      EntExtensions.RunOnBeforeEntityFormat(@Self, ADrawing, ADC);
    CalcObjMatrix;
    programlog.LogOutFormatStr(
      'uzeentacadtable: FormatEntity after CalcObjMatrix Local=(%.2f,%.2f,%.2f)',
      [Local.P_insert.x, Local.P_insert.y, Local.P_insert.z], LM_Info);
    // Вычисляем bounding box вручную до BuildGeometry (ConstObjArray ещё пуст)
    getoutbound(ADC);
    programlog.LogOutFormatStr(
      'uzeentacadtable: FormatEntity after getoutbound BB=(%.2f,%.2f)-(%.2f,%.2f)',
      [vp.BoundingBox.LBN.x, vp.BoundingBox.LBN.y, vp.BoundingBox.RTF.x, vp.BoundingBox.RTF.y], LM_Info);
  end;
  CalcActualVisible(ADC.DrawingContext.VActuality);
  programlog.LogOutFormatStr(
    'uzeentacadtable: FormatEntity after CalcActualVisible Visible=%d',
    [Ord(Visible)], LM_Info);
  if EFDraw in AStage then
  begin
    programlog.LogOutStr(
      'uzeentacadtable: FormatEntity calling BuildGeometry',
      LM_Info);
    // Строим геометрию только один раз (при первой отрисовке)
    BuildGeometry(ADrawing);
    programlog.LogOutStr(
      'uzeentacadtable: FormatEntity after BuildGeometry',
      LM_Info);
    if Assigned(EntExtensions) then
      EntExtensions.RunOnAfterEntityFormat(@Self, ADrawing, ADC);
  end;
  programlog.LogOutStr(
    'uzeentacadtable: FormatEntity END',
    LM_Info);
end;

// Использует отложенное форматирование (стадийный FormatEntity).
function GDBObjAcadTable.IsStagedFormatEntity: Boolean;
begin
  Result := True;
end;

// Откладывает построение геометрии: таблица будет построена после загрузки.
function GDBObjAcadTable.DXFDelayedBuildGeometry: Boolean;
begin
  Result := True;
end;

// Сохраняет таблицу в DXF-поток.
// TODO (этап 3): добавить полноценный экспорт в формат ACAD_TABLE.
procedure GDBObjAcadTable.SaveToDXF(var AOutStream: TZctnrVectorBytes;
  var ADrawing: TDrawingDef; var AIODXFContext: TIODXFSaveContext);
begin
  // Этап 1: сохранение не реализовано
  programlog.LogOutStr(
    'uzeentacadtable: SaveToDXF — экспорт не реализован (этап 3)',
    LM_Info);
end;

// Дополнительные данные после основного блока (не используются на этапе 1).
procedure GDBObjAcadTable.SaveToDXFFollow(var AOutStream: TZctnrVectorBytes;
  var ADrawing: TDrawingDef; var AIODXFContext: TIODXFSaveContext);
begin
  // Пустая реализация
end;

// Создаёт полную копию сущности, включая все данные таблицы.
function GDBObjAcadTable.Clone(AOwn: Pointer): PGDBObjEntity;
var
  NewTable: PGDBObjAcadTable;
  Idx, Idx2: Integer;
begin
  GetMem(Pointer(NewTable), SizeOf(GDBObjAcadTable));
  NewTable^.initnul(AOwn);
  CopyVPto(NewTable^);
  CopyExtensionsTo(NewTable^);

  NewTable^.FInsertPoint := FInsertPoint;
  NewTable^.FRowCount := FRowCount;
  NewTable^.FColCount := FColCount;
  NewTable^.FTableStyleHandle := FTableStyleHandle;

  // Копируем высоты строк
  for Idx := 0 to FRowHeights.Count - 1 do
    NewTable^.FRowHeights.PushBackData(FRowHeights.parray^[Idx]);

  // Копируем ширины столбцов
  for Idx := 0 to FColWidths.Count - 1 do
    NewTable^.FColWidths.PushBackData(FColWidths.parray^[Idx]);

  // Копируем тексты ячеек
  System.SetLength(NewTable^.FCellTexts, Length(FCellTexts));
  for Idx := 0 to High(FCellTexts) do
    NewTable^.FCellTexts[Idx] := FCellTexts[Idx];

  // Копируем новые структуры данных (этап 2)
  NewTable^.FTableStyle := FTableStyle;

  // Копируем строки
  System.SetLength(NewTable^.FRows, Length(FRows));
  for Idx := 0 to High(FRows) do
    NewTable^.FRows[Idx] := FRows[Idx];

  // Копируем столбцы
  System.SetLength(NewTable^.FCols, Length(FCols));
  for Idx := 0 to High(FCols) do
    NewTable^.FCols[Idx] := FCols[Idx];

  // Копируем ячейки
  System.SetLength(NewTable^.FCells, Length(FCells));
  for Idx := 0 to High(FCells) do
  begin
    System.SetLength(NewTable^.FCells[Idx], Length(FCells[Idx]));
    for Idx2 := 0 to High(FCells[Idx]) do
      NewTable^.FCells[Idx][Idx2] := FCells[Idx][Idx2];
  end;

  // Копируем объединения
  System.SetLength(NewTable^.FMerges, Length(FMerges));
  for Idx := 0 to High(FMerges) do
    NewTable^.FMerges[Idx] := FMerges[Idx];

  NewTable^.bp.ListPos.Owner := AOwn;
  Result := NewTable;
end;

// Возвращает числовой идентификатор типа объекта.
function GDBObjAcadTable.GetObjType: TObjID;
begin
  Result := GDBAcadTableID;
end;

// Возвращает строковое имя типа объекта.
function GDBObjAcadTable.GetObjTypeName: String;
begin
  Result := ObjN_GDBObjAcadTable;
end;

// --- Функции выделения памяти ---

// Выделяет память для объекта без инициализации.
function AllocAcadTable: Pointer;
begin
  GetMem(Result, SizeOf(GDBObjAcadTable));
end;

// Выделяет и инициализирует новый объект GDBObjAcadTable.
function AllocAndInitAcadTable(
  AOwner: PGDBObjGenericWithSubordinated): PGDBObjAcadTable;
begin
  GetMem(Pointer(Result), SizeOf(GDBObjAcadTable));
  Result^.initnul(AOwner);
  Result^.bp.ListPos.Owner := AOwner;
end;

initialization
  // Регистрация сущности ACAD_TABLE с привязкой к DXF-имени
  RegisterDXFEntity(
    GDBAcadTableID,
    'ACAD_TABLE',
    'AcadTable',
    @AllocAcadTable,
    @AllocAndInitAcadTable
  );

end.
