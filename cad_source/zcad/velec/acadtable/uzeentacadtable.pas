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
  uzegeometry, uzegeometrytypes, uzeffdxfsupport, uzMVReader,gzctnrVectorTypes,
  uzbLogIntf, uzclog, SysUtils, uzctnrvectordouble;

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
end;

// Освобождает динамически выделенные ресурсы.
destructor GDBObjAcadTable.done;
begin
  FRowHeights.done;
  FColWidths.done;
  System.SetLength(FCellTexts, 0);
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
begin
  programlog.LogOutStr('uzeentacadtable: LoadFromDXF START', LM_Info);

  CellIndex := 0;
  GroupCode := ARdr.ParseInteger;
  RowHeightsRead := False;
  ColWidthsRead := False;
  InCellData := False;
  CellTextRead := False;

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
          end
          else
            ARdr.SkipString; // Это флаг override ячейки
        end;

        // Высота строки (повторяется FRowCount раз)
        141:
        begin
          DoubleVal := ARdr.ParseDouble;
          if DoubleVal <= 0 then
            DoubleVal := CAcadTableDefaultRowHeight;
          FRowHeights.PushBackData(DoubleVal);
          RowHeightsRead := True;
        end;

        // Ширина столбца (повторяется FColCount раз)
        142:
        begin
          DoubleVal := ARdr.ParseDouble;
          if DoubleVal <= 0 then
            DoubleVal := CAcadTableDefaultColWidth;
          FColWidths.PushBackData(DoubleVal);
          ColWidthsRead := True;
        end;

        // Начало данных ячейки — код 171 (тип ячейки)
        171:
        begin
          InCellData := True;
          CellTextRead := False;
          ARdr.SkipString;
        end;

        // Текст ячейки — код 302 (DXF 2007+) — приоритет
        302:
        begin
          if InCellData and not CellTextRead then
          begin
            dxfLoadString(ARdr, CellText, AContext.Header);
            SetCellTextByIndex(CellIndex, CellText);
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
            Inc(CellIndex);
            CellTextRead := True;
          end
          else
            ARdr.SkipString;
        end;

        // Пропускаем служебные коды ячеек
        11, 21, 31, 90, 93, 94, 95, 96, 170, 172, 173, 174, 175, 176, 177, 178,
        145, 300, 301, 304, 274, 275, 276, 278, 279, 280, 281, 283, 284, 285, 286,
        288, 289, 63, 64, 65, 66, 68, 69, 70, 40, 41, 342, 343, 344, 340:
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

  programlog.LogOutFormatStr(
    'uzeentacadtable: LoadFromDXF END rows=%d cols=%d cells=%d',
    [FRowCount, FColCount, Length(FCellTexts)], LM_Info);
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
begin
  programlog.LogOutFormatStr(
    'uzeentacadtable: BuildVisualRepresentation START rows=%d cols=%d',
    [FRowCount, FColCount], LM_Info);
  ConstObjArray.Free;

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

  // --- Горизонтальные линии (верхняя, разделители, нижняя) ---
  CurrentY := 0;
  for RowIdx := 0 to FRowCount do
  begin
    pointer(PLine) := ConstObjArray.CreateInitObj(GDBLineID, @Self);
    PLine^.CoordInOCS.lBegin.x := 0;
    PLine^.CoordInOCS.lBegin.y := -CurrentY;
    PLine^.CoordInOCS.lBegin.z := 0;
    PLine^.CoordInOCS.lEnd.x := TotalWidth;
    PLine^.CoordInOCS.lEnd.y := -CurrentY;
    PLine^.CoordInOCS.lEnd.z := 0;
    CopyVPto(PLine^);
    PLine^.FormatEntity(ADrawing, ADC);

    if RowIdx < FRowCount then
      CurrentY := CurrentY + GetRowHeight(RowIdx);
  end;

  // --- Вертикальные линии (левая, разделители, правая) ---
  CurrentX := 0;
  for ColIdx := 0 to FColCount do
  begin
    pointer(PLine) := ConstObjArray.CreateInitObj(GDBLineID, @Self);
    PLine^.CoordInOCS.lBegin.x := CurrentX;
    PLine^.CoordInOCS.lBegin.y := 0;
    PLine^.CoordInOCS.lBegin.z := 0;
    PLine^.CoordInOCS.lEnd.x := CurrentX;
    PLine^.CoordInOCS.lEnd.y := -TotalHeight;
    PLine^.CoordInOCS.lEnd.z := 0;
    CopyVPto(PLine^);
    PLine^.FormatEntity(ADrawing, ADC);

    if ColIdx < FColCount then
      CurrentX := CurrentX + GetColWidth(ColIdx);
  end;

  // --- Текст ячеек ---
  CurrentY := 0;
  for RowIdx := 0 to FRowCount - 1 do
  begin
    RowH := GetRowHeight(RowIdx);
    CurrentX := 0;

    for ColIdx := 0 to FColCount - 1 do
    begin
      ColW := GetColWidth(ColIdx);
      CellStr := GetCellText(RowIdx, ColIdx);

      if CellStr <> '' then
      begin
        pointer(PMText) := ConstObjArray.CreateInitObj(GDBMTextID, @Self);
        PMText^.Template := UTF8ToString(CellStr);
        PMText^.textprop.size := CAcadTableDefaultTextHeight;
        PMText^.linespacef := 1;
        PMText^.Width := ColW * 0.9;
        PMText^.textprop.justify := jstl;
        PMText^.Local.P_insert.x :=
          CurrentX + CAcadTableDefaultTextHeight * 0.5;
        PMText^.Local.P_insert.y :=
          -(CurrentY + RowH * 0.5);
        PMText^.Local.P_insert.z := 0;
        PMText^.TXTStyle :=
          pointer(ADrawing.GetTextStyleTable^.getDataMutable(0));
        CopyVPto(PMText^);
        PMText^.FormatEntity(ADrawing, ADC);
      end;

      CurrentX := CurrentX + ColW;
    end;

    CurrentY := CurrentY + RowH;
  end;

  programlog.LogOutFormatStr(
    'uzeentacadtable: BuildVisualRepresentation OK ' +
    'rows=%d cols=%d W=%.2f H=%.2f',
    [FRowCount, FColCount, TotalWidth, TotalHeight], LM_Info);
end;

// --- Методы сущности ---

// Строит геометрию таблицы. Вызывается после добавления в чертёж.
procedure GDBObjAcadTable.BuildGeometry(var ADrawing: TDrawingDef);
var
  m4: TzeTypedMatrix4d;
  ir: itrec;
  pv: PGDBObjEntity;
  DC: TDrawContext;
begin
  programlog.LogOutFormatStr(
    'uzeentacadtable: BuildGeometry START built=%d rows=%d cols=%d Insert=(%.2f,%.2f)',
    [Ord(FGeometryBuilt), FRowCount, FColCount, FInsertPoint.x, FInsertPoint.y], LM_Info);

  if not FGeometryBuilt then
  begin
    DC := ADrawing.CreateDrawingRC;

    // Сохраняем матрицу таблицы
    m4 := getmatrix^;
    // Временно устанавливаем единичную матрицу для построения геометрии
    objmatrix := onematrix;

    // Строим визуальное представление (линии и текст) в локальных координатах
    BuildVisualRepresentation(ADrawing, DC);

    // Применяем трансформацию таблицы ко всем созданным объектам
    pv := ConstObjArray.beginiterate(ir);
    if pv <> nil then
    begin
      repeat
        // Применяем трансформацию матрицы таблицы к объекту
        pv^.transform(m4);
        pv := ConstObjArray.iterate(ir);
      until pv = nil;
    end;

    FGeometryBuilt := True;
    // Восстанавливаем матрицу таблицы
    objmatrix := m4;
  end;

  // Строим дерево отрисовки из ConstObjArray
  inherited BuildGeometry(ADrawing);
end;

// Форматирует сущность для отображения (расчёт матриц и отрисовка).
procedure GDBObjAcadTable.FormatEntity(var ADrawing: TDrawingDef;
  var ADC: TDrawContext; AStage: TEFStages = EFAllStages);
begin
  programlog.LogOutFormatStr(
    'uzeentacadtable: FormatEntity CalcCS=%d Draw=%d',
    [Ord(EFCalcEntityCS in AStage), Ord(EFDraw in AStage)], LM_Info);
  if EFCalcEntityCS in AStage then
  begin
    if Assigned(EntExtensions) then
      EntExtensions.RunOnBeforeEntityFormat(@Self, ADrawing, ADC);
    CalcObjMatrix;
    CalcBB(ADC);
  end;
  CalcActualVisible(ADC.DrawingContext.VActuality);
  if EFDraw in AStage then
  begin
    // Строим геометрию только один раз (при первой отрисовке)
    BuildGeometry(ADrawing);
    if Assigned(EntExtensions) then
      EntExtensions.RunOnAfterEntityFormat(@Self, ADrawing, ADC);
  end;
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
  Idx: Integer;
begin
  GetMem(Pointer(NewTable), SizeOf(GDBObjAcadTable));
  NewTable^.initnul(AOwn);
  CopyVPto(NewTable^);
  CopyExtensionsTo(NewTable^);

  NewTable^.FInsertPoint := FInsertPoint;
  NewTable^.FRowCount := FRowCount;
  NewTable^.FColCount := FColCount;

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
