{
*****************************************************************************
*                                                                           *
*  This file is part of the ZCAD                                            *
*                                                                           *
*  See the file COPYING.txt, included in this distribution,                 *
*  for details about the copyright.                                         *
*                                                                           *
*****************************************************************************
}
unit uzctacadtable;
{$Codepage UTF8}
{$Mode delphi}{$H+}

interface

uses
  SysUtils,
  Math,
  Classes,
  fpcunit,
  testregistry,
  Interfaces,
  uzeffdxf,
  uzedrawingsimple,
  uzeffmanager,
  uzgldrawcontext,
  uzeconsts,
  uzeTypes,
  gzctnrVectorTypes,
  uzegeometry,
  uzegeometrytypes,
  uzeentity,
  uzeentmtext,
  uzeentline,
  uzeentgenericsubentry,
  uzeentcomplex,
  UGDBVisibleTreeArray,
  uzeenttext,
  uzeentblockinsert,
  uzeenttable,
  uzeacadtable_types,
  uzeacadtable_model;

type
  TAcadTableStyleTest = class(TTestCase)
  published
    procedure LoadsCellTextStylesFromDXFTableStyle;
    procedure LoadsBreakSettingsFromDXF;
    procedure LoadsBreakSettingsFromSecondSample;
    procedure RendersBrokenTableAsSeparatedFragments;
    procedure LoadsSplitTableAsSingleMergedObject;
    procedure AppliesTableStyleToContinuationParts;
    // issue #1305, часть 1: трансформация (перенос) должна перестраивать
    // визуальное представление таблицы.
    procedure TransformationMovesRenderedTable;
    // issue #1305, часть 2b: снятие признака разрыва объединяет
    // части-продолжения в единую непрерывную таблицу.
    procedure ClearingBreakMergesContinuationParts;
    // issue #1313: снятие разбиения должно удалить повторённые верхние
    // метки из частей-продолжений.
    procedure ClearingBreakRemovesRepeatedTopLabels;
    // issue #1307, часть 1: интервал между частями (Break spacing)
    // читается из round-trip данных DXF (≈0.99).
    procedure LoadsBreakSpacingFromDXF;
    // issue #1307, часть 2: высота разбиения (Break height) читается
    // из round-trip данных DXF (≈2.0486).
    procedure LoadsBreakHeightFromDXF;
    // issue #1307, часть 2: изменение высоты разбиения пересегментирует
    // таблицу с автоопределением числа частей.
    procedure ChangingBreakHeightResegmentsTable;
    // issue #1307, часть 1: изменение интервала смещает части-продолжения
    // (меняется ширина отрисованного представления).
    procedure ChangingBreakSpacingRepositionsParts;
    // issue #1315: изменение направления разбиения должно перемещать
    // части-продолжения влево или вниз от главной таблицы.
    procedure ChangingBreakDirectionRepositionsParts;
    // issue #1309, часть 1: при загрузке разорванной таблицы признак повтора
    // верхних меток определяется по содержимому частей-продолжений.
    procedure DetectsBreakRepeatTopOnLoad;
    // issue #1309, часть 2: снятие признака повтора удаляет повторяющиеся
    // строки-метки из всех частей, а возврат — добавляет их обратно.
    procedure TogglingBreakRepeatTopAddsAndRemovesLabelRows;
    // issue #1311: строка данных, смещённая на место удалённых верхних меток,
    // должна сохранять своё форматирование.
    procedure ClearingBreakRepeatTopKeepsDataRowFormatting;
    // issue #1309: неразорванная таблица не имеет повтора верхних меток.
    procedure NonBrokenTableHasNoBreakRepeatTop;
  end;

implementation

function FindFirstAcadTable(const ARoot: PGDBObjGenericSubEntry): PGDBObjAcadTable;
var
  IR: itrec;
  PEntity: PGDBObjEntity;
begin
  Result := nil;
  PEntity := ARoot^.ObjArray.beginiterate(IR);
  while PEntity <> nil do
  begin
    if PEntity^.GetObjType = GDBAcadTableID then
      Exit(PGDBObjAcadTable(PEntity));
    PEntity := ARoot^.ObjArray.iterate(IR);
  end;
end;

function CountAcadTables(const ARoot: PGDBObjGenericSubEntry): Integer;
var
  IR: itrec;
  PEntity: PGDBObjEntity;
begin
  Result := 0;
  PEntity := ARoot^.ObjArray.beginiterate(IR);
  while PEntity <> nil do
  begin
    if PEntity^.GetObjType = GDBAcadTableID then
      Inc(Result);
    PEntity := ARoot^.ObjArray.iterate(IR);
  end;
end;

function LoadDrawingFromDXF(const AFileName: string; var ADrawing: TSimpleDrawing): Integer;
var
  DC: TDrawContext;
  ZDC: TZDrawingContext;
begin
  ADrawing.init(nil);
  DC := ADrawing.CreateDrawingRC;
  ZDC.CreateRec(ADrawing, ADrawing.pObjRoot^, TLOLoad, DC);
  AddFromDXF(AFileName, ZDC);
  Result := ADrawing.pObjRoot^.ObjArray.Count;
end;

// Геометрия AcadTable (линии и MText ячеек) генерируется в собственный
// ConstObjArray объекта, а не в корень чертежа. Построение отложенное
// (DXFDelayedBuildGeometry=True), поэтому тесты обязаны сначала вызвать
// BuildGeometry на таблице, а затем обходить именно её ConstObjArray.
procedure BuildTableGeometry(var ADrawing: TSimpleDrawing;
  ATable: PGDBObjAcadTable);
begin
  ATable^.BuildGeometry(ADrawing);
end;

procedure CollectMTextStyles(var AArr: GDBObjEntityTreeArray;
  AStyles: TStrings);
var
  IR: itrec;
  PEntity: PGDBObjEntity;
  PMText: PGDBObjMText;
begin
  PEntity := AArr.beginiterate(IR);
  while PEntity <> nil do
  begin
    if PEntity^.GetObjType = GDBMTextID then
    begin
      PMText := PGDBObjMText(PEntity);
      if (PMText^.TXTStyle <> nil) and (PMText^.TXTStyle^.Name <> '') then
        if AStyles.IndexOf(PMText^.TXTStyle^.Name) < 0 then
          AStyles.Add(PMText^.TXTStyle^.Name);
    end;
    PEntity := AArr.iterate(IR);
  end;
end;

procedure CollectLineBounds(var AArr: GDBObjEntityTreeArray;
  out AMinX, AMaxX: Double; out AHasGap: Boolean);
var
  IR: itrec;
  PEntity: PGDBObjEntity;
  PLine: PGDBObjLine;
  MinSegX, MaxSegX: Double;
  Segments: array of record
    MinX: Double;
    MaxX: Double;
  end;
  SegCount, i, j: Integer;
  TmpMin, TmpMax: Double;
begin
  AMinX := 0;
  AMaxX := 0;
  AHasGap := False;
  SegCount := 0;

  PEntity := AArr.beginiterate(IR);
  while PEntity <> nil do
  begin
    if PEntity^.GetObjType = GDBLineID then
    begin
      PLine := PGDBObjLine(PEntity);
      MinSegX := Min(PLine^.CoordInWCS.lBegin.x, PLine^.CoordInWCS.lEnd.x);
      MaxSegX := Max(PLine^.CoordInWCS.lBegin.x, PLine^.CoordInWCS.lEnd.x);
      SetLength(Segments, SegCount + 1);
      Segments[SegCount].MinX := MinSegX;
      Segments[SegCount].MaxX := MaxSegX;
      Inc(SegCount);
    end;
    PEntity := AArr.iterate(IR);
  end;

  if SegCount = 0 then
    Exit;

  for i := 0 to SegCount - 2 do
    for j := i + 1 to SegCount - 1 do
      if Segments[i].MinX > Segments[j].MinX then
      begin
        TmpMin := Segments[i].MinX;
        TmpMax := Segments[i].MaxX;
        Segments[i].MinX := Segments[j].MinX;
        Segments[i].MaxX := Segments[j].MaxX;
        Segments[j].MinX := TmpMin;
        Segments[j].MaxX := TmpMax;
      end;

  AMinX := Segments[0].MinX;
  AMaxX := Segments[0].MaxX;
  for i := 1 to SegCount - 1 do
  begin
    if Segments[i].MinX > AMaxX + 1e-6 then
      AHasGap := True;
    if Segments[i].MaxX > AMaxX then
      AMaxX := Segments[i].MaxX;
  end;
end;

procedure CollectLineBounds2D(var AArr: GDBObjEntityTreeArray;
  out AMinX, AMaxX, AMinY, AMaxY: Double; out AHasLines: Boolean);
var
  IR: itrec;
  PEntity: PGDBObjEntity;
  PLine: PGDBObjLine;
  LineMinX, LineMaxX, LineMinY, LineMaxY: Double;
begin
  AMinX := 0;
  AMaxX := 0;
  AMinY := 0;
  AMaxY := 0;
  AHasLines := False;

  PEntity := AArr.beginiterate(IR);
  while PEntity <> nil do
  begin
    if PEntity^.GetObjType = GDBLineID then
    begin
      PLine := PGDBObjLine(PEntity);
      LineMinX := Min(PLine^.CoordInWCS.lBegin.x, PLine^.CoordInWCS.lEnd.x);
      LineMaxX := Max(PLine^.CoordInWCS.lBegin.x, PLine^.CoordInWCS.lEnd.x);
      LineMinY := Min(PLine^.CoordInWCS.lBegin.y, PLine^.CoordInWCS.lEnd.y);
      LineMaxY := Max(PLine^.CoordInWCS.lBegin.y, PLine^.CoordInWCS.lEnd.y);
      if not AHasLines then
      begin
        AMinX := LineMinX;
        AMaxX := LineMaxX;
        AMinY := LineMinY;
        AMaxY := LineMaxY;
        AHasLines := True;
      end
      else
      begin
        if LineMinX < AMinX then AMinX := LineMinX;
        if LineMaxX > AMaxX then AMaxX := LineMaxX;
        if LineMinY < AMinY then AMinY := LineMinY;
        if LineMaxY > AMaxY then AMaxY := LineMaxY;
      end;
    end;
    PEntity := AArr.iterate(IR);
  end;
end;

// Собирает высоты текста (textprop.size) всех MText, отрисованных таблицей.
procedure CollectMTextSizes(var AArr: GDBObjEntityTreeArray;
  out AMinSize, AMaxSize: Double; out ACount: Integer);
var
  IR: itrec;
  PEntity: PGDBObjEntity;
  PMText: PGDBObjMText;
  Sz: Double;
begin
  AMinSize := 0;
  AMaxSize := 0;
  ACount := 0;
  PEntity := AArr.beginiterate(IR);
  while PEntity <> nil do
  begin
    if PEntity^.GetObjType = GDBMTextID then
    begin
      PMText := PGDBObjMText(PEntity);
      Sz := PMText^.textprop.size;
      if ACount = 0 then
      begin
        AMinSize := Sz;
        AMaxSize := Sz;
      end
      else
      begin
        if Sz < AMinSize then AMinSize := Sz;
        if Sz > AMaxSize then AMaxSize := Sz;
      end;
      Inc(ACount);
    end;
    PEntity := AArr.iterate(IR);
  end;
end;

function FindMTextSizeByTemplate(var AArr: GDBObjEntityTreeArray;
  const ATemplate: String; out ASize: Double): Boolean;
var
  IR: itrec;
  PEntity: PGDBObjEntity;
  PMText: PGDBObjMText;
begin
  Result := False;
  ASize := 0;
  PEntity := AArr.beginiterate(IR);
  while PEntity <> nil do
  begin
    if PEntity^.GetObjType = GDBMTextID then
    begin
      PMText := PGDBObjMText(PEntity);
      if PMText^.Template = ATemplate then
      begin
        ASize := PMText^.textprop.size;
        Result := True;
        Exit;
      end;
    end;
    PEntity := AArr.iterate(IR);
  end;
end;

function CountMTextByTemplate(var AArr: GDBObjEntityTreeArray;
  const ATemplate: String): Integer;
var
  IR: itrec;
  PEntity: PGDBObjEntity;
  PMText: PGDBObjMText;
begin
  Result := 0;
  PEntity := AArr.beginiterate(IR);
  while PEntity <> nil do
  begin
    if PEntity^.GetObjType = GDBMTextID then
    begin
      PMText := PGDBObjMText(PEntity);
      if PMText^.Template = ATemplate then
        Inc(Result);
    end;
    PEntity := AArr.iterate(IR);
  end;
end;

procedure TAcadTableStyleTest.LoadsCellTextStylesFromDXFTableStyle;
var
  Drawing: TSimpleDrawing;
  AcadTable: PGDBObjAcadTable;
  StyleNames: TStringList;
  EntityCount: Integer;
begin
  EntityCount := LoadDrawingFromDXF(
    ExpandFileName('../../../cad_source/test/tablerazdel.dxf'), Drawing);
  try
    Check(EntityCount > 0, 'DXF должен загрузить сущности');

    AcadTable := FindFirstAcadTable(Drawing.pObjRoot);
    AssertNotNull('Ожидалась сущность AcadTable', AcadTable);
    BuildTableGeometry(Drawing, AcadTable);

    StyleNames := TStringList.Create;
    try
      StyleNames.Sorted := False;
      StyleNames.Duplicates := dupIgnore;
      CollectMTextStyles(AcadTable^.ConstObjArray, StyleNames);

      Check(StyleNames.Count > 0, 'Ожидались текстовые сущности таблицы');
      Check(StyleNames.IndexOf('newtext') >= 0,
        'Стиль newtext должен применяться к части ячеек таблицы');
      Check(StyleNames.Count > 1,
        'Таблица должна использовать более одного текстового стиля');
    finally
      StyleNames.Free;
    end;
  finally
    Drawing.done;
  end;
end;

procedure TAcadTableStyleTest.LoadsBreakSettingsFromDXF;
var
  Drawing: TSimpleDrawing;
  AcadTable: PGDBObjAcadTable;
begin
  LoadDrawingFromDXF(
    ExpandFileName('../../../cad_source/test/tablerazdel.dxf'), Drawing);
  try
    AcadTable := FindFirstAcadTable(Drawing.pObjRoot);
    AssertNotNull('Ожидалась сущность AcadTable', AcadTable);

    // tablerazdel.dxf — это таблица, физически разорванная на три части
    // (две части-продолжения). Признак разрыва вычисляемый: даже если
    // DXF-флаг разбиения у отдельных ACAD_TABLE снят, наличие частей-
    // продолжений означает, что таблица разорвана, поэтому
    // BreakEnabled = True (issue #1305, часть 2a). Раньше инспектор
    // ошибочно показывал здесь Break enabled = false.
    CheckTrue(AcadTable^.BreakEnabled,
      'Разорванная на части таблица должна показывать Break enabled = True');
    CheckTrue(AcadTable^.ContinuationPartCount > 0,
      'Разорванная таблица должна хранить части-продолжения');
    // Подробные параметры разрыва (повтор меток, ручное положение,
    // интервал) у round-trip разорванной таблицы хранятся не в самой
    // сущности ACAD_TABLE, а в объектах TABLEBREAKDATA секции OBJECTS,
    // и их чтение выходит за рамки issue #1305 (части 2a/2b — это
    // корректное определение факта разрыва и его снятие).
  finally
    Drawing.done;
  end;
end;

procedure TAcadTableStyleTest.LoadsBreakSettingsFromSecondSample;
var
  Drawing: TSimpleDrawing;
  AcadTable: PGDBObjAcadTable;
begin
  LoadDrawingFromDXF(
    ExpandFileName('../../../cad_source/test/tablerazdel2.dxf'), Drawing);
  try
    AcadTable := FindFirstAcadTable(Drawing.pObjRoot);
    AssertNotNull('Ожидалась сущность AcadTable', AcadTable);

    // tablerazdel2.dxf — одиночный неразорванный ACAD_TABLE: одна
    // сущность без частей-продолжений и без DXF-флага разрыва.
    // Вычисляемый признак разрыва не должен срабатывать ложно —
    // BreakEnabled = False (issue #1305, часть 2a: корректное
    // определение признаков разделения работает в обе стороны —
    // неразорванная таблица не должна показывать Break enabled = True).
    CheckEquals(0, AcadTable^.ContinuationPartCount,
      'Неразорванная таблица не должна иметь частей-продолжений');
    CheckFalse(AcadTable^.BreakEnabled,
      'Неразорванная таблица должна показывать Break enabled = False');
  finally
    Drawing.done;
  end;
end;

procedure TAcadTableStyleTest.RendersBrokenTableAsSeparatedFragments;
var
  Drawing: TSimpleDrawing;
  AcadTable: PGDBObjAcadTable;
  MinX, MaxX: Double;
  HasGap: Boolean;
begin
  LoadDrawingFromDXF(
    ExpandFileName('../../../cad_source/test/tablerazdel.dxf'), Drawing);
  try
    AcadTable := FindFirstAcadTable(Drawing.pObjRoot);
    AssertNotNull('Ожидалась сущность AcadTable', AcadTable);
    BuildTableGeometry(Drawing, AcadTable);

    CollectLineBounds(AcadTable^.ConstObjArray, MinX, MaxX, HasGap);
    Check(HasGap,
      'После применения правил разбиения у AcadTable должны быть разрывы между фрагментами');
    Check(MaxX - MinX > 20.0,
      'Ширина визуализации должна включать несколько разнесённых фрагментов таблицы');
  finally
    Drawing.done;
  end;
end;

// Разделённая по ширине таблица (tablerazdel.dxf) сохранена в DXF как три
// отдельные ACAD_TABLE. Все три части должны быть объединены в один объект
// AcadTable и отображаться вместе (issue #1300).
procedure TAcadTableStyleTest.LoadsSplitTableAsSingleMergedObject;
var
  Drawing: TSimpleDrawing;
  AcadTable: PGDBObjAcadTable;
  TableCount: Integer;
begin
  LoadDrawingFromDXF(
    ExpandFileName('../../../cad_source/test/tablerazdel.dxf'), Drawing);
  try
    TableCount := CountAcadTables(Drawing.pObjRoot);
    CheckEquals(1, TableCount,
      'Три части разделённой таблицы должны загружаться как один объект AcadTable');

    AcadTable := FindFirstAcadTable(Drawing.pObjRoot);
    AssertNotNull('Ожидалась сущность AcadTable', AcadTable);
    CheckEquals(2, AcadTable^.ContinuationPartCount,
      'В главную таблицу должны быть поглощены две части-продолжения');
  finally
    Drawing.done;
  end;
end;

// Все три части разделённой таблицы (tablerazdel.dxf) используют один и тот же
// DXF-стиль таблицы (handle 342 = "87", высоты текста 0.18/0.25). Части-
// продолжения поглощаются до вызова BuildGeometry, поэтому раньше их стиль
// оставался по умолчанию и текст рендерился высотой CAcadTableDefaultTextHeight
// (2.5) — намного крупнее ячеек, из-за чего «разъезжался». После исправления
// табличный стиль применяется ко всем частям, и высота текста не превышает
// высоту из DXF (issue #1300).
procedure TAcadTableStyleTest.AppliesTableStyleToContinuationParts;
var
  Drawing: TSimpleDrawing;
  AcadTable: PGDBObjAcadTable;
  MinSize, MaxSize: Double;
  TextCount: Integer;
begin
  LoadDrawingFromDXF(
    ExpandFileName('../../../cad_source/test/tablerazdel.dxf'), Drawing);
  try
    AcadTable := FindFirstAcadTable(Drawing.pObjRoot);
    AssertNotNull('Ожидалась сущность AcadTable', AcadTable);
    Check(AcadTable^.ContinuationPartCount > 0,
      'Таблица должна содержать части-продолжения');

    BuildTableGeometry(Drawing, AcadTable);
    CollectMTextSizes(AcadTable^.ConstObjArray, MinSize, MaxSize, TextCount);
    Check(TextCount > 0, 'Таблица должна отрисовать текст ячеек');
    // Высоты текста стиля 0.18/0.25 — задаём порог заметно ниже значения
    // по умолчанию 2.5, чтобы поймать неприменённый стиль у продолжений.
    Check(MaxSize < CAcadTableDefaultTextHeight - 1e-6,
      'Высота текста частей-продолжений должна браться из DXF-стиля, ' +
      'а не из значения по умолчанию');
  finally
    Drawing.done;
  end;
end;

// issue #1305, часть 1. До исправления визуальное представление таблицы
// строилось один раз и было защищено флагом FGeometryBuilt, поэтому при
// переносе (ручкой переноса) сама таблица не перестраивалась — двигалась
// только ручка. Теперь дочерние объекты переформатируются каждый кадр и
// пересчитывают свои WCS-координаты из objmatrix владельца, поэтому
// перенос таблицы сдвигает все её линии на ту же величину.
procedure TAcadTableStyleTest.TransformationMovesRenderedTable;
const
  Shift = 100.0;
var
  Drawing: TSimpleDrawing;
  AcadTable: PGDBObjAcadTable;
  DC: TDrawContext;
  MinX1, MaxX1, MinX2, MaxX2: Double;
  HasGap: Boolean;
  Matrix: TzeTypedMatrix4d;
begin
  LoadDrawingFromDXF(
    ExpandFileName('../../../cad_source/test/tablerazdel.dxf'), Drawing);
  try
    AcadTable := FindFirstAcadTable(Drawing.pObjRoot);
    AssertNotNull('Ожидалась сущность AcadTable', AcadTable);

    DC := Drawing.CreateDrawingRC;
    AcadTable^.FormatEntity(Drawing, DC);
    CollectLineBounds(AcadTable^.ConstObjArray, MinX1, MaxX1, HasGap);
    Check(MaxX1 > MinX1, 'Таблица должна отрисовать линии в WCS');

    // Переносим таблицу на Shift по оси X, как это делает ручка переноса.
    Matrix := CreateTranslationMatrix(Shift, 0, 0);
    AcadTable^.TransformAt(PGDBObjEntity(AcadTable), @Matrix);
    AcadTable^.FormatEntity(Drawing, DC);
    CollectLineBounds(AcadTable^.ConstObjArray, MinX2, MaxX2, HasGap);

    CheckEquals(MinX1 + Shift, MinX2, 1e-6,
      'После переноса таблицы её линии должны сдвинуться на ту же величину ' +
      '(issue #1305, часть 1)');
    CheckEquals(MaxX1 + Shift, MaxX2, 1e-6,
      'Правая граница перенесённой таблицы должна сдвинуться на ту же величину');
  finally
    Drawing.done;
  end;
end;

// issue #1305, часть 2b. Разорванная таблица (tablerazdel.dxf) загружается
// как один объект с двумя частями-продолжениями. Снятие признака разрыва
// (BreakEnabled := False) должно объединить все части в одну непрерывную
// таблицу: части-продолжения исчезают, а строки выстраиваются сверху вниз.
procedure TAcadTableStyleTest.ClearingBreakMergesContinuationParts;
var
  Drawing: TSimpleDrawing;
  AcadTable: PGDBObjAcadTable;
  RowsBefore: Integer;
begin
  LoadDrawingFromDXF(
    ExpandFileName('../../../cad_source/test/tablerazdel.dxf'), Drawing);
  try
    AcadTable := FindFirstAcadTable(Drawing.pObjRoot);
    AssertNotNull('Ожидалась сущность AcadTable', AcadTable);

    CheckTrue(AcadTable^.ContinuationPartCount > 0,
      'Разорванная таблица должна содержать части-продолжения');
    CheckTrue(AcadTable^.BreakEnabled,
      'До снятия разрыва BreakEnabled должен быть True');
    RowsBefore := AcadTable^.RowCount;

    // Снимаем признак разрыва — таблица должна перестроиться сверху вниз.
    AcadTable^.BreakEnabled := False;

    CheckEquals(0, AcadTable^.ContinuationPartCount,
      'После снятия разрыва части-продолжения должны быть объединены');
    CheckFalse(AcadTable^.BreakEnabled,
      'После объединения BreakEnabled должен быть False');
    CheckTrue(AcadTable^.RowCount > RowsBefore,
      'Объединённая таблица должна содержать строки всех частей ' +
      '(issue #1305, часть 2b)');
  finally
    Drawing.done;
  end;
end;

// issue #1313. Если разорванная таблица содержит повтор верхних меток
// (Title/Header) в частях-продолжениях, то при снятии BreakEnabled эти
// повторы должны исчезнуть: итоговая таблица становится непрерывной и
// содержит только один набор верхних меток. При повторном включении
// BreakEnabled сохранённые параметры разбиения должны примениться снова.
procedure TAcadTableStyleTest.ClearingBreakRemovesRepeatedTopLabels;
var
  Drawing: TSimpleDrawing;
  AcadTable: PGDBObjAcadTable;
  ExpectedRows, PartIdx, RepeatRows, RowsAfterDisable: Integer;
begin
  LoadDrawingFromDXF(
    ExpandFileName('../../../cad_source/test/tablerazdel.dxf'), Drawing);
  try
    AcadTable := FindFirstAcadTable(Drawing.pObjRoot);
    AssertNotNull('Ожидалась сущность AcadTable', AcadTable);

    CheckTrue(AcadTable^.BreakEnabled,
      'До снятия разбиения таблица должна быть разорванной');
    CheckTrue(AcadTable^.BreakRepeatTopLabels,
      'Тестовый DXF должен содержать повтор верхних меток');

    ExpectedRows := AcadTable^.RowCount;
    for PartIdx := 0 to AcadTable^.ContinuationPartCount - 1 do
    begin
      RepeatRows := 0;
      if AcadTable^.ContinuationPartCellText(PartIdx, 0, 0) = 'Zagolovok' then
        Inc(RepeatRows);
      if AcadTable^.ContinuationPartCellText(PartIdx, 1, 0) = 'A' then
        Inc(RepeatRows);
      Inc(ExpectedRows,
        AcadTable^.ContinuationPartRowCount(PartIdx) - RepeatRows);
    end;

    AcadTable^.BreakEnabled := False;
    CheckEquals(ExpectedRows, AcadTable^.RowCount,
      'Снятие разбиения должно объединить только логические строки, ' +
      'без повторённых верхних меток из частей-продолжений');

    BuildTableGeometry(Drawing, AcadTable);
    CheckEquals(1,
      CountMTextByTemplate(AcadTable^.ConstObjArray, 'Zagolovok'),
      'После снятия разбиения заголовок должен отрисоваться один раз');
    CheckEquals(1,
      CountMTextByTemplate(AcadTable^.ConstObjArray, 'A'),
      'После снятия разбиения шапка должна отрисоваться один раз');

    RowsAfterDisable := AcadTable^.RowCount;
    CheckTrue(AcadTable^.BreakHeight > 0,
      'Тестовый DXF должен содержать положительную высоту разбиения');
    AcadTable^.BreakHeight := AcadTable^.BreakHeight / 2;
    CheckEquals(0, AcadTable^.ContinuationPartCount,
      'При выключенном разбиении изменение высоты не должно разбивать таблицу');
    CheckEquals(RowsAfterDisable, AcadTable^.RowCount,
      'При выключенном разбиении высота разбиения должна игнорироваться');

    AcadTable^.BreakEnabled := True;
    CheckTrue(AcadTable^.BreakEnabled,
      'Повторное включение должно снова установить разбиение таблицы');
    CheckTrue(AcadTable^.ContinuationPartCount > 0,
      'Повторное включение должно применить сохранённую высоту разбиения');
    CheckEquals('Zagolovok', AcadTable^.ContinuationPartCellText(0, 0, 0),
      'Повторное включение должно вернуть повтор заголовка в продолжение');
    CheckEquals('A', AcadTable^.ContinuationPartCellText(0, 1, 0),
      'Повторное включение должно вернуть повтор шапки в продолжение');
  finally
    Drawing.done;
  end;
end;

// issue #1307, часть 1. Интервал между частями разорванной таблицы
// (Break spacing) хранится в round-trip данных DXF (XRECORD
// ACAD_ROUNDTRIP_2008_TABLE_ENTITY, первое значение группы 40). Для
// tablerazdel.dxf он равен ≈0.99 и согласуется с геометрией: шаг между
// точками вставки частей (13.49) минус ширина таблицы (12.5) = 0.99.
// Раньше это свойство вычислялось неверно (issue #1307).
procedure TAcadTableStyleTest.LoadsBreakSpacingFromDXF;
var
  Drawing: TSimpleDrawing;
  AcadTable: PGDBObjAcadTable;
begin
  LoadDrawingFromDXF(
    ExpandFileName('../../../cad_source/test/tablerazdel.dxf'), Drawing);
  try
    AcadTable := FindFirstAcadTable(Drawing.pObjRoot);
    AssertNotNull('Ожидалась сущность AcadTable', AcadTable);

    CheckEquals(0.99, AcadTable^.BreakSpacing, 1e-4,
      'Интервал между частями (Break spacing) должен читаться из DXF ≈0.99');
  finally
    Drawing.done;
  end;
end;

// issue #1307, часть 2. Высота разбиения (Break height) — порог суммарной
// высоты строк, после которого строки переносятся в следующую часть. Она
// хранится только в round-trip данных DXF (второе значение группы 40
// XRECORD) и не выводится из геометрии. Для tablerazdel.dxf — ≈2.0486.
// Раньше это свойство полностью отсутствовало (issue #1307).
procedure TAcadTableStyleTest.LoadsBreakHeightFromDXF;
var
  Drawing: TSimpleDrawing;
  AcadTable: PGDBObjAcadTable;
begin
  LoadDrawingFromDXF(
    ExpandFileName('../../../cad_source/test/tablerazdel.dxf'), Drawing);
  try
    AcadTable := FindFirstAcadTable(Drawing.pObjRoot);
    AssertNotNull('Ожидалась сущность AcadTable', AcadTable);

    CheckEquals(2.048584359034151, AcadTable^.BreakHeight, 1e-4,
      'Высота разбиения (Break height) должна читаться из DXF ≈2.0486');
  finally
    Drawing.done;
  end;
end;

// issue #1307, часть 2. Изменение высоты разбиения должно пересегментировать
// таблицу: число частей определяется автоматически так, чтобы суммарная
// высота строк в каждой части не превышала заданного порога. tablerazdel.dxf
// загружается как [5,5,4] строк (главная часть + 2 продолжения = 14 строк).
//   • Очень большой порог → все строки помещаются в одну часть (0 продолжений,
//     главная часть содержит все 14 строк).
//   • Возврат к исходному порогу ≈2.0486 → снова 2 части-продолжения.
//   • Очень малый порог → больше частей, чем исходно.
procedure TAcadTableStyleTest.ChangingBreakHeightResegmentsTable;
var
  Drawing: TSimpleDrawing;
  AcadTable: PGDBObjAcadTable;
  MainRowsBefore: Integer;
begin
  LoadDrawingFromDXF(
    ExpandFileName('../../../cad_source/test/tablerazdel.dxf'), Drawing);
  try
    AcadTable := FindFirstAcadTable(Drawing.pObjRoot);
    AssertNotNull('Ожидалась сущность AcadTable', AcadTable);

    CheckEquals(2, AcadTable^.ContinuationPartCount,
      'Исходно таблица разбита на главную часть и 2 продолжения');
    MainRowsBefore := AcadTable^.RowCount;

    // Очень большой порог — все строки умещаются в одной части.
    AcadTable^.BreakHeight := 1000.0;
    CheckEquals(0, AcadTable^.ContinuationPartCount,
      'При большом пороге все строки должны помещаться в одну часть');
    CheckTrue(AcadTable^.RowCount > MainRowsBefore,
      'Объединённая главная часть должна содержать строки всех частей');

    // Возврат к исходному порогу — снова две части-продолжения.
    AcadTable^.BreakHeight := 2.048584359034151;
    CheckEquals(2, AcadTable^.ContinuationPartCount,
      'При исходном пороге ≈2.0486 таблица снова разбивается на 3 части');

    // Очень малый порог — каждая часть вмещает меньше строк, частей больше.
    AcadTable^.BreakHeight := 0.5;
    CheckTrue(AcadTable^.ContinuationPartCount > 2,
      'При малом пороге число частей-продолжений должно вырасти');
  finally
    Drawing.done;
  end;
end;

// issue #1307, часть 1. Изменение интервала между частями должно смещать
// части-продолжения относительно главной части. Таблица разбита по ширине
// (части идут вправо), поэтому увеличение интервала раздвигает части и
// увеличивает общую ширину отрисованного представления.
procedure TAcadTableStyleTest.ChangingBreakSpacingRepositionsParts;
var
  Drawing: TSimpleDrawing;
  AcadTable: PGDBObjAcadTable;
  MinX1, MaxX1, MinX2, MaxX2: Double;
  HasGap: Boolean;
begin
  LoadDrawingFromDXF(
    ExpandFileName('../../../cad_source/test/tablerazdel.dxf'), Drawing);
  try
    AcadTable := FindFirstAcadTable(Drawing.pObjRoot);
    AssertNotNull('Ожидалась сущность AcadTable', AcadTable);
    CheckTrue(AcadTable^.ContinuationPartCount > 0,
      'Таблица должна содержать части-продолжения');

    BuildTableGeometry(Drawing, AcadTable);
    CollectLineBounds(AcadTable^.ConstObjArray, MinX1, MaxX1, HasGap);
    Check(MaxX1 > MinX1, 'Таблица должна отрисовать линии в WCS');

    // Увеличиваем интервал между частями — части должны раздвинуться.
    AcadTable^.BreakSpacing := 10.0;
    BuildTableGeometry(Drawing, AcadTable);
    CollectLineBounds(AcadTable^.ConstObjArray, MinX2, MaxX2, HasGap);

    CheckTrue(MaxX2 > MaxX1 + 1e-6,
      'Увеличение интервала должно раздвинуть части и увеличить общую ширину');
  finally
    Drawing.done;
  end;
end;

// issue #1315. Направление разбиения должно быть редактируемым и менять
// положение частей-продолжений: Left переносит их влево от главной таблицы,
// Down — ниже главной таблицы.
procedure TAcadTableStyleTest.ChangingBreakDirectionRepositionsParts;
var
  Drawing: TSimpleDrawing;
  AcadTable: PGDBObjAcadTable;
  RightMinX, RightMaxX, RightMinY, RightMaxY: Double;
  LeftMinX, LeftMaxX, LeftMinY, LeftMaxY: Double;
  DownMinX, DownMaxX, DownMinY, DownMaxY: Double;
  HasLines: Boolean;
begin
  LoadDrawingFromDXF(
    ExpandFileName('../../../cad_source/test/tablerazdel.dxf'), Drawing);
  try
    AcadTable := FindFirstAcadTable(Drawing.pObjRoot);
    AssertNotNull('Ожидалась сущность AcadTable', AcadTable);
    CheckTrue(AcadTable^.ContinuationPartCount > 0,
      'Таблица должна содержать части-продолжения');
    CheckEquals(Ord(atbdRight), Ord(AcadTable^.BreakDirection),
      'Исходное направление тестовой таблицы должно быть Right');

    BuildTableGeometry(Drawing, AcadTable);
    CollectLineBounds2D(AcadTable^.ConstObjArray,
      RightMinX, RightMaxX, RightMinY, RightMaxY, HasLines);
    CheckTrue(HasLines, 'Таблица должна отрисовать линии в WCS');

    AcadTable^.BreakDirection := atbdLeft;
    CheckEquals(Ord(atbdLeft), Ord(AcadTable^.BreakDirection),
      'Свойство BreakDirection должно принимать значение Left');
    BuildTableGeometry(Drawing, AcadTable);
    CollectLineBounds2D(AcadTable^.ConstObjArray,
      LeftMinX, LeftMaxX, LeftMinY, LeftMaxY, HasLines);
    CheckTrue(HasLines, 'Таблица с направлением Left должна отрисовать линии');
    CheckTrue(LeftMinX < RightMinX - 1e-6,
      'Left должен переносить части-продолжения влево от главной таблицы');
    CheckTrue(LeftMaxX < RightMaxX - 1e-6,
      'Left не должен оставлять части-продолжения справа от главной таблицы');

    AcadTable^.BreakDirection := atbdDown;
    CheckEquals(Ord(atbdDown), Ord(AcadTable^.BreakDirection),
      'Свойство BreakDirection должно принимать значение Down');
    BuildTableGeometry(Drawing, AcadTable);
    CollectLineBounds2D(AcadTable^.ConstObjArray,
      DownMinX, DownMaxX, DownMinY, DownMaxY, HasLines);
    CheckTrue(HasLines, 'Таблица с направлением Down должна отрисовать линии');
    CheckTrue(DownMinY < RightMinY - 1e-6,
      'Down должен переносить части-продолжения ниже главной таблицы');
    CheckTrue(DownMaxX < RightMaxX - 1e-6,
      'Down должен перестать раскладывать части вправо');
  finally
    Drawing.done;
  end;
end;

// issue #1309, часть 1. Файл test/tablerazdel.dxf — разорванная таблица из
// трёх сегментов. Главная часть: строка 0 «Zagolovok» (Title), строка 1
// «A..E» (Header), далее строки данных. Каждый сегмент-продолжение начинается
// с тех же строк «Zagolovok» и «A..E», то есть таблица была разорвана с
// повтором верхних меток. Признак должен определяться по содержимому, а не по
// флагам подавления заголовков (реальный DXF приходит с TableFlags=22).
procedure TAcadTableStyleTest.DetectsBreakRepeatTopOnLoad;
var
  Drawing: TSimpleDrawing;
  AcadTable: PGDBObjAcadTable;
begin
  LoadDrawingFromDXF(
    ExpandFileName('../../../cad_source/test/tablerazdel.dxf'), Drawing);
  try
    AcadTable := FindFirstAcadTable(Drawing.pObjRoot);
    AssertNotNull('Ожидалась сущность AcadTable', AcadTable);
    CheckEquals(2, AcadTable^.ContinuationPartCount,
      'Таблица должна содержать две части-продолжения');
    CheckTrue(AcadTable^.BreakRepeatTopLabels,
      'Повтор верхних меток должен определяться как True');
    // Обе части-продолжения должны начинаться с повторённых строк-меток.
    CheckEquals('Zagolovok', AcadTable^.ContinuationPartCellText(0, 0, 0),
      'Часть 0, строка 0 должна повторять заголовок «Zagolovok»');
    CheckEquals('A', AcadTable^.ContinuationPartCellText(0, 1, 0),
      'Часть 0, строка 1 должна повторять шапку «A..E»');
    CheckEquals('Zagolovok', AcadTable^.ContinuationPartCellText(1, 0, 0),
      'Часть 1, строка 0 должна повторять заголовок «Zagolovok»');
    CheckEquals('A', AcadTable^.ContinuationPartCellText(1, 1, 0),
      'Часть 1, строка 1 должна повторять шапку «A..E»');
  finally
    Drawing.done;
  end;
end;

// issue #1309, часть 2 / issue #1311. Установка BreakRepeatTopLabels=False
// должна удалить повторяющиеся ведущие строки-метки (Title+Header, две строки)
// и пересегментировать строки по BreakHeight: освободившееся место заполняется
// строками из следующих частей, пустые части исчезают. Возврат в True добавляет
// метки обратно и снова пересегментирует таблицу.
procedure TAcadTableStyleTest.TogglingBreakRepeatTopAddsAndRemovesLabelRows;
var
  Drawing: TSimpleDrawing;
  AcadTable: PGDBObjAcadTable;
  P0Before, P1Before: Integer;
begin
  LoadDrawingFromDXF(
    ExpandFileName('../../../cad_source/test/tablerazdel.dxf'), Drawing);
  try
    AcadTable := FindFirstAcadTable(Drawing.pObjRoot);
    AssertNotNull('Ожидалась сущность AcadTable', AcadTable);
    P0Before := AcadTable^.ContinuationPartRowCount(0);
    P1Before := AcadTable^.ContinuationPartRowCount(1);
    CheckTrue(AcadTable^.BreakRepeatTopLabels,
      'Исходно повтор верхних меток должен быть True');

    // Снимаем повтор: две строки-метки удаляются, строки следующих частей
    // подтягиваются в освободившееся место, пустая часть исчезает.
    AcadTable^.BreakRepeatTopLabels := False;
    CheckEquals(1, AcadTable^.ContinuationPartCount,
      'После снятия повтора строки должны перераспределиться без пустой части');
    CheckEquals(P0Before, AcadTable^.ContinuationPartRowCount(0),
      'Первая часть-продолжение должна заполнить освободившееся место');
    CheckEquals(-1, AcadTable^.ContinuationPartRowCount(1),
      'Пустая вторая часть-продолжение должна исчезнуть');
    CheckFalse(AcadTable^.RecomputeBreakRepeatTopLabels,
      'После удаления меток автоопределение должно давать False');
    // Первой строкой части 0 теперь должна стать строка данных «11».
    CheckEquals('11', AcadTable^.ContinuationPartCellText(0, 0, 0),
      'После снятия повтора часть 0 должна начинаться со строки данных');

    // Возвращаем повтор: строки-метки добавляются обратно.
    AcadTable^.BreakRepeatTopLabels := True;
    CheckEquals(2, AcadTable^.ContinuationPartCount,
      'После возврата повтора таблица снова должна разбиться на две части-продолжения');
    CheckEquals(P0Before, AcadTable^.ContinuationPartRowCount(0),
      'После возврата повтора число строк части 0 должно восстановиться');
    CheckEquals(P1Before, AcadTable^.ContinuationPartRowCount(1),
      'После возврата повтора число строк части 1 должно восстановиться');
    CheckTrue(AcadTable^.RecomputeBreakRepeatTopLabels,
      'После добавления меток автоопределение должно давать True');
    CheckEquals('Zagolovok', AcadTable^.ContinuationPartCellText(0, 0, 0),
      'После возврата повтора часть 0 снова начинается с «Zagolovok»');
    CheckEquals('A', AcadTable^.ContinuationPartCellText(0, 1, 0),
      'После возврата повтора часть 0 снова содержит шапку «A..E»');
  finally
    Drawing.done;
  end;
end;

// issue #1311. При снятии Repeat top labels строка данных «11» становится
// первой визуальной строкой части-продолжения. Её стиль должен остаться стилем
// данных, а не стать стилем Title/Header по новому локальному индексу строки.
procedure TAcadTableStyleTest.ClearingBreakRepeatTopKeepsDataRowFormatting;
var
  Drawing: TSimpleDrawing;
  AcadTable: PGDBObjAcadTable;
  SizeBefore, SizeAfter: Double;
begin
  LoadDrawingFromDXF(
    ExpandFileName('../../../cad_source/test/tablerazdel.dxf'), Drawing);
  try
    AcadTable := FindFirstAcadTable(Drawing.pObjRoot);
    AssertNotNull('Ожидалась сущность AcadTable', AcadTable);

    BuildTableGeometry(Drawing, AcadTable);
    CheckTrue(FindMTextSizeByTemplate(
      AcadTable^.ConstObjArray, '11', SizeBefore),
      'До снятия повтора должна отрисоваться строка данных 11');

    AcadTable^.BreakRepeatTopLabels := False;
    BuildTableGeometry(Drawing, AcadTable);
    CheckTrue(FindMTextSizeByTemplate(
      AcadTable^.ConstObjArray, '11', SizeAfter),
      'После снятия повтора должна отрисоваться строка данных 11');

    CheckEquals(SizeBefore, SizeAfter, 1e-6,
      'Смещение строки данных на место верхних меток не должно менять высоту текста');
  finally
    Drawing.done;
  end;
end;

// issue #1309. Неразорванная таблица (test/tablerazdel2.dxf) не имеет
// частей-продолжений, поэтому признак повтора верхних меток равен False.
procedure TAcadTableStyleTest.NonBrokenTableHasNoBreakRepeatTop;
var
  Drawing: TSimpleDrawing;
  AcadTable: PGDBObjAcadTable;
begin
  LoadDrawingFromDXF(
    ExpandFileName('../../../cad_source/test/tablerazdel2.dxf'), Drawing);
  try
    AcadTable := FindFirstAcadTable(Drawing.pObjRoot);
    AssertNotNull('Ожидалась сущность AcadTable', AcadTable);
    CheckEquals(0, AcadTable^.ContinuationPartCount,
      'Неразорванная таблица не должна иметь частей-продолжений');
    CheckFalse(AcadTable^.BreakRepeatTopLabels,
      'Неразорванная таблица не имеет повтора верхних меток');
  finally
    Drawing.done;
  end;
end;

begin
  RegisterTests([TAcadTableStyleTest]);
end.
