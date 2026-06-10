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

    CheckFalse(AcadTable^.BreakEnabled, 'Разрыв таблицы должен читаться из DXF');
    CheckEquals(Ord(atbdRight), Ord(AcadTable^.BreakDirection),
      'Направление разрыва должно читаться из DXF');
    CheckTrue(AcadTable^.BreakRepeatTopLabels,
      'Повторение верхних меток должно читаться из DXF');
    CheckTrue(AcadTable^.BreakRepeatBottomLabels,
      'Повторение нижних меток должно читаться из DXF');
    CheckTrue(AcadTable^.BreakManualPosition,
      'Ручное положение частей таблицы должно читаться из DXF');
    CheckFalse(AcadTable^.BreakManualHeight,
      'Ручная высота частей таблицы должна читаться из DXF');
    CheckEquals(1.0, AcadTable^.BreakSpacing, 1e-9,
      'Интервал между частями таблицы должен читаться из DXF');
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

    CheckTrue(AcadTable^.BreakEnabled,
      'Признак включенного разрыва должен читаться из DXF');
    CheckEquals(Ord(atbdRight), Ord(AcadTable^.BreakDirection),
      'Направление разрыва должно читаться из второго DXF-образца');
    CheckTrue(AcadTable^.BreakRepeatTopLabels,
      'Повторение верхних меток должно читаться из второго DXF-образца');
    CheckTrue(AcadTable^.BreakRepeatBottomLabels,
      'Повторение нижних меток должно читаться из второго DXF-образца');
    CheckTrue(AcadTable^.BreakManualPosition,
      'Ручное положение частей должно читаться из второго DXF-образца');
    CheckFalse(AcadTable^.BreakManualHeight,
      'Ручная высота частей должна читаться из второго DXF-образца');
    CheckEquals(0.0, AcadTable^.BreakSpacing, 1e-9,
      'Интервал между частями таблицы должен читаться из второго DXF-образца');
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

begin
  RegisterTests([TAcadTableStyleTest]);
end.
