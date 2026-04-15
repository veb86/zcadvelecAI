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
  uzeenttext,
  uzeentblockinsert,
  uzeenttable,
  uzeentacadtable;

type
  TAcadTableStyleTest = class(TTestCase)
  published
    procedure LoadsCellTextStylesFromDXFTableStyle;
    procedure LoadsBreakSettingsFromDXF;
    procedure LoadsBreakSettingsFromSecondSample;
    procedure RendersBrokenTableAsSeparatedFragments;
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

procedure CollectMTextStyles(const ARoot: PGDBObjGenericSubEntry;
  AStyles: TStrings);
var
  IR: itrec;
  PEntity: PGDBObjEntity;
  PMText: PGDBObjMText;
begin
  PEntity := ARoot^.ObjArray.beginiterate(IR);
  while PEntity <> nil do
  begin
    if PEntity^.GetObjType = GDBMTextID then
    begin
      PMText := PGDBObjMText(PEntity);
      if (PMText^.TXTStyle <> nil) and (PMText^.TXTStyle^.Name <> '') then
        if AStyles.IndexOf(PMText^.TXTStyle^.Name) < 0 then
          AStyles.Add(PMText^.TXTStyle^.Name);
    end;
    PEntity := ARoot^.ObjArray.iterate(IR);
  end;
end;

procedure CollectLineBounds(const ARoot: PGDBObjGenericSubEntry;
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

  PEntity := ARoot^.ObjArray.beginiterate(IR);
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
    PEntity := ARoot^.ObjArray.iterate(IR);
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

procedure TAcadTableStyleTest.LoadsCellTextStylesFromDXFTableStyle;
var
  Drawing: TSimpleDrawing;
  StyleNames: TStringList;
  EntityCount: Integer;
begin
  EntityCount := LoadDrawingFromDXF(
    ExpandFileName('../../../cad_source/test/tablerazdel.dxf'), Drawing);
  try
    Check(EntityCount > 0, 'DXF должен загрузить сущности');

    StyleNames := TStringList.Create;
    try
      StyleNames.Sorted := False;
      StyleNames.Duplicates := dupIgnore;
      CollectMTextStyles(Drawing.pObjRoot, StyleNames);

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
  MinX, MaxX: Double;
  HasGap: Boolean;
begin
  LoadDrawingFromDXF(
    ExpandFileName('../../../cad_source/test/tablerazdel.dxf'), Drawing);
  try
    CollectLineBounds(Drawing.pObjRoot, MinX, MaxX, HasGap);
    Check(HasGap,
      'После применения правил разбиения у AcadTable должны быть разрывы между фрагментами');
    Check(MaxX - MinX > 20.0,
      'Ширина визуализации должна включать несколько разнесённых фрагментов таблицы');
  finally
    Drawing.done;
  end;
end;

begin
  RegisterTests([TAcadTableStyleTest]);
end.
