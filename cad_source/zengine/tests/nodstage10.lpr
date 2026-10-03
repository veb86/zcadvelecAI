program nodstage10;

// issue #1465: этап 10 ТЗ cad_source/zengine/TZ_NOD_NamedObjectDictionary.md
// (данные разрыва таблиц ACAD_TABLE через модель NOD, исследование
// cad_source/zengine/TZ_AcadTable_NOD_issue1465.md). Тест проверяет:
//
//  1. Индекс TZAcadTableNODIndex по эталонным файлам AutoCAD: основная
//     сущность — владелец расширенного словаря, флаги разрыва (вкл., повтор
//     верха, ручные позиции и высоты), промежуток, число повторяемых строк,
//     записи высот/позиций, диапазоны логических строк со смещениями,
//     продолжения, TABLECONTENT/TABLEGEOMETRY, типы строк Title/Header/Data,
//     ACDB_RECOMPOSE_DATA корневого словаря; 64-битные хэндлы.
//  2. Прежняя запись ZCAD (#1381: XRECORD без владельца, маркер
//     ZCAD_SPLIT_TABLE_ENTITY, 360 — основная сущность) читается.
//  3. Несколько разорванных таблиц в одном файле: у каждой — свои
//     продолжения и флаги (прежние глобальные сканы брали только первую).
//  4. Запись без разрыва (70 2) — таблица не считается разорванной.
//  5. Обратная сборка пар (BuildAcadTableSplitPairs) и повторный разбор
//     дают те же данные; формат совпадает с эталоном AutoCAD.
//
// Использование:
//   nodstage10 [<корень репозитория>]

{$mode objfpc}{$H+}

uses
  SysUtils, Classes,
  uzeTypes,
  uzeffdxfobjects, uzeffdxfnod, uzeffdxfnodacadtable;

const
  TestDir = 'cad_source/test/';
  Razdel1File = TestDir + 'acadtablerazdel2007_1.dxf';
  Razdel4File = TestDir + 'acadtablerazdel2007_4.dxf';
  BugBreakFile = TestDir + 'bugbreaktable.dxf';
  LegacyFile = TestDir + 'tablebugheader3.dxf';
  Acad2007File = 'DXFTableSaveNEW/acadtable2007.dxf';
  { Допуск сравнения вещественных значений }
  Eps = 1e-6;

var
  Root: string;
  Failed: Integer;

procedure Fail(const S: string);
begin
  writeln('FAIL: ', S);
  Inc(Failed);
end;

procedure Ok(const S: string);
begin
  writeln('ok:   ', S);
end;

procedure Check(ACondition: Boolean; const S: string);
begin
  if ACondition then
    Ok(S)
  else
    Fail(S);
end;

procedure CheckEquals(const AExpected, AActual, S: string);
begin
  if AExpected = AActual then
    Ok(S + ' = ' + AActual)
  else
    Fail(Format('%s: expected "%s", got "%s"', [S, AExpected, AActual]));
end;

procedure CheckInt(AExpected, AActual: Int64; const S: string);
begin
  CheckEquals(IntToStr(AExpected), IntToStr(AActual), S);
end;

procedure CheckFloat(AExpected, AActual: Double; const S: string);
begin
  if Abs(AExpected - AActual) < Eps then
    Ok(Format('%s = %g', [S, AActual]))
  else
    Fail(Format('%s: expected %g, got %g', [S, AExpected, AActual]));
end;

type
  TTestProc = procedure;

procedure Run(const AName: string; AProc: TTestProc);
begin
  writeln('--- ', AName);
  try
    AProc();
  except
    on E: Exception do
      Fail(AName + ': ' + E.ClassName + ': ' + E.Message);
  end;
  Flush(Output);
end;

{ Секция OBJECTS файла (0/SECTION … 0/ENDSEC). }
function ExtractObjectsSection(const AFileName: string): string;
var
  L: TStringList;
  I, J: Integer;
begin
  Result := '';
  L := TStringList.Create;
  try
    L.LoadFromFile(AFileName);
    I := 0;
    while I + 3 < L.Count do begin
      if (Trim(L[I]) = '0') and (Trim(L[I + 1]) = 'SECTION') and
         (Trim(L[I + 2]) = '2') and (Trim(L[I + 3]) = 'OBJECTS') then begin
        J := I;
        while J + 1 < L.Count do begin
          Result := Result + L[J] + LineEnding + L[J + 1] + LineEnding;
          if (Trim(L[J]) = '0') and (Trim(L[J + 1]) = 'ENDSEC') then
            Exit;
          Inc(J, 2);
        end;
        Exit;
      end;
      Inc(I, 2);
    end;
  finally
    L.Free;
  end;
end;

function LoadModel(const AText, AName: string): TZNODModel;
begin
  Result := TZNODModel.Create;
  if not Result.LoadFromText(AText) then
    Fail(AName + ': LoadFromText: ' + Result.ParseError);
end;

function H(const S: string): TDWGHandle;
begin
  if not TryDXFStrToHandle(S, Result) then
    raise Exception.Create('bad handle ' + S);
end;

function HandlesText(const AHandles: TZDXFHandleArray): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to High(AHandles) do begin
    if I > 0 then
      Result := Result + ',';
    Result := Result + DXFHandleToStr(AHandles[I]);
  end;
end;

function IntsText(const AValues: TZAcadTableRowTypes): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to High(AValues) do begin
    if I > 0 then
      Result := Result + ',';
    Result := Result + IntToStr(AValues[I]);
  end;
end;

{ Диапазоны строк: «первая..последняя@смещение X» через запятую }
function RangesText(const AInfo: TZAcadTableSplitInfo): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to High(AInfo.RowRanges) do begin
    if I > 0 then
      Result := Result + ',';
    Result := Result + Format('%d..%d@%.2f', [AInfo.RowRanges[I].StartRow,
      AInfo.RowRanges[I].EndRow, AInfo.RowRanges[I].OffsetX],
      DefaultFormatSettings);
  end;
end;

{ Флаги записей высот через запятую }
function HeightFlagsText(const AInfo: TZAcadTableSplitInfo): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to High(AInfo.Heights) do begin
    if I > 0 then
      Result := Result + ',';
    Result := Result + IntToStr(AInfo.Heights[I].Flags);
  end;
end;

function LoadIndexFromFile(const AFile: string;
  out AModel: TZNODModel): TZAcadTableNODIndex;
begin
  AModel := LoadModel(ExtractObjectsSection(Root + AFile), AFile);
  Result := TZAcadTableNODIndex.Create(AModel);
end;

procedure CheckMain(AIndex: TZAcadTableNODIndex; const AEntity: string;
  out AInfo: TZAcadTableSplitInfo);
begin
  Check(AIndex.FindByEntity(H(AEntity), AInfo),
    'entity ' + AEntity + ' has round-trip record');
  Check(IsAcadTableSplit(AInfo), 'entity ' + AEntity + ' is split');
  Check(not AInfo.Legacy, 'AutoCAD layout parsed strictly');
end;

procedure CheckContinuationsOwner(AIndex: TZAcadTableNODIndex;
  const AInfo: TZAcadTableSplitInfo);
var
  I: Integer;
  Main: TDWGHandle;
begin
  for I := 0 to High(AInfo.Continuations) do
    Check(AIndex.IsContinuation(AInfo.Continuations[I], Main) and
      (Main = AInfo.EntityHandle), 'continuation ' +
      DXFHandleToStr(AInfo.Continuations[I]) + ' -> main');
  Check(not AIndex.IsContinuation(AInfo.EntityHandle, Main),
    'main entity is not a continuation');
end;

procedure TestRazdel1;
var
  Model: TZNODModel;
  Index: TZAcadTableNODIndex;
  Info: TZAcadTableSplitInfo;
begin
  Index := LoadIndexFromFile(Razdel1File, Model);
  try
    CheckInt(1, Index.Count, 'records');
    CheckMain(Index, 'EB', Info);
    CheckEquals('358', DXFHandleToStr(Info.XRecordHandle), 'xrecord');
    CheckEquals('2B5', DXFHandleToStr(Info.ContentHandle), 'content');
    CheckEquals('2B6', DXFHandleToStr(Info.GeometryHandle), 'geometry');
    CheckInt(CAcadTableBreakEnable or CAcadTableBreakRepeatTop,
      Info.BreakFlags, 'flags');
    CheckInt(CAcadTableBreakDirectionRight, Info.BreakDirection, 'direction');
    CheckFloat(0.99, Info.BreakSpacing, 'spacing');
    CheckInt(2, Info.TopLabelRows, 'top label rows');
    CheckInt(0, Info.BottomLabelRows, 'bottom label rows');
    CheckEquals('2', HeightFlagsText(Info), 'height flags');
    CheckFloat(2.04858435903415, Info.Heights[0].Height, 'break height');
    CheckEquals('2..4@0.00,5..7@13.49,8..9@26.98', RangesText(Info),
      'row ranges');
    CheckEquals('2B7,307', HandlesText(Info.Continuations), 'continuations');
    CheckEquals('0,1,2,2,1,2,2,2,2,2', IntsText(Info.RowTypes),
      'row types Title/Header/Data');
    CheckContinuationsOwner(Index, Info);
    CheckEquals('2AB', DXFHandleToStr(Index.RecomposeHandle), 'recompose');
    CheckEquals('87,BA,BE,EB', HandlesText(Index.RecomposeRefs),
      'recompose refs');
  finally
    Index.Free;
    Model.Free;
  end;
end;

procedure TestRazdel4ManualPositionsHeights;
var
  Model: TZNODModel;
  Index: TZAcadTableNODIndex;
  Info: TZAcadTableSplitInfo;
begin
  Index := LoadIndexFromFile(Razdel4File, Model);
  try
    CheckMain(Index, 'EB', Info);
    CheckInt(CAcadTableBreakEnable or CAcadTableBreakRepeatTop or
      CAcadTableBreakManualHeights, Info.BreakFlags, 'flags (manual heights)');
    CheckEquals('2,3,3,2', HeightFlagsText(Info), 'height flags');
    CheckFloat(1.535976928622925, Info.Heights[1].Height, 'height part 2');
    CheckFloat(6.362778521174274, Info.Heights[1].Y, 'position Y part 2');
    CheckEquals('2..4@0.00,5..6@13.49,7..7@26.98,8..8@40.47,9..9@53.96',
      RangesText(Info), 'row ranges');
    CheckEquals('54C,5BC,62C,69C', HandlesText(Info.Continuations),
      'continuations');
  finally
    Index.Free;
    Model.Free;
  end;
end;

procedure TestReferenceAcad2007;
var
  Model: TZNODModel;
  Index: TZAcadTableNODIndex;
  Info: TZAcadTableSplitInfo;
begin
  Index := LoadIndexFromFile(Acad2007File, Model);
  try
    CheckMain(Index, 'EB', Info);
    CheckInt(27, Info.BreakFlags, 'flags (manual positions + heights)');
    CheckInt(3, Info.TopLabelRows, 'top label rows');
    CheckEquals('3..9@0.00,10..12@10.03,13..14@22.80', RangesText(Info),
      'row ranges');
    CheckFloat(-5.448387361068885, Info.RowRanges[1].OffsetY,
      'manual offset Y part 2');
    CheckEquals('A7A,AEE', HandlesText(Info.Continuations), 'continuations');
  finally
    Index.Free;
    Model.Free;
  end;
end;

procedure TestBugBreak64BitHandles;
var
  Model: TZNODModel;
  Index: TZAcadTableNODIndex;
  Info: TZAcadTableSplitInfo;
begin
  Index := LoadIndexFromFile(BugBreakFile, Model);
  try
    CheckMain(Index, '10003AE68', Info);
    CheckInt(1, Info.TopLabelRows, 'top label rows');
    CheckInt(11, Length(Info.RowRanges), 'ranges');
    CheckInt(10, Length(Info.Continuations), 'continuations');
    CheckEquals('10103D119', DXFHandleToStr(Info.Continuations[0]),
      'first continuation');
    CheckInt(240, Info.RowRanges[10].StartRow, 'last range start');
    CheckInt(258, Info.RowRanges[10].EndRow, 'last range end');
    CheckContinuationsOwner(Index, Info);
  finally
    Index.Free;
    Model.Free;
  end;
end;

procedure TestLegacyZCADRecord;
var
  Model: TZNODModel;
  Index: TZAcadTableNODIndex;
  Info: TZAcadTableSplitInfo;
begin
  Index := LoadIndexFromFile(LegacyFile, Model);
  try
    Check(Index.FindByEntity(H('A6'), Info), 'legacy main entity A6');
    Check(Info.Legacy, 'legacy record detected');
    Check(IsAcadTableSplit(Info), 'legacy table is split');
    CheckInt(3, Info.BreakFlags, 'legacy flags');
    CheckFloat(31.3333333333, Info.Heights[0].Height, 'legacy break height');
    CheckEquals('A7,A8', HandlesText(Info.Continuations),
      'legacy continuations');
    CheckContinuationsOwner(Index, Info);
  finally
    Index.Free;
    Model.Free;
  end;
end;

{ Объекты расширенного словаря сущности AEntity с round-trip записью. }
function XDictText(const AEntity, ADict, AXRec: string;
  AXRecObj: TZDXFRawObject): string;
var
  I: Integer;
begin
  Result :=
    '0'#10'DICTIONARY'#10'5'#10 + ADict + #10'330'#10 + AEntity + #10 +
    '100'#10'AcDbDictionary'#10'280'#10'1'#10'281'#10'1'#10 +
    '3'#10'ACAD_XREC_ROUNDTRIP'#10'360'#10 + AXRec + #10 +
    '0'#10'XRECORD'#10'5'#10 + AXRec + #10'330'#10 + ADict + #10 +
    '100'#10'AcDbXrecord'#10;
  for I := 0 to AXRecObj.PairCount - 1 do
    Result := Result + IntToStr(AXRecObj.Pairs[I].Code) + #10 +
      AXRecObj.Pairs[I].Value + #10;
end;

function SampleInfo(AContinuation: TDWGHandle;
  AFlags: Integer): TZAcadTableSplitInfo;
begin
  Result := Default(TZAcadTableSplitInfo);
  Result.ContentHandle := H('500');
  Result.GeometryHandle := H('501');
  Result.BreakFlags := AFlags;
  Result.BreakDirection := CAcadTableBreakDirectionRight;
  Result.BreakSpacing := 1.5;
  Result.TopLabelRows := 2;
  SetLength(Result.Heights, 2);
  Result.Heights[0].Height := 10;
  Result.Heights[0].Flags := CAcadTableHeightHasHeight;
  Result.Heights[1].X := 30;
  Result.Heights[1].Y := -4.25;
  Result.Heights[1].Height := 7.5;
  Result.Heights[1].Flags := CAcadTableHeightHasPosition or
    CAcadTableHeightHasHeight;
  SetLength(Result.RowRanges, 2);
  Result.RowRanges[0].StartRow := 2;
  Result.RowRanges[0].EndRow := 5;
  Result.RowRanges[1].OffsetX := 30;
  Result.RowRanges[1].OffsetY := -4.25;
  Result.RowRanges[1].StartRow := 6;
  Result.RowRanges[1].EndRow := 9;
  SetLength(Result.Continuations, 1);
  Result.Continuations[0] := AContinuation;
end;

function BuildBody(const AInfo: TZAcadTableSplitInfo): TZDXFRawObject;
begin
  Result := TZDXFRawObject.Create;
  BuildAcadTableSplitPairs(AInfo, Result);
end;

procedure TestBuildParseRoundTrip;
var
  Src, Info: TZAcadTableSplitInfo;
  Body: TZDXFRawObject;
  Model: TZNODModel;
  Index: TZAcadTableNODIndex;
begin
  Src := SampleInfo(H('20'), CAcadTableBreakEnable or
    CAcadTableBreakRepeatTop or CAcadTableBreakManualPositions or
    CAcadTableBreakManualHeights);
  Body := BuildBody(Src);
  Model := LoadModel(XDictText('10', '100', '101', Body), 'built');
  Index := TZAcadTableNODIndex.Create(Model);
  try
    CheckEquals('280,102,360,70,90,90,40,90,90,90',
      Format('%d,%d,%d,%d,%d,%d,%d,%d,%d,%d', [Body.Pairs[0].Code,
      Body.Pairs[1].Code, Body.Pairs[2].Code, Body.Pairs[3].Code,
      Body.Pairs[4].Code, Body.Pairs[5].Code, Body.Pairs[6].Code,
      Body.Pairs[7].Code, Body.Pairs[8].Code, Body.Pairs[9].Code]),
      'AutoCAD pair order');
    CheckMain(Index, '10', Info);
    CheckInt(Src.BreakFlags, Info.BreakFlags, 'flags');
    CheckFloat(1.5, Info.BreakSpacing, 'spacing');
    CheckInt(2, Info.TopLabelRows, 'top label rows');
    CheckEquals('2,3', HeightFlagsText(Info), 'height flags');
    CheckFloat(-4.25, Info.Heights[1].Y, 'height position Y');
    CheckFloat(7.5, Info.Heights[1].Height, 'height value');
    CheckEquals('2..5@0.00,6..9@30.00', RangesText(Info), 'ranges');
    CheckFloat(-4.25, Info.RowRanges[1].OffsetY, 'range offset Y');
    CheckEquals('20', HandlesText(Info.Continuations), 'continuations');
    CheckEquals('500', DXFHandleToStr(Info.ContentHandle), 'content');
    CheckEquals('501', DXFHandleToStr(Info.GeometryHandle), 'geometry');
  finally
    Index.Free;
    Model.Free;
    Body.Free;
  end;
end;

procedure TestSeveralTables;
var
  B1, B2: TZDXFRawObject;
  Model: TZNODModel;
  Index: TZAcadTableNODIndex;
  Info: TZAcadTableSplitInfo;
  Main: TDWGHandle;
begin
  B1 := BuildBody(SampleInfo(H('20'), CAcadTableBreakEnable or
    CAcadTableBreakRepeatTop));
  B2 := BuildBody(SampleInfo(H('40'), CAcadTableBreakEnable or
    CAcadTableBreakManualPositions));
  Model := LoadModel(XDictText('10', '100', '101', B1) +
    XDictText('30', '200', '201', B2), 'two tables');
  Index := TZAcadTableNODIndex.Create(Model);
  try
    CheckInt(2, Index.Count, 'two records');
    Check(Index.IsContinuation(H('40'), Main) and (Main = H('30')),
      'continuation 40 belongs to table 30');
    Check(Index.IsContinuation(H('20'), Main) and (Main = H('10')),
      'continuation 20 belongs to table 10');
    Check(Index.FindByEntity(H('30'), Info), 'table 30 found');
    CheckInt(CAcadTableBreakEnable or CAcadTableBreakManualPositions,
      Info.BreakFlags, 'table 30 has own flags');
  finally
    Index.Free;
    Model.Free;
    B2.Free;
    B1.Free;
  end;
end;

procedure TestSingleLayout;
const
  SingleBody =
    '280'#10'1'#10'102'#10'ACAD_ROUNDTRIP_2008_TABLE_ENTITY'#10 +
    '360'#10'102'#10'70'#10'2'#10'90'#10'1'#10'10'#10'0'#10'20'#10'0'#10 +
    '30'#10'0'#10'90'#10'0'#10'90'#10'5'#10'361'#10'103'#10;
var
  Empty: TZDXFRawObject;
  Model: TZNODModel;
  Index: TZAcadTableNODIndex;
  Info: TZAcadTableSplitInfo;
begin
  Empty := TZDXFRawObject.Create;
  Model := LoadModel(XDictText('10', '100', '101', Empty) + SingleBody +
    '0'#10'TABLECONTENT'#10'5'#10'102'#10'330'#10'101'#10 +
    '1'#10'TABLEROW_BEGIN'#10'90'#10'1'#10 +
    '1'#10'TABLEROW_BEGIN'#10'90'#10'3'#10, 'single');
  Index := TZAcadTableNODIndex.Create(Model);
  try
    Check(Index.FindByEntity(H('10'), Info), 'single-part record found');
    CheckInt(CAcadTableLayoutSingle, Info.Layout, 'layout 70 = 2');
    Check(not IsAcadTableSplit(Info), 'not split');
    CheckEquals('103', DXFHandleToStr(Info.GeometryHandle), 'geometry');
    CheckEquals('0,2', IntsText(Info.RowTypes), 'row types');
  finally
    Index.Free;
    Model.Free;
    Empty.Free;
  end;
end;

var
  I: Integer;
begin
  Root := '';
  for I := 1 to ParamCount do
    Root := IncludeTrailingPathDelimiter(ParamStr(I));
  Failed := 0;

  Run('TestRazdel1', @TestRazdel1);
  Run('TestRazdel4ManualPositionsHeights', @TestRazdel4ManualPositionsHeights);
  Run('TestReferenceAcad2007', @TestReferenceAcad2007);
  Run('TestBugBreak64BitHandles', @TestBugBreak64BitHandles);
  Run('TestLegacyZCADRecord', @TestLegacyZCADRecord);
  Run('TestBuildParseRoundTrip', @TestBuildParseRoundTrip);
  Run('TestSeveralTables', @TestSeveralTables);
  Run('TestSingleLayout', @TestSingleLayout);

  if Failed = 0 then
    writeln('OK')
  else begin
    writeln('FAILED: ', Failed);
    Halt(1);
  end;
end.
