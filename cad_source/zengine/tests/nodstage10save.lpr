program nodstage10save;

// issue #1465: этап 10 ТЗ cad_source/zengine/TZ_NOD_NamedObjectDictionary.md,
// сквозной цикл DXF → NOD → AcadTable → объект ZCAD → NOD → DXF.
// Для каждого файла-образца с таблицами тест:
//
//  1. читает чертёж и снимает сводку всех таблиц: размеры, точка вставки,
//     стиль, флаги разрыва (вкл., повтор меток, ручные положение и высота),
//     промежуток и высота разрыва, строки (тип Title/Header/Data, высота,
//     тексты ячеек), части-продолжения (строки, положение, высота);
//  2. сохраняет чертёж в DXF 2007 (savedxf20XX) — дважды: с сохранёнными
//     исходными сущностями (raw) и после правки (модельный путь записи);
//  3. повторно читает сохранённый файл — сводка обязана совпасть;
//  4. по модели NOD сохранённого файла (TZAcadTableNODIndex) проверяет
//     round-trip записи таблиц, разрыв и продолжения, а также
//     ACDB_RECOMPOSE_DATA: ссылки на все главные таблицы.
//
// Использование:
//   nodstage10save [<корень репозитория>]

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, Interfaces,
  uzeffdxf, uzeffdxfout, uzedrawingsimple, uzeffmanager,
  uzgldrawcontext, uzeTypes, uzeconsts, gzctnrVectorTypes,
  uzegeometrytypes, uzeentity,
  uzeffdxfobjects, uzeffdxfnod, uzeffdxfnodacadtable,
  uzeacadtable_types, uzeacadtable_model;

const
  TestDir = 'cad_source/test/';
  { Чертёж без таблиц: запись ACDB_RECOMPOSE_DATA не нужна }
  NoTablesSample = 'polylinearc';
  TZFile = 'cad_source/zengine/TZ_NOD_NamedObjectDictionary.md';
  TZAcadTableFile = 'cad_source/zengine/TZ_AcadTable_NOD_issue1465.md';
  TemplateFile =
    'environment/runtimefiles/AllCPU-AllOS/common/cfg/components/' +
    'savetemplate2007.dxf';
  { Образцы: разорванные таблицы AutoCAD (ручные положение/высота),
    таблицы, сохранённые ZCAD, и таблицы с ошибками типов строк/высот }
  Samples: array[0..13] of string = (
    'acadtablerazdel2007_1', 'acadtablerazdel2007_2',
    'acadtablerazdel2007_3', 'acadtablerazdel2007_4',
    'zcadtablerazdel2007_1', 'zcadtablerazdel2007_2',
    'zcadtablerazdel2007_3', 'zcadtablerazdel2007_4',
    'tablerazdel', 'tablerazdel2', 'tablerazdel2007_1',
    'tablebugheader', 'bugbreaktable', 'tableheighttextbug');

type
  { Итоги сводки чертежа для сверки с индексом NOD сохранённого файла }
  TDrawingStats = record
    Tables: Integer;
    SplitTables: Integer;
    Continuations: Integer;
  end;

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

procedure CheckInt(AExpected, AActual: Int64; const S: string);
begin
  if AExpected = AActual then
    Ok(Format('%s = %d', [S, AActual]))
  else
    Fail(Format('%s: expected %d, got %d', [S, AExpected, AActual]));
end;

procedure LoadDrawing(const AFileName: string; var ADrawing: TSimpleDrawing);
var
  DC: TDrawContext;
  ZDC: TZDrawingContext;
begin
  ADrawing.init(nil);
  DC := ADrawing.CreateDrawingRC;
  ZDC.CreateRec(ADrawing, ADrawing.pObjRoot^, TLOLoad, DC);
  AddFromDXF(AFileName, ZDC);
end;

{ TSimpleDrawing.done не освобождает строковые поля (см. nodstage2). }
procedure DoneDrawing(var ADrawing: TSimpleDrawing);
begin
  ADrawing.done;
  ADrawing.RawClassesSection := '';
  ADrawing.RawObjectsSection := '';
end;

function F(AValue: Double): string;
begin
  Result := FormatFloat('0.###', AValue);
end;

procedure DumpRows(T: PGDBObjAcadTable; AOut: TStrings);
var
  R, C: Integer;
  S: string;
begin
  for R := 0 to T^.RowCount - 1 do begin
    S := Format('  row %d type=%d h=%s:', [R, T^.RowStyleTypeAt(R),
      F(T^.RowHeightAt(R))]);
    for C := 0 to T^.ColCount - 1 do
      S := S + ' [' + T^.CellTextAt(R, C) + ']';
    AOut.Add(S);
  end;
end;

procedure DumpParts(T: PGDBObjAcadTable; AOut: TStrings);
var
  P, R: Integer;
  S: string;
  Pt: TzePoint3d;
  H: Double;
begin
  for P := 0 to T^.ContinuationPartCount - 1 do begin
    T^.ContinuationPartPlacement(P, Pt, H);
    S := Format('  part %d rows=%d ins=(%s,%s) h=%s:',
      [P, T^.ContinuationPartRowCount(P), F(Pt.x), F(Pt.y), F(H)]);
    for R := 0 to T^.ContinuationPartRowCount(P) - 1 do
      S := S + ' [' + T^.ContinuationPartCellText(P, R, 0) + ']';
    AOut.Add(S);
  end;
end;

procedure DumpTable(T: PGDBObjAcadTable; AOut: TStrings);
begin
  AOut.Add(Format('table rows=%d cols=%d ins=(%s,%s) style=%s',
    [T^.RowCount, T^.ColCount, F(T^.InsertPoint.x), F(T^.InsertPoint.y),
     T^.TableStyleName]));
  AOut.Add(Format('  break en=%d dir=%d rtop=%d rbot=%d mpos=%d mh=%d ' +
    'sp=%s h=%s', [Ord(T^.BreakEnabled), Ord(T^.BreakDirection),
     Ord(T^.BreakRepeatTopLabels), Ord(T^.BreakRepeatBottomLabels),
     Ord(T^.BreakManualPosition), Ord(T^.BreakManualHeight),
     F(T^.BreakSpacing), F(T^.BreakHeight)]));
  DumpRows(T, AOut);
  DumpParts(T, AOut);
end;

{ Правка таблицы в ZCAD: смена направления разрыва туда и обратно
  сбрасывает исходные сущности, и запись идёт модельным путём. }
procedure TouchTable(T: PGDBObjAcadTable);
var
  Dir: TAcadTableBreakDirection;
begin
  Dir := T^.BreakDirection;
  if Dir = atbdRight then
    T^.BreakDirection := atbdLeft
  else
    T^.BreakDirection := atbdRight;
  T^.BreakDirection := Dir;
end;

function DumpDrawing(var ADrawing: TSimpleDrawing; AOut: TStrings;
  AEdit: Boolean): TDrawingStats;
var
  IR: itrec;
  E: PGDBObjEntity;
  T: PGDBObjAcadTable;
begin
  Result := Default(TDrawingStats);
  E := ADrawing.pObjRoot^.ObjArray.beginiterate(IR);
  while E <> nil do begin
    if E^.GetObjType = GDBAcadTableID then begin
      T := PGDBObjAcadTable(E);
      DumpTable(T, AOut);
      Inc(Result.Tables);
      if T^.ContinuationPartCount > 0 then
        Inc(Result.SplitTables);
      Inc(Result.Continuations, T^.ContinuationPartCount);
      if AEdit then
        TouchTable(T);
    end;
    E := ADrawing.pObjRoot^.ObjArray.iterate(IR);
  end;
end;

{ Секция OBJECTS файла (0/SECTION … 0/ENDSEC). }
function ExtractObjectsSection(const AFileName: string): string;
var
  L: TStringList;
  I: Integer;
  InObjects: Boolean;
begin
  Result := '';
  InObjects := False;
  L := TStringList.Create;
  try
    L.LoadFromFile(AFileName);
    I := 0;
    while I + 1 < L.Count do begin
      if not InObjects then
        InObjects := (Trim(L[I]) = '2') and (Trim(L[I + 1]) = 'OBJECTS');
      if InObjects then begin
        Result := Result + L[I] + LineEnding + L[I + 1] + LineEnding;
        if (Trim(L[I]) = '0') and (Trim(L[I + 1]) = 'ENDSEC') then
          Exit;
      end;
      Inc(I, 2);
    end;
  finally
    L.Free;
  end;
end;

function HandleInList(AHandle: TDWGHandle;
  const AList: TZDXFHandleArray): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(AList) do
    if AList[I] = AHandle then
      Exit(True);
  Result := False;
end;

{ Сверка индекса NOD сохранённого файла со сводкой чертежа }
procedure CheckIndex(AIndex: TZAcadTableNODIndex;
  const AStats: TDrawingStats; const AName: string);
var
  I, Split, Conts: Integer;
  Info: TZAcadTableSplitInfo;
begin
  CheckInt(AStats.Tables, AIndex.Count, AName + ': round-trip records');
  Check(AIndex.RecomposeHandle <> 0, AName + ': ACDB_RECOMPOSE_DATA');
  Split := 0;
  Conts := 0;
  for I := 0 to AIndex.Count - 1 do begin
    Info := AIndex.Infos[I];
    Check(HandleInList(Info.EntityHandle, AIndex.RecomposeRefs),
      AName + ': recompose refers to table ' +
      DXFHandleToStr(Info.EntityHandle));
    if IsAcadTableSplit(Info) and (Length(Info.Continuations) > 0) then
      Inc(Split);
    Inc(Conts, Length(Info.Continuations));
  end;
  CheckInt(AStats.SplitTables, Split, AName + ': split records');
  CheckInt(AStats.Continuations, Conts, AName + ': continuations');
end;

procedure CheckSavedNOD(const AFileName, AName: string;
  const AStats: TDrawingStats);
var
  Model: TZNODModel;
  Index: TZAcadTableNODIndex;
begin
  Model := TZNODModel.Create;
  try
    if not Model.LoadFromText(ExtractObjectsSection(AFileName)) then begin
      Fail(AName + ': LoadFromText: ' + Model.ParseError);
      Exit;
    end;
    Index := TZAcadTableNODIndex.Create(Model);
    try
      CheckIndex(Index, AStats, AName);
    finally
      Index.Free;
    end;
  finally
    Model.Free;
  end;
end;

procedure ReportDiff(ABefore, AAfter: TStrings; const AName: string);
var
  I: Integer;
begin
  for I := 0 to ABefore.Count - 1 do
    if (I >= AAfter.Count) or (ABefore[I] <> AAfter[I]) then begin
      Fail(Format('%s: line %d before "%s"', [AName, I, ABefore[I]]));
      if I < AAfter.Count then
        writeln('      after  "', AAfter[I], '"');
      Exit;
    end;
  if AAfter.Count > ABefore.Count then
    Fail(AName + ': extra lines after re-read');
end;

{ Сравнение дампов таблиц до записи и после повторного чтения }
procedure CompareDumps(Before, After: TStringList; const AName: string);
begin
  if Before.Text = After.Text then
    Ok(AName + ': tables equal after re-read')
  else
    ReportDiff(Before, After, AName);
end;

{ Цикл чтение → запись → чтение одного образца в режиме raw или edit }
procedure RoundTripSample(const ASample: string; AEdit: Boolean);
var
  Drawing: TSimpleDrawing;
  Before, After: TStringList;
  Stats: TDrawingStats;
  Name, OutFile: string;
begin
  Name := ASample + BoolToStr(AEdit, ' (edit)', ' (raw)');
  OutFile := GetTempDir(False) + 'nodstage10save_' + ASample +
    BoolToStr(AEdit, '_edit', '_raw') + '.dxf';
  Before := TStringList.Create;
  After := TStringList.Create;
  try
    LoadDrawing(Root + TestDir + ASample + '.dxf', Drawing);
    Stats := DumpDrawing(Drawing, Before, AEdit);
    Check(Stats.Tables > 0, Name + ': sample has tables');
    Check(savedxf20XX(OutFile, Root + TemplateFile, Drawing, ZCDxf2007),
      Name + ': savedxf20XX');
    DoneDrawing(Drawing);
    LoadDrawing(OutFile, Drawing);
    DumpDrawing(Drawing, After, False);
    DoneDrawing(Drawing);
    CompareDumps(Before, After, Name);
    CheckSavedNOD(OutFile, Name, Stats);
  finally
    Before.Free;
    After.Free;
  end;
end;

{ Без таблиц в чертеже обработчик ACDB_RECOMPOSE_DATA ничего не пишет }
procedure TestNoTables;
var
  Drawing: TSimpleDrawing;
  OutFile: string;
begin
  OutFile := GetTempDir(False) + 'nodstage10save_' + NoTablesSample + '.dxf';
  LoadDrawing(Root + TestDir + NoTablesSample + '.dxf', Drawing);
  try
    Check(savedxf20XX(OutFile, Root + TemplateFile, Drawing, ZCDxf2007),
      NoTablesSample + ': savedxf20XX');
  finally
    DoneDrawing(Drawing);
  end;
  Check(Pos(CAcadTableRecomposeKey, ExtractObjectsSection(OutFile)) = 0,
    NoTablesSample + ': no ACDB_RECOMPOSE_DATA without tables');
end;

{ Раздел этапа 10 в ТЗ NOD и описание исследования issue #1465 }
procedure TestDocs;
var
  L: TStringList;
begin
  Check(FileExists(Root + TZAcadTableFile), 'docs: ' + TZAcadTableFile);
  L := TStringList.Create;
  try
    L.LoadFromFile(Root + TZFile);
    Check(Pos('### Этап 10. ', L.Text) > 0, 'TZ: stage 10 section');
    Check(Pos('**Статус: выполнен (issue #1465).**', L.Text) > 0,
      'TZ: stage 10 status');
  finally
    L.Free;
  end;
end;

procedure TestSamples;
var
  I: Integer;
begin
  for I := Low(Samples) to High(Samples) do begin
    writeln('--- ', Samples[I]);
    RoundTripSample(Samples[I], False);
    RoundTripSample(Samples[I], True);
    Flush(Output);
  end;
end;

begin
  Root := '';
  if ParamCount > 0 then
    Root := IncludeTrailingPathDelimiter(ParamStr(1));
  Failed := 0;
  try
    TestSamples;
    TestNoTables;
    TestDocs;
  except
    on E: Exception do
      Fail('nodstage10save: ' + E.ClassName + ': ' + E.Message);
  end;
  if Failed = 0 then
    writeln('OK')
  else begin
    writeln('FAILED: ', Failed);
    Halt(1);
  end;
end.
