program nodstage2;

// issue #1446: этап 2 ТЗ cad_source/zengine/TZ_NOD_NamedObjectDictionary.md
// (подключение NOD pre-pass к чтению DXF). Тест проверяет:
//
//  1. BuildDXFNODModel: корректная секция — модель с NOD; нарушенная
//     структура — False, описание ошибки и пустая модель; пустой текст —
//     пустая модель без ошибки. Быстрый разбор кодов групп (оптимизация
//     pre-pass) даёт тот же результат, что TryStrToInt.
//  2. AddFromDXF для DXF 2000+ строит модель до разбора TABLES/BLOCKS/
//     ENTITIES (в момент вызова DXFNODModelBuiltProc сущностей ещё нет),
//     модель совпадает с моделью секции OBJECTS файла.
//  3. R12: pre-pass не вызывается, модель пустая, даже если в файле есть
//     секция OBJECTS.
//  4. Ошибка разбора OBJECTS не прерывает загрузку: сущности загружены,
//     модель пустая. Файл без секции OBJECTS — пустая модель.
//  5. На всех тестовых DXF (cad_source/test, шаблоны сохранения, эталоны
//     этапа 0) модель строится без ошибок и совпадает с моделью секции
//     OBJECTS файла; для DXF 2000+ NOD найден.
//  6. Стили таблиц к загрузке ещё не подключены (этап 5):
//     DXFTableStyleTable после AddFromDXF пуста, как и до этапа 2.
//  7. При включённом модуле лога NOD трасса pre-pass форматируется без ошибок.
//
// Режим --bench (приёмка этапа 2 по времени): для больших DXF сравнивается
// время AddFromDXF и время pre-pass (BuildDXFNODModel по RawObjectsSection).
// Провал — доля pre-pass больше 5 % на файле приёмки (ops.dxf из
// dxfloadbench.cmd); для файлов с большими таблицами (секция OBJECTS в
// десятки мегабайт) доля выводится для сведения.
//
// Использование:
//   nodstage2 [<корень репозитория>] [--bench]

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, Interfaces,
  uzeffdxf, uzedrawingsimple, uzeffmanager, uzeffdxfsupport,
  uzgldrawcontext, uzeconsts, uzeTypes,
  uzeffdxfobjects, uzeffdxfnod,
  uzbLogTypes, uzclog, uzeffdxfnodlog,
  // Регистрация загрузчика сущности ACAD_TABLE (как в nodstage0/1), чтобы
  // файлы с таблицами загружались так же, как в ZCAD.
  uzeacadtable_types, uzeacadtable_model, uzeacadtable_dxf_write;

const
  TemplatesDir = 'environment/runtimefiles/AllCPU-AllOS/common/cfg/components/';
  DataDir = 'cad_source/zengine/tests/data/nod/';
  GoldenDir = 'cad_source/zengine/tests/data/nod/golden/';
  TestDir = 'cad_source/test/';
  EtalonFile = 'cad_source/test/tablestyleetalon.dxf';
  { Допустимая доля pre-pass во времени загрузки (приёмка этапа 2) }
  MaxPrePassPercent = 5.0;

var
  Root: string;
  Failed: Integer;
  BenchMode: Boolean;

  { Состояние, записанное DXFNODModelBuiltProc во время загрузки }
  HookCalls: Integer;
  HookEntityCount: Integer;
  HookSignature: string;
  HookNODHandle: string;
  HookTableStyleDict: string;

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

type
  TTestProc = procedure;

{ Запускает проверку; исключение считается провалом проверки. }
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

{ Текст файла без изменений (переводы строк сохраняются). }
function ReadFileText(const AFileName: string): string;
var
  F: TFileStream;
begin
  F := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(Result, F.Size);
    if F.Size > 0 then
      F.ReadBuffer(Result[1], F.Size);
  finally
    F.Free;
  end;
end;

{ Секция OBJECTS файла (0/SECTION … 0/ENDSEC) так же, как её сохраняет
  загрузчик в RawObjectsSection (строки через LineEnding). '' — секции нет. }
function ExtractObjectsSection(const AFileName: string): string;
var
  L: TStringList;
  I, J: Integer;
  B: TStringBuilder;
begin
  Result := '';
  L := TStringList.Create;
  B := TStringBuilder.Create;
  try
    L.LoadFromFile(AFileName);
    I := 0;
    while I + 3 < L.Count do begin
      if (Trim(L[I]) = '0') and (Trim(L[I + 1]) = 'SECTION') and
         (Trim(L[I + 2]) = '2') and (Trim(L[I + 3]) = 'OBJECTS') then begin
        J := I;
        while J + 1 < L.Count do begin
          B.Append(L[J]).Append(LineEnding).Append(L[J + 1]).Append(LineEnding);
          if (Trim(L[J]) = '0') and (Trim(L[J + 1]) = 'ENDSEC') then
            Break;
          Inc(J, 2);
        end;
        Break;
      end;
      Inc(I, 2);
    end;
    Result := B.ToString;
  finally
    B.Free;
    L.Free;
  end;
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

{ Полное описание модели: объекты, владельцы, реакторы, все пары. }
function ModelSignature(AModel: TZNODModel): string;
var
  I, J: Integer;
  Obj: TZDXFRawObject;
  B: TStringBuilder;
begin
  B := TStringBuilder.Create;
  try
    B.Append(Format('objects=%d dicts=%d roots=%d', [AModel.Objects.Count,
      AModel.Dictionaries.Count, AModel.RootDictionaryCount]));
    for I := 0 to AModel.Objects.Count - 1 do begin
      Obj := AModel.Objects[I];
      B.Append(Format('|%s %s o=%s r=%s x=%s n=%d', [Obj.ObjType,
        Obj.HandleStr, DXFHandleToStr(Obj.OwnerHandle), HandlesText(Obj.Reactors),
        DXFHandleToStr(Obj.XDictHandle), Obj.PairCount]));
      for J := 0 to Obj.PairCount - 1 do
        B.Append(Format(';%d=%s', [Obj.Pairs[J].Code, Obj.Pairs[J].Value]));
    end;
    Result := B.ToString;
  finally
    B.Free;
  end;
end;

function NODHandleText(AModel: TZNODModel): string;
begin
  if AModel.NOD = nil then
    Result := '<nil>'
  else
    Result := AModel.NOD.RawObject.HandleStr;
end;

{ Хэндл словаря NOD/ACAD_TABLESTYLE ('' — ключа нет) }
function TableStyleDictText(AModel: TZNODModel): string;
var
  Dict: TZDXFDictionary;
begin
  Dict := AModel.ResolveDictionary('ACAD_TABLESTYLE');
  if Dict = nil then
    Result := ''
  else
    Result := Dict.RawObject.HandleStr;
end;

{ DXFNODModelBuiltProc: запоминает модель и число сущностей чертежа в
  момент pre-pass (должно быть 0 — ENTITIES ещё не разбирались). }
procedure OnNODModelBuilt(const AModel: TZNODModel; var dwgCtx: TZDrawingContext);
begin
  Inc(HookCalls);
  HookEntityCount := dwgCtx.PDrawing^.pObjRoot^.ObjArray.Count;
  HookSignature := ModelSignature(AModel);
  HookNODHandle := NODHandleText(AModel);
  HookTableStyleDict := TableStyleDictText(AModel);
end;

procedure ResetHook;
begin
  HookCalls := 0;
  HookEntityCount := -1;
  HookSignature := '';
  HookNODHandle := '';
  HookTableStyleDict := '';
end;

function LoadDrawing(const AFileName: string; var ADrawing: TSimpleDrawing): TDXFHeaderInfo;
var
  DC: TDrawContext;
  ZDC: TZDrawingContext;
begin
  ResetHook;
  DC := ADrawing.CreateDrawingRC;
  ZDC.CreateRec(ADrawing, ADrawing.pObjRoot^, TLOLoad, DC);
  Result := AddFromDXF(AFileName, ZDC);
end;

function IsDXF20XX(const AHeader: TDXFHeaderInfo): Boolean;
begin
  Result := AHeader.Version in [AC1014, AC1015, AC1018, AC1021, AC1024, AC1027, AC1032];
end;

function EmptySignature: string;
var
  Model: TZNODModel;
begin
  Model := TZNODModel.Create;
  try
    Result := ModelSignature(Model);
  finally
    Model.Free;
  end;
end;

{ 1. BuildDXFNODModel }
procedure TestBuildDXFNODModel;
var
  Model: TZNODModel;
  Error: string;
begin
  Model := TZNODModel.Create;
  try
    Check(BuildDXFNODModel(ExtractObjectsSection(Root + EtalonFile), Model, Error),
      'etalon: BuildDXFNODModel returns True');
    CheckEquals('', Error, 'etalon: error');
    CheckInt(41, Model.Objects.Count, 'etalon: objects');
    CheckEquals('C', NODHandleText(Model), 'etalon: NOD');
    CheckEquals('86', TableStyleDictText(Model), 'etalon: NOD/ACAD_TABLESTYLE');

    { Модель переиспользуется: нарушенная структура очищает её полностью }
    Check(not BuildDXFNODModel(ReadFileText(Root + DataDir + 'nod_broken.dxf'),
      Model, Error), 'nod_broken: BuildDXFNODModel returns False');
    Check(Pos('line', Error) > 0, 'nod_broken: error describes the line: ' + Error);
    CheckInt(0, Model.Objects.Count, 'nod_broken: objects (empty model)');
    CheckInt(0, Model.Dictionaries.Count, 'nod_broken: dictionaries (empty model)');
    CheckEquals('<nil>', NODHandleText(Model), 'nod_broken: NOD');

    Check(BuildDXFNODModel('', Model, Error), 'empty text: BuildDXFNODModel returns True');
    CheckEquals('', Error, 'empty text: error');
    CheckInt(0, Model.Objects.Count, 'empty text: objects');

    Check(BuildDXFNODModel(ReadFileText(Root + DataDir + 'nod_unknown_keys.dxf'),
      Model, Error), 'nod_unknown_keys: BuildDXFNODModel returns True');
    CheckEquals('C', NODHandleText(Model), 'nod_unknown_keys: NOD');
  finally
    Model.Free;
  end;
end;

{ 1a. Коды групп: быстрый разбор (без выделения строки) и запасной
  TryStrToInt дают тот же результат, что и до оптимизации pre-pass. }
function ParseCodes(const AText: string; out AError: string): string;
var
  List: TZDXFRawObjectList;
  I: Integer;
begin
  Result := '';
  List := TZDXFRawObjectList.Create(True);
  try
    if not ParseDxfObjectsSection(AText, List, AError) then
      Exit('<error>');
    if List.Count <> 1 then
      Exit(Format('<%d objects>', [List.Count]));
    for I := 0 to List[0].PairCount - 1 do
      Result := Result + Format('%d=%s;', [List[0].Pairs[I].Code, List[0].Pairs[I].Value]);
  finally
    List.Free;
  end;
end;

procedure TestGroupCodes;
const
  LF = #10;
var
  Error: string;
begin
  CheckEquals('5=A;1=x;-3=y;5=z;1000000000=w;7= v ;',
    ParseCodes('  0' + LF + 'XRECORD' + LF + '  5' + LF + 'A' + LF +
      '1' + LF + 'x' + LF + ' -3' + LF + 'y' + LF +
      { Не цифры/больше 9 цифр — через TryStrToInt, как раньше }
      '+5' + LF + 'z' + LF + '1000000000' + LF + 'w' + LF +
      { Пробелы и табуляция вокруг кода; значение не обрезается }
      #9'7 '#13 + LF + ' v ' + LF, Error), 'group codes: fast path and fallback');
  CheckEquals('<error>', ParseCodes('  0' + LF + 'XRECORD' + LF +
    '99999999999999999999' + LF + 'v' + LF, Error), 'group code out of range');
  Check(Pos('invalid group code "99999999999999999999"', Error) > 0,
    'group code out of range: ' + Error);
  CheckEquals('<error>', ParseCodes('  0' + LF + 'XRECORD' + LF + ' - ' + LF + 'v' + LF,
    Error), 'group code "-"');
  Check(Pos('line 3: invalid group code "-"', Error) > 0, 'group code "-": ' + Error);
end;

{ 2. Pre-pass в AddFromDXF: до ENTITIES, та же модель, что у секции файла. }
procedure TestPrePassBeforeEntities;
var
  Drawing: TSimpleDrawing;
  Model: TZNODModel;
  Error: string;
  Header: TDXFHeaderInfo;
begin
  Drawing.init(nil);
  Model := TZNODModel.Create;
  try
    Header := LoadDrawing(Root + EtalonFile, Drawing);
    Check(IsDXF20XX(Header), 'etalon: DXF 2000+');
    CheckInt(1, HookCalls, 'etalon: DXFNODModelBuiltProc calls');
    { В эталоне сущностей нет; порядок «pre-pass до ENTITIES» проверяется
      на файлах с сущностями в TestBrokenObjects/TestNoObjects/TestAllFiles }
    CheckInt(0, HookEntityCount, 'etalon: entities at the moment of pre-pass');
    CheckEquals('C', HookNODHandle, 'etalon: NOD in load context');
    CheckEquals('86', HookTableStyleDict, 'etalon: NOD/ACAD_TABLESTYLE in load context');
    Check(BuildDXFNODModel(ExtractObjectsSection(Root + EtalonFile), Model, Error),
      'etalon: OBJECTS section of the file is parsed');
    Check(HookSignature = ModelSignature(Model),
      'etalon: model of load context equals model of the file');
    { 6. Стили таблиц — этап 5 }
    CheckInt(0, Drawing.DXFTableStyleTable.Count,
      'etalon: DXFTableStyleTable is empty after AddFromDXF (stage 5)');
  finally
    Model.Free;
    Drawing.done;
  end;
end;

{ 3–4. R12, ошибка разбора OBJECTS, нет секции OBJECTS. }
procedure CheckEmptyModelLoad(const AFileName: string; AExpect20XX: Boolean;
  AExpectRawObjects: Boolean);
var
  Drawing: TSimpleDrawing;
  Header: TDXFHeaderInfo;
begin
  Drawing.init(nil);
  try
    Header := LoadDrawing(Root + DataDir + AFileName, Drawing);
    Check(IsDXF20XX(Header) = AExpect20XX,
      Format('%s: DXF 2000+ = %s', [AFileName, BoolToStr(AExpect20XX, True)]));
    CheckInt(1, HookCalls, AFileName + ': DXFNODModelBuiltProc calls');
    Check(HookSignature = EmptySignature,
      AFileName + ': model is empty (' + Copy(HookSignature, 1, 40) + ')');
    CheckEquals('<nil>', HookNODHandle, AFileName + ': NOD');
    CheckInt(0, HookEntityCount, AFileName + ': entities at the moment of pre-pass');
    Check((Drawing.RawObjectsSection <> '') = AExpectRawObjects,
      Format('%s: RawObjectsSection is filled = %s',
        [AFileName, BoolToStr(AExpectRawObjects, True)]));
    CheckInt(1, Drawing.pObjRoot^.ObjArray.Count,
      AFileName + ': LINE is loaded (loading is not interrupted)');
  finally
    Drawing.done;
  end;
end;

procedure TestR12;
begin
  { В файле корректная секция OBJECTS с NOD: модель пустая только потому,
    что для R12 pre-pass не вызывается. }
  CheckEmptyModelLoad('nod_load_r12.dxf', False, True);
end;

procedure TestBrokenObjects;
begin
  CheckEmptyModelLoad('nod_load_broken_objects.dxf', True, True);
end;

procedure TestNoObjects;
begin
  CheckEmptyModelLoad('nod_load_no_objects.dxf', True, False);
end;

procedure CollectDXF(const ADir: string; AList: TStringList);
var
  SR: TSearchRec;
begin
  if FindFirst(ADir + '*', faAnyFile, SR) = 0 then
    try
      repeat
        if (SR.Name = '.') or (SR.Name = '..') then
          Continue;
        if (SR.Attr and faDirectory) <> 0 then
          CollectDXF(ADir + SR.Name + PathDelim, AList)
        else if SameText(ExtractFileExt(SR.Name), '.dxf') then
          AList.Add(ADir + SR.Name);
      until FindNext(SR) <> 0;
    finally
      FindClose(SR);
    end;
end;

{ 5. Все тестовые файлы. }
procedure TestAllFiles;
var
  Files: TStringList;
  I, Count20XX, CountWithEntities: Integer;
  Drawing: TSimpleDrawing;
  Header: TDXFHeaderInfo;
  Model: TZNODModel;
  Error, Name, Section: string;
  Parsed: Boolean;
begin
  Files := TStringList.Create;
  Model := TZNODModel.Create;
  try
    CollectDXF(Root + TestDir, Files);
    CollectDXF(Root + GoldenDir, Files);
    Files.Add(Root + TemplatesDir + 'savetemplate2000.dxf');
    Files.Add(Root + TemplatesDir + 'savetemplate2007.dxf');
    Files.Add(Root + TemplatesDir + 'empty.dxf');
    Files.Sort;
    Count20XX := 0;
    CountWithEntities := 0;
    for I := 0 to Files.Count - 1 do begin
      Name := ExtractRelativePath(Root, Files[I]);
      Drawing.init(nil);
      try
        try
          Header := LoadDrawing(Files[I], Drawing);
        except
          on E: Exception do begin
            { Файлы, которые не загружаются и без pre-pass, — не предмет
              этого теста; pre-pass при этом уже отработал (или нет). }
            writeln(Format('skip: %s: AddFromDXF: %s: %s', [Name, E.ClassName, E.Message]));
            Continue;
          end;
        end;
        if HookCalls <> 1 then begin
          Fail(Format('%s: DXFNODModelBuiltProc calls = %d', [Name, HookCalls]));
          Continue;
        end;
        if not IsDXF20XX(Header) then begin
          Check(HookSignature = EmptySignature, Name + ': not DXF 2000+, model is empty');
          Continue;
        end;
        Inc(Count20XX);
        Section := ExtractObjectsSection(Files[I]);
        Parsed := BuildDXFNODModel(Section, Model, Error);
        if not Parsed then
          Fail(Name + ': OBJECTS section is not parsed: ' + Error)
        else if HookEntityCount <> 0 then
          Fail(Format('%s: %d entities before pre-pass', [Name, HookEntityCount]))
        else if HookNODHandle = '<nil>' then
          Fail(Name + ': NOD not found')
        else if HookSignature <> ModelSignature(Model) then
          Fail(Name + ': model of load context differs from model of the file')
        else begin
          if Drawing.pObjRoot^.ObjArray.Count > 0 then
            Inc(CountWithEntities);
          Ok(Format('%s: %d objects, NOD %s, %d entities', [Name,
            Model.Objects.Count, HookNODHandle, Drawing.pObjRoot^.ObjArray.Count]));
        end;
      finally
        Drawing.done;
      end;
    end;
    Check(Count20XX >= 60, Format('DXF 2000+ files checked: %d of %d', [Count20XX, Files.Count]));
    Check(CountWithEntities >= 20, Format(
      'DXF 2000+ files with entities loaded after pre-pass: %d', [CountWithEntities]));
  finally
    Model.Free;
    Files.Free;
  end;
end;

{ 7. Трасса pre-pass при включённом модуле лога NOD. }
procedure TestTraceLog;
var
  Drawing: TSimpleDrawing;
  OldLevel: TLogLevel;
begin
  Check(not programlog.isModuleEnabled(NODLogModuleId),
    'log: module ' + NOD_LOG_MODULE_NAME + ' is disabled by default');
  OldLevel := programlog.GetCurrentLogLevel;
  programlog.EnableModule(NODLogModuleId);
  programlog.SetCurrentLogLevel(LM_Info, True);
  try
    Drawing.init(nil);
    try
      LoadDrawing(Root + EtalonFile, Drawing);
    finally
      Drawing.done;
    end;
    Drawing.init(nil);
    try
      LoadDrawing(Root + DataDir + 'nod_load_broken_objects.dxf', Drawing);
    finally
      Drawing.done;
    end;
    Ok('log: pre-pass trace messages are formatted without errors');
  finally
    programlog.DisableModule(NODLogModuleId);
    programlog.SetCurrentLogLevel(OldLevel, True);
  end;
end;

{ --bench: доля pre-pass во времени загрузки больших DXF. }
procedure TestBench;
const
  BenchFiles: array[0..3] of string = (
    // Файл dxfloadbench.cmd
    'environment/runtimefiles/AllCPU-AllOS/zcadelectrotech/data/examples/test_dxf/ops.dxf',
    'cad_source/test/bugbreaktable.dxf',
    'cad_source/test/tableheighttextbug.dxf',
    EtalonFile);
  Repeats = 3;
var
  I, R: Integer;
  Drawing: TSimpleDrawing;
  Model: TZNODModel;
  Error, Section, Msg: string;
  T0, LoadMs, PrePassMs: QWord;
  Percent: Double;
begin
  for I := Low(BenchFiles) to High(BenchFiles) do begin
    LoadMs := High(QWord);
    Section := '';
    for R := 1 to Repeats do begin
      Drawing.init(nil);
      try
        T0 := GetTickCount64;
        LoadDrawing(Root + BenchFiles[I], Drawing);
        T0 := GetTickCount64 - T0;
        if T0 < LoadMs then
          LoadMs := T0;
        Section := Drawing.RawObjectsSection;
      finally
        Drawing.done;
      end;
    end;
    { Pre-pass короткий — меряем 10 повторов и берём среднее }
    Model := TZNODModel.Create;
    try
      T0 := GetTickCount64;
      for R := 1 to 10 do
        BuildDXFNODModel(Section, Model, Error);
      PrePassMs := (GetTickCount64 - T0) div 10;
    finally
      Model.Free;
    end;
    if LoadMs = 0 then
      LoadMs := 1;
    Percent := 100.0 * PrePassMs / LoadMs;
    Msg := Format('bench: %s: load %d ms, OBJECTS %d bytes, pre-pass %d ms (%.2f %%)',
      [BenchFiles[I], LoadMs, Length(Section), PrePassMs, Percent]);
    if I = 0 then
      Check(Percent <= MaxPrePassPercent, Msg)
    else
      writeln('info: ', Msg);
  end;
end;

var
  I: Integer;
begin
  Root := '';
  BenchMode := False;
  for I := 1 to ParamCount do
    if ParamStr(I) = '--bench' then
      BenchMode := True
    else
      Root := IncludeTrailingPathDelimiter(ParamStr(I));
  Failed := 0;
  DXFNODModelBuiltProc := @OnNODModelBuilt;

  if BenchMode then
    Run('TestBench', @TestBench)
  else begin
    Run('TestBuildDXFNODModel', @TestBuildDXFNODModel);
    Run('TestGroupCodes', @TestGroupCodes);
    Run('TestPrePassBeforeEntities', @TestPrePassBeforeEntities);
    Run('TestR12', @TestR12);
    Run('TestBrokenObjects', @TestBrokenObjects);
    Run('TestNoObjects', @TestNoObjects);
    Run('TestAllFiles', @TestAllFiles);
    Run('TestTraceLog', @TestTraceLog);
  end;

  DXFNODModelBuiltProc := nil;
  if Failed = 0 then
    writeln('OK')
  else begin
    writeln('FAILED: ', Failed);
    Halt(1);
  end;
end.
