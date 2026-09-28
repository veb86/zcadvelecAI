program nodstage3;

// issue #1448: этап 3 ТЗ cad_source/zengine/TZ_NOD_NamedObjectDictionary.md
// (реестр NOD-обработчиков uzeffdxfnodregistry). Тест проверяет:
//
//  1. Регистрация: порядок — порядок регистрации; пустой ключ и повторная
//     регистрация ключа (без учёта регистра) отклоняются; удаление.
//  2. RunNODLoadHandlers на синтетической модели: LoadProc вызывается только
//     для зарегистрированных ключей, которые есть в NOD и ссылаются на
//     словарь, в порядке регистрации (не в порядке ключей NOD); словарь-ветку
//     реестр помечает «забранным»; исключение в LoadProc не мешает
//     остальным обработчикам; модель без NOD и nil — вызовов нет.
//  3. AddFromDXF: LoadProc вызывается до разбора ENTITIES со словарём своего
//     ключа; для R12 и файла с нарушенной секцией OBJECTS не вызывается;
//     исключение в LoadProc не прерывает загрузку.
//  4. TZNODSaveSession: фильтр MinVersion, ReserveHandles и WriteObjects
//     идемпотентны.
//  5. Приёмка этапа 3: с зарегистрированным обработчиком-заглушкой
//     (ключ ACAD_TABLESTYLE) round-trip загрузка → сохранение даёт вывод,
//     совпадающий с эталонами этапа 0 (tablestyleetalon, empty; шаблоны
//     2000 и 2007; для tablestyleetalon — data/nod/stage3: с этапа 5
//     эталоны golden содержат стили эталона, а заглушка их не пишет). Заглушка вызывается при сохранении ровно по одному разу
//     (Classes, Reserve, Save — в порядке секций файла), в SaveProc приходит хэндл NOD
//     сохранённого файла. Обработчик с MinVersion = AC1018 при сохранении
//     в DXF 2000 не вызывается.
//  6. Контракт записи на обработчике, который пишет данные: хэндл из
//     ReserveHandlesProc выделен до сущностей (меньше хэндлов ENTITIES) и
//     меньше $HANDSEED, класс из ClassesProc — внутри секции CLASSES,
//     словарь из SaveProc — внутри OBJECTS (владелец — NOD файла),
//     повторяющихся хэндлов нет.
//  7. Модуль лога NOD выключен по умолчанию; при включении трасса реестра
//     форматируется без ошибок.
//
// Использование:
//   nodstage3 [<корень репозитория>]

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, Interfaces,
  uzeffdxf, uzeffdxfout, uzedrawingsimple, uzeffmanager, uzeffdxfsupport,
  uzgldrawcontext, uzeconsts, uzeTypes, uzctnrVectorBytesStream,
  uzeffdxfobjects, uzeffdxfnod, uzeffdxfnodregistry,
  uzbLogTypes, uzclog, uzeffdxfnodlog,
  // Регистрация загрузчика сущности ACAD_TABLE (как в nodstage0), чтобы
  // tablestyleetalon.dxf читался и сохранялся как в ZCAD.
  uzeacadtable_types, uzeacadtable_model, uzeacadtable_dxf_write,
  // Загрузчик LWPOLYLINE для EntitiesFile.
  uzeentlwpolyline;

const
  TemplatesDir = 'environment/runtimefiles/AllCPU-AllOS/common/cfg/components/';
  DataDir = 'cad_source/zengine/tests/data/nod/';
  GoldenDir = 'cad_source/zengine/tests/data/nod/golden/';
  { Вывод tablestyleetalon без записи стилей таблиц (эталоны golden до
    этапа 5): заглушка ACAD_TABLESTYLE стили не загружает и не пишет }
  Stage3Dir = 'cad_source/zengine/tests/data/nod/stage3/';
  EtalonFile = 'cad_source/test/tablestyleetalon.dxf';
  { Файл с сущностями и ключом ACAD_TABLESTYLE в NOD (в эталоне сущностей нет) }
  EntitiesFile = 'cad_source/test/polylinearc.dxf';

  { Ключ NOD, на который регистрируется заглушка (есть в эталоне) }
  TableStyleKey = 'ACAD_TABLESTYLE';
  { Ключ обработчика, который пишет данные (п. 6) }
  WriterKey = 'ZCAD_NODSTAGE3';
  WriterClassName = 'ZCADNODSTAGE3';

  { Переменные HEADER, зависящие от времени сохранения (как в nodstage0) }
  VolatileHeaderVars: array[0..3] of string = (
    '$TDCREATE', '$TDUCREATE', '$TDUPDATE', '$TDUUPDATE');

  { Синтетическая секция OBJECTS для RunNODLoadHandlers:
    KEY_A → словарь D (2 записи), KEY_B → XRECORD E (не словарь),
    KEY_C → F0 (объекта нет), KEY_R → словарь 10, KEY_UNREG → словарь 11
    (обработчика нет). }
  SyntheticObjects =
    '0'#10'SECTION'#10'2'#10'OBJECTS'#10 +
    '0'#10'DICTIONARY'#10'5'#10'C'#10'330'#10'0'#10'100'#10'AcDbDictionary'#10'281'#10'1'#10 +
    '3'#10'KEY_A'#10'350'#10'D'#10 +
    '3'#10'KEY_B'#10'350'#10'E'#10 +
    '3'#10'KEY_C'#10'350'#10'F0'#10 +
    '3'#10'KEY_R'#10'350'#10'10'#10 +
    '3'#10'KEY_UNREG'#10'350'#10'11'#10 +
    '0'#10'DICTIONARY'#10'5'#10'D'#10'330'#10'C'#10'100'#10'AcDbDictionary'#10'281'#10'1'#10 +
    '3'#10'Item1'#10'350'#10'12'#10'3'#10'Item2'#10'350'#10'13'#10 +
    '0'#10'XRECORD'#10'5'#10'E'#10'330'#10'C'#10'100'#10'AcDbXrecord'#10 +
    '0'#10'DICTIONARY'#10'5'#10'10'#10'330'#10'C'#10'100'#10'AcDbDictionary'#10 +
    '0'#10'DICTIONARY'#10'5'#10'11'#10'330'#10'C'#10'100'#10'AcDbDictionary'#10 +
    '0'#10'XRECORD'#10'5'#10'12'#10'330'#10'D'#10'100'#10'AcDbXrecord'#10 +
    '0'#10'XRECORD'#10'5'#10'13'#10'330'#10'D'#10'100'#10'AcDbXrecord'#10 +
    '0'#10'ENDSEC'#10;

var
  Root: string;
  Failed: Integer;
  { Журнал вызовов обработчиков: 'load:A:D:2', 'reserve:stub', … }
  Events: TStringList;
  { Число сущностей чертежа в момент последнего LoadProc }
  LoadEntityCount: Integer;
  { Хэндл NOD, переданный в последний SaveProc }
  SavedNODHandle: TDWGHandle;
  { Хэндл, выделенный ReserveHandlesProc обработчика-писателя }
  WriterDictHandle: TDWGHandle;

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

{ Запускает проверку; исключение считается провалом проверки. Реестр
  после проверки очищается. }
procedure Run(const AName: string; AProc: TTestProc);
begin
  writeln('--- ', AName);
  Events.Clear;
  try
    AProc();
  except
    on E: Exception do
      Fail(AName + ': ' + E.ClassName + ': ' + E.Message);
  end;
  while NODHandlerCount > 0 do
    UnregisterNODHandler(GetNODHandler(0).Key);
  Flush(Output);
end;

function EventsText: string;
begin
  Result := StringReplace(Trim(Events.Text), LineEnding, ' ', [rfReplaceAll]);
end;

{ ---------- Обработчики ---------- }

procedure LogLoad(const AName: string; const AModel: TZNODModel;
  const ADict: TZDXFDictionary; var ADrawing: TSimpleDrawing);
var
  I: Integer;
begin
  LoadEntityCount := ADrawing.pObjRoot^.ObjArray.Count;
  Events.Add(Format('load:%s:%s:%d:%s', [AName, ADict.RawObject.HandleStr,
    ADict.Count, BoolToStr(AModel.IsHandleClaimed(ADict.Handle), 'claimed', 'free')]));
  { Обработчик сам помечает объекты ветки }
  for I := 0 to ADict.Count - 1 do
    AModel.ClaimHandle(ADict[I].TargetHandle);
end;

procedure LoadA(const AModel: TZNODModel; const ADict: TZDXFDictionary;
  var ADrawing: TSimpleDrawing);
begin
  LogLoad('A', AModel, ADict, ADrawing);
end;

procedure LoadB(const AModel: TZNODModel; const ADict: TZDXFDictionary;
  var ADrawing: TSimpleDrawing);
begin
  LogLoad('B', AModel, ADict, ADrawing);
end;

procedure LoadC(const AModel: TZNODModel; const ADict: TZDXFDictionary;
  var ADrawing: TSimpleDrawing);
begin
  LogLoad('C', AModel, ADict, ADrawing);
end;

procedure LoadMissing(const AModel: TZNODModel; const ADict: TZDXFDictionary;
  var ADrawing: TSimpleDrawing);
begin
  LogLoad('MISSING', AModel, ADict, ADrawing);
end;

procedure LoadRaise(const AModel: TZNODModel; const ADict: TZDXFDictionary;
  var ADrawing: TSimpleDrawing);
begin
  Events.Add('load:R:raise');
  raise Exception.Create('nodstage3: LoadProc failure');
end;

{ Заглушка: ничего не пишет и хэндлов не выделяет. }
procedure StubLoad(const AModel: TZNODModel; const ADict: TZDXFDictionary;
  var ADrawing: TSimpleDrawing);
begin
  LogLoad('stub', AModel, ADict, ADrawing);
  if AModel.ResolveDictionary(TableStyleKey) <> ADict then
    Events.Add('load:stub:wrong-dict');
end;

function StubReserve(var ADrawing: TSimpleDrawing;
  var AIODXFContext: TIODXFSaveContext): TDWGHandle;
begin
  Events.Add('reserve:stub');
  Result := 0;
end;

procedure StubClasses(var AOutStream: TZctnrVectorBytes;
  var ADrawing: TSimpleDrawing; var AIODXFContext: TIODXFSaveContext);
begin
  Events.Add('classes:stub');
end;

procedure StubSave(var AOutStream: TZctnrVectorBytes;
  var ADrawing: TSimpleDrawing; var AIODXFContext: TIODXFSaveContext;
  ADictHandle, ANODHandle: TDWGHandle);
begin
  Events.Add(Format('save:stub:%d', [ADictHandle]));
  SavedNODHandle := ANODHandle;
end;

{ Обработчик-писатель: выделяет хэндл словаря, объявляет класс и пишет
  пустой словарь-ветку, принадлежащий NOD. }
function WriterReserve(var ADrawing: TSimpleDrawing;
  var AIODXFContext: TIODXFSaveContext): TDWGHandle;
begin
  Events.Add('reserve:writer');
  WriterDictHandle := AIODXFContext.handle;
  Inc(AIODXFContext.handle);
  Result := WriterDictHandle;
end;

procedure AddPair(var AOutStream: TZctnrVectorBytes; ACode: Integer;
  const AValue: string);
begin
  AOutStream.TXTAddStringEOL(IntToStr(ACode));
  AOutStream.TXTAddStringEOL(AValue);
end;

procedure WriterClasses(var AOutStream: TZctnrVectorBytes;
  var ADrawing: TSimpleDrawing; var AIODXFContext: TIODXFSaveContext);
begin
  Events.Add('classes:writer');
  AddPair(AOutStream, 0, 'CLASS');
  AddPair(AOutStream, 1, WriterClassName);
  AddPair(AOutStream, 2, 'AcDbZcadNodStage3');
  AddPair(AOutStream, 3, 'ObjectDBX Classes');
  AddPair(AOutStream, 90, '0');
  AddPair(AOutStream, 280, '0');
  AddPair(AOutStream, 281, '0');
end;

procedure WriterSave(var AOutStream: TZctnrVectorBytes;
  var ADrawing: TSimpleDrawing; var AIODXFContext: TIODXFSaveContext;
  ADictHandle, ANODHandle: TDWGHandle);
begin
  Events.Add(Format('save:writer:%s', [BoolToStr(ADictHandle = WriterDictHandle, 'dict', 'wrong-dict')]));
  SavedNODHandle := ANODHandle;
  AddPair(AOutStream, 0, 'DICTIONARY');
  AddPair(AOutStream, 5, DXFHandleToStr(ADictHandle));
  AddPair(AOutStream, 102, '{ACAD_REACTORS');
  AddPair(AOutStream, 330, DXFHandleToStr(ANODHandle));
  AddPair(AOutStream, 102, '}');
  AddPair(AOutStream, 330, DXFHandleToStr(ANODHandle));
  AddPair(AOutStream, 100, 'AcDbDictionary');
  AddPair(AOutStream, 281, '1');
end;

procedure RegisterStub(AMinVersion: TACDWGVer);
begin
  if not RegisterNODHandler(TableStyleKey, 'TABLESTYLE', @StubLoad,
      @StubReserve, @StubSave, @StubClasses, AMinVersion, 'Standard') then
    Fail('RegisterNODHandler(stub) failed');
end;

{ ---------- Вспомогательные ---------- }

procedure LoadDrawing(const AFileName: string; var ADrawing: TSimpleDrawing);
var
  DC: TDrawContext;
  ZDC: TZDrawingContext;
begin
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

function EntityCountAfterLoad(const AFileName: string): Integer;
var
  Drawing: TSimpleDrawing;
begin
  Drawing.init(nil);
  try
    LoadDrawing(AFileName, Drawing);
    Result := Drawing.pObjRoot^.ObjArray.Count;
  finally
    DoneDrawing(Drawing);
  end;
end;

type
  TDXFPairs = record
    Codes: array of Integer;
    Values: array of string;
    Count: Integer;
  end;

{ Все пары файла (код, значение без пробелов по краям) }
function ReadPairs(const AFileName: string): TDXFPairs;
var
  L: TStringList;
  I, Code: Integer;
begin
  Result.Count := 0;
  L := TStringList.Create;
  try
    L.LoadFromFile(AFileName);
    SetLength(Result.Codes, L.Count div 2);
    SetLength(Result.Values, L.Count div 2);
    I := 0;
    while I + 1 < L.Count do begin
      if not TryStrToInt(Trim(L[I]), Code) then
        raise Exception.CreateFmt('%s: line %d: bad group code "%s"',
          [AFileName, I + 1, L[I]]);
      Result.Codes[Result.Count] := Code;
      Result.Values[Result.Count] := Trim(L[I + 1]);
      Inc(Result.Count);
      Inc(I, 2);
    end;
  finally
    L.Free;
  end;
end;

{ Индекс пары 2/<AName> после 0/SECTION (-1 — секции нет) }
function SectionStart(const P: TDXFPairs; const AName: string): Integer;
var
  I: Integer;
begin
  for I := 1 to P.Count - 1 do
    if (P.Codes[I] = 2) and (P.Values[I] = AName) and
       (P.Codes[I - 1] = 0) and (P.Values[I - 1] = 'SECTION') then
      Exit(I);
  Result := -1;
end;

{ Индекс пары 0/ENDSEC, закрывающей секцию, начатую в AStart }
function SectionEnd(const P: TDXFPairs; AStart: Integer): Integer;
var
  I: Integer;
begin
  for I := AStart to P.Count - 1 do
    if (P.Codes[I] = 0) and (P.Values[I] = 'ENDSEC') then
      Exit(I);
  Result := -1;
end;

function PairsText(const P: TDXFPairs; AFrom, ATo: Integer): string;
var
  SB: TStringList;
  I: Integer;
begin
  SB := TStringList.Create;
  try
    for I := AFrom to ATo do begin
      SB.Add(IntToStr(P.Codes[I]));
      SB.Add(P.Values[I]);
    end;
    Result := SB.Text;
  finally
    SB.Free;
  end;
end;

{ Модель секции OBJECTS файла }
function ObjectsModelOfFile(const AFileName: string): TZNODModel;
var
  P: TDXFPairs;
  S, E: Integer;
begin
  P := ReadPairs(AFileName);
  Result := TZNODModel.Create;
  S := SectionStart(P, 'OBJECTS');
  if S < 0 then
    Exit;
  E := SectionEnd(P, S);
  if not Result.LoadFromText(PairsText(P, S - 1, E)) then
    Fail(AFileName + ': OBJECTS parse error: ' + Result.ParseError);
end;

{ Значение $HANDSEED. savedxf20XX пишет его как handle + $100000000
  (фиксированная ширина для правки на месте), старший разряд отбрасывается. }
function HandSeed(const P: TDXFPairs): TDWGHandle;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to P.Count - 3 do
    if (P.Codes[I] = 9) and (P.Values[I] = '$HANDSEED') then begin
      TryDXFStrToHandle(P.Values[I + 1], Result);
      if Result >= $100000000 then
        Dec(Result, $100000000);
      Exit;
    end;
end;

{ Заменяет значения переменных HEADER, зависящих от времени (как nodstage0). }
procedure NormalizeDXF(AText: TStringList);
var
  I, J: Integer;
begin
  I := 0;
  while I < AText.Count - 3 do begin
    if (Trim(AText[I]) = '9') then
      for J := Low(VolatileHeaderVars) to High(VolatileHeaderVars) do
        if Trim(AText[I + 1]) = VolatileHeaderVars[J] then begin
          AText[I + 3] := '<time>';
          Break;
        end;
    Inc(I);
  end;
end;

{ Сравнивает файл с эталоном (каталог ADir, по умолчанию GoldenDir) }
procedure CheckGolden(const AOutFile, AGolden: string; const ADir: string = GoldenDir);
var
  Actual, Expected: TStringList;
  I: Integer;
begin
  Actual := TStringList.Create;
  Expected := TStringList.Create;
  try
    Actual.LoadFromFile(AOutFile);
    NormalizeDXF(Actual);
    Expected.LoadFromFile(Root + ADir + AGolden);
    for I := 0 to Actual.Count - 1 do
      if (I >= Expected.Count) or (Actual[I] <> Expected[I]) then begin
        if I < Expected.Count then
          Fail(Format('%s: line %d: expected "%s", got "%s" (actual output: %s)',
            [AGolden, I + 1, Expected[I], Actual[I], AOutFile]))
        else
          Fail(Format('%s: output is longer than golden (%d > %d lines, actual output: %s)',
            [AGolden, Actual.Count, Expected.Count, AOutFile]));
        Exit;
      end;
    if Actual.Count <> Expected.Count then
      Fail(Format('%s: output is shorter than golden (%d < %d lines, actual output: %s)',
        [AGolden, Actual.Count, Expected.Count, AOutFile]))
    else
      Ok(Format('%s: output matches golden %s (%d lines)',
        [ExtractFileName(AOutFile), AGolden, Actual.Count]));
  finally
    Actual.Free;
    Expected.Free;
  end;
end;

{ Загружает AFileName ('' — пустой чертёж) и сохраняет по шаблону. }
procedure LoadAndSave(const AFileName, ATemplate: string; AVer: TZCDxfVersion;
  const AOutFile: string);
var
  Drawing: TSimpleDrawing;
begin
  Drawing.init(nil);
  try
    if AFileName <> '' then
      LoadDrawing(AFileName, Drawing);
    if not savedxf20XX(AOutFile, Root + TemplatesDir + ATemplate, Drawing, AVer) then
      Fail('savedxf20XX failed for ' + AOutFile);
  finally
    DoneDrawing(Drawing);
  end;
end;

function OutPath(const AName: string): string;
begin
  Result := GetTempDir(False) + 'nodstage3_' + AName;
end;

{ ---------- 1. Регистрация ---------- }

procedure TestRegistration;
var
  H: TZNODHandler;
begin
  CheckInt(0, NODHandlerCount, 'registry: empty at start');
  Check(RegisterNODHandler('KEY_B', 'XRECORD', @LoadB, nil, nil), 'register KEY_B');
  Check(RegisterNODHandler('KEY_A', 'XRECORD', @LoadA, nil, nil), 'register KEY_A');
  Check(not RegisterNODHandler('key_a', 'XRECORD', @LoadC, nil, nil),
    'duplicate key (other case) is rejected');
  Check(not RegisterNODHandler('', 'XRECORD', @LoadC, nil, nil),
    'empty key is rejected');
  CheckInt(2, NODHandlerCount, 'registry: count');
  CheckEquals('KEY_B', GetNODHandler(0).Key, 'registry: #0 (registration order)');
  CheckEquals('KEY_A', GetNODHandler(1).Key, 'registry: #1');
  Check(FindNODHandler('key_A', H) and (H.Key = 'KEY_A') and
    (H.MinVersion = CNODHandlerDefaultMinVersion) and (H.LoadProc = @LoadA),
    'FindNODHandler: case-insensitive, default MinVersion');
  Check(UnregisterNODHandler('KEY_B'), 'unregister KEY_B');
  Check(not UnregisterNODHandler('KEY_B'), 'unregister KEY_B again: not found');
  CheckInt(1, NODHandlerCount, 'registry: count after unregister');
  CheckEquals('KEY_A', GetNODHandler(0).Key, 'registry: #0 after unregister');
  CheckEquals('AC1015', ACDWGVerName(CNODHandlerDefaultMinVersion),
    'ACDWGVerName(default MinVersion)');
end;

{ ---------- 2. RunNODLoadHandlers на синтетической модели ---------- }

procedure TestRunLoadSynthetic;
var
  Model: TZNODModel;
  Drawing: TSimpleDrawing;
  Called: Integer;
begin
  Model := TZNODModel.Create;
  Drawing.init(nil);
  try
    Check(Model.LoadFromText(SyntheticObjects), 'synthetic OBJECTS parsed');
    Check(Model.NOD <> nil, 'synthetic NOD found');
    { Порядок регистрации отличается от порядка ключей в NOD }
    RegisterNODHandler('KEY_MISSING', 'X', @LoadMissing, nil, nil);
    RegisterNODHandler('KEY_R', 'X', @LoadRaise, nil, nil);
    RegisterNODHandler('key_a', 'X', @LoadA, nil, nil);
    RegisterNODHandler('KEY_B', 'X', @LoadB, nil, nil);
    RegisterNODHandler('KEY_C', 'X', @LoadC, nil, nil);
    Called := RunNODLoadHandlers(Model, Drawing);
    CheckInt(1, Called, 'RunNODLoadHandlers: successful LoadProc calls');
    CheckEquals('load:R:raise load:A:D:2:claimed', EventsText,
      'RunNODLoadHandlers: calls (registration order, only dictionaries in NOD)');
    Check(Model.IsHandleClaimed($D), 'dictionary of KEY_A is claimed by registry');
    Check(Model.IsHandleClaimed($10), 'dictionary of KEY_R is claimed (LoadProc raised)');
    Check(Model.IsHandleClaimed($12) and Model.IsHandleClaimed($13),
      'entries of KEY_A are claimed by handler');
    Check(not Model.IsHandleClaimed($E), 'KEY_B (not a dictionary) is not claimed');
    Check(not Model.IsHandleClaimed($11), 'KEY_UNREG (no handler) is not claimed');
    CheckInt(4, Model.ClaimedHandleCount, 'claimed handle count');

    Events.Clear;
    CheckInt(0, RunNODLoadHandlers(nil, Drawing), 'RunNODLoadHandlers(nil)');
    Model.Clear;
    CheckInt(0, RunNODLoadHandlers(Model, Drawing), 'RunNODLoadHandlers(model without NOD)');
    CheckEquals('', EventsText, 'no LoadProc calls without NOD');
  finally
    DoneDrawing(Drawing);
    Model.Free;
  end;
end;

{ ---------- 3. AddFromDXF ---------- }

procedure LoadGroupStub(const AModel: TZNODModel; const ADict: TZDXFDictionary;
  var ADrawing: TSimpleDrawing);
begin
  LogLoad('group', AModel, ADict, ADrawing);
end;

procedure TestLoad;
var
  Expected: Integer;
begin
  Expected := EntityCountAfterLoad(Root + EntitiesFile);
  Check(Expected > 0, Format('%s without handlers: %d entities',
    [EntitiesFile, Expected]));

  RegisterStub(CNODHandlerDefaultMinVersion);
  RegisterNODHandler('ACAD_GROUP', 'GROUP', @LoadGroupStub, nil, nil);
  RegisterNODHandler('ZCAD_NOT_IN_NOD', 'X', @LoadMissing, nil, nil);
  EntityCountAfterLoad(Root + EtalonFile);
  CheckEquals('load:stub:86:3:claimed load:group:D:0:claimed', EventsText,
    'etalon: LoadProc calls (ACAD_TABLESTYLE → 86, ACAD_GROUP → D)');

  { LoadProc вызывается до разбора ENTITIES }
  Events.Clear;
  LoadEntityCount := -1;
  CheckInt(Expected, EntityCountAfterLoad(Root + EntitiesFile),
    EntitiesFile + ' with handlers: entities');
  Check(Pos('load:stub:', EventsText) = 1,
    EntitiesFile + ': LoadProc of ACAD_TABLESTYLE is called (' + EventsText + ')');
  CheckInt(0, LoadEntityCount, EntitiesFile + ': entities at LoadProc time (before ENTITIES)');

  Events.Clear;
  CheckInt(1, EntityCountAfterLoad(Root + DataDir + 'nod_load_r12.dxf'), 'R12: entities');
  CheckEquals('', EventsText, 'R12 (OBJECTS with ACAD_GROUP): no LoadProc calls');

  Events.Clear;
  CheckInt(1, EntityCountAfterLoad(Root + DataDir + 'nod_load_broken_objects.dxf'),
    'broken OBJECTS: entities');
  CheckEquals('', EventsText, 'broken OBJECTS: no LoadProc calls');

  { Исключение в LoadProc не прерывает загрузку }
  UnregisterNODHandler(TableStyleKey);
  RegisterNODHandler(TableStyleKey, 'TABLESTYLE', @LoadRaise, nil, nil);
  Events.Clear;
  CheckInt(Expected, EntityCountAfterLoad(Root + EntitiesFile),
    EntitiesFile + ', LoadProc raises: entities still loaded');
  Events.Clear;
  EntityCountAfterLoad(Root + EtalonFile);
  CheckEquals('load:group:D:0:claimed load:R:raise', EventsText,
    'etalon, LoadProc raises: next handlers are called');
end;

{ ---------- 4. TZNODSaveSession ---------- }

procedure TestSaveSession;
var
  Session: TZNODSaveSession;
  Drawing: TSimpleDrawing;
  Ctx: TIODXFSaveContext;
  Stream: TZctnrVectorBytes;
begin
  RegisterNODHandler(WriterKey, 'X', nil, @WriterReserve, @WriterSave,
    @WriterClasses, AC1015);
  RegisterNODHandler(TableStyleKey, 'TABLESTYLE', nil, @StubReserve,
    @StubSave, @StubClasses, AC1018);

  Session := TZNODSaveSession.Create(AC1015);
  try
    CheckInt(1, Session.Count, 'session 2000: handlers (MinVersion AC1018 skipped)');
    CheckEquals(WriterKey, Session.Handlers[0].Key, 'session 2000: handler');
  finally
    Session.Free;
  end;

  Drawing.init(nil);
  Ctx.InitRec;
  Stream.init(1024);
  Session := TZNODSaveSession.Create(AC1021);
  try
    CheckInt(2, Session.Count, 'session 2007: handlers');
    Check(Session.LoadTemplate(Root + TemplatesDir + 'savetemplate2007.dxf'),
      'session: template NOD found');
    CheckEquals('C', DXFHandleToStr(Session.TemplateNODHandle), 'session: template NOD handle');
    Ctx.handle := $100;
    Session.ReserveHandles(Drawing, Ctx);
    Session.ReserveHandles(Drawing, Ctx);
    CheckEquals('reserve:writer reserve:stub', EventsText, 'ReserveHandles is idempotent');
    CheckEquals('100', DXFHandleToStr(Session.DictHandles[0]), 'DictHandles[0]');
    CheckEquals('101', DXFHandleToStr(Ctx.handle), 'handles allocated by handler');
    Events.Clear;
    Session.WriteObjects(Stream, Drawing, Ctx, $C);
    Session.WriteObjects(Stream, Drawing, Ctx, $C);
    CheckEquals('save:writer:dict save:stub:0', EventsText, 'WriteObjects is idempotent');
    CheckEquals('C', DXFHandleToStr(SavedNODHandle), 'SaveProc: NOD handle');
  finally
    Session.Free;
    Stream.done;
    Ctx.Done;
    DoneDrawing(Drawing);
  end;

  { Без обработчиков шаблон не читается }
  UnregisterNODHandler(WriterKey);
  UnregisterNODHandler(TableStyleKey);
  Session := TZNODSaveSession.Create(AC1021);
  try
    Check(not Session.LoadTemplate(Root + TemplatesDir + 'savetemplate2007.dxf') and
      (Session.TemplateNODHandle = 0), 'session without handlers: template is not read');
  finally
    Session.Free;
  end;
end;

{ ---------- 5. Приёмка: round-trip с заглушкой ---------- }

type
  TRoundTripCase = record
    Source: string;
    Template: string;
    Ver: TZCDxfVersion;
    Golden: string;
    Dir: string;
  end;

const
  RoundTripCases: array[0..3] of TRoundTripCase = (
    (Source: EtalonFile; Template: 'savetemplate2000.dxf'; Ver: ZCDxf2000; Golden: 'tablestyleetalon_2000.dxf'; Dir: Stage3Dir),
    (Source: EtalonFile; Template: 'savetemplate2007.dxf'; Ver: ZCDxf2007; Golden: 'tablestyleetalon_2007.dxf'; Dir: Stage3Dir),
    (Source: '';         Template: 'savetemplate2000.dxf'; Ver: ZCDxf2000; Golden: 'empty_2000.dxf'; Dir: GoldenDir),
    (Source: '';         Template: 'savetemplate2007.dxf'; Ver: ZCDxf2007; Golden: 'empty_2007.dxf'; Dir: GoldenDir)
  );

procedure TestRoundTripStub;
var
  I: Integer;
  C: TRoundTripCase;
  OutFile, Src, ExpectedEvents: string;
  Model: TZNODModel;
begin
  RegisterStub(CNODHandlerDefaultMinVersion);
  for I := Low(RoundTripCases) to High(RoundTripCases) do begin
    C := RoundTripCases[I];
    Events.Clear;
    SavedNODHandle := 0;
    OutFile := OutPath('stub_' + C.Golden);
    Src := '';
    if C.Source <> '' then
      Src := Root + C.Source;
    LoadAndSave(Src, C.Template, C.Ver, OutFile);
    CheckGolden(OutFile, C.Golden, C.Dir);
    { Секция CLASSES идёт раньше ENTITIES, поэтому ClassesProc вызывается
      до ReserveHandlesProc }
    ExpectedEvents := 'classes:stub reserve:stub save:stub:0';
    if C.Source <> '' then
      ExpectedEvents := 'load:stub:86:3:claimed ' + ExpectedEvents;
    CheckEquals(ExpectedEvents, EventsText, C.Golden + ': handler calls');
    Model := ObjectsModelOfFile(OutFile);
    try
      Check((Model.NOD <> nil) and (SavedNODHandle = Model.NOD.Handle),
        Format('%s: SaveProc got NOD handle of saved file (%s)',
          [C.Golden, DXFHandleToStr(SavedNODHandle)]));
    finally
      Model.Free;
    end;
  end;

  { MinVersion выше версии файла: в DXF 2000 обработчик не вызывается }
  UnregisterNODHandler(TableStyleKey);
  RegisterStub(AC1018);
  Events.Clear;
  OutFile := OutPath('stub_minver_tablestyleetalon_2000.dxf');
  LoadAndSave(Root + EtalonFile, 'savetemplate2000.dxf', ZCDxf2000, OutFile);
  CheckGolden(OutFile, 'tablestyleetalon_2000.dxf', Stage3Dir);
  CheckEquals('load:stub:86:3:claimed', EventsText,
    'MinVersion AC1018, DXF 2000: only LoadProc is called');
  Events.Clear;
  OutFile := OutPath('stub_minver_tablestyleetalon_2007.dxf');
  LoadAndSave(Root + EtalonFile, 'savetemplate2007.dxf', ZCDxf2007, OutFile);
  CheckGolden(OutFile, 'tablestyleetalon_2007.dxf', Stage3Dir);
  CheckEquals('load:stub:86:3:claimed classes:stub reserve:stub save:stub:0', EventsText,
    'MinVersion AC1018, DXF 2007: all procs are called');
end;

{ ---------- 6. Контракт записи на обработчике-писателе ---------- }

procedure CheckWriterOutput(const AOutFile, AName: string);
var
  P: TDXFPairs;
  S, E, I, ClassIdx, EntityHandles: Integer;
  H, MinEntity: TDWGHandle;
  Model: TZNODModel;
  Obj: TZDXFRawObject;
begin
  P := ReadPairs(AOutFile);

  S := SectionStart(P, 'CLASSES');
  E := SectionEnd(P, S);
  ClassIdx := -1;
  for I := 0 to P.Count - 1 do
    if (P.Codes[I] = 1) and (P.Values[I] = WriterClassName) then
      ClassIdx := I;
  Check((S >= 0) and (ClassIdx > S) and (ClassIdx < E),
    AName + ': class of ClassesProc is inside CLASSES');

  S := SectionStart(P, 'ENTITIES');
  E := SectionEnd(P, S);
  MinEntity := High(TDWGHandle);
  EntityHandles := 0;
  for I := S to E do
    if (P.Codes[I] = 5) and TryDXFStrToHandle(P.Values[I], H) then begin
      Inc(EntityHandles);
      if H < MinEntity then
        MinEntity := H;
    end;
  if EntityHandles = 0 then
    Ok(AName + ': no entities')
  else
    Check(WriterDictHandle < MinEntity,
      Format('%s: reserved handle %s < entity handles (min %s): reserved before ENTITIES',
        [AName, DXFHandleToStr(WriterDictHandle), DXFHandleToStr(MinEntity)]));
  Check((WriterDictHandle > 0) and (WriterDictHandle < HandSeed(P)),
    Format('%s: reserved handle %s < $HANDSEED %s',
      [AName, DXFHandleToStr(WriterDictHandle), DXFHandleToStr(HandSeed(P))]));

  Model := ObjectsModelOfFile(AOutFile);
  try
    CheckInt(0, Model.DuplicateHandleCount, AName + ': duplicate handles in OBJECTS');
    Obj := Model.FindObject(WriterDictHandle);
    Check((Obj <> nil) and (Model.FindDictionary(WriterDictHandle) <> nil),
      AName + ': dictionary of SaveProc is inside OBJECTS');
    Check((Model.NOD <> nil) and (Obj <> nil) and
      (Obj.OwnerHandle = Model.NOD.Handle) and (SavedNODHandle = Model.NOD.Handle),
      AName + ': dictionary owner = NOD handle passed to SaveProc');
    Check((Obj <> nil) and (Model.Objects.Last = Obj),
      AName + ': dictionary is the last object before ENDSEC');
  finally
    Model.Free;
  end;
end;

procedure TestWriterContract;
var
  OutFile: string;
begin
  RegisterNODHandler(WriterKey, 'X', nil, @WriterReserve, @WriterSave, @WriterClasses);
  { Обработчик ACAD_TABLESTYLE только с LoadProc: при записи не вызывается }
  RegisterNODHandler(TableStyleKey, 'TABLESTYLE', @StubLoad, nil, nil);

  Events.Clear;
  OutFile := OutPath('writer_tablestyleetalon_2007.dxf');
  LoadAndSave(Root + EtalonFile, 'savetemplate2007.dxf', ZCDxf2007, OutFile);
  CheckEquals('load:stub:86:3:claimed classes:writer reserve:writer save:writer:dict',
    EventsText, 'writer, etalon 2007: calls');
  CheckWriterOutput(OutFile, 'writer, etalon 2007');

  Events.Clear;
  OutFile := OutPath('writer_tablestyleetalon_2000.dxf');
  LoadAndSave(Root + EtalonFile, 'savetemplate2000.dxf', ZCDxf2000, OutFile);
  CheckEquals('load:stub:86:3:claimed classes:writer reserve:writer save:writer:dict',
    EventsText, 'writer, etalon 2000: calls');
  CheckWriterOutput(OutFile, 'writer, etalon 2000');

  { Файл с сущностями: хэндл ветки выделен раньше хэндлов сущностей }
  Events.Clear;
  OutFile := OutPath('writer_polylinearc_2000.dxf');
  LoadAndSave(Root + EntitiesFile, 'savetemplate2000.dxf', ZCDxf2000, OutFile);
  CheckEquals('load:stub:7E:1:claimed classes:writer reserve:writer save:writer:dict',
    EventsText, 'writer, ' + EntitiesFile + ' 2000: calls');
  CheckWriterOutput(OutFile, 'writer, ' + EntitiesFile + ' 2000');
end;

{ ---------- 7. Лог ---------- }

procedure TestTraceLog;
var
  OldLevel: TLogLevel;
  Model: TZNODModel;
  Drawing: TSimpleDrawing;
begin
  Check(not programlog.isModuleEnabled(NODLogModuleId),
    'log: module ' + NOD_LOG_MODULE_NAME + ' is disabled by default');
  OldLevel := programlog.GetCurrentLogLevel;
  programlog.EnableModule(NODLogModuleId);
  programlog.SetCurrentLogLevel(LM_Info, True);
  Model := TZNODModel.Create;
  Drawing.init(nil);
  try
    RegisterNODHandler('KEY_A', 'X', @LoadA, nil, nil);
    RegisterNODHandler('KEY_B', 'X', @LoadB, nil, nil);
    RegisterNODHandler('KEY_C', 'X', @LoadC, nil, nil);
    RegisterNODHandler(WriterKey, 'X', nil, @WriterReserve, @WriterSave,
      @WriterClasses, AC1018);
    Model.LoadFromText(SyntheticObjects);
    RunNODLoadHandlers(Model, Drawing);
    LoadAndSave(Root + EtalonFile, 'savetemplate2000.dxf', ZCDxf2000,
      OutPath('log_2000.dxf'));
    LoadAndSave(Root + EtalonFile, 'savetemplate2007.dxf', ZCDxf2007,
      OutPath('log_2007.dxf'));
    Ok('log: registry trace messages are formatted without errors');
  finally
    DoneDrawing(Drawing);
    Model.Free;
    programlog.DisableModule(NODLogModuleId);
    programlog.SetCurrentLogLevel(OldLevel, True);
  end;
end;

begin
  Root := '';
  if ParamCount > 0 then
    Root := IncludeTrailingPathDelimiter(ParamStr(1));
  Failed := 0;
  { Этап 4: адаптер ACAD_TABLESTYLE регистрируется в initialization
    uzeffdxfout; тесты этапа 3 проверяют реестр на своих заглушках,
    поэтому адаптер снимается (он проверяется в nodstage4). }
  UnregisterNODHandler('ACAD_TABLESTYLE');
  Events := TStringList.Create;
  try
    Run('TestRegistration', @TestRegistration);
    Run('TestRunLoadSynthetic', @TestRunLoadSynthetic);
    Run('TestLoad', @TestLoad);
    Run('TestSaveSession', @TestSaveSession);
    Run('TestRoundTripStub', @TestRoundTripStub);
    Run('TestWriterContract', @TestWriterContract);
    Run('TestTraceLog', @TestTraceLog);
  finally
    Events.Free;
  end;

  if Failed = 0 then
    writeln('OK')
  else begin
    writeln('FAILED: ', Failed);
    Halt(1);
  end;
end.
