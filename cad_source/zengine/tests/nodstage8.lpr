program nodstage8;

// issue #1458: этап 8 ТЗ cad_source/zengine/TZ_NOD_NamedObjectDictionary.md
// (тесты, документация, лог). Тест проверяет:
//
//  1. Трассировочный лог NOD выключен по умолчанию: модуль NOD
//     зарегистрирован выключенным, NODLogTraceEnabled = False.
//  2. Загрузка и сохранение (DXF 2007 и 2000) штатных чертежей со стилями
//     таблиц, мультивыносок и сохраняемыми ветками NOD при настройках лога
//     по умолчанию не пишут в лог ни одного сообщения модуля NOD, а модули
//     NOD не пишут мимо него сообщений ниже LM_Warning (они видны всегда);
//     предупреждения допустимы.
//  3. Включение трассы так же, как ключом командной строки «lem NOD»
//     (включение модуля по имени, uzcsysinfo.pas): в логе есть разбор
//     (OBJECTS, NOD, TABLES), работа обработчиков (LoadProc/SaveProc,
//     стили таблиц и мультивыносок) и выделение хэндлов; уровень лога
//     выше LM_Info трассу глушит; после «ldm NOD» трасса снова пуста.
//  4. Тесты NOD подключены к сборке: у каждого nodstage*.lpr есть .lpi,
//     цель nodtests в Makefile и сценарий nodtests.sh берут все
//     nodstage*.lpr, CI (.github/workflows/nodtests.yml) вызывает цель
//     nodtests; в ТЗ у этапов 0–8 проставлен статус.
//
// Использование:
//   nodstage8 [<корень репозитория>]

{$mode objfpc}{$H+}


uses
  SysUtils, StrUtils, Classes, Interfaces,
  uzeffdxf, uzeffdxfout, uzedrawingsimple, uzeffmanager, uzeffdxfsupport,
  uzgldrawcontext, uzeconsts, uzeTypes,
  uzbLogTypes, uzclog, uzeffdxfnodlog,
  uzestylesmleaderdxfnod,
  // Регистрация загрузчика сущности ACAD_TABLE (как в nodstage0/4/5/6/7)
  uzeacadtable_types, uzeacadtable_model, uzeacadtable_dxf_write;

const
  TemplatesDir = 'environment/runtimefiles/AllCPU-AllOS/common/cfg/components/';
  Template2007 = 'savetemplate2007.dxf';
  Template2000 = 'savetemplate2000.dxf';
  EtalonFile = 'cad_source/test/tablestyleetalon.dxf';
  MLeaderFile = 'cad_source/test/mleaderblock.dxf';
  TestsDir = 'cad_source/zengine/tests/';
  TZFile = 'cad_source/zengine/TZ_NOD_NamedObjectDictionary.md';
  WorkflowFile = '.github/workflows/nodtests.yml';

  { Штатные чертежи: стили таблиц (эталон), сохраняемые ветки NOD
    (polylinearc.dxf: ACAD_SCALELIST, DWGPROPS, ACDB_RECOMPOSE_DATA),
    стили мультивыносок }
  DrawingFiles: array[0..3] of string = (
    EtalonFile,
    'cad_source/test/polylinearc.dxf',
    'cad_source/test/+mleader2008.dxf',
    MLeaderFile);

  { Префиксы сообщений модулей NOD (разбор, обработчики, запись) }
  NODMessagePrefixes: array[0..7] of string = (
    'uzeffdxfnod:', 'uzeffdxfnodregistry:', 'uzeffdxfnodzcad:',
    'uzeffdxfobjects:', 'uzestylestablesdxfnod:', 'uzestylesmleaderdxfnod:',
    'uzeffdxf: NOD', 'uzeffdxfout: ');

type
  { Бэкенд лога, собирающий сообщения в память: трасса модуля NOD
    отдельно; сообщения модулей NOD мимо модуля NOD — по уровню:
    предупреждения (LM_Warning и выше) и «утечки» трассы (ниже LM_Warning,
    видны при настройках лога по умолчанию). }
  TCaptureBackend = object(TLogerBaseBackend)
    procedure DoLog(const msg: TLogMsg; MsgOptions: TMsgOpt;
      LogMode: TLogLevel; LMDI: TModuleDesk); virtual;
  end;

var
  Root: string;
  Failed: Integer;
  Capture: TCaptureBackend;
  CaptureHandle: TLogExtHandle;
  Trace, Warnings, Leaks: TStringList;

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

function IsNODMessage(const AMsg: string): Boolean;
var
  I: Integer;
begin
  for I := Low(NODMessagePrefixes) to High(NODMessagePrefixes) do
    if Pos(NODMessagePrefixes[I], AMsg) = 1 then
      Exit(True);
  Result := False;
end;

procedure TCaptureBackend.DoLog(const msg: TLogMsg; MsgOptions: TMsgOpt;
  LogMode: TLogLevel; LMDI: TModuleDesk);
begin
  if LMDI = NODLogModuleId then
    Trace.Add(msg)
  else if IsNODMessage(msg) then begin
    if LogMode >= LM_Warning then
      Warnings.Add(msg)
    else
      Leaks.Add(msg);
  end;
end;

procedure ClearCapture;
begin
  Trace.Clear;
  Warnings.Clear;
  Leaks.Clear;
end;

{ Есть ли в трассе сообщение, содержащее все подстроки AParts }
function TraceHas(const AParts: array of string): Boolean;
var
  I, J: Integer;
  Found: Boolean;
begin
  for I := 0 to Trace.Count - 1 do begin
    Found := True;
    for J := Low(AParts) to High(AParts) do
      if Pos(AParts[J], Trace[I]) = 0 then begin
        Found := False;
        Break;
      end;
    if Found then
      Exit(True);
  end;
  Result := False;
end;

procedure CheckTrace(const AParts: array of string; const S: string);
begin
  Check(TraceHas(AParts), 'trace: ' + S);
end;

{ Первые сообщения списка — для диагностики провала }
procedure DumpFirst(AList: TStrings; const ATitle: string);
var
  I: Integer;
begin
  for I := 0 to AList.Count - 1 do begin
    if I >= 10 then begin
      writeln('  ... ', AList.Count - I, ' more');
      Break;
    end;
    writeln('  ', ATitle, ': ', AList[I]);
  end;
end;

function ReadFileText(const AFileName: string): string;
var
  L: TStringList;
begin
  L := TStringList.Create;
  try
    L.LoadFromFile(AFileName);
    Result := L.Text;
  finally
    L.Free;
  end;
end;

{ ---------- Вспомогательные ---------- }

{ Загрузка DXF с установкой кодовой страницы чертежа из $DWGCODEPAGE —
  как LoadDXFviaZEnfine (cad_source/zcad/register/uzcregfileformats.pas) }
procedure LoadDrawing(const AFileName: string; var ADrawing: TSimpleDrawing);
var
  DC: TDrawContext;
  ZDC: TZDrawingContext;
  Hdr: TDXFHeaderInfo;
begin
  DC := ADrawing.CreateDrawingRC;
  ZDC.CreateRec(ADrawing, ADrawing.pObjRoot^, TLOLoad, DC);
  Hdr := AddFromDXF(AFileName, ZDC);
  if Hdr.DWGCodePage <> CP_INVALID then
    ADrawing.DXFCodePage := SysCP2ZCCodePage(Hdr.iDWGCodePage);
end;

{ TSimpleDrawing.done не освобождает строковые поля (см. nodstage2). }
procedure DoneDrawing(var ADrawing: TSimpleDrawing);
begin
  ADrawing.done;
  ADrawing.RawClassesSection := '';
  ADrawing.RawObjectsSection := '';
end;

{ Загрузка чертежа и сохранение в DXF 2007 и 2000 }
procedure LoadAndSave(const AFileName: string);
var
  Drawing: TSimpleDrawing;
  Base: string;
begin
  Base := GetTempDir(False) + 'nodstage8_' +
    ChangeFileExt(ExtractFileName(AFileName), '');
  Drawing.init(nil);
  try
    LoadDrawing(Root + AFileName, Drawing);
    if not savedxf20XX(Base + '_2007.dxf', Root + TemplatesDir + Template2007,
      Drawing, ZCDxf2007) then
      Fail('savedxf20XX 2007 failed for ' + AFileName);
    if not savedxf20XX(Base + '_2000.dxf', Root + TemplatesDir + Template2000,
      Drawing, ZCDxf2000) then
      Fail('savedxf20XX 2000 failed for ' + AFileName);
  finally
    DoneDrawing(Drawing);
  end;
end;

{ ---------- 1. Лог выключен по умолчанию ---------- }

procedure TestDefaults;
begin
  Check(not programlog.isModuleEnabled(NODLogModuleId),
    'log: module ' + NOD_LOG_MODULE_NAME + ' is disabled by default');
  Check(not NODLogTraceEnabled, 'log: NODLogTraceEnabled = False by default');
  Check(programlog.RegisterModule(NOD_LOG_MODULE_NAME) = NODLogModuleId,
    'log: module ' + NOD_LOG_MODULE_NAME + ' is registered once');
  Check(LM_Info >= programlog.GetCurrentLogLevel,
    'log: default level passes LM_Info (trace is cut by the module only)');
end;

{ ---------- 2. Загрузка и сохранение без трассы ---------- }

procedure TestSilentByDefault;
var
  I: Integer;
begin
  for I := Low(DrawingFiles) to High(DrawingFiles) do begin
    ClearCapture;
    LoadAndSave(DrawingFiles[I]);
    CheckInt(0, Trace.Count, ExtractFileName(DrawingFiles[I]) +
      ': NOD trace messages with default log settings');
    DumpFirst(Trace, 'trace');
    { Трасса модулей NOD не должна обходить модуль NOD (LM_Info и ниже
      в модуле по умолчанию видны всегда) }
    CheckInt(0, Leaks.Count, ExtractFileName(DrawingFiles[I]) +
      ': NOD trace messages outside of module ' + NOD_LOG_MODULE_NAME);
    DumpFirst(Leaks, 'leak');
    { Предупреждения допустимы (например, ключ ACAD_MLEADERSTYLE не пишется
      в DXF 2000), они выводятся для сведения }
    if Warnings.Count > 0 then
      writeln('  ', ExtractFileName(DrawingFiles[I]), ': NOD warnings: ',
        Warnings.Count);
    DumpFirst(Warnings, 'warning');
  end;
end;

{ ---------- 3. Трасса «lem NOD» ---------- }

procedure TestTraceEnabled;
var
  OldLevel: TLogLevel;
begin
  { Как ключ командной строки «lem NOD» (uzcsysinfo.pas) }
  programlog.EnableModule(NOD_LOG_MODULE_NAME);
  try
    Check(programlog.isModuleEnabled(NODLogModuleId),
      'lem NOD: module is enabled');
    Check(NODLogTraceEnabled, 'lem NOD: NODLogTraceEnabled = True');

    ClearCapture;
    LoadAndSave(EtalonFile);
    Check(Trace.Count > 0, Format('lem NOD: %s: %d trace messages',
      [ExtractFileName(EtalonFile), Trace.Count]));
    { Разбор }
    CheckTrace(['uzeffdxf: NOD pre-pass:', 'objects'], 'NOD pre-pass');
    CheckTrace(['uzeffdxfnod: OBJECTS parsed:'], 'OBJECTS parsed');
    CheckTrace(['uzeffdxfnod: NOD ', 'keys', 'ACAD_TABLESTYLE='], 'NOD keys');
    { Обработчики }
    CheckTrace(['load: key "ACAD_TABLESTYLE", dictionary'],
      'LoadProc of ACAD_TABLESTYLE');
    CheckTrace(['load: key "ACAD_MLEADERSTYLE"'],
      'LoadProc of ACAD_MLEADERSTYLE');
    CheckTrace(['save: key "ACAD_TABLESTYLE": objects'],
      'SaveProc of ACAD_TABLESTYLE');
    { Выделение хэндлов }
    CheckTrace(['save: key "ACAD_TABLESTYLE": dictionary handle'],
      'handle of ACAD_TABLESTYLE dictionary');
    CheckTrace(['save: NOD entry "ACAD_TABLESTYLE" ->'],
      'NOD entry of ACAD_TABLESTYLE');
    CheckTrace(['save: template NOD'], 'template NOD');
    CheckTrace(['uzestylestablesdxfnod: ', '"Standard"'],
      'table style handler: style loaded');
    CheckTrace(['uzestylestablesdxfnod: ', 'TABLESTYLE'],
      'table style handler: TABLESTYLE written');
    CheckInt(0, Leaks.Count, 'lem NOD: NOD trace messages outside of module');

    { Стили мультивыносок: индекс символьных таблиц (NeedsSymbolTables) }
    ClearCapture;
    LoadAndSave(MLeaderFile);
    CheckTrace(['uzeffdxf: NOD pre-pass:', 'symbol records'],
      ExtractFileName(MLeaderFile) + ': pre-pass of TABLES');
    CheckTrace(['uzeffdxfnod: TABLES parsed:'],
      ExtractFileName(MLeaderFile) + ': TABLES parsed');
    CheckTrace(['load: key "ACAD_MLEADERSTYLE", dictionary'],
      ExtractFileName(MLeaderFile) + ': LoadProc of ACAD_MLEADERSTYLE');
    CheckTrace(['uzestylesmleaderdxfnod: ', '"Standard"'],
      ExtractFileName(MLeaderFile) + ': multileader style loaded');
    CheckTrace(['uzestylesmleaderdxfnod: ', 'хэндлы'],
      ExtractFileName(MLeaderFile) + ': handles of multileader styles');
    CheckInt(0, Leaks.Count, ExtractFileName(MLeaderFile) +
      ': NOD trace messages outside of module');

    { Сохраняемые ветки NOD: выделение хэндлов их объектам }
    ClearCapture;
    LoadAndSave('cad_source/test/polylinearc.dxf');
    CheckTrace(['load: NOD key "ACAD_SCALELIST" preserved'],
      'polylinearc.dxf: ACAD_SCALELIST preserved');
    CheckTrace(['save: preserved branches:', 'get new handles'],
      'polylinearc.dxf: handles of preserved branches');
    CheckTrace(['save: preserved key "ACAD_SCALELIST" is written'],
      'polylinearc.dxf: preserved key written');

    { Уровень выше LM_Info глушит трассу и при включённом модуле }
    OldLevel := programlog.GetCurrentLogLevel;
    programlog.SetCurrentLogLevel(LM_Warning, True);
    try
      Check(not NODLogTraceEnabled,
        'lem NOD, level LM_Warning: NODLogTraceEnabled = False');
      ClearCapture;
      LoadAndSave(EtalonFile);
      CheckInt(0, Trace.Count, 'lem NOD, level LM_Warning: trace messages');
    finally
      programlog.SetCurrentLogLevel(OldLevel, True);
    end;
  finally
    { Как ключ «ldm NOD» }
    programlog.DisableModule(NOD_LOG_MODULE_NAME);
  end;
  Check(not programlog.isModuleEnabled(NODLogModuleId),
    'ldm NOD: module is disabled');
  ClearCapture;
  LoadAndSave(EtalonFile);
  CheckInt(0, Trace.Count, 'ldm NOD: trace messages');
end;

{ ---------- 4. Тесты в Makefile и CI, статус в ТЗ ---------- }

procedure TestBuildIntegration;
var
  SR: TSearchRec;
  Names: TStringList;
  Makefile, Script, Workflow, TZ: string;
  I, P: Integer;
begin
  Names := TStringList.Create;
  try
    Names.Sorted := True;
    if FindFirst(Root + TestsDir + 'nodstage*.lpr', faAnyFile, SR) = 0 then
      try
        repeat
          Names.Add(ChangeFileExt(SR.Name, ''));
        until FindNext(SR) <> 0;
      finally
        FindClose(SR);
      end;
    Check(Names.IndexOf('nodstage0') >= 0, 'tests: nodstage0.lpr found');
    Check(Names.IndexOf('nodstage8') >= 0, 'tests: nodstage8.lpr found');
    for I := 0 to Names.Count - 1 do
      Check(FileExists(Root + TestsDir + Names[I] + '.lpi'),
        'tests: ' + Names[I] + '.lpi exists');
  finally
    Names.Free;
  end;

  Makefile := ReadFileText(Root + TestsDir + 'Makefile');
  Check(Pos('NODTESTS:=$(basename $(wildcard nodstage*.lpr))', Makefile) > 0,
    'Makefile: NODTESTS = all nodstage*.lpr');
  Check(Pos(#10'nodtests:', Makefile) > 0, 'Makefile: target nodtests');
  Check(Pos('nodtests.sh $(NODTESTS)', Makefile) > 0,
    'Makefile: nodtests runs nodtests.sh');
  Check(Pos(#10'nodtests-lazbuild:', Makefile) > 0,
    'Makefile: target nodtests-lazbuild');

  Script := ReadFileText(Root + TestsDir + 'nodtests.sh');
  Check(Pos('cad_source/zengine/tests/nodstage*.lpr', Script) > 0,
    'nodtests.sh: all nodstage*.lpr by default');
  Check(Pos('exit 1', Script) > 0, 'nodtests.sh: non-zero exit code on failure');

  Check(FileExists(Root + WorkflowFile), 'CI: ' + WorkflowFile + ' exists');
  if FileExists(Root + WorkflowFile) then begin
    Workflow := ReadFileText(Root + WorkflowFile);
    Check(Pos('make -C cad_source/zengine/tests nodtests', Workflow) > 0,
      'CI: runs make nodtests');
    Check(Pos('pull_request:', Workflow) > 0, 'CI: runs on pull requests');
  end;

  TZ := ReadFileText(Root + TZFile);
  for I := 0 to 8 do
    Check(Pos(Format('### Этап %d.', [I]), TZ) > 0,
      Format('TZ: stage %d section', [I]));
  { Статус «выполнен» — у каждого из этапов 0–8 (и последующих, этап 9 —
    issue #1459) }
  I := 0;
  P := Pos('**Статус: выполнен', TZ);
  while P > 0 do begin
    Inc(I);
    P := PosEx('**Статус: выполнен', TZ, P + 1);
  end;
  Check(I >= 9, Format('TZ: stages with status "выполнен": %d (>= 9)', [I]));
end;

begin
  Root := '';
  if ParamCount > 0 then
    Root := IncludeTrailingPathDelimiter(ParamStr(1));
  Failed := 0;
  Trace := TStringList.Create;
  Warnings := TStringList.Create;
  Leaks := TStringList.Create;
  Capture.init;
  CaptureHandle := programlog.addBackend(Capture, '', []);
  try
    Run('TestDefaults', @TestDefaults);
    Run('TestSilentByDefault', @TestSilentByDefault);
    Run('TestTraceEnabled', @TestTraceEnabled);
    Run('TestBuildIntegration', @TestBuildIntegration);
  finally
    programlog.removeBackend(CaptureHandle);
    Capture.Done;
    Leaks.Free;
    Warnings.Free;
    Trace.Free;
  end;

  if Failed = 0 then
    writeln('OK')
  else begin
    writeln('FAILED: ', Failed);
    Halt(1);
  end;
end.
