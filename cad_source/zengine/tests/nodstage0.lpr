program nodstage0;

// issue #1442: этап 0 ТЗ cad_source/zengine/TZ_NOD_NamedObjectDictionary.md
// (поддержка Named Object Dictionary). Тест проверяет:
//
//  1. TSimpleDrawing.init инициализирует DXFTableStyleTable даже в
//     «грязной» памяти (ZCAD выделяет чертёж через Getmem без обнуления,
//     см. TZCADDrawingsManager.CreateDWG). До исправления поле не
//     инициализировалось, и таблица содержала мусор.
//  2. TSimpleDrawing.done освобождает стили DXFTableStyleTable вместе с
//     вложенными CellFormats (нет утечек памяти).
//  3. Модули стилей zengine не зависят от uzcinterface (модуль слоя zcad).
//  4. Текущее поведение сохранения DXF зафиксировано эталонами
//     (golden-файлы в cad_source/zengine/tests/data/nod/golden/):
//     шаблоны savetemplate2000/savetemplate2007 — пустой чертёж, чертёж
//     со стилями таблиц и чертёж cad_source/test/tablestyleetalon.dxf.
//     Следующие этапы ТЗ сравнивают свой вывод с этими эталонами.
//     Этап 4 (issue #1450) осознанно изменил tablestyles_*: ветка
//     ACAD_TABLESTYLE пишется NOD-обработчиком в конце OBJECTS (другие
//     хэндлы и порядок объектов), в DXF 2000 появились словарь, стили и
//     класс TABLESTYLE. Эталоны этапа 0 сохранены в data/nod/stage0/;
//     nodstage4 проверяет эквивалентность 2007 с точностью до хэндлов и
//     порядка. Этап 5 (issue #1452) осознанно изменил:
//     tablestyleetalon_* — стили эталона (aits, Standard, vebts) теперь
//     загружаются NOD-обработчиком и записываются (до этапа 5 они терялись
//     при загрузке); tablestyles_2000 — класс CELLSTYLEMAP в DXF 2000 без
//     группы 91, как класс TABLESTYLE и классы шаблона. Этап 6 (issue
//     #1454) осознанно изменил эталоны DXF 2007: empty_2007,
//     tablestyles_2007 и tablestyleetalon_2007 — запись APPID
//     ACAD_MLEADERVER; tablestyleetalon_2007 — ещё ветка ACAD_MLEADERSTYLE
//     со стилем Standard (создаётся при загрузке файла без стилей
//     мультивыносок) и класс MLEADERSTYLE. Эталоны DXF 2000 не изменились.
//     Issue #1465 осознанно изменил tablestyleetalon_*: поля ячеек
//     CELLSTYLEMAP (CELLMARGIN, группы 40) берутся из стиля (0.06 —
//     группы 40/41 TABLESTYLE эталона) вместо жёстко заданных 1.5.
//
// Использование:
//   nodstage0 [<корень репозитория>] [--update]
// --update перезаписывает эталоны текущим выводом (только при осознанном
// изменении формата записи).

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, Interfaces,
  uzeffdxf, uzeffdxfout, uzedrawingsimple, uzeffmanager,
  uzgldrawcontext, uzeconsts, uzeTypes, gzctnrVectorTypes,
  uzestylestablesdxf,
  // Регистрация загрузчика сущности ACAD_TABLE (uzeacadtable_model),
  // чтобы таблица из tablestyleetalon.dxf читалась и сохранялась как в ZCAD.
  uzeacadtable_types, uzeacadtable_model, uzeacadtable_dxf_write;

const
  TemplatesDir = 'environment/runtimefiles/AllCPU-AllOS/common/cfg/components/';
  GoldenDir = 'cad_source/zengine/tests/data/nod/golden/';
  EtalonFile = 'cad_source/test/tablestyleetalon.dxf';

type
  TDrawingSource = (dsEmpty, dsStyles, dsEtalon);

  TGoldenCase = record
    Source: TDrawingSource;
    Template: string;
    Ver: TZCDxfVersion;
    Golden: string;
  end;

const
  GoldenCases: array[0..5] of TGoldenCase = (
    (Source: dsEmpty;  Template: 'savetemplate2000.dxf'; Ver: ZCDxf2000; Golden: 'empty_2000.dxf'),
    (Source: dsEmpty;  Template: 'savetemplate2007.dxf'; Ver: ZCDxf2007; Golden: 'empty_2007.dxf'),
    (Source: dsStyles; Template: 'savetemplate2000.dxf'; Ver: ZCDxf2000; Golden: 'tablestyles_2000.dxf'),
    (Source: dsStyles; Template: 'savetemplate2007.dxf'; Ver: ZCDxf2007; Golden: 'tablestyles_2007.dxf'),
    (Source: dsEtalon; Template: 'savetemplate2000.dxf'; Ver: ZCDxf2000; Golden: 'tablestyleetalon_2000.dxf'),
    (Source: dsEtalon; Template: 'savetemplate2007.dxf'; Ver: ZCDxf2007; Golden: 'tablestyleetalon_2007.dxf')
  );

  { Переменные HEADER, которые зависят от времени сохранения. Их значения
    при сравнении с эталоном заменяются на заглушку. }
  VolatileHeaderVars: array[0..3] of string = (
    '$TDCREATE', '$TDUCREATE', '$TDUPDATE', '$TDUUPDATE');

var
  Root: string;
  UpdateGolden: Boolean;
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

type
  TTestProc = procedure;

{ Запускает проверку; исключение (например, access violation при работе
  с неинициализированной таблицей) считается провалом проверки. }
procedure Run(const AName: string; AProc: TTestProc);
begin
  try
    AProc();
  except
    on E: Exception do
      Fail(AName + ': ' + E.ClassName + ': ' + E.Message);
  end;
  Flush(Output);
end;

{ Добавляет в таблицу DXF-стилей стиль с тремя форматами ячеек
  (data, title, header), как это делает команда AddDXFTableStyle. }
procedure AddSampleStyle(var ATable: GDBDXFTableStyleArray;
  const AName: string; AHeight: Double);
var
  Style: PTGDBDXFTableStyle;
  Cell: TGDBDXFTableCellStyle;
  I: Integer;
begin
  Style := ATable.AddStyle(AName);
  if Style = nil then begin
    Fail('AddStyle("' + AName + '") returned nil');
    Exit;
  end;
  for I := 0 to 2 do begin
    FillChar(Cell, SizeOf(Cell), 0);
    Cell.TextHeight := AHeight * (I + 1);
    Cell.Alignment := 5;
    Cell.TextColor := 256;
    Cell.BackgroundColor := 257;
    Cell.BackgroundColorEnabled := False;
    Style^.CellFormats.PushBackData(Cell);
    Style^.CellTextStyleName[I] := 'Standard';
  end;
end;

{ 1. Инициализация DXFTableStyleTable в «грязной» памяти. }
procedure TestInitOnDirtyMemory;
var
  PDrawing: PTSimpleDrawing;
  Iter: itrec;
begin
  Getmem(Pointer(PDrawing), SizeOf(TSimpleDrawing));
  FillChar(PDrawing^, SizeOf(TSimpleDrawing), $A5);
  PDrawing^.init(nil);
  try
    if PDrawing^.DXFTableStyleTable.Count <> 0 then
      Fail(Format('DXFTableStyleTable.Count after init = %d, expected 0',
        [PDrawing^.DXFTableStyleTable.Count]))
    else if PDrawing^.DXFTableStyleTable.beginiterate(Iter) <> nil then
      Fail('DXFTableStyleTable.beginiterate after init <> nil')
    else if PDrawing^.GetDXFTableStyleTable^.getAddres('Standard') <> nil then
      Fail('DXFTableStyleTable.getAddres on empty table <> nil')
    else begin
      AddSampleStyle(PDrawing^.DXFTableStyleTable, 'Standard', 2.5);
      if PDrawing^.DXFTableStyleTable.Count <> 1 then
        Fail(Format('DXFTableStyleTable.Count after AddStyle = %d, expected 1',
          [PDrawing^.DXFTableStyleTable.Count]))
      else
        Ok('DXFTableStyleTable is initialized by TSimpleDrawing.init');
    end;
  finally
    PDrawing^.done;
    Freemem(Pointer(PDrawing));
  end;
end;

{ init/done чертежа с AStyles стилями таблиц. Вынесено в отдельную
  процедуру, чтобы временные строки (имена стилей) освобождались до
  замера памяти в HeapDeltaAfterInitDone. }
procedure InitDoneDrawing(AStyles: Integer);
var
  Drawing: TSimpleDrawing;
  I: Integer;
begin
  Drawing.init(nil);
  for I := 1 to AStyles do
    AddSampleStyle(Drawing.DXFTableStyleTable, 'LeakStyle' + IntToStr(I), I);
  Drawing.done;
end;

{ Сколько памяти остаётся занятой после init/done чертежа
  с AStyles стилями таблиц. }
function HeapDeltaAfterInitDone(AStyles: Integer): PtrInt;
var
  Before: PtrInt;
begin
  Before := GetFPCHeapStatus.CurrHeapUsed;
  InitDoneDrawing(AStyles);
  Result := PtrInt(GetFPCHeapStatus.CurrHeapUsed) - Before;
end;

{ 2. TSimpleDrawing.done освобождает DXFTableStyleTable. Сравниваются
  «остатки» памяти после init/done без стилей и со стилями: всё, что
  выделено под стили (объекты, CellFormats, строки), должно вернуться. }
procedure TestDoneFreesStyles;
var
  Base, WithStyles: PtrInt;
begin
  HeapDeltaAfterInitDone(1); // прогрев ленивых глобальных кэшей
  Base := HeapDeltaAfterInitDone(0);
  WithStyles := HeapDeltaAfterInitDone(20);
  if WithStyles <> Base then
    Fail(Format('TSimpleDrawing.done leaks %d bytes of DXFTableStyleTable '
      + '(heap delta: %d without styles, %d with 20 styles)',
      [WithStyles - Base, Base, WithStyles]))
  else
    Ok('TSimpleDrawing.done frees DXFTableStyleTable styles and CellFormats');
end;

{ Текст секции uses интерфейса модуля (без комментариев), в нижнем регистре. }
function InterfaceUses(const AFileName: string): string;
var
  S: TStringList;
  Text: string;
  P, E: Integer;
begin
  Result := '';
  S := TStringList.Create;
  try
    S.LoadFromFile(AFileName);
    Text := LowerCase(S.Text);
  finally
    S.Free;
  end;
  P := Pos(LineEnding + 'interface', Text);
  if P = 0 then
    Exit;
  P := Pos('uses', Copy(Text, P, MaxInt)) + P - 1;
  E := Pos(';', Copy(Text, P, MaxInt)) + P - 1;
  Result := Copy(Text, P, E - P);
end;

{ 3. Модули стилей zengine не используют uzcinterface. }
procedure TestNoZcadInterfaceDependency;
const
  Units: array[0..1] of string = (
    'cad_source/zengine/styles/uzestylestablesdxf.pas',
    'cad_source/zengine/styles/uzestylesmleaderdxf.pas');
var
  I: Integer;
  UsesText: string;
begin
  for I := Low(Units) to High(Units) do begin
    UsesText := InterfaceUses(Root + Units[I]);
    if UsesText = '' then
      Fail('cannot find interface uses clause in ' + Units[I])
    else if Pos('uzcinterface', UsesText) > 0 then
      Fail(Units[I] + ' depends on uzcinterface (zcad layer)')
    else
      Ok(ExtractFileName(Units[I]) + ' does not depend on uzcinterface');
  end;
end;

procedure LoadDrawing(const AFileName: string; var ADrawing: TSimpleDrawing);
var
  DC: TDrawContext;
  ZDC: TZDrawingContext;
begin
  DC := ADrawing.CreateDrawingRC;
  ZDC.CreateRec(ADrawing, ADrawing.pObjRoot^, TLOLoad, DC);
  AddFromDXF(AFileName, ZDC);
end;

{ Заменяет значения переменных HEADER, зависящих от времени, заглушкой.
  Остальной текст сравнивается без изменений. }
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

procedure SaveDrawing(const ACase: TGoldenCase; const AOutFile: string);
var
  Drawing: TSimpleDrawing;
begin
  Drawing.init(nil);
  try
    case ACase.Source of
      dsStyles: begin
        AddSampleStyle(Drawing.DXFTableStyleTable, 'Standard', 2.5);
        AddSampleStyle(Drawing.DXFTableStyleTable, 'ZCAD1442', 3.5);
      end;
      dsEtalon:
        LoadDrawing(Root + EtalonFile, Drawing);
    end;
    if not savedxf20XX(AOutFile, Root + TemplatesDir + ACase.Template,
                       Drawing, ACase.Ver) then
      Fail('savedxf20XX failed for ' + ACase.Golden);
  finally
    Drawing.done;
  end;
end;

{ 4. Вывод savedxf20XX совпадает с эталоном. }
procedure TestGolden(const ACase: TGoldenCase);
var
  OutFile, GoldenFile: string;
  Actual, Expected: TStringList;
  I: Integer;
begin
  OutFile := GetTempDir(False) + 'nodstage0_' + ACase.Golden;
  GoldenFile := Root + GoldenDir + ACase.Golden;
  SaveDrawing(ACase, OutFile);

  Actual := TStringList.Create;
  Expected := TStringList.Create;
  try
    Actual.LoadFromFile(OutFile);
    NormalizeDXF(Actual);
    if UpdateGolden then begin
      ForceDirectories(Root + GoldenDir);
      Actual.SaveToFile(GoldenFile);
      writeln('upd:  ', ACase.Golden, ' (', Actual.Count, ' lines)');
      Exit;
    end;
    if not FileExists(GoldenFile) then begin
      Fail('golden file not found: ' + GoldenFile);
      Exit;
    end;
    Expected.LoadFromFile(GoldenFile);
    for I := 0 to Actual.Count - 1 do
      if (I >= Expected.Count) or (Actual[I] <> Expected[I]) then begin
        if I < Expected.Count then
          Fail(Format('%s: line %d: expected "%s", got "%s" (actual output: %s)',
            [ACase.Golden, I + 1, Expected[I], Actual[I], OutFile]))
        else
          Fail(Format('%s: output is longer than golden (%d > %d lines, actual output: %s)',
            [ACase.Golden, Actual.Count, Expected.Count, OutFile]));
        Exit;
      end;
    if Actual.Count <> Expected.Count then
      Fail(Format('%s: output is shorter than golden (%d < %d lines, actual output: %s)',
        [ACase.Golden, Actual.Count, Expected.Count, OutFile]))
    else
      Ok(Format('%s matches golden (%d lines)', [ACase.Golden, Actual.Count]));
  finally
    Actual.Free;
    Expected.Free;
  end;
end;

var
  I: Integer;
begin
  Root := '';
  UpdateGolden := False;
  for I := 1 to ParamCount do
    if ParamStr(I) = '--update' then
      UpdateGolden := True
    else
      Root := IncludeTrailingPathDelimiter(ParamStr(I));
  Failed := 0;

  Run('TestInitOnDirtyMemory', @TestInitOnDirtyMemory);
  Run('TestDoneFreesStyles', @TestDoneFreesStyles);
  Run('TestNoZcadInterfaceDependency', @TestNoZcadInterfaceDependency);
  for I := Low(GoldenCases) to High(GoldenCases) do
    try
      TestGolden(GoldenCases[I]);
    except
      on E: Exception do
        Fail(GoldenCases[I].Golden + ': ' + E.ClassName + ': ' + E.Message);
    end;

  if Failed = 0 then
    writeln('OK')
  else begin
    writeln('FAILED: ', Failed);
    Halt(1);
  end;
end.
